;;; npy.el --- Extra features for Python developemnt  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 by Cyriakus "Mukuge" Hill

;; Author: Cyriakus "Mukuge" Hill <cyriakus.h@gmail.com>
;; Keywords: tools, processes
;; URL: https://github.com/mukuge/npy-mode.el/
;; Package-Version: 0.1.2
;; Package-Requires: ((emacs "26.1")(f "0.20.0")(s "1.7.0"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a simple minor mode which provides python-mode with extra
;; features such as better virtualenv supports.

;; The goal of v0.1 is to support Pipenv virtualenvs.  The minor mode
;; will create inferior Python processes dedicated to Pipenv virtual
;; environments on your system.

;; The `python-mode' has two types of inferior python processes:
;; global and buffer-dedicated.  Either of these can't nicely work
;; with (Pipenv) virtual environments as is.

;; The `python-x-mode' extends `python-mode', introducing
;; virtulaenv-dedicated inferior Python processes.  You can, for
;; example, send a function definition by `python-shell-send-defun' to
;; a single virtualenv-dedicated inferior Python process from multiple
;; Python files under a same virtual environment, even when you have
;; spawned multiple inferior Python processes for different
;; virtual environments simultaneously.

;; The main entry points are `px-x-run-python', which spawns new
;; inferior python process with the virtualenv at the current buffer.

;; Installation:

;; Place this file on a directory in your `load-path', and explicitly
;; require it, and call the initialization function.
;;
;;     (require 'npy)
;;     (npy-initialize)
;;

;;;
;;; Code:
;;;

(require 'cl-lib)
(require 'f)
(require 'python)
(require 's)
(require 'subr-x)

(defgroup npy nil
  "Nano support for Pipenv virtualenvs."
  :prefix "npy-"
  :group 'python)

;;; User customization

(defcustom npy-pipenv-executable
  "pipenv"
  "The name of the Pipenv executable."
  :type '(file :must-match t)
  :safe #'file-directory-p
  :group 'npy)

(defcustom npy-command-process-name
  "npy"
  "The name of processes for calling the pipenv executable."
  :type 'string
  :group 'npy)

(defcustom npy-command-process-buffer-name
  "*npy*"
  "The name of process buffers for calling the pipenv executable."
  :type 'string
  :group 'npy)

(defcustom npy-python-shell-buffer-name
  "*Pipenv shell*"
  "The name of a python shell buffer which has access to a Pipenv virtualenv."
  :type 'string
  :group 'npy)

(defcustom npy-shell-mode-buffer-init-command
  "exec pipenv shell"
  "The shell command to launch a python interactive mode for a virtualenv."
  :type 'string
  :group 'npy)

(defcustom npy-mode-line-prefix
  " Py"
  "Mode line lighter prefix for npy.

It's used by `npy-default-mode-line' when using dynamic mode line
lighter and is the only thing shown in the mode line otherwise."
  :group 'npy
  :type 'string)

(defcustom npy-no-virtualenv-mark
  "-"
  "The mark shown on the modeline when the buffer is outside any virtualenvs."
  :group 'npy
  :type 'string)

(defcustom npy-mode-line-function
  'npy-default-mode-line
  "The function to be used to generate project-specific mode-line.

The default function adds the project name to the mode-line."
  :group 'npy
  :type 'function)

(defcustom npy-dynamic-mode-line
  t
  "Update the mode-line dynamically if true.

This is for the global minor mode version to come."
  :group 'npy
  :type 'boolean)

(defcustom npy-dynamic-mode-line-in-dired-mode
  t
  "Update the mode-line dynamically in `dired-mode' if true.

This is for the global minor mode version to come."
  :group 'npy
  :type 'boolean)

(defcustom npy-keymap-prefix
  "\C-c'"
  "The npy keymap prefix."
  :group 'npy
  :type 'string)

(defcustom npy-pipenv-project-detection
  'exploring
  "The Pipenv project detection method in use.

The value should be 'exploring (default), or 'calling."
  :group 'npy
  :type 'symbol)

;;; Vars for Debug

(defvar npy--debug nil
  "Display debug info when non-nil.")

(defvar npy--python-shell-virtualenv-root-log nil
  "A list containing the values of `python-shell-virtualenv-root' called.")

(defun npy--debug (msg &rest args)
  "Print MSG and ARGS like `message', but only if debug output is enabled."
  (when npy--debug
    (apply #'message msg args)))

;;; A var for the mode line.

(defvar-local npy--mode-line npy-mode-line-prefix)

;;; Pipenv project and virtualenv core vars and their access functionss.

(defvar-local npy--pipenv-project-root nil
  "The root directory of the Pipenv project.")

(defvar-local npy--pipenv-project-name nil
  "The Pipenv project name or the name of the directory where Pipfile exists.")

(defvar-local npy--pipenv-project-name-with-hash nil
  "The Pipenv project name with hash.")

(defvar-local npy--pipenv-virtualenv-root nil
  "The virtualenv root directory of the Pipenv project.")

(defun npy--pipenv-get-name-with-hash (path)
  "Return the filename of PATH with a Pipenv hash suffix."
  (f-filename (npy-pipenv-compat-virtualenv-name path)))

(defun npy--fill-pipenv-project-names (root)
  "Fill the Pipenv project name vars by using ROOT."
  (cond ((and (stringp root) (f-directory-p root))
         (setq npy--pipenv-project-root root
               npy--pipenv-project-name (f-filename root)
               npy--pipenv-project-name-with-hash (npy--pipenv-get-name-with-hash root)))
        ((eql root 'no-virtualenv)
         (setq npy--pipenv-project-root 'no-virtualenv
               npy--pipenv-project-name 'no-virtualenv
               npy--pipenv-project-name-with-hash 'no-virtualenv))
        ((null root)
         (setq npy--pipenv-project-root nil
               npy--pipenv-project-name nil
               npy--pipenv-project-name-with-hash nil))
        (t (message "\"%s\" is not a valid value." root))))

(defun npy--fill-pipenv-virtualenv-root (venv-path)
  "Fill the Pipenv virtualenv root var with VENV-PATH."
  (case venv-path
    (nil (setq npy--pipenv-virtualenv-root nil))
    ('no-virtualenv (setq npy--pipenv-virtualenv-root 'no-virtualenv))
    (otherwise (setq npy--pipenv-virtualenv-root venv-path))))

(defun npy--clear-all-pipenv-project-vars ()
  "Clear all Pipenv project vars."
  (npy--fill-pipenv-project-names nil)
  (npy--fill-pipenv-virtualenv-root nil)
  (setq npy--python-shell-virtualenv-root-log nil))

(defun npy--get-pipenv-project-root-from-dotproject (venv-path)
  "Get the Pipenv project root from the `.project' file in VENV-PATH."
  (when (and (stringp venv-path)
             (f-directory-p venv-path))
    (with-temp-buffer
      (insert-file-contents (concat venv-path "/.project") nil)
      (buffer-string))))

(defun npy--set-pipenv-project-vars-from-dotproject (venv-path)
  "Set the Pipenv project root and names from the `.project' file in VENV-PATH."
  (if-let* ((root (npy--get-pipenv-project-root-from-dotproject venv-path)))
      (if (and (stringp root)
               (f-directory-p root))
          (npy--fill-pipenv-project-names root))
    (npy--fill-pipenv-project-names 'no-virtualenv)
    (message "\"%s\" is not a virtualenv root.")))

(defun npy--venvpath-to-prjname (venvpath)
  "Extract a project name from VENVPATH or a virtualenv path."
  (nth 1 (s-match "/\\([^/]*\\)-........$" venvpath)))

(defun npy--venvpath-to-prjroot (venvpath)
  "Extract a project root from VENVPATH or a virtualenv path."
  (nth 1 (s-match "\\(/.*\\)-........$" venvpath)))

;;; Pipenv compatibility functions.

(defun npy-pipenv-compat--sanitize (name)
  "Return sanitized NAME.

Replace dangerous characters and cut it to the first 42 letters
if it's longer than 42."
  (let ((char-cleaned (replace-regexp-in-string "[ $`!*@\"\\\r\n\t]" "_" name)))
    (if (> (length char-cleaned) 42)
        (substring char-cleaned 0 41)
      char-cleaned)))

(defun npy-pipenv-compat-base64-strip-padding-equals (str)
  "Strip trailing padding equal sings from STR."
  (replace-regexp-in-string "=+$" "" str))

(defun npy-pipenv-compat-base64-add-padding-equals (str)
  "Add trailing padding equal signs to STR."
  (let ((remaining (% (length str) 4)))
    (concat str
            (case remaining
              (1 "===")
              (2 "==")
              (3 "=")
              (otherwise "")))))

(defun npy-pipenv-compat-base64-to-base64urlsafe (str)
  "Convert STR (base64-encoded) to a base64urlsafe-encoded string."
  (npy-pipenv-compat-base64-add-padding-equals
   (replace-regexp-in-string
    "/" "_"
    (replace-regexp-in-string
     "+" "-"
     (npy-pipenv-compat-base64-strip-padding-equals str)))))

(defun npy-pipenv-compat-base64urlsafe-to-base64 (str)
  "Convert STR (base64urlsafe-encoded) to a base64-encoded string."
  (npy-pipenv-compat-base64-add-padding-equals
   (replace-regexp-in-string
    "_" "/"
    (replace-regexp-in-string
     "-" "+"
     (npy-pipenv-compat-base64-strip-padding-equals str)))))

(defun npy-pipenv-compat-base64urlsafe-encode (str)
  "Encode STR in base64urlsafe."
  (npy-pipenv-compat-base64-to-base64urlsafe
   (base64-encode-string str)))

(defun npy-pipenv-compat-base64urlsafe-decode (str)
  "Decode STR inbase64urlsafe."
  (base64-decode-string
   (npy-pipenv-compat-base64urlsafe-to-base64
    str)))

(defun npy-pipenv-compat-hex2bin (byte-sequence-in-hex)
  "Convert BYTE-SEQUENCE-IN-HEX into a binary sequence."
  (with-temp-buffer
    (setq buffer-file-coding-system 'raw-text)
    (let ((byte-in-hex ""))
      (while (> (length byte-sequence-in-hex) 0)
        (setq byte-in-hex (substring byte-sequence-in-hex 0 2))
        (insert (string-to-number byte-in-hex 16))
        (setq byte-sequence-in-hex (substring byte-sequence-in-hex 2 nil))))
    (buffer-string)))

(defun npy-pipenv-compat--get-virtualenv-hash (name)
  "Return the cleaned NAME and its encoded hash."
  ;; I haven't implemented the PIPENV_PYTHON feature since it's for CI purpose.
  ;; See: https://github.com/pypa/pipenv/issues/2124
  (let* ((clean-name (npy-pipenv-compat--sanitize name))
         (hash (secure-hash 'sha256 clean-name))
         (bin-hash (substring (npy-pipenv-compat-hex2bin hash) 0 6))
         (encoded-hash (npy-pipenv-compat-base64urlsafe-encode bin-hash)))
    (cl-values clean-name encoded-hash)))

(defun npy-pipenv-compat-virtualenv-name (name)
  "Return the virtualenv name of a NAME or path."
  (cl-multiple-value-bind (sanitized encoded-hash)
      (npy-pipenv-compat--get-virtualenv-hash name)
    (concat sanitized "-" encoded-hash)))

;;; Functions to call the `pipenv' executable.

(defun npy--clean-response (response)
  "Clean up RESPONSE from shell command."
  (s-chomp response))

(defun npy--force-wait (process)
  "Block while PROCESS exists but release after one sec anyway."
  (let ((counter 0))
    (while (and (process-live-p process)
                (< counter 10))
      (sit-for 0.1 t)
      (cl-incf counter))
    (when (>= counter 10)
      (npy--debug "The npy--force-wait limit has reached.")
      ;; FIXME: Might better to rase an exception.
      )))

(defun npy--make-pipenv-process (command &optional filter sentinel)
  "Make a Pipenv process from COMMAND; optional custom FILTER or SENTINEL."
  (make-process
   :name npy-command-process-name
   :buffer npy-command-process-buffer-name
   :command command
   :coding 'utf-8-unix
   :filter filter
   :sentinel sentinel))

;;; Functions to find Pipenv information by calling the `pipenv' exectable.

(defun npy--process-filter-for-venv(process response)
  "PROCESS filter for '--venv' to set the variables based on RESPONSE."
  (npy--debug "response: %s" response)
  (npy--debug "matched responce: %s" (s-match "\\(/.*\\)
" response))
  (when (s-equals-p (nth 0 (last (process-command process))) "--venv")
    (if-let* ((res (s-match "\\(/.*\\)
" response))
              (path (nth 1 res))
              (f-directory-p path))
        (progn
          (npy--fill-pipenv-virtualenv-root path)
          (npy--set-pipenv-project-vars-from-dotproject path))
      (if (s-match "No virtualenv has been created for this project yet!" response)
          (npy--fill-pipenv-virtualenv-root 'no-virtualenv)))))

(defun npy--process-filter-for-where(process response)
  "PROCESS filter for '--where' to set the variables based on RESPONSE."
  (npy--debug "response: %s" response)
  (npy--debug "matched responce: %s" (s-match "\\(/.*\\)
" response))
  (when (s-equals-p (nth 0 (last (process-command process))) "--where")
    (if-let* ((res (s-match "\\(/.*\\)
" response))
              (path (nth 1 res))
              (f-directory-p path))
        (npy--fill-pipenv-project-names path))
    (if (s-match "No Pipfile at project home." response)
        (npy--fill-pipenv-project-names 'no-virtualenv))))

(defun npy--find-pipenv-virtualenv-root-by-calling ()
  "Call `pipenv' with '--venv' to get the path to the virtualenv root."
  (let ((command (list npy-pipenv-executable "--venv"))
        (filter 'npy--process-filter-for-venv))
    (npy--make-pipenv-process command filter)))

(defun npy--find-pipenv-project-root-by-calling ()
  "Call `pipenv' with '--where' to get the Pipenv project root."
  (let ((command (list npy-pipenv-executable "--where"))
        (filter 'npy--process-filter-for-where))
    (npy--make-pipenv-process command filter)))

(defun npy--set-pipenv-project-vars-by-calling ()
  "Set the Pipenv project root variables by calling `pipenv'."
  (if (null npy--pipenv-project-root)
      (npy--force-wait (npy--find-pipenv-project-root-by-calling))
    npy--pipenv-project-root))

(defun npy--set-pipenv-virtualenv-root-var-by-calling ()
  "Set the Pipenv virtualenv root variable by calling `pipenv'."
  (if (null npy--pipenv-virtualenv-root)
      (npy--force-wait (npy--find-pipenv-virtualenv-root-by-calling))
    npy--pipenv-virtualenv-root))

(defun npy--set-pipenv-project-vars ()
  "Interface function to set the Pipenv project vars."
  (if (eql npy-pipenv-project-detection 'exploring)
      (npy--set-pipenv-project-vars-by-exploring)
    (npy--set-pipenv-project-vars-by-calling)))

(defun npy--set-pipenv-virtualenv-root-var ()
  "Set the Pipenv virtualenv root var."
  (npy--set-pipenv-virtualenv-root-var-by-calling))

;;; Functions to find Pipenv information by exploring directory structures.

(defun npy--set-pipenv-project-vars-by-exploring ()
  "Set the Pipenv project variables by exploring its path bottom-up."
  (if-let* ((filename (buffer-file-name (current-buffer)))
            (dirname (f-dirname filename))
            (root (npy--find-pipenv-project-root-by-exploring dirname)))
      (npy--fill-pipenv-project-names root)
    (npy--fill-pipenv-project-names 'no-virtualenv)))

(defun npy--find-pipenv-project-root-by-exploring (dirname)
  "Return the Pipenv project root if DIRNAME is under a project, otherwise nil."
  (npy--find-pipenv-project-root-by-exploring-impl (f-split (f-full dirname))))

(defun npy--find-pipenv-project-root-by-exploring-impl (dirname-list)
  "Return a Pipenv root if DIRNAME-LIST is under a project, otherwise nil.

DIRNAME-LIST should be the f-split style: e.g. (\"/\" \"usr\" \"local\")."
  (if (null dirname-list)
      nil
    (let ((dirname (apply #'f-join dirname-list)))
      (if (npy--pipenv-root-p dirname)
          dirname
        (npy--find-pipenv-project-root-by-exploring-impl (nbutlast dirname-list 1))))))
;; FIXME: Should rewite this as a non-recursive function.

(defun npy--pipenv-root-p (dirname)
  "Return t if DIRNAME is a Pipenv project root, otherwise nil."
  (f-exists-p (concat (f-full dirname) "/Pipfile")))

;;; Functions for the integrations with the inferior python mode.

(defun npy-python-shell-get-buffer-advice (orig-fun &rest orig-args)
  "Tweak the buffer entity in ORIG-ARGS.

Replace it with the inferior process for the project exists, otherwise
leave it untouched.  ORIG-FUN should be `python-shell-get-buffer'."
  (if (derived-mode-p 'inferior-python-mode)
      (current-buffer)
    (npy--set-pipenv-project-vars)
    (if (stringp npy--pipenv-project-name) ;; i.e. it's not 'no-virtualenv nor nil.
        (if-let* ((venv-dedicated-buffer-process-name
                   (format "*%s[v:%s;b:%s]*" python-shell-buffer-name
                           npy--pipenv-project-name
                           (f-filename (buffer-file-name))))
                  (venv-dedicated-process-name (format "*%s[v:%s]*"
                                                       python-shell-buffer-name
                                                       npy--pipenv-project-name)))
            (if-let (venv-dedicated-buffer-running
                     (comint-check-proc venv-dedicated-buffer-process-name))
                venv-dedicated-buffer-process-name
              (if-let (venv-dedicated-running
                       (comint-check-proc venv-dedicated-process-name))
                  venv-dedicated-process-name
                (let ((res (apply orig-fun orig-args))) ;; Maybe raising an error is better.
                  res))))
      (let ((res (apply orig-fun orig-args)))
        res))))

;;; Functions to manage the modeline.

(defun npy-default-mode-line ()
  "Report the Pipenv project name associated with the buffer in the modeline."
  (npy--set-pipenv-project-vars)
  (format "%s[v:%s]"
          npy-mode-line-prefix
          (cond ((stringp npy--pipenv-project-root) npy--pipenv-project-name)
                ((eq npy--pipenv-project-root 'no-virtualenv) npy-no-virtualenv-mark)
                (t "ERR"))))

(defun npy--update-mode-line ()
  "Update the npy modeline."
  (let ((mode-line (funcall npy-mode-line-function)))
    (setq npy--mode-line mode-line))
  (force-mode-line-update))

;;; Hook and advice functions.

(defun npy-write-file-advice (orig-fun &rest orig-args)
  "Update two variables when `write-file' (ORIG-FUN with ORIG-ARGS) is called.

The two variables are: `npy--pipenv-project-root' and
`npy--pipenv-project-name'"
  (let ((res (apply orig-fun orig-args)))
    (when (bound-and-true-p npy-mode)
      (npy--force-wait (npy--find-pipenv-project-root-by-calling))
      (npy--update-mode-line))
    res))

(defun npy-find-file-hook-function ()
  "Called by `find-file-hook' when `npy-mode' is on.

This is for the global minor mode version to come."
  (when npy-dynamic-mode-line
    (npy--update-mode-line)))

(defun npy-dired-mode-hook-function ()
  "Get the name and root of a Pipenv project, and update the mode line.

This is for the global minor mode version to come."
  (when npy-dynamic-mode-line-in-dired-mode
    (npy--force-wait (npy--find-pipenv-project-root-by-calling))
    (npy--update-mode-line)))

(defun npy-desktop-save-hook-function ()
  "Disable npy mode before `desktop-mode' saves configurations.

`npy' can significantly slow Emacs startup process, when
`desktop-mode' restores many files.  This is a temporary workaround
and will be removed in the future release when alternative methods
to detect Pipenv virtualenvs implemented.

This is for the global minor mode version to come."
  (npy-mode 0))

;;; Functions for debug.

(defun npy--show-pipenv-vars ()
  "Show Pipenv project information."
  (interactive)
  (message (concat "pipenv-project-root: %s\n"
                   "pipenv-project-name: %s\n"
                   "pipenv-project-name-with-hash: %s\n"
                   "pipenv-virtualenv-root: %s\n"
                   "python-shell-virtualenv-root: %s\n"
                   "python-shell-virtualenv-root-log: %s")
           npy--pipenv-project-root
           npy--pipenv-project-name
           npy--pipenv-project-name-with-hash
           npy--pipenv-virtualenv-root
           python-shell-virtualenv-root
           npy--python-shell-virtualenv-root-log))

(defun npy--clear-pipenv-vars ()
  "Clear Pipenv project information."
  (interactive)
  (npy--clear-all-pipenv-project-vars)
  (npy--show-pipenv-vars))

;;;
;;; User facing functions and its helpers.
;;;

(defmacro npy--when-valid (var it)
  "Do IT when VAR is valid, otherwise show a warning."
  `(cond ((stringp ,var)
          ,it)
         ((eq ,var 'no-virtualenv)
          (if (buffer-file-name)
              (message "No virtualenv has been created for this project yet!")
            (message "No virtualenv got deteced. Maybe because the buffer is not associated with a file.")))
         (t (message "Something wrong has happend in npy."))))

(defun npy-initialize ()
  "Initialize npy-mode."
  (with-eval-after-load "python"
    (add-hook 'python-mode-hook 'npy-mode)))

(defun npy-run-python ()
  "Run an inferior python process for a virtualenv.

When called interactively with `prefix-arg', it spawns a
buffer-dedicated inferior python process with the access to the
virtualenv."
  (interactive)
  (npy--force-wait (npy--find-pipenv-virtualenv-root-by-calling))
  (npy--set-pipenv-project-vars)
  (npy--when-valid
   npy--pipenv-virtualenv-root
   (let* ((exec-path (cons npy--pipenv-virtualenv-root exec-path))
          (python-shell-virtualenv-root npy--pipenv-virtualenv-root)
          (process-name (if current-prefix-arg
                            (format "%s[v:%s;b:%s]"
                                    python-shell-buffer-name
                                    npy--pipenv-project-name
                                    (f-filename (buffer-file-name)))
                          (format "%s[v:%s]"
                                  python-shell-buffer-name
                                  npy--pipenv-project-name))))
     (push python-shell-virtualenv-root npy--python-shell-virtualenv-root-log)
     (get-buffer-process
      (python-shell-make-comint (python-shell-calculate-command)
                                process-name t)))))

(defun npy-display-pipenv-project-root ()
  "Show the path to the Pipenv project root directory."
  (interactive)
  (npy--set-pipenv-project-vars)
  (npy--update-mode-line)
  (npy--when-valid
   npy--pipenv-project-root
   (message "Project: %s" npy--pipenv-project-root)))

(defun npy-update-pipenv-project-root ()
  "Update the Pipenv project root directory."
  (interactive)
  (npy--fill-pipenv-project-names nil)
  (npy-display-pipenv-project-root))

(defun npy-display-pipenv-virtualenv-root ()
  "Show the path to the Pipenv virtualenv root directory."
  (interactive)
  (npy--set-pipenv-virtualenv-root-var)
  (npy--update-mode-line)
  (npy--when-valid
   npy--pipenv-virtualenv-root
   (message "Virtualenv: %s" npy--pipenv-virtualenv-root)))

(defun npy-update-pipenv-virtualenv-root ()
  "Show the path to the Pipenv virtualenv root directory."
  (interactive)
  (npy--clear-all-pipenv-project-vars)
  (npy-display-pipenv-virtualenv-root))

(defun npy-pipenv-shell ()
  "Spawn a shell-mode shell and invoke a Pipenv shell."
  (interactive)
  (let ((name (generate-new-buffer-name npy-python-shell-buffer-name)))
    (pop-to-buffer name)
    (shell (current-buffer))
    (insert npy-shell-mode-buffer-init-command)
    (setq-local comint-process-echoes t)
    (comint-send-input)
    (comint-clear-buffer)))

;;; Defining the minor mode.

(defvar npy-command-map
  (let ((map (make-sparse-keymap)))
    ;; Shell interaction
    (define-key map "p" #'npy-run-python)
    (define-key map "s" #'npy-pipenv-shell)
    ;; Some util commands
    (define-key map "d" #'npy-display-pipenv-project-root)
    (define-key map "u" #'npy-update-pipenv-project-root)
    (define-key map "v" #'npy-display-pipenv-virtualenv-root)
    map)
  "Keymap for npy mode commands after `npy-keymap-prefix'.")
(fset 'npy-command-map npy-command-map)

(defvar npy-mode-map
  (let ((map (make-sparse-keymap)))
    (when npy-keymap-prefix
      (define-key map npy-keymap-prefix 'npy-command-map))
    map)
  "Keymap for npy mode.")

;;;###autoload
(define-minor-mode npy-mode
  "Minor mode for Python"
  :group 'npy
  :require 'npy
  :lighter npy--mode-line
  :keymap npy-mode-map
  :global nil
  (cond
   (npy-mode
    ;; These hooks are for when using npy as a global minor mode.
    ;;(add-hook 'find-file-hook 'npy-find-file-hook-function)
    ;;(add-hook 'dired-mode-hook 'npy-dired-mode-hook-function)
    ;;(add-hook 'desktop-save-hook 'npy-desktop-save-hook-function)
    (advice-add 'python-shell-get-buffer :around #'npy-python-shell-get-buffer-advice)
    (advice-add 'write-file :around #'npy-write-file-advice)
    (npy-update-pipenv-project-root))
   (t
    ;;(remove-hook 'find-file-hook 'npy-find-file-hook-function)
    ;;(remove-hook 'dired-mode-hook 'npy-dired-mode-hook-function)
    ;;(remove-hook 'desktop-save-hook 'npy-desktop-save-hook-function)
    (advice-remove 'python-shell-get-buffer #'npy-python-shell-get-buffer-advice)
    (advice-remove 'write-file #'npy-write-file-advice))))

(provide 'npy)

;;; npy.el ends here
