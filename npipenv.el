;;; py-x.el --- Extra features for Python developemnt  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 by Cyriakus "Mukuge" Hill

;; Author: Cyriakus "Mukuge" Hill <cyriakus.h@gmail.com>
;; Keywords: tools, processes
;; URL: https://github.com/mukuge/py-x-mode.el/
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
;;     (require 'py-x)
;;     (py-x-initialize)
;;

;;;
;;; Code:
;;;

(require 'cl-lib)
(require 'f)
(require 'python)
(require 's)
(require 'subr-x)

(defgroup py-x nil
  "Nano support for Pipenv virtualenvs."
  :prefix "py-x-"
  :group 'python)

;;; User customization

(defcustom py-x-pipenv-executable
  "pipenv"
  "The name of the Pipenv executable."
  :type '(file :must-match t)
  :safe #'file-directory-p
  :group 'py-x)

(defcustom py-x-command-process-name
  "py-x"
  "The name of processes for calling the pipenv executable."
  :type 'string
  :group 'py-x)

(defcustom py-x-command-process-buffer-name
  "*Py-x*"
  "The name of process buffers for calling the pipenv executable."
  :type 'string
  :group 'py-x)

(defcustom py-x-python-shell-buffer-name
  "*Pipenv shell*"
  "The name of a python shell buffer which has access to a Pipenv virtualenv."
  :type 'string
  :group 'py-x)

(defcustom py-x-shell-mode-buffer-init-command
  "exec pipenv shell"
  "The shell command to launch a python interactive mode for a virtualenv."
  :type 'string
  :group 'py-x)

(defcustom py-x-mode-line-prefix
  " Py"
  "Mode line lighter prefix for py-x.

It's used by `py-x-default-mode-line' when using dynamic mode line
lighter and is the only thing shown in the mode line otherwise."
  :group 'py-x
  :type 'string)

(defcustom py-x-no-virtualenv-mark
  "-"
  "The mark shown on the modeline when the buffer is outside any virtualenvs."
  :group 'py-x
  :type 'string)

(defcustom py-x-mode-line-function
  'py-x-default-mode-line
  "The function to be used to generate project-specific mode-line.

The default function adds the project name to the mode-line."
  :group 'py-x
  :type 'function)

(defcustom py-x-dynamic-mode-line
  t
  "Update the mode-line dynamically if true.

This is for the global minor mode version to come."
  :group 'py-x
  :type 'boolean)

(defcustom py-x-dynamic-mode-line-in-dired-mode
  t
  "Update the mode-line dynamically in `dired-mode' if true.

This is for the global minor mode version to come."
  :group 'py-x
  :type 'boolean)

(defcustom py-x-keymap-prefix
  "\C-c'"
  "The py-x keymap prefix."
  :group 'py-x
  :type 'string)

(defcustom py-x-pipenv-project-detection
  'exploring
  "The Pipenv project detection method in use.

The value should be 'exploring (default), or 'calling."
  :group 'py-x
  :type 'symbol)

;;; Vars for Debug

(defvar py-x--debug nil
  "Display debug info when non-nil.")

(defvar py-x--python-shell-virtualenv-root-log nil
  "A list containing the values of `python-shell-virtualenv-root' called.")

(defun py-x--debug (msg &rest args)
  "Print MSG and ARGS like `message', but only if debug output is enabled."
  (when py-x--debug
    (apply #'message msg args)))

;;; A var for the mode line.

(defvar-local py-x--mode-line py-x-mode-line-prefix)

;;; Pipenv project and virtualenv core vars and their access functionss.

(defvar-local py-x--pipenv-project-root nil
  "The root directory of the Pipenv project.")

(defvar-local py-x--pipenv-project-name nil
  "The Pipenv project name or the name of the directory where Pipfile exists.")

(defvar-local py-x--pipenv-project-name-with-hash nil
  "The Pipenv project name with hash.")

(defvar-local py-x--pipenv-virtualenv-root nil
  "The virtualenv root directory of the Pipenv project.")

(defun py-x--pipenv-get-name-with-hash (path)
  "Return the filename of PATH with a Pipenv hash suffix."
  (f-filename (py-x-pipenv-compat-virtualenv-name path)))

(defun py-x--fill-pipenv-project-names (root)
  "Fill the Pipenv project name vars by using ROOT."
  (cond ((and (stringp root) (f-directory-p root))
         (setq py-x--pipenv-project-root root
               py-x--pipenv-project-name (f-filename root)
               py-x--pipenv-project-name-with-hash (py-x--pipenv-get-name-with-hash root)))
        ((eql root 'no-virtualenv)
         (setq py-x--pipenv-project-root 'no-virtualenv
               py-x--pipenv-project-name 'no-virtualenv
               py-x--pipenv-project-name-with-hash 'no-virtualenv))
        ((null root)
         (setq py-x--pipenv-project-root nil
               py-x--pipenv-project-name nil
               py-x--pipenv-project-name-with-hash nil))
        (t (message "\"%s\" is not a valid value." root))))

(defun py-x--fill-pipenv-virtualenv-root (venv-path)
  "Fill the Pipenv virtualenv root var with VENV-PATH."
  (case venv-path
    (nil (setq py-x--pipenv-virtualenv-root nil))
    ('no-virtualenv (setq py-x--pipenv-virtualenv-root 'no-virtualenv))
    (otherwise (setq py-x--pipenv-virtualenv-root venv-path))))

(defun py-x--clear-all-pipenv-project-vars ()
  "Clear all Pipenv project vars."
  (py-x--fill-pipenv-project-names nil)
  (py-x--fill-pipenv-virtualenv-root nil)
  (setq py-x--python-shell-virtualenv-root-log nil))

(defun py-x--get-pipenv-project-root-from-dotproject (venv-path)
  "Get the Pipenv project root from the `.project' file in VENV-PATH."
  (when (and (stringp venv-path)
             (f-directory-p venv-path))
    (with-temp-buffer
      (insert-file-contents (concat venv-path "/.project") nil)
      (buffer-string))))

(defun py-x--set-pipenv-project-vars-from-dotproject (venv-path)
  "Set the Pipenv project root and names from the `.project' file in VENV-PATH."
  (if-let* ((root (py-x--get-pipenv-project-root-from-dotproject venv-path)))
      (if (and (stringp root)
               (f-directory-p root))
          (py-x--fill-pipenv-project-names root))
    (py-x--fill-pipenv-project-names 'no-virtualenv)
    (message "\"%s\" is not a virtualenv root.")))

(defun py-x--venvpath-to-prjname (venvpath)
  "Extract a project name from VENVPATH or a virtualenv path."
  (nth 1 (s-match "/\\([^/]*\\)-........$" venvpath)))

(defun py-x--venvpath-to-prjroot (venvpath)
  "Extract a project root from VENVPATH or a virtualenv path."
  (nth 1 (s-match "\\(/.*\\)-........$" venvpath)))

;;; Pipenv compatibility functions.

(defun py-x-pipenv-compat--sanitize (name)
  "Return sanitized NAME.

Replace dangerous characters and cut it to the first 42 letters
if it's longer than 42."
  (let ((char-cleaned (replace-regexp-in-string "[ $`!*@\"\\\r\n\t]" "_" name)))
    (if (> (length char-cleaned) 42)
        (substring char-cleaned 0 41)
      char-cleaned)))

(defun py-x-pipenv-compat-base64-strip-padding-equals (str)
  "Strip trailing padding equal sings from STR."
  (replace-regexp-in-string "=+$" "" str))

(defun py-x-pipenv-compat-base64-add-padding-equals (str)
  "Add trailing padding equal signs to STR."
  (let ((remaining (% (length str) 4)))
    (concat str
            (case remaining
              (1 "===")
              (2 "==")
              (3 "=")
              (otherwise "")))))

(defun py-x-pipenv-compat-base64-to-base64urlsafe (str)
  "Convert STR (base64-encoded) to a base64urlsafe-encoded string."
  (py-x-pipenv-compat-base64-add-padding-equals
   (replace-regexp-in-string
    "/" "_"
    (replace-regexp-in-string
     "+" "-"
     (py-x-pipenv-compat-base64-strip-padding-equals str)))))

(defun py-x-pipenv-compat-base64urlsafe-to-base64 (str)
  "Convert STR (base64urlsafe-encoded) to a base64-encoded string."
  (py-x-pipenv-compat-base64-add-padding-equals
   (replace-regexp-in-string
    "_" "/"
    (replace-regexp-in-string
     "-" "+"
     (py-x-pipenv-compat-base64-strip-padding-equals str)))))

(defun py-x-pipenv-compat-base64urlsafe-encode (str)
  "Encode STR in base64urlsafe."
  (py-x-pipenv-compat-base64-to-base64urlsafe
   (base64-encode-string str)))

(defun py-x-pipenv-compat-base64urlsafe-decode (str)
  "Decode STR inbase64urlsafe."
  (base64-decode-string
   (py-x-pipenv-compat-base64urlsafe-to-base64
    str)))

(defun py-x-pipenv-compat-hex2bin (byte-sequence-in-hex)
  "Convert BYTE-SEQUENCE-IN-HEX into a binary sequence."
  (with-temp-buffer
    (setq buffer-file-coding-system 'raw-text)
    (let ((byte-in-hex ""))
      (while (> (length byte-sequence-in-hex) 0)
        (setq byte-in-hex (substring byte-sequence-in-hex 0 2))
        (insert (string-to-number byte-in-hex 16))
        (setq byte-sequence-in-hex (substring byte-sequence-in-hex 2 nil))))
    (buffer-string)))

(defun py-x-pipenv-compat--get-virtualenv-hash (name)
  "Return the cleaned NAME and its encoded hash."
  ;; I haven't implemented the PIPENV_PYTHON feature since it's for CI purpose.
  ;; See: https://github.com/pypa/pipenv/issues/2124
  (let* ((clean-name (py-x-pipenv-compat--sanitize name))
         (hash (secure-hash 'sha256 clean-name))
         (bin-hash (substring (py-x-pipenv-compat-hex2bin hash) 0 6))
         (encoded-hash (py-x-pipenv-compat-base64urlsafe-encode bin-hash)))
    (cl-values clean-name encoded-hash)))

(defun py-x-pipenv-compat-virtualenv-name (name)
  "Return the virtualenv name of a NAME or path."
  (cl-multiple-value-bind (sanitized encoded-hash)
      (py-x-pipenv-compat--get-virtualenv-hash name)
    (concat sanitized "-" encoded-hash)))

;;; Functions to call the `pipenv' executable.

(defun py-x--clean-response (response)
  "Clean up RESPONSE from shell command."
  (s-chomp response))

(defun py-x--force-wait (process)
  "Block while PROCESS exists but release after one sec anyway."
  (let ((counter 0))
    (while (and (process-live-p process)
                (< counter 10))
      (sit-for 0.1 t)
      (cl-incf counter))
    (when (>= counter 10)
      (py-x--debug "The py-x--force-wait limit has reached.")
      ;; FIXME: Might better to rase an exception.
      )))

(defun py-x--make-pipenv-process (command &optional filter sentinel)
  "Make a Pipenv process from COMMAND; optional custom FILTER or SENTINEL."
  (make-process
   :name py-x-command-process-name
   :buffer py-x-command-process-buffer-name
   :command command
   :coding 'utf-8-unix
   :filter filter
   :sentinel sentinel))

;;; Functions to find Pipenv information by calling the `pipenv' exectable.

(defun py-x--process-filter-for-venv(process response)
  "PROCESS filter for '--venv' to set the variables based on RESPONSE."
  (py-x--debug "response: %s" response)
  (py-x--debug "matched responce: %s" (s-match "\\(/.*\\)
" response))
  (when (s-equals-p (nth 0 (last (process-command process))) "--venv")
    (if-let* ((res (s-match "\\(/.*\\)
" response))
              (path (nth 1 res))
              (f-directory-p path))
        (progn
          (py-x--fill-pipenv-virtualenv-root path)
          (py-x--set-pipenv-project-vars-from-dotproject path))
      (if (s-match "No virtualenv has been created for this project yet!" response)
          (py-x--fill-pipenv-virtualenv-root 'no-virtualenv)))))

(defun py-x--process-filter-for-where(process response)
  "PROCESS filter for '--where' to set the variables based on RESPONSE."
  (py-x--debug "response: %s" response)
  (py-x--debug "matched responce: %s" (s-match "\\(/.*\\)
" response))
  (when (s-equals-p (nth 0 (last (process-command process))) "--where")
    (if-let* ((res (s-match "\\(/.*\\)
" response))
              (path (nth 1 res))
              (f-directory-p path))
        (py-x--fill-pipenv-project-names path))
    (if (s-match "No Pipfile at project home." response)
        (py-x--fill-pipenv-project-names 'no-virtualenv))))

(defun py-x--find-pipenv-virtualenv-root-by-calling ()
  "Call `pipenv' with '--venv' to get the path to the virtualenv root."
  (let ((command (list py-x-pipenv-executable "--venv"))
        (filter 'py-x--process-filter-for-venv))
    (py-x--make-pipenv-process command filter)))

(defun py-x--find-pipenv-project-root-by-calling ()
  "Call `pipenv' with '--where' to get the Pipenv project root."
  (let ((command (list py-x-pipenv-executable "--where"))
        (filter 'py-x--process-filter-for-where))
    (py-x--make-pipenv-process command filter)))

(defun py-x--set-pipenv-project-vars-by-calling ()
  "Set the Pipenv project root variables by calling `pipenv'."
  (if (null py-x--pipenv-project-root)
      (py-x--force-wait (py-x--find-pipenv-project-root-by-calling))
    py-x--pipenv-project-root))

(defun py-x--set-pipenv-virtualenv-root-var-by-calling ()
  "Set the Pipenv virtualenv root variable by calling `pipenv'."
  (if (null py-x--pipenv-virtualenv-root)
      (py-x--force-wait (py-x--find-pipenv-virtualenv-root-by-calling))
    py-x--pipenv-virtualenv-root))

(defun py-x--set-pipenv-project-vars ()
  "Interface function to set the Pipenv project vars."
  (if (eql py-x-pipenv-project-detection 'exploring)
      (py-x--set-pipenv-project-vars-by-exploring)
    (py-x--set-pipenv-project-vars-by-calling)))

(defun py-x--set-pipenv-virtualenv-root-var ()
  "Set the Pipenv virtualenv root var."
  (py-x--set-pipenv-virtualenv-root-var-by-calling))

;;; Functions to find Pipenv information by exploring directory structures.

(defun py-x--set-pipenv-project-vars-by-exploring ()
  "Set the Pipenv project variables by exploring its path bottom-up."
  (if-let* ((filename (buffer-file-name (current-buffer)))
            (dirname (f-dirname filename))
            (root (py-x--find-pipenv-project-root-by-exploring dirname)))
      (py-x--fill-pipenv-project-names root)
    (py-x--fill-pipenv-project-names 'no-virtualenv)))

(defun py-x--find-pipenv-project-root-by-exploring (dirname)
  "Return the Pipenv project root if DIRNAME is under a project, otherwise nil."
  (py-x--find-pipenv-project-root-by-exploring-impl (f-split (f-full dirname))))

(defun py-x--find-pipenv-project-root-by-exploring-impl (dirname-list)
  "Return a Pipenv root if DIRNAME-LIST is under a project, otherwise nil.

DIRNAME-LIST should be the f-split style: e.g. (\"/\" \"usr\" \"local\")."
  (if (null dirname-list)
      nil
    (let ((dirname (apply #'f-join dirname-list)))
      (if (py-x--pipenv-root-p dirname)
          dirname
        (py-x--find-pipenv-project-root-by-exploring-impl (nbutlast dirname-list 1))))))
;; FIXME: Should rewite this as a non-recursive function.

(defun py-x--pipenv-root-p (dirname)
  "Return t if DIRNAME is a Pipenv project root, otherwise nil."
  (f-exists-p (concat (f-full dirname) "/Pipfile")))

;;; Functions for the integrations with the inferior python mode.

(defun py-x-python-shell-get-buffer-advice (orig-fun &rest orig-args)
  "Tweak the buffer entity in ORIG-ARGS.

Replace it with the inferior process for the project exists, otherwise
leave it untouched.  ORIG-FUN should be `python-shell-get-buffer'."
  (if (derived-mode-p 'inferior-python-mode)
      (current-buffer)
    (py-x--set-pipenv-project-vars)
    (if (stringp py-x--pipenv-project-name) ;; i.e. it's not 'no-virtualenv nor nil.
        (if-let* ((venv-dedicated-buffer-process-name
                   (format "*%s[v:%s;b:%s]*" python-shell-buffer-name
                           py-x--pipenv-project-name
                           (f-filename (buffer-file-name))))
                  (venv-dedicated-process-name (format "*%s[v:%s]*"
                                                       python-shell-buffer-name
                                                       py-x--pipenv-project-name)))
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

(defun py-x-default-mode-line ()
  "Report the Pipenv project name associated with the buffer in the modeline."
  (py-x--set-pipenv-project-vars)
  (format "%s[v:%s]"
          py-x-mode-line-prefix
          (cond ((stringp py-x--pipenv-project-root) py-x--pipenv-project-name)
                ((eq py-x--pipenv-project-root 'no-virtualenv) py-x-no-virtualenv-mark)
                (t "ERR"))))

(defun py-x--update-mode-line ()
  "Update the py-x modeline."
  (let ((mode-line (funcall py-x-mode-line-function)))
    (setq py-x--mode-line mode-line))
  (force-mode-line-update))

;;; Hook and advice functions.

(defun py-x-write-file-advice (orig-fun &rest orig-args)
  "Update two variables when `write-file' (ORIG-FUN with ORIG-ARGS) is called.

The two variables are: `py-x--pipenv-project-root' and
`py-x--pipenv-project-name'"
  (let ((res (apply orig-fun orig-args)))
    (when (bound-and-true-p py-x-mode)
      (py-x--force-wait (py-x--find-pipenv-project-root-by-calling))
      (py-x--update-mode-line))
    res))

(defun py-x-find-file-hook-function ()
  "Called by `find-file-hook' when `py-x-mode' is on.

This is for the global minor mode version to come."
  (when py-x-dynamic-mode-line
    (py-x--update-mode-line)))

(defun py-x-dired-mode-hook-function ()
  "Get the name and root of a Pipenv project, and update the mode line.

This is for the global minor mode version to come."
  (when py-x-dynamic-mode-line-in-dired-mode
    (py-x--force-wait (py-x--find-pipenv-project-root-by-calling))
    (py-x--update-mode-line)))

(defun py-x-desktop-save-hook-function ()
  "Disable py-x mode before `desktop-mode' saves configurations.

`py-x' can significantly slow Emacs startup process, when
`desktop-mode' restores many files.  This is a temporary workaround
and will be removed in the future release when alternative methods
to detect Pipenv virtualenvs implemented.

This is for the global minor mode version to come."
  (py-x-mode 0))

;;; Functions for debug.

(defun py-x--show-pipenv-vars ()
  "Show Pipenv project information."
  (interactive)
  (message (concat "pipenv-project-root: %s\n"
                   "pipenv-project-name: %s\n"
                   "pipenv-project-name-with-hash: %s\n"
                   "pipenv-virtualenv-root: %s\n"
                   "python-shell-virtualenv-root-log: %s\n")
           py-x--pipenv-project-root
           py-x--pipenv-project-name
           py-x--pipenv-project-name-with-hash
           py-x--pipenv-virtualenv-root
           py-x--python-shell-virtualenv-root-log))

(defun py-x--clear-pipenv-vars ()
  "Clear Pipenv project information."
  (interactive)
  (py-x--clear-all-pipenv-project-vars)
  (py-x--show-pipenv-vars))

;;;
;;; User facing functions and its helpers.
;;;

(defmacro py-x--when-valid (var it)
  "Do IT when VAR is valid, otherwise show a warning."
  `(cond ((stringp ,var)
          ,it)
         ((eq ,var 'no-virtualenv)
          (if (buffer-file-name)
              (message "No virtualenv has been created for this project yet!")
            (message "No virtualenv got deteced. Maybe because the buffer is not associated with a file.")))
         (t (message "Something wrong has happend in py-x."))))

(defun py-x-initialize ()
  "Initialize py-x-mode."
  (with-eval-after-load "python"
    (add-hook 'python-mode-hook 'py-x-mode)))

(defun py-x-run-python ()
  "Run an inferior python process for a virtualenv.

When called interactively with `prefix-arg', it spawns a
buffer-dedicated inferior python process with the access to the
virtualenv."
  (interactive)
  (py-x--force-wait (py-x--find-pipenv-virtualenv-root-by-calling))
  (py-x--set-pipenv-project-vars)
  (py-x--when-valid
   py-x--pipenv-virtualenv-root
   (let* ((exec-path (cons py-x--pipenv-virtualenv-root exec-path))
          (python-shell-virtualenv-root py-x--pipenv-virtualenv-root)
          (process-name (if current-prefix-arg
                            (format "%s[v:%s;b:%s]"
                                    python-shell-buffer-name
                                    py-x--pipenv-project-name
                                    (f-filename (buffer-file-name)))
                          (format "%s[v:%s]"
                                  python-shell-buffer-name
                                  py-x--pipenv-project-name))))
     (push python-shell-virtualenv-root py-x--python-shell-virtualenv-root-log)
     (get-buffer-process
      (python-shell-make-comint (python-shell-calculate-command)
                                process-name t)))))

(defun py-x-display-pipenv-project-root ()
  "Show the path to the Pipenv project root directory."
  (interactive)
  (py-x--set-pipenv-project-vars)
  (py-x--update-mode-line)
  (py-x--when-valid
   py-x--pipenv-project-root
   (message "Project: %s" py-x--pipenv-project-root)))

(defun py-x-update-pipenv-project-root ()
  "Update the Pipenv project root directory."
  (interactive)
  (py-x--fill-pipenv-project-names nil)
  (py-x-display-pipenv-project-root))

(defun py-x-display-pipenv-virtualenv-root ()
  "Show the path to the Pipenv virtualenv root directory."
  (interactive)
  (py-x--set-pipenv-virtualenv-root-var)
  (py-x--update-mode-line)
  (py-x--when-valid
   py-x--pipenv-virtualenv-root
   (message "Virtualenv: %s" py-x--pipenv-virtualenv-root)))

(defun py-x-update-pipenv-virtualenv-root ()
  "Show the path to the Pipenv virtualenv root directory."
  (interactive)
  (py-x--clear-all-pipenv-project-vars)
  (py-x-display-pipenv-virtualenv-root))

(defun py-x-pipenv-shell ()
  "Spawn a shell-mode shell and invoke a Pipenv shell."
  (interactive)
  (let ((name (generate-new-buffer-name py-x-python-shell-buffer-name)))
    (pop-to-buffer name)
    (shell (current-buffer))
    (insert py-x-shell-mode-buffer-init-command)
    (setq-local comint-process-echoes t)
    (comint-send-input)
    (comint-clear-buffer)))

;;; Defining the minor mode.

(defvar py-x-command-map
  (let ((map (make-sparse-keymap)))
    ;; Shell interaction
    (define-key map "p" #'py-x-run-python)
    (define-key map "s" #'py-x-pipenv-shell)
    ;; Some util commands
    (define-key map "d" #'py-x-display-pipenv-project-root)
    (define-key map "u" #'py-x-update-pipenv-project-root)
    (define-key map "v" #'py-x-display-pipenv-virtualenv-root)
    map)
  "Keymap for py-x mode commands after `py-x-keymap-prefix'.")
(fset 'py-x-command-map py-x-command-map)

(defvar py-x-mode-map
  (let ((map (make-sparse-keymap)))
    (when py-x-keymap-prefix
      (define-key map py-x-keymap-prefix 'py-x-command-map))
    map)
  "Keymap for py-x mode.")

;;;###autoload
(define-minor-mode py-x-mode
  "Minor mode for Python"
  :group 'py-x
  :require 'py-x
  :lighter py-x--mode-line
  :keymap py-x-mode-map
  :global nil
  (cond
   (py-x-mode
    ;; These hooks are for when using py-x as a global minor mode.
    ;;(add-hook 'find-file-hook 'py-x-find-file-hook-function)
    ;;(add-hook 'dired-mode-hook 'py-x-dired-mode-hook-function)
    ;;(add-hook 'desktop-save-hook 'py-x-desktop-save-hook-function)
    (advice-add 'python-shell-get-buffer :around #'py-x-python-shell-get-buffer-advice)
    (advice-add 'write-file :around #'py-x-write-file-advice)
    (py-x-update-pipenv-project-root))
   (t
    ;;(remove-hook 'find-file-hook 'py-x-find-file-hook-function)
    ;;(remove-hook 'dired-mode-hook 'py-x-dired-mode-hook-function)
    ;;(remove-hook 'desktop-save-hook 'py-x-desktop-save-hook-function)
    (advice-remove 'python-shell-get-buffer #'py-x-python-shell-get-buffer-advice)
    (advice-remove 'write-file #'py-x-write-file-advice))))

(provide 'py-x)

;;; py-x.el ends here
