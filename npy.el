;;; npy.el --- Extra features for Python developemnt  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 by Cyriakus "Mukuge" Hill

;; Author: Cyriakus "Mukuge" Hill <cyriakus.h@gmail.com>
;; Keywords: tools, processes
;; URL: https://github.com/mukuge/npy-mode.el/
;; Package-Version: 0.1.5
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

;; This is a simple minor mode which provides `python-mode' with extra
;; features such as better virtualenv supports.

;; The goal of v0.1 is to support Pipenv virtualenvs.  The minor mode
;; will create inferior Python processes dedicated to Pipenv virtual
;; environments on your system.

;; The `python-mode' has two types of inferior python processes:
;; global and buffer-dedicated.  Either of these can't nicely work
;; with (Pipenv) virtual environments as is.

;; The `npy-mode' extends `python-mode', introducing
;; virtualenv-dedicated and virtualenv-buffer-dedicated inferior
;; Python processes.  You can, for example, send a function definition
;; by `python-shell-send-defun' to a single virtualenv-dedicated
;; inferior Python process from multiple `python-mode' buffers which
;; are visiting Python files under the same Pipenv project, even when
;; you have spawned multiple inferior Python processes for different
;; virtual environments simultaneously.

;; The main entry points are `npy-run-python', which spawns new
;; inferior python process with access to the virtualenv associated
;; with the file the current buffer is visiting, and `npy-scratch',
;; which spawns new python scratch buffer which can, along with the
;; original `python-mode' buffer, interact with inferior python
;; buffers spawned by `npy-run-python'.

;; The virtualenv-dedicated and virtualenv-buffer-dedicated buffers
;; follows the rules below.

;; Rules:

;; 1. A python-mode buffer visiting a file in a Pipenv project has
;; npy-child-dedicatable-to set to itself.

;; 2. If npy-child-dedicatable-to is set, the buffer can spawn any
;; virtualenv-buffer-dedicated buffers, which inherit the
;; npy-child-dedicatable-to of the parent.

;; 3. If npy-child-dedicatable-to is set, the buffer can spawn any
;; virtualenv-dedicated buffers, but the npy-child-dedicatable-to of
;; the spawned buffers will be set to nil in spawned buffers.

;; 4. If npy-child-dedicatable-to is nil, the buffer can not spawn any
;; virtualenv-dedicated buffers but can spawn any virtualenv-dedicated
;; buffers.

;; 5. When a virtualenv-buffer-dedicated buffer is spawned from a
;; python-mode buffer, its npy-dedicated-to is set to that of
;; python-mode buffer.

;; 6. When a virtualenv-buffer-dedicated buffer is spawned from a
;; virtualenv-buffer-dedicated buffer, its npy-dedicated-to is set to
;; npy-child-dedicatable-to of the parent buffer.

;; 7. If the buffer to be spawned already exists and alive,
;; pop-to-buffer it.

;; 8. If the buffer to be spawned already exists but killed, raise an
;; error.

;; 9. If the buffer npy-child-dedicatable-to points is already killed
;; when spawning a virtualenv-buffer-dedicated buffer, raise an error.

;; 10. The precedence list of a python-mode buffer visiting a file in
;; a Pipenv project is: virtualenv-buffer-dedicated,
;; virtualenv-dedicated, dedicated, global.

;; 11. The precedence list of a virtualenv-dedicated buffer is:
;; virtualenv-dedicated, dedicated, global.

;; 12. The precedence list of a virtualenv-buffer-dedicated buffer is:
;; virtualenv-buffer-dedicated, virtualenv-dedicated, dedicated,
;; global.

;; 13. If the buffer to send a string exists but killed, don't raise
;; an error, just move down the precedence list.

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
(require 'gpc)
(require 'f)
(require 'nalist)
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

;;; A variable for the mode line.

(defvar-local npy--mode-line npy-mode-line-prefix)

;;; Pipenv project and virtualenv core variables and their access functions.

(defun npy-pipenv-project-root-fetcher ()
  "Fetch the Pipenv project root path if exists."
  (if (eq npy-pipenv-project-detection 'exploring)
      (let* ((buffer-filename (buffer-file-name (current-buffer)))
             (dirname (cond (buffer-filename (f-dirname buffer-filename))
                            (default-directory default-directory)
                            (t nil))))
        (if dirname
            (let ((root (npy--find-pipenv-project-root-by-exploring dirname)))
              (if root root 'no-virtualenv))
          'no-virtualenv))
    (let ((pipenv-res (s-chomp (shell-command-to-string "pipenv --where"))))
      (cond ((and (stringp pipenv-res) (f-directory-p pipenv-res)) pipenv-res)
            ((stringp pipenv-res) 'no-virtualenv)
            (t 'ERR)))))

(defun npy-pipenv-project-name-fetcher ()
  "Fetch the Pipenv project name if exists."
  (let ((root (gpc-get 'pipenv-project-root npy-env)))
    ;; FIXME: Need to implement gpc priority to eusure the
    ;; syncronization of cache entities when they refer other entities
    ;; in their fetchers.
    (cond ((stringp root) (f-filename root))
          ((eq root 'no-virtualenv) 'no-virtualenv)
          (t 'ERR))))

(defun npy-pipenv-project-name-with-hash-fetcher ()
  "Fetch the Pipenv project name with hash if exists."
  (let ((root (gpc-get 'pipenv-project-root npy-env)))
    ;; FIXME: Need to implement gpc priority to eusure the
    ;; syncronization of cache entities when they refer other entities
    ;; in their fetchers.
    (cond ((stringp root) (npy--pipenv-get-name-with-hash root))
          ((eq root 'no-virtualenv) 'no-virtualenv)
          (t 'ERR))))

(defun npy-pipenv-virtualenv-root-fetcher ()
  "Fetch the Pipenv virtualenv root path if exists."
  (let ((pipenv-res (s-chomp (shell-command-to-string "pipenv --venv"))))
    (cond ((and (stringp pipenv-res) (f-directory-p pipenv-res)) pipenv-res)
          ((stringp pipenv-res) 'no-virtualenv)
          (t 'ERR))))

(gpc-init npy-env
  '((pipenv-project-root nil npy-pipenv-project-root-fetcher)
    (pipenv-project-name nil npy-pipenv-project-name-fetcher)
    (pipenv-project-name-with-hash nil npy-pipenv-project-name-with-hash-fetcher)
    (pipenv-virtualenv-root nil npy-pipenv-virtualenv-root-fetcher)))
(gpc-make-variable-buffer-local npy-env)

(defvar npy-child-dedicatable-to nil
  "The buffer which a virtualenv-buffer-dedicated buffer to be spawned should dedicate to.")
(make-variable-buffer-local 'npy-child-dedicatable-to)

(defvar npy-scratch-buffer nil
  "Non-nil if the current buffer is a scratch buffer.")
(make-variable-buffer-local 'npy-scratch-buffer)

(defvar npy-shell-initialized nil
  "Non-nil means the inferior buffer is already initialized.")
(make-variable-buffer-local 'npy-shell-initialized)

(defvar npy-dedicated-to nil
  "The buffer which this buffer is dedicated to.")
(make-variable-buffer-local 'npy-dedicated-to)

(defun npy--pipenv-get-name-with-hash (path)
  "Return the filename of PATH with a Pipenv hash suffix."
  (f-filename (npy-pipenv-compat-virtualenv-name path)))

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
            (cl-case remaining
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

;;; Functions to find Pipenv information by exploring directory structures.

(defun npy--find-pipenv-project-root-by-exploring (dirname)
  "Return the Pipenv project root if DIRNAME is under a project, otherwise nil."
  (npy--find-pipenv-project-root-by-exploring-impl (f-split (f-full dirname))))

;; FIXME: Should rewrite this as a non-recursive function.
(defun npy--find-pipenv-project-root-by-exploring-impl (dirname-list)
  "Return a Pipenv root if DIRNAME-LIST is under a project, otherwise nil.

DIRNAME-LIST should be the f-split style: e.g. (\"/\" \"usr\" \"local\")."
  (if (null dirname-list)
      nil
    (let ((dirname (apply #'f-join dirname-list)))
      (if (npy--pipenv-root-p dirname)
          dirname
        (npy--find-pipenv-project-root-by-exploring-impl (nbutlast dirname-list 1))))))

(defun npy--pipenv-root-p (dirname)
  "Return t if DIRNAME is a Pipenv project root, otherwise nil."
  (f-exists-p (concat (f-full dirname) "/Pipfile")))

;;; Functions for the integrations with the inferior python mode.

(defun npy-python-shell-get-buffer-advice (orig-fun &rest orig-args)
  "Tweak the buffer entity in ORIG-ARGS.

Replace it with the inferior process for the project exists, otherwise
leave it untouched.  ORIG-FUN should be `python-shell-get-buffer'."
  (let ((project-name (gpc-get 'pipenv-project-name npy-env)))
    (cond ((derived-mode-p 'inferior-python-mode) (current-buffer))
          ((not (stringp project-name)) ; project-name is 'no-virtualenv, 'ERR, or nil.
           (let ((res (apply orig-fun orig-args)))
             res))
          (t (let ((associated-file-path
                    (cond (npy-dedicated-to ; means virtualenv-buffer-dedicated inf-py-buf or scratch-buf.
                           (buffer-file-name npy-dedicated-to))
                          (npy-scratch-buffer nil) ; means virtualenv-dedicated scratch-buf.
                          ((derived-mode-p 'inferior-python-mode) nil) ; means virtualenv-dedicated inf-py-buf.
                          (t (buffer-file-name)))))
               (let* ((venv-buffer-dedicated-process-name)
                      (venv-buffer-dedicated-running)
                      (venv-dedicated-process-name
                       (format "*%s[v:%s]*" python-shell-buffer-name project-name))
                      (venv-dedicated-running
                       (comint-check-proc venv-dedicated-process-name)))
                 (when associated-file-path
                   (setq venv-buffer-dedicated-process-name
                         (format "*%s[v:%s;b:%s]*" python-shell-buffer-name
                                 project-name
                                 (f-filename associated-file-path)))
                   (setq venv-buffer-dedicated-running
                         (comint-check-proc venv-buffer-dedicated-process-name)))
                 (cond (venv-buffer-dedicated-running venv-buffer-dedicated-process-name)
                       (venv-dedicated-running venv-dedicated-process-name)
                       (t (let ((res (apply orig-fun orig-args))) ;; Maybe raising an error is better.
                            res)))))))))

;;; Functions to manage the modeline.

(defun npy-default-mode-line ()
  "Report the Pipenv project name associated with the buffer in the modeline."
  (format "%s[v:%s]"
          npy-mode-line-prefix
          (gpc-get 'pipenv-project-name npy-env)))

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
      (gpc-fetch-all npy-env)
      (npy--update-mode-line))
    res))

(defun npy-find-file-hook-function ()
  "Called by `find-file-hook' when `npy-mode' is on.

This is for the global minor mode version to come."
  (when npy-dynamic-mode-line
    (npy--update-mode-line)))

(defun npy-desktop-save-hook-function ()
  "Disable npy mode before `desktop-mode' saves configurations.

`npy' can significantly slow Emacs startup process, when
`desktop-mode' restores many files.  This is a temporary workaround
and will be removed in the future release when alternative methods
to detect Pipenv virtualenvs implemented.

This is for the global minor mode version to come."
  (npy-mode 0))


;;;
;;; User facing functions and its helpers.
;;;

(defmacro npy--when-valid (var it)
  "Do IT when VAR is valid, otherwise show a warning."
  (declare (indent 1))
  `(cond ((stringp ,var)
          ,it)
         ((eq ,var 'no-virtualenv)
          (if (buffer-file-name)
              (message "No virtualenv has been created for this project yet!")
            (message "No virtualenv got deteced. Maybe because the buffer is not associated with a file.")))
         (t (message "Something wrong has happend in npy."))))

(defmacro npy-initialize ()
  "Initialize npy-mode."
  `(with-eval-after-load "python"
     (add-hook 'python-mode-hook 'npy-mode)))

(defun npy-run-python (&optional dedicated)
  "Run an inferior python process with access to a virtualenv.

When called interactively with `prefix-arg', it spawns a buffer
DEDICATED inferior python process with access to the virtualenv."
  (interactive
   (if current-prefix-arg
       (list t)
     (list nil)))
  (when (and dedicated (not npy-child-dedicatable-to))
    (error "You are not in a buffer associated with a file you can dedicate to"))
  (gpc-fetch-all npy-env)
  (let ((project-name (gpc-val 'pipenv-project-name npy-env)))
    (unless (stringp project-name)
      (error "You are not in a buffer associated with a Pipenv project: project-name is \"%s\"" project-name))
    (let* ((spawning-buffer (current-buffer))
           (maybe-dedicate-to npy-child-dedicatable-to)
           (venv-root (gpc-val 'pipenv-virtualenv-root npy-env))
           (exec-path (cons venv-root exec-path))
           (python-shell-virtualenv-root venv-root)
           (process-name (cond (dedicated (format "%s[v:%s;b:%s]"
                                                  python-shell-buffer-name
                                                  project-name
                                                  (f-filename (buffer-file-name npy-child-dedicatable-to))))
                               (t (format "%s[v:%s]"
                                          python-shell-buffer-name
                                          project-name)))))
      (prog1
          (pop-to-buffer
           (python-shell-make-comint (python-shell-calculate-command)
                                     process-name t))
        (unless npy-shell-initialized
          (when dedicated
            (setq npy-dedicated-to maybe-dedicate-to)
            (setq npy-child-dedicatable-to maybe-dedicate-to))
          (gpc-copy npy-env spawning-buffer (current-buffer))
          (gpc-lock npy-env)
          (setq npy-shell-initialized t))))))

(defun npy-display-pipenv-project-root ()
  "Show the path to the Pipenv project root directory."
  (interactive)
  (npy--update-mode-line)
  (npy--when-valid (gpc-get 'pipenv-project-root npy-env)
    (message "Project: %s" (gpc-get 'pipenv-project-root npy-env))))

(defun npy-update-pipenv-project-root ()
  "Update the Pipenv project root directory."
  (interactive)
  (gpc-get 'pipenv-project-root npy-env)
  (npy-display-pipenv-project-root))

(defun npy-display-pipenv-virtualenv-root ()
  "Show the path to the Pipenv virtualenv root directory."
  (interactive)
  (npy--update-mode-line)
  (npy--when-valid
      (gpc-get 'pipenv-virtualenv-root npy-env)
    (message "Virtualenv: %s" (gpc-get 'pipenv-virtualenv-root npy-env))))

(defun npy-update-pipenv-virtualenv-root ()
  "Show the path to the Pipenv virtualenv root directory."
  (interactive)
  (gpc-fetch 'pipenv-virtualenv-root npy-env)
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

(defun npy-scratch (&optional dedicated)
  "Get a python scratch buffer associated with a virtualenv.

When prefix DEDICATED is set, make the scratch buffer dedicate to
the buffer spawning it."
  (interactive
   (if current-prefix-arg
       (list t)
     (list nil)))
  (let ((project-name (gpc-val 'pipenv-project-name npy-env)))
    (unless (stringp project-name)
      (error "You are not in a buffer associated with a Pipenv project: project-name is \"%s\"" project-name))
    (when (and dedicated (not npy-child-dedicatable-to))
      (error "You are not in a buffer associated with a file you can dedicate to"))
    (let* ((spawning-buffer (current-buffer))
           (maybe-dedicate-to npy-child-dedicatable-to)
           (mode 'python-mode)
           (name (cond (dedicated
                        (format "*pyscratch[v:%s;b:%s]*"
                                project-name
                                (f-filename (buffer-file-name npy-child-dedicatable-to))))
                       (t (format "*pyscratch[v:%s]*" project-name))))
           (scratch-buf (get-buffer name)))
      (if (bufferp scratch-buf)
          (pop-to-buffer scratch-buf)
        (let ((content (when (region-active-p)
                         (buffer-substring-no-properties
                          (region-beginning) (region-end)))))
          (gpc-fetch-all npy-env)
          (setq scratch-buf (get-buffer-create name))
          (pop-to-buffer scratch-buf)
          (funcall mode)
          ;; Any variable assignments before this call in this
          ;; buffer get reset by this call.
          (setq npy-scratch-buffer t)
          (gpc-copy npy-env spawning-buffer scratch-buf)
          (gpc-lock npy-env)
          (npy--update-mode-line)
          (when content (save-excursion (insert content)))
          (if dedicated
              (progn
                (setq npy-dedicated-to maybe-dedicate-to)
                (setq npy-child-dedicatable-to maybe-dedicate-to))
            (setq npy-child-dedicatable-to nil) ; We need this to overwrite the value set by `npy-mode'.
            ))))))

(defun npy-show-python-environment ()
"Show Python environment information."
(interactive)
(message (concat "pipenv-project-root: %s\n"
                 "pipenv-project-name: %s\n"
                 "pipenv-project-name-with-hash: %s\n"
                 "pipenv-virtualenv-root: %s\n"
                 "python-shell-virtualenv-root: %s\n"
                 "npy-scratch-buffer: %s\n"
                 "npy-shell-initialized: %s\n"
                 "npy-dedicated-to: %s\n"
                 "npy-child-dedicatable-to %s\n")
         (gpc-val 'pipenv-project-root npy-env)
         (gpc-val 'pipenv-project-name npy-env)
         (gpc-val 'pipenv-project-name-with-hash npy-env)
         (gpc-val 'pipenv-virtualenv-root npy-env)
         python-shell-virtualenv-root
         npy-scratch-buffer
         npy-shell-initialized
         npy-dedicated-to
         npy-child-dedicatable-to))

;;; Info lookup support (EXPERIMENTAL)

;; Currently this extension is largely based on `pydoc-info' by Jon Waltman.

;;;###autoload
(require 'info-look)

(defvar python-dotty-syntax-table)

(defcustom npy-info-current-symbol-functions
  '(npy-info-current-symbol-python-el current-word)
  "Functions to be called to fetch symbol at point.
Each function is called with no argument and if it returns a
string, that value is used.  If it returns nil, next function
is tried.
Default (fallback to `current-word' when not using python.el):
   '(`npy-info-current-symbol-python-el' `current-word')."
  :group 'npy-info)

(defun npy-info-current-symbol-python-el ()
  "Return current symbol.  Requires python.el."
  (when (featurep 'python)
    (with-syntax-table python-dotty-syntax-table
      (current-word))))

(defun npy-info-python-symbol-at-point ()
  "Return the current Python symbol."
  (cl-loop for func in npy-info-current-symbol-functions
           when (funcall func)
           return it))

;;;###autoload
(defun npy-info-add-help (files &rest more-specs)
  "Add help specifications for a list of Info FILES.

The added specifications are tailored for use with Info files
generated from Sphinx documents.

MORE-SPECS are additional or overriding values passed to
`info-lookup-add-help'."
  (info-lookup-reset)
  (let (doc-spec)
    (dolist (f files)
      (push (list (format "(%s)Python Module Index" f)
                  'npy-info-lookup-transform-entry) doc-spec)
      (push (list (format "(%s)Index" f)
                  'npy-info-lookup-transform-entry) doc-spec))
    (apply 'info-lookup-add-help
           :mode 'python-mode
           :parse-rule 'npy-info-python-symbol-at-point
           :doc-spec doc-spec
           more-specs)))

;;;###autoload
(npy-info-add-help '("python372full"))

(defun npy-info-lookup-transform-entry (entry)
  "Transform a Python index ENTRY to a help item."
  (let* ((py-re "\\([[:alnum:]_.]+\\)(?)?"))
    (cond
     ;; foo.bar --> foo.bar
     ((string-match (concat "\\`" py-re "\\'") entry)
      entry)
     ;; keyword; foo --> foo
     ;; statement; foo --> foo
     ((string-match (concat "\\`\\(keyword\\|statement\\);? " py-re) entry)
      (replace-regexp-in-string " " "." (match-string 2 entry)))
     ;; foo (built-in ...) --> foo
     ((string-match (concat "\\`" py-re " (built-in .+)") entry)
      (replace-regexp-in-string " " "." (match-string 1 entry)))
     ;; foo.bar (module) --> foo.bar
     ((string-match (concat "\\`" py-re " (module)") entry)
      (replace-regexp-in-string " " "." (match-string 1 entry)))
     ;; baz (in module foo.bar) --> foo.bar.baz
     ((string-match (concat "\\`" py-re " (in module \\(.+\\))") entry)
      (replace-regexp-in-string " " "." (concat (match-string 2 entry) " "
                                                (match-string 1 entry))))
     ;; Bar (class in foo.bar) --> foo.bar.Bar
     ((string-match (concat "\\`" py-re " (class in \\(.+\\))") entry)
      (replace-regexp-in-string " " "." (concat (match-string 2 entry) " "
                                                (match-string 1 entry))))
     ;; bar (foo.Foo method) --> foo.Foo.bar
     ((string-match
       (concat "\\`" py-re " (\\(.+\\) \\(method\\|attribute\\))") entry)
      (replace-regexp-in-string " " "." (concat (match-string 2 entry) " "
                                                (match-string 1 entry))))
     ;; foo (C ...) --> foo
     ((string-match (concat "\\`" py-re " (C .*)") entry)
      (match-string 1 entry))
     ;; operator; foo --> foo
     ((string-match "\\`operator; \\(.*\\)" entry)
      (match-string 1 entry))
     ;; Python Enhancement Proposals; PEP XXX --> PEP XXX
     ((string-match "\\`Python Enhancement Proposals; \\(PEP .*\\)" entry)
      (match-string 1 entry))
     ;; RFC; RFC XXX --> RFC XXX
     ((string-match "\\`RFC; \\(RFC .*\\)" entry)
      (match-string 1 entry))
     (t
      entry))))

(defvar-local Info-hide-note-references t)

(defun npy-auto-hide-info-note-references (&rest _args)
  "Advice to automatically hide 'see' in info files generated by sphinx."
  ;; Adapted from http://www.sphinx-doc.org/en/stable/faq.html#displaying-links
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward
             "^ *Generated by \\(Sphinx\\|Docutils\\)"
             (save-excursion (search-forward "\x1f" nil t)) t)
        (setq Info-hide-note-references 'hide)))))

(advice-add 'info-insert-file-contents :after #'npy-auto-hide-info-note-references)

;;; Virtualenv activate automatic functions.

(defun npy-activate-virtualenv-automatic ()
  "Activate and change virtualenvs automatically following buffer positions in the system."
  (interactive)
  (advice-add 'make-process :around #'npy-advise-process-creation)
  (advice-add 'getenv :around #'npy-advice-getenv)
  ;; (advice-add 'find-file-noselect :around 'npy-advise-find-file-noselect)
  )
;; `pytest' and `python-pytest' work fine without the advice to
;; `find-file-noselet'.  So, currently it's disabled.  When we find a
;; mode which requires this advice to work with virtualenvs, we'll
;; uncomment this.

(defun npy-deactivate-virtualenv-automatic ()
  "Deactivate virtualenv automatic."
  (interactive)
  (advice-remove 'make-process  #'npy-advise-process-creation)
  (advice-remove 'getenv #'npy-advice-getenv)
  ;; (advice-remove 'find-file-noselect 'npy-advise-find-file-noselect)
  )

(defun npy-advise-find-file-noselect (orig-fun &rest orig-args)
  "Advise `find-file-noselet' :around with ORIG-FUN and ORIG-ARGS."
  (npy--debug "npy-advise-find-file-noselect called: %s" orig-args)
  (let ((res (apply orig-fun orig-args)))
    (npy--debug "its response: %s" res)
    (npy--debug "its buffer-file-name: %s" (buffer-file-name res))
    (when (and res (buffer-file-name res))
      (with-current-buffer res
        (gpc-get 'pipenv-project-name npy-env)
        (gpc-get 'pipenv-project-name-with-hash npy-env)))
    res))

(defun npy-advise-process-creation (orig-fun &rest orig-args)
  "Tweak `exec-path' and PATH during a `make-process' call.

Currently, ORIG-FUN should be `make-process', and ORIG-ARGS is
the arguments when it's called."
  (npy--debug "npy-advise-process-creation called in %s" (current-buffer))
  (let* ((project-name (gpc-get 'pipenv-project-name npy-env)))
    (npy--debug "project-name: %s" project-name)
    (if (stringp project-name)
        (let* ((venv-root (gpc-get 'pipenv-virtualenv-root npy-env))
               (venv-bin-path (concat venv-root "/bin/"))
               ;; (exec-path (cons venv-bin-path exec-path)) ; `make-process' doesn't need this.
               (orig-path (getenv "PATH")))
          (setenv "PATH" (concat venv-bin-path ":" orig-path))
          (npy--debug "temporary path: %s" (getenv "PATH"))
          (unwind-protect
              (let* ((res (apply orig-fun orig-args))
                     (paths (s-split ":" (getenv "PATH")))
                     (new-paths (cl-remove venv-bin-path paths :test 'equal)))
                (npy--debug "new paths: %s" new-paths)
                (setenv "PATH" (s-join ":" new-paths))
                res)
            (setenv "PATH" orig-path)))
      (let ((res (apply orig-fun orig-args)))
        res))))

(defun npy-advice-getenv (orig-fun &rest orig-args)
  "Append the virtualenv path when possible.

ORIG-FUN should be `getenv' and ORIG-ARGS is the arguments when
it's called."
  (let ((res (apply orig-fun orig-args)))
    (when (equal (car orig-args) "PATH")
      (let* ((venv-root (gpc-get 'pipenv-virtualenv-root npy-env))
             (venv-bin-path (when (stringp venv-root)
                              (concat venv-root "/bin/"))))
        (npy--debug "npy-advice-getenv called: args=%s venv-root %s" orig-args venv-root)
        (when venv-bin-path
          (let* ((path-list (s-split ":" res))
                 (clean-paths (s-join ":" (cl-remove venv-bin-path path-list :test 'equal))))
            (setq res (concat venv-bin-path ":" clean-paths))))))
    res))

(defun npy-dired-mode-hook-function ()
  "Set the name, root, and venv of a Pipenv project."
  (gpc-fetch-all npy-env))

(defun npy-compilation-mode-hook-function ()
  "Set the name, root, and venv of a Pipenv project."
  (gpc-fetch-all npy-env))

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
  "Minor mode to provide extensions to the Python development support in Emacs.

Currently, this mode supports the integration of Pipenv
virtualenvs and Emacs inferior python buffers.  You can spawn
virtualenv-dedicated python inferior python buffers,
virtualenv-buffer-dedicated inferior python buffers,
virtualenv-dedicated python scratch buffers, and
virtualenv-buffer-dedicated python scratch buffers."
  :group 'npy
  :require 'npy
  :lighter npy--mode-line
  :keymap npy-mode-map
  :global nil
  (cond
   (npy-mode
    ;; These hooks are for when using npy as a global minor mode.
    ;;(add-hook 'find-file-hook 'npy-find-file-hook-function)
    ;;(add-hook 'desktop-save-hook 'npy-desktop-save-hook-function)
    (add-hook 'dired-mode-hook 'npy-dired-mode-hook-function)
    (add-hook 'compilation-mode-hook 'npy-compilation-mode-hook-function)
    (advice-add 'python-shell-get-buffer :around #'npy-python-shell-get-buffer-advice)
    (advice-add 'write-file :around #'npy-write-file-advice)
    (npy-update-pipenv-project-root)
    (when (and (derived-mode-p 'python-mode))
      (setq npy-child-dedicatable-to (current-buffer))))
   (t
    ;;(remove-hook 'find-file-hook 'npy-find-file-hook-function)
    ;;(remove-hook 'desktop-save-hook 'npy-desktop-save-hook-function)
    (remove-hook 'dired-mode-hook 'npy-dired-mode-hook-function)
    (remove-hook 'compilation-mode-hook 'npy-compilation-mode-hook-function)
    (advice-remove 'python-shell-get-buffer #'npy-python-shell-get-buffer-advice)
    (advice-remove 'write-file #'npy-write-file-advice))))

(provide 'npy)

;;; npy.el ends here
