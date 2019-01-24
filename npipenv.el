;;; npipenv.el --- A nano support for Pipenv virtual environments  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 by Cyriakus "Mukuge" Hill

;; Author: Cyriakus "Mukuge" Hill <cyriakus.h@gmail.com>
;; Keywords: tools, processes
;; URL: https://github.com/mukuge/nPipenv/
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))

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

;; This is a simple minor mode which will create inferior Python
;; processes dedicated to Pipenv virtual environments on your system.

;; The `python-mode' has two types of inferior python processes:
;; global and buffer-dedicated.  Either of these can't nicely work
;; with (Pipenv) virtual environments as is.

;; The `nPipenv-mode' extends `python-mode', introducing
;; virtulaenv-dedicated infeior Python processes.  You can, for
;; example, send a function definition by `python-shell-send-defun' to
;; a single virtualenv-dedicated inferior Python process from multiple
;; Python files under a same virtual environmet, even when you have
;; spawned multiple inferior Python processes for different
;; virtual environments simultaneously.

;; The main entry points are `npipenv-run-python', which spanws new
;; inferior python process with the virtualenv at the current buffer.

;; Installation:

;; Place this file on a directory in your `load-path', and explicitly
;; require it, and call the initialization function.
;;
;;     (require 'npipenv)
;;     (npipenv-initialize)
;;

;;; Code:

(require 'cl-lib)
(require 'f)
(require 'python)
(require 's)
(require 'subr-x)

(defgroup npipenv nil
  "nano support for Pipenv venvs"
  :prefix "npipenv-"
  :group 'python)

;; User customization

(defcustom npipenv-pipenv-executable
  "pipenv"
  "The name of the Pipenv executable."
  :type '(file :must-match t)
  :safe #'file-directory-p
  :group 'npipenv)

(defcustom npipenv-command-process-name
  "nPipenv"
  "The name of processes for calling the pipenv executable."
  :type 'string
  :group 'npipenv)

(defcustom npipenv-command-process-buffer-name
  "*nPipenv*"
  "The name of process buffers for calling the pipenv executable."
  :type 'string
  :group 'npipenv)

(defcustom npipenv-python-shell-buffer-name
  "*Pipenv shell*"
  "The name of python shell buffers which run on Pipenv virtualenvs."
  :type 'string
  :group 'npipenv)

(defcustom npipenv-shell-mode-buffer-init-command
  "exec pipenv shell"
  "The shell command to launnch a python interactive mode on a virtualenv."
  :type 'string
  :group 'npipenv)

(defcustom npipenv-mode-line-prefix
  " nP"
  "Mode line lighter prefix for nPipenv.
It's used by `nPipenv-default-mode-line' when using dynamic mode line
lighter and is the only thing shown in the mode line otherwise."
  :group 'npipenv
  :type 'string)

(defcustom npipenv-no-virtualenv-mark "-"
  "The mark shown on the modeline when the buffer is outside any virtualenvs."
  :group 'npipev
  :type 'string)

(defcustom npipenv-mode-line-function 'npipenv-default-mode-line
  "The function to use to generate project-specific mode-line.
The default function adds the project name to the mode-line."
  :group 'npipenv
  :type 'function)

(defcustom npipenv-dynamic-mode-line t
  "If true, update the mode-line dynamically.
Only file buffers are affected by this, as the update happens via
`find-file-hook'."
  :group 'npipenv
  :type 'boolean)

(defcustom npipenv-dynamic-mode-line-in-dired-mode t
  "If true, update the mode-line dynamically in `dired-mode'.

Only file buffers are affected by this, as the update happens via
`dired-mode-hook'."
  :group 'npipenv
  :type 'boolean)

(defcustom npipenv-keymap-prefix "\C-c'"
  "The nPipenv keymap prefix."
  :group 'npipev
  :type 'string)

;; Internal code

(defvar npipenv--debug nil
  "Display debug info when non-nil.")

(defvar-local npipenv--pipenv-virtualenv-root nil
  "The root directory of the Pipenv virtualenv.")

(defvar-local npipenv--pipenv-project-name nil
  "The Pipenv project name or the name of the enclosing directory.")

(defvar-local npipenv--mode-line npipenv-mode-line-prefix)

(defun npipenv--debug (msg &rest args)
  "Print MSG and ARGS like `message', but only if debug output is enabled."
  (when npipenv--debug
    (apply #'message msg args)))

(defun npipenv--update-mode-line ()
  "Update the nPipenv mode-line."
  (let ((mode-line (funcall npipenv-mode-line-function)))
    (setq npipenv--mode-line mode-line))
  (force-mode-line-update))

(defun npipenv--clean-response (response)
  "Clean up RESPONSE from shell command."
  (s-chomp response))

(defun npipenv--force-wait (process)
  "Block while PROCESS exits and until the venv root gets a value successfully."
  (let ((counter 0))
    (while (and (process-live-p process)
                (< counter 10))
      (sit-for 0.1 t)
      (cl-incf counter))
    (if (>= counter 10)
        (npipenv--debug "The npipenv--force-wait limit has reached."))))

(defun npipenv--make-pipenv-process (command &optional filter sentinel)
  "Make a Pipenv process from COMMAND; optional custom FILTER or SENTINEL."
  (make-process
   :name npipenv-command-process-name
   :buffer npipenv-command-process-buffer-name
   :command command
   :coding 'utf-8-unix
   :filter filter
   :sentinel sentinel))

(defun npipenv--process-filter(process response)
  "PROCESS filter to set the variables based on RESPONSE.

The variables are `npipenv--pipenv-virtualenv-root' and
`npipenv--pipenv-project-name'."
  (npipenv--debug "response: %s" response)
  (npipenv--debug "matched responce: %s" (s-match "\\(/.*\\)
" response))
  (when (s-equals? (nth 0 (last (process-command process))) "--venv")
    (if-let* ((res (s-match "\\(/.*\\)
" response))
              (path (nth 1 res))
              (f-directory? path))
        (setq npipenv--pipenv-virtualenv-root path
              npipenv--pipenv-project-name (npipenv--venvpath-to-prjname path)))
    (if (s-match "No virtualenv has been created for this project yet!" response)
        (setq npipenv--pipenv-virtualenv-root 'no-virtualenv
              npipenv--pipenv-project-name 'no-virtualenv))))

(defun npipenv--get-venv-root ()
  "Call pipenv with '--venv' to get the path to the virtualenv root."
  (let ((command (list npipenv-pipenv-executable "--venv"))
        (filter 'npipenv--process-filter))
    (npipenv--make-pipenv-process command filter)))

(defun npipenv--fill-venv-root ()
  "Fill `npipenv--pipenv-virtualenv-root' when it's non-nill, by calling `npipenv--get-venv-root'."
  (if (null npipenv--pipenv-virtualenv-root)
      (npipenv--force-wait (npipenv--get-venv-root))
    npipenv--pipenv-virtualenv-root))

(defun npipenv--venvpath-to-prjname (venvpath)
  "Extract a project name from VENVPATH or a virtualenv path."
  (nth 1 (s-match "/\\([^/]*\\)-........$" venvpath)))

(defun npipenv-python-shell-get-buffer-advice (orig-fun &rest orig-args)
  "Replace the buffer in ORIG-ARGS if its inferior python mode exists.

ORIG-FUN must be `python-shell-get-buffer'."
  (npipenv--fill-venv-root)
  (if (stringp npipenv--pipenv-project-name) ;; i.e. it's not 'no-virtualenv
      (let* ((venv-buffer-name (format "*%s[v:%s]*" python-shell-buffer-name
                                       npipenv--pipenv-project-name))
             (venv-running (comint-check-proc venv-buffer-name)))
        (if venv-running
            venv-buffer-name
          (let ((res (apply orig-fun orig-args))) ;; Maybe raising an error is better.
            res)))
    (let ((res (apply orig-fun orig-args)))
      res)))

(defun npipenv-default-mode-line ()
  "Report the Pipenv project name associated with the buffer in the modeline."
  (npipenv--fill-venv-root)
  (format "%s[v:%s]"
          npipenv-mode-line-prefix
          (cond ((stringp npipenv--pipenv-virtualenv-root) npipenv--pipenv-project-name)
                ((eq npipenv--pipenv-virtualenv-root 'no-virtualenv) npipenv-no-virtualenv-mark)
                (t "ERR"))))

(defun npipenv-write-file-advice (orig-fun &rest orig-args)
  "Update two variables when `write-file' (ORIG-FUN with ORIG-ARGS) is called."
  (let ((res (apply orig-fun orig-args)))
    (when (bound-and-true-p npipenv-mode)
      (npipenv--force-wait (npipenv--get-venv-root))
      (npipenv--update-mode-line))
    res))

(defun npipenv-find-file-hook-function ()
  "Called by `find-file-hook' when `npipenv-mode' is on."
  (when npipenv-dynamic-mode-line
    (npipenv--update-mode-line)))

(defun npipenv-dired-mode-hook-function ()
  "Get the names of the Pipenv virtulenv root and project, and update the mode line."
  (when npipenv-dynamic-mode-line-in-dired-mode
    (npipenv--force-wait (npipenv--get-venv-root))
    (npipenv--update-mode-line)))

(defun npipenv-desktop-save-hook-function ()
  "Disable nPipenv before `desktop-mode' saves configurations.

nPipenv can significantly slow Emacs startup process, when
`desktop-mode' restores many files.  This is a temporaly workaroud
and will be removed in the future release when alternative methods
to detect Pipenv virtualenvs implemented."
  (npipenv-mode 0))

(defmacro npipenv--do-it-or-message (it)
  "Execute IT if the virtualenv exists, otherwise show an appropriate message."
  `(cond ((stringp npipenv--pipenv-virtualenv-root)
          ,it)
         ((eq npipenv--pipenv-virtualenv-root 'no-virtualenv)
          (if (buffer-file-name)
              (message "No virtualenv has been created for this project yet!")
            (message "No virtualenv got deteced. Maybe because the buffer is not associated with a file.")))
         (t (message "Something wrong has happend in nPipenv."))))

;; User facing functions

(defun npipenv-initialize ()
  "Initialize nPipenv."
  (with-eval-after-load "python"
    (add-hook 'python-mode-hook 'npipenv-mode)))

(defun npipenv-run-python ()
  "Run an inferior python shell on a virtulaenv."
  (interactive)
  (npipenv--fill-venv-root)
  (npipenv--do-it-or-message
   (let* ((exec-path (cons npipenv--pipenv-virtualenv-root exec-path))
          (python-shell-virtualenv-root npipenv--pipenv-virtualenv-root)
          (process-name (format "%s[v:%s]" python-shell-buffer-name
                                npipenv--pipenv-project-name)))
     (get-buffer-process
      (python-shell-make-comint (python-shell-calculate-command)
                                process-name t)))))

(defun npipenv-display-venv-root ()
  "Show path to the Pipenv project virtualenv root directory."
  (interactive)
  (npipenv--fill-venv-root)
  (npipenv--update-mode-line)
  (npipenv--do-it-or-message
   (message "Virtualenv: %s" npipenv--pipenv-virtualenv-root)))

(defun npipenv-update-venv-root ()
  "Update the Pipenv project virtualenv root directory."
  (interactive)
  (npipenv--force-wait (npipenv--get-venv-root))
  (npipenv--update-mode-line)
  (npipenv--do-it-or-message
   (message "Virtualenv: %s" npipenv--pipenv-virtualenv-root)))

(defun npipenv-shell ()
  "Spawn a shell-mode shell and activate a Pipenv virtualenv."
  (interactive)
  (let ((name (generate-new-buffer-name npipenv-python-shell-buffer-name)))
    (pop-to-buffer name)
    (shell (current-buffer))
    (insert npipenv-shell-mode-buffer-init-command)
    (setq-local comint-process-echoes t)
    (comint-send-input)
    (comint-clear-buffer)))

(defvar npipenv-command-map
  (let ((map (make-sparse-keymap)))
    ;; Shell interaction
    (define-key map "p" #'npipenv-run-python)
    (define-key map "s" #'npipenv-shell)
    ;; Some util commands
    (define-key map "d" #'npipenv-display-venv-root)
    (define-key map "u" #'npipenv-update-venv-root)
    map)
  "Keymap for nPipenv commands after `npipenv-keymap-prefix'.")
(fset 'npipenv-command-map npipenv-command-map)

(defvar npipenv-mode-map
  (let ((map (make-sparse-keymap)))
    (when npipenv-keymap-prefix
      (define-key map npipenv-keymap-prefix 'npipenv-command-map))
    map)
  "Keymap for nPipenv mode.")

;;;###autoload
(define-minor-mode npipenv-mode
  "Minor mode for Pipenv"
  :group 'npipenv
  :require 'npipenv
  :lighter npipenv--mode-line
  :keymap npipenv-mode-map
  :global nil
  (cond
   (npipenv-mode
    ;; These hooks are for when using nPipenv as a global minor mode.
    ;;(add-hook 'find-file-hook 'npipenv-find-file-hook-function)
    ;;(add-hook 'dired-mode-hook 'npipenv-dired-mode-hook-function)
    ;;(add-hook 'desktop-save-hook 'npipenv-desktop-save-hook-function)
    (advice-add 'python-shell-get-buffer :around #'npipenv-python-shell-get-buffer-advice)
    (advice-add 'write-file :around #'npipenv-write-file-advice))
   (t
    ;;(remove-hook 'find-file-hook 'npipenv-find-file-hook-function)
    ;;(remove-hook 'dired-mode-hook 'npipenv-dired-mode-hook-function)
    ;;(remove-hook 'desktop-save-hook 'npipenv-desktop-save-hook-function)
    (advice-remove 'python-shell-get-buffer #'npipenv-python-shell-get-buffer-advice)
    (advice-remove 'write-file #'npipenv-write-file-advice))))

(provide 'npipenv)

;;; npipenv.el ends here
