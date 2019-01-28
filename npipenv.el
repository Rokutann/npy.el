;;; npipenv.el --- A nano support for Pipenv virtual environments  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 by Cyriakus "Mukuge" Hill

;; Author: Cyriakus "Mukuge" Hill <cyriakus.h@gmail.com>
;; Keywords: tools, processes
;; URL: https://github.com/mukuge/nPipenv/
;; Package-Version: 0.1.1
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

;; This is a simple minor mode which will create inferior Python
;; processes dedicated to Pipenv virtual environments on your system.

;; The `python-mode' has two types of inferior python processes:
;; global and buffer-dedicated.  Either of these can't nicely work
;; with (Pipenv) virtual environments as is.

;; The `nPipenv-mode' extends `python-mode', introducing
;; virtulaenv-dedicated inferior Python processes.  You can, for
;; example, send a function definition by `python-shell-send-defun' to
;; a single virtualenv-dedicated inferior Python process from multiple
;; Python files under a same virtual environment, even when you have
;; spawned multiple inferior Python processes for different
;; virtual environments simultaneously.

;; The main entry points are `npipenv-run-python', which spawns new
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
  "Nano support for Pipenv virtualenvs."
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
  "The name of a python shell buffer which has access to a Pipenv virtualenv."
  :type 'string
  :group 'npipenv)

(defcustom npipenv-shell-mode-buffer-init-command
  "exec pipenv shell"
  "The shell command to launch a python interactive mode for a virtualenv."
  :type 'string
  :group 'npipenv)

(defcustom npipenv-mode-line-prefix
  " nP"
  "Mode line lighter prefix for nPipenv.

It's used by `nPipenv-default-mode-line' when using dynamic mode line
lighter and is the only thing shown in the mode line otherwise."
  :group 'npipenv
  :type 'string)

(defcustom npipenv-no-virtualenv-mark
  "-"
  "The mark shown on the modeline when the buffer is outside any virtualenvs."
  :group 'npipev
  :type 'string)

(defcustom npipenv-mode-line-function
  'npipenv-default-mode-line
  "The function to be used to generate project-specific mode-line.

The default function adds the project name to the mode-line."
  :group 'npipenv
  :type 'function)

(defcustom npipenv-dynamic-mode-line
  t
  "Update the mode-line dynamically if true.

This is for the global minor mode version to come."
  :group 'npipenv
  :type 'boolean)

(defcustom npipenv-dynamic-mode-line-in-dired-mode
  t
  "Update the mode-line dynamically in `dired-mode' if true.

This is for the global minor mode version to come."
  :group 'npipenv
  :type 'boolean)

(defcustom npipenv-keymap-prefix
  "\C-c'"
  "The nPipenv keymap prefix."
  :group 'npipenv
  :type 'string)

(defcustom npipenv-pipenv-project-detection
  'exploring
  "The Pipenv project detection method in use.

The value should be 'exploring (default), or 'calling."
  :group 'npipenv
  :type 'symbol)

;; Internal code

(defvar npipenv--debug nil
  "Display debug info when non-nil.")

(defvar-local npipenv--pipenv-project-root nil
  "The root directory of the Pipenv project.")

(defvar-local npipenv--pipenv-project-name nil
  "The Pipenv project name or the name of the directory where Pipfile exists.")

(defvar-local npipenv--pipenv-virtualenv-root nil
  "The virtualenv root directory of the Pipenv project.")

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
  "Block while PROCESS exits with one sec timeout."
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

(defun npipenv--process-filter-for-where(process response)
  "PROCESS filter for '--where' to set the variables based on RESPONSE."
  (npipenv--debug "response: %s" response)
  (npipenv--debug "matched responce: %s" (s-match "\\(/.*\\)
" response))
  (when (s-equals? (nth 0 (last (process-command process))) "--venv")
    (if-let* ((res (s-match "\\(/.*\\)
" response))
              (path (nth 1 res))
              (f-directory? path))
        (setq npipenv--pipenv-virtualenv-root path
              npipenv--pipenv-project-name (npipenv--venvpath-to-prjname path))
      (if (s-match "No virtualenv has been created for this project yet!" response)
          (setq npipenv--pipenv-virtualenv-root 'no-virtualenv
                npipenv--pipenv-project-name 'no-virtualenv)))))

(defun npipenv--process-filter-for-venv(process response)
  "PROCESS filter for '--venv' to set the variables based on RESPONSE."
  (npipenv--debug "response: %s" response)
  (npipenv--debug "matched responce: %s" (s-match "\\(/.*\\)
" response))
  (when (s-equals? (nth 0 (last (process-command process))) "--where")
    (if-let* ((res (s-match "\\(/.*\\)
" response))
              (path (nth 1 res))
              (f-directory? path))
        (setq npipenv--pipenv-project-root path
              npipenv--pipenv-project-name (f-filename path)))
    (if (s-match "No Pipfile at project home." response)
        (setq npipenv--pipenv-project-root 'no-virtualenv
              npipenv--pipenv-project-name 'no-virtualenv))))

(defun npipenv--get-pipenv-virtualenv-root ()
  "Call pipenv with '--venv' to get the path to the virtualenv root."
  (let ((command (list npipenv-pipenv-executable "--venv"))
        (filter 'npipenv--process-filter-for-where))
    (npipenv--make-pipenv-process command filter)))

(defun npipenv--get-pipenv-project-root ()
  "Call pipenv with '--where' to get the Pipenv project root."
  (let ((command (list npipenv-pipenv-executable "--where"))
        (filter 'npipenv--process-filter-for-venv))
    (npipenv--make-pipenv-process command filter)))

(defun npipenv--set-pipenv-project-root-by-calling ()
  "Set the Pipenv project root variable by calling the pipenv executable."
  (if (null npipenv--pipenv-project-root)
      (npipenv--force-wait (npipenv--get-pipenv-project-root))
    npipenv--pipenv-project-root))

(defun npipenv--set-pipenv-virtualenv-root-by-calling ()
  "Set the Pipenv virtualenv root variable by calling the pipenv executable."
  (if (null npipenv--pipenv-virtualenv-root)
      (npipenv--force-wait (npipenv--get-pipenv-virtualenv-root))
    npipenv--pipenv-virtualenv-root))

(defun npipenv--fill-pipenv-project-root ()
  "Fill `npipenv--pipenv-project-root' if it's non-nil."
  (if (eql npipenv-pipenv-project-detection 'exploring)
      (npipenv--set-pipenv-project-root-by-exploring)
    (npipenv--set-pipenv-project-root-by-calling)))

(defun npipenv--fill-pipenv-virtualenv-root ()
  "Fill `npipenv--pipenv-virtualenv-root' if it's non-nil."
  (npipenv--set-pipenv-virtualenv-root-by-calling))

(defun npipenv--venvpath-to-prjname (venvpath)
  "Extract a project name from VENVPATH or a virtualenv path."
  (nth 1 (s-match "/\\([^/]*\\)-........$" venvpath)))

(defun npipenv--venvpath-to-prjroot (venvpath)
  "Extract a project root from VENVPATH or a virtualenv path."
  (nth 1 (s-match "\\(/.*\\)-........$" venvpath)))

(defun npipenv-python-shell-get-buffer-advice (orig-fun &rest orig-args)
  "Tweak the buffer entity in ORIG-ARGS.

Replace it with the inferior process for the project exists, otherwise
leave it untouched.  ORIG-FUN should be `python-shell-get-buffer'."
  (npipenv--fill-pipenv-project-root)
  (if (stringp npipenv--pipenv-project-name) ;; i.e. it's not 'no-virtualenv
      (let* ((venv-dedicated-buffer-name (format "*%s[v:%s]*" python-shell-buffer-name
                                                 npipenv--pipenv-project-name))
             (venv-dedicated-running (comint-check-proc venv-dedicated-buffer-name)))
        (if venv-dedicated-running
            venv-dedicated-buffer-name
          (let ((res (apply orig-fun orig-args))) ;; Maybe raising an error is better.
            res)))
    (let ((res (apply orig-fun orig-args)))
      res)))

(defun npipenv-default-mode-line ()
  "Report the Pipenv project name associated with the buffer in the modeline."
  (npipenv--fill-pipenv-project-root)
  (format "%s[v:%s]"
          npipenv-mode-line-prefix
          (cond ((stringp npipenv--pipenv-project-root) npipenv--pipenv-project-name)
                ((eq npipenv--pipenv-project-root 'no-virtualenv) npipenv-no-virtualenv-mark)
                (t "ERR"))))

(defun npipenv-write-file-advice (orig-fun &rest orig-args)
  "Update two variables when `write-file' (ORIG-FUN with ORIG-ARGS) is called.

The two variables are: `npipenv--pipenv-project-root' and `npipenv--pipenv-project-name'"
  (let ((res (apply orig-fun orig-args)))
    (when (bound-and-true-p npipenv-mode)
      (npipenv--force-wait (npipenv--get-pipenv-project-root))
      (npipenv--update-mode-line))
    res))

(defun npipenv-find-file-hook-function ()
  "Called by `find-file-hook' when `npipenv-mode' is on.

This is for the global minor mode version to come."
  (when npipenv-dynamic-mode-line
    (npipenv--update-mode-line)))

(defun npipenv-dired-mode-hook-function ()
  "Get the name and root of a Pipenv project, and update the mode line.

This is for the global minor mode version to come."
  (when npipenv-dynamic-mode-line-in-dired-mode
    (npipenv--force-wait (npipenv--get-pipenv-project-root))
    (npipenv--update-mode-line)))

(defun npipenv-desktop-save-hook-function ()
  "Disable nPipenv before `desktop-mode' saves configurations.

nPipenv can significantly slow Emacs startup process, when
`desktop-mode' restores many files.  This is a temporary workaround
and will be removed in the future release when alternative methods
to detect Pipenv virtualenvs implemented.

This is for the global minor mode version to come."
  (npipenv-mode 0))

(defmacro npipenv--do-it-or-message (var it)
  "Check VAR and execute IT if the virtualenv exists, otherwise show a message."
  `(cond ((stringp ,var)
          ,it)
         ((eq ,var 'no-virtualenv)
          (if (buffer-file-name)
              (message "No virtualenv has been created for this project yet!")
            (message "No virtualenv got deteced. Maybe because the buffer is not associated with a file.")))
         (t (message "Something wrong has happend in nPipenv."))))

(defun npipenv--set-pipenv-project-root-by-exploring ()
  "Set the Pipenv project root variable by exploring the directory bottom-up."
  (if-let* ((filename (buffer-file-name (current-buffer)))
            (dirname (f-dirname filename))
            (root (npipenv--get-pipenv-project-root-by-exploring dirname)))
      (setq npipenv--pipenv-project-root root)
    (setq npipenv--pipenv-project-root 'no-virtualenv)))

(defun npipenv--get-pipenv-project-root-by-exploring (dirname)
  "Return the Pipenv project root if DIRNAME is under a project, otherwise nil."
  (npipenv--get-pipenv-project-root-by-exploring-impl (f-split (f-full dirname))))

(defun npipenv--get-pipenv-project-root-by-exploring-impl (dirname-list)
  "Return a Pipenv root if DIRNAME-LIST is under a project, otherwise nil.

DIRNAME-LIST should be the f-split style: e.g. (\"/\" \"usr\" \"local\")."
  (if (null dirname-list)
      nil
    (let ((dirname (apply #'f-join dirname-list)))
      (if (npipenv--is-pipenv-root? dirname)
          dirname
        (npipenv--get-pipenv-project-root-by-exploring-impl (nbutlast dirname-list 1))))))

(defun npipenv--is-pipenv-root? (dirname)
  "Return t if DIRNAME is a Pipenv project root, otherwise nil."
  (f-exists? (concat (f-full dirname) "/Pipfile")))

;; User facing functions

(defun npipenv-initialize ()
  "Initialize nPipenv."
  (with-eval-after-load "python"
    (add-hook 'python-mode-hook 'npipenv-mode)))

(defun npipenv-run-python ()
  "Run an inferior python shell for a virtualenv."
  (interactive)
  (npipenv--fill-pipenv-project-root)
  (npipenv--do-it-or-message
   npipenv--pipenv-project-root
   (let* ((exec-path (cons npipenv--pipenv-project-root exec-path))
          (python-shell-virtualenv-root npipenv--pipenv-project-root)
          (process-name (format "%s[v:%s]" python-shell-buffer-name
                                npipenv--pipenv-project-name)))
     (get-buffer-process
      (python-shell-make-comint (python-shell-calculate-command)
                                process-name t)))))

(defun npipenv-display-pipenv-project-root ()
  "Show the path to the Pipenv project root directory."
  (interactive)
  (npipenv--fill-pipenv-project-root)
  (npipenv--update-mode-line)
  (npipenv--do-it-or-message
   npipenv--pipenv-project-root
   (message "Project: %s" npipenv--pipenv-project-root)))

(defun npipenv-update-pipenv-project-root ()
  "Update the Pipenv project root directory."
  (interactive)
  (npipenv--force-wait (npipenv--get-pipenv-project-root))
  (npipenv--update-mode-line)
  (npipenv--do-it-or-message
   npipenv--pipenv-project-root
   (message "Project: %s" npipenv--pipenv-project-root)))

(defun npipenv-display-pipenv-virtualenv-root ()
  "Show the path to the Pipenv virtualenv root directory."
  (interactive)
  (npipenv--fill-pipenv-virtualenv-root)
  (npipenv--update-mode-line)
  (npipenv--do-it-or-message
   npipenv--pipenv-virtualenv-root
   (message "Virtualenv: %s" npipenv--pipenv-virtualenv-root)))

(defun npipenv-pipenv-shell ()
  "Spawn a shell-mode shell and invoke a Pipenv shell."
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
    (define-key map "s" #'npipenv-pipenv-shell)
    ;; Some util commands
    (define-key map "d" #'npipenv-display-pipenv-project-root)
    (define-key map "u" #'npipenv-update-pipenv-project-root)
    (define-key map "v" #'npipenv-display-pipenv-virtualenv-root)
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
    (advice-add 'write-file :around #'npipenv-write-file-advice)
    (npipenv-update-pipenv-project-root))
   (t
    ;;(remove-hook 'find-file-hook 'npipenv-find-file-hook-function)
    ;;(remove-hook 'dired-mode-hook 'npipenv-dired-mode-hook-function)
    ;;(remove-hook 'desktop-save-hook 'npipenv-desktop-save-hook-function)
    (advice-remove 'python-shell-get-buffer #'npipenv-python-shell-get-buffer-advice)
    (advice-remove 'write-file #'npipenv-write-file-advice))))

(provide 'npipenv)

;;; npipenv.el ends here
