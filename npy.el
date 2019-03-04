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

;; Installation:

;; Place this file on a directory in your `load-path', and explicitly
;; require it, and call the initialization function.
;;
;;     (require 'npy)
;;     (npy-initialize)
;;

;;; Code:

(require 'cl-lib)
(require 'gpc)
(require 'f)
(require 'info-look)
(require 'nalist)
(require 'python)
(require 's)
(require 'subr-x)

(defgroup npy nil
  "Nano support for Pipenv virtualenvs."
  :prefix "npy-"
  :group 'python)


;;; Variables.
(defcustom npy-keymap-prefix
  "\C-c'"
  "The npy keymap prefix."
  :group 'npy
  :type 'string)

(defcustom npy-pipenv-executable
  "pipenv"
  "The name of the Pipenv executable."
  :type '(file :must-match t)
  :safe #'file-directory-p
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

It's used by `npy-mode-line-report' when using dynamic mode line
lighter and is the only thing shown in the mode line otherwise."
  :group 'npy
  :type 'string)

(defcustom npy-no-virtualenv-mark
  "-"
  "The mark shown on the modeline when the buffer is outside any virtualenvs."
  :group 'npy
  :type 'string)

(defcustom npy-mode-line-function
  'npy-mode-line-report
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

(defvar-local npy-mode-line npy-mode-line-prefix)


;;; npy--debug: a debug facility.
(defvar npy--debug nil
  "Display debug info when non-nil.")

(defun npy--debug (msg &rest args)
  "Print MSG and ARGS like `message', but only if debug output is enabled."
  (when npy--debug
    (apply #'message msg args)))


;;; npy-env: a gpc cache for Python envrionemt information.
(defun npy-env-pipenv-project-root-fetcher ()
  "Fetch the Pipenv project root path if exists."
  (let* ((buffer-filename (buffer-file-name (current-buffer)))
         (dirname (cond (buffer-filename (f-dirname buffer-filename))
                        (default-directory default-directory)
                        (t nil))))
    (if dirname
        (let ((root (npy-env-find-pipenv-project-root-by-exploring dirname)))
          (if root root 'no-virtualenv))
      'no-virtualenv))) ;; THINKME: Using symbols for marking is a good idea?

(defun npy-env-pipenv-project-name-fetcher ()
  "Fetch the Pipenv project name if exists."
  (let ((root (gpc-get 'pipenv-project-root npy-env)))
    ;; FIXME: Need to implement gpc priority to eusure the
    ;; syncronization of cache entities when they refer other entities
    ;; in their fetchers.
    (cond ((stringp root) (f-filename root))
          ((eq root 'no-virtualenv) 'no-virtualenv)
          (t 'ERR))))

(defun npy-env-pipenv-project-name-with-hash-fetcher ()
  "Fetch the Pipenv project name with hash if exists."
  (let ((root (gpc-get 'pipenv-project-root npy-env)))
    ;; FIXME: Need to implement gpc priority to eusure the
    ;; syncronization of cache entities when they refer other entities
    ;; in their fetchers.
    (cond ((stringp root) (npy-env-make-pipenv-proect-name-with-hash root))
          ((eq root 'no-virtualenv) 'no-virtualenv)
          (t 'ERR))))

(defun npy-env-pipenv-virtualenv-root-fetcher ()
  "Fetch the Pipenv virtualenv root path if exists."
  (let* ((full-dir-path
          (cond ((buffer-file-name) (f-dirname (f-full (buffer-file-name))))
                (default-directory (f-full default-directory))
                (t nil)))
         (maybe-in-pool (if full-dir-path
                            (npy-env-pipenv-pool-check full-dir-path))))
    (if maybe-in-pool
        maybe-in-pool
      (let ((pipenv-res (s-chomp (shell-command-to-string "pipenv --venv"))))
        (cond ((and (stringp pipenv-res) (f-directory-p pipenv-res))
               (gpc-pool-pushnew
                (cons (gpc-get 'pipenv-project-root npy-env) pipenv-res)
                'pipenv-known-projects npy-env :test 'equal)
               pipenv-res)
              ((stringp pipenv-res)
               (when full-dir-path
                 (gpc-pool-delete-if #'(lambda (path)
                                         (s-matches-p (concat "^" path)
                                                      full-dir-path))
                                     'pipenv-non-project-dirs npy-env)
                 (gpc-pool-pushnew full-dir-path
                                   'pipenv-non-project-dirs npy-env :test 'equal))
               'no-virtualenv)
              (t 'ERR))))))

(cl-defun npy-env-pipenv-pool-check (full-dir-path)
  "Check if PATH is under a Pipenv project."
  (let ((maybe-in-white-pool
         (gpc-pool-member-if #'(lambda (white-pair)
                                 (s-matches-p (concat "^" (car white-pair))
                                              full-dir-path))
                             'pipenv-known-projects npy-env)))
    (if maybe-in-white-pool
        (progn
          (cdar maybe-in-white-pool))
      (let* ((maybe-in-black-pool
              (gpc-pool-member-if #'(lambda (black-path)
                                      (s-matches-p (concat "^" full-dir-path)
                                                   black-path))
                                  'pipenv-non-project-dirs npy-env)))
        (if maybe-in-black-pool
            'no-virtualenv
          nil)))))

(gpc-init npy-env
  '((pipenv-project-root nil npy-env-pipenv-project-root-fetcher)
    (pipenv-project-name nil npy-env-pipenv-project-name-fetcher)
    (pipenv-project-name-with-hash nil npy-env-pipenv-project-name-with-hash-fetcher)
    (pipenv-virtualenv-root nil npy-env-pipenv-virtualenv-root-fetcher)))
(gpc-pool-init 'pipenv-known-projects npy-env)
(gpc-pool-init 'pipenv-non-project-dirs npy-env)
(gpc-make-variable-buffer-local npy-env)

(cl-defun npy-env-initialize (&key (virtualenv nil))
  "Initialize `npy-env' and other virtualenv variables in a buffer.

Fetch the virtualenv root path when VIRTUALENV is non-nil.

This function is designed to be called in a major or minor mode
hook where you need to initialize npy-env and related
variables.."
  (make-local-variable 'python-shell-virtualenv-root)
  (when (derived-mode-p 'python-mode)
    (setq npy-buffer-child-dedicatable-to (current-buffer)))
  (gpc-get 'pipenv-project-root npy-env)   ;; FIXME: We need `gpc-get-all'.
  (gpc-get 'pipenv-project-name npy-env)
  (gpc-get 'pipenv-project-name-with-hash npy-env)
  (when virtualenv
    (let ((venv-root (gpc-get 'pipenv-virtualenv-root npy-env)))
      (setq python-shell-virtualenv-root
            (when (npy-env-valid-p venv-root) venv-root)))))

(defun npy-env-valid-p (object)
  "Return t if OBJECT is the path to a virtualenv root, otherwise nil.

Currently, this function only checks if the object is a string."
  (if (stringp object) t nil))

(defun npy-env-make-pipenv-proect-name-with-hash (path)
  "Return the filename of PATH with a Pipenv hash appended."
  (f-filename (npy-pipenv-compat-virtualenv-name path)))

;;; Functions to find Pipenv information by exploring directory structures.
(defun npy-env-find-pipenv-project-root-by-exploring (dirname)
  "Return the Pipenv project root if DIRNAME is under a project, otherwise nil."
  (npy-env-find-pipenv-project-root-by-exploring-impl (f-split (f-full dirname))))

;; FIXME: Should rewrite this as a non-recursive function.
(defun npy-env-find-pipenv-project-root-by-exploring-impl (dirname-list)
  "Return a Pipenv root if DIRNAME-LIST is under a project, otherwise nil.

DIRNAME-LIST should be the f-split style: e.g. (\"/\" \"usr\" \"local\")."
  (if (null dirname-list)
      nil
    (let ((dirname (apply #'f-join dirname-list)))
      (if (npy-env-pipenv-root-p dirname)
          dirname
        (npy-env-find-pipenv-project-root-by-exploring-impl
         (nbutlast dirname-list 1))))))

(defun npy-env-pipenv-root-p (dirname)
  "Return t if DIRNAME is a Pipenv project root, otherwise nil."
  (f-exists-p (concat (f-full dirname) "/Pipfile")))


;;; npy-buffer: variables to define npy buffers: npy inferior python buffers and npy scratch buffers.

(defvar npy-buffer-child-dedicatable-to nil
  "The buffer which a virtualenv-buffer-dedicated buffer to be spawned should dedicate to.")
(make-variable-buffer-local 'npy-buffer-child-dedicatable-to)

(defvar npy-buffer-scratch nil
  "Non-nil if the current buffer is a scratch buffer.")
(make-variable-buffer-local 'npy-buffer-scratch)

(defvar npy-buffer-shell-initialized nil
  "Non-nil means the inferior buffer is already initialized.")
(make-variable-buffer-local 'npy-buffer-shell-initialized)

(defvar npy-buffer-dedicated-to nil
  "The buffer which this buffer is dedicated to.")
(make-variable-buffer-local 'npy-buffer-dedicated-to)


;;; npy-pipenv-compat: Pipenv compatibility functions.
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


;;; npy-mode-line: Functions for the modeline.
(defun npy-mode-line-report ()
  "Report the Pipenv project name associated with the buffer in the modeline."
  (let ((root (gpc-get 'pipenv-project-name npy-env)))
    (format "%s[v:%s]"
            npy-mode-line-prefix
            (cond ((npy-env-valid-p root) root)
                  ((eq root 'no-virtualenv) npy-no-virtualenv-mark)
                  (t "E")))))

(defun npy-mode-line-update ()
  "Update the npy modeline."
  (let ((mode-line (funcall npy-mode-line-function)))
    (setq npy-mode-line mode-line))
  (force-mode-line-update))


;;; npy-pipenv-information: Functions to manage Pipenv project information on all buffers.
(defun npy-pipenv-project-information-get-from-all-buffers ()
  "Get and return the Pipenv project information from all buffers."
  (mapcar 'npy-pipenv-project-information-get (buffer-list)))

(defun npy-pipenv-project-information-get (buffer-or-name)
  "Get and return the Pipenv project information in BUFFER-OR-NAME."
  (let ((buffer (get-buffer buffer-or-name)))
    (when buffer
      (with-current-buffer buffer
        (list (buffer-name buffer)
              (gpc-pairs npy-env))))))

(defun npy-pipenv-project-information-cleanup-on-all-buffers ()
  "Clear the Pipenv project information on all buffers."
  (npy-mode 0)
  (gpc-pool-init 'pipenv-known-projects npy-env)
  (gpc-pool-init 'pipenv-non-project-dirs npy-env)
  (mapc 'npy-pipenv-project-information-clear (buffer-list))
  (npy-mode 1))

(defun npy-pipenv-project-information-clear (buffer-or-name)
  "Clear the Pipenv project information in BUFFER-OR-NAME."
  (let ((buffer (get-buffer buffer-or-name)))
    (when buffer
      (with-current-buffer buffer
        (setq npy-env nil)
        (kill-local-variable 'python-shell-virtualenv-root)))))

(defun npy-pipenv-project-information-fetch-all (buffer-or-name)
  "Call `gpc-fetch-all' for `npy-env' on BUFFER-OR-NAME."
  (let ((buffer (get-buffer buffer-or-name)))
    (when (and buffer (buffer-live-p buffer)
               (not (s-matches-p "^ " (buffer-name buffer))))
      (with-current-buffer buffer
        (gpc-fetch-all npy-env)))))

(defun npy-pipenve-project-information-fetch-all-on-all-buffers ()
  "Call `gpc-fetch-all' for `npy-env' on all buffers."
  (npy-mode 1)
  (mapc 'npy-pipenv-project-information-fetch-all (buffer-list)))


;;; Advice and hook functions.
(defun npy-advise-python-shell-get-buffer (orig-fun &rest orig-args)
  "Tweak the buffer entity in ORIG-ARGS.

Replace it with the inferior process for the project exists, otherwise
leave it untouched.  ORIG-FUN should be `python-shell-get-buffer'."
  (let ((project-name (gpc-get 'pipenv-project-name npy-env)))
    (cond ((derived-mode-p 'inferior-python-mode) (current-buffer))
          ((not (npy-env-valid-p project-name))
           (let ((res (apply orig-fun orig-args)))
             res))
          (t (let ((associated-file-path
                    (cond (npy-buffer-dedicated-to
                           ;; means virtualenv-buffer-dedicated
                           ;; inf-py-buf or scratch-buf.
                           (buffer-file-name npy-buffer-dedicated-to))
                          (npy-buffer-scratch nil)
                          ;; means virtualenv-dedicated scratch-buf.
                          ((derived-mode-p 'inferior-python-mode) nil)
                          ;; means virtualenv-dedicated inf-py-buf.
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
                 (cond (venv-buffer-dedicated-running
                        venv-buffer-dedicated-process-name)
                       (venv-dedicated-running venv-dedicated-process-name)
                       (t (let ((res (apply orig-fun orig-args)))
                            ;; Maybe raising an error is better.
                            res)))))))))

(defun npy-advise-find-file-noselect (orig-fun &rest orig-args)
  "Advise `find-file-noselet' :around with ORIG-FUN and ORIG-ARGS."
  (let ((res (apply orig-fun orig-args)))
    (when (and res (buffer-file-name res))
      (with-current-buffer res
        (gpc-get 'pipenv-project-name npy-env)
        (gpc-get 'pipenv-project-name-with-hash npy-env)))
    res))

(defun npy-advise-process-creation (orig-fun &rest orig-args)
  "Tweak `exec-path' and PATH during a `make-process' call.

Currently, ORIG-FUN should be `make-process', and ORIG-ARGS is
the arguments when it's called."
  (let* ((project-name (gpc-get 'pipenv-project-name npy-env)))
    (if (npy-env-valid-p project-name)
        (let* ((venv-root (gpc-get 'pipenv-virtualenv-root npy-env))
               (venv-bin-path (concat venv-root "/bin/"))
               ;; (exec-path (cons venv-bin-path exec-path))
               ;; `make-process' doesn't need this.
               (orig-path (getenv "PATH")))
          (setenv "PATH" (concat venv-bin-path ":" orig-path))
          (unwind-protect
              (let* ((res (apply orig-fun orig-args))
                     (paths (s-split ":" (getenv "PATH")))
                     (new-paths (cl-remove venv-bin-path paths :test 'equal)))
                (setenv "PATH" (s-join ":" new-paths))
                res)
            (setenv "PATH" orig-path)))
      (let ((res (apply orig-fun orig-args)))
        res))))

(defun npy-advise-getenv (orig-fun &rest orig-args)
  "Append the virtualenv path when possible.

ORIG-FUN should be `getenv' and ORIG-ARGS is the arguments when
it's called."
  (let ((res (apply orig-fun orig-args)))
    (when (equal (car orig-args) "PATH")
      (let* ((venv-root (gpc-get 'pipenv-virtualenv-root npy-env))
             (venv-bin-path (when (npy-env-valid-p venv-root)
                              (concat venv-root "/bin/"))))
        (when venv-bin-path
          (let* ((path-list (s-split ":" res))
                 (clean-paths
                  (s-join ":" (cl-remove venv-bin-path path-list :test 'equal))))
            (setq res (concat venv-bin-path ":" clean-paths))))))
    res))

(defun npy-advise-write-file (orig-fun &rest orig-args)
  "Update two variables when `write-file' (ORIG-FUN with ORIG-ARGS) is called.

The two variables are: `npy--pipenv-project-root' and
`npy--pipenv-project-name'"
  (let ((res (apply orig-fun orig-args)))
    (when (bound-and-true-p npy-mode)
      (gpc-fetch-all npy-env)
      (npy-mode-line-update))
    res))

(defun npy-hook-python-mode ()
  "Set the name, root, and venv of a Pipenv project."
  (npy-env-initialize :virtualenv t)
  (when npy-dynamic-mode-line
    (npy-mode-line-update)))

(defun npy-hook-find-file ()
  "Set the name, root, and venv of a Pipenv project."
  (npy-env-initialize :virtualenv t)
  (when npy-dynamic-mode-line
    (npy-mode-line-update)))

(defun npy-hook-dired-mode ()
  "Set the name, root, and venv of a Pipenv project."
  (npy-env-initialize :virtualenv t)
  (when npy-dynamic-mode-line
    (npy-mode-line-update)))

(defun npy-hook-compilation-mode ()
  "Set the name, root, and venv of a Pipenv project."
  (npy-env-initialize :virtualenv t)
  (when npy-dynamic-mode-line
    (npy-mode-line-update)))


;;; npy-info: Info lookup support (EXPERIMENTAL)
;; This extension is based on `pydoc-info' by Jon Waltman.
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
  (with-syntax-table python-dotty-syntax-table
    (current-word)))

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

(defun npy-info-advise-info-insert-file-contents (&rest _args)
  "Advise to automatically hide 'see' in info files generated by sphinx."
  ;; Adapted from http://www.sphinx-doc.org/en/stable/faq.html#displaying-links
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward
             "^ *Generated by \\(Sphinx\\|Docutils\\)"
             (save-excursion (search-forward "\x1f" nil t)) t)
        (make-local-variable 'Info-hide-note-references)
        (setq Info-hide-note-references 'hide)))))


;;; User facing functions and its helpers.
(defmacro npy-when-valid-do (var it)
  "Do IT when VAR is valid, otherwise show a warning."
  (declare (indent 1))
  `(cond ((npy-env-valid-p ,var)
          ,it)
         ((eq ,var 'no-virtualenv)
          (message "No virtualenv has been created for this project yet!"))
         (t (message "Something wrong has happend in npy."))))

;;;###autoload
(defun npy-run-python (&optional dedicated)
  "Run an inferior python process with access to a virtualenv.

When called interactively with `prefix-arg', it spawns a buffer
DEDICATED inferior python process with access to the virtualenv."
  (interactive
   (if current-prefix-arg
       (list t)
     (list nil)))
  (when (and dedicated (not npy-buffer-child-dedicatable-to))
    (error "You're not in a buffer associated with a file you can dedicate to"))
  (gpc-fetch-all npy-env)
  (let ((project-name (gpc-val 'pipenv-project-name npy-env)))
    (unless (npy-env-valid-p project-name)
      (error "You're not in a buffer associated with a Pipenv project: \"%s\""
             project-name))
    (let* ((spawner-buffer (current-buffer))
           (maybe-dedicate-to npy-buffer-child-dedicatable-to)
           (venv-root (gpc-val 'pipenv-virtualenv-root npy-env))
           (exec-path (cons venv-root exec-path))
           (python-shell-virtualenv-root venv-root)
           (process-name
            (cond (dedicated
                   (format "%s[v:%s;b:%s]"
                           python-shell-buffer-name
                           project-name
                           (f-filename (buffer-file-name
                                        npy-buffer-child-dedicatable-to))))
                  (t (format "%s[v:%s]"
                             python-shell-buffer-name
                             project-name)))))
      (prog1
          (pop-to-buffer
           (python-shell-make-comint (python-shell-calculate-command)
                                     process-name t))
        (unless npy-buffer-shell-initialized
          (when dedicated
            (setq npy-buffer-dedicated-to maybe-dedicate-to)
            (setq npy-buffer-child-dedicatable-to maybe-dedicate-to))
          (gpc-copy npy-env spawner-buffer (current-buffer))
          (gpc-lock npy-env)
          (setq npy-buffer-shell-initialized t))))))

;;;###autoload
(defun npy-display-pipenv-project-root ()
  "Show the path to the Pipenv project root directory."
  (interactive)
  (npy-mode-line-update)
  (npy-when-valid-do (gpc-get 'pipenv-project-root npy-env)
    (message "Project: %s" (gpc-get 'pipenv-project-root npy-env))))

;;;###autoload
(defun npy-update-pipenv-project-root ()
  "Update the Pipenv project root directory."
  (interactive)
  (gpc-get 'pipenv-project-root npy-env)
  (npy-display-pipenv-project-root))

;;;###autoload
(defun npy-display-pipenv-virtualenv-root ()
  "Show the path to the Pipenv virtualenv root directory."
  (interactive)
  (npy-mode-line-update)
  (npy-when-valid-do
      (gpc-get 'pipenv-virtualenv-root npy-env)
    (message "Virtualenv: %s" (gpc-get 'pipenv-virtualenv-root npy-env))))

;;;###autoload
(defun npy-update-pipenv-virtualenv-root ()
  "Show the path to the Pipenv virtualenv root directory."
  (interactive)
  (gpc-fetch 'pipenv-virtualenv-root npy-env)
  (npy-display-pipenv-virtualenv-root))

;;;###autoload
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

;;;###autoload
(defun npy-scratch (&optional dedicated)
  "Get a python scratch buffer associated with a virtualenv.

When prefix DEDICATED is set, make the scratch buffer dedicate to
the buffer spawning it."
  (interactive
   (if current-prefix-arg
       (list t)
     (list nil)))
  (let ((project-name (gpc-val 'pipenv-project-name npy-env)))
    (unless (npy-env-valid-p project-name)
      (error "You're not in a buffer associated with a Pipenv project:\"%s\""
             project-name))
    (when (and dedicated (not npy-buffer-child-dedicatable-to))
      (error "You're not in a buffer associated with a file you can dedicate to"))
    (let* ((spawner-buffer (current-buffer))
           (maybe-dedicate-to npy-buffer-child-dedicatable-to)
           (mode 'python-mode)
           (name (cond (dedicated
                        (format "*pyscratch[v:%s;b:%s]*"
                                project-name
                                (f-filename (buffer-file-name
                                             npy-buffer-child-dedicatable-to))))
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
          (setq npy-buffer-scratch t)
          (gpc-copy npy-env spawner-buffer scratch-buf)
          (gpc-lock npy-env)
          (npy-mode-line-update)
          (when content (save-excursion (insert content)))
          (if dedicated
              (progn
                (setq npy-buffer-dedicated-to maybe-dedicate-to)
                (setq npy-buffer-child-dedicatable-to maybe-dedicate-to))
            (setq npy-buffer-child-dedicatable-to nil)
            ;; We need this to overwrite the value set by `npy-mode'.
            ))))))

;;;###autoload
(defun npy-show-python-environment ()
  "Show Python environment information."
  (interactive)
  (message (concat "pipenv-project-root: %s\n"
                   "pipenv-project-name: %s\n"
                   "pipenv-project-name-with-hash: %s\n"
                   "pipenv-virtualenv-root: %s\n"
                   "python-shell-virtualenv-root: %s\n"
                   "npy-buffer-scratch-buffer: %s\n"
                   "npy-buffer-shell-initialized: %s\n"
                   "npy-buffer-dedicated-to: %s\n"
                   "npy-buffer-child-dedicatable-to %s\n")
           (gpc-val 'pipenv-project-root npy-env)
           (gpc-val 'pipenv-project-name npy-env)
           (gpc-val 'pipenv-project-name-with-hash npy-env)
           (gpc-val 'pipenv-virtualenv-root npy-env)
           python-shell-virtualenv-root
           npy-buffer-scratch
           npy-buffer-shell-initialized
           npy-buffer-dedicated-to
           npy-buffer-child-dedicatable-to))

;;;###autoload
(defun npy-activate-virtualenv-automatic ()
  "Switch the activated virtualenv automatically."
  (interactive)
  (advice-add 'make-process :around #'npy-advise-process-creation)
  (advice-add 'getenv :around #'npy-advise-getenv)
  ;; (advice-add 'find-file-noselect :around 'npy-advise-find-file-noselect)
  )
;; `pytest' and `python-pytest' work fine without the advice to
;; `find-file-noselet'.  So, currently it's disabled.  When we find a
;; mode which requires this advice to work with virtualenvs, we'll
;; uncomment this.

;;;###autoload
(defun npy-deactivate-virtualenv-automatic ()
  "Deactivate virtualenv automatic."
  (interactive)
  (advice-remove 'make-process  #'npy-advise-process-creation)
  (advice-remove 'getenv #'npy-advise-getenv)
  ;; (advice-remove 'find-file-noselect 'npy-advise-find-file-noselect)
  )

;;;###autoload
(defun npy-start-python-development ()
  "Enable `npy-mode' and initialize all buffers."
  (interactive)
  (npy-pipenv-project-information-cleanup-on-all-buffers)
  (npy-pipenve-project-information-fetch-all-on-all-buffers)
  (npy-activate-virtualenv-automatic))

;;;###autoload
(defun npy-finish-pythhon-development ()
  "Disable `npy-mode' and clear all cache data."
  (interactive)
  (npy-deactivate-virtualenv-automatic)
  (npy-pipenv-project-information-cleanup-on-all-buffers)
  (npy-mode 0))


;;; Defining the minor mode.
(defvar npy-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" #'npy-run-python)
    (define-key map "s" #'npy-pipenv-shell)
    (define-key map "S" #'npy-scratch)
    (define-key map "a" #'npy-activate-virtualenv-automatic)
    (define-key map "d" #'npy-deactivate-virtualenv-automatic)
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
  "Minor mode to extend the Python development support in Emacs.

When called interactively, toggle `npy-mode'.  With prefix ARG,
enable `npy-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `npy-mode' if ARG is omitted, nil
or positive.  If ARG is `toggle', toggle `npy-mode'.  Otherwise
behave as if called interactively.

\\{npy-mode-map}"
  :group 'npy
  :require 'npy
  :lighter npy-mode-line
  :keymap npy-mode-map
  :global t
  (cond
   (npy-mode
    (add-hook 'python-mode-hook 'npy-hook-python-mode)
    (add-hook 'find-file-hook 'npy-hook-find-file)
    (add-hook 'dired-mode-hook 'npy-hook-dired-mode)
    (add-hook 'compilation-mode-hook 'npy-hook-compilation-mode)
    (advice-add 'python-shell-get-buffer :around #'npy-advise-python-shell-get-buffer)
    (advice-add 'write-file :around #'npy-advise-write-file)
    (advice-add 'info-insert-file-contents :after #'npy-info-advise-info-insert-file-contents)
    (npy-env-initialize :virtualenv t))
   (t
    (remove-hook 'python-mode-hook 'npy-hook-python-mode)
    (remove-hook 'find-file-hook 'npy-hook-find-file)
    (remove-hook 'dired-mode-hook 'npy-hook-dired-mode)
    (remove-hook 'compilation-mode-hook 'npy-hook-compilation-mode)
    (advice-remove 'python-shell-get-buffer #'npy-advise-python-shell-get-buffer)
    (advice-remove 'write-file #'npy-advise-write-file)
    (advice-remove 'info-insert-file-contents #'npy-info-advise-info-insert-file-contents))))

(provide 'npy)

;;; npy.el ends here
