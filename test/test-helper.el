;;; test-helper.el --- npy: Test helpers.            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Cyriakus "Mukuge" Hill

;; Author: Cyriakus "Mukuge" Hill <cyriakus.h@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Test helpers for npy.

;;; Code:

(defun npy-helper-create-files (basedir filespec)
  "In BASEDIR, create files according to FILESPEC.

FILESPEC is a list consisting of strings and pairs, where the car
of a pair is a file name relative to BASEDIR and the cdr of a
pair is the content for that file."
  (dolist (spec filespec)
    (let* ((filename (if (stringp spec)
                         spec
                       (car spec)))
           (content (if (stringp spec)
                        ""
                      (cdr spec)))
           (fullname (format "%s/%s" basedir filename))
           (dirname (file-name-directory fullname)))
      (when (not (file-directory-p dirname))
        (make-directory dirname t))
      (write-region content nil fullname))))

(defun npy-helper-delete-files (basedir filespec)
  "In BASEDIR, delete all files in FILESPEC."
  (dolist (spec filespec)
    (let* ((filename (if (stringp spec)
                         spec
                       (car spec)))
           (content (if (stringp spec)
                        ""
                      (cdr spec)))
           (fullname (format "%s/%s" basedir filename))
           (dirname (file-name-directory fullname)))
      (when (and (file-directory-p dirname)
                 (file-exists-p fullname))
        (delete-file fullname)))))

(defmacro with-npy-sandbox (&rest body)
  "Clear up npy related variables before and after execute BODY."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (gpc-pool-clear 'pipenv-known-projects npy-env)
         (gpc-pool-clear 'pipenv-non-project-dirs npy-env)
         (gpc-pool-clear 'pip-known-projects npy-env)
         (gpc-pool-clear 'pip-non-project-dirs npy-env)
         ,@body)
     (gpc-pool-clear 'pipenv-known-projects npy-env)
     (gpc-pool-clear 'pipenv-non-project-dirs npy-env)
     (gpc-pool-clear 'pip-known-projects npy-env)
     (gpc-pool-clear 'pip-non-project-dirs npy-env)))

(defmacro with-files-in-playground (filespec &rest body)
  "Execute BODY in the playground specified by FILESPEC."
  (declare (indent 1))
  `(unwind-protect
       (progn
         (npy-helper-create-files npy-test/playground-path
                                  ',filespec)
         ,@body)
     (npy-helper-delete-files npy-test/playground-path ',filespec)))

(defmacro with-file-buffers (files &rest body)
  "Execute BODY after creating buffers visiting FILES."
  (declare (indent 1))
  (let ((file (cl-gensym "file-"))
        (buffers (cl-gensym "buffers-"))
        (buffer (cl-gensym "buffer-")))
    `(let ((,buffers nil))
       (dolist (,file ',files)
         (push (npy-helper-find-file-in-playground ,file) ,buffers))
       (unwind-protect
           (progn
             ,@body)
         (dolist (,buffer ,buffers)
           (when (buffer-live-p ,buffer)
             (kill-buffer ,buffer)))))))

(defmacro npy-helper-find-file-in-playground (filename)
  "Edit file FILENAME."
  `(find-file-noselect (concat npy-test/playground-path ,filename) nil nil))

(defmacro npy-helper-in-playground (&rest sequences)
  "Concatenate all the arguments and make the result a string."
  `(concat npy-test/playground-path ,@sequences))

(defun npy-helper-wait ()
  "Wait the Python interpreter."
  (sleep-for npy-test/python-wait))

(defmacro npy-helper-kill-inferior-python-buffers (&rest buffer-or-names)
  "Kill all of BUFFER-OR-NAMES, which are bound to Python inferior processes."
  (declare (indent 0))
  `(progn
     ,@(mapcar (lambda (buffer-or-name)
                 `(with-current-buffer ,buffer-or-name
                    (python-shell-send-string "quit()\n")
                    (npy-helper-wait)
                    (kill-buffer ,buffer-or-name)))
               buffer-or-names)))

(defvar npy-test/match-flag nil
  "Non-nil means there was at least one successful match.")

(defun npy-helper-match-filter (proc string)
  "Check if outputs of PROC containg STRING."
  (when (s-matches-p npy-test/venv-root-for-project1 string)
    (setq npy-test/match-flag t)))

(defun npy-helper-match-filter (regex string)
  "Check if outputs of PROC containg STRING."
  ;;  (write-region (format "string:%s\n" string) nil "/tmp/npy.log" t)
  (message "Python response: %s" string)
  (when (s-matches-p regex string)
    ;;    (write-region (format "%s" string) nil "/tmp/npy.log" t)
    (setq npy-test/match-flag t)))

(defmacro should-response-match (buffer python-command regex)
  "Check if the return value of PYTHON-COMMAND in BUFFER matches REGEX."
  (declare (indent 1))
  `(with-current-buffer ,buffer
     (let ((npy-test/match-flag nil)
           (comint-output-filter-functions
            '(ansi-color-process-output
              (lambda (output)
                (npy-helper-match-filter ,regex output))
              python-shell-comint-watch-for-first-prompt-output-filter
              python-pdbtrack-comint-output-filter-function
              python-comint-postoutput-scroll-to-bottom
              comint-watch-for-password-prompt
              )))
       (python-shell-send-string ,python-command)
       (npy-helper-wait)
       (should npy-test/match-flag))))

(defmacro let-to-kill (buffer-bindings &rest body)
  "Ensure kill BUFFER-BINDINGS after executing BODY."
  (declare (indent 1))
  `(let ,buffer-bindings
     (unwind-protect
         ,@body
       ,@(mapcar '(lambda (binding) `(npy-helper-kill-python-buffer ,(car binding))) buffer-bindings))))

(defun npy-helper-kill-python-buffer (buffer)
  "Kill a `python-mode' or `inferior-python-mode' BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (derived-mode-p 'inferior-python-mode)
          (npy-helper-kill-inferior-python-buffers buffer)
        (kill-buffer)))))


(defun npy-helper-write (string buffer)
  "Write STRING out to BUFFER."
  (mapc #'(lambda (char) (write-char char buffer)) string))

;; (setq filespec '(("project1/foo.py" . "VAR1 = 1")))
;; (setq npy-test/playground-path "/tmp/npy-playground/")

;; (defun npy-helper-buttercup-setup ()
;;   "Set up for buttercup testing."
;;   (npy-helper-create-files "/tmp/npy-playground/" filespec)
;;   (setq buf-a (npy-helper-find-file-in-playground "project1/foo.py"))
;;   (npy-helper-log-write "debug: buf-a: %s" buf-a)
;;   (with-current-buffer buf-a
;;     (npy-run-python)
;;     (npy-helper-wait))
;;   (setq inf-buf-a (get-buffer "*Python[Pipenv:project1]*"))
;;   (npy-helper-log-write "debug2: inf-buf-a: %s" inf-buf-a)
;;   )

;; (npy-helper-buttercup-setup)

;; (defun npy-helper-buttercup-teardown ()
;;   "Tear dwon for buttercup testing."

;;   )

(defvar npy-helper-log-file "/tmp/npy.log"
  "The path to the log file for `npy' testing.")

(defvar npy-helper-log-flag nil
  "Non-nil means the logging facility is on.")
(setq npy-helper-log-flag t)

(defun npy-helper-log-write (string &rest objects)
  "Write STRING to `npy-helper-log-file'.

This function replaces any format specifications in STRING with
encodings of the corresponding OBJECTS."
  (when npy-helper-log-flag
    (write-region (concat "["(current-time-string) "] "
                          (if objects
                              (apply #'format string objects)
                            string)
                          "\n")
                  nil npy-helper-log-file t)))

(provide 'test-helper)
;;; test-helper.el ends here
