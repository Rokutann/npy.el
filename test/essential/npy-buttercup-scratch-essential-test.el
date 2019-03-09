;;; npy-scratch-short-test.el --- New tests for npy.el.        -*- lexical-binding: t; -*-

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

;; New tests for npy.el.

;;; Code:

(require 'buttercup)
(require 'seq)
(require 'f)
(require 's)
(require 'exec-path-from-shell)

(message "Running tests on Emacs %s" emacs-version)

;; (after-load 'exec-path-from-shell
;;   (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
;;     (add-to-list 'exec-path-from-shell-variables var)))


(setq-default exec-path-from-shell-arguments nil)
(exec-path-from-shell-initialize)

(defvar npy-test/test-path
  (directory-file-name (file-name-directory load-file-name))
  "Path to tests directory.")

(defvar npy-test/root-path
  (directory-file-name (f-dirname (file-name-directory npy-test/test-path)))
  "Path to root directory.")

(defvar npy-test/playground-path
  "/tmp/npy-playground/"
  "Path to the playground for test.")

(defvar npy-test/python-wait
  1
  "Sleep for duration after inputting somethng to a Python interpreter.")

(load (expand-file-name "npy" npy-test/root-path) 'noerror 'nomessage)
(npy-mode 1)
;;(npy-initialize)

(setq npy-test/venv-root-for-project1 (s-chomp (shell-command-to-string "(cd /tmp/npy-playground/project1/ && pipenv --venv)")))
(setq npy-test/venv-root-for-project2 (s-chomp (shell-command-to-string "(cd /tmp/npy-playground/project2/ && pipenv --venv)")))

(describe "Python Scratch Buffer Feature:"
  (describe "when you have a virtualenv-buffer dedicated inferior
  python buffer spawned on a virtualenv-buffer dedicated python
  scratch buffer"
    (it "dispatches a code chunk sent from the python scratch
    buffer into the python inferior buffer"
      (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
        (with-file-buffers ("project1/buz.py")
          (set-buffer "buz.py")
          (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
          (npy-scratch t)
          (let-to-kill ((scratch-buf (get-buffer "*pyscratch[Pipenv:project1;b:buz.py]*")))
            (with-current-buffer scratch-buf
              (npy-run-python t)
              (npy-helper-wait))
            (let-to-kill ((inf-buf (get-buffer "*Python[Pipenv:project1;b:buz.py]*")))
              (npy-helper-write "VAR7 = \"from scratch\"\n" scratch-buf)
              (with-current-buffer scratch-buf
                (python-shell-send-buffer))
              (should-response-match inf-buf
                "print(VAR7)\n" "from scratch")))))))
  (describe "when you have a virtualenv-buffer dedicated python
  scratch buffer spanwed on a virtualenv-buffer dedicated
  inferior python buffer"
    (it "dispatches a code chunk sent from the python scratch
    buffer into tye python inferior buffer"
      (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
        (with-file-buffers ("project1/buz.py")
          (set-buffer "buz.py")
          (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
          (npy-run-python t)
          (npy-helper-wait)
          (let-to-kill ((inf-buf (get-buffer "*Python[Pipenv:project1;b:buz.py]*")))
            (set-buffer inf-buf)
            (npy-scratch t)
            (let-to-kill ((scratch-buf (get-buffer "*pyscratch[Pipenv:project1;b:buz.py]*")))
              (npy-helper-write "VAR5 = \"from scratch\"\n" scratch-buf)
              (with-current-buffer scratch-buf
                (python-shell-send-buffer))
              (should-response-match inf-buf
                "print(VAR5)\n" "from scratch"))))))))

(provide 'npy-scratch-short-test)
;;; npy-new-test.el ends here
