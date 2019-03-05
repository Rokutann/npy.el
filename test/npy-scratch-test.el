;;; npy-new-test.el --- New tests for npy.el.        -*- lexical-binding: t; -*-

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
          (npy-helper-kill-python-inferior-buffers buffer)
        (kill-buffer)))))

(defun npy-helper-write (string buffer)
  "Write STRING out to BUFFER."
  (mapc #'(lambda (char) (write-char char buffer)) string))

(ert-deftest npy-integration-test/spawn-an-npy-scratch/check-buffer-name ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
      (npy-scratch)
      (let-to-kill ((scratch-buf (get-buffer "*pyscratch[Pipenv:project1]*")))
        (should-not (eq  scratch-buf nil))))))

(ert-deftest npy-integration-test/spawn-an-npy-scratch-dedicated/check-buffer-name ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
      (npy-scratch t)
      (let-to-kill ((scratch-buf (get-buffer "*pyscratch[Pipenv:project1;b:buz.py]*")))
        (should-not (eq  scratch-buf nil))))))

(ert-deftest npy-integration-test/npy-scratch/send-string ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
      (npy-scratch)
      (npy-run-python)
      (npy-helper-wait)
      (let-to-kill ((scratch-buf (get-buffer "*pyscratch[Pipenv:project1]*"))
                    (inf-buf (get-buffer "*Python[Pipenv:project1]*")))
        (npy-helper-write "VAR1 = \"from scratch\"\n" scratch-buf)
        (with-current-buffer scratch-buf
          (python-shell-send-buffer))
        (should-response-match inf-buf
          "print(VAR1)\n" "from scratch")))))

(ert-deftest npy-integration-test/npy-scratch-dedicated/send-string ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
      (npy-run-python t)
      (npy-helper-wait)
      (set-buffer "buz.py")
      (npy-scratch t)
      (let-to-kill ((scratch-buf (get-buffer "*pyscratch[Pipenv:project1;b:buz.py]*"))
                    (inf-buf (get-buffer "*Python[Pipenv:project1;b:buz.py]*")))
        (should-not (null scratch-buf))
        (should-not (null inf-buf))
        (npy-helper-write "VAR2 = \"from scratch\"\n" scratch-buf)
        (with-current-buffer scratch-buf
          (python-shell-send-buffer))
        (should-response-match inf-buf
          "print(VAR2)\n" "from scratch")))))

(ert-deftest npy-integration-test/npy-scratch-dedicated-spawned-from-an-inf-buf-dedicated/send-string ()
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
          (npy-helper-write "VAR3 = \"from scratch\"\n" scratch-buf)
          (with-current-buffer scratch-buf
            (python-shell-send-buffer))
          (should-response-match inf-buf
            "print(VAR3)\n" "from scratch"))))))

(ert-deftest npy-integration-test/spawn-an-inf-buf-and-spawn-a-scratch-on-it/send-string ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
      (npy-run-python)
      (npy-helper-wait)
      (let-to-kill ((inf-buf (get-buffer "*Python[Pipenv:project1]*")))
        (message "inf-buf: %s" inf-buf)
        (set-buffer inf-buf)
        (npy-scratch)
        (let-to-kill ((scratch-buf (get-buffer "*pyscratch[Pipenv:project1]*")))
          (message "scratch-buf: %s" scratch-buf)
          (npy-helper-write "VAR4 = \"from scratch\"\n" scratch-buf)
          (with-current-buffer scratch-buf
            (python-shell-send-buffer))
          (should-response-match inf-buf
            "print(VAR4)\n" "from scratch"))))))

(ert-deftest npy-integration-test/spawn-a-dedicated-inf-buf-and-spawn-a-scratch-on-it ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
      (npy-run-python t)
      (npy-helper-wait)
      (let-to-kill ((inf-buf (get-buffer "*Python[Pipenv:project1;b:buz.py]*")))
        (set-buffer inf-buf)
        (npy-scratch)
        (let-to-kill ((scratch-buf (get-buffer "*pyscratch[Pipenv:project1]*")))
          (should-not (eq scratch-buf nil)))))))

(ert-deftest npy-integration-test/spawn-a-scratch-and-spawn-an-inf-buf-on-it ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
      (npy-scratch)
      (let-to-kill ((scratch-buf (get-buffer "*pyscratch[Pipenv:project1]*")))
        (with-current-buffer scratch-buf
          (npy-run-python)
          (npy-helper-wait))
        (let-to-kill ((inf-buf (get-buffer "*Python[Pipenv:project1]*")))
          (npy-helper-write "VAR6 = \"from scratch\"\n" scratch-buf)
          (with-current-buffer scratch-buf
            (python-shell-send-buffer))
          (should-response-match inf-buf
            "print(VAR6)\n" "from scratch"))))))

(ert-deftest npy-integration-test/spawn-a-virtualenv-buffer-dedicated-scratch-and-spawn-a-dedicated-inf-buf-on-it ()
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
            "print(VAR7)\n" "from scratch"))))))

(ert-deftest npy-integration-test/spawn-an-virtualenv-buffer-dedicated-inf-buf-and-spawn-a-virtualenv-buffer-dedicated-scratch-on-it-but-get-normal ()
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
            "print(VAR5)\n" "from scratch"))))))


(provide 'npy-new-test)
;;; npy-new-test.el ends here
