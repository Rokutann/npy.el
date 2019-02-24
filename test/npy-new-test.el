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

(defun npy-helper-write (string buffer)
  "Write STRING out to BUFFER."
  (mapc #'(lambda (char) (write-char char buffer)) string))

(ert-deftest npy-integration-test/spawn-an-npy-scratch/check-buffer-name ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
      (npy-scratch)
      (let ((scratch-buf (get-buffer "*pyscratch[v:project1]*")))
        (should-not (eq  scratch-buf nil))
        (kill-buffer scratch-buf)))))

(ert-deftest npy-integration-test/spawn-an-npy-scratch-dedicated/check-buffer-name ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
      (npy-scratch t)
      (let ((scratch-buf (get-buffer "*pyscratch[v:project1;b:buz.py]*")))
        (should-not (eq  scratch-buf nil))
        (kill-buffer scratch-buf)))))

(ert-deftest npy-integration-test/npy-scratch/send-string ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
      (npy-scratch)
      (npy-run-python)
      (npy-helper-wait)
      (let ((scratch-buf (get-buffer "*pyscratch[v:project1]*"))
            (inf-buf (get-buffer "*Python[v:project1]*")))
        (npy-helper-write "VAR1 = \"from scratch\"\n" scratch-buf)
        (with-current-buffer scratch-buf
          (python-shell-send-buffer))
        (should-response-match inf-buf
          "print(VAR1)\n" "from scratch")
        (npy-helper-kill-python-inferior-buffers inf-buf)
        (kill-buffer scratch-buf)))))

(ert-deftest npy-integration-test/npy-scratch-dedicated/send-string ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
      (npy-run-python t)
      (npy-helper-wait)
      (set-buffer "buz.py")
      (npy-scratch t)
      (let ((scratch-buf (get-buffer "*pyscratch[v:project1;b:buz.py]*"))
            (inf-buf (get-buffer "*Python[v:project1;b:buz.py]*")))
        (should-not (null scratch-buf))
        (should-not (null inf-buf))
        (npy-helper-write "VAR2 = \"from scratch\"\n" scratch-buf)
        (with-current-buffer scratch-buf
          (python-shell-send-buffer))
        (should-response-match inf-buf
          "print(VAR2)\n" "from scratch")
        (npy-helper-kill-python-inferior-buffers inf-buf)
        (kill-buffer scratch-buf)))))

(ert-deftest npy-integration-test/npy-scratch-dedicated-spawned-from-an-inf-buf-dedicated/send-string ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
      (npy-run-python t)
      (npy-helper-wait)
      (let ((inf-buf (get-buffer "*Python[v:project1;b:buz.py]*")))
        (set-buffer inf-buf)
        (npy-scratch t)
        (let ((scratch-buf (get-buffer "*pyscratch[v:project1;b:buz.py]*")))
          (npy-helper-write "VAR3 = \"from scratch\"\n" scratch-buf)
          (with-current-buffer scratch-buf
            (python-shell-send-buffer))
          (should-response-match inf-buf
            "print(VAR3)\n" "from scratch")
          (npy-helper-kill-python-inferior-buffers inf-buf)
          (kill-buffer scratch-buf))))))

(ert-deftest npy-integration-test/spawn-an-inf-buf-and-spawn-a-scratch-on-it/send-string ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
      (npy-run-python)
      (npy-helper-wait)
      (let ((inf-buf (get-buffer "*Python[v:project1]*")))
        (message "inf-buf: %s" inf-buf)
        (set-buffer inf-buf)
        (npy-scratch)
        (let ((scratch-buf (get-buffer "*pyscratch[v:project1]*")))
          (message "scratch-buf: %s" scratch-buf)
          (npy-helper-write "VAR4 = \"from scratch\"\n" scratch-buf)
          (with-current-buffer scratch-buf
            (python-shell-send-buffer))
          (should-response-match inf-buf
            "print(VAR4)\n" "from scratch")
          (npy-helper-kill-python-inferior-buffers inf-buf)
          (kill-buffer scratch-buf))))))

(ert-deftest npy-integration-test/spawn-an-inf-buf-and-try-spawn-a-dedicated-scratch-on-it-but-get-normal/send-string ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
      (npy-run-python)
      (npy-helper-wait)
      (let ((inf-buf (get-buffer "*Python[v:project1]*")))
        (set-buffer inf-buf)
        (npy-scratch t)
        (let ((scratch-buf (get-buffer "*pyscratch[v:project1]*")))
          (npy-helper-write "VAR5 = \"from scratch\"\n" scratch-buf)
          (with-current-buffer scratch-buf
            (python-shell-send-buffer))
          (should-response-match inf-buf
            "print(VAR5)\n" "from scratch")
          (npy-helper-kill-python-inferior-buffers inf-buf)
          (kill-buffer scratch-buf))))))

(ert-deftest npy-integration-test/spawn-a-dedicated-inf-buf-and-spawn-a-scratch-on-it ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
      (npy-run-python t)
      (npy-helper-wait)
      (let ((inf-buf (get-buffer "*Python[v:project1;b:buz.py]*")))
        (set-buffer inf-buf)
        (npy-scratch)
        (let ((scratch-buf (get-buffer "*pyscratch[v:project1]*")))
          (should-not (eq scratch-buf nil))
          (npy-helper-kill-python-inferior-buffers inf-buf)
          (kill-buffer scratch-buf))))))

(ert-deftest npy-integration-test/spawn-a-scratch-and-spawn-an-inf-buf-on-it ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
      (npy-scratch)
      (let ((scratch-buf (get-buffer "*pyscratch[v:project1]*")))
        (with-current-buffer scratch-buf
          (npy-run-python)
          (npy-helper-wait))
        (let ((inf-buf (get-buffer "*Python[v:project1]*")))
          (npy-helper-write "VAR6 = \"from scratch\"\n" scratch-buf)
          (with-current-buffer scratch-buf
            (python-shell-send-buffer))
          (should-response-match inf-buf
            "print(VAR6)\n" "from scratch")
          (npy-helper-kill-python-inferior-buffers inf-buf)
          (kill-buffer scratch-buf))))))

(ert-deftest npy-integration-test/spawn-a-virtualenv-buffer-dedicated-scratch-and-spawn-a-dedicated-inf-buf-on-it ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
      (npy-scratch t)
      (let ((scratch-buf (get-buffer "*pyscratch[v:project1;b:buz.py]*")))
        (with-current-buffer scratch-buf
          (npy-run-python t)
          (npy-helper-wait))
        (let ((inf-buf (get-buffer "*Python[v:project1;b:buz.py]*")))
          (npy-helper-write "VAR7 = \"from scratch\"\n" scratch-buf)
          (with-current-buffer scratch-buf
            (python-shell-send-buffer))
          (should-response-match inf-buf
            "print(VAR7)\n" "from scratch")
          (npy-helper-kill-python-inferior-buffers inf-buf)
          (kill-buffer scratch-buf))))))

(provide 'npy-new-test)
;;; npy-new-test.el ends here
