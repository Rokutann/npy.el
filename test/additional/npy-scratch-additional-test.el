;;; npy-scratch-additional-test.el --- Additional tests for npy-scratch. -*- lexical-binding: t; -*-

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

;; Additional tests for npy-scratch.

;;; Code:

(describe "npy-scratch"
  (describe "when called on a python-mode buffer"
    (it "spawns a virtualenv dedicated python scratch buffer with
    a name associated with the Pipenv project which the
    python-mode buffer where npy-scratch is called belongs to"
      (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
        (with-file-buffers ("project1/buz.py")
          (set-buffer "buz.py")
          (should (equal (gpc-val 'pipenv-project-root npy-env) (npy-helper-in-playground "project1")))
          (npy-scratch)
          (let-to-kill ((scratch-buf (get-buffer "*pyscratch[Pipenv:project1]*")))
            (should-not (eq  scratch-buf nil))))))
    (it "spanws a virtualenv-buffer dedicated python
    scratchbuffer with a name associated with the Pipenv project
    and the buffer name where npy-scratch was called"
      (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
        (with-file-buffers ("project1/buz.py")
          (set-buffer "buz.py")
          (should (equal (gpc-val 'pipenv-project-root npy-env) (npy-helper-in-playground "project1")))
          (npy-scratch t)
          (let-to-kill ((scratch-buf (get-buffer "*pyscratch[Pipenv:project1;b:buz.py]*")))
            (should-not (eq  scratch-buf nil)))))))
  (describe "when called on a virtualenv dedicated inferior python buffer"
    (it "spawns a virtualenv dedicated python scratch buffer with
    the Pipenv project name in its buffer name"
      (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
        (with-file-buffers ("project1/buz.py")
          (set-buffer "buz.py")
          (should (equal (gpc-val 'pipenv-project-root npy-env) (npy-helper-in-playground "project1")))
          (npy-run-python t)
          (npy-helper-wait)
          (let-to-kill ((inf-buf (get-buffer "*Python[Pipenv:project1;b:buz.py]*")))
            (set-buffer inf-buf)
            (npy-scratch)
            (let-to-kill ((scratch-buf (get-buffer "*pyscratch[Pipenv:project1]*")))
              (should-not (eq scratch-buf nil)))))))
    (it "spawns a virtualenv dedicated python scratch buffer
    dedicated to the same virtualenv"
      (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
        (with-file-buffers ("project1/buz.py")
          (set-buffer "buz.py")
          (should (equal (gpc-val 'pipenv-project-root npy-env) (npy-helper-in-playground "project1")))
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
              "print(VAR2)\n" "from scratch"))))))
  (describe "when called on a virtualenv-buffer dedicated inferior python buffer"
    (it "spawns a virtualenv-buffer dedicated python scratch
    buffer dedicated to the same virtualenv and the buffer"
      (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
        (with-file-buffers ("project1/buz.py")
          (set-buffer "buz.py")
          (should (equal (gpc-val 'pipenv-project-root npy-env) (npy-helper-in-playground "project1")))
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
                "print(VAR3)\n" "from scratch"))))))))

(describe "npy-run-python"
  (describe "when called on a virtualenv dedicated python scratch buffer"
    (it "spawns a virtualenv dedicated inferior python buffer
    dedicated to the same virtualenv"
      (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
        (with-file-buffers ("project1/buz.py")
          (set-buffer "buz.py")
          (should (equal (gpc-val 'pipenv-project-root npy-env) (npy-helper-in-playground "project1")))
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
    (describe "when called on a virtualenv-buffer dedicated
    python scratch buffer with dedicated set to t"
      (it "spawns a virtualenv-buffer dedicated inferior python
          buffer dedicated to the same virtualenv and buffer"
        (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
          (with-file-buffers ("project1/buz.py")
            (set-buffer "buz.py")
            (should (equal (gpc-val 'pipenv-project-root npy-env) (npy-helper-in-playground "project1")))
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
                  "print(VAR7)\n" "from scratch")))))))))

(provide 'npy-scratch-additional-test)
;;; npy-scratch-additional-test.el ends here
