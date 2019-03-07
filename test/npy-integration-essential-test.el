;;; npy-integration-essential-test.el --- npy: Tests.                      -*- lexical-binding: t; -*-

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

;; Tests for npy.

;;; Code:


;; The Map of the Playground
;;
;; /tmp/npy-playground/project1 <- a Pipenv project
;; /tmp/npy-playground/project1/test
;; /tmp/npy-playground/project1/lib
;;
;; /tmp/npy-playground/project2 <- a Pipenv project
;; /tmp/npy-playground/project2/deep/in/the/project
;;
;; /tmp/npy-playground/project3 <- not a Pipenv project

;; For executing tests in Emacs manually.
;; (setq npy-test/playground-path "/tmp/npy-playground/")

(ert-deftest npy-integration-test/spawn-two-inferior-python-buffers/send-two-strings-each-in-a-different-venv ()
  (with-files-in-playground (("project1/buz.py" . "VAR = \"from buz.py\"")
                             ("project2/foo.py" . "VAR = \"from foo.py\""))
    (with-file-buffers ("project1/buz.py" "project2/foo.py")
      (with-current-buffer "buz.py"
        (npy-run-python))
      (with-current-buffer "foo.py"
        (npy-run-python))
      (npy-helper-wait)
      (let ((python-inf-buf-1 (get-buffer "*Python[Pipenv:project1]*"))
            (python-inf-buf-2 (get-buffer "*Python[Pipenv:project2]*")))
        (with-current-buffer "buz.py"
          (python-shell-send-buffer))
        (with-current-buffer "foo.py"
          (python-shell-send-buffer))
        (should-response-match python-inf-buf-1
          "print(VAR)\n" "from buz.py")
        (should-response-match python-inf-buf-2
          "print(VAR)\n" "from foo.py")
        (npy-helper-kill-python-inferior-buffers python-inf-buf-1 python-inf-buf-2)))))

(ert-deftest npy-integration-test/spawn-three-inferior-python-buffers/send-strings-in-a-venv ()
  (with-files-in-playground (("project1/buz.py" . "VAR = \"from buz.py\"")
                             ("project1/foo.py" . "VAR = \"from foo.py\"")
                             ("project2/bar.py" . "VAR2 = \"from bar.py\""))
    (with-file-buffers ("project1/buz.py" "project1/foo.py" "project2/bar.py")
      (with-current-buffer "buz.py"
        (npy-run-python))
      (with-current-buffer "foo.py"
        (npy-run-python))
      (with-current-buffer "bar.py"
        (npy-run-python))
      (npy-helper-wait)
      (let ((python-inf-buf-1 (get-buffer "*Python[Pipenv:project1]*"))
            (python-inf-buf-2 (get-buffer "*Python[Pipenv:project2]*")))
        (with-current-buffer "foo.py"
          (python-shell-send-buffer))
        (should-response-match python-inf-buf-1
          "print(VAR)\n" "from foo.py")
        (with-current-buffer "bar.py"
          (python-shell-send-buffer))
        (should-response-match python-inf-buf-2
          "print(VAR2)\n" "from bar.py")
        (npy-helper-kill-python-inferior-buffers python-inf-buf-1 python-inf-buf-2)))))

(ert-deftest npy-integration-test/spawn-two-venv-dedicated-inf-buffers ()
  (with-files-in-playground (("project1/buz.py" . "VAR = \"from buz.py\"")
                             ("project1/foo.py" . "VAR = \"from foo.py\""))
    (with-file-buffers ("project1/buz.py" "project1/foo.py")
      (with-current-buffer "buz.py"
        (npy-run-python t))
      (with-current-buffer "foo.py"
        (npy-run-python t))
      (npy-helper-wait)
      (let ((python-inf-buf-1 (get-buffer "*Python[Pipenv:project1;b:buz.py]*"))
            (python-inf-buf-2 (get-buffer "*Python[Pipenv:project1;b:foo.py]*")))
        (with-current-buffer "buz.py"
          (python-shell-send-buffer))
        (with-current-buffer "foo.py"
          (python-shell-send-buffer))
        (should-response-match python-inf-buf-1
          "print(VAR)\n" "from buz.py")
        (should-response-match python-inf-buf-2
          "print(VAR)\n" "from foo.py")
        (npy-helper-kill-python-inferior-buffers python-inf-buf-1 python-inf-buf-2)))))

(provide 'npy-integration-essential-test)
;;; npy-integration-essential-test.el ends here
