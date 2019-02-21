;;; npy-test.el --- npy: Tests.                      -*- lexical-binding: t; -*-

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

(setq npy-test/venv-root-for-project1 (s-chomp (shell-command-to-string "(cd /tmp/npy-playground/project1/ && pipenv --venv)")))
(setq npy-test/venv-root-for-project2 (s-chomp (shell-command-to-string "(cd /tmp/npy-playground/project2/ && pipenv --venv)")))

(ert-deftest npy-integration-test/open-a-file-in-a-project/pipenv-project-root ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal npy--pipenv-project-root (@- "project1"))))))

(ert-deftest npy-integration-test/open-a-file-in-a-project/pipenv-virtualenv-root ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (npy-update-pipenv-virtualenv-root)
      (should (equal npy--pipenv-virtualenv-root npy-test/venv-root-for-project1)))))

(ert-deftest npy-integration-test/open-a-file-deep-in-a-project ()
  (with-files-in-playground (("project2/deep/in/the/project/buz.py" . "VAR = 1"))
    (with-file-buffers ("project2/deep/in/the/project/buz.py")
      (set-buffer "buz.py")
      (should (equal npy--pipenv-project-root (@- "project2"))))))

(ert-deftest npy-integration-test/open-a-non-project-file ()
  (with-files-in-playground (("project3/foo.py" . "VAR = 2"))
    (with-file-buffers ("project3/foo.py")
      (set-buffer "foo.py")
      (should (eq npy--pipenv-project-root 'no-virtualenv)))))

(ert-deftest npy-integration-test/open-two-files-in-a-project ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1")
                             ("project1/foo.py" . "VAR = 2"))
    (with-file-buffers ("project1/buz.py" "project1/foo.py")
      (set-buffer "buz.py")
      (should (equal npy--pipenv-project-root (@- "project1")))
      (set-buffer "foo.py")
      (should (equal npy--pipenv-project-root (@- "project1"))))))

(ert-deftest npy-integration-test/open-two-files-in-different-projects/pipenv-project-root ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1")
                             ("project2/foo.py" . "VAR = 2"))
    (with-file-buffers ("project1/buz.py" "project2/foo.py")
      (set-buffer "buz.py")
      (should (equal npy--pipenv-project-root (@- "project1")))
      (set-buffer "foo.py")
      (should (equal npy--pipenv-project-root (@- "project2"))))))

(ert-deftest npy-integration-test/open-two-files-in-different-projects/pipenv-virtualenv-root ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1")
                             ("project2/foo.py" . "VAR = 2"))
    (with-file-buffers ("project1/buz.py" "project2/foo.py")
      (set-buffer "buz.py")
      (npy-update-pipenv-virtualenv-root)
      (should (equal npy--pipenv-virtualenv-root npy-test/venv-root-for-project1))
      (set-buffer "foo.py")
      (npy-update-pipenv-virtualenv-root)
      (should (equal npy--pipenv-virtualenv-root npy-test/venv-root-for-project2)))))

(ert-deftest npy-integration-test/spawn-an-inferior-python-buffer/check-buffer-name ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal npy--pipenv-project-root (@- "project1")))
      (npy-run-python)
      (npy-helper-wait)
      (let ((python-inf-buf (get-buffer "*Python[v:project1]*")))
        (should-not (eq  python-inf-buf nil))
        (npy-helper-kill-python-inferior-buffers python-inf-buf)))))

(ert-deftest npy-integration-test/spawn-an-inferior-python-buffer/check-sys-path ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal npy--pipenv-project-root (@- "project1")))
      (npy-run-python)
      (npy-helper-wait)
      (let ((python-inf-buf (get-buffer "*Python[v:project1]*")))
        (should-not (eq  python-inf-buf nil))
        (should-response-match python-inf-buf
          "import sys\nprint(sys.path)\n" npy-test/venv-root-for-project1)
        (npy-helper-kill-python-inferior-buffers python-inf-buf)))))

(ert-deftest npy-integration-test/spawn-two-inferior-python-buffers/check-sys-path ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1")
                             ("project2/foo.py" . "VAR = 2"))
    (with-file-buffers ("project1/buz.py" "project2/foo.py")
      (with-current-buffer "buz.py"
        (npy-run-python))
      (with-current-buffer "foo.py"
        (npy-run-python))
      (npy-helper-wait)
      (let ((python-inf-buf-1 (get-buffer "*Python[v:project1]*"))
            (python-inf-buf-2 (get-buffer "*Python[v:project2]*")))
        (should-response-match python-inf-buf-1
          "import sys\nprint(sys.path)\n" npy-test/venv-root-for-project1)
        (should-response-match python-inf-buf-2
          "import sys\nprint(sys.path)\n" npy-test/venv-root-for-project2)
        (npy-helper-kill-python-inferior-buffers python-inf-buf-1 python-inf-buf-2)))))

(provide 'npy-test)
;;; npy-test.el ends here
