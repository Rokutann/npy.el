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

(ert-deftest npy-integration-test/open-a-file-in-a-project ()
  (with-files-in-playground
   '(("project1/buz.py" . "VAR = 1"))
   (@-find-file "project1/buz.py")
   (should (equal npy--pipenv-project-root (@- "project1")))
   (kill-buffer "buz.py")
   ))

(ert-deftest npy-integration-test/open-a-file-deep-in-a-project ()
  (with-files-in-playground
   '(("project2/deep/in/the/project/buz.py" . "VAR = 1"))
   (@-find-file "project2/deep/in/the/project/buz.py")
   (should (equal npy--pipenv-project-root (@- "project2")))
   (kill-buffer "buz.py")
   ))

(ert-deftest npy-integration-test/open-a-non-project-file ()
  (with-files-in-playground
   '(("project3/foo.py" . "VAR = 2"))
   (@-find-file "project3/foo.py")
   (should (eq npy--pipenv-project-root 'no-virtualenv))
   (kill-buffer "foo.py")
   ))

(ert-deftest npy-integration-test/open-two-files-in-a-project ()
  (with-files-in-playground
   '(("project1/buz.py" . "VAR = 1")
     ("project1/foo.py" . "VAR = 2"))
   (@-find-file "project1/buz.py")
   (should (equal npy--pipenv-project-root (@- "project1")))
   (@-find-file "project1/foo.py")
   (should (equal npy--pipenv-project-root (@- "project1")))
   (kill-buffer "buz.py")
   (kill-buffer "foo.py")
   ))

(ert-deftest npy-integration-test/open-two-files-in-different-projects ()
  (with-files-in-playground
   '(("project1/buz.py" . "VAR = 1")
     ("project2/foo.py" . "VAR = 2"))
   (@-find-file "project1/buz.py")
   (should (equal npy--pipenv-project-root (@- "project1")))
   (@-find-file "project2/foo.py")
   (should (equal npy--pipenv-project-root (@- "project2")))
   (kill-buffer "buz.py")
   (kill-buffer "foo.py")
   ))

(ert-deftest npy-integration-test/spawn-an-inferior-python-buffer ()
  (with-files-in-playground
   '(("project1/buz.py" . "VAR = 1"))
   (@-find-file "project1/buz.py")
   (should (equal npy--pipenv-project-root (@- "project1")))
   (npy-run-python)
   (should-not (eq (get-buffer "*Python[v:project1]*") nil))
   (kill-buffer "buz.py")
   ))


(provide 'npy-test)
;;; npy-test.el ends here
