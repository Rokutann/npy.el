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

(ert-deftest npy-test-sample-1 ()
  (with-files-in-playground
      '(("foo")
        ("bar.py" . "TUP = (1, 2)")
        ("project1/buz.py" . "VAR = 1")
        ("project3/foo.py" . "VAR = 2"))
    (@-find-file "project1/buz.py")
    (should (equal npy--pipenv-project-root (@- "project1")))
    ))

(ert-deftest npy-test-sample-2 ()
  (with-files-in-playground
      '(("foo")
        ("bar.py" . "TUP = (1, 2)")
        ("project1/buz.py" . "VAR = 1")
        ("project3/foo.py" . "VAR = 2"))
    (@-find-file "project3/foo.py")
    (should (eq npy--pipenv-project-root 'no-virtualenv))
    ))

(provide 'npy-test)
;;; npy-test.el ends here
