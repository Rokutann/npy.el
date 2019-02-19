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

(defmacro with-files-in-playground (filespec &rest body)
  "Execute BODY in the playground specified by FILESPEC."
  (declare (indent 1))
  `(unwind-protect
       (progn
         (npy-helper-create-files npy-test/playground-path
                                  ,filespec)
         ,@body)
     ;;(delete-directory npy-test/playground-path t)
     ))

(ert-deftest npy-test-sample ()
  (with-files-in-playground
      '(("foo")
        ("bar.py" . "TUP = (1, 2) ")
        ("project1/buz.py" . "VAR = 1")
        ("project3/foo.py" . "VAR = 2"))
    (find-file (concat npy-test/playground-path "project1/buz.py"))
    (should (equal npy--pipenv-project-root (concat npy-test/playground-path "project1")))
    (find-file (concat npy-test/playground-path "project3/foo.py"))
    (should (eq npy--pipenv-project-root 'no-virtualenv))
    ))

(provide 'npy-test)
;;; npy-test.el ends here
