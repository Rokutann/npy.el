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

(defmacro with-playground (filespec &rest body)
  "Execute BODY in the playground specified by FILESPEC."
  (declare (indent 1))
  `(unwind-protect
       (save-excursion
         (setq defalut-directory npy-test/playground-path)
         (npy-helper-create-files npy-test/playground-path
                                  ,filespec)
         ,@body)
     (delete-directory npy-test/playground-path t)))

(ert-deftest npy-test-sample ()
  (with-playground '(("foo")
                     ("bar.py" . "(1, 2)")
                     ("buz.el" . "(message default-directory)"))
    (find-file-noselect "/tmp/npy-test/buz.el")
    (eval-buffer)
    (kill-buffer)))

(provide 'npy-test)
;;; npy-test.el ends here
