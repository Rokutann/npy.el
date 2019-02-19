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
     ;;(delete-directory npy-test/playground-path t)
     ))

(ert-deftest npy-test-sample ()
  (with-playground '(("foo")
                     ("bar.py" . "TUP = (1, 2) ")
                     ("project/buz.py" . "VAR = 1"))
    (write-region (buffer-name) nil "/tmp/npy")
    (setq default-directory (concat npy-test/playground-path "project/"))
    (write-region default-directory nil "/tmp/npy2")
    (call-process "pipenv" nil t nil "install")
    (setq default-directory npy-test/playground-path)
    (write-region default-directory nil "/tmp/npy3")
    (unwind-protect
        (progn
          (find-file "/tmp/npy-test/project/buz.py")
          (write-region (buffer-name) nil "/tmp/npy4")
          (write-region (format "%s" npy--pipenv-project-root) nil "/tmp/npy5")

          )
      (setq default-directory (concat npy-test/playground-path "project/"))
      (call-process "pipenv" nil t nil "--rm"))))

(provide 'npy-test)
;;; npy-test.el ends here
