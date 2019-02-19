;;; test-helper.el --- npy: Test helpers.            -*- lexical-binding: t; -*-

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

;; Test helpers for npy.

;;; Code:

(defun npy-helper-create-files (basedir filespec)
  "In BASEDIR, create files according to FILESPEC.

FILESPEC is a list of two-element lists, where the first element
is a file name relative to BASEDIR and the second the content
for that file."
  (dolist (spec filespec)
    (let* ((filename (if (stringp spec)
                         spec
                       (car spec)))
           (content (if (stringp spec)
                         ""
                       (cadr spec)))
           (fullname (format "%s/%s" basedir filename))
           (dirname (file-name-directory fullname)))
      (when (not (file-directory-p dirname))
        (make-directory dirname t))
      (write-region content nil fullname))))

(provide 'test-helper)
;;; test-helper.el ends here
