;;; npy-scratch-short-test.el --- New tests for npy.el.        -*- lexical-binding: t; -*-

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


(ert-deftest npy-integration-test/spawn-a-virtualenv-buffer-dedicated-scratch-and-spawn-a-dedicated-inf-buf-on-it ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
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
            "print(VAR7)\n" "from scratch"))))))

(ert-deftest npy-integration-test/spawn-an-virtualenv-buffer-dedicated-inf-buf-and-spawn-a-virtualenv-buffer-dedicated-scratch-on-it-but-get-normal ()
  (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
    (with-file-buffers ("project1/buz.py")
      (set-buffer "buz.py")
      (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
      (npy-run-python t)
      (npy-helper-wait)
      (let-to-kill ((inf-buf (get-buffer "*Python[Pipenv:project1;b:buz.py]*")))
        (set-buffer inf-buf)
        (npy-scratch t)
        (let-to-kill ((scratch-buf (get-buffer "*pyscratch[Pipenv:project1;b:buz.py]*")))
          (npy-helper-write "VAR5 = \"from scratch\"\n" scratch-buf)
          (with-current-buffer scratch-buf
            (python-shell-send-buffer))
          (should-response-match inf-buf
            "print(VAR5)\n" "from scratch"))))))

(provide 'npy-scratch-short-test)
;;; npy-new-test.el ends here
