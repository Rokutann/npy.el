;;; npy-helper-test.el --- Testing for npy-helper functions.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Cyriakus "Mukuge" Hill

;; Author: Cyriakus "Mukuge" Hill <cyriakus.h@gmail.com>
;; Keywords: tools

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

;; Testing for npy-helper functions.

;;; Code:

(require 'cl-lib)

(defvar npy-helper-test-root
  "/tmp/npy-helper-test"
  "The root directory of npy helper tests.")

(describe "npy-helper-file-string"
  (it "returns the content of a file."
    (call-process-shell-command (format "mkdir -p %s" npy-helper-test-root))
    (call-process-shell-command (format "echo -n \"test\" > %s/file-a.py" npy-helper-test-root))
    (expect (npy-helper-file-string (format "%s/file-a.py" npy-helper-test-root)) :to-equal "test")
    (call-process-shell-command (format "rm %s/file-a.py" npy-helper-test-root)))
  (it "returns \"\" when the file at the path is empty."
    (call-process-shell-command (format "rm %s/file-b.py" npy-helper-test-root))
    (call-process-shell-command (format "touch %s/file-b.py" npy-helper-test-root))
    (expect (npy-helper-file-string (format "%s/file-b.py" npy-helper-test-root)) :to-equal "")
    (call-process-shell-command (format "rm %s/file-b.py" npy-helper-test-root))))

(describe "npy-helper-create-files"
  (it "creates an empty file when filespec is a string like (\"file-a.py\")."
    (npy-helper-create-files "/tmp/npy-helper-test" '("file-a.py"))
    (expect (f-exists-p (format "%s/file-a.py" npy-helper-test-root)) :to-be-truthy)
    (expect (f-size "/tmp/npy-helper-test/file-a.py") :to-equal 0)
    (call-process-shell-command (format "rm %s/file-a.py" npy-helper-test-root)))
  (it "creates a file with a content when filespec is an alist like ((\"file-b.py\" . \"import sys\\n\")."
    (npy-helper-create-files npy-helper-test-root '(("file-b.py" . "import sys\n")))
    (expect (f-exists-p (format "%s/file-b.py" npy-helper-test-root)) :to-be-truthy)
    (expect (npy-helper-file-string (format "%s/file-b.py" npy-helper-test-root)) :to-equal "import sys\n")
    (call-process-shell-command (format "rm %s/file-b.py" npy-helper-test-root)))
  (it "creates two files with a filespec (\"file-a.py\" (\"file-b.py\" . \"import sys\\n\")."
    (npy-helper-create-files npy-helper-test-root '("file-a.py" ("file-b.py" . "import sys\n")))
    (expect (f-exists-p (format "%s/file-a.py" npy-helper-test-root)) :to-be-truthy)
    (expect (f-size "/tmp/npy-helper-test/file-a.py") :to-equal 0)
    (expect (f-exists-p (format "%s/file-b.py" npy-helper-test-root)) :to-be-truthy)
    (expect (npy-helper-file-string (format "%s/file-b.py" npy-helper-test-root)) :to-equal "import sys\n")
    (call-process-shell-command (format "rm %s/file-b.py" npy-helper-test-root))
    (call-process-shell-command (format "rm %s/file-a.py" npy-helper-test-root))))

(describe "npy-helper-delete-files"
  (it "deletes an empty file when filespec is a string like (\"file-a.py\")."
    (npy-helper-create-files npy-helper-test-root '("file-a.py"))
    (npy-helper-delete-files npy-helper-test-root '("file-a.py"))
    (expect (f-exists-p (format "%s/file-a.py" npy-helper-test-root)) :not :to-be-truthy))
  (it "deletes a file when filespec is an alist like ((\"file-b.py\" . \"import sys\\n\")."
    (npy-helper-create-files npy-helper-test-root '(("file-b.py" . "import sys\n")))
    (npy-helper-delete-files npy-helper-test-root '(("file-b.py" . "import sys\n")))
    (expect (f-exists-p (format "%s/file-b.py" npy-helper-test-root)) :not :to-be-truthy))
  (it "deletes two files when filespec is (\"file-a.py\" (\"file-b.py\" . \"import sys\\n\")."
    (npy-helper-create-files npy-helper-test-root '("file-a.py" ("file-b.py" . "import sys\n")))
    (npy-helper-delete-files npy-helper-test-root '("file-a.py" ("file-b.py" . "import sys\n")))
    (expect (f-exists-p (format "%s/file-a.py" npy-helper-test-root)) :not :to-be-truthy)
    (expect (f-exists-p (format "%s/file-b.py" npy-helper-test-root)) :not :to-be-truthy)))

(describe "npy-helper-in-playground"
  (it "returns `npy-test/playground-path' when it has no argument."
    (expect (npy-helper-in-playground) :to-equal npy-test/playground-path))
  (it "returns (concat npy-test/playground-path \"foo.py\") when its argument is \"foo.py\""
    (expect (npy-helper-in-playground "foo.py") :to-equal (concat npy-test/playground-path "foo.py"))))

(describe "with-files-in-playground"
  (it "executes the body argument with files specified by the filefpec argument and delete all the files."
    (with-files-in-playground ("file-a.py" ("file-b.py" . "import sys\n"))
      (expect (f-exists-p (format "%s/file-a.py" npy-test/playground-path)) :to-be-truthy)
      (expect (f-exists-p (format "%s/file-b.py" npy-test/playground-path)) :to-be-truthy))
    (expect (f-exists-p (format "%s/file-a.py" npy-test/playground-path)) :not :to-be-truthy)
    (expect (f-exists-p (format "%s/file-b.py" npy-test/playground-path)) :not :to-be-truthy))
  (it "unwind-protects its file deletion."
    (condition-case nil
        (with-files-in-playground ("file-a.py" ("file-b.py" . "import sys\n"))
          (expect (f-exists-p (format "%s/file-a.py" npy-test/playground-path)) :to-be-truthy)
          (expect (f-exists-p (format "%s/file-b.py" npy-test/playground-path)) :to-be-truthy)
          (/ 1 0))
      (error nil))
    (expect (f-exists-p (format "%s/file-a.py" npy-test/playground-path)) :not :to-be-truthy)
    (expect (f-exists-p (format "%s/file-b.py" npy-test/playground-path)) :not :to-be-truthy)))

(describe "npy-helper-kill-python-buffer"
  (it "kills a `python-mode' buffer."
    (let ((buf (find-file-noselect (format "%s/file-a.py" npy-helper-test-root))))
      (expect (buffer-live-p buf) :to-be-truthy)
      (expect (cl-some #'(lambda (b)
                           (string= (buffer-file-name b)
                                    (format "%s/file-a.py" npy-helper-test-root)))
                       (buffer-list))
              :to-be-truthy)
      (npy-helper-kill-python-buffer buf)
      (expect (buffer-live-p buf) :not :to-be-truthy)
      (expect (cl-some #'(lambda (b)
                           (string= (buffer-file-name b)
                                    (format "%s/file-a.py" npy-test/playground-path)))
                       (buffer-list))
              :not :to-be-truthy))))

(describe "npy-helper-find-file-in-playground"
  (it "create a buffer visiting a file designated by the path argument which is relative to npy-test/playground-path."
    (let ((buf (npy-helper-find-file-in-playground "file-a.py")))
      (expect (buffer-file-name buf) :to-equal (concat npy-test/playground-path "file-a.py"))
      (npy-helper-kill-python-buffer buf)))
  (it "create a buffer visiting a directory designated by the path argument which is relative to npy-test/playground-path."
    (let ((buf (npy-helper-find-file-in-playground "project1")))
      (expect (buffer-file-name buf) :to-be nil)
      (expect (with-current-buffer buf (derived-mode-p 'dired-mode)) :not :to-be nil)
      (npy-helper-kill-python-buffer buf))))

(describe "npy-helper-kill-inferior-python-buffer"
  (it "kills an inferior python buffer without asking if you really want to kill a buffer with a process."
    (run-python)
    (npy-helper-wait)
    (expect (cl-some #'(lambda (b)
                         (string= (buffer-name b) "*Python*"))
                     (buffer-list)) :to-be-truthy)
    (npy-helper-kill-inferior-python-buffer "*Python*")
    (expect (cl-some #'(lambda (b)
                         (string= (buffer-name b) "*Python*"))
                     (buffer-list)) :not :to-be-truthy)))

(describe "npy-helper-kill-inferior-python-buffers"
  (it "kills an inferior python buffer without asking if you really want to kill a buffer with a process."
    (run-python)
    (npy-helper-wait)
    (expect (cl-some #'(lambda (b)
                         (string= (buffer-name b) "*Python*"))
                     (buffer-list)) :to-be-truthy)
    (npy-helper-kill-inferior-python-buffers "*Python*")
    (expect (cl-some #'(lambda (b)
                         (string= (buffer-name b) "*Python*"))
                     (buffer-list)) :not :to-be-truthy))
  (it "kills inferior python buffers without asking if you really want to kill buffers with processes."
    (let ((py-buf-a (npy-helper-find-file-in-playground "file-a.py"))
          (py-buf-b (npy-helper-find-file-in-playground "file-b.py")))
      (with-current-buffer py-buf-a
        (run-python (python-shell-calculate-command) t))
      (with-current-buffer py-buf-b
        (run-python (python-shell-calculate-command) t))
      (npy-helper-wait)
      (expect (cl-some #'(lambda (b)
                           (string= (buffer-name b) "*Python[file-a.py]*"))
                       (buffer-list)) :to-be-truthy)
      (expect (cl-some #'(lambda (b)
                           (string= (buffer-name b) "*Python[file-b.py]*"))
                       (buffer-list)) :to-be-truthy)
      (npy-helper-kill-inferior-python-buffers "*Python[file-a.py]*" "*Python[file-b.py]*")
      (expect (cl-some #'(lambda (b)
                           (string= (buffer-name b) "*Python[file-a.py]*"))
                       (buffer-list)) :not :to-be-truthy)
      (expect (cl-some #'(lambda (b)
                           (string= (buffer-name b) "*Python[file-b.py]*"))
                       (buffer-list)) :not :to-be-truthy)
      (npy-helper-kill-python-buffer py-buf-a)
      (npy-helper-kill-python-buffer py-buf-b)))
  (it "ignores a dead buffer."
    (run-python)
    (npy-helper-wait)
    (let ((buf (get-buffer "*Python*")))
      (expect (cl-some #'(lambda (b)
                           (string= (buffer-name b) "*Python*"))
                       (buffer-list)) :to-be-truthy)
      (npy-helper-kill-inferior-python-buffers buf)
      (expect (cl-some #'(lambda (b)
                           (string= (buffer-name b) "*Python*"))
                       (buffer-list)) :not :to-be-truthy)
      (expect (buffer-live-p buf) :not :to-be-truthy)
      (expect (npy-helper-kill-inferior-python-buffers buf) :not :to-throw))))

(describe "npy-helper-kill-pythonic-buffer"
  (it "kills an inferior python buffer without asking if you really want to kill a buffer with a process."
    (run-python)
    (npy-helper-wait)
    (expect (cl-some #'(lambda (b)
                         (string= (buffer-name b) "*Python*"))
                     (buffer-list)) :to-be-truthy)
    (npy-helper-kill-pythonic-buffer (get-buffer "*Python*"))
    (expect (cl-some #'(lambda (b)
                         (string= (buffer-name b) "*Python*"))
                     (buffer-list)) :not :to-be-truthy))
  (it "kills a `python-mode' buffer."
    (let ((buf (find-file-noselect (format "%s/file-a.py" npy-helper-test-root))))
      (expect (cl-some #'(lambda (b)
                           (string= (buffer-file-name b)
                                    (format "%s/file-a.py" npy-helper-test-root)))
                       (buffer-list))
              :to-be-truthy)
      (npy-helper-kill-pythonic-buffer buf)
      (expect (cl-some #'(lambda (b)
                           (string= (buffer-file-name b)
                                    (format "%s/file-a.py" npy-test/playground-path)))
                       (buffer-list))
              :not :to-be-truthy))))

(describe "npy-helper-file-string"
  (it "reads the desinated file and returns its contetent as a string."
    (with-files-in-playground ("file-a.py" ("file-b.py" . "123"))
      (expect (npy-helper-file-string (npy-helper-in-playground "file-a.py"))
              :to-equal "")
      (expect (npy-helper-file-string (npy-helper-in-playground "file-b.py"))
              :to-equal "123")
      )))

(describe "npy-helper-write"
  (it "inserts a string in a buffer."
    (let ((py-buf (npy-helper-find-file-in-playground "file.py")))
      (expect (with-current-buffer py-buf (buffer-string)) :to-equal "")
      (npy-helper-write "import sys\n" py-buf)
      (expect (with-current-buffer py-buf (buffer-string)) :to-equal "import sys\n")
      (kill-buffer py-buf))))

(provide 'npy-helper-test)
;;; npy-helper-test.el ends here
