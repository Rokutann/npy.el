;;; npy-env-test.el ---  Tests for npy-env.  -*- lexical-binding: t; -*-

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

;; Tests for npy-env.

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

(describe "npy-env and related buffer-local variables"
  (describe "one buffer cases:"
    (describe "when spawn a python-mode buffer visiting a file in a Pipenv project,"
      (it "are associated with the Pipenv project."
        (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
          (with-file-buffers ("project1/buz.py")
            (with-current-buffer "buz.py"
              (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project1"))
              (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project1")
              (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project1)
              (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project1)
              (expect npy-buffer-scratch :to-be nil)
              (expect npy-buffer-shell-initialized :to-be nil)
              (expect npy-buffer-dedicated-to :to-be nil)
              (expect (buffer-name npy-buffer-child-dedicatable-to) :to-equal "buz.py")))))
      (xit "are associated with the Pipenv project even when the file is located deep inside the project directory."
        (with-files-in-playground (("project2/deep/in/the/project/buz.py" . "VAR = 1"))
          (with-file-buffers ("project2/deep/in/the/project/buz.py")
            (with-current-buffer "buz.py"
              (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project2"))
              (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project2")
              (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project2)
              ;; FIXME: Currently, this value is :no-pipenv-project.
              (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project2)
              (expect npy-buffer-scratch :to-be nil)
              (expect npy-buffer-shell-initialized :to-be nil)
              (expect npy-buffer-dedicated-to :to-be nil)
              (expect (buffer-name npy-buffer-child-dedicatable-to) :to-equal "buz.py"))))))
    (describe "when spawn a non python-mode buffer visiting a file in a Pipenv project,"
      (it "are associated with the Pipenv project."
        (with-files-in-playground (("project1/Makefile" . "test-doctest:\n\tpytest --doctest-modules .\n"))
          (with-file-buffers ("project1/Makefile")
            (with-current-buffer "Makefile"
              (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project1"))
              (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project1")
              (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project1)
              (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project1)
              (expect npy-buffer-scratch :to-be nil)
              (expect npy-buffer-shell-initialized :to-be nil)
              (expect npy-buffer-dedicated-to :to-be nil)
              (expect npy-buffer-child-dedicatable-to :to-be nil))))))
    (describe "when spawn a dired-mode buffer visiting a directory in a Pipenv project,"
      (it "are associated with the Pipenv project."
        (with-file-buffers ("project1")
          (with-current-buffer "project1"
            (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project1"))
            (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project1")
            (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project1)
            (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project1)
            (expect npy-buffer-scratch :to-be nil)
            (expect npy-buffer-shell-initialized :to-be nil)
            (expect npy-buffer-dedicated-to :to-be nil)
            (expect npy-buffer-child-dedicatable-to :to-be nil))))
      (xit "are associated with the Pipenv project even when the file is located deep inside the project directory."
        (with-file-buffers ("project2/deep/in/the/project")
          (with-current-buffer "project"
            (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project2"))
            (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project2")
            (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project2)
            ;; FIXME: Currently, this value is :no-pipenv-project.
            (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project2)
            (expect npy-buffer-scratch :to-be nil)
            (expect npy-buffer-shell-initialized :to-be nil)
            (expect npy-buffer-dedicated-to :to-be nil)
            (expect npy-buffer-child-dedicatable-to :to-be nil)))))
    (describe "when spawn a python-mode buffer visiting a file not in a Pipenv project,"
      (it "are not associated with any Pipenv projects."
        (with-files-in-playground (("project3/foo.py" . "VAR = 2"))
          (with-file-buffers ("project3/foo.py")
            (with-current-buffer "foo.py"
              (expect (gpc-val 'pipenv-project-root npy-env) :to-be :no-pipenv-project)
              (expect (gpc-val 'pipenv-project-name npy-env) :to-be :no-pipenv-project)
              (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-be :no-pipenv-project)
              (expect python-shell-virtualenv-root :to-be nil)
              (expect npy-buffer-scratch :to-be nil)
              (expect npy-buffer-shell-initialized :to-be nil)
              (expect npy-buffer-dedicated-to :to-be nil)
              ;;(expect npy-buffer-child-dedicatable-to :to-be nil) ;; FIXME: Currently this value is foo.py.
              )))))
    (describe "when spawn a non python-mode buffer visiting a file not in a Pipenv project,"
      (it "are not associated with any Pipenv projects."
        (with-files-in-playground (("project3/Makefile" . "test-doctest:\n\tpytest --doctest-modules .\n"))
          (with-file-buffers ("project3/Makefile")
            (with-current-buffer "Makefile"
              (expect (gpc-val 'pipenv-project-root npy-env) :to-be :no-pipenv-project)
              (expect (gpc-val 'pipenv-project-name npy-env) :to-be :no-pipenv-project)
              (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-be :no-pipenv-project)
              (expect python-shell-virtualenv-root :to-be nil)
              (expect npy-buffer-scratch :to-be nil)
              (expect npy-buffer-shell-initialized :to-be nil)
              (expect npy-buffer-dedicated-to :to-be nil)
              (expect npy-buffer-child-dedicatable-to :to-be nil)
              )))))
    (describe "when spawn a dired-mode buffer visiting a directory not in a Pipenv project,"
      (it "are not associated with any Pipenv projects."
        (with-file-buffers ("project3")
          (with-current-buffer "project3"
            (expect (gpc-val 'pipenv-project-root npy-env) :to-be :no-pipenv-project)
            (expect (gpc-val 'pipenv-project-name npy-env) :to-be :no-pipenv-project)
            (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-be :no-pipenv-project)
            (expect python-shell-virtualenv-root :to-be nil)
            (expect npy-buffer-scratch :to-be nil)
            (expect npy-buffer-shell-initialized :to-be nil)
            (expect npy-buffer-dedicated-to :to-be nil)
            (expect npy-buffer-child-dedicatable-to :to-be nil))))))
  (describe "two buffer cases:"
    (describe "when spawn two python-mode buffers for a same Pipenv project successively,"
      (it "for the two buffers are associated with the same Pipenv project."
        (with-files-in-playground (("project1/buz.py" . "VAR = 1")
                                   ("project1/foo.py" . "VAR = 2"))
          (with-file-buffers ("project1/buz.py" "project1/foo.py")
            (with-current-buffer "buz.py"
              (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project1"))
              (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project1")
              (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project1)
              (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project1)
              (expect npy-buffer-scratch :to-be nil)
              (expect npy-buffer-shell-initialized :to-be nil)
              (expect npy-buffer-dedicated-to :to-be nil)
              (expect (buffer-name npy-buffer-child-dedicatable-to) :to-equal "buz.py"))
            (with-current-buffer "foo.py"
              (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project1"))
              (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project1")
              (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project1)
              (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project1)
              (expect npy-buffer-scratch :to-be nil)
              (expect npy-buffer-shell-initialized :to-be nil)
              (expect npy-buffer-dedicated-to :to-be nil)
              (expect (buffer-name npy-buffer-child-dedicatable-to) :to-equal "foo.py"))))))
    (describe "when spawn two python-mode buffers visiting files in different Pipenv projects successively,"
      (it "for the two buffers are associated with the Pipenv projects respectively"
        (with-files-in-playground (("project1/buz.py" . "VAR = 1")
                                   ("project2/foo.py" . "VAR = 2"))
          (with-file-buffers ("project1/buz.py" "project2/foo.py")
            (with-current-buffer "buz.py"
              (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project1"))
              (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project1")
              (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project1)
              (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project1)
              (expect npy-buffer-scratch :to-be nil)
              (expect npy-buffer-shell-initialized :to-be nil)
              (expect npy-buffer-dedicated-to :to-be nil)
              (expect (buffer-name npy-buffer-child-dedicatable-to) :to-equal "buz.py"))
            (with-current-buffer "foo.py"
              (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project2"))
              (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project2")
              (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project2)
              (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project2)
              (expect npy-buffer-scratch :to-be nil)
              (expect npy-buffer-shell-initialized :to-be nil)
              (expect npy-buffer-dedicated-to :to-be nil)
              (expect (buffer-name npy-buffer-child-dedicatable-to) :to-equal "foo.py"))))))
    (describe "when spawn a python-mode buffer visiting a file in a Pipenv project, and spawn a virtualenv dedicated inferior python buffer on it,"
      (it "for the two buffers are associated with the same Pipenv project."
        (unwind-protect
            (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
              (with-file-buffers ("project1/buz.py")
                (with-current-buffer "buz.py"
                  (npy-run-python)
                  (npy-helper-wait))
                (with-current-buffer "*Python[Pipenv:project1]*"
                  (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project1"))
                  (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project1")
                  (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project1)
                  (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project1)
                  (expect npy-buffer-scratch :to-be nil)
                  (expect npy-buffer-shell-initialized :to-be t)
                  (expect npy-buffer-dedicated-to :to-be nil)
                  (expect npy-buffer-child-dedicatable-to :to-be nil))))
          (npy-helper-kill-python-inferior-buffers "*Python[Pipenv:project1]*"))))
    (describe "when spawn a python-mode buffer visiting a file in a Pipenv project, and spawn a virtualenv-buffer dedicated inferior python buffer on it,"
      (it "for the two buffers are associated with the same Pipenv project."
        (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
          (with-file-buffers ("project1/buz.py")
            (with-current-buffer "buz.py"
              (npy-run-python t)
              (npy-helper-wait))
            (with-current-buffer "*Python[Pipenv:project1;b:buz.py]*"
              (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project1"))
              (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project1")
              (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project1)
              (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project1)
              (expect npy-buffer-scratch :to-be nil)
              (expect npy-buffer-shell-initialized :to-be t)
              (expect npy-buffer-dedicated-to :to-be (get-buffer "buz.py"))
              (expect npy-buffer-child-dedicatable-to :to-be (get-buffer "buz.py")))
            (npy-helper-kill-python-inferior-buffers "*Python[Pipenv:project1;b:buz.py]*")))))
    (describe "when spawn a python-mode buffer visiting a file in a Pipenv project, and spawn a virtualenv dedicated scratch buffer for Python on it,"
      (it "for the two buffers are associated with the same Pipenv project."
        (unwind-protect
            (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
              (with-file-buffers ("project1/buz.py")
                (with-current-buffer "buz.py"
                  (npy-scratch))
                (with-current-buffer "*pyscratch[Pipenv:project1]*"
                  (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project1"))
                  (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project1")
                  (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project1)
                  (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project1)
                  (expect npy-buffer-scratch :to-be t)
                  (expect npy-buffer-shell-initialized :to-be nil)
                  (expect npy-buffer-dedicated-to :to-be nil)
                  (expect npy-buffer-child-dedicatable-to :to-be nil))))
          (kill-buffer "*pyscratch[Pipenv:project1]*"))))
    (describe "when spawn a python-mode buffer visiting a file in a Pipenv project, and spawn a virtualenv-buffer dedicated scratch buffer for Python on it,"
      (it "for the two buffers are associated with the same Pipenv project."
        (unwind-protect
            (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
              (with-file-buffers ("project1/buz.py")
                (with-current-buffer "buz.py"
                  (npy-scratch t))
                (with-current-buffer "*pyscratch[Pipenv:project1;b:buz.py]*"
                  (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project1"))
                  (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project1")
                  (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project1)
                  (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project1)
                  (expect npy-buffer-scratch :to-be t)
                  (expect npy-buffer-shell-initialized :to-be nil)
                  (expect npy-buffer-dedicated-to :to-be (get-buffer "buz.py"))
                  (expect npy-buffer-child-dedicatable-to :to-be (get-buffer "buz.py")))))
          (kill-buffer "*pyscratch[Pipenv:project1;b:buz.py]*"))))
    (describe "when spawn a dired-mode buffer visiting a directory in a Pipenv project, and spawn a virtualenv dedicated inferior python buffer on it,"
      (it "for the two buffers are associated with the same Pipenv project."
        (unwind-protect
            (with-file-buffers ("project1")
              (with-current-buffer "project1"
                (npy-run-python)
                (npy-helper-wait))
              (with-current-buffer "*Python[Pipenv:project1]*"
                (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project1"))
                (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project1")
                (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project1)
                (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project1)
                (expect npy-buffer-scratch :to-be nil)
                (expect npy-buffer-shell-initialized :to-be t)
                (expect npy-buffer-dedicated-to :to-be nil)
                (expect npy-buffer-child-dedicatable-to :to-be nil)))
          (npy-helper-kill-python-inferior-buffers "*Python[Pipenv:project1]*"))))
    (describe "when spawn a dired-mode buffer visiting a directory in a Pipenv project, and spawn a virtualenv-buffer dedicated inferior python buffer on it,"
      (it "npy-run-python throws an error."
        (with-file-buffers ("project1")
          (with-current-buffer "project1"
            (expect (npy-run-python t) :to-throw 'error)))))
    (describe "when spawn a dired-mode buffer visiting a directory in a Pipenv project, and spawn a virtualenv dedicated scratch buffer for Python on it,"
      (it "for the two buffers are associated with the same Pipenv project."
        (unwind-protect
            (with-file-buffers ("project1")
              (with-current-buffer "project1"
                (npy-scratch))
              (with-current-buffer "*pyscratch[Pipenv:project1]*"
                (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project1"))
                (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project1")
                (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project1)
                (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project1)
                (expect npy-buffer-scratch :to-be t)
                (expect npy-buffer-shell-initialized :to-be nil)
                (expect npy-buffer-dedicated-to :to-be nil)
                (expect npy-buffer-child-dedicatable-to :to-be nil)))
          (kill-buffer "*pyscratch[Pipenv:project1]*"))))
    (describe "when spawn a dired-mode buffer visiting a directory in a Pipenv project, and spawn a virtualenv-buffer dedicated scratch buffer for Python on it,"
      (it "npy-scratch throws an error."
        (with-file-buffers ("project1")
          (with-current-buffer "project1"
            (expect (npy-scratch t) :to-throw 'error))))))
  (describe "three buffer cases:"
    (describe "when spawn a python-mode buffer visiting a file in a Pipenv project -> a virtualenv dedicated scratch buffer for Python -> a virtualenv dedicated inferior python buffer."
      (it "is, on the inferior buffer, associated with the Pipenv project."
        (unwind-protect
            (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
              (with-file-buffers ("project1/buz.py")
                (with-current-buffer "buz.py"
                  (npy-scratch))
                (with-current-buffer "*pyscratch[Pipenv:project1]*"
                  (npy-run-python)
                  (npy-helper-wait))
                (with-current-buffer "*Python[Pipenv:project1]*"
                  (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project1"))
                  (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project1")
                  (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project1)
                  (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project1)
                  (expect npy-buffer-scratch :to-be nil)
                  (expect npy-buffer-shell-initialized :to-be t)
                  (expect npy-buffer-dedicated-to :to-be nil)
                  (expect npy-buffer-child-dedicatable-to :to-be nil))))
          (kill-buffer "*pyscratch[Pipenv:project1]*")
          (npy-helper-kill-python-inferior-buffers "*Python[Pipenv:project1]*"))))
    (describe "when spawn a python-mode buffer visiting a file in a Pipenv project -> a virtualenv-buffer dedicated scratch buffer for Python -> a virtualenv dedicated inferior python buffer."
      (it "is, on the inferior buffer, associated with the Pipenv project."
        (unwind-protect
            (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
              (with-file-buffers ("project1/buz.py")
                (with-current-buffer "buz.py"
                  (npy-scratch t))
                (with-current-buffer "*pyscratch[Pipenv:project1;b:buz.py]*"
                  (npy-run-python)
                  (npy-helper-wait))
                (with-current-buffer "*Python[Pipenv:project1]*"
                  (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project1"))
                  (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project1")
                  (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project1)
                  (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project1)
                  (expect npy-buffer-scratch :to-be nil)
                  (expect npy-buffer-shell-initialized :to-be t)
                  (expect npy-buffer-dedicated-to :to-be nil)
                  (expect npy-buffer-child-dedicatable-to :to-be nil))))
          (kill-buffer "*pyscratch[Pipenv:project1;b:buz.py]*")
          (npy-helper-kill-python-inferior-buffers "*Python[Pipenv:project1]*"))))
    (describe "when spawn a python-mode buffer visiting a file in a Pipenv project -> a virtualenv-buffer dedicated scratch buffer for Python -> a virtualenv-buffer dedicated inferior python buffer."
      (it "is, on the inferior buffer, associated with the Pipenv project."
        (unwind-protect
            (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
              (with-file-buffers ("project1/buz.py")
                (with-current-buffer "buz.py"
                  (npy-scratch t))
                (with-current-buffer "*pyscratch[Pipenv:project1;b:buz.py]*"
                  (npy-run-python t)
                  (npy-helper-wait))
                (with-current-buffer "*Python[Pipenv:project1;b:buz.py]*"
                  (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project1"))
                  (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project1")
                  (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project1)
                  (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project1)
                  (expect npy-buffer-scratch :to-be nil)
                  (expect npy-buffer-shell-initialized :to-be t)
                  (expect npy-buffer-dedicated-to :to-be (get-buffer "buz.py"))
                  (expect npy-buffer-child-dedicatable-to :to-be (get-buffer "buz.py")))))
          (kill-buffer "*pyscratch[Pipenv:project1;b:buz.py]*")
          (npy-helper-kill-python-inferior-buffers "*Python[Pipenv:project1;b:buz.py]*"))))
    (describe "when spawn a python-mode buffer visiting a file in a Pipenv project -> a virtualenv dedicated scratch buffer for Python -> a virtualenv-buffer dedicated inferior python buffer."
      (it "is, on the inferior buffer, associated with the Pipenv project."
        (unwind-protect
            (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
              (with-file-buffers ("project1/buz.py")
                (with-current-buffer "buz.py"
                  (npy-scratch))
                (with-current-buffer "*pyscratch[Pipenv:project1]*"
                  (expect (npy-run-python t) :to-throw 'error))))
          (kill-buffer "*pyscratch[Pipenv:project1]*")
          )))
    (describe "when spawn a python-mode buffer visiting a file in a Pipenv project -> a virtualenv dedicated inferior python buffer -> a virtualenv dedicated scratch buffer for Python."
      (it "is, on the inferior buffer, associated with the Pipenv project."
        (unwind-protect
            (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
              (with-file-buffers ("project1/buz.py")
                (with-current-buffer "buz.py"
                  (npy-run-python)
                  (npy-helper-wait))
                (with-current-buffer "*Python[Pipenv:project1]*"
                  (npy-scratch))
                (with-current-buffer "*pyscratch[Pipenv:project1]*"
                  (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project1"))
                  (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project1")
                  (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project1)
                  (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project1)
                  (expect npy-buffer-scratch :to-be t)
                  (expect npy-buffer-shell-initialized :to-be nil)
                  (expect npy-buffer-dedicated-to :to-be nil)
                  (expect npy-buffer-child-dedicatable-to :to-be nil))))
          (kill-buffer "*pyscratch[Pipenv:project1]*")
          (npy-helper-kill-python-inferior-buffers "*Python[Pipenv:project1]*"))))
    (describe "when spawn a python-mode buffer visiting a file in a Pipenv project -> a virtualenv-buffer dedicated inferior python buffer -> a virtualenv dedicated scratch buffer for Python."
      (it "is, on the inferior buffer, associated with the Pipenv project."
        (unwind-protect
            (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
              (with-file-buffers ("project1/buz.py")
                (with-current-buffer "buz.py"
                  (npy-run-python t)
                  (npy-helper-wait))
                (with-current-buffer "*Python[Pipenv:project1;b:buz.py]*"
                  (npy-scratch))
                (with-current-buffer "*pyscratch[Pipenv:project1]*"
                  (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project1"))
                  (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project1")
                  (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project1)
                  (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project1)
                  (expect npy-buffer-scratch :to-be t)
                  (expect npy-buffer-shell-initialized :to-be nil)
                  (expect npy-buffer-dedicated-to :to-be nil)
                  (expect npy-buffer-child-dedicatable-to :to-be nil))))
          (kill-buffer "*pyscratch[Pipenv:project1]*")
          (npy-helper-kill-python-inferior-buffers "*Python[Pipenv:project1;b:buz.py]*"))))
    (describe "when spawn a python-mode buffer visiting a file in a Pipenv project -> a virtualenv-buffer dedicated inferior python buffer -> a virtualenv-buffer dedicated scratch buffer for Python."
      (it "is, on the inferior buffer, associated with the Pipenv project."
        (unwind-protect
            (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
              (with-file-buffers ("project1/buz.py")
                (with-current-buffer "buz.py"
                  (npy-run-python t)
                  (npy-helper-wait))
                (with-current-buffer "*Python[Pipenv:project1;b:buz.py]*"
                  (npy-scratch t))
                (with-current-buffer "*pyscratch[Pipenv:project1;b:buz.py]*"
                  (expect (gpc-val 'pipenv-project-root npy-env) :to-equal (@- "project1"))
                  (expect (gpc-val 'pipenv-project-name npy-env) :to-equal "project1")
                  (expect (gpc-val 'pipenv-virtualenv-root npy-env) :to-equal npy-test/venv-root-for-project1)
                  (expect python-shell-virtualenv-root :to-equal npy-test/venv-root-for-project1)
                  (expect npy-buffer-scratch :to-be t)
                  (expect npy-buffer-shell-initialized :to-be nil)
                  (expect npy-buffer-dedicated-to :to-be (get-buffer "buz.py"))
                  (expect npy-buffer-child-dedicatable-to :to-be (get-buffer "buz.py")))))
          (kill-buffer "*pyscratch[Pipenv:project1;b:buz.py]*")
          (npy-helper-kill-python-inferior-buffers "*Python[Pipenv:project1;b:buz.py]*"))))
    (describe "when spawn a python-mode buffer visiting a file in a Pipenv project -> a virtualenv dedicated inferior python buffer -> a virtualenv-buffer dedicated scratch buffer for Python."
      (it "npy-scratch throws an error."
        (unwind-protect
            (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
              (with-file-buffers ("project1/buz.py")
                (with-current-buffer "buz.py"
                  (npy-run-python)
                  (npy-helper-wait))
                (with-current-buffer "*Python[Pipenv:project1]*"
                  (expect (npy-scratch t) :to-throw 'error))))
          (npy-helper-kill-python-inferior-buffers "*Python[Pipenv:project1;b:buz.py]*"))))))

(provide 'npy-env-test)
;;; npy-env-test.el ends here
