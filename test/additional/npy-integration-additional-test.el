;;; npy-integration-additional-test.el --- Additional integration tests for npy.  -*- lexical-binding: t; -*-

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

;; Additional integration tests for npy.

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

(describe "npy-run-python"
  (describe "when called on a python-mode buffer visiting a file in a Pipenv project,"
    (it "- spawns a virrualenv dedicated inferior python buffer
    whose buffer name contains the Pipenv project name."
      (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
        (with-file-buffers ("project1/buz.py")
          (set-buffer "buz.py")
          (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
          (npy-run-python)
          (npy-helper-wait)
          (let ((python-inf-buf (get-buffer "*Python[Pipenv:project1]*")))
            (should-not (eq  python-inf-buf nil))
            (npy-helper-kill-inferior-python-buffers python-inf-buf)))))
    (it "- spawns a virrualenv dedicated inferior python buffer
    whose sys.path contans the virtualenv root of the Pipenv
    project."
      (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
        (with-file-buffers ("project1/buz.py")
          (set-buffer "buz.py")
          (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
          (npy-run-python)
          (npy-helper-wait)
          (let ((python-inf-buf (get-buffer "*Python[Pipenv:project1]*")))
            (should-not (eq  python-inf-buf nil))
            (should-response-match python-inf-buf
              "import sys\nprint(sys.path)\n" npy-test/venv-root-for-project1)
            (npy-helper-kill-inferior-python-buffers python-inf-buf))))))
  (describe "when called on a python-mode buffer visiting a file
  in a Pipenv project with `dedicated' set to t,"
    (it "- spawns a virrualenv-buffer dedicated inferior python
    buffer whose buffer name contains the Pipenv project name and
    the name of the buffer where it was spawned."
      (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
        (with-file-buffers ("project1/buz.py")
          (set-buffer "buz.py")
          (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
          (npy-run-python t)
          (npy-helper-wait)
          (let ((python-inf-buf (get-buffer "*Python[Pipenv:project1;b:buz.py]*")))
            (should-not (eq  python-inf-buf nil))
            (npy-helper-kill-inferior-python-buffers python-inf-buf)))))
    (it "- spawns a virrualenv-buffer dedicated inferior python
    buffer whose sys.path contans the virtualenv root."
      (with-files-in-playground (("project1/buz.py" . "VAR = 1")
                                 ("project2/foo.py" . "VAR = 2"))
        (with-file-buffers ("project1/buz.py" "project2/foo.py")
          (with-current-buffer "buz.py"
            (npy-run-python))
          (with-current-buffer "foo.py"
            (npy-run-python))
          (npy-helper-wait)
          (let ((python-inf-buf-1 (get-buffer "*Python[Pipenv:project1]*"))
                (python-inf-buf-2 (get-buffer "*Python[Pipenv:project2]*")))
            (should-response-match python-inf-buf-1
              "import sys\nprint(sys.path)\n" npy-test/venv-root-for-project1)
            (should-response-match python-inf-buf-2
              "import sys\nprint(sys.path)\n" npy-test/venv-root-for-project2)
            (npy-helper-kill-inferior-python-buffers python-inf-buf-1 python-inf-buf-2)))))))

(describe "python-shell-send-buffer"
  (describe "when there are a python-mode buffer visiting a file
   in a Pipenv project and a virtualenv dedicated inferior python
   buffer dedicated to the same virtualenv,"
    (describe "if called on the python-mode buffer with a code chunk,"
      (it "dispatches the chunk to the virtualenv dedicated inferior python buffer."
        (with-files-in-playground (("project1/buz.py" . "VAR = \"from buz.py\""))
          (with-file-buffers ("project1/buz.py")
            (with-current-buffer  "buz.py"
              (npy-run-python)
              (npy-helper-wait))
            (let ((python-inf-buf (get-buffer "*Python[Pipenv:project1]*")))
              (with-current-buffer "buz.py"
                (python-shell-send-buffer))
              (should-response-match python-inf-buf
                "print(VAR)\n" "from buz.py")
              (npy-helper-kill-inferior-python-buffers python-inf-buf)))))))
  (describe "when there are two python-mode buffers: buffer-1 is
  visiting a file in a Pipenv project, and buffer-2 is visiting a
  file not in a Pipenv project, and there are two inferior python
  buffers: inf-buf1 is dedicated to the Pipenv project and
  inf-buf-2 is a normal one,"
    (it "- dispatches a code chunk from buffer-1 into inf-buf-1,
    and a code chunk from buffer-2 into inf-buf-2."
      (with-files-in-playground (("project1/buz.py" . "VAR = \"from buz.py\"")
                                 ("project3/foo.py" . "VAR = \"from foo.py\""))
        (with-file-buffers ("project1/buz.py" "project3/foo.py")
          (with-current-buffer "buz.py"
            (npy-run-python))
          (with-current-buffer "foo.py"
            (run-python))
          (npy-helper-wait)
          (let ((python-inf-buf-1 (get-buffer "*Python[Pipenv:project1]*"))
                (python-inf-buf-2 (get-buffer "*Python*")))
            (with-current-buffer "buz.py"
              (python-shell-send-buffer))
            (with-current-buffer "foo.py"
              (python-shell-send-buffer))
            (should-response-match python-inf-buf-1
              "print(VAR)\n" "from buz.py")
            (should-response-match python-inf-buf-2
              "print(VAR)\n" "from foo.py")
            (npy-helper-kill-inferior-python-buffers python-inf-buf-1 python-inf-buf-2))))))
  (describe "when there are two python-mode buffers: buffer-1 is
  visiting a file in a Pipenv project, and buffer-2 is visiting a
  different file in the same Pipenv project, and there are two
  inferior python buffers: inf-buf1 is virtualenv
  dedicated (dedicated to the Pipenv project) and inf-buf-2 is
  virtualenv-buffer dedicated (dedicated to the Pipenv project
  the file buffer-2 visiting belongs to and buffer-2,"
    (it "- dispatches a code chunk from buffer-1 into inf-buf-1,
    and a code chunk from buffer-2 into inf-buf-2."
      (with-files-in-playground (("project1/buz.py" . "VAR = \"from buz.py\"")
                                 ("project1/foo.py" . "VAR = \"from foo.py\""))
        (with-file-buffers ("project1/buz.py" "project1/foo.py")
          (with-current-buffer "buz.py"
            (npy-run-python))
          (with-current-buffer "foo.py"
            (npy-run-python t))
          (npy-helper-wait)
          (let ((python-inf-buf-1 (get-buffer "*Python[Pipenv:project1]*"))
                (python-inf-buf-2 (get-buffer "*Python[Pipenv:project1;b:foo.py]*")))
            (with-current-buffer "buz.py"
              (python-shell-send-buffer))
            (with-current-buffer "foo.py"
              (python-shell-send-buffer))
            (should-response-match python-inf-buf-1
              "print(VAR)\n" "from buz.py")
            (should-response-match python-inf-buf-2
              "print(VAR)\n" "from foo.py")
            (npy-helper-kill-inferior-python-buffers python-inf-buf-1 python-inf-buf-2)))))))

(describe "python-shell-send-buffer"
  (describe "when there are two virtualenv-dedicated inferior
  python buffers: one is spawned on project1/buz.py and the other
  is spawned on project2/foo.py,"
    (it "- dispatches code chunks sent from two python-mode
    buffers into respective infeior buffers."
      (with-files-in-playground (("project1/buz.py" . "VAR = \"from buz.py\"")
                                 ("project2/foo.py" . "VAR = \"from foo.py\""))
        (with-file-buffers ("project1/buz.py" "project2/foo.py")
          (with-current-buffer "buz.py"
            (npy-run-python))
          (with-current-buffer "foo.py"
            (npy-run-python))
          (npy-helper-wait)
          (let ((python-inf-buf-1 (get-buffer "*Python[Pipenv:project1]*"))
                (python-inf-buf-2 (get-buffer "*Python[Pipenv:project2]*")))
            (with-current-buffer "buz.py"
              (python-shell-send-buffer))
            (with-current-buffer "foo.py"
              (python-shell-send-buffer))
            (should-response-match python-inf-buf-1
              "print(VAR)\n" "from buz.py")
            (should-response-match python-inf-buf-2
              "print(VAR)\n" "from foo.py")
            (npy-helper-kill-inferior-python-buffers python-inf-buf-1 python-inf-buf-2))))))
  (describe "when there are two virtualenv-dedicated inferior
  python buffers: inf-buf-1 is spawned on project1/buz.py and
  inf-buf-2 is spawned on project2/foo.py,"
    (it "- dispatches code chunks from project1/buz.py into
    inf-buf-1, project1/foo.py into inf-buf-1, and from
    project2/bar.py into inf-buf-2."
      (with-files-in-playground (("project1/buz.py" . "VAR = \"from buz.py\"")
                                 ("project1/foo.py" . "VAR = \"from foo.py\"")
                                 ("project2/bar.py" . "VAR2 = \"from bar.py\""))
        (with-file-buffers ("project1/buz.py" "project1/foo.py" "project2/bar.py")
          (with-current-buffer "buz.py"
            (npy-run-python))
          ;; (with-current-buffer "foo.py"
          ;;   (npy-run-python))
          (with-current-buffer "bar.py"
            (npy-run-python))
          (npy-helper-wait)
          (let ((python-inf-buf-1 (get-buffer "*Python[Pipenv:project1]*"))
                (python-inf-buf-2 (get-buffer "*Python[Pipenv:project2]*")))
            (with-current-buffer "foo.py"
              (python-shell-send-buffer))
            (should-response-match python-inf-buf-1
              "print(VAR)\n" "from foo.py")
            (with-current-buffer "bar.py"
              (python-shell-send-buffer))
            (should-response-match python-inf-buf-2
              "print(VAR2)\n" "from bar.py")
            (npy-helper-kill-inferior-python-buffers python-inf-buf-1 python-inf-buf-2))))))
  (describe "when there are two virtualenv-buffer dedicated
  python inferior buffers: inf-buf-1 is spawned on
  project1/buz.py and inf-buf-2 is spawned on project1/foo.py,"
    (it "- dispatches code chunks sent from project1/buz.py into
    inf-buf-1, and from project1/foo.py into inf-buf-2"
      (with-files-in-playground (("project1/buz.py" . "VAR = \"from buz.py\"")
                                 ("project1/foo.py" . "VAR = \"from foo.py\""))
        (with-file-buffers ("project1/buz.py" "project1/foo.py")
          (with-current-buffer "buz.py"
            (npy-run-python t))
          (with-current-buffer "foo.py"
            (npy-run-python t))
          (npy-helper-wait)
          (let ((python-inf-buf-1 (get-buffer "*Python[Pipenv:project1;b:buz.py]*"))
                (python-inf-buf-2 (get-buffer "*Python[Pipenv:project1;b:foo.py]*")))
            (with-current-buffer "buz.py"
              (python-shell-send-buffer))
            (with-current-buffer "foo.py"
              (python-shell-send-buffer))
            (should-response-match python-inf-buf-1
              "print(VAR)\n" "from buz.py")
            (should-response-match python-inf-buf-2
              "print(VAR)\n" "from foo.py")
            (npy-helper-kill-inferior-python-buffers python-inf-buf-1 python-inf-buf-2)))))))

(provide 'npy-integration-additional-test)
;;; npy-integration-additional-test.el ends here
