;;; npy-dispatch-feature-test.el --- Tests for npy dispatching feature. -*- lexical-binding: t; -*-

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

;; Tests for npy dispatching feature.

;;; Code:
(describe "dispatch feature:"
  (describe "when there is only one virtualenv dedicated inferior python buffer (inf-buf-a) spawned on a buffer (buf-a) in a Pipenv project (pipenv-a):"
    (describe "from python-mode buffers:"
      (describe "buf-a.")
      (describe "another python-mode buffer in pipenv-a.")
      (describe "a python-mode buffer in a different Pipenv project.")
      (describe "a python-mode buffer not in a Pipenv project."))
    (describe "from npy-scratch buffers:"
      (describe "a venv dedicated scratch buffer spawned on buf-a.")
      (describe "a venv-buffer-dedicated scratch buffer for buf-a.")
      (describe "a venv dedicated scratch buffer spawned on another python-mode buffer in pipenv-a.")
      (describe "a venv-buffer-dedicated scratch buffer spawnd on another python-mode in pipenv-a."))
    (describe "from inferior python buffers:"
      (describe "inf-buf-a.")
      (describe "a venv-buffer dedicated inf buffer for pipenv-a.")
      (describe "a venv dedicated inf buffer for another Pipenv project.")
      (describe "a venv-buffer dedicated inf buffer for another Pipenv project.")
      (describe "an ordinary inferior python buffer.")
      (describe "an ordinary dedicated inferior python buffer.")))
  (describe "when there is only one virtualenv-buffer dedicated inferior python buffer (inf-buf-a) for a buffer (buf-a) in a Pipenv project (pipenv-a):"
    (describe "from python-mode buffers:"
      (describe "buf-a.")
      (describe "another python-mode buffer in pipenv-a.")
      (describe "a python-mode buffer in a different Pipenv project.")
      (describe "a python-mode buffer not in a Pipenv project."))
    (describe "from npy-scratch buffers:"
      (describe "a venv dedicated scratch buffer spawned on buf-a.")
      (describe "a venv-buffer-dedicated scratch buffer spawnd on the python-mode buffer where the inf buf was spawned.")
      (describe "a venv dedicated scratch buffer spawned on another python-mode buffer where the inf buf was spawned.")
      (describe "a venv-buffer-dedicated scratch buffer spawnd on another python-mode buffer where the inf buf was spawned."))
    (describe "from inferior python buffers:"
      (describe "a venv dedicated inf buffer for the same Pipenv project.")
      (describe "the venv-buffer dedicated inf buffer itself.")
      (describe "a venv-buffer dedicated inf buffer for another python-mode in the same Pipenv project.")
      (describe "a venv dedicated inf buffer for another Pipenv project.")
      (describe "a venv-buffer dedicated inf buffer for another Pipenv project.")
      (describe "an ordinary inferior python buffer.")
      (describe "an ordinary dedicated inferior python buffer.")))
  (describe "when there are two virtualenv dedicated inferior python buffers (inf-buf-a and inf-buf-b) for two Pipenv projects (pipenv-a and pipenv-b):"
    (describe "from python-mode buffers:"
      (describe "a python-mode buffer in pipenv-a.")
      (describe "a python-mode buffer in pipenv-b."))
    (describe "from npy-scratch buffers:"
      (describe "a venv dedicated scratch buffer for pipenv-a.")
      (describe "a venv dedicated scratch buffer for pipenv-b.")
      (describe "a venv-buffer-dedicated scratch buffer for pipenv-a.")
      (describe "a venv-buffer-dedicated scratch buffer for pipenv-b."))
    (describe "from inferior python buffers:"
      (describe "the inf-buf-a.")
      (describe "the inf-buf-b.")))
  (describe "when there are two virtualenv-buffer dedicated inferior python buffer (inf-buf-a (dedicated to buf-a) and inf-buf-b (dedicated to buf-b)) for the same Pipenv project:"
    (describe "from python-mode buffers:"
      (describe "the inf-buf-a.")
      (describe "the inf-buf-b.")
      (describe "another python-mode buffer in the same Pipenv project."))
    (describe "from npy-scratch buffers:"
      (describe "a venv-buffer-dedicated scratch buffer for buf-a.")
      (describe "a venv-buffer-dedicated scratch buffer for buf-b."))
    (describe "from inferior python buffers:"
      (describe "the inf-buf-a.")
      (describe "the inf-buf-b.")))
  (describe "when there are one virtualenv didcated inferior python buffer and one virtualenv-buffer dedicated inferior python buffer for the same Pipenv project:"
    (describe "from python-mode buffers:"
      (describe "a python-mode buffer where the inf buffer was spawned.")
      (describe "another python-mode buffer in the same Pipenv project.")
      (describe "a python-mode buffer in a different Pipenv project.")
      (describe "a python-mode buffer not in a Pipenv project."))
    (describe "from npy-scratch buffers:"
      (describe "a venv dedicated scratch buffer spawned on the python-mode buffer where the inf buf was spawned.")
      (describe "a venv-buffer-dedicated scratch buffer spawnd on the python-mode buffer where the inf buf was spawned.")
      (describe "a venv dedicated scratch buffer spawned on another python-mode buffer where the inf buf was spawned.")
      (describe "a venv-buffer-dedicated scratch buffer spawnd on another python-mode buffer where the inf buf was spawned."))
    (describe "from inferior python buffers:"
      (describe "a venv dedicated inf buffer for the same Pipenv project.")
      (describe "the venv-buffer dedicated inf buffer itself.")
      (describe "a venv-buffer dedicated inf buffer for another python-mode in the same Pipenv project.")
      (describe "a venv dedicated inf buffer for another Pipenv project.")
      (describe "a venv-buffer dedicated inf buffer for another Pipenv project.")
      (describe "an ordinary inferior python buffer.")
      (describe "an ordinary dedicated inferior python buffer.")))

  (describe "when there are a virtualenv dedicated python scratch
  buffer and a virtualenv dedicated inferior python buffer
  spawned on the scratch buffer"
    (it "dispatches a code chunk sent from a virtualenv dedicated
  python scratch buffer to the inferior python buffer dedicated
  to the same virtualenv"
      (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
        (with-file-buffers ("project1/buz.py")
          (set-buffer "buz.py")
          (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
          (npy-scratch)
          (npy-run-python)
          (npy-helper-wait)
          (let-to-kill ((scratch-buf (get-buffer "*pyscratch[Pipenv:project1]*"))
                        (inf-buf (get-buffer "*Python[Pipenv:project1]*")))
            (npy-helper-write "VAR1 = \"from scratch\"\n" scratch-buf)
            (with-current-buffer scratch-buf
              (python-shell-send-buffer))
            (should-response-match inf-buf
              "print(VAR1)\n" "from scratch"))))))
  (describe "when there are a virtualenv-buffer dedicated python
  scratch buffer and an inferior python dedicated to the same
  virtualenv and buffer"
    (it "dispatches a code chunk sent from the scratch buffer to
    the inferior python buffer"
      (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
        (with-file-buffers ("project1/buz.py")
          (set-buffer "buz.py")
          (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
          (npy-run-python)
          (npy-helper-wait)
          (let-to-kill ((inf-buf (get-buffer "*Python[Pipenv:project1]*")))
            (message "inf-buf: %s" inf-buf)
            (set-buffer inf-buf)
            (npy-scratch)
            (let-to-kill ((scratch-buf (get-buffer "*pyscratch[Pipenv:project1]*")))
              (message "scratch-buf: %s" scratch-buf)
              (npy-helper-write "VAR4 = \"from scratch\"\n" scratch-buf)
              (with-current-buffer scratch-buf
                (python-shell-send-buffer))
              (should-response-match inf-buf
                "print(VAR4)\n" "from scratch"))))))))

(describe "npy-run-python"
  (describe "when called on a virtualenv dedicated python scratch buffer"
    (it "spawns a virtualenv dedicated inferior python buffer
    dedicated to the same virtualenv"
      (with-files-in-playground (("project1/buz.py" . "VAR = 1"))
        (with-file-buffers ("project1/buz.py")
          (set-buffer "buz.py")
          (should (equal (gpc-val 'pipenv-project-root npy-env) (@- "project1")))
          (npy-scratch)
          (let-to-kill ((scratch-buf (get-buffer "*pyscratch[Pipenv:project1]*")))
            (with-current-buffer scratch-buf
              (npy-run-python)
              (npy-helper-wait))
            (let-to-kill ((inf-buf (get-buffer "*Python[Pipenv:project1]*")))
              (npy-helper-write "VAR6 = \"from scratch\"\n" scratch-buf)
              (with-current-buffer scratch-buf
                (python-shell-send-buffer))
              (should-response-match inf-buf
                "print(VAR6)\n" "from scratch"))))))
    (describe "when called on a virtualenv-buffer dedicated
    python scratch buffer with dedicated set to t"
      (it "spawns a virtualenv-buffer dedicated inferior python
          buffer dedicated to the same virtualenv and buffer"
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
                  "print(VAR7)\n" "from scratch")))))))))

(provide 'npy-dispatch-feature-test)
;;; npy-dispatch-feature-test.el ends here
