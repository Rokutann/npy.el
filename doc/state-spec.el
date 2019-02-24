'(:rules
  "Rules:
1. A python-mode buffer visiting a file in a Pipenv project has npy-child-dedicatable-to set to itself.
2. If npy-child-dedicatable-to is set, the buffer can spawn any virtualenv-buffer-dedicated buffers, which inherit the npy-child-dedicatable-to of the parent.
3. If npy-child-dedicatable-to is set, the buffer can spawn any virtualenv-dedicated buffers, but the npy-child-dedicatable-to of the spawned buffers is set to nil.
4.  If npy-child-dedicatable-to is nil, the buffer can not spawn any virtualenv-dedicated buffers but spawn any virtualenv-dedicated buffers.
5. When a virtualenv-buffer-dedicated buffer is spawned on a python-mode buffer, its npy-dedicated-to is set to that pytho-mode buffer.
6. When a virtualenv-buffer-dedicated buffer is spawned on a virtualenv-buffer-dedicated buffer, its npy-dedicated-to is set to npy-child-dedicatable-to fo the parent.
7. If the buffer to spawn already exists and alive, pop-to-buffer it.
8. If the buffer to spawn already exists but killed, raise an error.
9. If the buffer npy-child-dedicatable-to points is already killed when spawning a virtualenv-buffer-dedicated buffer, raise an error.
10. The presedence list of a python-mode buffer visiting a file in a Pipenv project is: virtualenv-buffer-dedicated, virtualenv-dedicated, dedicated, global.
11. The presedence list of a virtualenv-dedicated buffer is: virtualenv-dedicated, dedicated, global.
12. The presedence list of a virtualenv-buffer-dedicated buffer is: virtualenv-buffer-dedicated, virtualenv-dedicated, dedicated, global.
13. If the buffer to send a string exists but killed, don't raise an error, just move down the precedence list."
  )

(defmacro register-symbols (&rest symbols)
  (declare (indent 0))
  `(progn
     ,@(mapcar #'(lambda (symbol) `(setq ,symbol nil)) symbols)))

(register-symbols
 python-mode-buffer-visiting-a-file-not-in-a-pipenv-project
 dedicated-inferior-python-buffer
 global-inferior-python-buffer
 python-mode-buffer-visiting-a-file-in-a-pipenv-project
 virtualenv-dedicated-inferior-python-buffer
 virtualenv-buffer-dedicated-inferior-python-buffer
 virtualenv-dedicated-python-scratch-buffer
 virtualenv-buffer-dedicated-python-scratch-buffer)

(setq state-machine
      '((:state E1 (python-mode-buffer-visiting-a-file-not-in-a-pipenv-project
                    :spawned-at nil
                    :dedicated-buffer nil
                    :associatable-buffer nil
                    :venv nil)
                ((:npy-run-python E1)
                 (:npy-run-python-t E1)
                 (:npy-scratch E1)
                 (:npy-scratch-t E1))
                (:send-string (:lookup-order dedicated global)))
        (:state E2 (dedicated-inferior-python-buffer
                    :spawned-at nil
                    :dedicated-buffer nil
                    :associatable-buffer nil
                    :venv nil)
                ((:npy-run-python E2)
                 (:npy-run-python-t E2)
                 (:npy-scratch E2)
                 (:npy-scratch-t E2))
                (:send-string (:lookup-order self)))
        (:state E3 (global-inferior-python-buffer
                    :spawned-at nil
                    :dedicated-buffer nil
                    :associatable-buffer nil
                    :venv nil)
                ((:npy-run-python E3)
                 (:npy-run-python-t E3)
                 (:npy-scratch E3)
                 (:npy-scratch-t E3))
                (:send-string (:lookup-order self)))
        (:state E4 (python-mode-buffer-visiting-a-file-in-a-pipenv-project
                    :spawned-at nil
                    :dedicated-buffer nil
                    :associatable-buffer self
                    :venv from-system)
                ((:npy-run-python RV1)
                 (:npy-run-python-t RB2)
                 (:npy-scratch SV1)
                 (:npy-scratch-t SB2)))
        (:state RV1 (virtualenv-dedicated-inferior-python-buffer
                     :spawned-at (E4) python-mode-buffer-visiting-a-file-in-a-pipenv-project
                     :dedicated-buffer nil
                     :associatable-buffer nil
                     :venv (self => :spawned-at => :venv))
                ((:npy-run-python self)
                 (:npy-run-python-t self :message "Not associated with a file buffer.")
                 (:npy-scratch SV3)
                 (:npy-scratch-t SB4)))
        (:state RB2 (virtualenv-buffer-dedicated-inferior-python-buffer
                     :spawned-at (E4) python-mode-buffer-visiting-a-file-in-a-pipenv-project
                     :dedicated-buffer (self => :spawned-at)
                     :associatable-buffer (self => :spawned-at)
                     :venv (self => :spawned-at => :venv))
                ((:npy-run-python RV3)
                 (:npy-run-python-t self)
                 (:npy-scratch SV3)
                 (:npy-scratch-t SB4)))
        (:state RB3 (virtualenv-buffer-dedicated-inferior-python-buffer
                     :spawned-at (SB2) virtualenv-buffer-dedicated-python-scratch-buffer
                     :dedicated-buffer (self => :spawned-at)
                     :associatable-buffer (self => :spawned-at)
                     :venv (self => :spawned-at => :venv))
                ((:npy-run-python RV3)
                 (:npy-run-python-t self)
                 (:npy-scratch SV3)
                 (:npy-scratch-t SB4)))
        (:state RV3 (virtualenv-dedicated-inferior-python-buffer
                     :spawned-at (RB2 RB3) virtualenv-buffer-dedicated-inferior-python-buffer
                     :dedicated-buffer nil
                     :associatable-buffer nil
                     :venv (self => :spawned-at))
                ((:npy-run-python self)
                 (:npy-run-python-t self :message "Not associated with a file buffer.")
                 (:npy-scratch SV3)
                 (:npy-scratch-t SB4)))
        (:state RV4 (virtualenv-dedicated-inferior-python-buffer
                     :spawned-at (SV1) virtualenv-dedicated-python-scratch-buffer
                     :dedicated-buffer nil
                     :associatable-buffer nil
                     :venv (self => :spawned-at :venv)))
        (:state RV5 (virtualenv-dedicated-inferior-python-buffer
                     :spawned-at (SB2) virtualenv-buffer-dedicated-python-scratch-buffer
                     :dedicated-buffer nil
                     :associatable-buffer nil
                     :venv (self => :spawned-at :venv)))
        (:state SV1 (virtualenv-dedicated-python-scratch-buffer
                     :spawned-at (E4) python-mode-buffer-visiting-a-file-in-a-pipenv-project
                     :dedicated-buffer nil
                     :associatable-buffer nil
                     :venv (self => :spawned-at => :venv))
                ((:npy-run-python RV4)
                 (:npy-run-python-t self :message "Not associated with a file buffer.")
                 (:npy-scratch self)
                 (:npy-scratch-t self :message "Not associated with a file buffer.")))
        (:state SB2 (virtualenv-buffer-dedicated-python-scratch-buffer
                     :spawned-at (E4) python-mode-buffer-visiting-a-file-in-a-pipenv-project
                     :dedicated-buffer (self => :spawned-at)
                     :associatable-buffer (self => :spawned-at)
                     :venv (self => :spawned-at => :venv))
                ((:npy-run-python RV5)
                 (:npy-run-python-t RB3) ;; writing
                 (:npy-scratch self)
                 (:npy-scratch-t self :message "Not associated with a file buffer.")))
        (:state SV3 (virtualenv-dedicated-python-scratch-buffer
                     :spawned-at virtualenv-dedicated-inferior-python-buffer
                     :dedicated-buffer nil
                     :associatable-buffer nil
                     :venv (self => :spawned-at => :venv)))
        (:state SB4 (virtualenv-buffer-dedicated-python-scratch-buffer
                     :spawned-at virtualenv-dedicated-inferior-python-buffer
                     :dedicated-buffer (self => :spawned-at)
                     :associatable-buffer (self => :spawned-at)
                     :venv (self => :spawned-at => :venv)))

        ))
