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

(setq rules
      '("A python-mode buffer visiting a file in a Pipenv project has :dedicatable-to set to itself."
        "If :dedicatable-to is set, the buffer can spawn any virtualenv-buffer dedicated buffers which inherit the :dedicatable-to."
        "If :dedicatable-to is set, the buffer can spawn any virtualenv-dedicated buffers but the :dedicatable-to of the spawned buffers is set to nil."
        "If :dedicatable-to is nil, the buffer can spawn any virtualenv-dedicated buffers."
        "If the buffer to spawn already exists and alive, pop up it."
        "If the buffer to spawn already exists but killed, raise an error."
        "If :dedicatable-to is killed when spawning a virtualenv-buffer-dedicated buffer, raise an error."
        "The presedence list of a python-mode buffer visiting a file in a Pipenv project is: virtualenv-buffer-dedicated, virtualenv-dedicated, dedicated, global."
        "The presedence list of a virtualenv-dedicated buffer is: virtualenv-dedicated, dedicated, global."
        "The presedence list of a virtualenv-buffer-dedicated buffer is: virtualenv-buffer-dedicated, virtualenv-dedicated, dedicated, global."
        "If the buffer to send a string exists but killed, move down the precedence list."
        ))

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
