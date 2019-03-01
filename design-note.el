(require 'npy)

;;;; The behavior of virtualenv-dedicated or
;;;; virtualenv-buffer-dedicated inferior python modes and python
;;;; scratch buffers.

;; The virtualenv-dedicated and virtualenv-buffer-dedicated buffers
;; follows the rules below.

;; Rules:

;; 1. A python-mode buffer visiting a file in a Pipenv project has
;; npy-child-dedicatable-to set to itself.

;; 2. If npy-child-dedicatable-to is set, the buffer can spawn any
;; virtualenv-buffer-dedicated buffers, which inherit the
;; npy-child-dedicatable-to of the parent.

;; 3. If npy-child-dedicatable-to is set, the buffer can spawn any
;; virtualenv-dedicated buffers, but the npy-child-dedicatable-to of
;; the spawned buffers will be set to nil in spawned buffers.

;; 4. If npy-child-dedicatable-to is nil, the buffer can not spawn any
;; virtualenv-dedicated buffers but can spawn any virtualenv-dedicated
;; buffers.

;; 5. When a virtualenv-buffer-dedicated buffer is spawned from a
;; python-mode buffer, its npy-dedicated-to is set to that of
;; python-mode buffer.

;; 6. When a virtualenv-buffer-dedicated buffer is spawned from a
;; virtualenv-buffer-dedicated buffer, its npy-dedicated-to is set to
;; npy-child-dedicatable-to of the parent buffer.

;; 7. If the buffer to be spawned already exists and alive,
;; pop-to-buffer it.

;; 8. If the buffer to be spawned already exists but killed, raise an
;; error.

;; 9. If the buffer npy-child-dedicatable-to points is already killed
;; when spawning a virtualenv-buffer-dedicated buffer, raise an error.

;; 10. The precedence list of a python-mode buffer visiting a file in
;; a Pipenv project is: virtualenv-buffer-dedicated,
;; virtualenv-dedicated, dedicated, global.

;; 11. The precedence list of a virtualenv-dedicated buffer is:
;; virtualenv-dedicated, dedicated, global.

;; 12. The precedence list of a virtualenv-buffer-dedicated buffer is:
;; virtualenv-buffer-dedicated, virtualenv-dedicated, dedicated,
;; global.

;; 13. If the buffer to send a string exists but killed, don't raise
;; an error, just move down the precedence list.


;;;; A plan to make npy to work with any python-related modes.

;; 1. populate npy-env to all buffers.

;; 1.a. hook find-file-noselect for file buffers. The value of npy-env
;; is obtained by exploring the system from (buffer-file-name)

;; Note:get-file-buffer seems the father of all find-file functions.

;; -> Turned out hooking find-file-noselect is powerful but needless
;; -> for now. Other things below collaboratively do a good job.

;; Here is the log.

(defun npy-advise-get-file-buffer (orig-fun &rest orig-args)
  "Advise `get-file-buffer' :around with ORIG-FUN and ORIG-ARGS."
  (message "get-file-buffer: %s" orig-args)
  (let ((res (apply orig-fun orig-args)))
    (message "in npy-env init step res: %s" res)
    res))

(advice-add 'get-file-buffer :around 'npy-advise-get-file-buffer)
(advice-remove 'get-file-buffer 'npy-advise-get-file-buffer)

;; So I've tried this advice to get-file-buffer. But the
;; get-file-buffer returned nil always...

;; Next, I've tried advising find-file-noselect:

(defun npy-advise-find-file-noselect (orig-fun &rest orig-args)
  "Advise `find-file-noselet' :around with ORIG-FUN and ORIG-ARGS."
  (message "find-file-noselect: %s" orig-args)
  (let ((res (apply orig-fun orig-args)))
    (message "its response: %s" res)
    (message "its buffer-file-name: %s" (buffer-file-name res))
    (when (and res (buffer-file-name res))
      (with-current-buffer res
        (gpc-get 'pipenv-project-name npy-env)
        (gpc-get 'pipenv-project-name-with-hash npy-env)))
    res))

(advice-add 'find-file-noselect :around 'npy-advise-find-file-noselect)
(advice-remove 'find-file-noselect 'npy-advise-find-file-noselect)

;; This worked fine.  But I was kind of surprised when I noticed that
;; find-file-noselect is called really frequently.  So, advising this
;; function costs a lot.

;; 1.b. advise functions to create buffers to copy the value on
;; npy-env from the parent.

;; Note: get-buffer-create seems the father of all file-less buffer
;; generate functions such as those in `comint' and `compilation'.

(defun npy-advise-get-buffer-create (orig-fun &rest orig-args)
  "Advise `get-buffer-create' :around with ORIG-FUN and ORIG-ARGS."
  (message "get-buffer-create %s" orig-args)
  (let ((res (apply orig-fun orig-args)))
    (message "its response: %s" res)
    (when res
      (if (buffer-file-name res)
          (with-current-buffer res
            (message "this is a file buffer.")
            (gpc-get 'pipenv-project-name npy-env)
            (gpc-get 'pipenv-project-name-with-hash npy-env)))
      (let ((parent (current-buffer)))
        (message "this is a non file buffer.")
        (message "parent: %s" parent)
        (gpc-get 'pipenv-project-name npy-env)
        (gpc-get 'pipenv-project-name-with-hash npy-env)
        (let ((npy-env-value (copy-alist npy-env)))
          (with-current-buffer res
            (message "npy-env pipenv-project-name: %s" (gpc-val 'pipenv-project-name npy-env))
            (unless npy-env
              (setq npy-env npy-env-value)
              (message "npy-env set: %s" (gpc-val 'pipenv-project-name npy-env))
              (gpc-lock npy-env))))))
    res))

(defun npy-advise-get-buffer-create (orig-fun &rest orig-args)
  "Advise `get-buffer-create' :around with ORIG-FUN and ORIG-ARGS."
  (npy--debug "get-buffer-create %s" orig-args)
  (let ((res (apply orig-fun orig-args)))
    (when res
      (if (buffer-file-name res)
          (with-current-buffer res
            (message "this is a file buffer.")
            (gpc-get 'pipenv-project-name npy-env)
            (gpc-get 'pipenv-project-name-with-hash npy-env)))
      (let ((parent (current-buffer)))
        (message "this is a non file buffer.")
        (message "parent: %s" parent)
        (gpc-get 'pipenv-project-name npy-env)
        (gpc-get 'pipenv-project-name-with-hash npy-env)
        (let ((npy-env-value (copy-alist npy-env)))
          (with-current-buffer res
            (message "npy-env pipenv-project-name: %s" (gpc-val 'pipenv-project-name npy-env))
            (unless npy-env
              (setq npy-env npy-env-value)
              (message "npy-env set: %s" (gpc-val 'pipenv-project-name npy-env))
              (gpc-lock npy-env))))))
    res))

(advice-add 'get-buffer-create :around 'npy-advise-get-buffer-create)
(advice-remove 'get-buffer-create 'npy-advise-get-buffer-create)

;; This doesn't work. I don't know why. The log messages seemed
;; good. But the npy-env value set to nil somewhere, probably, in the
;; bootstrapping procedures of minor-modes where they often kill all
;; local variables.


;; `get-buffer-create' is called often for both non file buffers and
;; file buffers.  In some cases, it's called for file buffers before
;; files are actually attached.  So, advising `get-buffer-create' is
;; not a good idea.

;; I've tried various hooks and advice functions. And my choice for
;; now is:

;; * dired-mode-hook to set npy-env for the default directories.

;; * compilation-mode-hook to set npy-env for the default directories.

;; * advice to getenv to dynamically add a virtualenv root bin to the
;;   call to getenv with "PATH".  This is for the function calls in
;;   the buffers with npy-env set: python-mode buffers,
;;   inferior-python-mode-buffers, scratch buffers for python-mode,
;;   dired-mode-buffers, compilation-mode-buffers.

;; * advice to make-process to dynamically insert the virtualenv root
;;   bin to getenv "PATH" calls in npy-ish buffers just before the
;;   calls to make-process.  I thought the advice to getenv itself
;;   above is sufficient but I was wrong.

;; None of pythonic modes uses `exec-path' so far. So, we commented
;; out the lines to modify `exec-path' in these hacks.

;; `anaconda-mode' uses `python-shell-virtualenv-root'. So, we need a
;; better handling over it.  `npy-run-python' set
;; `python-shell-virtualenv-root' temporarily inside.  We may be able
;; to delete this manipulation after establishing the better handling.


(provide 'design-note)
;;; design-note.el ends here
