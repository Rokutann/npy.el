;;;; A plan to make npy to work with any python-related modes.

;; 1. populate npy-env to all buffers.

;; 1.a. hook find-file-noselect for file buffers. The value of npy-env
;; is obtained by exploring the system from (buffer-file-name)

;; 1.b. advice functions to create buffers to copy the value on
;; npy-env from the parent.

;; 2. manipulate the path providing entities: exec-path and (getenv
;; "PATH")

;; 2.a advise entry functions of python-related mode with let binding
;; to manipulate exec-path for each call.

;; 2.b advise getenv to append venv-root when the buffer has stringp
;; venv root.

;; 3 implement activation functions

;; 3.a npy-activate-automatic enables automatic activation for all
;; buffers.

;; 3.b npy-activate-this-venv activates the feature only for this
;; venv.

;;;; A plan to make info-lookup better

;; 1 split the python info file.

;; 2 better the indexes of the info files.

;; 2 make c-h s to work in python-mode buffers with the info files.


(defun npy-advice-python-pytest--run (orig-fun &rest orig-args)
  ""
  (let* ((venv-root (gpc-get 'pipenv-virtualenv-root npy-env))
         (exec-path (if venv-root
                        (cons (concat venv-root "/bin/") exec-path)
                      execpath)))
    (message "exec-path: %s" exec-path)
    (let ((res (apply orig-fun orig-args)))
      res)))

(defun npy-advice-before-python-pytest--run (orig-fun &rest orig-args)
  ""
  (let* ((venv-root (gpc-get 'pipenv-virtualenv-root npy-env))
         (exec-path (if venv-root
                        (cons (concat venv-root "/bin/") exec-path)
                      execpath)))
    (message "exec-path: %s" exec-path)
    (let ((res (apply orig-fun orig-args)))
      res)))

(advice-add 'python-pytest--run :around #'npy-advice-python-pytest--run)
