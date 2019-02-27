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

;;; Prjectile as an example.

;; (projectile-mode
;;  ;; setup the commander bindings
;;  (projectile-commander-bindings)
;;  ;; initialize the projects cache if needed
;;  (unless projectile-projects-cache
;;    (setq projectile-projects-cache
;;          (or (projectile-unserialize projectile-cache-file)
;;              (make-hash-table :test 'equal))))
;;  (unless projectile-projects-cache-time
;;    (setq projectile-projects-cache-time
;;          (make-hash-table :test 'equal)))
;;  ;; load the known projects
;;  (projectile-load-known-projects)
;;  ;; update the list of known projects
;;  (projectile--cleanup-known-projects)
;;  (projectile-discover-projects-in-search-path)
;;  (add-hook 'find-file-hook 'projectile-find-file-hook-function)
;;  (add-hook 'projectile-find-dir-hook #'projectile-track-known-projects-find-file-hook t)
;;  (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook t t)
;;  (advice-add 'compilation-find-file :around #'compilation-find-file-projectile-find-compilation-buffer)
;;  (advice-add 'delete-file :before #'delete-file-projectile-remove-from-cache))
;; (t
;;  (remove-hook 'find-file-hook #'projectile-find-file-hook-function)
;;  (remove-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook t)
;;  (advice-remove 'compilation-find-file #'compilation-find-file-projectile-find-compilation-buffer)
;;  (advice-remove 'delete-file #'delete-file-projectile-remove-from-cache))))



;;;; A plan to make info-lookup better

;; 1 split the python info file.

;; 2 better the indexes of the info files.

;; 2 make c-h s to work in python-mode buffers with the info files.




(defun npy-advice-before-python-pytest--run (orig-fun &rest orig-args)
  ""
  (let* ((venv-root (gpc-get 'pipenv-virtualenv-root npy-env))
         (exec-path (if venv-root
                        (cons (concat venv-root "/bin/") exec-path)
                      execpath)))
    (message "exec-path: %s" exec-path)
    (let ((res (apply orig-fun orig-args)))
      res)))

(defun npy-advice-pytest-run (orig-fun &rest orig-args)
  ""
  (let* ((venv-root (gpc-get 'pipenv-virtualenv-root npy-env))
         (exec-path (if venv-root
                        (cons (concat venv-root "/bin/") exec-path)
                      execpath)))
    (npy--debug "exec-path: %s" exec-path)
    (let ((res (apply orig-fun orig-args)))
      res)))

(advice-add 'pytest-run :around #'npy-advice-pytest-run)
