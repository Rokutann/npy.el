;;;; A plan to make npy to work with any python-related modes.

;; 1. populate npy-env to all buffers.

;; 1.a. hook find-file-noselect for file buffers. The value of npy-env
;; is obtained by exploring the system from (buffer-file-name)

;; 1.b. advice functions to create buffers to copy the value on
;; npy-env from the parent.

;; Note: get-file-buffer seems the father of all find-file functions.

(defun npy-advise-get-file-buffer (orig-fun &rest orig-args)
  "Advise `get-file-buffer' :around with ORIG-FUN and ORIG-ARGS."
  (message "get-file-buffer: %s" orig-args)
  (let ((res (apply orig-fun orig-args)))
    (message "in npy-env init step res: %s" res)
    res))

(advice-add 'get-file-buffer :around 'npy-advise-get-file-buffer)
(advice-remove 'get-file-buffer 'npy-advise-get-file-buffer)

;; So I've tried these. But the get-file-buffer returned nil always...

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

;; This works fine for now.

;; Note: get-buffer-create seeems the father of all fileless buffer
;; generate functiosn such as those in `comint' and `compilation'.

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
;; good. But the npy-env values set diappeared somewhere.

;; `get-buffer-create' is called often for both non file buffers and
;; file buffers.  In some cases, it's called for file buffers before
;; files are actually attached.  So, advising `get-buffer-create' is
;; not an good idea.

;; So, let's go with the combination of `find-file-noselect' and some
;; hoooks or advices for non-file buffer.

;; Firstly, dired.

(defun npy-dired-mode-hook-function ()
  "Get the name and root of a Pipenv project, and update the mode line.

This is for the global minor mode version to come."
  (when dired-directory
    (message "dired-directory: %s" dired-directory)
    (let* ((project-root (npy--find-pipenv-project-root-by-exploring (f-full dired-directory)))
           (project-name (when project-root (f-filename project-root))))
      (gpc-set 'pipenv-project-root project-root npy-env)
      (gpc-set 'pipenv-project-name project-name npy-env)
      (gpc-lock npy-env))))

(add-hook 'dired-mode-hook 'npy-dired-mode-hook-function)
(remove-hook 'dired-mode-hook 'npy-dired-mode-hook-function)

(defun npy-dired-mode-hook-function ()
  "Get the name and root of a Pipenv project, and update the mode line.

This is for the global minor mode version to come."
  (when default-directory
    (message "default-directory: %s" default-directory)
    (let* ((project-root (npy--find-pipenv-project-root-by-exploring (f-full default-directory)))
           (project-name (when project-root (f-filename project-root))))
      (gpc-set 'pipenv-project-root project-root npy-env)
      (gpc-set 'pipenv-project-name project-name npy-env)
      (gpc-fetch 'pipenv-virtualenv-root npy-env))))

(add-hook 'dired-mode-hook 'npy-dired-mode-hook-function)
(remove-hook 'dired-mode-hook 'npy-dired-mode-hook-function)

(add-hook 'compilation-mode-hook 'npy-dired-mode-hook-function)
(remove-hook 'compilation-mode-hook 'npy-dired-mode-hook-function)



(defun npy-advise-compilation-find-file (orig-fun &rest orig-args)
  "Advise `compilation-find-file' :around with ORIG-FUN and ORIG-ARGS."
  (message "compilation-find-file %s" orig-args)
  (let ((res (apply orig-fun orig-args)))
    res))
(advice-add 'compilation-find-file :around 'npy-advise-compilation-find-file)
(advice-remove 'compilation-find-file 'npy-advise-compilation-find-file)

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


;;; Give advice to getenv. (EXPERIMENTAL)

(defun npy-advice-getenv (orig-fun &rest orig-args)
  "Append a virtualenv path when possible."
  (let ((res (apply orig-fun orig-args)))
    (when (equal (car orig-args) "PATH")
      (let* ((venv-root (gpc-get 'pipenv-virtualenv-root npy-env))
             (venv-bin-path (when (stringp venv-root)
                              (concat venv-root "/bin/"))))
        (npy--debug "getenv: args=%s venv-root %s" orig-args venv-root)
        (when venv-bin-path
          (setq res (concat venv-bin-path ":" res)))))
    res))

(advice-add 'getenv :around #'npy-advice-getenv)
(advice-remove 'getenv #'npy-advice-getenv)

(defun npy-util-klist-to-alist (keyword-list)
  "Return an association list consisting of the keyword and value of KEYWORD-LIST."
  (let ((res nil))
    (while keyword-list
      (push (cons (nth 0 keyword-list) (nth 1 keyword-list)) res)
      (setq keyword-list (cddr keyword-list)))
    res))

(defun npy-util-alist-to-klist (alist)
  "Return a keyword list consisting of the key and value of ALIST."
  (let ((res nil))
    (while alist
      (push (car (car alist)) res)
      (push (cdr (car alist)) res)
      (setq alist (cdr alist)))
    (reverse res)))

(npy-util-klist-to-alist '(:hoge piyo :puyo poyo))

(defun npy-advise-make-process (orig-fun &rest orig-args)
  "Append a virtualenv path when possible."
  (message "make-process advice called in %s" (current-buffer))
  (let ((venv-root (gpc-fetch 'pipenv-virtualenv-root npy-env)))
    (message "venv-root: %s" venv-root)
    (when (and venv-root
               (member :command orig-args))
      (let* ((arg-alist (npy-util-klist-to-alist orig-args))
             (command (alist-get :command arg-alist)))
        (when command
          (let ((maybe-command (concat venv-root "/bin/" command)))
            (when (f-exists-p maybe-command)
              (nalist-set :command maybe-command arg-alist)
              (setq orig-args (npy-util-alist-to-klist arg-alist))))))))
  (let ((res (apply orig-fun orig-args)))
    res))

(require 's)

(defun npy-advise-process-creation (orig-fun &rest orig-args)
  "Tweak `exec-path' and PATH during a `make-process' call.

ORIG-FUN should be `make-process', and ORIG-ARGS is the original
arguments of it when called."
  (npy--debug "process creation advice called in %s" (current-buffer))
  (let* ((venv-root (gpc-val 'pipenv-virtualenv-root npy-env)))
    (if (stringp venv-root)
        (let* ((venv-bin-path (concat venv-root "/bin/"))
               ;; (exec-path (cons venv-bin-path exec-path)) ; `make-process' doesn't need this.
               (orig-path (getenv "PATH")))
          (setenv "PATH" (concat venv-bin-path orig-path))
          (let* ((res (apply orig-fun orig-args))
                 (paths (s-split ":" (getenv "PATH")))
                 (new-paths (cl-remove venv-bin-path paths)))
            (setenv "PATH" (s-join ":" new-paths))
            res))
      (let ((res (apply orig-fun orig-args)))
        res))))

(advice-add 'make-process :around #'npy-advise-process-creation)
(advice-remove 'make-process  #'npy-advise-process-creation)

(defun npy-activate-virtualenv-automatic ()
  "Activate and change virtualenvs automatically following buffer positions in the system."
  (advice-add 'make-process :around #'npy-advise-process-creation)
  (advice-add 'getenv :around #'npy-advice-getenv))

(defun npy-deactivate-virtualenv-automatic ()
  "Deactivate virtualenv automatic."
  (advice-remove 'make-process  #'npy-advise-process-creation)
  (advice-remove 'getenv #'npy-advice-getenv))

;; The last one works.



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
p
(advice-add 'pytest-run :around #'npy-advice-pytest-run)
