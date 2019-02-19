(defmacro npy--copy-buffer-local-varq (var buffer)
  "Copy the value of a buffer-local VAR from BUFFER to the current buffer."
  `(setq ,var
         (buffer-local-value ',var ,buffer)))

(defun npy-display-mode-line ()
  "Report the Pipenv project name associated with the buffer in the modeline."
  (format "%s[v:%s]"
          npy-mode-line-prefix
          (cond ((stringp npy--pipenv-project-root) npy--pipenv-project-name)
                ((eq npy--pipenv-project-root 'no-virtualenv) npy-no-virtualenv-mark)
                (t "ERR"))))

(defun npy--copy-all-pipenv-local-vars (buffer)
  "Copy the values of Pipenv local vars from BUFFER to the current buffer."
  (npy--copy-buffer-local-varq npy--pipenv-project-root buffer)
  (npy--copy-buffer-local-varq npy--pipenv-project-name buffer)
  (npy--copy-buffer-local-varq npy--pipenv-project-name-with-hash buffer)
  (npy--copy-buffer-local-varq npy--pipenv-virtualenv-root buffer))

(npy--create-scratch-buffer (get-buffer "rdownloader.py"))

(defun npy--create-scratch-buffer (base-buf)
  "Create a scratch buffer for the Pipenv project BASE-BUF belongs."
  (let* ((project-name (buffer-local-value 'npy--pipenv-project-name base-buf))
         (new-buf-name (concat "*pyscratch[v:" project-name "]*")))
    (get-buffer-create new-buf-name)
    (with-current-buffer new-buf-name
      (unless (eql major-mode 'python-mode)
        (funcall 'python-mode))
      (npy--copy-all-pipenv-local-vars base-buf)
      (npy--redisplay-mode-line))))

(defun npy-generate-scratch-buffer ()
  "Generate a scratch buffer for the Pipenv project the current buffer belongs."
  (interactive)
  (npy--create-scratch-buffer (current-buffer)))

(let* ((base-buf (get-buffer "rdownloader.py"))
       (project-name (buffer-local-value 'npy--pipenv-project-name base-buf))
       (new-buf-name (concat "*pyscratch[v:" project-name "]*")))
  (get-buffer-create new-buf-name)
  (with-current-buffer new-buf-name
    (unless (eql major-mode 'python-mode)
      (funcall 'python-mode))
    (npy--copy-all-pipenv-local-vars base-buf)
    (npy--redisplay-mode-line)))

(let* ((base-buf (get-buffer "rdownloader.py"))
       )
  (with-current-buffer "*pyscratch[v:rdownloader]*"
    (npy--copy-all-pipenv-local-vars base-buf)
    ))

(let ((buf (get-buffer "rdownloader.py")))

  (with-current-buffer "*pyscratch*"
    (unless (eql major-mode 'python-mode)
      (funcall 'python-mode))
    (npy--copy-all-pipenv-local-vars buf)
    (npy--redisplay-mode-line)))

(defun npy--redisplay-mode-line ()
  "Update the npy modeline."
  (let ((mode-line (funcall #'npy-display-mode-line)))
    (setq npy--mode-line mode-line))
  (force-mode-line-update))


(defun npy-python-shell-get-buffer-advice (orig-fun &rest orig-args)
  "Tweak the buffer entity in ORIG-ARGS.

Replace it with the inferior process for the project exists, otherwise
leave it untouched.  ORIG-FUN should be `python-shell-get-buffer'."
  (if (derived-mode-p 'inferior-python-mode)
      (current-buffer)
    (npy--set-pipenv-project-vars)
    (if (stringp npy--pipenv-project-name) ;; i.e. it's not 'no-virtualenv nor nil.
        (if-let* ((venv-dedicated-buffer-process-name
                   (format "*%s[v:%s;b:%s]*" python-shell-buffer-name
                           npy--pipenv-project-name
                           (f-filename (buffer-file-name))))
                  (venv-dedicated-process-name (format "*%s[v:%s]*"
                                                       python-shell-buffer-name
                                                       npy--pipenv-project-name)))
            (if-let (venv-dedicated-buffer-running
                     (comint-check-proc venv-dedicated-buffer-process-name))
                venv-dedicated-buffer-process-name
              (if-let (venv-dedicated-running
                       (comint-check-proc venv-dedicated-process-name))
                  venv-dedicated-process-name
                (let ((res (apply orig-fun orig-args))) ;; Maybe raising an error is better.
                  res))))
      (let ((res (apply orig-fun orig-args)))
        res))))
