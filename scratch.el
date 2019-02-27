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

(getenv "PATH" )
(setenv "PATH" (concat (gpc-get 'pipenv-virtualenv-root npy-env) "/bin/:" (getenv "PATH")))


(defun npy-advice-getenv (orig-fun &rest orig-args)
  ""
  (let ((res (apply orig-fun orig-args)))
    (when (equal (car orig-args) "PATH")
      (let* ((venv-root (gpc-get 'pipenv-virtualenv-root npy-env))
             (venv-bin-path (when (stringp venv-root)
                              (concat venv-root "/bin/"))))
        (message "hel: %s" venv-root)
        (when venv-bin-path
          (setq res (concat venv-bin-path ":" res)))
        ))
    res))

(advice-add 'getenv :around #'npy-advice-getenv)
(advice-remove 'getenv #'npy-advice-getenv)


(require 'info-look)

(defvar python-dotty-syntax-table)

(defcustom npy-info-current-symbol-functions
  '(npy-info-current-symbol-python-el current-word)
  "Functions to be called to fetch symbol at point.
Each function is called with no argument and if it returns a
string, that value is used.  If it returns nil, next function
is tried.
Default (fallback to `current-word' when not using python.el):
   '(`npy-info-current-symbol-python-el' `current-word')."
  :group 'npy-info)

(defun npy-info-current-symbol-python-el ()
  "Return current symbol.  Requires python.el."
  (when (featurep 'python)
    (with-syntax-table python-dotty-syntax-table
      (current-word))))

(defun npy-python-symbol-at-point ()
  "Return the current Python symbol."
  (cl-loop for func in npy-info-current-symbol-functions
           when (funcall func)
           return it))

;;;###autoload
(defun npy-info-add-help (files &rest more-specs)
  "Add help specifications for a list of Info FILES.

The added specifications are tailored for use with Info files
generated from Sphinx documents.

MORE-SPECS are additional or overriding values passed to
`info-lookup-add-help'."
  (info-lookup-reset)
  (let (doc-spec)
    (dolist (f files)
      (push (list (format "(%s)Python Module Index" f)
                  'npy-info-lookup-transform-entry) doc-spec)
      (push (list (format "(%s)Index" f)
                  'npy-info-lookup-transform-entry) doc-spec))
    (apply 'info-lookup-add-help
           :mode 'python-mode
           :parse-rule 'npy-info-python-symbol-at-point
           :doc-spec doc-spec
           more-specs)))

;;;###autoload
(npy-info-add-help '("python372api"))

(defun npy-info-lookup-transform-entry (entry)
  "Transform a Python index ENTRY to a help item."
  (let* ((py-re "\\([[:alnum:]_.]+\\)(?)?"))
    (cond
     ;; foo.bar --> foo.bar
     ((string-match (concat "\\`" py-re "\\'") entry)
      entry)
     ;; keyword; foo --> foo
     ;; statement; foo --> foo
     ((string-match (concat "\\`\\(keyword\\|statement\\);? " py-re) entry)
      (replace-regexp-in-string " " "." (match-string 2 entry)))
     ;; foo (built-in ...) --> foo
     ((string-match (concat "\\`" py-re " (built-in .+)") entry)
      (replace-regexp-in-string " " "." (match-string 1 entry)))
     ;; foo.bar (module) --> foo.bar
     ((string-match (concat "\\`" py-re " (module)") entry)
      (replace-regexp-in-string " " "." (match-string 1 entry)))
     ;; baz (in module foo.bar) --> foo.bar.baz
     ((string-match (concat "\\`" py-re " (in module \\(.+\\))") entry)
      (replace-regexp-in-string " " "." (concat (match-string 2 entry) " "
                                                (match-string 1 entry))))
     ;; Bar (class in foo.bar) --> foo.bar.Bar
     ((string-match (concat "\\`" py-re " (class in \\(.+\\))") entry)
      (replace-regexp-in-string " " "." (concat (match-string 2 entry) " "
                                                (match-string 1 entry))))
     ;; bar (foo.Foo method) --> foo.Foo.bar
     ((string-match
       (concat "\\`" py-re " (\\(.+\\) \\(method\\|attribute\\))") entry)
      (replace-regexp-in-string " " "." (concat (match-string 2 entry) " "
                                                (match-string 1 entry))))
     ;; foo (C ...) --> foo
     ((string-match (concat "\\`" py-re " (C .*)") entry)
      (match-string 1 entry))
     ;; operator; foo --> foo
     ((string-match "\\`operator; \\(.*\\)" entry)
      (match-string 1 entry))
     ;; Python Enhancement Proposals; PEP XXX --> PEP XXX
     ((string-match "\\`Python Enhancement Proposals; \\(PEP .*\\)" entry)
      (match-string 1 entry))
     ;; RFC; RFC XXX --> RFC XXX
     ((string-match "\\`RFC; \\(RFC .*\\)" entry)
      (match-string 1 entry))
     (t
      entry))))
