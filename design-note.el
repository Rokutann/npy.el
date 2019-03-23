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


;;; About the interface between the file system and buffers.

;; * The original information about projects such as Pipenv and pip
;;   projects resides in the file system.

;; * The behavior of functions varies buffer to buffer along with
;;   which file it's visiting or the `default-directory' of the
;;   buffer.

;; * What kind of layer(s) we define between these two is the crux of
;;   the design of this software.

;; * As of v0.1.5, we have `npy-env', a buffer-local variable which
;;   holds the values of the projects the file visited by the buffer
;;   belongs to.

;; * Most of such information's scope is not buffer-local but
;;   dir-local.  So, we've created pools to hold the mapping between
;;   projects and their root directories.

;; * So, the current interface design is:

;; file system <-> pool: dir-proj mapping <-> cache: proj-buffer mapping

;; * As of v0.1.5, the pools are dedicated to a type of project. For
;;   example, `pipenv-known-projects' pool is only for Pipenv
;;   projects.

;; * Is this okay? Should we put together all the pools' information
;;   into a single pool?

;; * Yes. Because, project detections should be done not separately
;;   but all at once. For, we need to ask the user's decision if a
;;   file belongs to two different types of projects.

;; * Then how?

;; * The key should be a path, and the value should be a list of
;;   projects it belongs to and the additional information for the
;;   projects.

;; * The path should be dealt as a string or a list?

;; * Maybe a list, and the pool structure should be some kind of tree
;;   structures such as a kind of trie. Because, it it's a sting, to
;;   get information of its parent dir, we need to search the pool
;;   with the path of the parent again. It's unnatural, because if
;;   it's a list we just grab the parent by following cons cells.

;; * Elisp allows any letter to be used in symbol names. So, if we go
;;   with a trie, we can use a symbol instead of a string as the value
;;   of a node if symbol lookup is so faster than `string='.  But it
;;   would create lots of symbols that can be shown to the user when
;;   she does some completion if a dedicated obarray for a trie
;;   couldn't be implemented.

;; * We'll try implementing a trie in v0.1.7.

;; * So, what's the testing strategy for v0.1.6?

;; * Mainly add integration tests for new user facing functions. It's
;;   too early to write unit tests, since we'll rewrite pool logics in
;;   v0.1.7.

;; * Let's try `buttercup'. When it comes to integration testing, ert
;;   seemingly can manage not so large number of tests, since putting
;;   all descriptions of a test into a function name make reading
;;   tests hard.

;;; About PIPENV_MAX_DEPTH and PIPENV_NO_INHERIT

;; * pipenv has PIPENV_MAX_DEPTH and PIPENV_NO_INHERIT which can
;;   affect the relationships between a directory with a Pipfile and
;;   its subdirectories. We need to deal with this in the future
;;   release.

;; * The Pipenv poject information on the mode line should follow the
;;   behavior of the pipenv command.  So, if the user doesn't set
;;   PIPENV_MAX_DEPTH, at the directories deeper than grandchildren
;;   from a Pipenv project root, the mode line should tell the user
;;   that you are not in any Pipenv projects.

;; * The cache mechanism of v0.1.6 treats Pipenv projects as a simple
;;   top-down directory project.  We need more advanced cache
;;   mechanism to deal with the complication of the Pipenv project
;;   structures.

;; * A variant of trie seems suitable to implement a new cache
;;   mechanism.

;; * However, we need to think about how to deal with Pipenv projects
;;   on remote hosts, hopefully in collaboration with the anaconda
;;   mode.

;;; About the original python inferior buffers

;; * Should introduce a new flag: npy-buffer-original-inferior to
;;   isolate original inferior buffers from npy mechanism.

;;; About the global inferior python buffer

;; * Currently, if you want to spawn the global inferior python
;;   buffer, you need to call run-python on a non Pipenv python-mode
;;   buffer. It would be convenient if you could spawn the global
;;   inferior python buffer from anywhere, for example, by putting
;;   some prefix to npy-run-python.

;;; About the buttercup framework

;; * Buttercup ignores errors in setup scripts, suppressing debug
;;   messages and/or debugger calls.  So, it's difficult to debug
;;   setup scripts in before-all and/or before-each. We should write
;;   and debug a setup script as an independent function, then put it
;;   in before-all or before-each.

;; * The describe macro takes :var as an argument to define variables
;;   with let for the execution of the rest of the argument, but it's
;;   not documented.

;; * The features I want but couldn't find so far with Buttercup are:
;;   1) a command line option to set not a test directory but test
;;   file(s), 2) executing only tests previously failed, and 3) a flag
;;   to activate debug-on-error.

(provide 'design-note)
;;; design-note.el ends here
