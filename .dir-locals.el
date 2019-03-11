((nil . ((eval . (progn
                   (require 'projectile)
                   (puthash (projectile-project-root)
                            "cask exec buttercup test/essential -L . -l test/test-helper.el -l test/npy-buttercup-init.el"
                            projectile-test-cmd-map))))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
