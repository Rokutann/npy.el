((nil . ((eval . (progn
                   (require 'projectile)
                   (puthash (projectile-project-root)
                            "cask exec ert-runner"
                            projectile-test-cmd-map))))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
