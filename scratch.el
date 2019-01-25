;;; scratch.el --- Memo -*- lexical-bindig: t; -*-

;;; Commentary:

;;; Code:

(require 'f)

(file-exists-p "/Users/yfuna/Pipenv")

(f-full ".")

(f-split
 (f-dirname
  (buffer-file-name (current-buffer))))
("/" "Users" "yfuna" "dev" "npipenv")


(("/")
 ("/Users")
 ("/Users/yfuna"))


(defun npipenv--get-pipenv-project-root-by-exploring (dirname-list)
  "DIRNAME-LIST."
  (if (null dirname-list)
      nil
    (let ((dirname (apply #'f-join dirname-list)))
      (if (npipenv--is-pipenv-root? dirname)
          dirname
        (npipenv--get-pipenv-project-root-by-exploring (nbutlast dirname-list 1))))))

(npipenv--get-pipenv-project-root-by-exploring '("/" "Users" "yfuna" "dev" "npipenv"))
(npipenv--get-pipenv-project-root-by-exploring '("/" "Users" "yfuna" "dev" "rdownloader"))
(npipenv--get-pipenv-project-root-by-exploring '("/" "Users" "yfuna" "dev" "rdownloader" "src"))
(npipenv--get-pipenv-project-root-by-exploring-impl '("/" "Users" "yfuna" "dev" "npipenv"))
(npipenv--get-pipenv-project-root-by-exploring-impl '("/" "Users" "yfuna" "dev" "rdownloader"))
(npipenv--get-pipenv-project-root-by-exploring-impl '("/" "Users" "yfuna" "dev" "rdownloader" "src"))
(npipenv--get-pipenv-project-root-by-exploring ".")
(f-filename "/usr/local")


(defun npipenv--is-pipenv-root? (dirname)
  "DIRNAME."
  (f-exists? (concat dirname "/Pipfile")))

(is-pipenv-root? "/Users/yfuna/dev/rdownloader")

(let* ((filename ))
  path)

(provide 'scratch)
;;; scratch.el ends here
