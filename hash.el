;;; hash.el --- A Pipenv hash feature implementation -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)

(defun _sanitize (name)
  "NAME."
  (let ((converted (replace-regexp-in-string "[ $`!*@\"\\\r\n\t]" "_" name)))
    (if (> (length converted) 42)
        (substring converted 0 41)
      converted)))

(_sanitize "1$2`3!*4@\"\\5
6\	")

(provide 'hash)

;;; hash.el ends here
