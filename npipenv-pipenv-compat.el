;;; npipenv-pipenv-compat.el --- Pipenv compatibility functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 's)

(defun npipenv-pipenv-compat--sanitize (name)
  "Return sanitized NAME.

Replace dangerous characters and cut it to the first 42 letters
if it's longer than 42."
  (let ((char-cleaned (replace-regexp-in-string "[ $`!*@\"\\\r\n\t]" "_" name)))
    (if (> (length char-cleaned) 42)
        (substring char-cleaned 0 41)
      char-cleaned)))
;; (npipenv-pipenv-compat--sanitize "1$2`3!*4@\"\\5
;; 6\	")

(defun npipenv-pipenv-compat-base64-strip-padding-equals (str)
  "Strip trailing padding equal sings from STR."
  (replace-regexp-in-string "=+$" "" str))

(defun npipenv-pipenv-compat-base64-add-padding-equals (str)
  "Add trailing padding equal signs to STR."
  (let ((remaining (% (length str) 4)))
    (concat str
            (case remaining
              (1 "===")
              (2 "==")
              (3 "=")
              (otherwise "")))))

(defun npipenv-pipenv-compat-base64-to-base64urlsafe (str)
  "Convert STR (base64-encoded) to a base64urlsafe-encoded string."
  (npipenv-pipenv-compat-base64-add-padding-equals
   (replace-regexp-in-string
    "/" "_"
    (replace-regexp-in-string
     "+" "-"
     (npipenv-pipenv-compat-base64-strip-padding-equals str)))))

(defun npipenv-pipenv-compat-base64urlsafe-to-base64 (str)
  "Convert STR (base64urlsafe-encoded) to a base64-encoded string."
  (npipenv-pipenv-compat-base64-add-padding-equals
   (replace-regexp-in-string
    "_" "/"
    (replace-regexp-in-string
     "-" "+"
     (npipenv-pipenv-compat-base64-strip-padding-equals str)))))

(defun npipenv-pipenv-compat-base64urlsafe-encode (str)
  "Encode STR in base64urlsafe."
  (npipenv-pipenv-compat-base64-to-base64urlsafe
   (base64-encode-string str)))

(defun npipenv-pipenv-compat-base64urlsafe-decode (str)
  "Decode STR inbase64urlsafe."
  (base64-decode-string
   (npipenv-pipenv-compat-base64urlsafe-to-base64
    str)))

(defun npipenv-pipenv-compat-hex2bin (byte-sequence-in-hex)
  "Convert BYTE-SEQUENCE-IN-HEX into a binary sequence."
  (with-temp-buffer
    (setq buffer-file-coding-system 'raw-text)
    (let ((byte-in-hex ""))
      (while (> (length byte-sequence-in-hex) 0)
        (setq byte-in-hex (substring byte-sequence-in-hex 0 2))
        (insert (string-to-number byte-in-hex 16))
        (setq byte-sequence-in-hex (substring byte-sequence-in-hex 2 nil))))
    (buffer-string)))
;; (npipenv-pipenv-compat-hex2bin "417a667cb8")

;; I haven't implemented the PIPENV_PYTHON feature since it's for CI purpose.
;; See: https://github.com/pypa/pipenv/issues/2124

(defun npipenv-pipenv-compat--get-virtualenv-hash (name)
  "Return the cleaned NAME and its encoded hash."
  (let* ((clean-name (npipenv-pipenv-compat--sanitize name))
         (hash (secure-hash 'sha256 clean-name))
         (bin-hash (substring (npipenv-pipenv-compat-hex2bin hash) 0 6))
         (encoded-hash (npipenv-pipenv-compat-base64urlsafe-encode bin-hash)))
    (cl-values clean-name encoded-hash)))
;; (npipenv-pipenv-compat--get-virtualenv-hash "/i/hate/spam$")

(defun npipenv-pipenv-compat-virtualenv-name (name)
  "Return the virtualenv name of a NAME or path."
  (cl-multiple-value-bind (sanitized encoded-hash)
      (npipenv-pipenv-compat--get-virtualenv-hash name)
    (concat sanitized "-" encoded-hash)))
;; (npipenv-pipenv-compat-virtualenv-name "/i/hate/spam")





(provide 'npipenv-pipenv-compat)

;;; npipenv-pipenv-compat.el ends here
