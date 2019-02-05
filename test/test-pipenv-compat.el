;;; test-npy-pipenv-compat.el --- Tests for Pipenv compatibility functions.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Cyriakus "Mukuge" Hill

;; Author: Cyriakus "Mukuge" Hill <cyriakus.h@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for Pipenv compatibility functions.

;;; Code:

(ert-deftest pp-test-quote ()
  "Tests the rendering of `quote' symbols in `pp-to-string'."
  (should (equal (pp-to-string '(quote quote)) "'quote"))
  (should (equal (pp-to-string '((quote a) (quote b))) "('a 'b)\n"))
  (should (equal (pp-to-string '('a 'b)) "('a 'b)\n")))

(ert-deftest npy-pienv-compat-hash ()
  "Test the hash functions."
  (should (equal (npy-pipenv-compat--sanitize "1 2$3`4!5*6@7\"8\\9\rA\nB\tC")
                 "1_2_3_4_5_6_7_8_9_A_B_C"))
  )

(provide 'test-npy-pipenv-compat)
;;; test-pipenv-compat.el ends here
