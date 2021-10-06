;;; totp-test.el --- tests for totp.el               -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jürgen Hötzel

;; Author: Jürgen Hötzel <juergen@hoetzel.info>

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

;; 

;;; Code:

(require 'totp)
(require 'buttercup)

(describe "totp using 20 byte secrets"
	  :var ((test-matrix
		 '(("3132333435363738393031323334353637383930" 59 "287082")
		   ("3132333435363738393031323334353637383930" 1111111109 "081804"))))
	  (it "matches expected result"
	      (pcase-dolist (`(,secret ,time ,pin) test-matrix)
		(expect (totp secret time) :to-equal pin))))

(describe "totp using 10 byte secrets"
	  :var ((test-matrix
		 '(("A5037A674BC1D506F422" 1633531834 "255844")
		   ("A5037A674BC1D506F422" 0 "801507"))))
	  (it "matches expected result"
	      (pcase-dolist (`(,secret ,time ,pin) test-matrix)
		(expect (totp secret time) :to-equal pin))))

(provide 'totp-test)
;;; totp-test.el ends here
