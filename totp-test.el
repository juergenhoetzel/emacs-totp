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

(describe "base32"
	  (it "decodes encodings without padding"
	      (expect (format "%X" (totp--base32-to-number "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ"))
		      :to-equal "3132333435363738393031323334353637383930"))
	  (it "decodes encodings with interleaved whitespace and missing padding"
	      (expect (format "%X" (totp--base32-to-number "6BQA C4L3 O2FZ YNKR 6H7H IFFX 64GG SZZK SDEO FMV4 KVKI ASYE 65QA"))
		      :to-equal "F06001717B768B9C3551F1FE7414B7F70C69672A90C8E2B2BC5554804B04F760"))
	  (it "decodes encodings with padding"
	      (expect (format "%X" (totp--base32-to-number "6BQAC4L3O2FZYNKR6H7HIFFX64GGSZZKSDEOFMV4KVKIASYE65QA===="))
		      :to-equal "F06001717B768B9C3551F1FE7414B7F70C69672A90C8E2B2BC5554804B04F760")))

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

(describe "totp using 32 byte secrets"
	  :var ((time 1672587950.4334695)
		(pin "091645"))
	  (it "in base32 with interleaved whitespace and missing padding matches expected token"
	      (expect (totp "6BQA C4L3 O2FZ YNKR 6H7H IFFX 64GG SZZK SDEO FMV4 KVKI ASYE 65QA" time) :to-equal pin))
	  (it "in base32 matches expected token"
	      (expect (totp "6BQAC4L3O2FZYNKR6H7HIFFX64GGSZZKSDEOFMV4KVKIASYE65QA====" time) :to-equal pin)))

(provide 'totp-test)
;;; totp-test.el ends here
