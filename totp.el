;;; totp.el --- Time-based One-time Password (TOTP) -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jürgen Hötzel

;; Author: Jürgen Hötzel <juergen@hoetzel.info>
;; Package-Requires: ((emacs "27.1"))
;; Version:    0.1
;; URL: https://github.com/juergenhoetzel/emacs-totp
;; Keywords: tools pass password

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

;; Create TOPT using gnutls for crypto and auth source for secure storage of
;; shared secret
;; Emacs 27.1 is required for bignum support
;;


;;; Code:

(require 'bindat)
(require 'gnutls)
(require 'hexl)
(require 'auth-source)

(defgroup totp '()
  "Emacs mode for  Time-based One-time Passwords (TOTP)."
  :prefix "totp-"
  :group 'totp)

(defun totp-accounts ()
  "Return List of existing account names.
The actual accounts are retrieved using `auth-source-search'.  New
accounts can be creaing entering a non-existing account name using the command `totp'."
  (mapcar (apply-partially #'string-remove-prefix "TOTP:")
	  (cl-remove-if-not (lambda (host) (string-prefix-p "TOTP:" host))
			 (mapcar (lambda (token) (plist-get token :host)) (auth-source-search :max 10000)))))


(defun totp--auth-info (account &optional create)
  "Return auth source token for ACCOUNT.
if CREATE is non-nil create a new token."
  (car
   (auth-source-search
    :host (format "TOTP:%s" account); PREFIX in order to not to come into conflict with otherentries
    :user user-login-name
    :max 1
    :create create)))

(defun totp-copy-pin-as-kill (account)
  "Copy current time pin-code of ACCOUNT into the kill ring."
  (interactive (list (completing-read "TOTP Account: " (totp-accounts) nil)))
  (let* ((auth-source-creation-prompts
	  `((secret . ,(format "TOTP hex encoded secret for %s: " account))))
	 (auth-info (totp--auth-info account t))
	 (secret (plist-get auth-info :secret))
	 (save-function (plist-get auth-info :save-function)))
    (when (functionp secret)
      (setq secret (funcall secret)))
    (unless (string-match-p "^[0-9a-fA-F]\\{2\\}+$" secret)
      (user-error "Invalid hex encoded string: %s" secret))
    (when save-function
      (funcall save-function))
    (if (eq last-command 'kill-region)
        (kill-append (totp secret) nil)
      (kill-new (totp secret)))))

(defun totp--hex-decode-string (string)
  "Hex-decode STRING and return the result as a unibyte string."
  (apply #'unibyte-string
	 (seq-map (lambda (s) (hexl-htoi (aref s 0) (aref s 1)))
		  (seq-partition string 2))))

;;;###autoload
(defun totp(string &optional time digits)
  "Return a TOTP token using the secret hex STRING and current time.
TIME is used as counter value instead of current time, if non-nil.
DIGITS is tre  number of pin digits and defaults to 6."
  (let* ((key-bytes (totp--hex-decode-string (upcase string)))
	 (counter (truncate (/ (or time (time-to-seconds)) 30)))
	 (digits (or digits 6))
	 (format-string (format "%%0%dd" digits))
	 ;; we have to manually split the 64 bit number (u64 not supported in Emacs 27.2)
	 (counter-bytes (bindat-pack  '((:high u32) (:low u32))
				      `((:high . ,(ash counter -32)) (:low . ,(logand counter #xffffffff)))))
	 (mac (gnutls-hash-mac 'SHA1 key-bytes counter-bytes))
	 (offset (logand (bindat-get-field (bindat-unpack '((:offset u8)) mac 19) :offset) #xf)))
    (format format-string
	    (mod
	     (logand (bindat-get-field (bindat-unpack '((:totp-pin u32)) mac  offset) :totp-pin)
		     #x7fffffff)
	     (expt 10 digits)))))

(provide 'totp)
;;; totp.el ends here
