;;; totp-widget.el --- Time-based One-time Password (TOTP): Widget      -*- lexical-binding: t; -*-

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

;; Google Authenticator like widget

;;; Code:

(require 'totp)
(require 'widget)
(require 'wid-edit)

(defvar totp-widget-progress nil)
(defvar totp-widget-timer nil)

(defun totp-widget--elapsed ()
  "Return pin code elapsed time."
  (mod (truncate (time-to-seconds)) 30))

(defun totp-widget--cancel-timer ()
  "Cancel widget redisplay timer."
  (when totp-widget-timer
    (cancel-timer totp-widget-timer)
    (setf totp-widget-timer nil)))

(defun totp-widget ()
  "Create a widget for `totp-accounts'."
  (interactive)
  (totp-widget--cancel-timer)
  (switch-to-buffer "*TOTP Accounts*")
  (kill-all-local-variables)
  (make-local-variable 'totp-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "Current TOTP accounts:\n\n")
  (widget-insert "[")
  ;; FIXME: This should be read_only
  (setf totp-widget-progress
	(widget-create 'editable-field
                       :size 30
                       :format "%v" ; Text after the field!
                       (make-string (totp-widget--elapsed) ?#)))
  (widget-insert "]\n")
  (dolist (account (totp-accounts))
    (let ((secret (plist-get  (totp--auth-info account) :secret)))
      (when (functionp secret)
	(setq secret (funcall secret)))
      (widget-insert (format "\n%s: " account))
      (widget-create 'push-button :notify (lambda (&rest _)
					    (totp-copy-pin-as-kill account)
					    (message "Code for %s copyied to kill-ring" account))
		     (totp secret))))
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup)
  ;; FIXME: Prevent race condition
  (setf totp-widget-timer (run-with-timer 1 0.7 (lambda ()
						  (let ((elapsed (totp-widget--elapsed)))
						    (if (zerop elapsed)
							(totp-widget)
						      (save-excursion
							(widget-value-set totp-widget-progress (make-string elapsed ?#))))))))
  (add-hook 'kill-buffer-hook #'totp-widget--cancel-timer nil t))

(provide 'totp-widget)
;;; totp-widget.el ends here
