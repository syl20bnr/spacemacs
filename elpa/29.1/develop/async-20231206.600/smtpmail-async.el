;;; smtpmail-async.el --- Send e-mail with smtpmail.el asynchronously -*- lexical-binding: t -*-

;; Copyright (C) 2012-2022 Free Software Foundation, Inc.

;; Author: John Wiegley <jwiegley@gmail.com>
;; Created: 18 Jun 2012

;; Keywords: email async
;; X-URL: https://github.com/jwiegley/emacs-async

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

;; Send e-mail with smtpmail.el asynchronously.  To use:
;;
;;   (require 'smtpmail-async)
;;
;;   (setq send-mail-function 'async-smtpmail-send-it
;;         message-send-mail-function 'async-smtpmail-send-it)
;;
;; This assumes you already have smtpmail.el working.

;;; Code:

(defgroup smtpmail-async nil
  "Send e-mail with smtpmail.el asynchronously"
  :group 'smptmail)

(require 'async)
(require 'smtpmail)
(require 'message)

(defvar async-smtpmail-before-send-hook nil
  "Hook running in the child emacs in `async-smtpmail-send-it'.
It is called just before calling `smtpmail-send-it'.")

(defun async-smtpmail-send-it ()
  (let ((to          (message-field-value "To"))
        (buf-content (buffer-substring-no-properties
                      (point-min) (point-max))))
    (message "Delivering message to %s..." to)
    (async-start
     `(lambda ()
        (require 'smtpmail)
        (with-temp-buffer
          (insert ,buf-content)
          (set-buffer-multibyte nil)
          ;; Pass in the variable environment for smtpmail
          ,(async-inject-variables
            "\\`\\(smtpmail\\|async-smtpmail\\|\\(user-\\)?mail\\)-\\|auth-sources\\|epg\\|nsm"
            nil "\\`\\(mail-header-format-function\\|smtpmail-address-buffer\\|mail-mode-abbrev-table\\)")
          (run-hooks 'async-smtpmail-before-send-hook)
          (smtpmail-send-it)))
     (lambda (&optional _ignore)
       (message "Delivering message to %s...done" to)))))

(provide 'smtpmail-async)

;;; smtpmail-async.el ends here
