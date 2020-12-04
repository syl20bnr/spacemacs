;;; funcs.el -- Passwords Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Andrew Oppenlander <andrew.oppenlander@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//pass-completing-read ()
  "Read a password entry in the minibuffer, with completion."
  (completing-read "Password entry: " (password-store-list)))

(defun space-macs/pass-copy-and-describe (entry)
  "Copy the password to the clipboard, and show the multiline description for ENTRY"
  (interactive (list (space-macs//pass-completing-read)))
  (password-store-copy entry)
  (space-macs/pass-describe entry))

(defun space-macs/pass-describe (entry)
  "Show the multiline description for ENTRY"
  (interactive (list (space-macs//pass-completing-read)))
  (let ((description (s-join "\n" (cdr (s-lines (password-store--run-show entry))))))
    (message "%s" description)))


