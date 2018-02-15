;;; funcs.el -- Passwords Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Andrew Oppenlander <andrew.oppenlander@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//pass-completing-read ()
  "Read a password entry in the minibuffer, with completion."
  (completing-read "Password entry: " (password-store-list)))

(defun spacemacs/pass-copy-and-describe (entry)
  "Copy the password to the clipboard, and show the multiline description for ENTRY"
  (interactive (list (spacemacs//pass-completing-read)))
  (password-store-copy entry)
  (spacemacs/pass-describe entry))

(defun spacemacs/pass-describe (entry)
  "Show the multiline description for ENTRY"
  (interactive (list (spacemacs//pass-completing-read)))
  (let ((description (s-join "\n" (cdr (s-lines (password-store--run-show entry))))))
    (message "%s" description)))
