;; ;;; packages.el --- pass Layer packages File for Spacemacs
;; ;;
;; ;; Copyright (c) 2012-2014 Sylvain Benner
;; ;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;; ;;
;; ;; Author: Andrew Oppenlander <andrew.oppenlander@gmail.com>
;; ;; URL: https://github.com/syl20bnr/spacemacs
;; ;;
;; ;; This file is not part of GNU Emacs.
;; ;;
;; ;;; License: GPLv3

(setq pass-packages '(password-store))

(defun pass/init-password-store ()
  (use-package password-store
    :defer t
    :init
    (evil-leader/set-key
      "Psy" 'password-store-copy
      "Psg" 'password-store-generate
      "Psi" 'password-store-insert
      "Psc" 'password-store-edit
      "Psr" 'password-store-rename
      "Psd" 'password-store-remove
      "PsD" 'password-store-clear
      "PsI" 'password-store-init
      "Psw" 'password-store-url
      "Ps?" 'pass/password-store-describe
      "PsY" 'pass/password-store-copy-and-describe)
    :config
    (progn
      (defun password-store--completing-read ()
        "Read a password entry in the minibuffer, with completion.

This method is overridden to allow use of the dotspacemacs-use-ido variable."
        (if dotspacemacs-use-ido
            (ido-completing-read "Password entry: " (password-store-list))
          (helm-comp-read "Password entry: " (password-store-list))))

      (defun pass/password-store-copy-and-describe (entry)
        "Copy the password to the clipboard, and show the multiline description for ENTRY"
        (interactive (list (password-store--completing-read)))
        (password-store-copy entry)
        (pass/password-store-describe entry))

      (defun pass/password-store-describe (entry)
        "Show the multiline description for ENTRY"
        (interactive (list (password-store--completing-read)))
        (let ((description (s-join "\n" (cdr (s-lines (password-store--run-show entry))))))
          (message "%s" description)))
      )))
