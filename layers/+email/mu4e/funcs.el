;;; funcs.el --- mu4e Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defun mu4e/set-account ()
  "Set the account for composing a message.
This function tries to guess the correct account from the email address from the to and cc fields, or the maildir. If no unambiguous guess can be done, prompts the user."
  (let* ((emails (when mu4e-compose-parent-message
                   (mapcar (lambda (x) (downcase (cdr x)))
                           (append
                            (mu4e-message-field mu4e-compose-parent-message :to)
                            (mu4e-message-field mu4e-compose-parent-message :cc)
                            (mu4e-message-field mu4e-compose-parent-message :from)
                            ))))
         (maildir (when mu4e-compose-parent-message
                    (mu4e-message-field mu4e-compose-parent-message :maildir)))
         (account-candidates
          (cond
           (emails (delq nil (mapcar
                              (lambda (x) (when (member (downcase (cadr (assoc 'user-mail-address x))) emails)
                                            (car x)))
                              mu4e-account-alist)))
           (maildir (progn
             (string-match "/\\(.*?\\)/" maildir)
             (list (match-string 1 maildir))))
           (t (mapcar 'car mu4e-account-alist))))
         (account
          (if (equal (length account-candidates) 1)
              (car account-candidates)
            (completing-read "Compose with account:" account-candidates)))
         (account-vars (cdr (assoc account mu4e-account-alist))))
    (if account-vars
        (mu4e//map-set account-vars)
      (error "No email account found"))))


(defun mu4e//map-set (vars)
  "Setq an alist VARS of variables and values."
  (mapc (lambda (var) (set (car var) (cadr var)))
        vars))

(defun mu4e/mail-account-reset ()
  "Reset mail account info to first."
  (mu4e//map-set (cdar mu4e-account-alist)))

