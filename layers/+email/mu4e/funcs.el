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

(defun mu4e//search-account-by-mail-address (mailto)
  "Return the account given an email address in MAILTO."
  (car (rassoc-if (lambda (x)
                    (equal (cadr (assoc 'user-mail-address x)) (car mailto)))
                  mu4e-account-alist)))

(defun mu4e/set-account ()
  "Set the account for composing a message.
This function tries to guess the correct account from the email address first
then fallback to the maildir."
  (let* ((account
          (if mu4e-compose-parent-message
              (let* ((mailtos
                      (mu4e-message-field mu4e-compose-parent-message :to))
                     (mailto-account
                      (car (cl-remove-if-not
                            'identity
                            (mapcar 'mu4e//search-account-by-mail-address
                                    mailtos))))
                     (maildir
                      (mu4e-message-field mu4e-compose-parent-message :maildir))
                     (maildir-account
                      (progn
                        (string-match "/\\(.*?\\)/" maildir)
                        (match-string 1 maildir))))
                (or mailto-account maildir-account))
            (funcall mu4e-completing-read-function
                     "Compose with account:"
                     (mapcar (lambda (var) (car var)) mu4e-account-alist))))
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
