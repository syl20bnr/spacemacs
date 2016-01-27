;;; funcs.el --- mu4e Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun mu4e/set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir
                     (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
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

(defun mu4e/load-signature-from-file (file)
  "Load signature from FILE and strip separator if needed."
  (setq mu4e-compose-signature
        (with-temp-buffer
          (insert-file-contents file)
          (flush-lines message-signature-separator)
          (buffer-string))))

(defun mu4e/message-is-for-p (msg rx)
  "Check if to, cc or bcc field in MSG has any address in RX."
  (when (and msg rx)
    (or (mu4e-message-contact-field-matches msg :to rx)
        (mu4e-message-contact-field-matches msg :cc rx)
        (mu4e-message-contact-field-matches msg :bcc rx))))

(defun mu4e/set-send-function (function)
  "Set FUNCTION for sending mails."
  (setq send-mail-function         function
        message-send-mail-function function))
