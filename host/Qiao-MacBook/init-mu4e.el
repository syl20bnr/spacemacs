;; alternatively, for emacs-24 you can use:
(setq message-send-mail-function 'smtpmail-send-it
     smtpmail-stream-type 'starttls
     smtpmail-default-smtp-server "smtp.gmail.com"
     smtpmail-smtp-server "smtp.gmail.com"
     smtpmail-smtp-service 587)

;; something about ourselves
(setq
   user-mail-address "sylvain.benner@gmail.com"
   user-full-name  "Sylvain Benner"
   message-signature "syl20bnr")
