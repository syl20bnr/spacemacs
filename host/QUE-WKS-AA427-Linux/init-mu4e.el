;; alternatively, for emacs-24 you can use:
(setq message-send-mail-function 'smtpmail-send-it
     smtpmail-stream-type 'starttls
     smtpmail-default-smtp-server "smtp.gmail.com"
     smtpmail-smtp-server "smtp-ncsa.ubisoft.org"
     smtpmail-smtp-service 25)

;; something about ourselves
(setq
   user-mail-address "sylvain.benner@ubisoft.com"
   user-full-name  "Sylvain Benner"
   message-signature (concat "-Sylvain Benner - Team Stratus\n"
                             "http://stratus"))
