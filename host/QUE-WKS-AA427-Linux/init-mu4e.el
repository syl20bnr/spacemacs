(setq message-send-mail-function 'smtpmail-send-it
  starttls-use-gnutls t
  smtpmail-default-smtp-server "smtp-ncsa.ubisoft.org"
  smtpmail-smtp-server "smtp-ncsa.ubisoft.org"
  smtpmail-smtp-service 587)

;; something about ourselves
(setq
   user-mail-address "sylvain.benner@ubisoft.com"
   user-full-name  "Sylvain Benner"
   message-signature (concat "-Sylvain Benner - Team Stratus\n"
                             "http://stratus"))
