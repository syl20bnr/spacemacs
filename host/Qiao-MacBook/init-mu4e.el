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

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.
(setq mu4e-maildir-shortcuts '(
       ("/Coding"       . ?c)
       ("/INBOX"        . ?i)
       ("/Mailing_List" . ?l)
       ("/News"         . ?n)
       ("/spam"         . ?m)
       ("/Personal"     . ?p)
       ("/sent"         . ?s)
       ("/trash"        . ?t)
       ("/Work"         . ?w)
))
