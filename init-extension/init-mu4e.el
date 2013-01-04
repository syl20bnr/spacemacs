(require 'mu4e)

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")
(setq mu4e-maildir "~/Maildir")
(setq message-kill-buffer-on-exit t)

;; folders
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)
;; don't prompt for applying of marks, just apply
(setq mu4e-headers-leave-behavior 'apply)
;; no confirm on quit
(setq mu4e-confirm-quit nil)

;; date format
(setq mu4e-headers-date-format "%d/%b/%Y %H:%M")

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.
(setq mu4e-maildir-shortcuts
    '( ("/INBOX"               . ?i)
       ("/[Gmail].Sent Mail"   . ?s)
       ("/[Gmail].Trash"       . ?t)
       ("/[Gmail].All Mail"    . ?a)))

;; something about ourselves
(setq
   user-mail-address "sylvain.benner@gmail.com"
   user-full-name  "Sylvain Benner"
   message-signature "syl20bn")

;; alternatively, for emacs-24 you can use:
(setq message-send-mail-function 'smtpmail-send-it
     smtpmail-stream-type 'starttls
     smtpmail-default-smtp-server "smtp.gmail.com"
     smtpmail-smtp-server "smtp.gmail.com"
     smtpmail-smtp-service 587)

;; inline images
(setq mu4e-view-show-images t
      mu4e-view-image-max-width 800)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; prefere html version
(setq mu4e-view-prefer-html t)
;; html to text conversion program
(setq mu4e-html2text-command "html2text -utf8 -width 140")
;;(setq mu4e-html2text-command "html2markdown | grep -v '&nbsp_place_holder;'")

;; form magnars
;; Start mu4e in fullscreen, immediately ping for new mail
(defun mu4e-up-to-date-status ()
  (interactive)
  (window-configuration-to-register :mu4e-fullscreen)
  (mu4e)
  (mu4e-update-mail-show-window)
  (delete-other-windows))

;; form magnars
;; Restore previous window configuration
(defun mu4e-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :mu4e-fullscreen))
