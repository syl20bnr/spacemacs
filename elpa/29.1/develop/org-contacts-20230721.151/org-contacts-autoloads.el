;;; org-contacts-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from org-contacts.el

(autoload 'org-contacts-org-complete-function "org-contacts" "\
completion-at-point function to complete @name in `org-mode'.
Usage: (add-hook \\='completion-at-point-functions
                 #\\='org-contacts-org-complete-function nil \\='local)")
(autoload 'org-contacts "org-contacts" "\
Create agenda view for contacts matching NAME.

(fn NAME)" t)
(autoload 'org-contacts-setup-completion-at-point "org-contacts" "\
Add `org-contacts-message-complete-function' as a new function
to complete the thing at point.")
(if (fboundp 'org-link-set-parameters) (org-link-set-parameters "org-contact" :follow #'org-contacts-link-open :complete #'org-contacts-link-complete :store #'org-contacts-link-store :face 'org-contacts-link-face) (if (fboundp 'org-add-link-type) (org-add-link-type "org-contact" 'org-contacts-link-open)))
(autoload 'org-contacts-link-store "org-contacts" "\
Store the contact in `org-contacts-files' with a link.")
(autoload 'org-contacts-link-open "org-contacts" "\
Open contacts: link type with jumping or searching.

(fn QUERY)")
(autoload 'org-contacts-link-complete "org-contacts" "\
Create a org-contacts link using completion.

(fn &optional ARG)")
(register-definition-prefixes "org-contacts" '("org-contacts-"))

;;; End of scraped data

(provide 'org-contacts-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; org-contacts-autoloads.el ends here
