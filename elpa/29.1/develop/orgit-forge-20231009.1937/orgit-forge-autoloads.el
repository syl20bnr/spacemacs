;;; orgit-forge-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from orgit-forge.el

(with-eval-after-load "org" (org-link-set-parameters "orgit-topic" :store #'orgit-topic-store :follow #'orgit-topic-open :export #'orgit-topic-export :complete #'orgit-topic-complete-link))
(autoload 'orgit-topic-store "orgit-forge" "\
Store a link to a Forge-Topic mode buffer.

When the region selects a topic, then store a link to the
Forge-Topic mode buffer for that topic.")
(autoload 'orgit-topic-open "orgit-forge" "\


(fn ID)")
(autoload 'orgit-topic-export "orgit-forge" "\


(fn ID DESC FORMAT)")
(autoload 'orgit-topic-complete-link "orgit-forge" "\


(fn &optional ARG)")
(register-definition-prefixes "orgit-forge" '("orgit-topic-"))

;;; End of scraped data

(provide 'orgit-forge-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; orgit-forge-autoloads.el ends here
