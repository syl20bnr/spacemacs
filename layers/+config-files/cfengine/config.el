;; Figure out which cfengine mode (cfengine2 or cfengine3) to use for =.cf=
;; files based on file content.
;; (add-to-list 'auto-mode-alist '("\\.cf\\'" . cfengine-auto-mode))
(add-to-list 'auto-mode-alist '("\\.cf\\'" . cfengine3-mode))

;; Perhaps we should bind this to something some day.
;; (global-set-key [(control f4)] 'cfengine3-reformat-json-string)

;; Enable eldoc-mode for cfengine
;; This puts function prototypes into the minibuffer when hovering
;; over the function.
(add-hook 'cfengine3-mode-hook 'eldoc-mode)

;; Enable auto completion for cfengine
(spacemacs|defvar-company-backends cfengine3-mode)
