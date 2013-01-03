(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

(add-to-list 'completion-styles 'initials t)
(add-to-list 'ac-sources 'ac-source-semantic)
(setq-default ac-sources (cons 'ac-source-yasnippet ac-sources))

;; customization
(setq ac-auto-start 2
      ac-delay 0.
      ac-quick-help-delay 1.
      ac-use-fuzzy t
      ac-fuzzy-enable t
      tab-always-indent 'complete ; use 'complete when auto-complete is disabled
      ac-dwim t)

(global-set-key (kbd "M-SPC") 'ac-fuzzy-complete)
(define-key ac-completing-map (kbd "RET") 'ac-complete)

;;; list of modes where ac should be available
(dolist (mode '(emacs-lisp-mode
                org-mode
                python-mode))
  (add-to-list 'ac-modes mode))
