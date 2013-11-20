(use-package auto-complete
  :defer t
  :config
  (progn
    (require 'auto-complete-config)
    (ac-config-default)
    (add-to-list 'completion-styles 'initials t)
    (add-to-list 'ac-sources 'ac-source-semantic)
    (semantic-mode t)
    ;; customization
    (setq ac-auto-start 2
          ac-delay 0.
          ac-quick-help-delay 1.
          ac-use-fuzzy t
          ac-fuzzy-enable t
          tab-always-indent 'complete ; use 'complete when auto-complete is disabled
          ac-dwim t)
    ;; list of modes where ac should be available
    (dolist (mode '(emacs-lisp-mode
                    java-mode
                    org-mode
                    python-mode))
      (add-to-list 'ac-modes mode))
    ))
