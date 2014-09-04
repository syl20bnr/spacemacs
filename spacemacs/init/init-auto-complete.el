(use-package auto-complete
  :commands auto-complete-mode
  :init
  (progn 
    (add-to-hooks 'auto-complete-mode '(org-mode-hook
                                        prog-mode-hook
                                        erlang-mode-hook))
    (evil-leader/set-key "ta" 'auto-complete-mode))
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
    ))
