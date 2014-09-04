(use-package auto-highlight-symbol
  :commands auto-highlight-symbol-mode
  :init
  (add-to-hooks
   'auto-highlight-symbol-mode '(erlang-mode-hook
                                 prog-mode-hook
                                 org-mode-hook
                                 markdown-mode-hook
                                 ))
  :config
  (progn 
    (custom-set-variables
     '(ahs-case-fold-search nil)
     '(ahs-default-range (quote ahs-range-whole-buffer))
     '(ahs-idle-interval 0.5))
    (evil-leader/set-key
      "he" 'ahs-edit-mode
      "hn" 'ahs-forward
      "hp" 'ahs-backward
      "th" 'auto-highlight-symbol-mode)))


