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
  (custom-set-variables
   '(ahs-case-fold-search nil)
   '(ahs-default-range (quote ahs-range-whole-buffer))
   '(ahs-idle-interval 0.5)))


