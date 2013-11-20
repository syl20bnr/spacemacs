(use-package auto-highlight-symbol-mode
  :init
  (global-auto-highlight-symbol-mode t)
  :config
  (custom-set-variables
   '(ahs-case-fold-search nil)
   '(ahs-default-range (quote ahs-range-whole-buffer))
   '(ahs-idle-interval 0.5)))


