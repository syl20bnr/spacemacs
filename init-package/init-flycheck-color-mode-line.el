(require 'flycheck-color-mode-line)

(setq flycheck-check-syntax-automatically '(save mode-enabled))

(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
