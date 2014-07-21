(use-package flycheck
  :defer t
  :init
  (progn
    (dolist (mode '(c
                    coffee
                    elixir
                    js
                    json
                    python
                    ruby
                    scss
                    web))
      (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
                'flycheck-mode))
    (use-package flycheck-color-mode-line
      :defer t
      :init
      (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (setq flycheck-standard-error-navigation nil)
    ;; Custom fringe indicator
    (when (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'my-flycheck-fringe-indicator
        (vector #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00011100
                #b00111110
                #b00111110
                #b00111110
                #b00011100
                #b00000000
                #b00000000
                #b01111111)))
    (flycheck-define-error-level 'error
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-error)
    (flycheck-define-error-level 'warning
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-warning)
    (flycheck-define-error-level 'info
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-info)))
