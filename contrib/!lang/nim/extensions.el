(setq nim-pre-extensions
      '())

(setq nim-post-extensions
      ;; taken from https://github.com/ALSchwalm/flycheck-nim
      '(flycheck-nim))

(defun nim/init-flycheck-nim ()
  (add-hook 'nim-mode-hook 'flycheck-mode)
  (use-package flycheck-nim
    :if (configuration-layer/layer-usedp 'syntax-checking)))
