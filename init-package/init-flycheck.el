(require 'flycheck)

(dolist (mode '(elixir
                json
                python
                ruby))

(add-hook (intern (concat (symbol-name mode) "-mode-hook"))
		  'flycheck-mode))

(setq flycheck-check-syntax-automatically (delq 'new-line flycheck-check-syntax-automatically))
