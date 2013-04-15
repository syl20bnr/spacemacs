(require 'flycheck)

(dolist (mode '(elixir
                json
                python))

(add-hook (intern (concat (symbol-name mode) "-mode-hook"))
		  'flycheck-mode))
