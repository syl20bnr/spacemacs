(dolist (mode '(c++
                json
                racket
                elisp
                LaTeX
                yaml))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
            'flycheck-mode))
