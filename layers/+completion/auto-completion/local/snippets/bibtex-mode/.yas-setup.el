(require 'yasnippet)

(add-hook 'bibtex-mode-hook
          '(lambda () (set (make-local-variable 'yas-indent-line) nil)))