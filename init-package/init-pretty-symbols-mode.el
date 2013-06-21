(setq pretty-symbol-categories '(lambda relational logical))

(dolist (mode '(emacs-lisp
                java
                python
                ruby))
(add-hook (intern (concat (symbol-name mode) "-mode-hook"))
		  'pretty-symbols-mode))
