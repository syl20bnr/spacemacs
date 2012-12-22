(eval-after-load 'paredit
;; need a binding that works in the terminal
'(progn
   (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
   (define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp)))

(dolist (mode '(scheme emacs-lisp lisp clojure clojurescript))
(when (> (display-color-cells) 8)
  (font-lock-add-keywords (intern (concat (symbol-name mode) "-mode"))
						  '(("(\\|)" . 'esk-paren-face))))
(add-hook (intern (concat (symbol-name mode) "-mode-hook"))
		  'paredit-mode))
