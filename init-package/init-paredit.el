(use-package paredit
  :defer t
  :config
  (progn
    (dolist (mode '(scheme emacs-lisp lisp clojure clojurescript))
      (when (> (display-color-cells) 8)
        (font-lock-add-keywords (intern (concat (symbol-name mode) "-mode"))
                                '(("(\\|)" . 'esk-paren-face))))
      (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
                'paredit-mode))))
