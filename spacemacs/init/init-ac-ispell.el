(use-package ac-ispell
  :defer t
  :init
  (progn
    (custom-set-variables
     '(ac-ispell-requires 4))
    (eval-after-load "auto-complete"
      '(progn
         (ac-ispell-setup)))
    (add-hook 'markdown-mode-hook 'ac-ispell-ac-setup)))
