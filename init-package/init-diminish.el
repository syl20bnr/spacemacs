(require 'diminish)
(eval-after-load "auto-complete"
  '(diminish 'auto-complete-mode "AC"))

(eval-after-load "autopair"
  '(diminish 'autopair-mode "AP"))

(eval-after-load "paredit"
  '(diminish 'paredit-mode "PE"))

(eval-after-load "rainbow-mode"
  '(diminish 'rainbow-mode))

(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode))

(eval-after-load "yasnippet"
  '(diminish 'yas-minor-mode))

(eval-after-load "emacs-eclim"
  '(diminish 'eclim-mode "ecl"))

(eval-after-load "centered-cursor-mode"
  '(diminish 'centered-cursor-mode))

(eval-after-load 'elisp-slime-nav
  '(diminish 'elisp-slime-nav-mode))

;; -----------------------------------------------------------------------------

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq mode-name "elisp")))
