(require 'diminish)
(eval-after-load "auto-complete"
  '(diminish 'auto-complete-mode "ac"))

(eval-after-load "autopair"
  '(diminish 'autopair-mode))

(eval-after-load "eproject"
  '(diminish 'eproject-mode "prj"))

(eval-after-load "paredit"
  '(diminish 'paredit-mode "pedit"))

(eval-after-load "rainbow-mode"
  '(diminish 'rainbow-mode))

(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode))

(eval-after-load "yasnippet"
  '(diminish 'yas-minor-mode))

;; -----------------------------------------------------------------------------

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq mode-name "elisp")))
