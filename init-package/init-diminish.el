(require 'diminish)

;; Major modes abbrev --------------------------------------------------------

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq mode-name "Elisp |")))

(add-hook 'erlang-mode-hook
          (lambda ()
            (setq mode-name "Erl |")))

(add-hook 'python-mode-hook
          (lambda ()
            (setq mode-name "Py |")))

;; Minor modes abbrev --------------------------------------------------------

(eval-after-load "auto-complete"
  '(diminish 'auto-complete-mode "AC"))

(eval-after-load "autopair"
  '(diminish 'autopair-mode "AP"))

(eval-after-load "centered-cursor-mode"
  '(diminish 'centered-cursor-mode "CC"))

(eval-after-load "emacs-eclim"
  '(diminish 'eclim-mode "EE"))

(eval-after-load "eproject"
  '(diminish 'eproject-mode "EP"))

(eval-after-load "flymake-patch"
  '(diminish 'flymake-mode "FLY"))

(eval-after-load "paredit"
  '(diminish 'paredit-mode "PE"))

(eval-after-load "yasnippet"
  '(diminish 'yas-minor-mode "YAS"))

(eval-after-load "projectile"
  '(diminish 'projectile-mode "PT"))

;; Minor Mode (hidden) ------------------------------------------------------

(eval-after-load 'elisp-slime-nav
  '(diminish 'elisp-slime-nav-mode))

(eval-after-load "hi-lock"
  '(diminish 'hi-lock-mode))

(eval-after-load "rainbow-mode"
  '(diminish 'rainbow-mode))

(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode))
