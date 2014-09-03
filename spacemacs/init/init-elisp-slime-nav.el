;; Elisp go-to-definition with M-. and back again with M-,
(use-package elisp-slime-nav
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t))))
