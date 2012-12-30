;; Elisp go-to-definition with M-. and back again with M-,
(require 'elisp-slime-nav)
(require 'evil)
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
