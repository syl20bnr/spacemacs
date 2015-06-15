(add-hook 'before-save-hook 'delete-trailing-whitespace)

(delete-selection-mode t)

(setq initial-major-mode 'emacs-lisp-mode)

(global-prettify-symbols-mode)
