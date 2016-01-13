;;; packages.el --- lispy layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Justin Burkett <justin@burkett.cc>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst lispy-packages
  '(lispy))

(defun lispy/init-lispy ()
  (use-package lispy
    :defer t
    :init
    (progn
      (defun spacemacs/lispy-off ()
        (when lispy-mode
          (lispy-mode -1)))
      (defvar-local spacemacs-lispy-mode nil)
      (defun spacemacs/toggle-lispy ()
        "Toggle `lispy-mode'. If `holy-mode' is enabled, this is
just a simple toggle. For vim and hybrid style (depending on the
value of `hybrid-mode') this adds or removes hooks that turn on
`lispy-mode' only in hybrid/insert state and only in the current
buffer."
        (interactive)
        (pcase (list (bound-and-true-p holy-mode)
                     (bound-and-true-p hybrid-mode)
                     spacemacs-lispy-mode)
          (`(t ,_ ,_)
           (call-interactively 'lispy-mode))
          (`(nil t t)
           (remove-hook 'evil-hybrid-state-entry-hook 'lispy-mode t)
           (remove-hook 'evil-hybrid-state-exit-hook 'spacemacs/lispy-off t)
           (when (eq evil-state 'hybrid) (spacemacs/lispy-off))
           (setq spacemacs-lispy-mode nil)
           (message "Lispy-mode disabled in hybrid state for this buffer."))
          (`(nil t nil)
           (add-hook 'evil-hybrid-state-entry-hook 'lispy-mode nil t)
           (add-hook 'evil-hybrid-state-exit-hook 'spacemacs/lispy-off nil t)
           (when (eq evil-state 'hybrid) (lispy-mode))
           (setq spacemacs-lispy-mode t)
           (message "Lispy-mode enabled in hybrid state for this buffer."))
          (`(nil nil t)
           (remove-hook 'evil-insert-state-entry-hook 'lispy-mode t)
           (remove-hook 'evil-insert-state-exit-hook 'spacemacs/lispy-off t)
           (when (eq evil-state 'insert) (spacemacs/lispy-off))
           (setq spacemacs-lispy-mode nil)
           (message "Lispy-mode disabled in insert state for this buffer."))
          (`(nil nil nil)
           (add-hook 'evil-insert-state-entry-hook 'lispy-mode nil t)
           (add-hook 'evil-insert-state-exit-hook 'spacemacs/lispy-off nil t)
           (when (eq evil-state 'insert) (lispy-mode))
           (setq spacemacs-lispy-mode t)
           (message "Lispy-mode enabled in insert state for this buffer."))))
      (spacemacs/set-leader-keys "k" 'spacemacs/toggle-lispy))
    :config (define-key lispy-mode-map (kbd "M-n") 'lispy-mark-symbol)))

;;; packages.el ends here
