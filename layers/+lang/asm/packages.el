;;; packages.el --- Asm Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Tu, Do Hoang <tuhdo1710@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq asm-packages
      '(
        ;; package names go here
        asm-mode
        electric-indent-mode
        ggtags
        helm-gtags
        nasm-mode
        x86-lookup
        ))

(defun asm/init-asm-mode ()
  (use-package asm-mode
    :init
    (spacemacs/set-leader-keys-for-major-mode 'asm-mode "h" 'x86-lookup)
    :config
    (progn
      ;; We need to insert a non-indented line, otherwise it's annoying
      ;; everytime we insert a comment for a routine
      (define-key asm-mode-map (kbd "C-j") 'newline)
      (add-hook 'asm-mode-hook #'asm-generic-setup))))

(defun asm/post-init-electric-indent-mode ()
  (spacemacs/add-to-hooks 'asm-electric-indent-local-mode-off
                   '(asm-mode-hook nasm-mode-hook)))

(defun asm/init-nasm-mode ()
  "Setup for built-in `nasm-mode', which could be thought as improved `asm-mode'"
  (use-package nasm-mode
    :init
    (progn
      (add-hook 'nasm-mode-hook #'asm-generic-setup)
      (add-to-list 'auto-mode-alist '("\\.[n]*\\(asm\\|s\\)\\'" . nasm-mode))
      (spacemacs/set-leader-keys-for-major-mode 'nasm-mode "h" 'x86-lookup))
    :config
    (progn
      ;; We need to insert a non-indented line, otherwise it's annoying
      ;; everytime we insert a comment for a routine
      (define-key nasm-mode-map (kbd "C-j") 'newline)
      ;; we use the advised `asm-colon' because `nasm-colon indents the whole line, even
      ;; inside a comment
      (define-key nasm-mode-map (kbd ":") 'asm-colon))))

(defun asm/init-x86-lookup ()
  (use-package x86-lookup
    :init
    (progn
      ;; when a user installed `pdf-tools', use it for viewing PDF document.
      (when (package-installed-p 'pdf-tools)
        (setq x86-lookup-browse-pdf-function 'x86-lookup-browse-pdf-pdf-tools)))))

(defun asm/post-init-company ()
  (spacemacs|add-company-hook asm-mode)
  (spacemacs|add-company-hook nasm-mode))

(defun asm/post-init-ggtags ()
  (add-hook 'asm-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun asm/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'asm-mode))
