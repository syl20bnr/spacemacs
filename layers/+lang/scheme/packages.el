;;; packages.el --- Scheme Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq scheme-packages
      '(
        company
        evil-cleverparens
        geiser
        ggtags
        counsel-gtags
        helm-gtags
        parinfer
        ))

(defun scheme/post-init-company ()
  ;; Geiser provides completion as long as company mode is loaded.
  (spacemacs|add-company-backends :modes scheme-mode))

(defun scheme/pre-init-evil-cleverparens ()
  (spacemacs|use-package-add-hook evil-cleverparens
    :pre-init
    (add-to-list 'evil-lisp-safe-structural-editing-modes 'scheme-mode)))

(defun scheme/init-geiser ()
  (use-package geiser
    :commands run-geiser
    :init (spacemacs/register-repl 'geiser 'geiser-mode-switch-to-repl "geiser")
    :config
    (progn
      ;; prefixes
      (spacemacs/declare-prefix-for-mode 'scheme-mode "mc" "compiling")
      (spacemacs/declare-prefix-for-mode 'scheme-mode "mg" "navigation")
      (spacemacs/declare-prefix-for-mode 'scheme-mode "mh" "documentation")
      (spacemacs/declare-prefix-for-mode 'scheme-mode "mi" "insertion")
      (spacemacs/declare-prefix-for-mode 'scheme-mode "mm" "macroexpansion")
      (spacemacs/declare-prefix-for-mode 'scheme-mode "ms" "repl")
      ;; key bindings
      (spacemacs/set-leader-keys-for-major-mode 'scheme-mode
        "'"  'geiser-mode-switch-to-repl
        ","  'lisp-state-toggle-lisp-state

        "cc" 'geiser-compile-current-buffer
        "cp" 'geiser-add-to-load-path

        "eb" 'geiser-eval-buffer
        "ee" 'geiser-eval-last-sexp
        "ef" 'geiser-eval-definition
        "el" 'lisp-state-eval-sexp-end-of-line
        "er" 'geiser-eval-region

        "gb" 'geiser-pop-symbol-stack
        "gm" 'geiser-edit-module
        "gn" 'next-error
        "gN" 'previous-error

        "hh" 'geiser-doc-symbol-at-point
        "hd" 'geiser-doc-look-up-manual
        "hm" 'geiser-doc-module
        "h<" 'geiser-xref-callers
        "h>" 'geiser-xref-callees

        "il" 'geiser-insert-lambda

        "me" 'geiser-expand-last-sexp
        "mf" 'geiser-expand-definition
        "mx" 'geiser-expand-region

        "si" 'geiser-mode-switch-to-repl
        "sb" 'geiser-eval-buffer
        "sB" 'geiser-eval-buffer-and-go
        "sf" 'geiser-eval-definition
        "sF" 'geiser-eval-definition-and-go
        "se" 'geiser-eval-last-sexp
        "sr" 'geiser-eval-region
        "sR" 'geiser-eval-region-and-go
        "ss" 'geiser-set-scheme))))

(defun scheme/post-init-ggtags ()
  (add-hook 'scheme-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun scheme/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'scheme-mode))

(defun scheme/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'scheme-mode))

(defun scheme/post-init-parinfer ()
  (add-hook 'scheme-mode-hook 'parinfer-mode))
