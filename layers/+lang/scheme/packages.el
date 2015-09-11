;;; packages.el --- Scheme Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq scheme-packages
      '(geiser))

(defun scheme/init-geiser ()
  (use-package geiser
    :commands run-geiser
    :config
    (progn
      (evil-leader/set-key-for-mode 'scheme-mode
        "mcc" 'geiser-compile-current-buffer
        "mcp" 'geiser-add-to-load-path

        "mgg" 'geiser-edit-symbol-at-point
        "mgb" 'geiser-pop-symbol-stack
        "mgm" 'geiser-edit-module
        "mgn" 'next-error
        "mgN" 'previous-error

        "mhh" 'geiser-doc-symbol-at-point
        "mhd" 'geiser-doc-look-up-manual
        "mhm" 'geiser-doc-module
        "mh<" 'geiser-xref-callers
        "mh>" 'geiser-xref-callees

        "mil" 'geiser-insert-lambda

        "mme" 'geiser-expand-last-sexp
        "mmf" 'geiser-expand-definition
        "mmx" 'geiser-expand-region

        "msi" 'geiser-mode-switch-to-repl
        "msb" 'geiser-eval-buffer
        "msB" 'geiser-eval-buffer-and-go
        "msf" 'geiser-eval-definition
        "msF" 'geiser-eval-definition-and-go
        "mse" 'geiser-eval-last-sexp
        "msr" 'geiser-eval-region
        "msR" 'geiser-eval-region-and-go))))

(when (configuration-layer/layer-usedp 'auto-completion)

  (defun scheme/post-init-company ()
    ;; Geiser provides completion as long as company mode is loaded.
    (spacemacs|add-company-hook scheme-mode)))
