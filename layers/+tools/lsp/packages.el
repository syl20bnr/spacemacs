;;; packages.el --- Language Server Protocol packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Fangrui Song <i@maskray.me>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst lsp-packages
  '(
    (company-lsp :requires company)
    ;; `flycheck-lsp' does not exist so we defined it as built-in to avoid
    ;; fetching it from ELPA repositories.
    ;; this logical package serves to hook all flycheck related configuration
    ;; for LSP.
    (flycheck-lsp :requires flycheck :location built-in)
    lsp-mode
    lsp-ui
    ))

(defun lsp/init-company-lsp ()
  (use-package company-lsp
    :defer t
    :init
    ;; Language servers have better idea filtering and sorting,
    ;; don't filter results on the client side.
    (setq company-transformers nil
          company-lsp-async t
          company-lsp-cache-candidates nil)))

(defun lsp/init-flycheck-lsp ()
  ;; Disable lsp-flycheck.el in favor of lsp-ui-flycheck.el
  (setq lsp-enable-flycheck nil))

(defun lsp/init-lsp-mode ()
  (use-package lsp-mode
    :defer t
    :config
    (progn
      (spacemacs|hide-lighter lsp-mode))))

(defun lsp/init-lsp-ui ()
  (use-package lsp-ui
    :defer t
    :init (add-hook 'lsp-mode-hook #'lsp-ui-mode)
    :config
    (progn
      (spacemacs//lsp-sync-peek-face)
      (add-hook 'spacemacs-post-theme-change-hook
                #'spacemacs//lsp-sync-peek-face))))
