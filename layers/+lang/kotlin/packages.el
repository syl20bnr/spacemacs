;;; packages.el --- kotlin layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Shanavas M <shanavas@disroot.org>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst kotlin-packages
  '(
    company
    flycheck
    (flycheck-kotlin :requires flycheck)
    ggtags
    counsel-gtags
    helm-gtags
    kotlin-mode))

(defun kotlin/post-init-company ()
  (space-macs//kotlin-setup-company))

(defun kotlin/post-init-flycheck ()
  (space-macs/enable-flycheck 'kotlin-mode))

(defun kotlin/init-flycheck-kotlin ()
  (use-package flycheck-kotlin
    :defer t
    :init (add-hook 'flycheck-mode-hook #'flycheck-kotlin-setup)))

(defun kotlin/init-kotlin-mode ()
  (use-package kotlin-mode
    :defer t
    :init
    (progn
      (setq lsp-clients-kotlin-server-executable kotlin-lsp-jar-path)
      (add-hook 'kotlin-mode-hook #'space-macs//kotlin-setup-backend))))

(defun kotlin/post-init-ggtags ()
  (add-hook 'kotlin-mode-local-vars-hook #'space-macs/ggtags-mode-enable))

(defun kotlin/post-init-helm-gtags ()
  (space-macs/helm-gtags-define-keys-for-mode 'kotlin-mode))

(defun kotlin/post-init-counsel-gtags ()
  (space-macs/counsel-gtags-define-keys-for-mode 'kotlin-mode))


