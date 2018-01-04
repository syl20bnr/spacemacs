;;; packages.el --- kotlin layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Shanavas M <shanavas@disroot.org>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst kotlin-packages
  '(
    flycheck
    (flycheck-kotlin :requires flycheck)
    ggtags
    helm-gtags
    kotlin-mode
    ))

(defun kotlin/post-init-flycheck ()
  (spacemacs/enable-flycheck 'kotlin-mode))

(defun kotlin/init-flycheck-kotlin ()
  (use-package flycheck-kotlin
    :defer t
    :init (add-hook 'flycheck-mode-hook #'flycheck-kotlin-setup)))

(defun kotlin/post-init-ggtags ()
  (add-hook 'kotlin-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun kotlin/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'kotlin-mode))

(defun kotlin/init-kotlin-mode ()
  (use-package kotlin-mode
    :defer t))
