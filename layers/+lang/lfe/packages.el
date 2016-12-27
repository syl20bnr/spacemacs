;;; packages.el --- LFE Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sergey Yanovitsky <sergey.yanovitsky@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq lfe-packages
  '(
    company
    lfe-mode
    ggtags
    helm-gtags
    flycheck
    ))

(defun lfe/post-init-company ()
  (add-hook 'lfe-mode-hook 'company-mode))

(defun lfe/init-lfe-mode ()
  (use-package lfe-mode
    :defer t
    :init
    (progn
      (add-hook 'lfe-mode-hook 'spacemacs/run-prog-mode-hooks))))

(defun lfe/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'lfe-mode))

(defun lfe/post-init-ggtags ()
  (add-hook 'lfe-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun lfe/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'lfe-mode))
