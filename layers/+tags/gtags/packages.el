;;; packages.el --- gtags Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq gtags-packages
  '(
    eldoc
    helm-gtags
    ggtags
    ))

(defun gtags/init-ggtags ()
  (use-package ggtags
    :defer t
    :init
    (progn
      ;; modes that do not have a layer, add here.
      (add-hook 'awk-mode-hook (lambda () (ggtags-mode 1)))
      (add-hook 'shell-mode-hook (lambda () (ggtags-mode 1)))
      (add-hook 'tcl-mode-hook (lambda () (ggtags-mode 1)))
      (add-hook 'vhdl-mode-hook (lambda () (ggtags-mode 1)))

      (spacemacs/ggtags-enable-eldoc 'tcl-mode)
      (spacemacs/ggtags-enable-eldoc 'vhdl-mode))))

(when (configuration-layer/layer-usedp 'spacemacs-helm)
  (defun gtags/init-helm-gtags ()
    (use-package helm-gtags
      :defer t
      :init
      (progn
        (setq helm-gtags-ignore-case t
              helm-gtags-auto-update t
              helm-gtags-use-input-at-cursor t
              helm-gtags-pulse-at-cursor t)
        ;; modes that do not have a layer, define here
        (spacemacs/helm-gtags-define-keys-for-mode 'tcl-mode)
        (spacemacs/helm-gtags-define-keys-for-mode 'vhdl-mode)
        (spacemacs/helm-gtags-define-keys-for-mode 'awk-mode)
        (spacemacs/helm-gtags-define-keys-for-mode 'dired-mode)
        (spacemacs/helm-gtags-define-keys-for-mode 'compilation-mode)
        (spacemacs/helm-gtags-define-keys-for-mode 'shell-mode))
      :config
      (progn
        ;; if anyone uses helm-gtags, they would want to use these key bindings
        (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
        (define-key helm-gtags-mode-map (kbd "C-x 4 .") 'helm-gtags-find-tag-other-window)
        (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
        (define-key helm-gtags-mode-map (kbd "M-*") 'helm-gtags-pop-stack)))))
