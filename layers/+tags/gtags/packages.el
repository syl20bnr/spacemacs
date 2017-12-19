;;; packages.el --- gtags Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;;    and: Christian E. Hopps <chopps@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst gtags-packages
  '(
    ggtags
    (counsel-gtags :requires ivy)
    (helm-gtags :requires helm)
    ))

(defun gtags/init-counsel-gtags ()
  (use-package counsel-gtags
    :defer t
    :init
    (progn
      (setq counsel-gtags-ignore-case t
            counse1-gtags-auto-update t)
      ;; modes that do not have a layer, define here
      (spacemacs/counsel-gtags-define-keys-for-mode 'tcl-mode)
      (spacemacs/counsel-gtags-define-keys-for-mode 'vhdl-mode)
      (spacemacs/counsel-gtags-define-keys-for-mode 'awk-mode)
      (spacemacs/counsel-gtags-define-keys-for-mode 'dired-mode)
      (spacemacs/counsel-gtags-define-keys-for-mode 'compilation-mode)
      (spacemacs/counsel-gtags-define-keys-for-mode 'shell-mode))))

(defun gtags/init-ggtags ()
  (use-package ggtags
    :defer t
    :init
    (progn
      ;; modes that do not have a layer, add here.
      (add-hook 'awk-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)
      (add-hook 'shell-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)
      (add-hook 'tcl-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)
      (add-hook 'vhdl-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)
      (spacemacs|add-toggle ggtags-mode
        :status ggtags-mode
        :on (ggtags-mode nil)
        :off (ggtags-mode -1)
        :documentation "Toggle GNU Global source code tagging system."
        :evil-leader "tG"))
    :config
    (progn
      (when (configuration-layer/package-used-p 'helm-gtags)
        ;; If anyone uses helm-gtags, they would want to use these key bindings.
        ;; These are bound in `ggtags-mode-map', since the functionality of
        ;; `helm-gtags-mode' is basically entirely contained within
        ;; `ggtags-mode-map' --- this way we don't have to enable both.
        ;; Note: all of these functions are autoloadable.
        (define-key ggtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
        (define-key ggtags-mode-map (kbd "C-x 4 .") 'helm-gtags-find-tag-other-window)
        (define-key ggtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
        (define-key ggtags-mode-map (kbd "M-*") 'helm-gtags-pop-stack))
      (spacemacs|diminish ggtags-mode " Ⓖ" " G"))))

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
      (spacemacs/helm-gtags-define-keys-for-mode 'shell-mode))))
