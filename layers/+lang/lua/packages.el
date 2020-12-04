;;; packages.el --- Lua Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq lua-packages
      '(
        company
        (company-lua :requires company)
        flycheck
        ggtags
        counsel-gtags
        helm-gtags
        lua-mode
        ))

(defun lua/post-init-flycheck ()
  (space-macs/enable-flycheck 'lua-mode))

(defun lua/init-lua-mode ()
  (use-package lua-mode
    :defer t
    :mode ("\\.lua\\'" . lua-mode)
    :interpreter ("lua" . lua-mode)
    :init (progn
            (space-macs/register-repl 'lua #'lua-show-process-buffer "lua")
            (add-hook 'lua-mode-local-vars-hook #'space-macs//lua-setup-backend))))

(defun lua/post-init-company ()
  (add-hook 'lua-mode-local-vars-hook #'space-macs//lua-setup-company))

(defun lua/init-company-lua ()
  (use-package company-lua
    :defer t))

(defun lua/post-init-ggtags ()
  (add-hook 'lua-mode-local-vars-hook #'space-macs/ggtags-mode-enable))

(defun lua/post-init-counsel-gtags ()
  (space-macs/counsel-gtags-define-keys-for-mode 'lua-mode))

(defun lua/post-init-helm-gtags ()
  (space-macs/helm-gtags-define-keys-for-mode 'lua-mode))


