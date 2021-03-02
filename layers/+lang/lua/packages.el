;;; packages.el --- Lua Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst lua-packages
  '(
    company
    (company-lua :requires company)
    flycheck
    ggtags
    counsel-gtags
    helm-gtags
    lua-mode))

(defun lua/post-init-flycheck ()
  (spacemacs/enable-flycheck 'lua-mode))

(defun lua/init-lua-mode ()
  (use-package lua-mode
    :defer t
    :mode ("\\.lua\\'" . lua-mode)
    :interpreter ("lua" . lua-mode)
    :init
    (progn
      (spacemacs/register-repl 'lua #'lua-show-process-buffer "lua")
      (add-hook 'lua-mode-local-vars-hook #'spacemacs//lua-setup-backend)

      ;; Set global settings
      (setq lua-indent-level 2
            lua-indent-string-contents t)

      ;; Set general bindings
      (spacemacs/declare-prefix-for-mode 'lua-mode "ms" "REPL")
      (spacemacs/set-leader-keys-for-major-mode 'lua-mode
        "hd" 'lua-search-documentation
        "sb" 'lua-send-buffer
        "sf" 'lua-send-defun
        "sl" 'lua-send-current-line
        "sr" 'lua-send-region
        "'" 'lua-show-process-buffer)

      ;; Set lua-mode specific bindings
      (when (eq (spacemacs//lua-backend) `lua-mode)
        (spacemacs/declare-prefix-for-mode 'lua-mode "mh" "help")
        (spacemacs/declare-prefix-for-mode 'lua-mode "mg" "goto")))))

(defun lua/post-init-company ()
  (add-hook 'lua-mode-local-vars-hook #'spacemacs//lua-setup-company))

(defun lua/init-company-lua ()
  (use-package company-lua
    :if (eq (spacemacs//lua-backend) 'lua-mode)
    :defer t))

(defun lua/post-init-ggtags ()
  (add-hook 'lua-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun lua/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'lua-mode))

(defun lua/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'lua-mode))
