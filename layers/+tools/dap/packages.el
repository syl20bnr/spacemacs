;;; packages.el --- DAP mode functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Ivan Yonchovski (yyoncho@gmail.com)
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst dap-packages
  '(dap-mode))

(defun dap/init-dap-mode ()
  (use-package dap-mode
    :after (lsp-mode)
    :hook ((dap-ui-sessions-mode . evil-evilified-state)
           (dap-ui-breakpoints-ui-list-mode . evil-evilified-state)
           (dap-ui-locals-mode . evil-evilified-state)
           (dap-ui-inspect-mode . evil-evilified-state))
    :config
    (progn
      (dap-mode 1)
      (dap-ui-mode 1)

      (spacemacs|add-toggle dap-mouse
        :status dap-tooltip-mode
        :on (progn (dap-tooltip-mode)
                   (tooltip-mode))
        :off (progn (dap-tooltip-mode -1)
                    (tooltip-mode -1))
        :documentation "Enable mouse support in DAP mode.")

      (when dap-enable-mouse-support
        (spacemacs/toggle-dap-mouse-on)))))
