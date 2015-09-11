;;; packages.el --- Windows Scripts Layer packages File for Spacemacs
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

(setq windows-scripts-packages
  '(
    powershell
    ))

(defun windows-scripts/init-powershell ()
  (use-package powershell
    :mode ("\\.ps1$" . powershell-mode)
    :defer t
    :init
    (progn
      (defun powershell/define-text-objects ()
        (spacemacs|define-text-object "$" "dollarparen" "$(" ")")
        )
      (add-hook 'powershell-mode-hook 'powershell/define-text-objects)
      (evil-leader/set-key
        "asp" 'powershell)
      (evil-leader/set-key-for-mode 'powershell-mode
        "mrr" 'powershell-regexp-to-regex)

    ;; TODO
    ;; - split out powershell
    ;; - get help output with mgg (Get-Help) or Get-Help -online
    ;; - 
    )))
