;;; packages.el --- typescript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: JAremko <w3techplaygound@gmail.com> 
;; URL: https://github.com/JAremko/spacemacs-pr
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq typescript-packages '(tide))

(defun typescript/init-tide ()
  (use-package tide
    :defer t
    :init (progn
            (evilified-state-evilify tide-references-mode tide-references-mode-map
              (kbd "C-j") 'tide-find-previous-reference
              (kbd "C-k") 'tide-find-next-reference
              (kbd "C-l") 'tide-goto-reference)
            (add-hook 'typescript-mode-hook
              (lambda ()
                (tide-setup)
                (flycheck-mode t)
                (setq flycheck-check-syntax-automatically '(save mode-enabled))
                (eldoc-mode t)
                (when (configuration-layer/package-usedp 'company) 
                  (company-mode-on)))))
    :config (progn
              (spacemacs/declare-prefix-for-mode 'typescript-mode "mg" "goto")
              (spacemacs/declare-prefix-for-mode 'typescript-mode "mh" "help")
              (spacemacs/declare-prefix-for-mode 'typescript-mode "mn" "name")
              (defun typescript-jump-to-type-definition ()
                (interactive) (tide-jump-to-definition t))
              (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
                "gb" 'tide-jump-back
                "gi" 'typescript-jump-to-type-definition
                "gg" 'tide-jump-to-definition
                "hd" 'tide-documentation-at-point
                "nr" 'tide-rename-symbol
                "r"  'tide-references
                "s"  'tide-restart-server))))
