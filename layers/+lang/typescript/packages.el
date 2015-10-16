;;; packages.el --- typescript Layer packages File for Spacemacs
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

(setq typetide-packages '(tide))

(defun typetide/init-tide ()
  "Initialize tide package"
  (use-package tide
    :defer t
    :init
    (progn
      (add-hook 'typescript-mode-hook
                (lambda ()
                  (evil-leader/set-key-for-mode 'typescript-mode
                    "mgg" 'tide-jump-to-definition
                    "mhh" 'tide-documentation-at-point
                    "mrr" 'tide-rename-symbol)
                  (tide-setup)
                  (flycheck-mode t)
                  (setq flycheck-check-syntax-automatically '(save mode-enabled))
                  (eldoc-mode t)
                  (company-mode-on))))
    :config (message "Tide mode loaded")))
