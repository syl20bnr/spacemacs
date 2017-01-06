;;; packages.el --- Purescript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Ryan L. Bell
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(setq purescript-packages
  '(company
    flycheck
    purescript-mode
    psci
    psc-ide
    popwin))

(defun purescript/post-init-company ()
  (spacemacs|add-company-hook purescript-mode))

(defun purescript/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'purescript-mode))

(defun purescript/init-purescript-mode ()
  (use-package purescript-mode
    :defer t
    :init
    (progn
      (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
      (spacemacs/set-leader-keys-for-major-mode 'purescript-mode
        "i="  'purescript-mode-format-imports
        "i`"  'purescript-navigate-imports-return
        "ia"  'purescript-align-imports
        "in"  'purescript-navigate-imports))))

(defun purescript/init-psci ()
  (use-package psci
    :defer t
    :init
    (progn
      (spacemacs/register-repl 'psci 'psci "purescript")
      (add-hook 'purescript-mode-hook 'inferior-psci-mode)
      (spacemacs/set-leader-keys-for-major-mode 'purescript-mode
        "'"  'psci
        "sb" 'psci/load-current-file!
        "si" 'psci
        "sm" 'psci/load-module!
        "sp" 'psci/load-project-modules!))))

(defun purescript/init-psc-ide ()
  (use-package psc-ide
    :defer t
    :init
    (progn
      (add-hook 'purescript-mode-hook 'psc-ide-mode)
      (spacemacs/declare-prefix-for-mode 'purescript-mode "mm" "purescript/psc-ide")
      (push 'company-psc-ide-backend company-backends-purescript-mode)

      (customize-set-variable 'psc-ide-add-import-on-completion purescript-add-import-on-completion)
      (customize-set-variable 'psc-ide-rebuild-on-save purescript-enable-rebuild-on-save)

      (add-to-list 'spacemacs-jump-handlers-purescript-mode 'psc-ide-goto-definition)
      (spacemacs/set-leader-keys-for-major-mode 'purescript-mode
        "mt"  'psc-ide-add-clause
        "mcs" 'psc-ide-case-split
        "ms"  'psc-ide-server-start
        "mb"  'psc-ide-rebuild
        "mq"  'psc-ide-server-quit
        "ml"  'psc-ide-load-all
        "mL"  'psc-ide-load-module
        "mia" 'psc-ide-add-import
        "mis" 'psc-ide-flycheck-insert-suggestion
        "ht"  'psc-ide-show-type))))

(defun purescript/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*psc-ide-rebuild*" :tail t :noselect t) popwin:special-display-config)))
