;;; packages.el --- Purescript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ryan L. Bell
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(setq purescript-packages
  '(
    company
    purescript-mode
    psci
    psc-ide
    ))

(defun purescript/post-init-company ()
  (spacemacs|add-company-hook purescript-mode))

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
      (add-hook 'purescript-mode-hook 'inferior-psci-mode)
      (spacemacs/set-leader-keys-for-major-mode 'purescript-mode
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
      (push 'company-psc-ide-backend company-backends-purescript-mode)
      (spacemacs/set-leader-keys-for-major-mode 'purescript-mode
        "ms" 'psc-ide-server-start
        "ml" 'psc-ide-load-module
        "ht" 'psc-ide-show-type))))
