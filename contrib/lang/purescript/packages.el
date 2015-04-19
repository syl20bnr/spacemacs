;;; packages.el --- Purescript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2015 Ryan L. Bell & Contributors
;;
;; Author: Ryan L. Bell
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(setq purescript-packages
  '(
    purescript-mode
    psci
    ))

(defun purescript/init-purescript-mode ()
  (use-package purescript-mode
    :defer t
    :init
    (progn
      (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
      (evil-leader/set-key-for-mode 'purescript-mode
        "mi="  'purescript-mode-format-imports
        "mi`"  'purescript-navigate-imports-return
        "mia"  'purescript-align-imports
        "min"  'purescript-navigate-imports))))

(defun purescript/init-psci ()
  (use-package psci
    :defer t
    :init
    (progn
      (add-hook 'purescript-mode-hook 'inferior-psci-mode)
      (evil-leader/set-key-for-mode 'purescript-mode
        "msb" 'psci/load-current-file!
        "msi" 'psci
        "msm" 'psci/load-module!
        "msp" 'psci/load-project-modules!))))
