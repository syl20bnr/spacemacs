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
