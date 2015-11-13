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
    company
    purescript-mode
    psci
    psc-ide
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

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun purescript/post-init-company ()
    (spacemacs|add-company-hook purescript-mode))

  (defun purescript/init-psc-ide ()
    (use-package psc-ide
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :init
      (progn
        (push 'psc-ide company-backends-purescript-mode)
        (add-hook 'purescript-mode-hook 'psc-ide-mode)
        (spacemacs/set-leader-keys-for-major-mode 'purescript-mode
          "ss" 'psc-ide-server-start
          "ht" 'psc-ide-show-type
          "sl" 'psc-ide-load-module)))))
