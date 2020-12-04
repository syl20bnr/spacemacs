;;; packages.el --- Nim Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Max Gonzih
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst nim-packages
  '(
    company
    flycheck
    flycheck-nim
    nim-mode))

(defun nim/post-init-company ()
  (space-macs//nim-setup-company))

(defun nim/post-init-flycheck ()
  (space-macs/enable-flycheck 'nim-mode)
  (space-macs/enable-flycheck 'nimscript-mode))

(defun nim/init-flycheck-nim ()
  (use-package flycheck-nim
    :if (configuration-layer/package-used-p 'flycheck)))

(defun nim/init-nim-mode ()
  (use-package nim-mode
    :defer t
    :init (add-hook 'nim-mode-hook #'space-macs//nim-setup-backend)
    :config
    (progn
      ;; Set non lsp bindings
      (when (eq (space-macs//nim-backend) 'company-nim)
        (space-macs/declare-prefix-for-mode 'nim-mode "mg" "goto")
        (space-macs/declare-prefix-for-mode 'nim-mode "mh" "help")
        (space-macs/set-leader-keys-for-major-mode 'nim-mode
          "hh" 'nimsuggest-show-doc))

      ;; Set general bindings
      (space-macs/declare-prefix-for-mode 'nim-mode "mc" "compile")
      (space-macs/set-leader-keys-for-major-mode 'nim-mode
        "cr" 'space-macs/nim-compile-run
        "gb" 'pop-tag-mark))))


