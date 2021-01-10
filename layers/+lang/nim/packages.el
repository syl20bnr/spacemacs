;;; packages.el --- Nim Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Max Gonzih
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst nim-packages
  '(
    company
    flycheck
    flycheck-nim
    nim-mode))

(defun nim/post-init-company ()
  (spacemacs//nim-setup-company))

(defun nim/post-init-flycheck ()
  (spacemacs/enable-flycheck 'nim-mode)
  (spacemacs/enable-flycheck 'nimscript-mode))

(defun nim/init-flycheck-nim ()
  (use-package flycheck-nim
    :if (configuration-layer/package-used-p 'flycheck)))

(defun nim/init-nim-mode ()
  (use-package nim-mode
    :defer t
    :init (add-hook 'nim-mode-hook #'spacemacs//nim-setup-backend)
    :config
    (progn
      ;; Set non lsp bindings
      (when (eq (spacemacs//nim-backend) 'company-nim)
        (spacemacs/declare-prefix-for-mode 'nim-mode "mg" "goto")
        (spacemacs/declare-prefix-for-mode 'nim-mode "mh" "help")
        (spacemacs/set-leader-keys-for-major-mode 'nim-mode
          "hh" 'nimsuggest-show-doc))

      ;; Set general bindings
      (spacemacs/declare-prefix-for-mode 'nim-mode "mc" "compile")
      (spacemacs/set-leader-keys-for-major-mode 'nim-mode
        "cr" 'spacemacs/nim-compile-run
        "gb" 'pop-tag-mark))))
