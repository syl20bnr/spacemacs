;;; packages.el --- Nim Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Max Gonzih
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq nim-packages
      '(
        company
        flycheck
        flycheck-nim
        nim-mode
        ))

(defun nim/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-nimsuggest
    :modes nim-mode nimscript-mode))

(defun nim/post-init-flycheck ()
  (spacemacs/enable-flycheck 'nim-mode))

(defun nim/init-flycheck-nim ()
  (use-package flycheck-nim
    :if (configuration-layer/package-used-p 'flycheck)))

(defun nim/init-nim-mode ()
  (use-package nim-mode
    :defer t
    :init
    (progn
      (add-hook 'nim-mode-hook 'nimsuggest-mode)
      (add-to-list 'spacemacs-jump-handlers-nim-mode 'nimsuggest-find-definition))
    :config
    (progn
      (defun spacemacs/nim-compile-run ()
        (interactive)
        (shell-command "nim compile --run main.nim"))

      (spacemacs/set-leader-keys-for-major-mode 'nim-mode
        "cr" 'spacemacs/nim-compile-run
        "gb" 'pop-tag-mark
        "hh" 'nimsuggest-show-doc))))
