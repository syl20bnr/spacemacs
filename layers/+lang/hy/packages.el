;;; packages.el --- Hy Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq hy-packages
      '(
        company
        evil-cleverparens
        hy-mode
        ob-hy
        pyenv-mode
        pyvenv
        smartparens
        ))

(defun hy/post-init-company ()
  ;; Autocompletion now fit for use, not all symbols complete, hy bug
  (spacemacs|add-company-backends
    :backends company-hy
    :modes hy-mode inferior-hy-mode))

(defun hy/pre-init-evil-cleverparens ()
  (spacemacs|use-package-add-hook evil-cleverparens
    :pre-init
    (add-to-list 'evil-lisp-safe-structural-editing-modes 'hy-mode)))

(defun hy/init-hy-mode ()
  (use-package hy-mode
    :defer t
    :mode ("\\.hy\\'" . hy-mode)
    :interpreter ("hy" . hy-mode)
    :config
    (progn
      ;; Disable this unless using special branch
      (setq hy-shell-use-control-codes? nil)
      ;; key bindings
      (spacemacs/declare-prefix-for-mode 'hy-mode "md" "debug")
      (spacemacs/declare-prefix-for-mode 'hy-mode "mt" "test")
      (spacemacs/declare-prefix-for-mode 'hy-mode "me" "eval")
      (spacemacs/declare-prefix-for-mode 'hy-mode "ms" "REPL")
      (spacemacs/declare-prefix-for-mode 'hy-mode "mv" "pyvenv")
      (spacemacs/declare-prefix-for-mode 'hy-mode "mh" "help")
      (spacemacs/set-leader-keys-for-major-mode 'hy-mode
        "'" 'hy-shell-start-or-switch-to-shell

        "dd" 'hy-insert-pdb
        "dt" 'hy-insert-pdb-threaded
        "hh" 'hy-describe-thing-at-point

        "eb" 'hy-shell-eval-buffer
        "eB" 'spacemacs/hy-shell-eval-buffer-and-go
        "ec" 'hy-shell-eval-current-form
        "eC" 'spacemacs/hy-shell-eval-current-form-and-go
        "ei" 'hy-shell-start-or-switch-to-shell
        "er" 'hy-shell-eval-region
        "eR" 'spacemacs/hy-shell-eval-region-and-go

        "sb" 'hy-shell-eval-buffer
        "sB" 'spacemacs/hy-shell-eval-buffer-and-go
        "sc" 'hy-shell-eval-current-form
        "sC" 'spacemacs/hy-shell-eval-current-form-and-go
        "si" 'hy-shell-start-or-switch-to-shell
        "sr" 'hy-shell-eval-region
        "sR" 'spacemacs/hy-shell-eval-region-and-go

        "tA" 'spacemacs/python-test-pdb-all
        "ta" 'spacemacs/python-test-all
        "tM" 'spacemacs/python-test-pdb-module
        "tm" 'spacemacs/python-test-module))))

(defun hy/pre-init-ob-hy ()
  (spacemacs|use-package-add-hook org
    :post-config
    (use-package ob-hy
      :init (add-to-list 'org-babel-load-languages '(hy . t)))))
(defun hy/init-ob-hy ())

(defun hy/pre-init-pyenv-mode ()
  (add-to-list 'spacemacs--python-pyenv-modes 'hy-mode))

(defun hy/pre-init-pyvenv ()
  (add-to-list 'spacemacs--python-pyvenv-modes 'hy-mode))

(defun hy/post-init-smartparens ()
  (add-hook 'hy-mode-hook 'smartparens-mode))
