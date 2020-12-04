;;; packages.el --- Hy Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
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
        pipenv
        smartparens
        ))

(defun hy/post-init-company ()
  ;; Autocompletion now fit for use, not all symbols complete, hy bug
  (space-macs|add-company-backends
    :backends company-hy
    :modes hy-mode inferior-hy-mode))

(defun hy/pre-init-evil-cleverparens ()
  (space-macs|use-package-add-hook evil-cleverparens
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
      (space-macs/declare-prefix-for-mode 'hy-mode "md" "debug")
      (space-macs/declare-prefix-for-mode 'hy-mode "mt" "test")
      (space-macs/declare-prefix-for-mode 'hy-mode "me" "eval")
      (space-macs/declare-prefix-for-mode 'hy-mode "ms" "REPL")
      (space-macs/declare-prefix-for-mode 'hy-mode "mv" "pyvenv")
      (space-macs/declare-prefix-for-mode 'hy-mode "mh" "help")
      (space-macs/set-leader-keys-for-major-mode 'hy-mode
        "'" 'run-hy

        "dd" 'hy-insert-pdb
        "dt" 'hy-insert-pdb-threaded
        "hh" 'hy-describe-thing-at-point

        "eb" 'hy-shell-eval-buffer
        "eB" 'space-macs/hy-shell-eval-buffer-and-go
        "ec" 'hy-shell-eval-current-form
        "eC" 'space-macs/hy-shell-eval-current-form-and-go
        "ei" 'run-hy
        "er" 'hy-shell-eval-region
        "eR" 'space-macs/hy-shell-eval-region-and-go

        "sb" 'hy-shell-eval-buffer
        "sB" 'space-macs/hy-shell-eval-buffer-and-go
        "sc" 'hy-shell-eval-current-form
        "sC" 'space-macs/hy-shell-eval-current-form-and-go
        "si" 'hy-shell-start-or-switch-to-shell
        "sr" 'hy-shell-eval-region
        "sR" 'space-macs/hy-shell-eval-region-and-go

        "tA" 'space-macs/python-test-pdb-all
        "ta" 'space-macs/python-test-all
        "tM" 'space-macs/python-test-pdb-module
        "tm" 'space-macs/python-test-module))))

(defun hy/pre-init-ob-hy ()
  (space-macs|use-package-add-hook org
    :post-config
    (use-package ob-hy
      :init (add-to-list 'org-babel-load-languages '(hy . t)))))
(defun hy/init-ob-hy ())

(defun hy/pre-init-pyenv-mode ()
  (add-to-list 'space-macs--python-pyenv-modes 'hy-mode))

(defun hy/pre-init-pyvenv ()
  (add-to-list 'space-macs--python-pyvenv-modes 'hy-mode))

(defun hy/pre-init-pipenv ()
  (add-to-list 'space-macs--python-pipenv-modes 'hy-mode))

(defun hy/post-init-smartparens ()
  (add-hook 'hy-mode-hook 'smartparens-mode))


