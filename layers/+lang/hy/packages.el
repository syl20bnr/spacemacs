;;; packages.el --- Hy Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(setq hy-packages
      '(
        company
        evil-cleverparens
        hy-mode
        ob-hy
        pyenv-mode
        pyvenv
        pipenv
        smartparens))


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
      "'" 'run-hy

      "dd" 'hy-insert-pdb
      "dt" 'hy-insert-pdb-threaded
      "hh" 'hy-describe-thing-at-point

      "eb" 'hy-shell-eval-buffer
      "eB" 'spacemacs/hy-shell-eval-buffer-and-go
      "ec" 'hy-shell-eval-current-form
      "eC" 'spacemacs/hy-shell-eval-current-form-and-go
      "ei" 'run-hy
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
      "tm" 'spacemacs/python-test-module)))

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

(defun hy/pre-init-pipenv ()
  (add-to-list 'spacemacs--python-pipenv-modes 'hy-mode))

(defun hy/post-init-smartparens ()
  (add-hook 'hy-mode-hook #'spacemacs//activate-smartparens))
