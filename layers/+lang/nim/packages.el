;;; packages.el --- Nim Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Max Gonzih
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
    :init
    (progn
      (add-hook 'nim-mode-hook #'spacemacs//nim-setup-backend)
      (add-to-list 'spacemacs-jump-handlers-nim-mode 'nimsuggest-find-definition))
    :config
    (progn
      ;; Set non lsp bindings
      (when (eq nim-backend 'company-nim)
        (spacemacs/declare-prefix-for-mode 'nim-mode "mg" "goto")
        (spacemacs/declare-prefix-for-mode 'nim-mode "mh" "help")
        (spacemacs/set-leader-keys-for-major-mode 'nim-mode
          "hh" 'nimsuggest-show-doc))

      ;; Set general bindings
      (spacemacs/declare-prefix-for-mode 'nim-mode "mc" "compile")
      (spacemacs/set-leader-keys-for-major-mode 'nim-mode
        "cr" 'spacemacs/nim-compile-run
        "gb" 'pop-tag-mark))))
