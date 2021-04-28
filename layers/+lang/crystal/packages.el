;;; packages.el --- Crystal Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Brantou <brantou89@gmail.com>
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


(defconst crystal-packages
  '((ameba :location (recipe :fetcher github
                             :repo "veelenga/ameba.el"
                             :files ("ameba.el")))
    company
    crystal-mode
    flycheck
    (flycheck-crystal :requires flycheck)
    inf-crystal
    ob-crystal
    play-crystal))

(defun crystal/post-init-company ()
  (spacemacs//crystal-setup-company))

(defun crystal/init-ameba ()
  (use-package ameba
    :defer t
    :init
    (progn
      (add-hook 'crystal-mode-hook 'ameba-mode)
      (spacemacs/declare-prefix-for-mode 'crystal-mode "mua" "ameba")
      (spacemacs/set-leader-keys-for-major-mode 'crystal-mode
        "uad" 'ameba-check-directory
        "uaf" 'ameba-check-current-file
        "uap" 'ameba-check-project))))

(defun crystal/init-crystal-mode ()
  (use-package crystal-mode
    :defer t
    :init
    (progn
      (add-hook 'crystal-mode-hook 'spacemacs//crystal-auto-format-setup)
      (add-hook 'crystal-mode-hook #'spacemacs//crystal-setup-backend)
      (spacemacs/declare-prefix-for-mode 'crystal-mode "mu" "tool")
      (unless (eq crystal-backend 'lsp)
        (spacemacs/declare-prefix-for-mode 'crystal-mode "mg" "goto")
        (spacemacs/declare-prefix-for-mode 'crystal-mode "mt" "test")
        (spacemacs/declare-prefix-for-mode 'crystal-mode "ma" "action"))
      (spacemacs/set-leader-keys-for-major-mode 'crystal-mode
        "ga" 'crystal-spec-switch
        "tb" 'crystal-spec-buffer
        "tp" 'crystal-spec-all
        "uc" 'crystal-tool-context
        "ue" 'crystal-tool-expand
        "uf" 'crystal-tool-format
        "ui" 'crystal-tool-imp
        "ax" 'spacemacs/crystal-run-main))))

(defun crystal/post-init-flycheck ()
  (spacemacs/enable-flycheck 'crystal-mode))

(defun crystal/init-flycheck-crystal ()
  (use-package flycheck-crystal))

(defun crystal/init-inf-crystal ()
  (use-package inf-crystal
    :defer t
    :init
    (progn
      (spacemacs/register-repl 'inf-crystal 'inf-crystal "inf-crystal")
      (add-hook 'crystal-mode-hook 'inf-crystal-minor-mode)
      (spacemacs/declare-prefix-for-mode 'crystal-mode "ms" "repl")
      (spacemacs/set-leader-keys-for-major-mode 'crystal-mode
        "'" 'inf-crystal
        "sb" 'crystal-send-buffer
        "sB" 'crystal-send-buffer-and-go
        "sf" 'crystal-send-definition
        "sF" 'crystal-send-definition-and-go
        "si" 'inf-crystal
        "sr" 'crystal-send-region
        "sR" 'crystal-send-region-and-go
        "ss" 'crystal-switch-to-inf))))

(defun crystal/pre-init-ob-crystal ()
  (spacemacs|use-package-add-hook org
    :post-config
    (use-package ob-crystal
      :init (add-to-list 'org-babel-load-languages '(crystal . t)))))
(defun crystal/init-ob-crystal ())

(defun crystal/init-play-crystal ()
  (use-package play-crystal
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'crystal-mode "me" "play")
      (spacemacs/set-leader-keys-for-major-mode 'crystal-mode
        "eb" 'play-crystal-submit-buffer
        "ee" 'play-crystal-browse
        "ei" 'play-crystal-insert
        "er" 'play-crystal-submit-region))))
