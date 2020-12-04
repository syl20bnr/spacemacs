;;; packages.el --- Crystal Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Brantou <brantou89@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

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
  (space-macs//crystal-setup-company))

(defun crystal/init-ameba()
  (use-package ameba
    :defer t
    :init
    (progn
      (add-hook 'crystal-mode-hook 'ameba-mode)
      (space-macs/declare-prefix-for-mode 'crystal-mode "mua" "ameba")
      (space-macs/set-leader-keys-for-major-mode 'crystal-mode
        "uad" 'ameba-check-directory
        "uaf" 'ameba-check-current-file
        "uap" 'ameba-check-project))))

(defun crystal/init-crystal-mode()
  (use-package crystal-mode
    :defer t
    :init
    (progn
      (add-hook 'crystal-mode-hook 'space-macs//crystal-auto-format-setup)
      (add-hook 'crystal-mode-hook #'space-macs//crystal-setup-backend)
      (space-macs/declare-prefix-for-mode 'crystal-mode "mu" "tool")
      (unless (eq (space-macs//crystal-backend) 'lsp)
        (space-macs/declare-prefix-for-mode 'crystal-mode "mg" "goto")
        (space-macs/declare-prefix-for-mode 'crystal-mode "mt" "test")
        (space-macs/declare-prefix-for-mode 'crystal-mode "ma" "action"))
      (space-macs/set-leader-keys-for-major-mode 'crystal-mode
        "ga" 'crystal-spec-switch
        "tb" 'crystal-spec-buffer
        "tp" 'crystal-spec-all
        "uc" 'crystal-tool-context
        "ue" 'crystal-tool-expand
        "uf" 'crystal-tool-format
        "ui" 'crystal-tool-imp
        "ax" 'space-macs/crystal-run-main))))

(defun crystal/post-init-flycheck()
  (space-macs/enable-flycheck 'crystal-mode))

(defun crystal/init-flycheck-crystal ()
  (use-package flycheck-crystal))

(defun crystal/init-inf-crystal()
  (use-package inf-crystal
    :defer t
    :init
    (progn
      (space-macs/register-repl 'inf-crystal 'inf-crystal "inf-crystal")
      (add-hook 'crystal-mode-hook 'inf-crystal-minor-mode)
      (space-macs/declare-prefix-for-mode 'crystal-mode "ms" "repl")
      (space-macs/set-leader-keys-for-major-mode 'crystal-mode
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
  (space-macs|use-package-add-hook org
    :post-config
    (use-package ob-crystal
      :init (add-to-list 'org-babel-load-languages '(crystal . t)))))
(defun crystal/init-ob-crystal ())

(defun crystal/init-play-crystal()
  (use-package play-crystal
    :defer t
    :init
    (progn
      (space-macs/declare-prefix-for-mode 'crystal-mode "me" "play")
      (space-macs/set-leader-keys-for-major-mode 'crystal-mode
        "eb" 'play-crystal-submit-buffer
        "ee" 'play-crystal-browse
        "ei" 'play-crystal-insert
        "er" 'play-crystal-submit-region))))


