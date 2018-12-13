;;; packages.el --- Crystal Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Brantou <brantou89@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst crystal-packages
  '(
    (ameba :location (recipe :fetcher github
                             :repo "veelenga/ameba.el"
                             :files ("ameba.el")))
    company
    crystal-mode
    flycheck
    (flycheck-crystal :requires flycheck)
    inf-crystal
    ob-crystal
    play-crystal
    ))

(defun crystal/init-ameba()
  (use-package ameba
    :defer t
    :init (add-hook 'crystal-mode-hook 'ameba-mode)
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'crystal-mode "ma" "ameba")
      (spacemacs/set-leader-keys-for-major-mode 'crystal-mode
        "ad" 'ameba-check-directory
        "af" 'ameba-check-current-file
        "ap" 'ameba-check-project))))

(defun crystal/post-init-company()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes crystal-mode
    :variables company-tooltip-align-annotations t))

(defun crystal/init-crystal-mode()
  (use-package crystal-mode
    :defer t
    :config
    (progn
      (add-hook 'crystal-mode-hook 'spacemacs//crystal-auto-format-setup)

      (spacemacs/declare-prefix-for-mode 'crystal-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'crystal-mode "mt" "test")
      (spacemacs/declare-prefix-for-mode 'crystal-mode "mu" "tool")
      (spacemacs/declare-prefix-for-mode 'crystal-mode "mx" "execute")
      (spacemacs/set-leader-keys-for-major-mode 'crystal-mode
        "ga" 'crystal-spec-switch
        "tb" 'crystal-spec-buffer
        "tp" 'crystal-spec-all
        "uc" 'crystal-tool-context
        "ue" 'crystal-tool-expand
        "uf" 'crystal-tool-format
        "ui" 'crystal-tool-imp
        "xx" 'spacemacs/crystal-run-main))))

(defun crystal/post-init-flycheck()
  (spacemacs/enable-flycheck 'crystal-mode))

(defun crystal/init-flycheck-crystal ()
  (use-package flycheck-crystal
    :defer t
    :init (add-hook 'crystal-mode-hook 'flycheck-mode)))

(defun crystal/init-inf-crystal()
  (use-package inf-crystal
    :defer t
    :init
    (progn
      (spacemacs/register-repl 'inf-crystal 'inf-crystal "inf-crystal")
      (add-hook 'crystal-mode-hook 'inf-crystal-minor-mode))
    :config
    (progn
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

(defun crystal/init-play-crystal()
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

