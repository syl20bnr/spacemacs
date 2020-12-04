;;; packages.el --- NixOS Layer packages File for Space-macs
;;
;; Copyright (c) 2015-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst nixos-packages
  '((company-nixos-options :requires company)
     flycheck
     (helm-nixos-options :requires helm)
     nix-mode
     nixos-options))

(defun nixos/post-init-company ()
  (let ((backends '(company-capf)))
    (when (configuration-layer/package-used-p 'company-nixos-options)
      (add-to-list 'backends 'company-nixos-options t))
    (eval `(space-macs|add-company-backends
              :backends ,backends
              :modes nix-mode))))

(defun nixos/init-company-nixos-options ()
  (use-package company-nixos-options
    :defer t))

(defun nixos/init-helm-nixos-options ()
  (use-package helm-nixos-options
    :defer t
    :init
    (progn
      (space-macs/set-leader-keys
        "h>" 'helm-nixos-options))))

(defun nixos/init-nix-mode ()
  (use-package nix-mode
    :defer t
    :mode "\\.nix\\'"
    :init
    (add-to-list 'space-macs-indent-sensitive-modes 'nix-mode)
    :config
    (electric-indent-mode -1)))

(defun nixos/init-nixos-options ()
  (use-package nixos-options :defer t))

(defun nixos/post-init-flycheck ()
  (space-macs/enable-flycheck 'nix-mode))


