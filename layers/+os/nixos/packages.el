;;; packages.el --- NixOS Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst nixos-packages
      '(company
        (company-nixos-options :requires company)
        flycheck
        (helm-nixos-options :requires helm)
        nix-mode
        nixos-options))

(defun nixos/post-init-company ()
  (when nixos-enable-company
    (let ((backends '(company-capf)))
      (when (configuration-layer/package-used-p 'company-nixos-options)
        (add-to-list 'backends 'company-nixos-options t))
      (eval `(spacemacs|add-company-backends
               :backends ,backends
               :modes nix-mode)))))

(defun nixos/init-company-nixos-options ()
 (use-package company-nixos-options
   :if nixos-enable-company
   :defer t))

(defun nixos/init-helm-nixos-options ()
  (use-package helm-nixos-options
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "h>" 'helm-nixos-options))))

(defun nixos/init-nix-mode ()
  (use-package nix-mode
    :defer t
    :mode "\\.nix\\'"
    :init
    (add-to-list 'spacemacs-indent-sensitive-modes 'nix-mode)
    :config
    (electric-indent-mode -1)))

(defun nixos/init-nixos-options ()
  (use-package nixos-options :defer t))

(defun nixos/post-init-flycheck ()
  (spacemacs/enable-flycheck 'nix-mode))
