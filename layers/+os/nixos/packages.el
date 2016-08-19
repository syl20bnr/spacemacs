(setq nixos-packages
      '(
        company
        (company-nixos-options :toggle (configuration-layer/package-usedp 'company))
        (helm-nixos-options :toggle (configuration-layer/package-usedp 'helm))
        nix-mode
        nixos-options
        ))

(defun nixos/post-init-company ()
  (spacemacs|add-company-hook nix-mode)
  (push 'company-capf company-backends-nix-mode))

(defun nixos/init-company-nixos-options ()
  (use-package company-nixos-options
    :defer t
    :init
    (push 'company-nixos-options company-backends-nix-mode)))

(defun nixos/init-helm-nixos-options ()
  (use-package helm-nixos-options
    :config
    (spacemacs/set-leader-keys
      "h>" 'helm-nixos-options)))

(defun nixos/init-nix-mode ()
  (use-package nix-mode)
  (add-to-list 'spacemacs-indent-sensitive-modes 'nix-mode))

(defun nixos/init-nixos-options ()
  (use-package nixos-options))
