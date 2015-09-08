(setq nixos-packages '(nix-mode
                       company
                       nixos-options
                       helm-nixos-options
                       company-nixos-options))

(defun nixos/init-nix-mode ()
  (use-package nix-mode))

(defun nixos/init-nixos-options ()
  (use-package nixos-options))

(defun nixos/init-helm-nixos-options ()
  (use-package helm-nixos-options
    :config
    (evil-leader/set-key
      "h>" 'helm-nixos-options)))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun nixos/post-init-company ()
    (spacemacs|add-company-hook nix-mode)
    (push 'company-capf company-backends-nix-mode))

  (defun nixos/init-company-nixos-options ()
    (use-package company-nixos-options
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :init
      (push 'company-nixos-options company-backends-nix-mode))))
