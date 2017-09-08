(setq nixos-packages
      '(
        company
        flycheck
        (company-nixos-options :requires company)
        (helm-nixos-options :requires helm)
        nix-mode
        nixos-options
        ))

(defun nixos/post-init-company ()
  (let ((backends '(company-capf)))
    (when (configuration-layer/package-used-p 'company-nixos-options)
      (add-to-list 'backends 'company-nixos-options t))
    (eval `(spacemacs|add-company-backends
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
      (spacemacs/set-leader-keys
        "h>" 'helm-nixos-options))))

(defun nixos/init-nix-mode ()
  (use-package nix-mode)
  (add-to-list 'spacemacs-indent-sensitive-modes 'nix-mode))

(defun nixos/init-nixos-options ()
  (use-package nixos-options :defer t))

(defun nixos/post-init-flycheck ()
  (spacemacs/enable-flycheck 'nix-mode))
