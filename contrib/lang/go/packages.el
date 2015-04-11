(defvar go-packages
  '(
    company
    company-go
    flycheck
    go-mode
    go-eldoc
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun go/post-init-flycheck ()
    (add-hook 'go-mode-hook 'flycheck-mode))

(defun go/init-go-mode()
  (use-package go-mode
    :defer t
    :config
    (progn
      (add-hook 'before-save-hook 'gofmt-before-save)

      (defun spacemacs/go-run-package-tests ()
        (interactive)
        (shell-command "go test"))

      (evil-leader/set-key-for-mode 'go-mode
        "mdp" 'godoc-at-point
        "mig" 'go-goto-imports
        "mia" 'go-import-add
        "mir" 'go-remove-unused-imports
        "mpb" 'go-play-buffer
        "mpr" 'go-play-region
        "mpd" 'go-download-play
        "mgg" 'godef-jump
        "mtp" 'spacemacs/go-run-package-tests))))

(defun go/init-go-eldoc()
    (add-hook 'go-mode-hook 'go-eldoc-setup))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun go/post-init-company ()
    (spacemacs|add-company-hook go-mode))

  (defun go/init-company-go ()
    (use-package company-go
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :init
      (push '(company-go :with company-yasnippet) company-backends-go-mode))))
