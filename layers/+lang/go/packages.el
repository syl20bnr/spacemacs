(setq go-packages
  '(
    company
    company-go
    flycheck
    go-mode
    go-eldoc
    ))

(defun go/post-init-flycheck ()
    (add-hook 'go-mode-hook 'flycheck-mode))

(defun go/init-go-mode()
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-env "GOPATH"))

  (use-package go-mode
    :defer t
    :config
    (progn
      (add-hook 'before-save-hook 'gofmt-before-save)

      (defun spacemacs/go-run-package-tests ()
        (interactive)
        (shell-command "go test"))

      (evil-leader/set-key-for-mode 'go-mode
        "mhh" 'godoc-at-point
        "mig" 'go-goto-imports
        "mia" 'go-import-add
        "mir" 'go-remove-unused-imports
        "meb" 'go-play-buffer
        "mer" 'go-play-region
        "med" 'go-download-play
        "mga" 'ff-find-other-file
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
      (push 'company-go company-backends-go-mode))))
