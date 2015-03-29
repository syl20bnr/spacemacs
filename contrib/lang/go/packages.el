(defvar go-packages
  '(
    flycheck
    go-mode
    go-eldoc
    go-autocomplete
    company-go
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun go/init-flycheck ()
    (add-hook 'go-mode-hook 'flycheck-mode))

(defun go/init-go-mode()
  (use-package go-mode
    :defer t
    :config
      (add-hook 'before-save-hook 'gofmt-before-save)
      (evil-leader/set-key-for-mode 'go-mode
        "mdp"  'godoc-at-point
        "mig"  'go-goto-imports
        "mia"  'go-import-add
        "mir"  'go-remove-unused-imports
        "mpb"  'go-play-buffer
        "mpr"  'go-play-region
        "mpd"  'go-download-play
        "mgg"   'godef-jump
      )
    ))

(defun go/init-go-eldoc()
    (add-hook 'go-mode-hook 'go-eldoc-setup))

(defun go/init-go-autocomplete()
  (use-package go-autocomplete
    :if (boundp 'ac-sources)
    :defer t
    :init (add-to-list 'ac-sources 'ac-source-go)
  )
)
(defun go/init-company-go ()
 (use-package company-go
   :if (configuration-layer/package-declaredp 'company)
   :defer t
   :init
   (progn
     (spacemacs|add-mode-company-backend go-mode company-go))))
