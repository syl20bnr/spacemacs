(defvar go-packages
  '(
    flycheck
    go-mode
    go-eldoc
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun go/init-flycheck ()
    (add-hook 'go-mode-hook 'flycheck-mode))

(defun go/init-go-mode()
  (use-package go-mode
    :defer t
    :init
    (progn
      (evil-leader/set-key
        "mdp"  'godoc-at-point
        "mig"  'go-goto-imports
        "mia"  'go-import-add
        "mir"  'go-remove-unused-imports
        "mpb"  'go-play-buffer
        "mpr"  'go-play-region
        "mpd"  'go-download-play
        "mg"   'godef-jump
      ))
    :config
    (add-hook 'before-save-hook 'gofmt-before-save)
    ))

(defun go/init-go-eldoc()
    (add-hook 'go-mode-hook 'go-eldoc-setup))
