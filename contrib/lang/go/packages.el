(defvar go-packages
  '(
    go-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

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
      )
    )
    :config
    (add-hook 'before-save-hook 'gofmt-before-save)
    (add-hook 'go-mode-hook 'flycheck-mode)
    )
  )

