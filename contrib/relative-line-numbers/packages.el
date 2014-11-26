(defvar relative-line-numbers-packages
  '(
    linum-relative
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar relative-line-numbers-excluded-packages '()
  "List of packages to exclude.")

(defun relative-line-numbers/init-linum-relative ()
  "Initialize linum-relative"
  (use-package linum-relative
    :config
    (progn
      (setq linum-format 'linum-relative)
      (setq linum-relative-current-symbol "")

      (evil-leader/set-key "tr" 'linum-relative-toggle)))
  )
