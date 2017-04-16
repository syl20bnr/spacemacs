(defvar evil-annoying-arrows-packages '(evil-annoying-arrows)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar evil-annoying-arrows-excluded-packages '()
  "List of packages to exclude.")

(defun evil-annoying-arrows/init-evil-annoying-arrows ()
  (global-evil-annoying-arrows-mode t)
  (setq evil-annoying-arrows-too-far-count 10)
  (setq evil-annoying-arrows-super-annoying-mode nil))
