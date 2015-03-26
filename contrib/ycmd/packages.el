(defvar ycmd-packages
  '(
    ycmd
    flycheck-ycmd
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(unless (boundp 'ycmd-server-command)
  (message "YCMD won't work unless you set the ycmd-server-command variable to the path to a ycmd install."))

(when (member 'company-mode dotspacemacs-configuration-layers)
  (add-to-list 'ycmd-packages 'company-ycmd))

(defun ycmd/init-ycmd ()
  (use-package ycmd
    :defer t
    :init
    (progn
      ;; we don't use ycmd-setup, to correctly lazy-load ycmd we
      ;; define excplicitly the hooks here
      (add-hook 'c++-mode-hook 'ycmd-mode)
      (setq-default ycmd-global-config
                    (expand-file-name (concat user-emacs-directory
                                              "contrib/ycmd/global_conf.py")))
      (evil-leader/set-key-for-mode 'c++-mode
        "mgg" 'ycmd-goto
        "mgG" 'ycmd-goto-imprecise))))

(defun ycmd/init-company-ycmd ()
  (use-package company-ycmd
    :defer t
    :init
    (progn
      (spacemacs|add-mode-company-backend c-mode company-ycmd)
      (spacemacs|add-mode-company-backend c++-mode company-ycmd))))

(defun ycmd/init-flycheck-ycmd ()
  (use-package flycheck-ycmd
    :defer t
    :init (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup)))
