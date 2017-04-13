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

(defvar ycmd/all-the-modes nil
  "use ycmd for all the modes it supports, most will only use fancy keyword completion.")

(defun ycmd/init-ycmd ()
  (use-package ycmd
    :init
    (progn
      (if ycmd/all-the-modes
          (ycmd-setup)
        (add-hook 'c++-mode-hook 'ycmd-mode))
      (setq-default ycmd-global-config
                    (expand-file-name "~/.emacs.d/contrib/ycmd/global_conf.py")))
    :config
    (evil-leader/set-key-for-mode 'c++-mode
      "mg" 'ycmd-goto
      "mG" 'ycmd-goto-imprecise)))

(defun ycmd/init-company-ycmd ()
  (use-package company-ycmd
    :init (company-ycmd-setup)))

(defun ycmd/init-flycheck-ycmd ()
  (use-package flycheck-ycmd
    :init
    (progn
      (add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results)
      (add-to-list 'flycheck-checkers 'ycmd))))
