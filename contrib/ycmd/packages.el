(defvar ycmd-packages
  '(
    company-ycmd
    flycheck-ycmd
    ycmd
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(unless (boundp 'ycmd-server-command)
  (message (concat "YCMD won't work unless you set the ycmd-server-command "
                   "variable to the path to a ycmd install.")))

(defun ycmd/init-company-ycmd ()
  (use-package company-ycmd
    :if (configuration-layer/layer-usedp 'auto-completion)
    :defer t
    :init (push '(company-ycmd :with company-yasnippet)
                company-backends-c-c++)))

(defun ycmd/init-flycheck-ycmd ()
  (use-package flycheck-ycmd
    :defer t
    :init (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup)))

(defun ycmd/init-ycmd ()
  (use-package ycmd
    :defer t
    :init
    (progn
      ;; we don't use ycmd-setup, to correctly lazy-load ycmd we
      ;; define excplicitly the hooks here
      (add-hook 'c++-mode-hook 'ycmd-mode)
      (unless (boundp 'ycmd-global-config)
        (setq-default ycmd-global-config
                      (expand-file-name "~/.emacs.d/contrib/ycmd/global_conf.py")))
      (evil-leader/set-key-for-mode 'c++-mode
        "mgg" 'ycmd-goto
        "mgG" 'ycmd-goto-imprecise))))
