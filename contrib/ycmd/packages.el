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
    :commands company-ycmd
    :init (push '(company-ycmd :with company-yasnippet)
                company-backends-c-mode-common)))

(when (configuration-layer/layer-usedp 'syntax-checking)
  (defun ycmd/init-flycheck-ycmd ()
    (use-package flycheck-ycmd
      :if (configuration-layer/package-usedp 'flycheck)
      :defer t
      :init (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup))))

(defun ycmd/init-ycmd ()
  (use-package ycmd
    :defer t
    :init
    (progn
      ;; we don't use ycmd-setup, to correctly lazy-load ycmd we
      ;; define excplicitly the hooks here
      (add-hook 'c++-mode-hook 'ycmd-mode)
      (unless (boundp 'ycmd-global-config)
        (let ((dir (configuration-layer/get-layer-property 'ycmd :dir)))
          (setq-default ycmd-global-config (concat dir "global_conf.py"))))
      (evil-leader/set-key-for-mode 'c++-mode
        "mgg" 'ycmd-goto
        "mgG" 'ycmd-goto-imprecise))))
