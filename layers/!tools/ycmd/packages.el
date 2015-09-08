(setq ycmd-packages
  '(
    company-ycmd
    flycheck-ycmd
    ycmd
    ))

(unless (boundp 'ycmd-server-command)
  (message (concat "YCMD won't work unless you set the ycmd-server-command "
                   "variable to the path to a ycmd install.")))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun ycmd/init-company-ycmd ()
    (use-package company-ycmd
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :commands company-ycmd)))

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
    (unless (boundp 'ycmd-global-config)
      (let ((dir (configuration-layer/get-layer-property 'ycmd :dir)))
        (setq-default ycmd-global-config (concat dir "global_conf.py"))))))
