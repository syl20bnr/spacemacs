(setq ycmd-packages
  '(
    (company-ycmd :toggle (configuration-layer/package-usedp 'company))
    (flycheck-ycmd :toggle (configuration-layer/package-usedp 'flycheck))
    ycmd
    ))

(unless (boundp 'ycmd-server-command)
  (message (concat "YCMD won't work unless you set the ycmd-server-command "
                   "variable to the path to a ycmd install.")))

(defun ycmd/init-company-ycmd ()
  (use-package company-ycmd
    :defer t
    :commands company-ycmd))

(defun ycmd/init-flycheck-ycmd ()
  (use-package flycheck-ycmd
    :defer t
    :init (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup)))

(defun ycmd/init-ycmd ()
  (use-package ycmd
    :defer t
    :init
    (progn
      (unless (boundp 'ycmd-global-config)
        (setq-default ycmd-global-config
                      (concat (configuration-layer/get-layer-path 'ycmd)
                              "global_conf.py")))
      (setq-default ycmd-parse-conditions '(save mode-enabled)))))
