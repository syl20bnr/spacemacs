(defvar ess-packages
  '(
    ess
    ess-R-data-view
    ess-R-object-popup
    ess-smart-underscore
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun ess/init-ess ()
  ;; ESS is not quick to load so we just load it when
  ;; we need it (see my-keybindings.el for the associated
  ;; keybinding)
  (defun load-ess-on-demand ()
    (interactive)
    (use-package ess-site)
    (use-package ess-smart-underscore)
    (use-package ess-R-object-popup)
    (use-package ess-R-data-view)
    )
  (evil-leader/set-key "ess" 'load-ess-on-demand)

  ;; R --------------------------------------------------------------------------
  (eval-after-load "ess-site"
    '(progn
       (evil-leader/set-key-for-mode 'ess-mode
         "mB" 'ess-eval-buffer-and-go
         "mb" 'ess-eval-buffer
         "mF" 'ess-eval-function-and-go
         "mf" 'ess-eval-function
         "mi" 'R
         "mL" 'ess-eval-line-and-go
         "ml" 'ess-eval-line
         "mp" 'ess-R-object-popup
         "mR" 'ess-eval-region-and-go
         "mr" 'ess-eval-region
         "mS" 'ess-eval-function-or-paragraph-and-step
         "ms" 'ess-eval-region-or-line-and-step
         "mvp" 'ess-R-dv-pprint
         "mvt" 'ess-R-dv-ctable
         )
       (define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-input)
       (define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input))))
