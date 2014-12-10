(defvar ess-packages
  '(
    ess
    ess-R-data-view
    ess-R-object-popup
    ess-smart-underscore
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar ess-excluded-packages '()
  "List of packages to exclude.")

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
         "mi" 'R
         "mp" 'ess-R-object-popup
         "mB" 'ess-eval-buffer-and-go
         "mb" 'ess-eval-buffer
         "mD" 'ess-eval-function-or-paragraph-and-step
         "md" 'ess-eval-region-or-line-and-step
         "mL" 'ess-eval-line-and-go
         "ml" 'ess-eval-line
         "mR" 'ess-eval-region-and-go
         "mr" 'ess-eval-region
         "mT" 'ess-eval-function-and-go
         "mt" 'ess-eval-function
         "mvp" 'ess-R-dv-pprint
         "mvt" 'ess-R-dv-ctable
         )
       (define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-input)
       (define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input))))
