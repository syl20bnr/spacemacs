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
