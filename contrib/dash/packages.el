(defvar dash-packages
  '(
    dash-at-point
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun dash/init-dash-at-point ()
  (use-package dash-at-point
    :commands dash-at-point
    :init
    (evil-leader/set-key "dd" 'dash-at-point)
    (evil-leader/set-key "dD" 'dash-at-point-with-docset)))
