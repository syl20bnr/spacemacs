(defvar dash-packages
   '()
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(if (system-is-mac)
    (push 'dash-at-point dash-packages))

(if (system-is-linux)
    (push 'helm-dash dash-packages))

(defun dash/init-dash-at-point ()
  (use-package dash-at-point
    :commands dash-at-point
    :init
    (evil-leader/set-key "dd" 'dash-at-point)
    (evil-leader/set-key "dD" 'dash-at-point-with-docset)))

(defun dash/init-helm-dash ()
  (use-package helm-dash
    :commands helm-dash
    :init
      (evil-leader/set-key "dd" 'helm-dash-at-point)
      (evil-leader/set-key "dD" 'helm-dash)
    :config
    (progn
      (dash/activate-package-docsets dash/helm-dash-docset-newpath)
     )))
