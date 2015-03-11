(defun dash/activate-package-docsets (root)
  (progn
    (setq helm-dash-docsets-path root)
    (setq helm-dash-common-docsets (helm-dash-installed-docsets))

    (message
     (format "activated %d docsets from: %s"
             (length helm-dash-common-docsets) root))
))
