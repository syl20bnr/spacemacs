 ;; see conditional package inclusion
(setq dash-packages '(helm-dash))

(cond
 ((spacemacs/system-is-mac)
  (push 'dash-at-point dash-packages))
 ((or (spacemacs/system-is-linux)
      (spacemacs/system-is-mswindows))
  (push 'zeal-at-point dash-packages)))

(defun dash/init-helm-dash ()
  (use-package helm-dash
    :defer t
    :init
    (spacemacs/set-leader-keys
      "dh" 'helm-dash-at-point
      "dH" 'helm-dash)
    :config
    (defun dash//activate-package-docsets (path)
      "Add dash docsets from specified PATH."
      (setq helm-dash-docsets-path path
            helm-dash-common-docsets (helm-dash-installed-docsets))
      (message (format "activated %d docsets from: %s"
                       (length helm-dash-common-docsets) path)))
    (dash//activate-package-docsets dash-helm-dash-docset-path)))

(defun dash/init-dash-at-point ()
  (use-package dash-at-point
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "dd" 'dash-at-point)
      (spacemacs/set-leader-keys "dD" 'dash-at-point-with-docset))))

(defun dash/init-zeal-at-point ()
  (use-package zeal-at-point
    :defer t
    :init
    (spacemacs/set-leader-keys
      "dd" 'zeal-at-point
      "dD" 'zeal-at-point-set-docset)
    :config
    ;; This lets users seach in multiple docsets
    (push '(web-mode . "html,css,javascript") zeal-at-point-mode-alist)
    ))
