 ;; see conditional package inclusion
(setq dash-packages '())

(cond
 ((system-is-mac) (push 'dash-at-point dash-packages))
 ((system-is-linux) (push 'helm-dash dash-packages)))

(defun dash/init-dash-at-point ()
  (use-package dash-at-point
    :defer t
    :init
    (progn
      (evil-leader/set-key "dd" 'dash-at-point)
      (evil-leader/set-key "dD" 'dash-at-point-with-docset))))

(defun dash/init-helm-dash ()
  (use-package helm-dash
    :defer t
    :init
    (progn
      (evil-leader/set-key "dd" 'helm-dash-at-point)
      (evil-leader/set-key "dD" 'helm-dash))
    :config
    (progn
      (defun dash//activate-package-docsets (path)
        "Add dash docsets from specified PATH."
        (setq helm-dash-docsets-path path
              helm-dash-common-docsets (helm-dash-installed-docsets))
        (message (format "activated %d docsets from: %s"
                         (length helm-dash-common-docsets) path)))
      (dash//activate-package-docsets dash-helm-dash-docset-path))))
