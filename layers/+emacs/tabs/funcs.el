(defun spacemacs//tabs-timer-initialize (secs)
  (setq spacemacs-tabs-timer (run-with-timer secs nil (lambda () (centaur-tabs-local-mode 1)))))

(defun spacemacs//tabs-timer-hide ()
  (spacemacs//tabs-timer-initialize tabs-auto-hide-delay))

(defun spacemacs//tabs-switch-and-hide (arg)
  (cancel-timer spacemacs-tabs-timer)
  (centaur-tabs-local-mode 1)
  ;; (if arg
  ;;     (centaur-tabs-backward)
  ;;   (centaur-tabs-forward))
  (pcase arg
    ('backward (centaur-tabs-backward))
    ('forward (centaur-tabs-forward))
    ('backward-group (centaur-tabs-backward-group))
    ('forward-group (centaur-tabs-forward-group)))
  (centaur-tabs-local-mode 0)
  (spacemacs//tabs-timer-hide))

(defun spacemacs//centaur-tabs-forward-and-hide ()
  (spacemacs//tabs-switch-and-hide 'forward))

(defun spacemacs//centaur-tabs-backward-and-hide ()
  (spacemacs//tabs-switch-and-hide 'backward))

(defun spacemacs/tabs-forward ()
  (interactive)
  (if tabs-auto-hide
      (spacemacs//centaur-tabs-forward-and-hide)
    (centaur-tabs-forward)))

(defun spacemacs/tabs-backward ()
  (interactive)
  (if tabs-auto-hide
      (spacemacs//centaur-tabs-backward-and-hide)
    (centaur-tabs-backward)))

(defun spacemacs/tabs-forward-group-and-hide ()
  (interactive)
  (spacemacs//tabs-switch-and-hide 'forward-group))

(defun spacemacs/tabs-backward-group-and-hide ()
  (interactive)
  (spacemacs//tabs-switch-and-hide 'backward-group))
