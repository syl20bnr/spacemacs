(setq olivetti-packages '(olivetti))

(defun olivetti/init-olivetti ()
  "Initialize Olivetti."
  (use-package olivetti
    :defer t
    :init
    (progn
      (spacemacs|define-transient-state olivetti
        :title "Olivetti"
        :on-enter (turn-on-olivetti-mode)
        :bindings
        ("[" olivetti-shrink "Shrink")
        ("]" olivetti-expand "Expand")
        ("s" olivetti-set-width "Set width")
        ("Q" olivetti-mode "Turn off" :exit t)
        ("q" nil "Quit" :exit t))
      (spacemacs/set-leader-keys "To" 'spacemacs/olivetti-transient-state/body))
    :config
    (spacemacs|hide-lighter olivetti-mode)))
