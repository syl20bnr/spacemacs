(setq rcirc-post-extensions
      '(helm-rcirc))

(when (configuration-layer/layer-usedp 'spacemacs-helm)
  (defun rcirc/init-helm-rcirc ()
    (use-package helm-rcirc
      :commands helm-rcirc-auto-join-channels
      :init
      (spacemacs/set-leader-keys "irc" 'helm-rcirc-auto-join-channels))))
