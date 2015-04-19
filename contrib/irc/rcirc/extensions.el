(setq rcirc-post-extensions
  '(
    helm-rcirc
    ))

(defun rcirc/init-helm-rcirc ()
  (use-package helm-rcirc
    :commands helm-rcirc-auto-join-channels
    :init
    (evil-leader/set-key "irc" 'helm-rcirc-auto-join-channels)))
