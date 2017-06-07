(defun short-key-state (modeon)
  (if modeon
      (evil-evilified-state)
    (evil-normal-state)))

(advice-add 'realgud-short-key-mode-setup :before #'short-key-state)
