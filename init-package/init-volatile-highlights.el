(use-package volatile-highlights
  :init
  (volatile-highlights-mode t)
  :config
  (progn
  ;; define extensions for evil mode
  (vhl/define-extension 'evil_past_after 'evil-paste-after)
  (vhl/install-extension 'evil_past_after)
  )
)
