(after "volatile-highlights-autoloads"
  (require 'volatile-highlights)
  (volatile-highlights-mode t)

  ;; define extensions for evil mode
  (vhl/define-extension 'evil_past_after 'evil-paste-after)
  (vhl/install-extension 'evil_past_after)
  )
