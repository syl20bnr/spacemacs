(use-package google-translate
  :commands (google-translate-query-translate
             google-translate-at-point
             google-translate-query-translate-reverse
             google-translate-at-point-reverse)
  :config
  (progn
    (setq google-translate-enable-ido-completion t)
    (setq google-translate-default-source-language "En")
    (setq google-translate-default-target-language "Fr")))
