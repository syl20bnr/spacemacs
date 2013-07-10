(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(add-hook 'ace-jump-mode-end-hook 'golden-ratio)
