(spacemacs/declare-prefix
  "ai" "AI Completion - minuet"
  "aic" "Configurations")

(spacemacs/set-leader-keys
  "aii" 'minuet-show-suggestion
  "aiI" 'minuet-complete-with-minibuffer
  "aicp" 'minuet-configure-provider
  "aiM" 'minuet-auto-suggestion-mode
  )
