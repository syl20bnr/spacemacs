(spacemacs/declare-prefix "ai" "AI Completion - minuet")
(spacemacs/set-leader-keys
  "aii" 'minuet-complete-with-minibuffer
  "aiI" 'minuet-show-suggestion
  "aicp" 'minuet-configure-provider
  "ail" 'minuet-accept-suggestion-line
  "aiL" 'minuet-accept-suggestion
  "aij" 'minuet-next-suggestion
  "aik" 'minuet-previous-suggestion
  "aih" 'minuet-dismiss-suggestion
  )
