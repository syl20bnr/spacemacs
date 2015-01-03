# fasd contribution layer for Spacemacs

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(fasd)
  "List of contribution to load."
)
```

This simply adds the fasd-emacs and a single keybinding

## Keybindings

Key Binding         |                 Description
--------------------|------------------------------------------------------------------
<kbd>SPC f z</kbd>  | find a file with fasd
