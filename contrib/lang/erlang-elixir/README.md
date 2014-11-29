# Erlang/Elixir contribution layer for Spacemacs

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(defvar dotspacemacs-configuration-layers '(erlang-elixir)
  "List of contribution to load."
)
```

### Erlang

`Spacemacs` uses [EDTS][edts] as an Erlang coding environment.

    Key Binding     |                 Description
--------------------|------------------------------------------------------------
<kbd>SPC m d</kbd>  | show man page documentation
<kbd>SPC m e</kbd>  | go to next issue
<kbd>SPC m g</kbd>  | go to definition
<kbd>SPC m G</kbd>  | find a module in the current project
<kbd>SPC m h</kbd>  | open the header file under point
<kbd>SPC m l</kbd>  | find a function in the current module
<kbd>SPC m m</kbd>  | go to the macro definition under point
<kbd>SPC m r</kbd>  | go to the record definition under point

