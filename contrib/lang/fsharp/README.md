# F# contribution layer for Spacemacs

![logo_fsharp](img/fsharp.png)

## Description

This layer adds support for F# language using [fsharpbindng](https://github.com/fsharp/fsharpbinding).

## Packages Included

- [fsharp-mode](https://github.com/fsharp/fsharpbinding) 

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(fsharp))
```

## Key Bindings

### Compilation

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>mcc</kbd>        | Build the project 

### Navigation

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>mcd</kbd>        | Go to definition at point
<kbd>men</kbd>        | Next error
<kbd>mep</kbd>        | Previous error

### REPL

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>mer</kbd>        | Evaluate region
<kbd>mep</kbd>        | Evaluate phrase
<kbd>mef</kbd>        | Evaluate buffer
<kbd>mss</kbd>        | Start REPL

### Helpers (documentation, info)

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>mst</kbd>        | Show tooltip at point
