# F# contribution layer for Spacemacs

![logo_fsharp](img/fsharp.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [F# contribution layer for Spacemacs](#f-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Packages Included](#packages-included)
    - [Install](#install)
    - [Key Bindings](#key-bindings)
        - [REPL](#repl)

<!-- markdown-toc end -->

## Description

This layer adds support for F# language using [fsharpbinding][].

## Packages Included

- [fsharp-mode][]

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(fsharp))
```

## Key Bindings


    Key Binding       |                 Description
----------------------|--------------------------------------------------------
<kbd>mcc</kbd>        | Build the project
<kbd>mgg</kbd>        | Go to definition at point
<kbd>mht</kbd>        | Show tooltip help at point

### REPL

    Key Binding       |                 Description
----------------------|--------------------------------------------------------
<kbd>msb</kbd>        | Send buffer to the REPL
<kbd>msB</kbd>        | Send buffer to the REPL and switch to the REPL in `insert state`
<kbd>msi</kbd>        | Start a REPL
<kbd>msp</kbd>        | Send phrase to the REPL
<kbd>msP</kbd>        | Send phrase to the REPL and switch to the REPL in `insert state`
<kbd>msr</kbd>        | Send region to the REPL
<kbd>msR</kbd>        | Send region to the REPL and switch to the REPL in `insert state`
<kbd>mss</kbd>        | Show the REPL

[fsharpbinding]: https://github.com/fsharp/fsharpbinding
[fsharp-mode]: https://github.com/fsharp/fsharpbinding
