# SML contribution layer for Spacemacs

![logo_sml](img/sml.jpeg)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [SML contribution layer for Spacemacs](#sml-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key Bindings](#key-bindings)
        - [Form Completion](#form-completion)
        - [REPL](#repl)

<!-- markdown-toc end -->


## Description

Adds support for the [SML](http://www.smlnj.org) programming language.

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(sml))
```

## Key Bindings

### Form Completion

    Key Binding  |           Description
-----------------|------------------------------------------------------------------------------------------
<kbd>M-SPC</kbd> | Inserts a space and completes the form before the cursor.
<kbd>\|</kbd>     | Inserts a \| and adds a double arrow or copies the function name. Generally just works.

### REPL

    Key Binding       |           Description
----------------------|-----------------------------------------------------------------
<kbd>SPC m s b</kbd>  | Send buffer to REPL and switch to REPL buffer in `insert state`
<kbd>SPC m s r</kbd>  | Send region to REPL and switch to REPL buffer in `insert state`
<kbd>SPC m s s</kbd>  | Run the sml REPL or switch to it if the REPL is already running
