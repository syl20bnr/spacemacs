# Racket contribution layer for Spacemacs

![logo_racket](img/racket.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Racket contribution layer for Spacemacs](#racket-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key Bindings](#key-bindings)
        - [Navigation](#navigation)
        - [Documentation](#documentation)
        - [Tests](#tests)
        - [REPL](#repl)
        - [Other key bindings](#other-key-bindings)

<!-- markdown-toc end -->

## Description

Adds support for the [Racket](http://racket-lang.org/) programming language.

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(racket))
```

## Key Bindings

### Navigation

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m g `</kbd>  | Return to previous location
<kbd>SPC m g g</kbd>  | Go to definition of symbol at point
<kbd>SPC m g m</kbd>  | Go to module at point
<kbd>SPC m g r</kbd>  | Open require path

### Documentation

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m h d</kbd>  | Describes the function at point in a `Racket Describe` buffer
<kbd>SPC m h h</kbd>  | View documentation of the identifier or string at point.

### Tests

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m t b</kbd>  | Run tests of buffer
<kbd>SPC m t B</kbd>  | Run tests of buffer with coverage

### REPL

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m s b</kbd>  | Send buffer to REPL
<kbd>SPC m s B</kbd>  | Send buffer to REPL and switch to REPL buffer in `insert state`
<kbd>SPC m s e</kbd>  | Send last sexp to REPL
<kbd>SPC m s E</kbd>  | Send last sexp to REPL and switch to REPL in `insert state`
<kbd>SPC m s f</kbd>  | Send function at point to REPL
<kbd>SPC m s F</kbd>  | Send function at point and switch to REPL in `insert state`
<kbd>SPC m s i</kbd>  | Start a REPL or switch to REPL buffer
<kbd>SPC m s r</kbd>  | Send region to REPL
<kbd>SPC m s R</kbd>  | Send region to REPL and switch to REPL in `insert state`
<kbd>SPC m s s</kbd>  | Show and switch to REPL buffer

### Other key bindings

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m i l</kbd>  | Insert lambda character
<kbd>H-r</kbd>        | Run current file and open REPL (`H` is hyper, *may* be bound to command on OSX)
