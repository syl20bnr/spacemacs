# Clojure contribution layer for Spacemacs

![logo](clojure.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Clojure contribution layer for Spacemacs](#clojure-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Packages Included](#packages-included)
    - [Install](#install)
        - [Cider](#cider)
    - [Key Bindings](#key-bindings)
        - [Documentation](#documentation)
        - [Evaluation](#evaluation)
        - [Goto](#goto)
        - [REPL](#repl)
        - [Tests](#tests)

<!-- markdown-toc end -->

## Description

This layer adds support for [Clojure][] language using [Cider][].

## Packages Included

- [Cider][]
- [clojure-mode][](https://github.com/clojure-emacs/clojure-mode)
- [Clj Refactor][](https://github.com/clojure-emacs/clj-refactor.el)
- [Cider Auto Complete][](https://github.com/clojure-emacs/ac-cider)

### Goodies

- Pretty symbols for anonymous functions and set literals, like
`(λ [a] (+ a 5))`, `ƒ(+ % 5)`, and `∈{2 4 6}`."

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(clojure)
  "List of contribution to load."
)
```

### Cider

Cider requires nRepl middleware to function, please check the installation
instructions at the [cider repository][cider_install].

## Key Bindings

### Documentation

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m d d</kbd>  | cider doc
<kbd>SPC m d g</kbd>  | cider grimoire
<kbd>SPC m d j</kbd>  | cider javadoc

### Evaluation

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m e b</kbd>  | eval buffer
<kbd>SPC m e r</kbd>  | eval region
<kbd>SPC m e s</kbd>  | eval last sexp

### Goto

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m g v</kbd>  | goto var
<kbd>SPC m g e</kbd>  | goto error
<kbd>SPC m g s</kbd>  | goto symbol

### REPL

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m j</kbd>    | cider jack in
<kbd>SPC m k</kbd>    | cider load-buffer
<kbd>SPC m z</kbd>    | switch to repl

### Tests

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m t t</kbd>  | run tests

[Clojure]: http://clojure.org
[Cider]: https://github.com/clojure-emacs/cider
[cider_install]: (https://github.com/clojure-emacs/cider#installation)
[clojure-mode]: https://github.com/clojure-emacs/clojure-mode
[Clj Refactor]: https://github.com/clojure-emacs/clj-refactor.el
[Cider Auto Complete]: (https://github.com/clojure-emacs/ac-cider
