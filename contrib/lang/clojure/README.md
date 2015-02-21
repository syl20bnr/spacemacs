# Clojure contribution layer for Spacemacs

![logo_clojure](img/clojure.png) ![logo_cider](img/cider.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Clojure contribution layer for Spacemacs](#clojure-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Packages Included](#packages-included)
    - [Install](#install)
        - [Layer](#layer)
        - [Pretty Symbols](#pretty-symbols)
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
- [clojure-mode][]
- [Clj Refactor][]
- [Cider Auto Complete][]
- [align-cljlet][]

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(clojure))
```

### Pretty Symbols

Pretty symbols for anonymous functions, set literals and partial, like `(λ [a] (+ a 5))`, `ƒ(+ % 5)`, `∈{2 4 6}` and `Ƥ`.

To enable this feature, add the following snippet to the dotspacemacs/config
section of your `~/.spacemacs` file:

```elisp
(setq clojure-enable-fancify-symbols t)
```

Or set this variable when loading the configuration layer:
```elisp
(setq-default dotspacemacs-configuration-layers
'((clojure :variables clojure-enable-fancify-symbols t)))
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
<kbd>SPC m g g</kbd>  | goto var
<kbd>SPC m g e</kbd>  | goto error
<kbd>SPC m g r</kbd>  | goto resource
<kbd>SPC m g b</kbd>  | go back

### REPL

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m s b</kbd>  | send and eval buffer in REPL
<kbd>SPC m s B</kbd>  | send and eval buffer and switch to REPL in `insert state`
<kbd>SPC m s e</kbd>  | send and eval last sexp in REPL
<kbd>SPC m s E</kbd>  | send and eval last sexp and switch to REPL in `insert state`
<kbd>SPC m s f</kbd>  | send and eval function in REPL
<kbd>SPC m s F</kbd>  | send and eval function and switch to REPL in `insert state`
<kbd>SPC m s i</kbd>  | start REPL
<kbd>SPC m s n</kbd>  | send and eval ns form in REPL
<kbd>SPC m s N</kbd>  | send and eval ns form and switch to REPL in `insert state`
<kbd>SPC m s s</kbd>  | switch to REPL

### Tests

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m t t</kbd>  | run tests

### Refactoring

    Key Binding         |                 Description
------------------------|------------------------------------------------------------
<kbd>SPC m r a d</kbd>  | add declaration
<kbd>SPC m r a i</kbd>  | add import to ns
<kbd>SPC m r a m</kbd>  | add missing libspec
<kbd>SPC m r a p</kbd>  | add project dependency
<kbd>SPC m r a r</kbd>  | add require to ns
<kbd>SPC m r a u</kbd>  | add use to ns
<kbd>SPC m r c c</kbd>  | cycle coll
<kbd>SPC m r c i</kbd>  | cycle if
<kbd>SPC m r c n</kbd>  | clean ns
<kbd>SPC m r c p</kbd>  | cycle privacy
<kbd>SPC m r d k</kbd>  | destructure keys
<kbd>SPC m r e f</kbd>  | extract function
<kbd>SPC m r e l</kbd>  | expand let
<kbd>SPC m r f u</kbd>  | find usages
<kbd>SPC m r h d</kbd>  | hotload dependency
<kbd>SPC m r i l</kbd>  | introduce let
<kbd>SPC m r m f</kbd>  | move form
<kbd>SPC m r m l</kbd>  | move to let
<kbd>SPC m r p c</kbd>  | project clean
<kbd>SPC m r p f</kbd>  | promote function
<kbd>SPC m r r d</kbd>  | remove debug fns
<kbd>SPC m r r f</kbd>  | rename file
<kbd>SPC m r r l</kbd>  | remove let
<kbd>SPC m r r r</kbd>  | remove unused requires
<kbd>SPC m r r s</kbd>  | rename symbol
<kbd>SPC m r r u</kbd>  | replace use
<kbd>SPC m r s n</kbd>  | sort ns
<kbd>SPC m r s p</kbd>  | sort project dependencies
<kbd>SPC m r s r</kbd>  | stop referring
<kbd>SPC m r t f</kbd>  | thread first all
<kbd>SPC m r t h</kbd>  | thread
<kbd>SPC m r t l</kbd>  | thread last all
<kbd>SPC m r u a</kbd>  | unwind all
<kbd>SPC m r u w</kbd>  | unwind

[Clojure]: http://clojure.org
[Cider]: https://github.com/clojure-emacs/cider
[cider_install]: https://github.com/clojure-emacs/cider#installation
[clojure-mode]: https://github.com/clojure-emacs/clojure-mode
[Clj Refactor]: https://github.com/clojure-emacs/clj-refactor.el
[Cider Auto Complete]: https://github.com/clojure-emacs/ac-cider
[align-cljlet]: https://github.com/gstamp/align-cljlet
