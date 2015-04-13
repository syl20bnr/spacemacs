# Clojure contribution layer for Spacemacs

![logo_clojure](img/clojure.png) ![logo_cider](img/cider.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Clojure contribution layer for Spacemacs](#clojure-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Features](#features)
    - [Install](#install)
        - [Layer](#layer)
        - [Pretty Symbols](#pretty-symbols)
        - [Cider and clj-refactor](#cider-and-clj-refactor)
            - [Quick Start with lein](#quick-start-with-lein)
            - [More details](#more-details)
    - [Key Bindings](#key-bindings)
        - [Documentation](#documentation)
        - [Evaluation](#evaluation)
        - [Goto](#goto)
        - [REPL](#repl)
        - [Tests](#tests)
        - [Refactoring](#refactoring)
        - [Reformatting](#reformatting)

<!-- markdown-toc end -->

## Description

This layer adds support for [Clojure][] language using [Cider][].

## Features

- REPL via [cider][]
- Refactoring via [clj-refactor][]
- Auto completion via [ac-cider][]
- Automatic formatting via [align-cljlet][]

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

### Cider and clj-refactor

#### Quick Start with lein

- Install `lein` via your OS package manager.
- Create a file `~/.lein/profiles.clj` with the following content:

```clj
{:user {:plugins [[cider/cider-nrepl "0.9.0-SNAPSHOT"]
                  [refactor-nrepl "0.3.0-SNAPSHOT"]]
        :dependencies [[alembic "0.3.2"]
                       [org.clojure/tools.nrepl "0.2.7"]]}}
```

#### More details

More info regarding installation of nREPL middleware can be found here:
- cider: [cider repo][cider_install]
- clj-refactor: [refactor-nrepl][]

## Key Bindings

### Documentation

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m h h</kbd>  | cider doc
<kbd>SPC m h g</kbd>  | cider grimoire
<kbd>SPC m h j</kbd>  | cider javadoc

### Evaluation

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m e b</kbd>  | eval buffer
<kbd>SPC m e e</kbd>  | eval last sexp
<kbd>SPC m e f</kbd>  | eval function at point
<kbd>SPC m e r</kbd>  | eval region

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
<kbd>SPC m s i</kbd>  | start REPL (cider-jack-in)
<kbd>SPC m s n</kbd>  | send and eval ns form in REPL
<kbd>SPC m s N</kbd>  | send and eval ns form and switch to REPL in `insert state`
<kbd>SPC m s q</kbd>  | kill REPL (cider-quit)
<kbd>SPC m s r</kbd>  | send and eval region in REPL
<kbd>SPC m s R</kbd>  | send and eval region and switch to REPL in `insert state`
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

### Reformatting

Forms currently handled:
- let
- when-let
- if-let
- binding
- loop
- with-open
- literal hashes {}
- defroute
- cond
- condp (except :>> subforms)

More info [here][align-cljlet].

    Key Binding         |                 Description
------------------------|------------------------------------------------------------
<kbd>SPC m f l</kbd>      | reformat current form

[Clojure]: http://clojure.org
[Cider]: https://github.com/clojure-emacs/cider
[cider_install]: https://github.com/clojure-emacs/cider#installation
[clojure-mode]: https://github.com/clojure-emacs/clojure-mode
[clj-refactor]: https://github.com/clojure-emacs/clj-refactor.el
[ac-cider]: https://github.com/clojure-emacs/ac-cider
[align-cljlet]: https://github.com/gstamp/align-cljlet
[refactor-nrepl]: https://github.com/clojure-emacs/refactor-nrepl
