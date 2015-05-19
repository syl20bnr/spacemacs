# Idris contribution layer for Spacemacs

![logo](img/idris.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Idris contribution layer for Spacemacs](#idris-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [Idris](#idris)
    - [Key bindings](#key-bindings)
        - [Shorthands](#shorthands)
        - [Interactive editing](#interactive-editing)
        - [Documentation](#documentation)
        - [File loading](#file-loading)
        - [Active term manipulations](#active-term-manipulations)
        - [Build system](#build-system)
        - [REPL](#repl)

<!-- markdown-toc end -->

## Description

This layer adds support for the [Idris][] language.

*This layer is in construction, it needs your contributions and bug reports.*

## Install

### Layer

To use this layer, add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(idris))
```

### Idris

Idris can be installed using Haskell's cabal:

```sh
cabal install idris
```

Binaries are also available for some platforms at
http://www.idris-lang.org/download/

## Key bindings

All Idris specific bindings are prefixed with the major-mode leader <kbd>SPC
m</kbd>.

### Shorthands

Several (but not all) of the evil-leader shorthands that `idris-mode` provides
are reproduced under the local leader.

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC m r</kbd>   | Load current buffer into Idris.
<kbd>SPC m t</kbd>   | Get the type for the identifier under point.
<kbd>SPC m d</kbd>   | Create an initial pattern match clause for a type declaration.
<kbd>SPC m c</kbd>   | Case split the pattern variable under point.
<kbd>SPC m w</kbd>   | Add a with block for the pattern-match clause under point.
<kbd>SPC m p</kbd>   | Attempt to solve a metavariable automatically.

### Interactive editing

Interactive editing commands are prefixed by <kbd>SPC m i</kbd>.

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC m i s</kbd> | Create an initial pattern match clause for a type declaration.
<kbd>SPC m i m</kbd> | Add missing pattern-match cases to an existing definition.
<kbd>SPC m i a</kbd> | Attempt to solve a metavariable automatically.
<kbd>SPC m i e</kbd> | Extract a metavariable or provisional definition name to an explicit top level definition.
<kbd>SPC m i c</kbd> | Case split the pattern variable under point.
<kbd>SPC m i w</kbd> | Add a with block for the pattern-match clause under point.
<kbd>SPC m i r</kbd> | Refine by name, without recursive proof search.

### Documentation

Documentation commands are prefixed by <kbd>SPC m h</kbd>.

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC m h d</kbd> | Search the documentation for the name under point.
<kbd>SPC m h a</kbd> | Search the documentation for a string.
<kbd>SPC m h s</kbd> | Search the documentation regarding a particular type.
<kbd>SPC m h t</kbd> | Get the type for the identifier under point.

### File loading

File loading commands are prefixed by <kbd>SPC m l</kbd>.

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC m l b</kbd> | Load the current buffer into Idris.
<kbd>SPC m l n</kbd> | Extend the region to be loaded, if such a region exists, one line at a time.
<kbd>SPC m l p</kbd> | Contract the region to be loaded, if such a region exists, one line at a time.
<kbd>SPC m l N</kbd> | Contract the region to be loaded, if such a region exists, one line at a time.

### Active term manipulations

Active term manipulations are prefixed by <kbd>SPC m m</kbd>.

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC m m n</kbd> | Normalize the term at point.
<kbd>SPC m m i</kbd> | Show implicits for the term at point.
<kbd>SPC m m h</kbd> | Hide implicits for the term at point.
<kbd>SPC m m c</kbd> | Show the core language for the term at point.

### Build system

Commands for `ipkg` are prefixed by <kbd>SPC m b</kbd>.

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC m b c</kbd> | Clean the package, removing `.ibc` files.`
<kbd>SPC m b b</kbd> | Build the package.
<kbd>SPC m b i</kbd> | Install the package to the user's repository, building first if necessary.

When inside a package file, you can insert a field with <kbd>SPC m f</kbd>.

### REPL

You can pop to the corresponding Idris REPL with <kbd>SPC m s</kbd>.

[Idris]: http://www.idris-lang.org/
