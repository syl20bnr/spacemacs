# Agda contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Agda contribution layer for Spacemacs](#agda-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [Agda](#agda)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer adds support for the [Agda][] programming language.

Some features:
- Faces redefined to correctly play with themes.
- Spacemacs bindings to Agda's interactive tools.

**This layer is in construction, it needs your contributions and bug reports.**

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(agda))
```

### Agda

Quick instructions to install Agda assuming you have cabal installed:

```sh
cabal install alex happy "cpphs < 1.19" agda
```

Then check that `agda` is available on your `PATH` and seen by Emacs.

## Key bindings

The key bindings of this layer don't follow the Spacemacs conventions,
we opted to a simple transcription of stock Agda mode key bindings to
Spacemacs leader key.

All Agda specific bindings are prefixed with the major-mode leader
<kbd>SPC m</kbd>.

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m =</kbd>    | Show constraints.
<kbd>SPC m ?</kbd>    | Show all goals.
<kbd>SPC m ,</kbd>    | Shows the type of the goal at point and the currect context.
<kbd>SPC m .</kbd>    | Shows the context, the goal and the given expression's inferred type.
<kbd>SPC m a</kbd>    | Simple proof search.
<kbd>SPC m b</kbd>    | Go to the previous goal, if any and activate goal-navigation micro-state.
<kbd>SPC m c</kbd>    | Refine the pattern variables given in the goal.
<kbd>SPC m d</kbd>    | Infers the type of the given expression.
<kbd>SPC m e</kbd>    | Show the context of the goal at point.
<kbd>SPC m f</kbd>    | Go to the next goal, if any and activate goal-navigation micro-state.
<kbd>SPC m h</kbd>    | Compute the type of a hypothetical helper function.
<kbd>SPC m l</kbd>    | Load current buffer.
<kbd>SPC m n</kbd>    | Computes the normal form of the given expression, using the scope of the current goal or, if point is not in a goal, the top-level scope.
<kbd>SPC m o</kbd>    | Shows all the top-level names in the given module.
<kbd>SPC m r</kbd>    | Refine the goal at point.
<kbd>SPC m s</kbd>    | Solves all goals that are already instantiated internally.
<kbd>SPC m t</kbd>    | Show the type of the goal at point.
<kbd>SPC m x c</kbd>  | Compile current module.
<kbd>SPC m x d</kbd>  | Removes buffer annotations (overlays and text properties).
<kbd>SPC m x h</kbd>  | Toggle display of implicit arguments.
<kbd>SPC m x q</kbd>  | Quit and clean up after agda2.
<kbd>SPC m x r</kbd>  | Kill and restart the *agda2* buffer and load `agda2-toplevel-module'.
<kbd>SPC m w</kbd>    | Explains why a given name is in scope.

[Agda]: http://wiki.portal.chalmers.se/agda/pmwiki.php
