# Agda contribution layer for Spacemacs

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

This layer requires Agda to be installed, and available on the `PATH` as seen by Emacs.

## Key bindings

All Agda specific bindings are prefixed with the major-mode leader
<kbd>SPC m</kbd>.

Top-level commands are prefixed by <kbd>SPC m</kbd>:

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m l</kbd>    | Load current buffer.
<kbd>SPC m a</kbd>    | Simple proof search.
<kbd>SPC m r</kbd>    | Refine the goal at point.
<kbd>SPC m s</kbd>    | Solves all goals that are already instantiated internally.
<kbd>SPC m t</kbd>    | Show the type of the goal at point.
<kbd>SPC m c</kbd>    | Refine the pattern variables given in the goal.
<kbd>SPC m f</kbd>    | Go to the next goal, if any.
<kbd>SPC m ?</kbd>    | Show all goals.
<kbd>SPC m e</kbd>    | Show the context of the goal at point.
<kbd>SPC m b</kbd>    | Go to the previous goal, if any.
<kbd>SPC m ,</kbd>    | Shows the type of the goal at point and the currect context.
<kbd>SPC m =</kbd>    | Show constraints.
<kbd>SPC m h</kbd>    | Compute the type of a hypothetical helper function.
<kbd>SPC m d</kbd>    | Infers the type of the given expression.
<kbd>SPC m w</kbd>    | Explains why a given name is in scope.
<kbd>SPC m .</kbd>    | Shows the context, the goal and the given expression's inferred type.
<kbd>SPC m o</kbd>    | Shows all the top-level names in the given module.
<kbd>SPC m n</kbd>    | Computes the normal form of the given expression, using the scope of the current goal or, if point is not in a goal, the top-level scope.


[Agda]: http://wiki.portal.chalmers.se/agda/pmwiki.php
