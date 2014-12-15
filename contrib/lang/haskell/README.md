# Haskell contribution layer for Spacemacs

![logo](haskell.png)

This layer adds support for the [Haskell][] language.
The layer uses company-ghc for completetion.

**This layer is still not fully adapted for Spacemacs, it needs you, Haskell experts, to
improve it and make it consistent with the Spacemacs experience.**

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(haskell)
  "List of contribution to load."
)
```

## Key bindings

All Haskell specific bindings starts with `m`:

### Haskell source code:

    Key Binding            |                 Description
---------------------------|------------------------------------------------------------
<kbd> SPC m C </kbd>     | compile the current project
<kbd> SPC m l </kbd>     | loads or reloads the current file into the REPL
<kbd> SPC m t </kbd>     | gets the type of the identifier under the cursor
<kbd> SPC m i </kbd>     | gets information for the identifier under the cursor
<kbd> SPC m b </kbd>     | build the current cabal project 
<kbd> SPC m c c</kbd>    | cabal actions
<kbd> SPC m c v</kbd>    | visit the cabal file
<kbd> SPC m `</kbd>     | show the REPL
<kbd> SPC m k </kbd>     | clear the REPL
<kbd> SPC m z </kbd>     | focus the REPL
<kbd> SPC m j </kbd>     | jump to definition or tag
<kbd> SPC m d </kbd>     | find or generate documentation for the identifier under the cursor
<kbd> SPC m h </kbd>     | do a Hoogle lookup
<kbd> SPC m H </kbd>     | do a Hayoo lookup

### Cabal file:

    Key Binding            |                 Description
---------------------------|------------------------------------------------------------
<kbd>SPC m d</kbd>       | add a dependency to the project
<kbd>SPC m b</kbd>       | go to benchmark section
<kbd>SPC m e</kbd>       | go to executable section
<kbd>SPC m t</kbd>       | go to test-suite section
<kbd>SPC m m</kbd>       | go to exposed modules
<kbd>SPC m l</kbd>       | go to libary section
<kbd>SPC m n</kbd>       | go to next subsection
<kbd>SPC m p</kbd>       | go to previous subsection
<kbd>SPC m N</kbd>       | go to next section
<kbd>SPC m P</kbd>       | go to previous section
<kbd>SPC m f</kbd>       | find or create source-file under the cursor

**TODO**

[Haskell]: https://www.haskell.org/
