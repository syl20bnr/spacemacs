# Haskell contribution layer for Spacemacs

![logo](img/haskell.png)

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

This layer requires some external binaries to function.
These are listed below, and can all be installed with `cabal install`:
- `ghc-mod`
- `hlint`
- `stylish-haskell`

Next Emacs needs to know of the directory of the binaries `~/.cabal/bin/` (or where ever cabal installs to by default on your system), which means that the directory has to be either added to your systems' `$PATH` or to Emacs' own `exec-path`.

Only one of the options below is needed:

- Add it to `$PATH`:
`export PATH=~/.cabal/bin/:$PATH`
Sidenote: emacs.app for OS X does not pick up `$PATH` from `~/.bashrc` or `~/.zshrc` when launched from outside a terminal.

- Add it to Emacs, add the following to `.spacemacs` in `dotspacemacs/config`:
`(add-to-list 'exec-path "~/.cabal/bin/")`

## Key bindings

All Haskell specific bindings are prefixed with <kbd>SPC m</kbd>

### Haskell source code:

#### Haskell commands:
Top-level commands are prefixed by <kbd>SPC m</kbd>:

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m t</kbd>    | gets the type of the identifier under the cursor
<kbd>SPC m i</kbd>    | gets information for the identifier under the cursor
<kbd>SPC m u</kbd>    | finds uses of identifier
<kbd>SPC m g</kbd>    | go to definition or tag
<kbd>SPC m f</kbd>    | format buffer using haskell-stylish

#### Documentation commands:
Documentation commands are prefixed by <kbd>SPC m h</kbd>

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m h d</kbd>  | find or generate Haddock documentation for the identifier under the cursor
<kbd>SPC m h h</kbd>  | do a Hoogle lookup
<kbd>SPC m h y</kbd>  | do a Hayoo lookup


#### Cabal commands:
Cabal commands are prefixed by <kbd>SPC m c</kbd>:

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m c a</kbd>  | cabal actions
<kbd>SPC m c b</kbd>  | build the current cabal project, i.e. invoke `cabal build`
<kbd>SPC m c c</kbd>  | compile the current project, i.e. invoke `ghc`
<kbd>SPC m c v</kbd>  | visit the cabal file

#### Debug commands:
Debug commands are prefixed by <kbd>SPC m d</kbd>:

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m d d </kbd> | start debug process, needs to be run first
<kbd>SPC m d b </kbd> | insert breakpoint at function
<kbd>SPC m d n </kbd> | next breakpoint
<kbd>SPC m d N </kbd> | previous breakpoint
<kbd>SPC m d B </kbd> | delete breakpoint
<kbd>SPC m d c </kbd> | continue current process
<kbd>SPC m d a </kbd> | abandon current process
<kbd>SPC m d r </kbd> | refresh process buffer

#### REPL commands:
REPL commands are prefixed by <kbd>SPC m s</kbd>:

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m s b</kbd>  | load or reload the current buffer into the REPL
<kbd>SPC m s c</kbd>  | clear the REPL
<kbd>SPC m s s</kbd>  | show the REPL
<kbd>SPC m s S</kbd>  | show and switch to the REPL

### Cabal files:

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m d</kbd>    | add a dependency to the project
<kbd>SPC m b</kbd>    | go to benchmark section
<kbd>SPC m e</kbd>    | go to executable section
<kbd>SPC m t</kbd>    | go to test-suite section
<kbd>SPC m m</kbd>    | go to exposed modules
<kbd>SPC m l</kbd>    | go to libary section
<kbd>SPC m n</kbd>    | go to next subsection
<kbd>SPC m p</kbd>    | go to previous subsection
<kbd>SPC m N</kbd>    | go to next section
<kbd>SPC m P</kbd>    | go to previous section
<kbd>SPC m f</kbd>    | find or create source-file under the cursor

**TODO**

[Haskell]: https://www.haskell.org/
