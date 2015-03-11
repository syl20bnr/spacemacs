# Haskell contribution layer for Spacemacs

![logo](img/haskell.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Haskell contribution layer for Spacemacs](#haskell-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [Cabal packages](#cabal-packages)
        - [OS X](#os-x)
        - [Optional extras](#optional-extras)
            - [GHCi-ng support](#ghci-ng-support)
            - [structured-haskell-mode](#structured-haskell-mode)
            - [hindent](#hindent)
    - [Key bindings](#key-bindings)
        - [Haskell source code:](#haskell-source-code)
            - [Haskell commands:](#haskell-commands)
            - [Documentation commands:](#documentation-commands)
            - [Cabal commands:](#cabal-commands)
            - [Debug commands:](#debug-commands)
            - [REPL commands:](#repl-commands)
        - [Cabal files:](#cabal-files)

<!-- markdown-toc end -->


## Description

This layer adds support for the [Haskell][] language.

Features:
- auto-completion with [company-ghc][],
- auto-indentation with [hi2][].

**This layer is in construction, it needs your contributions and bug reports.**

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(haskell))
```

### Cabal packages

This layer requires some [cabal][] packages:
- `ghc-mod`
- `hlint`
- `stylish-haskell`

To install them, use the following command: 

```sh
cabal install stylish-haskell hlint ghc-mod
```

Next Emacs needs to know where to find these binaries, you can locate them with
the following shell command:

```sh
dirname $(which ghc-mod)
```

Then you have to add this path to your system `$PATH` (preferred):

```sh
export PATH=~/.cabal/bin/:$PATH
```

_or_ to the Emacs `exec-path` variable in the `dotspacemacs/init` function of
your `.spacemacs` file:

```elisp
(add-to-list 'exec-path "~/.cabal/bin/")
```

**Note:** it is important to add the path in the `dotspacemacs/init` function,
so that the path is added before any layers is loaded.

### OS X

Note that `emacs.app` for OS X does not pick up `$PATH` from `~/.bashrc` or
`~/.zshrc` when launched from outside a terminal. If you want it to do so, install [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell) package.

### Optional extras
The Haskell layer supports some extra features that can be enabled through
layer variables.

#### GHCi-ng support
[ghci-ng][] adds some nice features to `haskell-mode`, and is supported in
Spacemacs by a layer variable:

Follow the instructions to install [ghci-ng][] (remember to add `:set +c`
in `~/.ghci`, next set the layer variable:
```elisp
;; List of configuration layers to load.
dotspacemacs-configuration-layers
'(company-mode
  git
  (haskell :variables haskell-enable-ghci-ng-support t))
```

Once ghci-ng is enabled, two of the old keybindings are overriden with improved
versions from ghci-ng, and a new keybinding available: 

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m t</kbd>    | gets the type of the identifier under the cursor or for the active region
<kbd>SPC m g g</kbd>  | go to definition
<kbd>SPC m u</kbd>    | finds uses of identifier

#### structured-haskell-mode
[structured-haskell-mode][], or shm, replaces hi2 (and any other
Haskell-indentation modes) and adds some nice functionality.
To enable shm, run `cabal install structured-haskell-mode` and set the layer
variable:
```elisp
;; List of configuration layers to load.
dotspacemacs-configuration-layers '(company-mode (haskell :variables haskell-enable-shm-support t) git)
```
After shm has been enabled, some of the evil normal-mode bindings are overridden:

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>D</kbd>          | `shm/kill-line`
<kbd>R</kbd>          | `shm/raise`
<kbd>P</kbd>          | `shm/yank`
<kbd>(</kbd>          | `shm/forward-node`
<kbd>)</kbd>          | `shm/backward-node`

For a nice visualization of these functions, please refer to the github page
for [structured-haskell-mode][].

#### hindent
[hindent]() is an extensible Haskell pretty printer, which let's you
reformat your code. You need to install the executable with `cabal
install hindent`; to enable it set:
```elisp
;; List of configuration layers to load.
dotspacemacs-configuration-layers
  '(company-mode
    git
    (haskell :variables haskell-enable-hindent-support t))
```
By default it uses the style called `fundamental`, if you want to use
another, `johan-tibell`, run `M-x customize-variable
hindent-style`.

## Key bindings

All Haskell specific bindings are prefixed with <kbd>SPC m</kbd>

### Haskell source code:

#### Haskell commands:
Top-level commands are prefixed by <kbd>SPC m</kbd>:

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m t</kbd>    | gets the type of the identifier under the cursor
<kbd>SPC m i</kbd>    | gets information for the identifier under the cursor
<kbd>SPC m g g</kbd>  | go to definition or tag
<kbd>SPC m f</kbd>    | format buffer using haskell-stylish
<kbd>SPC m F</kbd>    | format declaration using hindent (if enabled)

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

[Haskell]: https://www.haskell.org/
[cabal]: https://www.haskell.org/cabal/
[company-ghc]: https://github.com/iquiw/company-ghc
[hi2]: https://github.com/nilcons/hi2
[ghci-ng]: https://github.com/chrisdone/ghci-ng
[structured-haskell-mode]: https://github.com/chrisdone/structured-haskell-mode
[hindent]: https://github.com/chrisdone/hindent
