# evil-lisp-state
[![MELPA](http://melpa.org/packages/evil-lisp-state-badge.svg)](http://melpa.org/#/evil-lisp-state)

Adds a new [evil][evil-link] state to navigate lisp code and edit sexp trees
using mnemonic key bindings.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [evil-lisp-state](#evil-lisp-state)
    - [Install](#install)
        - [Package manager](#package-manager)
        - [Manually](#manually)
    - [Principle](#principle)
    - [Commands and key bindings](#commands-and-key-bindings)
        - [hjkl](#hjkl)
        - [Other commands:](#other-commands)
    - [Configuration](#configuration)

<!-- markdown-toc end -->

## Install

### Package manager

You can either install `evil-lisp-state` from [MELPA][melpa-link]:

```
 M-x package-install evil-list-state
```

Or add it to your `Cask` file:

```elisp
(source melpa)

(depends-on "evil-lisp-state")
```

### Manually

Add `evil-lisp-state.el` to your load path. `evil-lisp-state` requires
both `evil`, `evil-leader` and `smartparens` to be installed.

## Principle

To execute a command while in normal state, the evil-leader is used.
By default, the prefix for each command is `<leader> m`.
Each command when executed set the current state to `lisp state`.

Examples:

- to slurp three times while in normal state:

    <leader> m s s s

- to wrap a symbol in parenthesis then slurping two times:

    <leader> m w s s

## Commands and key bindings

### hjkl

Evil Lisp state binds the most common commands on hjkl:

Key Binding                | Function
---------------------------|------------------------------------------------------------
<kbd>\<leader\> m h</kbd>  | previous symbol
<kbd>\<leader\> m H</kbd>  | forward barf sexp (move the current symbol or sexp outside)
<kbd>\<leader\> m j</kbd>  | next closing parenthesis
<kbd>\<leader\> m J</kbd>  | wrap symbol with parenthesis (down one level)
<kbd>\<leader\> m k</kbd>  | previous opening parenthesis
<kbd>\<leader\> m K</kbd>  | unwrap current sexp (up one level)
<kbd>\<leader\> m l</kbd>  | next symbol
<kbd>\<leader\> m L</kbd>  | forward slurp sexp (move next outside sexp into current one)

So with just hjkl keys you can:
- navigate between symbols and sexps
- slurp and barf symbols and sexps
- wrap and unwrap symbols and sexps

**Notes:**
Slurping, barfing and wrapping are also bound on other keys.

### Other commands:

Key Binding                 | Function
----------------------------|------------------------------------------------------------
<kbd>\<leader\> m (</kbd>   | insert expression before (same level as current one)
<kbd>\<leader\> m )</kbd>   | insert expression after (same level as current one)
<kbd>\<leader\> m a</kbd>   | absorb expression
<kbd>\<leader\> m b</kbd>   | forward barf expression
<kbd>\<leader\> m B</kbd>   | backward barf expression
<kbd>\<leader\> m c</kbd>   | convolute expression
<kbd>\<leader\> m i</kbd>   | switch to `insert state`
<kbd>\<leader\> m I</kbd>   | go to beginning of current expression and switch to `insert state`
<kbd>\<leader\> m m</kbd>   | merge (join) expression
<kbd>\<leader\> m n</kbd>   | forwared slurp expression
<kbd>\<leader\> m N</kbd>   | backward slurp expression
<kbd>\<leader\> m p</kbd>   | paste after
<kbd>\<leader\> m P</kbd>   | paste before
<kbd>\<leader\> m q</kbd>   | unwrap current expression and kill all symbols after point
<kbd>\<leader\> m Q</kbd>   | unwrap current expression and kill all symbols before point
<kbd>\<leader\> m r</kbd>   | raise expression (replace parent expression by current one)
<kbd>\<leader\> m T</kbd>   | transpose expression
<kbd>\<leader\> m u</kbd>   | undo
<kbd>\<leader\> m C-r</kbd> | redo
<kbd>\<leader\> m v</kbd>   | switch to `visual state`
<kbd>\<leader\> m V</kbd>   | switch to `visual line state`
<kbd>\<leader\> m C-v</kbd> | switch to `visual block state`
<kbd>\<leader\> m w</kbd>   | wrap expression with parenthesis
<kbd>\<leader\> m W</kbd>   | unwrap expression
<kbd>\<leader\> m xs</kbd>  | delete symbol
<kbd>\<leader\> m xw</kbd>  | delete word
<kbd>\<leader\> m xx</kbd>  | delete expression
<kbd>\<leader\> m y</kbd>   | copy expression

## Configuration

Key bindings are set only for `emacs-lisp-mode` by default.
It is possible to add major modes with the variable
`evil-lisp-state-major-modes`.

The prefix key is `<leader> m` by default, it is possible to
change the `m` key to anything else with the variable
`evil-lisp-state-leader-prefix`. Set it to an empty string
if you want all the commands to be directly available
under the `<leader>` key.

[evil-link]: https://gitorious.org/evil/pages/Home
[smartparens-link]: https://github.com/Fuco1/smartparens/wiki
[melpa-link]: http://melpa.org/
