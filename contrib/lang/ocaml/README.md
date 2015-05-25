# Ocaml contribution layer for Spacemacs

![logo](img/ocaml.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Ocaml contribution layer for Spacemacs](#ocaml-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [OPAM packages](#opam-packages)
    - [Key Bindings](#key-bindings)
        - [REPL (utop)](#repl-utop)
    - [TODO](#todo)

<!-- markdown-toc end -->

## Description

This is a very basic layer for editing ocaml files.

- Syntax highlighting (major-mode) via [tuareg-mode][]
- Error reporting, completion and type display via [merlin][]
- auto-completion with company mode via [merlin][]
- syntax-checking via [flycheck-ocaml][]

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(ocaml))
```

### OPAM packages

This layer requires some [opam](http://opam.ocaml.org) packages:

- `merlin` for auto-completion
- `utop`
- `ocp-indent`

To install them, use the following command: 

```sh
opam install merlin utop ocp-indent
```

## Key Bindings

    Key Binding       |                 Description
----------------------|--------------------------------------------------------
<kbd>SPC m c c</kbd>  | Compile
<kbd>SPC m c p</kbd>  | Check .merlin for errors
<kbd>SPC m c r</kbd>  | Refresh changed .cmis in merlin
<kbd>SPC m e C</kbd>  | Check for errors in current buffer
<kbd>SPC m e n</kbd>  | Jump to next error
<kbd>SPC m e N</kbd>  | Jump back to previous error
<kbd>SPC m g a</kbd>  | Switch ML <-> MLI
<kbd>SPC m g b</kbd>  | Go back to the last position where the user did a locate
<kbd>SPC m g g</kbd>  | Locate the identifier under point (same window)
<kbd>SPC m g G</kbd>  | Locate the identifier under point (different window)
<kbd>SPC m g l</kbd>  | Prompt for identifier and locate
<kbd>SPC m g i</kbd>  | Prompt for module name and switch to ML file
<kbd>SPC m g I</kbd>  | Prompt for module name and switch to MLI file
<kbd>SPC m h h</kbd>  | Document the identifier under point
<kbd>SPC m h t</kbd>  | Highlight identifier under cursor and print its type
<kbd>SPC m h T</kbd>  | Prompt for expression and show its type
<kbd>SPC m r d</kbd>  | Case analyze the current enclosing

### REPL (utop)

    Key Binding       |                 Description
----------------------|--------------------------------------------------------
<kbd>SPC m s b</kbd>  | Send buffer to the REPL
<kbd>SPC m s B</kbd>  | Send buffer to the REPL and switch to the REPL in `insert state`
<kbd>SPC m s i</kbd>  | Start a REPL
<kbd>SPC m s p</kbd>  | Send phrase to the REPL
<kbd>SPC m s P</kbd>  | Send phrase to the REPL and switch to the REPL in `insert state`
<kbd>SPC m s r</kbd>  | Send region to the REPL
<kbd>SPC m s R</kbd>  | Send region to the REPL and switch to the REPL in `insert state`
<kbd>C-j</kbd>        | (in REPL) next item in history
<kbd>C-k</kbd>        | (in REPL) previous item in history

## TODO

- Add more proper spacemacs key-bindings for basic merlin tasks
- Add proper keybindings for ocamldebug
- Add more keybindings for tuareg-mode

[tuareg-mode]: https://github.com/ocaml/tuareg
[merlin]: https://github.com/the-lambda-church/merlin
[flycheck-ocaml]: https://github.com/diml/utop
