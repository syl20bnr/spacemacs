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
<kbd>SPC m e t</kbd>  | Highlight identifier under cursor and print its type

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
