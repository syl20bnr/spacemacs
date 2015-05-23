# Emacs Lisp contribution layer for Spacemacs

![logo](img/emacs-lisp.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Emacs Lisp contribution layer for Spacemacs](#emacs-lisp-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)
        - [srefactor](#srefactor)

<!-- markdown-toc end -->

## Description

This layer gathers all the configuration related to emacs-lisp. This should
always be in your dotfile, it is not recommended to uninstall it.

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(emacs-lisp))
```

## Key bindings

Key Binding                                  | Description
---------------------------------------------|-------------------------------------------
<kbd>SPC m g g</kbd>                         | go to definition of symbol under point
<kbd>SPC m h h</kbd>                         | describe symbol at point
<kbd>SPC m e $</kbd> or <kbd>SPC m e l</kbd> | go to end of current line and evaluate
<kbd>SPC m e b</kbd>                         | evaluate current buffer
<kbd>SPC m e c</kbd>                         | evaluate current form (start with `defun`, `setq`, etc...)
<kbd>SPC m e e</kbd>                         | evaluate sexp before point
<kbd>SPC m e r</kbd>                         | evaluate current region
<kbd>SPC m e f</kbd>                         | evaluation current function
<kbd>SPC m ,</kbd>                           | toggle `lisp state`
<kbd>SPC m t b</kbd>                         | run tests of current buffer
<kbd>SPC m t q</kbd>                         | run `ert`
<kbd>SPC m d m</kbd>                         | open [macrostep][] micro-state

### srefactor

The [semantic layer][semantic-layer] should be installed for these key bindings
to become active.

Key Binding          | Description
---------------------|------------------------------------------------------------
<kbd>SPC m = b</kbd> | format current buffer
<kbd>SPC m = f</kbd> | format current function
<kbd>SPC m = o</kbd> | format all on one line
<kbd>SPC m = s</kbd> | format current sexp

[macrostep]: https://github.com/joddie/macrostep
[semantic-layer]: ../../semantic/README.md
