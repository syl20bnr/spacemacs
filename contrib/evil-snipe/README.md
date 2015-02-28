# evil-snipe contribution layer for Spacemacs

![logo](img/Cat_With_Rifle.jpg)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [evil-snipe contribution layer for Spacemacs](#evil-snipe-contribution-layer-for-spacemacs)
    - [Description](#description)
        - [Improved f and t searches](#improved-f-and-t-searches)
        - [Improved precision search](#improved-precision-search)
    - [Install](#install)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description
The package [evil-snipe](https://github.com/hlissner/evil-snipe)
- enables more efficient searches with `f/F/t/T`.
- adds a new, more precise search with `s/S`

`evil-snipe` changes `s/S` behavior in order to search forward/backwards in the
buffer with two chars.

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(evil-snipe))
```

### Alternate behavior for `f/F` and `t/T`

This alternate behavior is disabled by default, to enable it set the
layer variable `evil-snipe-enable-alternate-f-and-t-behaviors` to `t`:

```elisp
(setq-default dotspacemacs-configuration-layers
  '(evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t ))
```

Instead of repeating searches with `,/;` you can just press `f/t` again to
continue the search (`F/T` to go the opposite direction). 

Evil-snipe also adds several scope options for searches (set
`evil-snipe-scope` and `evil-snipe-repeat-scope` to one of these, the default
value is `buffer`):

Value            | Description
-----------------|------------------------------------------------------------
buffer           | search in the rest of the buffer after the cursor (`vim-sneak` behavior)
line             | search in the current line after the cursor (`vim-seek` behavior)
visible          | search in the rest of the visible buffer only
whole-line       | same as `line`, but highlight matches on either side of cursor
whole-buffer     | same as `buffer`, but highlight *all* matches in buffer
whole-visible    | same as 'visible, but highlight *all* visible matches in buffer

If you do not want to replace the regular `f/F/t/T` behavior, just
remove this line from `evil-snipe/packages.el`:
`(evil-snipe-replace-evil)`

## Key bindings

TODO
