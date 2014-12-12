# smex contribution layer for Spacemacs

![Smex](smex.png)

This layer replaces `helm-M-x` by [smex][] which is built on top of `ido`.
`ido` can perform flex matching with the [flx-ido][] mode which is already
activated in the Spacemacs layer.

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(smex)
  "List of contribution to load."
)
```

## Key bindings

    Key Binding     |                 Description
--------------------|------------------------------------------------------------
<kbd>SPC :</kbd>    | all Emacs commands (interactive functions)
<kbd>SPC m :</kbd>  | current major mode commands 

## `jk` on leader

The experimental feature [evil leader on `jk`][jk] is supported in this layer.
While the smex minibuffer is opened press quickly <kbd>jk</kbd> then:

    Key Binding     |                 Description
--------------------|------------------------------------------------------------
<kbd>g</kbd>        | go to the definition of current command
<kbd>h d f</kbd>    | describe the current command
<kbd>h w</kbd>      | show the key binding for the current command

[smex]: https://github.com/nonsequitur/smex
[flx-ido]: https://github.com/lewang/flx
[jk]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#jk-to-trigger-evil-leader
