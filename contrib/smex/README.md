# smex contribution layer for Spacemacs

![logo](img/smex.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [smex contribution layer for Spacemacs](#smex-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer replaces `helm-M-x` by [smex][] which is built on top of `ido`.
`ido` can perform flex matching with the [flx-ido][] mode which is already
activated in the Spacemacs layer.

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(smex))
```

## Key bindings

    Key Binding     |                 Description
--------------------|------------------------------------------------------------
<kbd>SPC :</kbd>    | all Emacs commands (interactive functions)
<kbd>SPC m :</kbd>  | current major mode commands 

[smex]: https://github.com/nonsequitur/smex
[flx-ido]: https://github.com/lewang/flx
