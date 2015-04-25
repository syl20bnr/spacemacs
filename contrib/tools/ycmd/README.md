# YCMD contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [YCMD contribution layer for Spacemacs](#ycmd-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [YCMD](#ycmd)
        - [Company](#company)
    - [Configuration](#configuration)
    - [Key Bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer adds [emacs-ycmd][] support.

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(ycmd))
```

### YCMD

In order to use this layer you must have a local [ycmd][] installation and must
set the `ycmd-server-command` variable to reflect the path to that installation.
See the [emacs-ycmd][] readme for more instructions on this.

### Company

This package also requires the `company-mode` layer in order to get actual
completion rather than just flychecking and key bindings.

## Configuration

By default this layer only activates ycmd for `c++-mode`.

If you want ycmd suppoert in other modes you might just want to add it for
specific languages like: ```elisp (add-hook 'c++-mode-hook 'ycmd-mode) ```

## Key Bindings

Adds `SPC m g g` go to definition binding to `c++-mode` as well as `SPC m g G`
for the more imprecise but faster version.

[ycmd]: https://github.com/Valloric/ycmd#building
[emacs-ycmd]: https://github.com/abingham/emacs-ycmd
