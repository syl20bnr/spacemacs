# dockerfile contribution layer for Spacemacs

![logo](img/dockerfile.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [dockerfile contribution layer for Spacemacs](#dockerfile-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer provides syntax highlighting and build functionality for Docker files

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(dockerfile))
```

## Key bindings

Key Binding   | Description
--------------|------------------------------------------------------------
`<SPC> m c b` | build current buffer via `dockerfile-build-buffer`
