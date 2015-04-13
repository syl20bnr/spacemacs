# Shell Scripting contribution layer for Spacemacs

![logo](img/fish.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Shell Scripting contribution layer for Spacemacs](#shell-scripting-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key Bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This simple layer adds support for shell scripting.

Incuded packages for extensions:
- `.fish`: [fish shell][]

**Note** For Windows scripting see the layer `windows-scripts`

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(shell-scripts))
```

## Key Bindings

None for now.

[fish]: https://github.com/fish-shell/fish-shell
