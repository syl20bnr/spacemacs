# Puppet contribution layer for Spacemacs

![logo](img/puppet.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Puppet contribution layer for Spacemacs](#puppet-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer aims at providing support for the Puppet DSL using [puppet-mode][].

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(puppet)
  "List of contribution to load."
)
```

## Key bindings

The following key bindings are available in Puppet Mode:

Key Binding | Description
------------|--------------------------------------------------
`<SPC> m {` | Move to the beginning of the current block
`<SPC> m }` | Move to the end of the current block
`<SPC> m a` | Align parameters in the current block
`<SPC> m '` | Toggle string quoting between single and double
`<SPC> m ;` | Blank the string at point
`<SPC> m j` | Jump to a `class`, `define`, variable or resource
`<SPC> m c` | Apply the current manifest in dry-run mode
`<SPC> m v` | Validate the syntax of the current manifest
`<SPC> m l` | Check the current manifest for semantic issues
`<SPC> m $` | Interpolate with ${} in double quoted strings


Use `M-x customize-group RET puppet` to customize Puppet Mode.

[puppet-mode]: https://github.com/lunaryorn/puppet-mode
