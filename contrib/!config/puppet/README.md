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
(setq-default dotspacemacs-configuration-layers '(puppet))
```

## Key bindings

The following key bindings are available in Puppet Mode:

Key Binding         | Description
--------------------|--------------------------------------------------
<kbd>SPC m {</kbd> | Move to the beginning of the current block
<kbd>SPC m }</kbd> | Move to the end of the current block
<kbd>SPC m a</kbd> | Align parameters in the current block
<kbd>SPC m '</kbd> | Toggle string quoting between single and double
<kbd>SPC m ;</kbd> | Blank the string at point
<kbd>SPC m j</kbd> | Jump to a `class`, `define`, variable or resource
<kbd>SPC m c</kbd> | Apply the current manifest in dry-run mode
<kbd>SPC m v</kbd> | Validate the syntax of the current manifest
<kbd>SPC m l</kbd> | Check the current manifest for semantic issues
<kbd>SPC m $</kbd> | Interpolate with ${} in double quoted strings

Use `M-x customize-group RET puppet` to customize Puppet Mode.

[puppet-mode]: https://github.com/lunaryorn/puppet-mode
