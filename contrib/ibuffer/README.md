# ibuffer contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [ibuffer contribution layer for Spacemacs](#ibuffer-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [Grouping buffers](#grouping-buffers)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer configures Emacs ibuffer for Spacemacs.

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(ibuffer))
```

### Grouping buffers

Buffers can be grouped by major-modes or projectile projects.
By default buffers are grouped by major-modes.

To change how buffers are grouped set the layer variable
`ibuffer-group-buffers-by` to one of the following supported
values:
- `modes` to group buffers by major-modes (default)
- `projects` to group buffers by projectile projects
- `nil` to not group buffers

Example:

```elisp
(setq-default dotspacemacs-configuration-layers '(
  (ibuffer :variables ibuffer-group-buffers-by 'projects)))
```

## Key bindings

Key Binding         | Description
--------------------|------------------------------------------------------------
<kbd>SPC b B</kbd>  | open ibuffer menu

**Note** The layer will also replace regular <kbd>C-x C-b</kbd> by ibuffer.
