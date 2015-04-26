# org-present contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [org-present contribution layer for Spacemacs](#org-present-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

Adds a presentation mode to your orgmode file.

See <https://github.com/rlister/org-present> for details.

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(org-present))
```

## Key bindings

Run `SPC : org-present` to switch the current OrgMode buffer into presentation mode.

Then, you can use the following keys:

Key Binding   | Description
--------------|------------------------------------------------------------
`h`           | org-present-prev
`l`           | org-present-next
`q`           | org-present-quit
