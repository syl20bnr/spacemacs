# ERC contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [ERC contribution layer for Spacemacs](#erc-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

Basic layer for ERC.

TBD:

- show mode-line tracking of active channels (so that you know when to <kbd>C-c C-SPC</kbd> or it's equivalent evil-ized shortcut)
- evil-ized keyboard shortcuts
- documentation on storing credentials and preferred channels list

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(erc))
```

## Key bindings

Key Binding   | Description
--------------|------------------------------------------------------------
`<SPC> a e`   | Starts ERC
