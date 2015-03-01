# Ace-window contribution layer for Spacemacs


<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Ace-window contribution layer for Spacemacs](#ace-window-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer adds support for [ace-window](https://github.com/abo-abo/ace-window) with a function for [ace-delete-window](https://github.com/abo-abo/ace-window/releases/tag/0.7.0). 

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(ace-window))
```

## Key bindings

Key Binding   | Description
--------------|------------------------------------------------------------
`C-x o`       | run ace-window
`<SPC> w w`   | run ace-window
`<SPC> w C`   | run ace-delete-window

