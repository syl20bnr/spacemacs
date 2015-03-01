# Ace-window contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Ace-window contribution layer for Spacemacs](#ace-window-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer adds support for [ace-window][].

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(ace-window))
```

## Key bindings

Key Binding           | Description
----------------------|------------------------------------------------------------
<kbd>SPC b m m</kbd>  | swap a buffer with another
<kbd>SPC w c</kbd>    | delete a window (replace `delete-window`)
<kbd>SPC w m</kbd>    | maximize a window (replace `toggle-maximize-buffer`)
<kbd>SPC w w</kbd>    | switch to a window (replace `other-window`)

[ace-window]: https://github.com/abo-abo/ace-window
