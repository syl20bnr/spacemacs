# Colors contribution layer for Spacemacs

![logo](rainbow_dash.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Colors contribution layer for Spacemacs](#colors-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)
        - [Rainbow Identifiers](#rainbow-identifiers)
    - [Screenshots](#screenshots)
        - [Python](#python)

<!-- markdown-toc end -->

## Description

This layer colors your life with:
- [rainbow identifiers][]

[rainbow identifiers][] mode will colorize each variables and functions with an
almost unique color. The keywords and the declaration of function colors are
disabled in order to make the colorized variables and functions stand out.

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(colors)
  "List of contribution to load."
)
```

## Key bindings

The prefix associated with colors is <kbd>C</kbd>.

### Rainbow Identifiers

`rainbow-identifiers` mode can be toggled on and off with:

    <SPC> t C i

Note that the toggle is local to the current buffer.

The `saturation` and `lightness` of identifier colors can be adjusted live
with the micro-state:

Key Binding   | Description
--------------|------------------------------------------------------------
`<SPC> C i s` | initiate change `saturation` mini-mode
`<SPC> C i l` | initiate change `lightness` mini-mode
`+`           | increase the `saturation` or `lightness`
`-`           | decrease the `saturation` or `lightness`
`=`           | reset the `saturation` or `lightness`
Any other key | leave the change mini-mode

## Screenshots

### Python

![theme_tweaks_python](img/theme-tweaks-python.png)

[rainbow identifiers]: https://github.com/Fanael/rainbow-identifiers
