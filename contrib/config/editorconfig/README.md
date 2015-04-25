# EditorConfig Layer for Spacemacs

![EditorConfig](img/editorconfig.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [EditorConfig Layer for Spacemacs](#editorconfig-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [EditorConfig](#editorconfig)

<!-- markdown-toc end -->

## Description

This layer provides support for [EditorConfig][].
Syntax highlighting is also provided for `.editorconfig` files.

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(editorconfig))
```

### EditorConfig

If you have not installed the EditorConfig C Core already,
follow the instructions [here][instructions].

[EditorConfig]: http://editorconfig.org/
[instructions]: https://github.com/editorconfig/editorconfig-core-c/blob/master/INSTALL.md
