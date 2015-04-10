# EditorConfig Layer for Spacemacs

![EditorConfig](img/editorconfig.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [EditorConfig Layer for Spacemacs](#editorconfig-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [EditorConfig](#editorconfig)
        - [Layer](#layer)

<!-- markdown-toc end -->

## Description

This layer provides support for [EditorConfig](http://editorconfig.org/). Syntax highlighting is also provided for `.editorconfig` files.

## Install

### EditorConfig

If you have not installed the EditorConfig C Core already, follow the instructions [here](https://github.com/editorconfig/editorconfig-core-c/blob/master/INSTALL.md).

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(editorconfig))
```
