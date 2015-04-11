# Syntax Checking configuration layer for Spacemacs

![logo](img/flycheck.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Syntax Checking configuration layer for Spacemacs](#syntax-checking-configuration-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key Bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer provides on the fly syntax checking using [Flycheck][] to Spacemacs.

## Install

To use this configuration layer add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(syntax-checking))
```

## Key Bindings

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC e c</kbd>   | clear errors
<kbd>SPC e l</kbd>   | display a list of all the errors
<kbd>SPC t f</kbd>   | toggle flycheck

[Flycheck]: http://www.flycheck.org/
