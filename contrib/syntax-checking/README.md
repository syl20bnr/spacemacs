# Syntax Checking configuration layer for Spacemacs

![logo](img/flycheck.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Syntax Checking configuration layer for Spacemacs](#syntax-checking-configuration-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [Enabling/Disabling tooltips](#enablingdisabling-tooltips)
    - [Key Bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer provides on the fly syntax and spelling checking using
[Flycheck][] and [Flyspell][].

## Install

### Layer

To use this configuration layer add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(syntax-checking))
```

### Enabling/Disabling tooltips

By default tooltips are enabled and used whenever it is possible.
You can disable them by setting the variable `syntax-checking-enable-tooltips`
to `nil`:

```elisp
(setq-default dotspacemacs-configuration-layers
  '((syntax-checking :variables syntax-checking-enable-tooltips nil)))
```


## Key Bindings

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC e c</kbd>   | clear errors
<kbd>SPC e l</kbd>   | display a list of all the errors
<kbd>SPC S c</kbd>   | 
<kbd>SPC t s</kbd>   | toggle flycheck
<kbd>SPC t S</kbd>   | toggle flyspell

[Flycheck]: http://www.flycheck.org/
[Flyspell]: http://www-sop.inria.fr/members/Manuel.Serrano/flyspell/flyspell.html
