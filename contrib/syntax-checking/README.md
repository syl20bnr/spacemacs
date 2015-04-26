# Syntax Checking configuration layer for Spacemacs

![logo](img/flycheck.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Syntax Checking configuration layer for Spacemacs](#syntax-checking-configuration-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Variables](#variables)
    - [Key Bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer provides on the fly syntax checking using [Flycheck][] to Spacemacs.

## Install

To use this configuration layer add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(syntax-checking))
```

### Variables

To set any variables for this layer, add the following to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(
  (syntax-checking :variables
                   syntax-checking-flycheck-pos-tip nil)
)
```

    Variable                       |                 Description
-----------------------------------|------------------------------------------------------------------------------------------------------------------
syntax-checking-flycheck-pos-tip   | If nil (default t), the [flycheck-pos-tip](https://github.com/flycheck/flycheck-pos-tip) package will be disabled 


## Key Bindings

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC e c</kbd>   | clear errors
<kbd>SPC e l</kbd>   | display a list of all the errors
<kbd>SPC t f</kbd>   | toggle flycheck

[Flycheck]: http://www.flycheck.org/
