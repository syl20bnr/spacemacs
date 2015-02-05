# PHP contribution layer for Spacemacs

![logo](img/php.png)
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [PHP contribution layer for Spacemacs](#php-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)

<!-- markdown-toc end -->

## Description
This layer adds PHP language support to Spacemacs

Features:
- Edit php files using [php-mode][]

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(php)
"List of contribution to load."
)

Key Binding          | Description
---------------------|--------------------------------------------
<kbd>SPC m s r</kbd> | Send region to be evaluated.
<kbd>SPC m c v</kbd> | Display current php-mode version.
<kbd>SPC m a l</kbd> | Show arguments list.
<kbd>SPC m s d</kbd> | Search in the PHP documentation.
<kbd>SPC m b m</kbd> | Browse PHP documentation.
```
[php-mode]: https://github.com/ejmr/php-mode
