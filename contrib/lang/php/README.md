#PHP contribution layer for Spacemacs
 
![logo](img/php.png)
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [PHP contribution layer for Spacemacs](#php-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)

<!-- markdown-toc end -->

## Description
This layer adds PHP language support to Spacemacs.

Features:
- Edit PHP files using [php-mode][]
- Edit Drupal files
- Run tests with PHPUnit
- Reformat code with PHP CBF

The `gtags` layer is strongly recommended.

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(php))
```
[php-mode]: https://github.com/ejmr/php-mode 
