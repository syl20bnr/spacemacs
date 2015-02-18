# HTML contribution layer for Spacemacs

![logo](img/html.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [HTML contribution layer for Spacemacs](#html-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key Bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer adds support for editing HTML and CSS.

Features:
- Editing HTML and CSS file using [web-mode][]
- Support for Scss and Less files
- Generate HTML and CSS coding using [emmet-mode][]
- Tags navigation on key `%` using [evil-matchit][]

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(html))
```

## Key Bindings

### commands

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC m h</kbd>   | quick navigate CSS rules using helm

[web-mode]: http://web-mode.org/
[emmet-mode]: https://github.com/smihica/emmet-mode
[evil-matchit]: https://github.com/redguardtoo/evil-matchit

