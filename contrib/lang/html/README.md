# HTML contribution layer for Spacemacs

![logo](img/html.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [HTML contribution layer for Spacemacs](#html-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key Bindings](#key-bindings)
        - [Web mode](#web-mode)
        - [CSS/Scss](#cssscss)

<!-- markdown-toc end -->

## Description

This layer adds support for editing HTML and CSS.

Features:
- Editing HTML and CSS file using [web-mode][]
- Support for Sass/Scss and Less files
- Generate HTML and CSS coding using [emmet-mode][]
- Tags navigation on key `%` using [evil-matchit][]

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(html))
```

## Key Bindings

### Web mode

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC m g p</kbd> | quickly navigate CSS rules using `helm`
<kbd>SPC m e h</kbd> | highlight DOM errors
<kbd>SPC m g b</kbd> | go to the beginning of current element
<kbd>SPC m g c</kbd> | go to the first child element
<kbd>SPC m g p</kbd> | go to the parent element
<kbd>SPC m g s</kbd> | go to next sibling
<kbd>SPC m h p</kbd> | show xpath of the current element
<kbd>SPC m r c</kbd> | clone the current element
<kbd>SPC m r d</kbd> | delete the current element (does not delete the children)
<kbd>SPC m r r</kbd> | rename current element
<kbd>SPC m r w</kbd> | wrap current element
<kbd>SPC m z</kbd>   | fold/unfold current element

A micro-state is also defined, start it with <kbd>SPC m .</kbd> or
<kbd>, .</kbd>

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>?</kbd>         | Toggle full help
<kbd>c</kbd>         | clone current element
<kbd>d</kbd>         | delete (vanish) current element (does not delete the children)
<kbd>h</kbd>         | previous element
<kbd>l</kbd>         | next element
<kbd>L</kbd>         | next sibling element
<kbd>k</kbd>         | parent element
<kbd>j</kbd>         | first child element
<kbd>p</kbd>         | show xpath of current element
<kbd>r</kbd>         | rename current element
<kbd>q</kbd>         | leave the micro-state
<kbd>w</kbd>         | wrap current element

### CSS/Scss

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC m g h</kbd> | quickly navigate CSS rules using `helm`

[web-mode]: http://web-mode.org/
[emmet-mode]: https://github.com/smihica/emmet-mode
[evil-matchit]: https://github.com/redguardtoo/evil-matchit

