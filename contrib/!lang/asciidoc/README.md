# Asciidoc contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Asciidoc contribution layer for Spacemacs](#asciidoc-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)
        - [Element insertion](#element-insertion)
        - [Promotion, Demotion](#promotion-demotion)

<!-- markdown-toc end -->

## Description

This layer adds [AsciiDoc][] markup language support to Spacemacs.

Feature
- asciidoc format support via [adoc-mode][]
- `.adoc` files are associated with `adoc-mode` by default

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(asciidoc))
```

## Key bindings

### Element insertion

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m h 1<kbd>   | Insert title level 1
<kbd>SPC m h I<kbd>   | Insert title level 1 (the most important one)
<kbd>SPC m h 2<kbd>   | Insert title level 2
<kbd>SPC m h i<kbd>   | Insert title level 2 (the most usual one)
<kbd>SPC m h 3<kbd>   | Insert title level 3
<kbd>SPC m h 4<kbd>   | Insert title level 4
<kbd>SPC m h 5<kbd>   | Insert title level 5
<kbd>SPC m h 5<kbd>   | Insert title level 5
<kbd>SPC m x b<kbd>   | Boldface selected
<kbd>SPC m x i<kbd>   | Italicize selected


### Promotion, Demotion
    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>M-h<kbd>   | Promote title level
<kbd>M-l<kbd>   | Denote title level

[AsciiDoc]: https://asciidoctor.org
[adoc-mode]: https://github.com/sensorflo/adoc-mode
