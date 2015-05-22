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

This layer adds asciidoc support to Spacemacs.

Feature
- asciidoc files support via [adoc-mode][]

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(asciidoc))
```

## Key bindings

### Element insertion

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m t 1<kbd>   | Insert title level 1
<kbd>SPC m t 2<kbd>   | Insert title level 2
<kbd>SPC m t 3<kbd>   | Insert title level 3
<kbd>SPC m t 4<kbd>   | Insert title level 4
<kbd>SPC m t 5<kbd>   | Insert title level 5
<kbd>SPC m t 5<kbd>   | Insert title level 5
<kbd>SPC m s s<kbd>   | Insert bold
<kbd>SPC m s e<kbd>   | Insert italic



### Promotion, Demotion
    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m t p<kbd>   | Promote title
<kbd>SPC m t d<kbd>   | Demote title



