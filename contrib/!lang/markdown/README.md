# Markdown contribution layer for Spacemacs

![logo](img/markdown.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Markdown contribution layer for Spacemacs](#markdown-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Usage](#usage)
    - [Key bindings](#key-bindings)
        - [Element insertion](#element-insertion)
        - [Element removal](#element-removal)
        - [Completion, and Cycling](#completion-and-cycling)
        - [Following and Jumping](#following-and-jumping)
        - [Indentation](#indentation)
        - [Header navigation](#header-navigation)
        - [Buffer-wide commands](#buffer-wide-commands)
        - [List editing](#list-editing)
        - [Movement](#movement)
        - [Promotion, Demotion](#promotion-demotion)

<!-- markdown-toc end -->

## Description

This layer adds markdown support to Spacemacs.

Features:
- markdown files support via [markdown-mode][]
- TOC generation via [markdown-toc][]

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(markdown))
```

## Usage

To generate a table of contents type on top of the buffer:
<kbd>SPC : markdown-toc/generate-toc RET</kbd>

## Key bindings

### Element insertion

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m -</kbd>    | insert horizontal line
<kbd>SPC m h i</kbd>  | insert header dwim
<kbd>SPC m h I</kbd>  | insert header setext dwim
<kbd>SPC m h 1</kbd>  | insert header atx 1
<kbd>SPC m h 2</kbd>  | insert header atx 2
<kbd>SPC m h 3</kbd>  | insert header atx 3
<kbd>SPC m h 4</kbd>  | insert header atx 4
<kbd>SPC m h 5</kbd>  | insert header atx 5
<kbd>SPC m h 6</kbd>  | insert header atx 6
<kbd>SPC m h !</kbd>  | insert header setext 1
<kbd>SPC m h @</kbd>  | insert header setext 2
<kbd>SPC m i l</kbd>  | insert link
<kbd>SPC m i L</kbd>  | insert reference link dwim
<kbd>SPC m i u</kbd>  | insert uri
<kbd>SPC m i f</kbd>  | insert footnote
<kbd>SPC m i w</kbd>  | insert wiki link
<kbd>SPC m i i</kbd>  | insert image
<kbd>SPC m i I</kbd>  | insert reference image
<kbd>SPC m x b</kbd>  | make region bold or insert bold
<kbd>SPC m x i</kbd>  | make region italic or insert italic
<kbd>SPC m x c</kbd>  | make region code or insert code
<kbd>SPC m x q</kbd>  | make region blockquote or insert blockquote
<kbd>SPC m x Q</kbd>  | blockquote region
<kbd>SPC m x p</kbd>  | make region or insert pre
<kbd>SPC m x P</kbd>  | pre region

### Element removal

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m k</kbd>    | kill thing at point

### Completion, and Cycling

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m =</kbd>    | promote
<kbd>SPC m -</kbd>    | demote
<kbd>SPC m ]</kbd>    | complete

### Following and Jumping

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m o</kbd>    | follow thing at point
<kbd>SPC m j</kbd>    | jump

### Indentation

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m \></kbd>   | indent region
<kbd>SPC m \<</kbd>   | exdent region

### Header navigation

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>gj</kbd>         | outline forward same level
<kbd>gk</kbd>         | outline backward same level
<kbd>gh</kbd>         | outline up one level
<kbd>gl</kbd>         | outline next visible heading

### Buffer-wide commands

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m c ]</kbd>  | cleanup list numbers
<kbd>SPC m c c</kbd>  | kill ring save
<kbd>SPC m c e</kbd>  | preview
<kbd>SPC m c m</kbd>  | complete buffer
<kbd>SPC m c n</kbd>  | check refs
<kbd>SPC m c o</kbd>  | export and preview
<kbd>SPC m c p</kbd>  | other window
<kbd>SPC m c r</kbd>  | render buffer
<kbd>SPC m c v</kbd>  | export
<kbd>SPC m c w</kbd>  | open

### List editing

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m l h</kbd>  | promote
<kbd>SPC m l i</kbd>  | insert list item
<kbd>SPC m l j</kbd>  | move down
<kbd>SPC m l k</kbd>  | move up
<kbd>SPC m l l</kbd>  | demote

### Movement

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m {</kbd>    | backward paragraph
<kbd>SPC m }</kbd>    | forward paragraph
<kbd>SPC m N</kbd>    | next link
<kbd>SPC m P</kbd>    | previous link

### Promotion, Demotion

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>M-k</kbd>        | markdown-move-up
<kbd>M-j</kbd>        | markdown-move-down
<kbd>M-h</kbd>        | markdown-promote
<kbd>M-l</kbd>        | markdown-demote

[markdown-mode]: http://jblevins.org/git/markdown-mode.git/
[markdown-toc]: https://github.com/ardumont/markdown-toc

