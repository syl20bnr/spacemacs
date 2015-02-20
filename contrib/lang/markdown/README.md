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
        - [Promotion, Demotion, Completion, and Cycling](#promotion-demotion-completion-and-cycling)
        - [Following and Jumping](#following-and-jumping)
        - [Indentation](#indentation)
        - [Header navigation](#header-navigation)
        - [Buffer-wide commands](#buffer-wide-commands)
        - [List editing](#list-editing)
        - [Movement](#movement)

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
<kbd>SPC m "</kbd>    | insert hr
<kbd>SPC m a l</kbd>  | insert link
<kbd>SPC m a L</kbd>  | insert reference link dwim
<kbd>SPC m a u</kbd>  | insert uri
<kbd>SPC m a f</kbd>  | insert footnote
<kbd>SPC m a w</kbd>  | insert wiki link
<kbd>SPC m i i</kbd>  | insert image
<kbd>SPC m i I</kbd>  | insert reference image
<kbd>SPC m t h</kbd>  | insert header dwim
<kbd>SPC m t H</kbd>  | insert header setext dwim
<kbd>SPC m t 1</kbd>  | insert header atx 1
<kbd>SPC m t 2</kbd>  | insert header atx 2
<kbd>SPC m t 3</kbd>  | insert header atx 3
<kbd>SPC m t 4</kbd>  | insert header atx 4
<kbd>SPC m t 5</kbd>  | insert header atx 5
<kbd>SPC m t 6</kbd>  | insert header atx 6
<kbd>SPC m t !</kbd>  | insert header setext 1
<kbd>SPC m t @</kbd>  | insert header setext 2
<kbd>SPC m s s</kbd>  | insert bold
<kbd>SPC m s e</kbd>  | insert italic
<kbd>SPC m s c</kbd>  | insert code
<kbd>SPC m s b</kbd>  | insert blockquote
<kbd>SPC m s B</kbd>  | blockquote region
<kbd>SPC m s p</kbd>  | insert pre
<kbd>SPC m s P</kbd>  | pre region

### Element removal

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m k</kbd>    | kill thing at point

### Promotion, Demotion, Completion, and Cycling

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
<kbd>SPC m n</kbd>    | outline next visible heading
<kbd>SPC m p</kbd>    | outline previous visible heading
<kbd>SPC m f</kbd>    | outline forward same level
<kbd>SPC m b</kbd>    | outline backward same level
<kbd>SPC m u</kbd>    | outline up heading

### Buffer-wide commands

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m c m</kbd>  | complete buffer
<kbd>SPC m c p</kbd>  | other window
<kbd>SPC m c e</kbd>  | preview
<kbd>SPC m c v</kbd>  | export
<kbd>SPC m c o</kbd>  | export and preview
<kbd>SPC m c w</kbd>  | open
<kbd>SPC m c c</kbd>  | kill ring save
<kbd>SPC m c n</kbd>  | check refs
<kbd>SPC m c ]</kbd>  | cleanup list numbers

### List editing

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m l k</kbd>  | move up
<kbd>SPC m l j</kbd>  | move down
<kbd>SPC m l h</kbd>  | promote
<kbd>SPC m l l</kbd>  | demote
<kbd>SPC m l i</kbd>  | insert list item

### Movement

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m {</kbd>    | backward paragraph
<kbd>SPC m }</kbd>    | forward paragraph
<kbd>SPC m N</kbd>    | next link
<kbd>SPC m P</kbd>    | previous link

[markdown-mode]: http://jblevins.org/git/markdown-mode.git/
[markdown-toc]: https://github.com/ardumont/markdown-toc

