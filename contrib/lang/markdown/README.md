# Markdown contribution layer for Spacemacs

![logo](img/markdown.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Colors contribution layer for Spacemacs](#colors-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)
        - [Ledger](#ledger)

<!-- markdown-toc end -->

## Description

This layer adds markdown support to Spacemacs.

Features:
- markdown files support via [markdown-mode][]
- TOC generation via [markdown-toc][]

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(markdown)
  "List of contribution to load."
)
```

## Usage

To generate a table of contents type on top of the buffer:
<kbd>SPC : markdown-toc/generate-toc RET</kbd>

## Key bindings

**TODO**

[markdown-mode]: http://jblevins.org/git/markdown-mode.git/
[markdown-toc]: https://github.com/ardumont/markdown-toc

