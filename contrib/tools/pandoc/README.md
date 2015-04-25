# Pandoc contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Pandoc contribution layer for Spacemacs](#pandoc-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [Pandoc](#pandoc)
    - [Usage](#usage)
    - [Keybindings](#keybindings)

<!-- markdown-toc end -->

## Description

This layer adds support for [Pandoc][].

Pandoc is a universal document converter. It makes it easy to e.g. convert a
Markdown file to org mode or vice versa. It can also export your text to PDF or
DOCX.

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(pandoc))
```

### Pandoc

To use the mode please [install][] pandoc first.

## Usage

For a full list of possible conversions see the [Pandoc website][Pandoc].

An explanation of all the options offered by `pandoc-mode` can be found at the
[Pandoc-mode website][Pandoc-github].

## Keybindings

    Key Binding     |                 Description
--------------------|------------------------------------------------------------
<kbd>SPC P /</kbd>  | Start pandoc-mode and open menu

[Pandoc]: http://johnmacfarlane.net/pandoc
[Pandoc-github]: http://joostkremers.github.io/pandoc-mode
[install]: http://pandoc.org/installing.html
