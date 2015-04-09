<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Pandoc mode](#pandoc-mode)
    - [Keybindings](#keybindings)

<!-- markdown-toc end -->

# Pandoc mode #

[Pandoc](http://johnmacfarlane.net/pandoc/) is a universal
document converter. It makes it easy to e.g. convert a Markdown file to org mode
or vice versa. It can also export your text to PDF or DOCX.

To use the mode please install pandoc first.

For a full list of possible conversions see the [Pandoc website](http://johnmacfarlane.net/pandoc/).

An explanation of all the options offered by pandoc-mode can be found at the
[Pandoc-mode website](http://joostkremers.github.io/pandoc-mode/). The command
`C-c /` is mapped to <kbd>SPC P /</kbd> in Spacemacs.

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(pandoc))
```
## Keybindings

    Key Binding     |                 Description
--------------------|------------------------------------------------------------
<kbd>SPC P /</kbd>  | Start pandoc-mode and open menu

