# Vim-empty-lines contribution layer for Spacemacs

## Description

This layer is a replaces evil-nerd-commentor with evil-commentor, 
for those who desire behaviour like `vim-commentary`'s.

> Use <kbd>gcc</kbd> to comment out a line (takes a count),
> <kbd>gc</kbd> to comment out the target of a motion (for example,
> <kbd>gcap</kbd> to comment out a paragraph), <kbd>gc</kbd> in visual
> mode to comment out the selection.

For more details see the [evil-commentary][] repository.

## Install

To use this contribution add it to your `~/.spacemacs`.

```elisp
(setq-default dotspacemacs-configuration-layers '(evil-commentary))
```
[evil-commentary]: https://github.com/linktohack/evil-commentary
