# Evil-commentary contribution layer for Spacemacs

## Description

This layer replaces [evil-nerd-commenter][] with [evil-commentary][] for those
who prefer the behaviour of [vim-commentary][].

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
[vim-commentary]: https://github.com/tpope/vim-commentary
[evil-nerd-commenter]: https://github.com/redguardtoo/evil-nerd-commenter
