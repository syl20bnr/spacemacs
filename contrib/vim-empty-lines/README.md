# Vim-empty-lines contribution layer for Spacemacs

## Description

This layer is a drop-in replacement for the `vi-tilde-fringe` mode,
for those who desire behaviour closer to `vim`'s.

It has better compatibility with retina displays, as it uses a text
overlay using your font, rather than a pixel-art tilde. The empty line
indicators are overlaid in within the buffer as in `vim`, and not in
the fringe. The indicator behaviour with trailing empty lines matches
`vim`'s behaviour.

For details, see the [vim-empty-lines-mode][] repository.

## Install

To use this contribution add it to your `~/.spacemacs`.

```elisp
(setq-default dotspacemacs-configuration-layers '(vim-empty-lines))
```
[vim-empty-lines-mode]: https://github.com/jmickelin/vim-empty-lines-mode
