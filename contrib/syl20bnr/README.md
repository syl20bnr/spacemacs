# syl20bnr configuration layer for Spacemacs

## Install

To use this configuration add it to your `~/.spacemacs`

```elisp
(defvar dotspacemacs-configuration-layers '(syl20bnr)
  "List of contribution to load."
)
```

## Evil

Prevent the point from moving back when leaving the insert mode.

## Color Theme

Instead of colorized keywords, this configuration layer has *colorized
variables and functions* using the [rainbow identifiers][rainbow-identifiers]
mode. Each variable and function has a unique color associated to it.

### Screenshots

#### Python

![theme_tweaks_python](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/contrib/syl20bnr/doc/theme-tweaks-python.png)

[rainbow-identifiers]: https://github.com/Fanael/rainbow-identifiers
