# Autohotkey contribution layer for Spacemacs #

Syntax highlighting and Emacs functions for use with [AutoHotkey][www.autohotkey.com] or [AutoHotkey_L][ahkscript.org]. Using a combined implementation of ahk-mode from Xah Lee's `xahk-mode` and Robert Widhopf-Fenk's `autohotkey-mode`.  Updated with the latest ahk and ahk_l commands found in the latest revision of [SciTE4AutoHotkey[http://fincs.ahk4.net/scite4ahk/].

Contributed and maintained by [Rich Alesi][https://www.github.com/ralesi].

## Features ##

- **Auto-completion**
- **Documentation Lookup**
- **Execute Code Snippets
- **Correct Indentation and Commenting**

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(defvar dotspacemacs-configuration-layers '(autohotkey)
  "List of contribution to load."
)
```

## Key Bindings

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> m D`       | open documentation in `browser`
`<SPC> m E`       | execute file with `autohtokey.exe`

