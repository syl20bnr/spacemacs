# Autohotkey contribution layer for Spacemacs

![logo](img/ahk.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Autohotkey contribution layer for Spacemacs](#autohotkey-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Features](#features)
    - [Install](#install)
    - [Key Bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

Syntax highlighting and Emacs functions for use with [AutoHotkey][] or
[AutoHotkey_L][].

Using a combined implementation of ahk-mode from Xah Lee's `xahk-mode`
and Robert Widhopf-Fenk's `autohotkey-mode`.  Updated with the latest
ahk and ahk_l commands found in the latest revision of
[SciTE4AutoHotkey][].

Contributed and maintained by [Rich Alesi][].

## Features

- Auto-completion
- Documentation Lookup
- Execute Code Snippets
- Correct Indentation and Commenting

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(autohotkey))
```

## Key Bindings

    Key Binding     |                 Description
--------------------|------------------------------------------------------------
<kbd>SPC m d</kbd>  | open documentation in `browser`
<kbd>SPC m e</kbd>  | execute file with `autohotkey.exe`

[AutoHotkey]: http://www.autohotkey.com
[AutoHotkey_L]: http://ahkscript.org
[SciTE4AutoHotkey]: http://fincs.ahk4.net/scite4ahk/
[Rich Alesi]: https://www.github.com/ralesi
