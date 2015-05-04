# Better Defaults contribution layer for Spacemacs

![logo](img/emacs.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Better Defaults contribution layer for Spacemacs](#better-defaults-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Functions](#functions)
        - [smart-move-beginning-of-line](#smart-move-beginning-of-line)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer enhances the default commands of Emacs and is primarily intended
to be used with the `emacs` editing style as it does not change anything
in the Vim key bindings.

However the `emacs` editing style is not required, you can still use this
layer while you are using the `vim` editing style if you have some kind of
mixed style.

The commands defined in this layer are taken from various sources like
[Prelude][].

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(better-defaults))
```

## Functions

### smart-move-beginning-of-line

Pressed one time, go to the first non-whitespace character of the line,
pressed again, go to the beginning of the line.

## Key bindings

Key Binding   | Description
--------------|------------------------------------------------------------
`C-a`         | smart beginning of line
`C-o`         | get into Vim normal mode to execute one command, then go back Emacs editing mode
`M-o` (Dired) | Open file in other window without moving point. It is the replacement for `C-o` in Dired.
`C-y`         | Automatically indenting after pasting. With prefix argument, paste text as it is

[Prelude]: https://github.com/bbatsov/prelude
