# fasd contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [fasd contribution layer for Spacemacs](#fasd-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [fasd](#fasd)
    - [Keybindings](#keybindings)

<!-- markdown-toc end -->

## Description

This layer adds integration of [fasd][] which is a command line tool
to quickly jump between locations in a POSIX shell.

The integration is implemented in the package [emacs-fasd][].

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(fasd))
```

### fasd

[fasd][] must be installed on your system. The general installation
instructions can be found in the repository [README][fasd-install]

On OS X, it can be installed via [homebrew][]:

```sh
$ brew install fasd
```

## Keybindings

Key Binding                 |                 Description
----------------------------|------------------------------------------------------------------
<kbd>SPC f a s</kbd>        | find a file or directory with fasd
<kbd>SPC f a d</kbd>        | find a directory with fasd
<kbd>SPC f a f</kbd>        | find a file with fasd

[fasd]: https://github.com/clvv/fasd
[fasd-install]: https://github.com/clvv/fasd#install
[emacs-fasd]: https://github.com/steckerhalter/emacs-fasd
[homebrew]: https://github.com/Homebrew/homebrew
