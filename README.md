![title](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/title.png)
[![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/syl20bnr/spacemacs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)[![Twitter][]](http://www.twitter.com/spacemacs)

[philosophy][] | [goals][] | [for who?][] | [screenshots][]

# Introduction

`Spacemacs` is a user-friendly and well-documented Emacs Advanced kit.

`Advanced` means here that it heavily and happily performs non-trivial tweaks
and customization in order to make itself easy to use and unique.

`Advanced` does not mean that you have to be an advanced Emacs user, beginners
are welcome and encouraged to use `Spacemacs` since it provides a simple
architecture to contribute changes and hack others contributions.

Its status is `beta` for now, essentially because of the contribution system
that is not yet finished but already usable.

And now, to use a well known catch line from [Emacs Live][emacs_live]:

    M-x start-spacing !

# Prerequisites

`Spacemacs` is tested with Emacs 24.3 and 24.4. It should boot on all the major
OSes where these versions can be installed.

# Install

1) Backup your current `~/.emacs.d` and clone the repo _with the submodules_:

    cd ~
    mv .emacs.d .emacs.bak
    git clone --recursive http://github.com/syl20bnr/spacemacs ~/.emacs.d

`master` is a stable branch, if you want the "bleeding edge" checkout the
`develop` branch.

2) Launch Emacs, the first time a bunch of packages will be downloaded and
installed. When the package installation is complete restart Emacs and
`Spacemacs` should be ready to use.

In case of failure see [troubleshoot section][troubleshoot] in documentation.

# Configuration

## Configuration layers

`Spacemacs` is based on [configuration layers][config]. To create your own
configuration layer:

    <SPC> : config-system/create-layer RET

Choose a name and a layer skeleton will be create in the `private`
directory.

Then to use your newly created configuraton layer, add it to your
`~/.spacemacs` file (see next section).

## Dotfile ~/.spacemacs

To install it:

    <SPC> : dotspacemacs/install RET

Refers directly to the file `~/.spacemacs` to get documentation or
navigate to the [dotfile configuration][dotfile] section of [DOCUMENTATION.md][].

# Contributions

To consult the contribution guidelines go to [CONTRIBUTE.md][].

# Help commands

## Key bindings

1) By default, [guide-key][] and [guide-key-tip][] are enabled.

Whenever you press a prefix command (like `<SPC>`) and wait for one second,
a buffer appear listing the possible keys following this prefix.

2) You can also easily get a full list of all the key bindings by pressing:

    <SPC> ?

To narrow the list to `Spacemacs` specific key bindings set the pattern to
something like the regular expression:

    `^SPC\ b`

The example above will list all the `buffer` related bindings.

## Other describe functions

Emacs `describe-xxx` function are accessible with the following bindings:

Key Binding   |                 Description
--------------|------------------------------------------------------------------
`<SPC> h d f` | describe-function
`<SPC> h d k` | describe-key
`<SPC> h d m` | describe-mode
`<SPC> h d v` | describe-variable

[Twitter]: http://i.imgur.com/tXSoThF.png
[philosophy]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#philosophy
[goals]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#goals
[for who?]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#who-can-benefit-from-this-
[screenshots]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#screenshots
[config]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#configuration-layers
[dotfile]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#dotfile-configuration
[troubleshoot]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#troubleshoot
[DOCUMENTATION.md]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md
[CONTRIBUTE.md]: https://github.com/syl20bnr/spacemacs/blob/master/CONTRIBUTE.md
[emacs_live]: https://github.com/overtone/emacs-live
[guide-key]: https://github.com/kai2nenobu/guide-key
[guide-key-tip]: https://github.com/aki2o/guide-key-tip
