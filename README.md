<a name="top"></a>
[![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/syl20bnr/spacemacs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Build Status](https://travis-ci.org/syl20bnr/spacemacs.svg)](https://travis-ci.org/syl20bnr/spacemacs) [![Buy A Drink](https://img.shields.io/badge/Paypal-Buy%20a%20Drink-blue.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=ESFVNPKP4Y742) [![Recommend Spacemacs](https://img.shields.io/badge/Slant-Recommend-ff69b4.svg)](http://www.slant.co/topics/12/~what-are-the-best-programming-text-editors)[![Twitter][]](http://www.twitter.com/spacemacs)
***
<p align="center"><img src="/doc/img/title2.png" alt="Spacemacs"/></p>
<p align="center">
<b><a href="doc/DOCUMENTATION.org#core-pillars">philosophy</a></b>
|
<b><a href="doc/DOCUMENTATION.org#goals">goals</a></b>
|
<b><a href="doc/DOCUMENTATION.org#user-content-who-can-benefit-from-this">for whom?</a></b>
|
<b><a href="doc/DOCUMENTATION.org#screenshots">screenshots</a></b>
|
<b><a href="doc/DOCUMENTATION.org">documentation</a></b>
|
<b><a href="doc/CONTRIBUTE.org">contribute</a></b>
|
<b><a href="doc/DOCUMENTATION.org#achievements">achievements</a></b>
|
<b><a href="#faq">FAQ</a></b>
</p>
***

**Quick Install:**

    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Introduction](#introduction)
- [Features](#features)
- [Documentation](#documentation)
- [Getting Help](#getting-help)
- [Prerequisites](#prerequisites)
    - [Emacs](#emacs)
        - [Linux distros](#linux-distros)
        - [OS X](#os-x)
        - [Windows](#windows)
- [Install](#install)
    - [Spacemacs logo](#spacemacs-logo)
- [Update](#update)
    - [Automatic update](#automatic-update)
    - [On develop branch](#on-develop-branch)
    - [Revert to a specific version](#revert-to-a-specific-version)
- [Contributions](#contributions)
- [License](#license)
- [Supporting Spacemacs](#supporting-spacemacs)

<!-- markdown-toc end -->

# Introduction

Spacemacs is a proposition of a complete new experience for Emacs focused
on ergonomics, mnemonics and consistency. 

Clone it, launch it then press the space bar and let you guided by the built-in
interactive list of key-bindings. You can also press the `[?]` button on the
home buffer for the first essential key bindings to know.

Spacemacs can be used by Emacs or Vim users flawlessly, it is even possible
to mix the two styles. You can switch between any style at any moment which
make it a very good tool for pair-programming.

Spacemacs is currently in beta, and contributions are very welcome.

![spacemacs_python](doc/img/spacemacs-python.png)

# Features

- **Great documentation:** Access documentation in Emacs with
<kbd>SPC f e h</kbd>
- **Beautiful GUI:** you'll love the distraction free UI and its functional
mode-line.
- **Excellent ergonomics:** all the key bindings are accessible by pressing
the <kbd>space bar</kbd> or <kbd>alt-m</kbd>.
- **Mnemonic key bindings:** commands have mnemonic prefixes like
<kbd>SPC b</kbd> for all the buffer commands or <kbd>SPC p</kbd> for the
project commands.
- **Batteries included:** discover hundreds of ready-to-use packages nicely
organised in configuration layers following a set of
[conventions][CONVENTIONS.org].

# Documentation

A comprehensive documentation is available for each layer by pressing
<kbd>SPC f e h</kbd>. 

To go to the general documentation [click here][DOCUMENTATION.org].

You can also check the [Quick Start guide][QUICK_START.org] and the
[FAQ][FAQ.org].

# Getting Help

If you need help, ask your question in the [Gitter Chat][] and a member of the
community will help you out.

If you prefer IRC, connect to the [Gitter Chat IRC server][] and join the
`#syl20bnr/spacemacs` channel.

# Prerequisites

## Emacs

`Spacemacs` is operational with Emacs 24.3 but Emacs 24.4 and above are
recommended to enjoy the full experience.

Some modes require third-party tools that you'll have to install via your
favorite package manager.

### Linux distros

Install Emacs from the package manager of your Linux distribution.

You should install the "emacs" package, not the "xemacs" package.
XEmacs is an old fork of Emacs. The X in its name is unrelated to X11.
Both Emacs and XEmacs have graphical support.

### OS X

We recommend the homebrew [emacs-mac-port][] formula:

```sh
$ brew tap railwaycat/emacsmacport
$ brew install emacs-mac --with-spacemacs-icon  # OR, brew cask install emacs-mac
```

It is also recommended to add the [osx layer][] to your [dotfile][]:

```elisp
(setq-default dotspacemacs-configuration-layers '(osx))
```

Note that the `emacs-mac-port` server behaves differently than the regular
Emacs server.
Details can be found on the emacs-mac-port [README][emacs-mac-port-server].

### Windows

Good quality builds can be found [on this page][emacs-for-windows]. It is
recommended to install the most stable build.

Be sure to declare a environment variable named `HOME` pointing to your user
directory `C:\Users\<username>`. Then you can clone Spacemacs in this directory.

Sometimes you'll get the following error when you first start Emacs:

```
The directory ~/.emacs.d/server is unsafe
```

To fix it change the owner of the directory `~/.emacs.d/server`:
  - from Properties select the Tab “Security”,
  - select the button “Advanced”,
  - select the Tab “Owner”
  - change the owner to your account name

Source: [Stackoverflow][so-server-unsafe]

For efficient searches we recommend to install `pt` ([the platinum searcher][]).
`pt` version 1.7.7 or higher is required.

# Install

1. If you have an existing Emacs configuration, back it up first:

   ```sh
   cd ~
   mv .emacs.d .emacs.bak
   ```

2. Clone the repository:

   ```sh
   git clone --recursive https://github.com/syl20bnr/spacemacs ~/.emacs.d
   ```

   `master` is the stable branch and it is _immutable_, **DO NOT** make any
   modification to it or you will break the update mechanism. If you want to
   fork Spacemacs safely use the `develop` branch where you handle the update
   manually.

3. Launch Emacs. Spacemacs will automatically install the packages it requires.

4. Restart Emacs to complete the installation.

If the mode-line turns red then be sure to visit the
[troubleshooting][troubleshoot] guide and consult the [FAQ](#faq).

## Spacemacs logo

If you are using Ubuntu and Unity then you can add the Spacemacs logo by
following the instructions [here][cpaulik-unity-icon].

If you're on a mac follow [these instructions][icon-mac-instructions]. You can
find an .icns version of the logo by [Nasser Alshammari](http://www.nass3r.com)
in [his repository][icon-repository].

# Update

Spacemacs has a built-in notification of a new version when you are on the
`master` branch. If you are on the `develop` branch then you'll have to
update Spacemacs manually by updating your repository.

## Automatic update

When a new version is available a little arrow appears in the mode-line.

Its color depends on the number of versions available since your last update.
Green means that your current version is recent, orange and red that your
current version is older.

[[file:doc/img/powerline-update.png]]

Click on the arrow to update Spacemacs to the last version.

## On develop branch

1. Update Emacs packages by clicking (press `RET`) on the `[Update]` link of
the starting page.

2. Close Emacs and update the git repository:

   ```sh
   git pull --rebase
   ```

3. Restart Emacs to complete the upgrade.

## Revert to a specific version

To revert to a specific version you just have to checkout the corresponding
branch, for instance to revert to the last `0.103`:

   ```sh
   git checkout origin/release-0.103
   ```

# Contributions

`Spacemacs` is a community-driven project, it needs _you_ to keep it up to
date and propose useful and complete configuration!

Before contributing be sure to consult the
[contribution guidelines][CONTRIBUTE.org] and [conventions][CONVENTIONS.org].

# License

The license is GPLv3 for all parts specific to `Spacemacs`, this includes:
- the initialization and core files
- all the layer files.
- the documentation

For the packages shipped in this repository you can refer to the files header.

[Spacemacs logo][] by [Nasser Alshammari][] released under a Creative Commons
license.

# Supporting Spacemacs

The best way to support Spacemacs is to contribute to it either by reporting
bugs, helping the community on the [Gitter Chat][] or sending pull requests.

If you want to show your support financially you can buy a drink to the
maintainer by clicking on the [Paypal badge](#top).

Thank you !

[Twitter]: http://i.imgur.com/tXSoThF.png
[CONTRIBUTE.org]: doc/CONTRIBUTE.org
[CONVENTIONS.org]: doc/CONVENTIONS.org
[DOCUMENTATION.org]: doc/DOCUMENTATION.org
[QUICK_START.org]: doc/QUICK_START.org
[FAQ.org]: doc/FAQ.org
[HOWTOs.org]: doc/HOWTOs.org
[VIMUSERS.org]: doc/VIMUSERS.org
[dotfile]: doc/DOCUMENTATION.org#dotfile-configuration
[troubleshoot]: doc/DOCUMENTATION.org#troubleshoot
[osx layer]: layers/osx/README.org
[Gitter Chat]: https://gitter.im/syl20bnr/spacemacs
[Gitter Chat IRC server]: https://irc.gitter.im/
[emacs-mac-port]: https://github.com/railwaycat/homebrew-emacsmacport
[emacs-mac-port-server]: https://github.com/railwaycat/emacs-mac-port/blob/master/README-mac#L210-L213
[emacs-for-windows]: http://emacsbinw64.sourceforge.net/
[the platinum searcher]: https://github.com/monochromegane/the_platinum_searcher
[so-server-unsafe]: http://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
[Spacemacs logo]: https://github.com/nashamri/spacemacs-logo
[Nasser Alshammari]: https://github.com/nashamri
[cpaulik-unity-icon]: http://splendidabacus.com/posts/2015/03/spacemacs-unity-icon/
[icon-mac-instructions]: http://www.idownloadblog.com/2014/07/16/how-to-change-app-icon-mac/
[icon-repository]: https://github.com/nashamri/spacemacs-logo
