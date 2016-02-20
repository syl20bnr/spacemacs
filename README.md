<a name="top"></a>
<a href="http://github.com/syl20bnr/spacemacs"><img src="https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg" alt="Made with Spacemacs"></a><a href="http://www.twitter.com/spacemacs"><img src="http://i.imgur.com/tXSoThF.png" alt="Twitter" align="right"></a><br>
***
<p align="center"><img src="/doc/img/title2.png" alt="Spacemacs"/></p>
<p align="center">
<b><a href="http://spacemacs.org/doc/DOCUMENTATION#orgheadline5">philosophy</a></b>
|
<b><a href="http://spacemacs.org/doc/DOCUMENTATION#orgheadline8">for whom?</a></b>
|
<b><a href="http://spacemacs.org/doc/DOCUMENTATION#orgheadline7">screenshots</a></b>
|
<b><a href="http://spacemacs.org/doc/DOCUMENTATION">documentation</a></b>
|
<b><a href="CONTRIBUTING.org">contribute</a></b>
|
<b><a href="http://spacemacs.org/doc/DOCUMENTATION#orgheadline187">achievements</a></b>
|
<b><a href="http://spacemacs.org/doc/FAQ">FAQ</a></b>
</p>
***
<p align="center">
<a href="https://gitter.im/syl20bnr/spacemacs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge"><img src="https://badges.gitter.im/Join Chat.svg" alt="Gitter"></a>
<a href="https://travis-ci.org/syl20bnr/spacemacs"><img src="https://travis-ci.org/syl20bnr/spacemacs.svg" alt="Build Status"></a>
<a href="https://waffle.io/syl20bnr/spacemacs"><img src="https://badge.waffle.io/syl20bnr/spacemacs.png?label=Merging...&title=Merging" alt="PR being merged"></a>
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=ESFVNPKP4Y742"><img src="https://img.shields.io/badge/Paypal-Donate-blue.svg" alt="Donate"></a>
<a href="http://www.slant.co/topics/12/~what-are-the-best-programming-text-editors"><img src="https://img.shields.io/badge/Slant-Recommend-ff69b4.svg" alt="Recommend it"></a>
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
    - [Default installation](#default-installation)
    - [Installation alongside another configuration](#installation-alongside-another-configuration)
    - [Spacemacs logo](#spacemacs-logo)
- [Update](#update)
    - [Automatic update](#automatic-update)
    - [On develop branch](#on-develop-branch)
    - [Revert to a specific version](#revert-to-a-specific-version)
- [Quotes](#quotes)
- [Contributions](#contributions)
- [License](#license)
- [Supporting Spacemacs](#supporting-spacemacs)

<!-- markdown-toc end -->

# Introduction

Spacemacs is a new way to experience Emacs -- a sophisticated and
polished set-up focused on ergonomics, mnemonics and consistency.

Just clone it, launch it, then press the space bar to explore the
interactive list of carefully-chosen key bindings. You can also press
the home buffer's `[?]` button for some great first key bindings to
try.

Spacemacs can be used naturally by both Emacs and Vim users -- you can
even mix the two editing styles. Switching easily between input styles
makes Spacemacs a great tool for pair-programming.

Spacemacs is currently in beta, and contributions are very welcome.

![spacemacs_python](doc/img/spacemacs-python.png)

# Features

- **Great documentation:** access documentation in Emacs with
<kbd>SPC h SPC</kbd>.
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

Comprehensive documentation is available for each layer by pressing
<kbd>SPC h SPC</kbd>.

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

Spacemacs is operational with Emacs 24.3, but Emacs 24.4 and above are highly
recommended to enjoy a full experience. The next Emacs major release, Emacs 25,
is not *officially* supported but is partially working (i.e. bugs should be
expected).

Some modes require third-party tools that you'll have to install via your
favorite package manager.

### Linux distros

Install Emacs from the package manager of your Linux distribution.

You should install the "emacs" package, not the "xemacs" package.
XEmacs is an old fork of Emacs. The X in its name is unrelated to X11.
Both Emacs and XEmacs have graphical support.

**Note:** Ubuntu LTS 12.04 and 14.04 repositories have only Emacs 24.3
available. You are advised to [build from source][build_source] Emacs 24.4 or
greater, as most packages require this version. The same may be true for other
distributions as well.

### OS X

The recommended way of installing Emacs on OS X is using [homebrew][]:

```sh
$ brew tap d12frosted/emacs-plus
$ brew install emacs-plus --with-cocoa --with-gnutls --with-librsvg --with-imagemagick --with-spacemacs-icon
$ brew linkapps
```

*Note:* these homebrew commands will install Emacs, and link it to your
`/Applications` directory. You still need to run the `git clone` mentioned at
the start of this file. That will populate your `~/.emacs.d` directory, which
is what transforms a regular Emacs into Spacemacs.

*Note:* the proposed `emacs-plus` tap is indentical to the `emacs` formulae,
it just defines some additional options like `--with-spacemacs-icon`.

*Note:* after you have completed the [install process below](#install), it is
also recommended to add the [osx layer][] to your [dotfile][]. Install
instructions are available in the [osx layer][] documentation.

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

## Default installation

1. If you have an existing Emacs configuration, back it up first:

   ```sh
   cd ~
   mv .emacs.d .emacs.d.bak
   mv .emacs .emacs.bak
   ```

   Don't forget to backup and *remove* `~/.emacs` file otherwise Spacemacs
   **WILL NOT** load since that file prevents Emacs from loading the proper
   initialization file.

2. Clone the repository:

   ```sh
   git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
   ```

   `master` is the stable branch and it is _immutable_, **DO NOT** make any
   modification to it or you will break the update mechanism. If you want to
   fork Spacemacs safely use the `develop` branch where you handle the update
   manually.

3. Launch Emacs. Spacemacs will automatically install the packages it requires.
   If you get an error regarding package downloads then you may try to disable
   the HTTPS protocol by starting Emacs with

   ```sh
   emacs --insecure
   ```

   You may wish to clear out your `.emacs.d/elpa` directory before doing this,
   so that any corrupted packages you may have downloaded will be reinstalled.

4. Restart Emacs to complete the installation.

If the mode-line turns red then be sure to consult the [FAQ][FAQ.org].

## Installation alongside another configuration

To try out Spacemacs (or any other Emacs configuration you desire) without
having to go through the trouble of backing up you `~/.emacs.d` directory and
then cloning the new configuration:

```sh
mkdir ~/spacemacs
git clone git@github.com:syl20bnr/spacemacs.git ~/spacemacs/.emacs.d
HOME=~/spacemacs emacs
```

Note: If you're on Fish shell, you will need to modify the last command to: `env
HOME=$HOME/spacemacs emacs`

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

## Automatic update (on master branch)

When a new version is available a little arrow appears in the mode-line.

Its color depends on the number of versions available since your last update.
Green means that your current version is recent, orange and red mean that your
current version is older.

![powerline_update](doc/img/powerline-update.png)

Click on the arrow to update Spacemacs to the last version.

## Manual update (on master branch)

(Remove the angle brackets when typing the lines below into your shell.)

```sh
git fetch
git reset --hard <tag version which you are updating to>
```

## On develop branch

1. Update Emacs packages by clicking (press `RET`) on the `[Update Packages]` link of
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

**After you update, either manually, or automatically, you are advised to update
  your packages by clicking the `[Update Packages]` button on the Spacemacs Home
  Buffer.**

# Quotes

[Quote][quote01] by [ashnur](https://github.com/ashnur):

    «I feel that spacemacs is an aircraft carrier and I am playing table tennis on the deck as a freerider.»

# Contributions

Spacemacs is a community-driven project, it needs _you_ to keep it up to
date and propose useful and complete configuration!

Before contributing be sure to consult the
[contribution guidelines][CONTRIBUTING.org] and [conventions][CONVENTIONS.org].

# Communities

- [Gitter Chat]
- [Stack Exchange]
- [Reddit]

# License

The license is GPLv3 for all parts specific to Spacemacs, this includes:
- the initialization and core files
- all the layer files
- the documentation

For the packages shipped in this repository you can refer to the files header.

[Spacemacs logo][] by [Nasser Alshammari][] released under a Creative Commons
license.

# Supporting Spacemacs

The best way to support Spacemacs is to contribute to it either by reporting
bugs, helping the community on the [Gitter Chat][] or sending pull requests.

If you want to show your support financially you can contribute to [Bountysource][] or buy a drink for the
maintainer by clicking on the [Paypal badge](#top).

If you used spacemacs in a project and you want to show that fact, you can use
the spacemacs badge: [![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://github.com/syl20bnr/spacemacs)

- For Markdown:

   ```
   [![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://github.com/syl20bnr/spacemacs)
   ```

- For HTML:

   ```
   <a href="https://github.com/syl20bnr/spacemacs"><img src="https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg" /></a>
   ```

- For Org-mode:

   ```
   [[https://github.com/syl20bnr/spacemacs][file:https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg]]
   ```

Thank you!

[Twitter]: http://i.imgur.com/tXSoThF.png
[CONTRIBUTING.org]: CONTRIBUTING.org
[CONVENTIONS.org]: http://spacemacs.org/doc/CONVENTIONS
[DOCUMENTATION.org]: http://spacemacs.org/doc/DOCUMENTATION
[QUICK_START.org]: http://spacemacs.org/doc/QUICK_START
[FAQ.org]: http://spacemacs.org/doc/FAQ
[VIMUSERS.org]: http://spacemacs.org/doc/VIMUSERS
[dotfile]: http://spacemacs.org/doc/DOCUMENTATION#orgheadline45
[osx layer]: http://spacemacs.org/layers/osx/README
[Gitter Chat]: https://gitter.im/syl20bnr/spacemacs
[Gitter Chat IRC server]: https://irc.gitter.im/
[homebrew]: http://brew.sh
[emacs-for-windows]: http://emacsbinw64.sourceforge.net/
[the platinum searcher]: https://github.com/monochromegane/the_platinum_searcher
[so-server-unsafe]: http://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
[Spacemacs logo]: https://github.com/nashamri/spacemacs-logo
[Nasser Alshammari]: https://github.com/nashamri
[cpaulik-unity-icon]: http://splendidabacus.com/posts/2015/03/spacemacs-unity-icon/
[icon-mac-instructions]: http://www.idownloadblog.com/2014/07/16/how-to-change-app-icon-mac/
[icon-repository]: https://github.com/nashamri/spacemacs-logo
[Stack Exchange]: http://emacs.stackexchange.com/questions/tagged/spacemacs
[Reddit]: https://www.reddit.com/r/spacemacs
[quote01]: https://gitter.im/syl20bnr/spacemacs?at=568e627a0cdaaa62045a7df6
[build_source]: https://www.gnu.org/software/emacs/manual/html_node/efaq/Installing-Emacs.html
[Bountysource]: https://salt.bountysource.com/teams/spacemacs
