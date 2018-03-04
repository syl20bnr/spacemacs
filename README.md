<a name="top"></a>
<a href="http://spacemacs.org"><img src="https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg" alt="Made with Spacemacs"></a><a href="http://www.twitter.com/spacemacs"><img src="http://i.imgur.com/tXSoThF.png" alt="Twitter" align="right"></a><br>
- - -
<p align="center"><img src="/doc/img/title2.png" alt="Spacemacs"/></p>
<p align="center">
<b><a href="http://spacemacs.org/doc/DOCUMENTATION#orgheadline5">philosophy</a></b>
|
<b><a href="http://spacemacs.org/doc/DOCUMENTATION#orgheadline8">for whom?</a></b>
|
<b><a href="http://spacemacs.org/doc/DOCUMENTATION#orgheadline7">screenshots</a></b>
|
<b><a href="http://spacemacs.org/doc/DOCUMENTATION.html">documentation</a></b>
|
<b><a href="CONTRIBUTING.org">contribute</a></b>
|
<b><a href="http://spacemacs.org/doc/DOCUMENTATION#achievements">achievements</a></b>
|
<b><a href="http://spacemacs.org/doc/FAQ">FAQ</a></b>
</p>

- - -

<p align="center">
<a href="https://gitter.im/syl20bnr/spacemacs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge"><img src="https://badges.gitter.im/Join Chat.svg" alt="Gitter"></a>
<a href="https://travis-ci.org/syl20bnr/spacemacs"><img src="https://travis-ci.org/syl20bnr/spacemacs.svg" alt="Build Status"></a>
<a href="https://waffle.io/syl20bnr/spacemacs"><img src="https://badge.waffle.io/syl20bnr/spacemacs.png?label=Merging...&title=Merging" alt="PR being merged"></a>
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=ESFVNPKP4Y742"><img src="https://img.shields.io/badge/Paypal-Donate-blue.svg" alt="Donate"></a>
<a href="https://shop.spreadshirt.com/spacemacs-shop"><img src="https://img.shields.io/badge/Shop-T--Shirts-blue.svg" alt="Donate"></a>
<a href="http://www.slant.co/topics/12/~what-are-the-best-programming-text-editors"><img src="https://img.shields.io/badge/Slant-Recommend-ff69b4.svg" alt="Recommend it"></a>
</p>

- - -

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
        - [macOS](#macos)
        - [Windows](#windows)
- [Install](#install)
    - [Default installation](#default-installation)
    - [Alternate installations](#alternate-installations)
        - [Modify HOME environment variable](#modify-home-environment-variable)
        - [Modify spacemacs-start-directory variable](#modify-spacemacs-start-directory-variable)
    - [Spacemacs logo](#spacemacs-logo)
- [Update](#update)
    - [Automatic update (on master branch)](#automatic-update-on-master-branch)
    - [Manual update (on master branch)](#manual-update-on-master-branch)
    - [On develop branch](#on-develop-branch)
    - [Revert to a specific version](#revert-to-a-specific-version)
- [Quotes](#quotes)
- [Contributions](#contributions)
- [Communities](#communities)
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

You can also check the [general documentation][DOCUMENTATION.org],
[quick start guide][QUICK_START.org] and the [FAQ][FAQ.org].

# Getting Help

If you need help, ask your question in the [Gitter Chat][] and a member of the
community will help you out.

If you prefer IRC, connect to the [Gitter Chat IRC server][] and join the
`#syl20bnr/spacemacs` channel.

# Prerequisites

## Emacs

Spacemacs requires Emacs 24.4 or above. The development version of Emacs (at the
time of writing, this is 25.2) is not *officially* supported, but should
nevertheless be expected to work.

Some modes require third-party tools that you'll have to install via your
favorite package manager.

### Linux distros

Install Emacs from the package manager of your Linux distribution.

You should install the "emacs" package, not the "xemacs" package.
XEmacs is an old fork of Emacs. The X in its name is unrelated to X11.
Both Emacs and XEmacs have graphical support.

**Note:** Ubuntu LTS 12.04 and 14.04 repositories have only Emacs 24.3
available. You have to [build from source][build_source] Emacs 24.4 or greater,
as Spacemacs won't work with 24.3. The same may be true for other distributions
as well.

### macOS

The recommended way of installing Emacs on macOS is using [homebrew][]:

```sh
$ brew tap d12frosted/emacs-plus
$ brew install emacs-plus
$ brew linkapps emacs-plus
```

*Note:* these homebrew commands will install GNU Emacs, and link it to your
`/Applications` directory. You still need to run the `git clone` mentioned at
the start of this file. That will populate your `~/.emacs.d` directory, which is
what transforms a regular GNU Emacs into Spacemacs.

*Note:* the proposed `emacs-plus` tap is identical to the `emacs` formulae, it
just builds GNU Emacs with support of several features by default along with
providing Spacemacs icon.
See [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus) for more
information.

*Note*: to have the title bar match your theme background color,
consider using instead:

``` sh
$ brew install emacs-plus --HEAD --with-natural-title-bars
```

*Note:* after you have completed the [install process](#install) below, it is
also recommended to add the [osx layer][] to your [dotfile][]. Install
instructions are available in the [osx layer][] documentation.

*Note:* if the powerline separators on the spaceline are a different (less
saturated) color than the rest of the line, you can add following snippet to
`dotspacemacs/user-config` in your `.spacemacs` file.

```elisp
(setq ns-use-srgb-colorspace nil)
```

Keep in mind that this is not ideal solution as it affects all colours in Emacs.
Another option is to use different powerline separator. For example, `alternate`
and `bar` diminishes the difference. And using `utf-8` separator makes it go
away completely without the need to change colour space. In order to change
powerline separator put following snippet in `dotspacemacs/user-config`.

```eslip
(setq powerline-default-separator 'utf-8)
```

For more information about powerline separators, please refer to appropriate
section in [Documentation][DOCUMENTATION.org].

### Windows

You can download good quality builds from the [emacs-w64 project][emacs-for-windows].
It is recommended to install the most recent [stable build][emacs-for-windows-stable].

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

Source: [Stack Overflow][so-server-unsafe]

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

3. (Optional) Install the [Source Code Pro][] font.

   If you are running in terminal you'll also need to change font settings of
   your terminal.

4. Launch Emacs. Spacemacs will automatically install the packages it requires.
   If you get an error regarding package downloads then you may try to disable
   the HTTPS protocol by starting Emacs with

   ```sh
   emacs --insecure
   ```

   Or you can set the `dotspacemacs-elpa-https` to `nil` in your dotfile to
   remove the need to start Emacs with `--insecure` argument. You may wish to
   clear out your `.emacs.d/elpa` directory before doing this, so that any
   corrupted packages you may have downloaded will be re-installed.

5. Restart Emacs to complete the installation.

If the mode-line turns red then be sure to consult the [FAQ][FAQ.org].

## Alternate installations

It may be useful to clone Spacemacs outside Emacs dotdirectory `~/.emacs.d` so
you can try Spacemacs without replacing completely our own configuration.
There is currently two possibilities to support alternative location for
Spacemacs configuration.

### Modify HOME environment variable

This solution is ideal to quickly try Spacemacs without compromising your
existing configuration.

```sh
mkdir ~/spacemacs
git clone https://github.com/syl20bnr/spacemacs.git ~/spacemacs/.emacs.d
HOME=~/spacemacs emacs
```

Note: If you're on Fish shell, you will need to modify the last command to: `env
HOME=$HOME/spacemacs emacs`

### Modify spacemacs-start-directory variable

This solution is better suited to "embed" Spacemacs into your own configuration.
Say you cloned Spacemacs in `~/.emacs.d/spacemacs/` then drop these lines in
`~/.emacs.d/init.el`:

```elisp
(setq spacemacs-start-directory "~/.emacs.d/spacemacs/")
(load-file (concat spacemacs-start-directory "init.el"))
```

## Spacemacs logo

For Ubuntu users, follow this guide to
[change the logo in Unity][cpaulik-unity-icon].

For Mac users, you need to [download the .icns version of the logo][icon-repository],
then [change the logo on Dock][icon-mac-instructions].

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

[Quote][quote02] by [deuill](https://github.com/deuill):

    «I LOVE SPACEMACS AND MAGIT

     That is all»

# Contributions

Spacemacs is a community-driven project, it needs _you_ to keep it up to
date and propose great and useful configuration for all the things!

Before contributing be sure to consult the
[contribution guidelines][CONTRIBUTING.org] and [conventions][CONVENTIONS.org].

Here is a throughput graph of the repository for the last few weeks:

[![Throughput Graph](https://graphs.waffle.io/syl20bnr/spacemacs/throughput.svg)](https://waffle.io/syl20bnr/spacemacs/metrics)

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

[Spacemacs logo][] by [Nasser Alshammari][] released under a [Creative Commons Attribution-ShareAlike 4.0 International License.](http://creativecommons.org/licenses/by-sa/4.0/)

# Supporting Spacemacs

The best way to support Spacemacs is to contribute to it either by reporting
bugs, helping the community on the [Gitter Chat][] or sending pull requests.

You can show your love for the project by getting cool Spacemacs t-shirts, mugs
and more in the [Spacemacs Shop][].

If you want to show your support financially you can contribute to [Bountysource][] or buy a drink for the
maintainer by clicking on the [Paypal badge](#top).

If you used spacemacs in a project and you want to show that fact, you can use
the spacemacs badge: [![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)

- For Markdown:

   ```
   [![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)
   ```

- For HTML:

   ```
   <a href="http://spacemacs.org"><img src="https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg" /></a>
   ```

- For Org-mode:

   ```
   [[http://spacemacs.org][file:https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg]]
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
[osx layer]: http://spacemacs.org/layers/+os/osx/README.html
[Gitter Chat]: https://gitter.im/syl20bnr/spacemacs
[Gitter Chat IRC server]: https://irc.gitter.im/
[homebrew]: http://brew.sh
[emacs-for-windows]: http://emacsbinw64.sourceforge.net/
[emacs-for-windows-stable]: https://sourceforge.net/projects/emacsbinw64/files/release/
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
[quote02]: https://gitter.im/syl20bnr/spacemacs?at=5768456c6577f032450cfedb
[build_source]: https://www.gnu.org/software/emacs/manual/html_node/efaq/Installing-Emacs.html
[Bountysource]: https://salt.bountysource.com/teams/spacemacs
[Source Code Pro]: https://github.com/adobe-fonts/source-code-pro
[Spacemacs Shop]: https://shop.spreadshirt.com/spacemacs-shop
