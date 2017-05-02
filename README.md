<a name="top" id="fork-destination-box"></a>
<a href="http://spacemacs.org"><img src="https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg" alt="Made with Spacemacs"></a><a href="http://www.twitter.com/spacemacs"><img src="http://i.imgur.com/tXSoThF.png" alt="Twitter" align="right"></a><br>
***
<p align="center"><img src="/doc/img/title2.png" alt="Spacemacs"/></p>
<p align="center">
<b><a href="http://spacemacs.org/doc/DOCUMENTATION#core-pillars">philosophy</a></b>
|
<b><a href="http://spacemacs.org/doc/DOCUMENTATION#who-can-benefit-from-this">for whom?</a></b>
|
<b><a href="http://spacemacs.org/doc/DOCUMENTATION#screenshots">screenshots</a></b>
|
<b><a href="http://spacemacs.org/doc/DOCUMENTATION.html">documentation</a></b>
|
<b><a href="CONTRIBUTING.org">contribute</a></b>
|
<b><a href="http://spacemacs.org/doc/DOCUMENTATION#achievements">achievements</a></b>
|
<b><a href="http://spacemacs.org/doc/FAQ">FAQ</a></b>
</p>
***
<p align="center">
<a href="https://gitter.im/syl20bnr/spacemacs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge"><img src="https://badges.gitter.im/Join Chat.svg" alt="Gitter"></a>
<a href="https://travis-ci.org/syl20bnr/spacemacs"><img src="https://travis-ci.org/syl20bnr/spacemacs.svg" alt="Build Status"></a>
<a href="https://waffle.io/syl20bnr/spacemacs"><img src="https://badge.waffle.io/syl20bnr/spacemacs.png?label=Merging...&title=Merging" alt="PR being merged"></a>
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=ESFVNPKP4Y742"><img src="https://img.shields.io/badge/Paypal-Donate-blue.svg" alt="Donate"></a>
<a href="https://shop.spreadshirt.com/spacemacs-shop"><img src="https://img.shields.io/badge/Shop-T--Shirts-blue.svg" alt="Donate"></a>
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
        - [macOS](#macos)
        - [Windows](#windows)
- [Install](#install)
    - [Default installation](#default-installation)
    - [Alternative installations](#alternative-installations)
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
Spacemacs is a new way of experiencing Emacs -- it's a sophisticated and
polished set-up, focused on ergonomics, mnemonics and consistency.

Just clone and launch it, then press the space bar to explore the interactive
list of carefully-chosen key bindings. You can also press the home buffer's
`[?]` button for some great first key bindings to try.

Spacemacs can be used naturally by both Emacs and Vim users -- you can even mix
the two editing styles. Being able to quickly switch between input styles, makes
Spacemacs a great tool for pair-programming.

Spacemacs is currently in beta, and any contributions are very welcome.

![spacemacs_python](doc/img/spacemacs-python.png)

# Features
- **Great documentation:** access the Spacemacs documentation with
<kbd>SPC h SPC</kbd>.
- **Beautiful GUI:** you'll love the distraction free UI and its functional
mode-line.
- **Excellent ergonomics:** all the key bindings are accessible by pressing the
<kbd>space bar</kbd> or <kbd>alt-m</kbd>.
- **Mnemonic key bindings:** commands have mnemonic prefixes like
<kbd>SPC b</kbd> for all the buffer commands or <kbd>SPC p</kbd> for the project
commands.
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
Spacemacs is an extension of a popular text editor called Emacs. Thus you need
to first install base Emacs and then download the Spacemacs extension files with
Git.

## Emacs
Spacemacs requires Emacs 24.4 or above. The development version of Emacs is not
*officially* supported, but it should nevertheless be expected to work.

Some modes require third-party tools that you'll have to install via your
favorite package manager.

### Linux distros
Install the `emacs` package with your linux distributions package manager.

Do not install the `xemacs` package. It's not supported by Spacemacs. XEmacs is
an old fork of Emacs. The X in its name is unrelated to X11.

Emacs has graphical support.

**Note:** The Ubuntu LTS 12.04 and 14.04 repositories only have Emacs version
24.3 available. Version 24.4 or greater needs to be [built from source][build_source].
This might also be true for other linux distributions.

### macOS
The recommended way of installing Emacs on macOS is using [Homebrew][]. It's a
package manager for macOS. Once Homebrew is installed, run the following
commands in the terminal to install both Emacs and the default Source Code Pro
font:

```sh
brew tap d12frosted/emacs-plus
brew install emacs-plus
brew linkapps emacs-plus
brew tap caskroom/fonts
brew cask install font-source-code-pro
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
```

**Notes:**
The proposed `emacs-plus` tap is identical to the `emacs` formulae, it just
builds GNU Emacs with support for several features by default, including the
Spacemacs icon. See [emacs-plus][] for more information.

After completing the Spacemacs [install process](#install), then it's also
recommended to add the [osx layer][] to your [dotfile][]. Installation
instructions are available in the documentation for the [osx layer][].

If the powerline separators on the spaceline have a different (less saturated)
color than the rest of the line, then you can add the following snippet to the
`dotspacemacs/user-config` section in your `~/.spacemacs` file:

```elisp
(setq ns-use-srgb-colorspace nil)
```

Keep in mind that this is not an ideal solution as it affects all colours in
Emacs. Another option is to change the powerline separators. For example to
`alternate` or `bar`, they diminish the color difference. Or change them to
`utf-8`, which makes them go away completely, without having to change colour
space. In order to change the powerline separators, put the following snippet in
the `dotspacemacs/user-config` section of your `~/.spacemacs` file:

```eslip
(setq powerline-default-separator 'utf-8)
```

For more information about powerline separators, please refer to the
[powerline documentation][]

### Windows
You can download good quality builds from the [emacs-w64 project][emacs-for-windows].
It is recommended to install the most recent [stable build][emacs-for-windows-stable].

Be sure to declare an environment variable named `HOME` that's pointing to your
user directory `C:\Users\<username>`. Then you can clone Spacemacs into that
directory.

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

For efficient searches we recommend installing `pt` ([the platinum searcher][]).
`pt` version 1.7.7 or higher is required.

# Install
## Default installation
1. If you have an existing Emacs configuration, back it up first by running the
   following code in your terminal:

   ```sh
   cd ~
   mv .emacs.d .emacs.d.bak
   mv .emacs .emacs.bak
   ```

   Don't forget to backup and *remove* the `~/.emacs` file. Otherwise Spacemacs
   **WILL NOT** be able to load. Because that file prevents Emacs from loading
   the proper initialization file.

2. Clone the repository with [Git][]:

   ```sh
   git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
   ```

   `master` is the stable branch and it is _immutable_, **DO NOT** make any
   modification to it or you will break the update mechanism. If you want to
   fork Spacemacs safely, use the `develop` branch where you'll handle updates
   manually.

   **Note for Windows users**
   If you use windows, then you'll have to modify the git command by inserting
   the correct path to your `.emacs.d` folder. The dot before the folder means
   that it's hidden. You'll have to search for hidden files to find the folder.
   When you have found the folder, substitute the original path with the correct
   one. The proper code would look something like this:

   ```sh
   git clone https://github.com/syl20bnr/spacemacs /path/to/your/.emacs.d
   ```

3. Install the default fonts

   It's recommended to install [Source Code Pro][] by Adobe, as the default
   font. It ensures that, for example the symbols on the modeline (bottom bar)
   looks correct. It's also recommended to use a "Fallback font". These
   depend on the system:

   - GNU/Linux: *NanumGothic* (package named *fonts-nanum* on Debian, for
     example)
   - macOS: *Arial Unicode MS*
   - Windows: *MS Gothic* or *Lucida Sans Unicode*

   If the modeline doesn't look similar to the
   [picture at the top of this page](#introduction), then make sure you have the
   correct fallback font installed.

   If you're running in a terminal then you'll also need to change the terminals
   font settings.

4. Launch Emacs, and answer the questions in the Dotfile wizard installer. If
   you are new to Emacs and Spacemacs, then it's fine to just accept the default
   choices. It's easy to try the other choices later, without having to
   reinstall Spacemacs. They can be changed in the dotfile `~/.spacemacs`.

   After answering the questions, then Spacemacs starts downloading and
   installing the packages it requires. When the packages have been installed.
   Restart Emacs to complete the installation.

**Notes:**
If you get an error regarding package downloads, then you can try to
disable the HTTPS protocol by starting Emacs with the `--insecure` argument:

```sh
emacs --insecure
```

Or you can set the `dotspacemacs-elpa-https` variable to `nil` in your
dotfile `~/.spacemacs`. That will remove the need to start Emacs with the
`--insecure` argument. You may also want to clear out your `.emacs.d/elpa`
directory before doing this, so that any corrupted packages you may have
downloaded will be re-installed.

`error: Package 'package-build-' is unavailable` may occur due to heavy network
taffic. You can fix it by setting the `dotspacemacs-elpa-timeout` variable to
`70` in your dotfile.

`Warning (bytecomp)` and other compilation warnings are perfectly normal. If
you're curious, you can find out why these occur [here][compilation-warnings].

If the mode-line turns red then be sure to consult the [FAQ][FAQ.org].

## Alternative installations
There are currently, two supported, alternative locations, for a Spacemacs
configuration.

### Modify HOME environment variable
This solution is ideal for quickly trying Spacemacs without compromising your
existing configuration. Clone Spacemacs outside the Emacs dotdirectory
`~/.emacs.d` and modify the HOME environment variable.

```sh
mkdir ~/spacemacs
git clone https://github.com/syl20bnr/spacemacs.git ~/spacemacs/.emacs.d
HOME=~/spacemacs emacs
```

Note: If you're using the Fish shell, then you'll need to modify the last
command to: `env HOME=$HOME/spacemacs emacs`

### Modify spacemacs-start-directory variable
This solution is better suited to "embed" Spacemacs into your own configuration.
If you've cloned Spacemacs into `~/.emacs.d/spacemacs/`, then drop the following
lines in the `~/.emacs.d/init.el` file:

```elisp
(setq spacemacs-start-directory "~/.emacs.d/spacemacs/")
(load-file (concat spacemacs-start-directory "init.el"))
```

## Spacemacs logo
For Ubuntu users, follow this guide to
[change the logo in Unity][cpaulik-unity-icon].

For Mac users, you need to [download the .icns version of the logo][icon-repository],
then [change the logo on the Dock][icon-mac-instructions].

# Update
Spacemacs shows a notification when a new version is available (only when you
are on the default `master` branch). If you are on the `develop` branch then
you'll have to update the Spacemacs repository manually.

## Automatic update (on master branch)
When a new version is available then a little arrow appears in the mode-line.

Its color depends on the number of versions that have been released since your
last update. Green means that you have a recent version, orange and red means
that you have an older version.

![powerline_update](doc/img/powerline-update.png)

Click on the arrow to update Spacemacs to the latest version.

## Manual update (on master branch)
Remove the `<` and `>` angle brackets when you're typing the lines below into
your shell. And replace the text: "tag version which you are updating to" with a
tagged version. This page lists the [latest tags][]

```sh
git fetch
git reset --hard <tag version which you are updating to>
```

## On develop branch
1. Close Emacs and update the git repository:

   ```sh
   git pull --rebase
   ```

2. Restart Emacs to complete the upgrade.

## Revert to a specific version
To revert to a specific version, just checkout the corresponding branch. For
instance to revert to version `0.200`, type the following command:

   ```sh
   git checkout origin/release-0.200
   ```

**After updating Spacemacs (either manually or automatically), then you also
should check if any updates are available for your packages. On the Spacemacs
Home Buffer `SPC b h`, click (press `RET`) on the `[Update Packages]` button.**

# Quotes
[Quote][quote01] by [ashnur](https://github.com/ashnur):

    «I feel that spacemacs is an aircraft carrier and I am playing table tennis
    on the deck as a freerider.»

[Quote][quote02] by [deuill](https://github.com/deuill):

    «I LOVE SPACEMACS AND MAGIT

     That is all»

# Contributions
Spacemacs is a community-driven project, it needs _you_ to keep it up to date
and to propose great and useful configurations for all the things!

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

For the packages shipped in this repository, you can refer to the files header.

[Spacemacs logo][] by [Nasser Alshammari][] released under a
[Creative Commons Attribution-ShareAlike 4.0 International License.](http://creativecommons.org/licenses/by-sa/4.0/)

# Supporting Spacemacs
The best way to support Spacemacs is to contribute to it either by reporting
bugs, helping the community on the [Gitter Chat][] or sending pull requests.

You can show your love for the project by getting cool Spacemacs t-shirts, mugs
and more in the [Spacemacs Shop][].

If you want to show your support financially, then you can contribute to
[Bountysource][], or buy a drink for the maintainer by clicking on the
[Paypal badge](#top).

If you used spacemacs in a project, and you want to show that fact, you can use
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
[dotfile]: http://spacemacs.org/doc/DOCUMENTATION#dotfile-configuration
[osx layer]: http://spacemacs.org/layers/+os/osx/README.html
[Gitter Chat]: https://gitter.im/syl20bnr/spacemacs
[Gitter Chat IRC server]: https://irc.gitter.im/
[Homebrew]: http://brew.sh
[emacs-plus]: https://github.com/d12frosted/homebrew-emacs-plus
[powerline documentation]: http://spacemacs.org/doc/DOCUMENTATION.html#powerline-separators
[emacs-for-windows]: http://emacsbinw64.sourceforge.net/
[emacs-for-windows-stable]: https://sourceforge.net/projects/emacsbinw64/files/release/
[the platinum searcher]: https://github.com/monochromegane/the_platinum_searcher
[so-server-unsafe]: http://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
[Spacemacs logo]: https://github.com/nashamri/spacemacs-logo
[Nasser Alshammari]: https://github.com/nashamri
[compilation-warnings]: https://github.com/quelpa/quelpa/issues/90#issuecomment-137982713
[cpaulik-unity-icon]: http://splendidabacus.com/posts/2015/03/spacemacs-unity-icon/
[icon-mac-instructions]: http://www.idownloadblog.com/2014/07/16/how-to-change-app-icon-mac/
[latest tags]: https://github.com/syl20bnr/spacemacs/tags
[icon-repository]: https://github.com/nashamri/spacemacs-logo
[Stack Exchange]: http://emacs.stackexchange.com/questions/tagged/spacemacs
[Reddit]: https://www.reddit.com/r/spacemacs
[quote01]: https://gitter.im/syl20bnr/spacemacs?at=568e627a0cdaaa62045a7df6
[quote02]: https://gitter.im/syl20bnr/spacemacs?at=5768456c6577f032450cfedb
[build_source]: https://www.gnu.org/software/emacs/manual/html_node/efaq/Installing-Emacs.html
[Bountysource]: https://salt.bountysource.com/teams/spacemacs
[Source Code Pro]: https://github.com/adobe-fonts/source-code-pro
[Spacemacs Shop]: https://shop.spreadshirt.com/spacemacs-shop
[Git]: https://git-scm.com/downloads
