<a name="top" id="fork-destination-box"></a>
<a href="http://space-macs.org"><img src="https://cdn.rawgit.com/syl20bnr/space-macs/442d025779da2f62fc86c2082703697714db6514/assets/space-macs-badge.svg" alt="Made with Space-macs"></a><a href="http://www.twitter.com/space-macs"><img src="http://i.imgur.com/tXSoThF.png" alt="Twitter" align="right"></a><br>
- - -
<p align="center"><img src="/doc/img/title2.png" alt="Space-macs"/></p>
<p align="center">
<b><a href="http://space-macs.org/doc/DOCUMENTATION#core-pillars">philosophy</a></b>
|
<b><a href="http://space-macs.org/doc/DOCUMENTATION#who-can-benefit-from-this">for whom?</a></b>
|
<b><a href="http://space-macs.org/doc/DOCUMENTATION#screenshots">screenshots</a></b>
|
<b><a href="http://space-macs.org/doc/DOCUMENTATION.html">documentation</a></b>
|
<b><a href="CONTRIBUTING.org">contribute</a></b>
|
<b><a href="http://space-macs.org/doc/DOCUMENTATION#achievements">achievements</a></b>
|
<b><a href="http://space-macs.org/doc/FAQ">FAQ</a></b>
</p>

- - -

<p align="center">
<a href="https://gitter.im/syl20bnr/space-macs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge"><img src="https://badges.gitter.im/Join Chat.svg" alt="Gitter"></a>
<a href="https://circleci.com/gh/syl20bnr/space-macs/tree/develop"><img src="https://circleci.com/gh/syl20bnr/space-macs/tree/develop.svg?style=shield" alt="Build Status"></a>
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=ESFVNPKP4Y742"><img src="https://img.shields.io/badge/Paypal-Donate-blue.svg" alt="Donate"></a>
<a href="https://shop.spreadshirt.com/space-macs-shop"><img src="https://img.shields.io/badge/Shop-T--Shirts-blue.svg" alt="Donate"></a>
<a href="http://www.slant.co/topics/12/~what-are-the-best-programming-text-editors"><img src="https://img.shields.io/badge/Slant-Recommend-ff69b4.svg" alt="Recommend it"></a>
</p>

- - -


**Quick Install:**

This assumes you don't have an existing e-macs setup and want to run Space-macs as
your config. If you do have one, look at
the [full installation instructions](#install) for other options.

* For stable releases:
  ```shell
  git clone https://github.com/syl20bnr/space-macs ~/.e-macs.d
  ```

* For development updates and participation:
  ```shell
  git clone -b develop https://github.com/syl20bnr/space-macs ~/.e-macs.d
  ```

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Introduction](#introduction)
- [Features](#features)
- [Documentation](#documentation)
- [Getting Help](#getting-help)
- [Prerequisites](#prerequisites)
    - [e-macs](#e-macs)
        - [Linux distros](#linux-distros)
        - [macOS](#macos)
        - [Windows](#windows)
- [Install](#install)
    - [Default installation](#default-installation)
    - [Alternative installations](#alternative-installations)
        - [Modify HOME environment variable](#modify-home-environment-variable)
        - [Modify space-macs-start-directory variable](#modify-space-macs-start-directory-variable)
    - [Space-macs logo](#space-macs-logo)
- [Update](#update)
    - [Automatic update (on master branch)](#automatic-update-on-master-branch)
    - [Manual update (on master branch)](#manual-update-on-master-branch)
    - [On develop branch](#on-develop-branch)
    - [Revert to a specific version](#revert-to-a-specific-version)
- [Quotes](#quotes)
- [Contributions](#contributions)
- [Communities](#communities)
- [Space-macs Everywhere](#space-macs-everywhere)
- [License](#license)
- [Supporting Space-macs](#supporting-space-macs)

<!-- markdown-toc end -->

# Introduction
Space-macs is a new way of experiencing e-macs -- it's a sophisticated and
polished set-up, focused on ergonomics, mnemonics and consistency.

Just clone and launch it, then press the space bar to explore the interactive
list of carefully-chosen key bindings. You can also press the home buffer's
`[?]` button for some great first key bindings to try.

Space-macs can be used naturally by both e-macs and Vim users -- you can even mix
the two editing styles. Being able to quickly switch between input styles, makes
Space-macs a great tool for pair-programming.

Space-macs is currently in beta, and any contributions are very welcome.

![space-macs_python](doc/img/space-macs-python.png)

# Features
- **Great documentation:** access the Space-macs documentation with
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
`#syl20bnr/space-macs` channel.

Last but not least there are a lot of high class tutorials available on YouTube:
* Jack of Some's Space-macs related channel [here](https://www.youtube.com/watch?v=r-BHx7VNX5s&list=PLd_Oyt6lAQ8Rxb0HUnGbRrn6R4Cdt2yoI)
* GDQuest's Game Design oriented tutorials to Space-macs [here](https://www.youtube.com/watch?v=hCNOB5jjtmc&list=PLhqJJNjsQ7KFkMVBunWWzFD8SlH714qm4)
* Practicalli's Clojure tutorials based on Space-macs [here](https://www.youtube.com/watch?v=ZKuQDrQLAnc&list=PLpr9V-R8ZxiCHMl2_dn1Fovcd34Oz45su)
* Eivind Fonn's classic Space-macs ABC [here](https://www.youtube.com/watch?v=ZFV5EqpZ6_s&list=PLrJ2YN5y27KLhd3yNs2dR8_inqtEiEweE)

# Prerequisites
Space-macs is an extension of a popular text editor called e-macs. Thus you need
to first install base e-macs and then download the Space-macs extension files with
Git.

## e-macs
Space-macs requires e-macs 25.1 or above. The development version of e-macs is not
*officially* supported, but it should nevertheless be expected to work.

Some modes require third-party tools that you'll have to install via your
favorite package manager.

### Linux distros
Install the `e-macs` package with your linux distributions package manager.

Do not install the `xe-macs` package. It's not supported by Space-macs. Xe-macs is
an old fork of e-macs. The X in its name is unrelated to X11.

e-macs has graphical support.

**Note:** The Ubuntu LTS 12.04 and 14.04 repositories only have e-macs version
24.3 available. Version 25.1 or greater needs to be [built from source][build_source].
This might also be true for other linux distributions.

### macOS

#### Install e-macs

##### Using e-macs-plus

```
brew tap d12frosted/e-macs-plus
# to install e-macs 26
brew install e-macs-plus
# or to install e-macs 27
brew install e-macs-plus@27 --with-space-macs-icon
# or to install e-macs 28
brew install e-macs-plus@28 --with-space-macs-icon
brew link e-macs-plus
```

##### Using e-macs-mac

```
brew tap railwaycat/e-macsmacport
brew install e-macs-mac
brew link e-macs-mac
```

##### Using cask

Homebrew now recommends to use the cask version with the following message:
"Please try the Cask for a better-supported Cocoa version". To install the cask
version:

```
brew cask install e-macs
```

This installs a pre-built package from https://e-macsformacosx.com/

##### Other ways

If you're not comfortable with the ways mentioned above, then
[e-macsWiki](https://www.e-macswiki.org/e-macs/e-macsForMacOS#toc12) lists down
a few ways to install e-macs for macOS.

#### Install Source Code Pro font

Once e-macs is installed, run the following commands in the terminal to install
the default Source Code Pro font:

```sh
brew tap homebrew/cask-fonts
brew cask install font-source-code-pro
```

#### Install Space-macs

```
git clone https://github.com/syl20bnr/space-macs ~/.e-macs.d
```

**Notes:**
After completing the Space-macs [install process](#install), then it's also
recommended to add the [osx layer][] to your [dotfile][]. Installation
instructions are available in the documentation for the [osx layer][].

Depending on the installed version of GnuTLS securely installing e-macs
packages may fail. In this case it is possible to install using
`e-macs --insecure`. However be aware that this means your packages will
be transferred using http, use at your own risk.

You might also have some issues when doing some search on your projects, you
probably want to install grep through homebrew with default names:

```sh
$ brew install grep --with-default-names
```

### Windows
Download the official 64-bit (x86_64) stable builds from the [GNU FTP][e-macs-for-windows].

You'll also need `gzip` and put it in your path, to download it go to the
[GNUWin32 project page][gzip-for-windows]

Be sure to declare an environment variable named `HOME` that's pointing to your
user directory `C:\Users\<username>`. Then you can clone Space-macs into that
directory.

If the following error occurs after starting e-macs:

```
The directory ~/.e-macs.d/server is unsafe
```

Fix it by changing the owner of the directory `~/.e-macs.d/server`:
  - from Properties select the Tab â€œSecurityâ€,
  - select the button â€œAdvancedâ€,
  - select the Tab â€œOwnerâ€
  - change the owner to your account name

Source: [Stack Overflow][so-server-unsafe]

For efficient searches we recommend installing `pt` ([the platinum searcher][]).
`pt` version 1.7.7 or higher is required.

**Notes:**
Depending on the installed version of GnuTLS securely installing e-macs
packages may fail. In this case it is possible to install using
`e-macs --insecure`. However be aware that this means your packages will
be transferred using http, use at your own risk.

# Install
## Default installation
1. If you have an existing e-macs configuration, back it up first by running the
   following code in your terminal:

   ```sh
   cd ~
   mv .e-macs.d .e-macs.d.bak
   mv .e-macs .e-macs.bak
   ```

   Don't forget to backup and *remove* the `~/.e-macs` file. Otherwise Space-macs
   **WILL NOT** be able to load. Because that file prevents e-macs from loading
   the proper initialization file.

2. Clone the repository with [Git][]:

   ```sh
   git clone https://github.com/syl20bnr/space-macs ~/.e-macs.d
   ```

   Or

   ```sh
   git clone --depth 1 https://github.com/syl20bnr/space-macs ~/.e-macs.d
   ```

   In case you have a limited internet connection or speed.

   `master` is the stable branch and it is _immutable_, **DO NOT** make any
   modification to it or you will break the update mechanism. If you want to
   fork Space-macs safely, use the `develop` branch where you'll handle updates
   manually.

   **Note for Windows users**
   If you use windows, then you'll have to modify the git command by inserting
   the correct path to your `.e-macs.d` folder. The dot before the folder means
   that it's hidden. You'll have to search for hidden files to find the folder.
   When you have found the folder, substitute the original path with the correct
   one. The proper code would look something like this:

   ```sh
   git clone https://github.com/syl20bnr/space-macs /path/to/your/.e-macs.d
   ```

3. (Optional) Install the default fonts

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

4. Launch e-macs. Space-macs will automatically install the packages it requires.
   There is a well-known issue with some GPG keys having expired end of 2019.
   This can be fixed by upgrading to e-macs 26.3 or above or by manually adding
   the new keys using something like:
   ```sh
   gpg --homedir ~/.e-macs.d/elpa/gnupg --receive-keys 066DAFCB81E42C40
   ```
   If you have a restrictive firewall it may help to manually specify the keyserver:
   ```sh
   gpg --keyserver keyserver.ubuntu.com --homedir ~/.e-macs.d/elpa/gnupg/ --receive-keys 066DAFCB81E42C40
   ```

5. Launch e-macs, and answer the questions in the Dotfile wizard installer. If
   you are new to e-macs and Space-macs, then it's fine to just accept the default
   choices. It's easy to try the other choices later, without having to
   reinstall Space-macs. They can be changed in the dotfile `~/.space-macs`.

   After answering the questions, then Space-macs starts downloading and
   installing the packages it requires. When the packages have been installed.
   Restart e-macs to complete the installation.

**Notes:**
If you are behind a firewall or similar and you get an error regarding package
downloads then you may try to disable the HTTPS protocol by starting e-macs with
```sh
e-macs --insecure
```
but this should be a last resort because of the security implications.

You can set the `dotspace-macs-elpa-https` variable to `nil` in your
dotfile `~/.space-macs` but this has the same security implications as the
insecure flag. You may also want to clear out your `.e-macs.d/elpa`
directory before doing this, so that any corrupted packages you may have
downloaded will be re-installed.

`error: Package 'package-build-' is unavailable` may occur due to heavy network
taffic. You can fix it by setting the `dotspace-macs-elpa-timeout` variable to
`70` in your dotfile.

`Warning (bytecomp)` and other compilation warnings are perfectly normal. If
you're curious, you can find out why these occur [here][compilation-warnings].

If the mode-line turns red then be sure to consult the [FAQ][FAQ.org].

## Alternative installations
There are currently, two supported, alternative locations, for a Space-macs
configuration.

### Modify HOME environment variable
This solution is ideal for quickly trying Space-macs without compromising your
existing configuration. Clone Space-macs outside the e-macs dotdirectory
`~/.e-macs.d` and modify the HOME environment variable.

```sh
mkdir ~/space-macs
git clone https://github.com/syl20bnr/space-macs.git ~/space-macs/.e-macs.d
HOME=~/space-macs e-macs
```

Note: If you're using the Fish shell, then you'll need to modify the last
command to: `env HOME=$HOME/space-macs e-macs`

### Modify space-macs-start-directory variable
This solution is better suited to "embed" Space-macs into your own configuration.
If you've cloned Space-macs into `~/.e-macs.d/space-macs/`, then drop the following
lines in the `~/.e-macs.d/init.el` file:

```elisp
(setq space-macs-start-directory "~/.e-macs.d/space-macs/")
(load-file (concat space-macs-start-directory "init.el"))
```

## Space-macs logo
For Ubuntu users, follow this guide to
[change the logo in Unity][cpaulik-unity-icon].

For macOS users, you need to [download the .icns version of the logo][icon-repository],
then [change the logo on the Dock][icon-mac-instructions].

# Update
Space-macs shows a notification when a new version is available (only when you
are on the default `master` branch). If you are on the `develop` branch then
you'll have to update the Space-macs repository manually.

## Automatic update (on master branch)
When a new version is available then a little arrow appears in the mode-line.

Its color depends on the number of versions that have been released since your
last update. Green means that you have a recent version, orange and red means
that you have an older version.

![powerline_update](doc/img/powerline-update.png)

Click on the arrow to update Space-macs to the latest version.

## Manual update (on master branch)
Remove the `<` and `>` angle brackets when you're typing the lines below into
your shell. And replace the text: "tag version which you are updating to" with a
tagged version. This page lists the [latest tags][]

```sh
git fetch
git reset --hard <tag version which you are updating to>
```

## On develop branch
1. Close e-macs and update the git repository:

   ```sh
   git pull --rebase
   ```

2. Restart e-macs to complete the upgrade.

## Revert to a specific version
To revert to a specific version, just checkout the corresponding branch. For
instance to revert to version `0.200`, type the following command:

   ```sh
   git checkout origin/release-0.200
   ```

**After updating Space-macs (either manually or automatically), then you also
should check if any updates are available for your packages. On the Space-macs
Home Buffer `SPC b h`, click (press `RET`) on the `[Update Packages]` button.**

# Quotes
[Quote][quote01] by [ashnur](https://github.com/ashnur):

    Â«I feel that space-macs is an aircraft carrier and I am playing table tennis
    on the deck as a freerider.Â»

[Quote][quote02] by [deuill](https://github.com/deuill):

    Â«I LOVE SPACe-macs AND MAGIT

     That is allÂ»

# Contributions
Space-macs is a community-driven project, it needs _you_ to keep it up to date
and to propose great and useful configurations for all the things!

Before contributing be sure to consult the
[contribution guidelines][CONTRIBUTING.org] and [conventions][CONVENTIONS.org].

# Communities
- [Gitter Chat]
- [Stack Exchange]
- [Reddit]

# Space-macs Everywhere

Once you've learned the Space-macs key bindings, you can use them in other IDEs/tools, thanks to the following projects:
- [Intellimacs](https://github.com/MarcoIeni/intellimacs) - Space-macs' like key bindings for IntelliJ platform
- [Spaceclipse](https://github.com/MarcoIeni/spaceclipse) - Space-macsâ€™ like key bindings for Eclipse
- [SpaceVim](https://github.com/SpaceVim/SpaceVim) - A community-driven modular vim distribution
- [VSpaceCode](https://github.com/VSpaceCode/VSpaceCode) - Space-macsâ€™ like key bindings for Visual Studio Code

# License
The license is GPLv3 for all parts specific to Space-macs, this includes:
- the initialization and core files
- all the layer files
- the documentation

For the packages shipped in this repository, you can refer to the files header.

[Space-macs logo][] by [Nasser Alshammari][] released under a
[Creative Commons Attribution-ShareAlike 4.0 International License.](http://creativecommons.org/licenses/by-sa/4.0/)

# Supporting Space-macs
The best way to support Space-macs is to contribute to it either by reporting
bugs, helping the community on the [Gitter Chat][] or sending pull requests.

You can show your love for the project by getting cool Space-macs t-shirts, mugs
and more in the [Space-macs Shop][].

If you want to show your support financially, then you can contribute to
[Bountysource][], or buy a drink for the maintainer by clicking on the
[Paypal badge](#top).

If you used space-macs in a project, and you want to show that fact, you can use
the space-macs badge: [![Built with Space-macs](https://cdn.rawgit.com/syl20bnr/space-macs/442d025779da2f62fc86c2082703697714db6514/assets/space-macs-badge.svg)](http://space-macs.org)

- For Markdown:

   ```markdown
   [![Built with Space-macs](https://cdn.rawgit.com/syl20bnr/space-macs/442d025779da2f62fc86c2082703697714db6514/assets/space-macs-badge.svg)](http://space-macs.org)
   ```

- For HTML:

   ```html
   <a href="http://space-macs.org"><img alt="Built with Space-macs" src="https://cdn.rawgit.com/syl20bnr/space-macs/442d025779da2f62fc86c2082703697714db6514/assets/space-macs-badge.svg" /></a>
   ```

- For Org-mode:

   ```org
   [[http://space-macs.org][file:https://cdn.rawgit.com/syl20bnr/space-macs/442d025779da2f62fc86c2082703697714db6514/assets/space-macs-badge.svg]]
   ```

Thank you!

[Twitter]: http://i.imgur.com/tXSoThF.png
[CONTRIBUTING.org]: CONTRIBUTING.org
[CONVENTIONS.org]: http://space-macs.org/doc/CONVENTIONS
[DOCUMENTATION.org]: http://space-macs.org/doc/DOCUMENTATION
[QUICK_START.org]: http://space-macs.org/doc/QUICK_START
[FAQ.org]: http://space-macs.org/doc/FAQ
[VIMUSERS.org]: http://space-macs.org/doc/VIMUSERS
[dotfile]: http://space-macs.org/doc/DOCUMENTATION#dotfile-configuration
[osx layer]: http://space-macs.org/layers/+os/osx/README.html
[Gitter Chat]: https://gitter.im/syl20bnr/space-macs
[Gitter Chat IRC server]: https://irc.gitter.im/
[Homebrew]: http://brew.sh
[e-macs-plus]: https://github.com/d12frosted/homebrew-e-macs-plus
[powerline documentation]: http://space-macs.org/doc/DOCUMENTATION.html#powerline-separators
[e-macs-for-windows]: https://ftp.gnu.org/gnu/e-macs/windows/
[gzip-for-windows]: http://gnuwin32.sourceforge.net/packages/gzip.htm
[the platinum searcher]: https://github.com/monochromegane/the_platinum_searcher
[so-server-unsafe]: http://stackoverflow.com/questions/885793/e-macs-error-when-calling-server-start
[Space-macs logo]: https://github.com/nashamri/space-macs-logo
[Nasser Alshammari]: https://github.com/nashamri
[compilation-warnings]: https://github.com/quelpa/quelpa/issues/90#issuecomment-137982713
[cpaulik-unity-icon]: http://splendidabacus.com/posts/2015/03/space-macs-unity-icon/
[icon-mac-instructions]: http://www.idownloadblog.com/2014/07/16/how-to-change-app-icon-mac/
[latest tags]: https://github.com/syl20bnr/space-macs/tags
[icon-repository]: https://github.com/nashamri/space-macs-logo
[Stack Exchange]: http://e-macs.stackexchange.com/questions/tagged/space-macs
[Reddit]: https://www.reddit.com/r/space-macs
[quote01]: https://gitter.im/syl20bnr/space-macs?at=568e627a0cdaaa62045a7df6
[quote02]: https://gitter.im/syl20bnr/space-macs?at=5768456c6577f032450cfedb
[build_source]: https://www.gnu.org/software/e-macs/manual/html_node/efaq/Installing-e-macs.html
[Bountysource]: https://salt.bountysource.com/teams/space-macs
[Source Code Pro]: https://github.com/adobe-fonts/source-code-pro
[Space-macs Shop]: https://shop.spreadshirt.com/space-macs-shop
[Git]: https://git-scm.com/downloads


