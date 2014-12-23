![title](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/img/title.png)
[![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/syl20bnr/spacemacs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)[![Twitter][]](http://www.twitter.com/spacemacs)

[philosophy][] | [goals][] | [for who?][] | [screenshots][] | [documentation][DOCUMENTATION.md] | [contribute][CONTRIBUTE.md] | [achievements][] | [FAQ][]

**Quick Install:**

    git clone --recursive http://github.com/syl20bnr/spacemacs ~/.emacs.d

# Introduction

`Spacemacs` is a user-friendly and well-documented Emacs kit that integrates the
best Emacs packages out there. It uses [Evil Mode][] to combine the ergonomic
editing features of Vim with the extensibility of Emacs.

Spacemacs is designed to be approachable to users coming from Vim--you do not
need any prior experience with Emacs to get started.

If you are already an experienced Emacs user, you will appreciate the clean
customization system and tight integration of the included packages.

Spacemacs is currently in beta, and contributions are welcome.

And now, to use a well known catch line from [Emacs Live][emacs_live]:

    M-x start-spacing !

# Features

### Batteries Included

Spacemacs integrates hundreds of packages and is ready to use with no additional
configuration. It provides excellent support for many languages, including the
following:

- [Elixir][]
- [Haskell][]
- [JavaScript][]
- LaTex
- [Python][]
- [R][]
- [Ruby][]
- SCSS
- [Scala][]
- [Clojure][]
- [C-C++][]

It also comes with [Git support][] and [project management][] tools. All these
features are loaded on-demand to keep startup time short.

The Spacemacs community provides [additional configurations][contrib layers]
that extend the default distribution.

**[Visit the Documentation][DOCUMENTATION.MD]**

### Nice UI

Spacemacs looks good. It comes with high-quality themes and a custom low-clutter
modeline.

![spacemacs_python](doc/img/spacemacs-python.png)

### Excellent Evil Support

Spacemacs is designed around Vim keyboard bindings, provided by [Evil Mode][].
The packages distributed with Spacemacs are customized to integrate seamlessly
with Evil.

Spacemacs improves upon Vim by using task-specific states to group related
commands. These states reduce the keystrokes needed to issue repetitive commands
and reduce the number of keyboard bindings to learn.

### Convenient and Mnemonic Key Bindings

`Spacemacs` organizes key bindings into mnemonic groups. For example, commands
to operate on the buffer are prefixed by `<SPC> b`, and commands to operate on
the project are under `<SPC> p`.

There is no need to learn convoluted Emacs key chords--Spacemacs uses memorable
bindings that are easy to type.

### Great [Documentation][DOCUMENTATION.MD]

Most of Spacemacs' features are extensively documented, along with key bindings
and configuration options.

If you need help, ask your question in the [Gitter Chat][] and a member of the
community will help you out.

# Prerequisites

## Emacs version

`Spacemacs` is tested with Emacs 24.3 and 24.4. It should boot on all the major
OSes where these versions can be installed.

Some modes require third-party tools that you'll have to install via your
favorite package manager.

## OS X

The recommended version for OS X is [emacs-mac-port][]. It can be installed
from [homebrew][] with the following commands:

```sh
$ brew tap railwaycat/emacsmacport
$ brew install emacs-mac
```

The default key handling is different from the official OS X port. To correct
this you can put this in your [dotfile][]:

```elisp
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(setq mac-pass-control-to-system nil)

(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'copy-region-as-kill)
```

# Install

1. If you have an existing Emacs configuration, back it up:

   ```sh
   cd ~
   mv .emacs.d .emacs.bak
   ```

2. Clone this repository _with its submodules_:

   ```sh
   git clone --recursive http://github.com/syl20bnr/spacemacs ~/.emacs.d
   ```

   `master` is the stable branch and is regularly updated. Switch to the `develop`
   branch if you want to use the bleeding-edge version.

3. Launch Emacs. Spacemacs will automatically install the packages it requires.

4. Restart Emacs to complete the installation.

See the [troubleshooting][troubleshoot] guide if you have any issues.

# Update

Spacemacs currently requires manual updates using the following procedure:

1. Update Emacs packages.
  1. Open the package list using `<SPC> a P` or `M-x paradox-list-packages`
  2. Mark all packages for updating by pressing `U`, and
  3. install them with `x`.

  [See the documentation][using_package_buf] for more information about how to use
  the package list.

2. Close Emacs and update the git repository:

   ```sh
   git pull --rebase
   git submodule sync; git submodule update
   ```

3. Restart Emacs to complete the upgrade.

# Configuration

`Spacemacs` divides its configuration into self-contained units called
[configuration layers][config]. It uses a dotfile, `~/.spacemacs`, to control
which of these features to enable.

## Configuration layers

A configuration layer is a directory containing at least the following files:

- `packages.el`: Defines and configures packages to be downloaded from Emacs package repositories
- `extensions.el`: Configures packages that do not need to be downloaded with
  the package manager, such as built-in Emacs features and git submodules.

You should create your own configuration layers in the [private][] directory.
The following command automates this process:

    <SPC> : config-system/create-layer RET

_Caveat:_ For your privacy, the contents of the `private` directory are not
under source control. See the documentation for a discussion on how to
[manage your private configuration][manage_config].

Any configuration layers you create must be explicitly loaded in your
`~/.spacemacs` file.

## Dotfile (.spacemacs)

The `.spacemacs` file controls which features to load and provides a way to
customize Spacemacs' loading sequence.

The following command will create `.spacemacs` in your home directory:

    <SPC> : dotspacemacs/install RET

To open the installed dotfile:

    <SPC> f e d

To load configuration layers, add them to the list beside
`dotspacemacs-configuration-layers`:

```lisp
;; List of configuration layers to load.
dotspacemacs-configuration-layers '(company-mode smex)
```

The comments in this file contain further information about how to customize
Spacemacs. See the [dotfile configuration][dotfile] section of the documentation
for more information.

# Learning Spacemacs

## The leader key

Spacemacs key bindings use a leader key which is set by default to
<kbd>SPC</kbd> key (space bar).

You can change it easily by setting the variable `dotspacemacs-leader-key` in
your `~/.spacemacs` file.

## Universal argument

In spacemacs the universal argument is by default on `<SPC> u` instead of `C-u`
which is used to scroll up as in Vim.

## Configuration layers and Packages discovery

By using `helm-spacemacs` with <kbd>SPC f e h</kbd> you can quickly search
for a package and get the name of the layers using it.

You can also easily go to the `README.md` of a layer or go to the initialization
function of a package.

## Key bindings discovery

Thanks to [guide-key][], whenever a prefix command is pressed (like `<SPC>`)
a buffer appears after one second listing the possible keys for this prefix.

It is also possible to search for specific key bindings by pressing:

    <SPC> ?

To narrow the list to `Spacemacs` key bindings starting with prefix `<SPC>`,
set the pattern to something like the regular expression:

    SPC\ b

The example above will list all the `buffer` related bindings.

## Describe functions

`Describe functions` are powerful Emacs introspection commands to get information
about functions, variables, modes and so on.

These functions are accessible with the following bindings:

Key Binding   |                 Description
--------------|------------------------------------------------------------------
`<SPC> h d f` | describe-function
`<SPC> h d k` | describe-key
`<SPC> h d m` | describe-mode
`<SPC> h d v` | describe-variable

# Contributions

`Spacemacs` needs you!

Especially to grow the number of configuration layers, for instance to support new languages.

If you are ready to contribute please consult the [contribution guidelines][CONTRIBUTE.md]
first.

# FAQ

1. **Why installed packages with package-install are automatically deleted by
Spacemacs when it starts ?**
To declare new packages you have to create a new configuration layer, see
the quick start guide [here](https://github.com/syl20bnr/spacemacs#configuration).

2. **Why the fonts on Windows looks so crappy ?**
You can installed [MacType][] on Windows to get very nice looking fonts. It is
also recommended to disable the smooth scrolling on Windows.

3. **The Spacemacs banner is ugly, what should I do ?**
Install the default font supported by Spacemacs or choose a fixed witdh font.
More information in the [font section][] of the documentation.

4. **Why the powerline has no arrows in terminal even with a patched font ?**
Emacs powerline implementation does not use patched fonts. There exist currently
no mode-lines in Emacs that support patched font.
The corresponding feature request for the powerline can be found
[here][pw-patched-fonts].

5. **Why the powerline colors are not correct on OS X ?**
This is a [known issue][powerline-srgb-issue] as of Emacs 24.4 due to
`ns-use-srgb-colorspace` defaulting to true. It is recommended to use
the [emacs-mac-port][] build. See the [install OSX section][] for more
info on this.

[Twitter]: http://i.imgur.com/tXSoThF.png
[philosophy]: https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md#philosophy
[goals]: https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md#goals
[for who?]: https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md#who-can-benefit-from-this-
[screenshots]: https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md#screenshots
[config]: https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md#configuration-layers
[dotfile]: https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md#dotfile-configuration
[manage_config]: https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md#managing-private-configuration-layers
[using_package_buf]: https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md#using-the-package-list-buffer
[achievements]: https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md#achievements
[troubleshoot]: https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md#troubleshoot
[contrib layers]: https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md#using-configuration-layers
[Git support]: https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md#working-with-git
[ace-jump]: https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md#vim-motions-with-ace-jump-mode
[project management]: https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md#project-management
[Evil Mode]: https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md#evil
[private]: https://github.com/syl20bnr/spacemacs/tree/master/private
[DOCUMENTATION.md]: https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md
[font section]: https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md#font
[CONTRIBUTE.md]: https://github.com/syl20bnr/spacemacs/blob/master/doc/CONTRIBUTE.md
[FAQ]: https://github.com/syl20bnr/spacemacs#faq
[dotfile]: https://github.com/syl20bnr/spacemacs#dotfile-spacemacs
[install OSX section]: https://github.com/syl20bnr/spacemacs#os-x
[emacs_live]: https://github.com/overtone/emacs-live
[guide-key]: https://github.com/kai2nenobu/guide-key
[guide-key-tip]: https://github.com/aki2o/guide-key-tip
[evil-lisp-state]: https://github.com/syl20bnr/evil-lisp-state
[evil-nerd-commenter]: https://github.com/redguardtoo/evil-nerd-commenter
[Gitter Chat]: https://gitter.im/syl20bnr/spacemacs
[pw-patched-fonts]: https://github.com/milkypostman/powerline/issues/15
[MacType]: https://code.google.com/p/mactype/
[emacs-mac-port]: https://github.com/railwaycat/emacs-mac-port
[homebrew]: https://github.com/Homebrew/homebrew
[Elixir]: https://github.com/syl20bnr/spacemacs/blob/master/contrib/lang/erlang-elixir
[Haskell]: https://github.com/syl20bnr/spacemacs/blob/master/contrib/lang/haskell
[JavaScript]: https://github.com/syl20bnr/spacemacs/blob/master/contrib/lang/javascript
[Python]: https://github.com/syl20bnr/spacemacs/blob/master/contrib/lang/python
[R]: https://github.com/syl20bnr/spacemacs/blob/master/contrib/lang/ess
[Ruby]: https://github.com/syl20bnr/spacemacs/blob/master/contrib/lang/ruby
[Scala]: https://github.com/syl20bnr/spacemacs/blob/master/contrib/lang/scala
[Clojure]: https://github.com/syl20bnr/spacemacs/blob/master/contrib/lang/clojure
[C-C++]: https://github.com/syl20bnr/spacemacs/blob/master/contrib/lang/c-c++
[powerline-srgb-issue]: https://github.com/milkypostman/powerline/issues/54
