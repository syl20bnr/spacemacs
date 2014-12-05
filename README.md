![title](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/title.png)
[![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/syl20bnr/spacemacs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)[![Twitter][]](http://www.twitter.com/spacemacs)

[philosophy][] | [goals][] | [for who?][] | [screenshots][] | [achievements][] | [contribute][CONTRIBUTE.md] | [documentation][DOCUMENTATION.md]

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

### Convenient and Mnemonic Key Bindings

`Spacemacs` organizes key bindings by mnemonic namespaces. If you are looking
for commands to operate on your buffer, they are right under `<SPC> b`, if you
want to operate on your project, then it is `<SPC> p`, etc...

There is no need to learn convoluted Emacs chords, everything you need is under
bindings that are easy to type and easy to remember.

### Excellent Evil Support

Spacemacs comes with Vim modal editing through [Evil Mode][]. Everything is
designed with it in mind from the key bindings to the user interface.
This includes fancy goodies like a micro-state for editing all occurences of a
symbol and extra packages like [ace-jump][],[evil-lisp-state][] and [evil-nerd-commenter][].

### Batteries Included

Comes with configuration for hundreds of packages that make it fantastic out of the
box. Many languages like Python, Ruby, Scala, R, SCSS, Elixir and Javascript come with modes,
configuration and convenient key bindings. It also comes with [Git support][], [project management][]
and auto-completion. And all of this is optimized and lazy-loaded so you still get fast boot times!

And if that isn't enough you can use [community contributed][contrib layers] configuration
layers for nice configurations of packages that aren't in the default distribution.

### Great [Documentation][DOCUMENTATION.MD]

Most Spacemacs features come with extensive documentation including key bindings, configuration options and
explanations for beginners. And if you can't find the answers you need, ask your question in the [Gitter Chat][] and
a member of the community will help you out.

**[Visit the Documentation][DOCUMENTATION.MD]**

### Nice UI

Spacemacs is designed to look nice in a minimal and functional way. It comes with good theme support and a highly customized
Powerline. The Powerline includes features like quick window switching numbers, Evil mode colors, and nice mode icons.

![spacemacs_python](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/spacemacs-python.png)

# Prerequisites

`Spacemacs` is tested with Emacs 24.3 and 24.4. It should boot on all the major
OSes where these versions can be installed.

Some modes require third-party tools that you'll have to install via your
favorite package manager.

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

# Update

For now the update of `Spacemacs` is manual.

1) It is important to _first_ update the Emacs packages. In Emacs:

```
<SPC> a P <wait> U x y <wait> y
```

For more information about the package list buffer refer to
[Using the package list buffer][using_package_buf] of the documentation.

2) Close Emacs and update the git repository:

```sh
$ git pull --rebase
$ git submodule sync; git submodule update
```

Then restart Emacs.

# Configuration

## Configuration layers

`Spacemacs` is based on [configuration layers][config]. To create your own
configuration layer:

    <SPC> : config-system/create-layer RET

After entering a name, a layer skeleton will be created in the [private][]
directory where you'll find the following files:
- `packages.el` to list the elpa packages
- `exentsions.el` for any other package that is not available in a elpa
repository.

The `private` directory is ignored by Git.

To use your newly created configuration layer, add it to your `~/.spacemacs`
file (see next section).

Note that this approach leaves your layer not source controlled. To get more
info on the different approaches to manage your layers, refer to the
[Managing private configuration layers][manage_config] section of the
documentation.

## Dotfile ~/.spacemacs

`Spacemacs` has a convenient dotfile. You have to install it by invoking the
following command inside Emacs:

    <SPC> : dotspacemacs/install RET

Refers directly to the file `~/.spacemacs` to get documentation or
navigate to the [dotfile configuration][dotfile] section of [DOCUMENTATION.md][].

# Learning Spacemacs

## Universal argument

In spacemacs the universal argument is by default on `<SPC> u` instead of `C-u`
which is used to scroll up as in Vim.

## Key bindings discovery

Thanks to [guide-key][], whenever a prefix command is pressed (like `<SPC>`)
a buffer appears after one second listing the possible keys for this prefix.

It is also possible to search for specific key bindings by pressing:

    <SPC> ?

To narrow the list to `Spacemacs` key bindings starting with prefix `<SPC>`,
set the pattern to something like the regular expression:

    `SPC\ b`

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

- Why manually installed packages with `package-install` are automatically
deleted by `Spacemacs` when it starts ?
To declare new packages you have to create a new configuration layer, see
the quick start guide [here](https://github.com/syl20bnr/spacemacs#configuration).


[Twitter]: http://i.imgur.com/tXSoThF.png
[philosophy]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#philosophy
[goals]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#goals
[for who?]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#who-can-benefit-from-this-
[screenshots]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#screenshots
[config]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#configuration-layers
[dotfile]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#dotfile-configuration
[manage_config]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#managing-private-configuration-layers
[using_package_buf]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#using-the-package-list-buffer
[achievements]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#achievements
[troubleshoot]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#troubleshoot
[contrib layers]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#using-configuration-layers
[Git support]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#working-with-git
[ace-jump]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#vim-motions-with-ace-jump-mode
[project management]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#project-management
[Evil Mode]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md#evil
[private]: https://github.com/syl20bnr/spacemacs/tree/master/private
[DOCUMENTATION.md]: https://github.com/syl20bnr/spacemacs/blob/master/DOCUMENTATION.md
[CONTRIBUTE.md]: https://github.com/syl20bnr/spacemacs/blob/master/CONTRIBUTE.md
[emacs_live]: https://github.com/overtone/emacs-live
[guide-key]: https://github.com/kai2nenobu/guide-key
[guide-key-tip]: https://github.com/aki2o/guide-key-tip
[evil-lisp-state]: https://github.com/syl20bnr/evil-lisp-state
[evil-nerd-commenter]: https://github.com/redguardtoo/evil-nerd-commenter
[Gitter Chat]: https://gitter.im/syl20bnr/spacemacs
