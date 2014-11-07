# Spacemacs Advanced Kit

![title](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/title.png)
[![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/syl20bnr/spacemacs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

**Quick Install:**

    git clone --recursive http://github.com/syl20bnr/spacemacs .emacs.d

_Jump to [Install](#install) for more info and [here][CONTRIBUTE.md-PR]
for contribution guidelines_

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Spacemacs Advanced Kit](#spacemacs-advanced-kit)
    - [Introduction](#introduction)
    - [Philosophy](#philosophy)
        - [Easy](#easy)
        - [Consistency](#consistency)
        - [Crowd-Configured](#crowd-configured)
    - [Goals](#goals)
    - [Screenshots](#screenshots)
    - [Who can benefit from this ?](#who-can-benefit-from-this-)
    - [Prerequisites](#prerequisites)
    - [Install](#install)
        - [Troubleshoot](#troubleshoot)
            - [Loading fails](#loading-fails)
            - [I have no file ~/.spacemacs](#i-have-no-file-spacemacs)
    - [Help commands](#help-commands)
        - [Key bindings](#key-bindings)
        - [Other describe functions](#other-describe-functions)
    - [Configuration layers](#configuration-layers)
        - [Structure](#structure)
        - [Extensions and Packages declaration and initialization](#extensions-and-packages-declaration-and-initialization)
        - [Packages synchronization (Vundle like feature)](#packages-synchronization-vundle-like-feature)
    - [Contribution layers](#contribution-layers)
        - [Adding a contribution layer](#adding-a-contribution-layer)
        - [Submitting a contribution layer upstream](#submitting-a-contribution-layer-upstream)
        - [Themes Megapack example](#themes-megapack-example)
    - [Pull Request Guidelines](#pull-request-guidelines)
    - [Dotfile Configuration](#dotfile-configuration)
        - [Installation](#installation)
        - [Content](#content)
            - [Using contributions layers](#using-contributions-layers)
            - [Excluding packages](#excluding-packages)
            - [Hooks](#hooks)
            - [Custom variables](#custom-variables)
    - [Main principles](#main-principles)
        - [Evil](#evil)
            - [States](#states)
            - [Base States](#base-states)
        - [Evil leader](#evil-leader)
        - [Universal argument](#universal-argument)
        - [Micro-states](#micro-states)
    - [Color theme](#color-theme)
    - [UI elements](#ui-elements)
        - [Toggles](#toggles)
        - [Mode-line](#mode-line)
            - [Flycheck integration](#flycheck-integration)
            - [Anzu integration](#anzu-integration)
            - [Powerline separators](#powerline-separators)
            - [Minor Modes](#minor-modes)
    - [Base packages](#base-packages)
        - [Evil plugins](#evil-plugins)
        - [Helm extensions](#helm-extensions)
    - [Commands](#commands)
        - [Return to normal mode](#return-to-normal-mode)
        - [Executing Vim, Emacs and shell commands](#executing-vim-emacs-and-shell-commands)
        - [Navigating](#navigating)
            - [Point/Cursor](#pointcursor)
            - [Vim motions with ace-jump mode](#vim-motions-with-ace-jump-mode)
            - [Window manipulation](#window-manipulation)
                - [Golden ratio](#golden-ratio)
            - [Buffers and Files](#buffers-and-files)
            - [Ido](#ido)
            - [NeoTree file tree](#neotree-file-tree)
            - [Bookmarks](#bookmarks)
        - [Searching](#searching)
            - [Persistent highlighting](#persistent-highlighting)
            - [Auto-highlight symbols](#auto-highlight-symbols)
            - [Visual Star](#visual-star)
            - [Listing symbols by semantic](#listing-symbols-by-semantic)
            - [Helm-swoop](#helm-swoop)
        - [Editing](#editing)
            - [Text manipulation commands](#text-manipulation-commands)
            - [Change font size](#change-font-size)
            - [Increase/Decrease numbers](#increasedecrease-numbers)
            - [Spell checking](#spell-checking)
            - [Region selection](#region-selection)
            - [Region narrowing](#region-narrowing)
            - [Line formatting](#line-formatting)
            - [Auto-completion](#auto-completion)
            - [Commenting](#commenting)
        - [Errors handling](#errors-handling)
        - [Project management](#project-management)
        - [Working with Git](#working-with-git)
            - [Magit](#magit)
            - [Quick guide for recurring use cases in Magit](#quick-guide-for-recurring-use-cases-in-magit)
            - [Git gutter bitmaps](#git-gutter-bitmaps)
        - [Editing Lisp code](#editing-lisp-code)
            - [Philosophy](#philosophy)
            - [Intuitive navigation model](#intuitive-navigation-model)
            - [Key bindings maps](#key-bindings-maps)
                - [Regular normal state bindings](#regular-normal-state-bindings)
                - [Lisp specific bindings](#lisp-specific-bindings)
        - [Modes](#modes)
            - [Helm](#helm)
            - [Erlang](#erlang)
            - [Ledger](#ledger)
            - [Org](#org)
            - [Perforce](#perforce)
            - [Python](#python)
                - [Inferior REPL process](#inferior-repl-process)
                - [Testing in Python](#testing-in-python)
                - [Other Python commands](#other-python-commands)
            - [JavaScript](#javascript)
            - [R (ESS)](#r-ess)
                - [Inferior REPL process](#inferior-repl-process)
                - [Other R commands](#other-r-commands)
            - [rcirc](#rcirc)
    - [Tips](#tips)
        - [Tips for Emacs users](#tips-for-emacs-users)
        - [Tips for Spacemacs advanced users](#tips-for-spacemacs-advanced-users)
    - [TODO list](#todo-list)
    - [Thank you](#thank-you)

<!-- markdown-toc end -->

## Introduction

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

## Philosophy

Three core pillars: Easy, Consistency, "Crowd-Configured".

### Easy

`Spacemacs` organizes key bindings by mnemonic namespaces. If you are looking
for commands to operate on your buffer, they are right under `<SPC> b`, if you
want to operate on your project, then it is `<SPC> p`, etc...

`Spacemacs` comes with a dedicated major mode `spacemacs-mode`. Its goal is to
give useful feedbacks and perform maintenance tasks easily.

### Consistency

Similar functionalities should have the same key binding. For instance if you are
looking for the definition of a function, the binding is `<SPC> m g`, `m` for
`major mode` and `g` for `go to`. And no matter what is the major mode it
should be the same binding.

### Crowd-Configured

This term does not really exist but I'm sure you know what it means.

This is the most powerful feature of `Spacemacs`. Anybody can submit upstream
his or her configuration layer and anybody can use it in a second by adding it
in a dotfile and by optionally filtering it (ie. removing unwanted packages).

So by cloning this repository you have a centralized place of configured
packages tuned by expert in their domain. And most importantly it should be
consistent with the whole experience provided by `Spacemacs`.

If some packages are missing from core `Spacemacs` but they are present in
several contribution layers, chances are that they should be in core and we
can easily move them there.

If any of this core pillars are violated open an issue and we'll try to fix
this.

## Goals

- **Bring the power of modal editing** to the powerful Emacs editing platform.

- Integrate nicely with `Evil` states (`Vim` modes): `Spacemacs` tries to
**keep your fingers on the home row** as much as possible, no matter the mode
you are in.

- **Crowed-configured**: Contribute your own personal layer upstream and
everybody can use it.

- **Minimalistic and nice UI**, keep your available screen space for what
matters: your text files.

- **Mnemonic and consistent key bindings** which should be easier to learn
and remember.

- **Fast boot time**.

- **Lower the risk of RSI**.

- Hopefully, if it's not already the case:

Ɛ>Ɛ>Ɛ> **make you love modal editing!** <3<3<3

## Screenshots

_Startup_
![spacemacs_startup](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/spacemacs-startup.png)

_Python_
![spacemacs_python](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/spacemacs-python.png)

_Terminal (urxvt)_
![spacemacs_urxvt](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/spacemacs-urxvt.png)

*Note: Even though screenshots are updated frequently, `Spacemacs` is evolving
quickly and the screenshots may not reflect exactly the current state of the
project.*

## Who can benefit from this ?

`Spacemacs` is first intended to be used by **Vim users** who want to go to the
next level by using Emacs.

It is also a good fit for people wanting to **lower the [risk of RSI][RSI]**
induced by the default Emacs key bindings. 

Emacs users wanting to learn **a different way to edit files** or wanting to
learn Vim key bindings (see [Tips for Emacs users](#tips-for-emacs-users)).

As a side note, if you are a programmer and you don't know Vim key bindings
yet, I deeply recommend you to learn the basics as recommended in
[Sacha Chua's one-page guide][sacha_guide] about how to learn Emacs.

## Prerequisites

`Spacemacs` is tested with Emacs 24.3 and 24.4. It should boot on all the major
OSes where these versions can be installed.

Some packages require external tools to work, a list of all dependencies will
be provided in this read me. _Stay tuned._

## Install

1) Backup your current `.emacs.d` and clone the repo _with the submodules_:

    cd ~
    mv .emacs.d .emacs.bak
    git clone --recursive http://github.com/syl20bnr/spacemacs .emacs.d

`master` is a stable branch, if you want the "bleeding edge" checkout the
`develop` branch.

2) Launch Emacs, the first time a bunch of packages will be downloaded and
installed. When the package installation is complete restart Emacs and
`Spacemacs` should be ready to use.

### Troubleshoot

#### Loading fails

If during the first boot of Emacs nothing seems to happen or if the
installation seems to abort prematurely, you can check for an error message
by opening the `*Warning*` buffer:

    C-x b warning RET

_('C-x b' means 'Ctrl + x then b' and 'RET' means 'return')_

Then you can copy/paste the error in a [Github issue][issues], thank you.

#### I have no file ~/.spacemacs

You have to manually copy the `~/.emacs.d/.spacemacs.template` file to
`~/.spacemacs`

## Help commands

### Key bindings

1) By default, [guide-key][] and [guide-key-tip][] are enabled.

Whenever you press a prefix command (like `<SPC>`) and wait for one second,
a buffer appear listing the possible keys following this prefix.

2) You can also easily get a full list of all the key bindings by pressing:

    <SPC> ?

To narrow the list to `Spacemacs` specific key bindings set the pattern to
something like the regular expression:

    `^SPC\ b`

The example above will list all the `buffer` related bindings.

### Other describe functions

Emacs `describe-xxx` function are accessible with the following bindings:

Key Binding   |                 Description
--------------|------------------------------------------------------------------
`<SPC> h d f` | describe-function
`<SPC> h d k` | describe-key
`<SPC> h d m` | describe-mode
`<SPC> h d v` | describe-variable

## Configuration layers

_This part of Spacemacs is still in beta, the structure can change over
time. Refer to commit messages for more information in case of big changes._

### Structure

Configuration is organized in layers. Each layer has the following structure:

```
[layer_name]
  |__ [extensions]
  | |__ [mode 1]
  | |     ...
  | |__ [mode n]
  |__ config.el
  |__ extensions.el
  |__ funcs.el
  |__ keybindings.el
  |__ packages.el

[] = directory
```

Where:

      File        |                          Usage
------------------|-----------------------------------------------------------
config.el         | Emacs built-in configuration or mandatory configuration
extensions.el     | The list of extensions to load and the functions to initialize them
funcs.el          | Various functions and macros (often used in keybindings.el)
keybindings.el    | Emacs built-in key bindings or mandatory key bindings
packages.el       | The list of packages to install and the functions to initialize them

`Packages` are `ELPA` packages which can be installed from an `ELPA` compliant
repository, and `Extensions` are generally elisp code from git submodules.

### Extensions and Packages declaration and initialization

`Extensions` and `Packages` are declared in variables `<layer>-pre-extensions`,
`<layer>-post-extensions` and `<layer>-packages` where `<layer>` is the layer
name. `Pre-Extensions` are loaded before `Packages` and `Post-Extensions` are
loaded after `Packages`.

They are processed in alphabetical order so sometimes you'll have to use
some `eval-after-load` black magic.

To initialize an extension or a package `xxx`, define a function with this
format in `extensions.el` or `packages.el`:

```elisp
(defun <layer>/init-xxx ()
   ...body
)
```

### Packages synchronization (Vundle like feature)

`Spacemacs` features a synchronization engine for the ELPA packages. It means
that `Spacemacs` will auto-install the new packages in `<layer>-packages` lists
_and_ auto-delete orphan packages in your `elpa` directory.

It effectively makes `Spacemacs` to behave like [Vundle][vundle].

## Contribution layers

`Spacemacs` leverages the configuration layers in order to make it possible for
you to share your own layer with other `Spacemacs` users. This kind of layer is
called `contribution layer`.

### Adding a contribution layer

Just create a configuration layer in `~/.emacs.d/contrib` or in a path that is
registered in `dotspacemacs-configuration-layer-path` variable of your
`~/.spacemacs` dotile (see the [dotfile section]() for more info on this file).

The base files of a configuration layer (see [structure](#structure)) are
optional so you just have to create only the files you need in your layer.
For instance if you just want to add packages then only the `packages.el` file
is necessary (as it is the case for the [Themes Megapack][themes-megapack]
layer).

### Submitting a contribution layer upstream

See the [CONTRIBUTE.md-CL][] file.

### Themes Megapack example

This is a simple contribution layer listing a bunch of themes, you can find it
[here][themes-megapack].

To install it, just add `themes-megapack` to your `~/.spacemacs`. You have now
installed around 100 themes you are free to try with `<SPC> h t` (helm-themes).

## Pull Request Guidelines

See the [CONTRIBUTE.md-PR][] file.

## Dotfile Configuration

User configuration can be stored in your `~/.spacemacs` file.

### Installation

`~/.spacemacs` is an optional file. If you want to use it you have to copy it
manually from the template file `~/.emacs.d/.spacemacs.template`

```sh
$ cp ~/.emacs.d/.spacemacs.template ~/.spacemacs
```

### Content

#### Using contributions layers

To use a contribution layer, add it to the `dotspacemacs-configuration-layers`
variable of your `~/.spacemacs`.

For instance to add the configuration layer of [RMS](#thank-you):
```elisp
(setq-default dotspacemacs-configuration-layers '(rms))
```
If this layer does not exist you can still try another one in
[the `contrib` directory](https://github.com/syl20bnr/spacemacs/tree/master/contrib).

By default contribution layers are expected to be stored in `~/.emacs.d/contrib`
and we encourage you to submit your layers upstream in order to share them,
grow the package coverage of `Spacemacs` and dispatch responsibilities for their
maintenance. But of course you are free to keep them somewhere else, if this is
your case you can declare additional paths where `Spacemacs` can look for
contribution layers. This is done by setting the list
`dotspacemacs-configuration-layer-path` in your `~/.spacemacs`:

```elisp
(setq-default dotspacemacs-configuration-layer-path '("~/.mycontribs/"))
```

#### Excluding packages

You can exclude packages you don't want to install with the variable
`dotspacemacs-excluded-packages`, this variable can exclude both packages and
extensions (see [Configuration layers](#configuration-layers) for more info
on packages and extensions).

For instance to disable the `rainbow-delimiters` package:
```elisp
(setq-default dotspacemacs-excluded-packages '(rainbow-delimiters))
```

Note that for now, excluded packages that have been installed are not
uninstalled. You'll have to delete them manually from your `~/.emacs.d/elpa`
directory.

#### Hooks

Two special functions of the `~/.spacemacs` file can be used to perform
configuration at the beginning and end of `Spacemacs` loading process.

- `dotspacemacs/init` is triggered at the very beginning of `Spacemacs`
loading.
- `dotspacemacs/config` is triggered at the very end of `Spacemacs` loading.

#### Custom variables

Custom variables configuration from `M-x customize-group` which are
automatically saved by Emacs are stored at the end of your `~/.spacemacs`
file.

## Main principles

### Evil

`Spacemacs` uses the [evil][evil] mode to emulate Vim key bindings. It is a
very complete emulation, maybe the most advanced. In fact, Evil is much more
than just a Vim emulation. It has more states than Vim for instance.

#### States

`Spacemacs` has 6 states:

- **Normal** (orange) - like the `normal mode of Vim`, used to execute and
                        combine commands
- **Insert** (green) - like the `insert mode of Vim`, used to actually insert
                       text
- **Visual** (gray) - like the `visual mode of Vim`, used to make text
                      selection
- **Motion** (purple) - exclusive to `Evil`, used to navigate read only
                        buffers
- **Emacs** (blue) - exclusive to `Evil`, using this state is like using a
                     regular Emacs without Vim
- **Lisp** (pink) - exclusive to `Spacemacs`, used to navigate Lisp code and
                    modify it

#### Base States

_(I apologize in advance for the number of repetition of the word `state` in
this section, but I encourage you to read again this section until you correctly
grasp the concept of `base state` since it is an important concept in
`Spacemacs`)_

`Spacemacs` has a notion of `base state`. A `base state` is the state you are
when leaving the `insert state`.

The typical `base state` in Vim is the `normal state` and it is the only one.
`Spacemacs` has more than one base state, here is the list:
- normal
- lisp

This allows a coder of Lisp to completely replace the `normal state` by the
`lisp state`. Indeed, once you fire up the `lisp state` you can just go back
and forth between the `insert state` and the `lisp state`. 

Of course there is a rule to break this in order to be able to go back to the
`normal state`. It is pretty simple:

*When in a `base state`, `ESC` or `fd` will always set you back to the
`normal state`.*

So to go back to the `normal state` while in `lisp state` just hit `ESC` or
`fd`.

### Evil leader

`Spacemacs` heavily uses the [evil-leader][evil-leader] mode which brings the
Vim leader key to the Emacs world.

This leader key is commonly set to `,` by Vim users, in `Spacemacs` the leader
key is set on `SPC` (space bar, this is why the name `spacemacs`). This key is
the most accessible key on a keyboard and it is pressed with the thumb which is
a good choice to lower the risk of [RSI][RSI].

So with `Spacemacs` there is no need to remap your keyboard modifiers to
attempt to reduce the risk of RSI, every command can be executed very easily
while you are in `normal` mode by pressing the `SPC` leader key, here are a
few examples:

- Save a buffer: `<SPC> f s`
- Save all opened buffers: `<SPC> f S`
- Open (switch) to a buffer with `helm`: `<SPC> b s`

### Universal argument

The universal argument `C-u` is an important command in Emacs but it is also
a very handy Vim key binding to scroll up.

`Spacemacs` binds `C-u` to `scroll-up` and change the universal argument
binding to `<SPC> u`.

### Micro-states

`Spacemacs` defines a wide variety of `micro-states` (temporary overlay maps)
where it makes sense. This prevent from repetitive and tedious presses on the
`SPC` key.

When a `micro-state` is active, a documentation is displayed in the minibuffer.
Additional information may as well be displayed in the minibuffer.

[Auto-highlight-symbol micro-state](#auto-highlight-and-edition-of-symbols):
![spacemacs_ahs_micro_state](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/spacemacs-ahs-micro-state.png)

[Text scale micro-state](#change-font-size):
![spacemacs_scale_micro_state](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/spacemacs-scale-micro-state.png)

## Color theme

By default, `Spacemacs` uses the theme [Solarized][solarized-theme].

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> c t`       | cycle between `Spacemacs` themes
`<SPC> h t`       | select a theme using a `helm` buffer

`Spacemacs` available themes:
- [Solarized][solarized-theme]
- [Monokai][monokai-theme]
- [Zenburn][zenburn-theme]

## UI elements

`Spacemacs` has a minimalistic and distraction free UI with a lot of subtle
customization which make it unique compared to other kits:
 - beautiful custom [powerline][powerline] mode-line
 [with color feedback](#flycheck-integration) according to current
 [Flycheck][flycheck]
 status
 - unicode symbols for minor mode lighters which appear in the mode-line
 - [custom fringe bitmaps](#errors-handling) and error feedbacks for
 [Flycheck][flycheck]
 - [custom fringe bitmaps](#git-gutter-bitmaps) for [git gutter][git-gutter]
 - dedicated startup page with a mode aimed at easily managing `Spacemacs`

### Toggles

Some UI indicators can be toggled on and off (toggles start with `t`):

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> t 8`       | display a mark on the 80th column
`<SPC> t F`       | toggle display of the fringe
`<SPC> t n`       | show the absolute line numbers

### Mode-line

The mode line is an heavily customized [powerline][powerline] with the
following capabilities:
- show the window number
- color code for current state
- show the number of search occurrences via anzu
- toggle flycheck info
- toggle minor mode lighters

Reminder of the color codes for the states:

   Evil State     |       Color
------------------|------------------
Normal            | Orange
Insert            | Green
Visual            | Grey
Emacs             | Blue
Motion            | Purple
Lisp              | Pink

Some elements can be dynamically toggled:

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> t m m`     | toggle the minor mode lighters
`<SPC> t m f`     | toggle the flycheck info

#### Flycheck integration

When [Flycheck][flycheck] minor mode is enabled, a new element appears showing
the number of errors, warnings and info.

![powerline-wave](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/powerline-wave.png)

#### Anzu integration

[Anzu][anzu] shows the number of occurrence when performing a search. `Spacemacs`
integrates nicely the Anzu status by displaying it temporarily when `n` or `N` are
being pressed. See the `5/6` segment on the screenshot below. 

![powerline-anzu](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/powerline-anzu.png)

#### Powerline separators

It is possible to easily customize the `powerline separator` by setting the
`powerline-default-separator` variable in your `~./spacemacs`. For instance
if you want to set back the separator to the well-known `arrow` separator
add the following snippet to your configuration file:

```elisp
(defun dotspacemacs/config ()
  "This is were you can ultimately override default Spacemacs configuration.
This function is called at the very end of Spacemacs initialization."
  (setq powerline-default-separator 'arrow)
```

To save you the time to try all the possible separators provided by the
powerline, here is an exhaustive set of screenshots:

    Separator     |                 Screenshot
------------------|------------------------------------------------------------
`alternate`       | ![powerline-alternate](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/powerline-alternate.png)
`arrow`           | ![powerline-arrow](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/powerline-arrow.png)
`arrow-fade`      | ![powerline-arrow-fade](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/powerline-arrow-fade.png)
`bar`             | ![powerline-bar](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/powerline-bar.png)
`box`             | ![powerline-box](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/powerline-box.png)
`brace`           | ![powerline-brace](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/powerline-brace.png)
`butt`            | ![powerline-butt](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/powerline-butt.png)
`chamfer`         | ![powerline-chamfer](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/powerline-chamfer.png)
`contour`         | ![powerline-contour](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/powerline-contour.png)
`curve`           | ![powerline-curve](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/powerline-curve.png)
`rounded`         | ![powerline-rounded](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/powerline-rounded.png)
`roundstub`       | ![powerline-roundstub](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/powerline-roundstub.png)
`slant`           | ![powerline-slant](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/powerline-slant.png)
`wave`            | ![powerline-wave](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/powerline-wave.png)
`zigzag`          | ![powerline-zigzag](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/powerline-zigzag.png)
`nil`             | ![powerline-nil](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/powerline-nil.png)

#### Minor Modes

`Spacemacs` uses [diminish][diminish] mode to reduce the size of minor mode
indicators:

The minor mode area can be toggled on and off with:

    <SPC> t m m

   Lighter   |                              Mode
-------------|-----------------------------------------------------------------
⊞            | [golden-ratio][golden-ratio] mode
Ⓐ            | [auto-complete][auto-complete] mode
Ⓒ            | [centered-cursor][centered-cursor] mode
eⓅ           | [e-project][e-project] mode
Ⓕ            | flycheck mode
Ⓕ2           | flymake mode
Ⓖ            | guide-key mode
(Ⓟ)          | paredit mode
Ⓢ            | flyspell mode
(Ⓢ)          | [smartparens][sp] mode
Ⓨ            | [yasnippet][yasnippet] mode

**Note:** in terminal the regular indicators are used instead of the utf-8
ones.

## Base packages

`Spacemacs` main mechanics rely largely on `Evil` and `Helm` base packages.
They are both extended with various packages to build on their foundations.

### Evil plugins

`Spacemacs` ships with the following evil plugins:

                 Mode                   |             Description
----------------------------------------|--------------------------------------
[evil-leader][]                         | vim leader that bring a new layer of keys in normal mode
[evil-little-word][]                    | port of [camelcasemotion.vim][]
[evil-visualstar][]                     | search for current selection with `*`
[evil-exchange][]                       | port of [vim-exchange][]
[evil-surround][]                       | port of [vim-surround][]
[evil-nerd-commenter][]                 | port of [nerdcommenter][]
[evil-search-highlight-persist][]       | emulation of hlsearch behavior
[evil-numbers][]                        | like C-a/C-x in vim
[NeoTree][neotree]                      | mimic [NERD Tree][nerdtree]

### Helm extensions

`Spacemacs` tries to use [helm][helm] as much as possible.
[helm][helm] is coupled to [popwin][popwin] so `helm` window always appears in
a new temporary window at the bottom.

The following `helm` modes are installed with `Spacemacs`:

Key Binding | Mode                                    | Description
------------|-----------------------------------------|------------------------
`<SPC> h s` | [helm-swoop][hswoop]                    | search for occurrences within a file and edit the result
`<SPC> h y` | [helm-c-yasnippet][hyas]                | select snippets
`<SPC> h t` | [helm-themes][hthemes]                  | select a theme
`<SPC> p f` | [helm-projectile][projectile]           | select files within a projectile project
`<SPC> ?`   | [helm-descbinds][hdescbinds]            | show key bindings
`<SPC> s c` | [cofi/helm-flyspell-correct][hflyspell] | choose a corrected word

## Commands

Every sequences must be performed in `normal` mode.

### Return to normal mode

`ESC` is the default key to return to normal mode. This is one of the main
design flaw in Vim key bindings because the `ESC` key is very far from the
home row.

The popular way to avoid this is to replace `ESC` by `jj` pressed rapidly.
Unfortunately it is pretty difficult in Emacs to keep a consistent behavior
with this sequence (same thing with `jk` or `kj`).
`Spacemacs` uses the sequence `fd` instead of `jj` which works in any Evil
state and in any buffer and in the minibuffer.

This sequence can be customized in your `~/.spacemacs`, for instance to
revert back to the popular configuration using `jj` (not recommended) add this
to your file:

```elisp
(defun dotspacemacs/init ()
  "User initialization for Spacemacs. This function is called at the very startup."
  (setq-default spacemacs-normal-state-sequence '(?j . ?j))
  (setq-default spacemacs-normal-state-sequence-delay 0.2)
)
```

### Executing Vim, Emacs and shell commands

Command     |                 Key Binding
:----------:|------------------------------------------------------------------
Vim         | `:`
Emacs       | `<SPC> :`
Shell       | `<SPC> !`

### Navigating

#### Point/Cursor

Navigation is performed using the Vi key bindings `hjkl`.

Key Binding |                 Description
------------|------------------------------------------------------------------
`h`         | move cursor left
`j`         | move cursor down
`k`         | move cursor up
`l`         | move cursor right
`H`         | move quickly up (10 lines at a time)
`L`         | move quickly down (10 lines at a time)
`<SPC> j h` | go to the beginning of line (and set a mark at the previous location in the line)
`<SPC> j l` | go to the end of line (and set a mark at the previous location in the line)
`<SPC> z z` | lock the cursor at the center of the screen 

#### Vim motions with ace-jump mode

`Spacemacs` uses the `evil` integration of [ace-jump mode][ace-jump] which
enables the invocation of `ace-jump-mode` during motions.

It is useful for deleting visually a set of lines, try the following sequence
in a buffer containing some text:

    d <SPC> l

Key Binding   |                 Description
--------------|------------------------------------------------------------------
`<SPC> <SPC>` | initiate ace jump char mode
`<SPC> l`     | initiate ace jump line mode
``<SPC> ```   | go back to the previous location (before the jump)

#### Window manipulation

Every window has a number displayed at the start of the mode-line and can
be quickly accessed using `<SPC> number`.

Key Binding   |                    Description
--------------|----------------------------------------------------------------
`<SPC> 1`     | go to first window
`<SPC> 2`     | go to window number 2
`<SPC> 3`     | go to window number 3
`<SPC> 4`     | go to window number 4
`<SPC> 5`     | go to window number 5
`<SPC> 6`     | go to window number 6
`<SPC> 7`     | go to window number 7
`<SPC> 8`     | go to window number 8
`<SPC> 9`     | go to window number 9
`<SPC> 0`     | go to window number 10

Windows manipulation commands (start with `w`):

Key Binding   |                 Description
--------------|----------------------------------------------------------------
`<SPC> w b`   | split a window horizontally
`<SPC> w c`   | close a window
`<SPC> w d`   | toggle window dedication (dedicated window cannot be used by a mode)
`<SPC> w H`   | move window to the left
`<SPC> w J`   | move window to the bottom
`<SPC> w K`   | move window to the top
`<SPC> w L`   | move window to the right
`<SPC> w m`   | maximize/minimize a window
`<SPC> w M`   | maximize/minimize a window, when maximized the buffer is centered
`<SPC> w p m` | open messages buffer in a popup window
`<SPC> w p p` | close the current sticky popup window
`<SPC> w r`   | rotate windows clockwise
`<SPC> w R`   | rotate windows counter-clockwise
`<SPC> w u`   | undo window layout (used to effectively undo a close window)
`<SPC> w U`   | redo window layout
`<SPC> w v`   | split a window vertically
`<SPC> w w`   | cycle and focus between windows

##### Golden ratio

Split windows can be dynamically resized depending on whether they are selected
or not. Resizing is performed by the [golden-ratio][golden-ratio] mode.
By default `golden-ratio` if off.

The mode can be toggled on and off with:

    <SPC> t g

#### Buffers and Files

`Spacemacs` uses `ido` for opening files since `ido` way to navigate
the file system is better than `helm` in my opinion (especially because `ido` can
remember the last selected directories and buffers, maybe helm can do this ?).
`ido` is also used to kill buffers.

Buffer manipulation commands (start with `b`):

Key Binding   |                 Description
--------------|----------------------------------------------------------------
`<SPC> b d`   | delete the current buffer (beware the associated file is also deleted)
`<SPC> b k`   | kill the current buffer
`<SPC> b K`   | kill all buffers except the current one
`<SPC> b m h` | move a buffer to the left
`<SPC> b m j` | move a buffer to the bottom
`<SPC> b m k` | move a buffer to the top
`<SPC> b m l` | move a buffer to the right
`<SPC> b n`   | switch to next buffer
`<SPC> b p`   | switch to previous buffer
`<SPC> b r`   | rename the current buffer
`<SPC> b s`   | switch to a buffer using `helm`
`<SPC> b w`   | toggle read-only

Files manipulation commands (start with `f`):

Key Binding   |                 Description
--------------|----------------------------------------------------------------
`<SPC> f f`   | open a file using `ido`
`<SPC> f i`   | open your `init.el` file
`<SPC> f s`   | save a file
`<SPC> f S`   | save all files
`<SPC> f t`   | toggle file tree side bar using [NeoTree][neotree]
`<SPC> f y`   | show current file absolute path in the minibuffer

#### Ido

`Spacemacs` displays the `ido` minibuffer vertically thanks to the
[ido-vertical-mode][ido-vertical-mode].

Basic `ido` operations can be done with `Ctrl` key:

Key Binding   |                 Description
--------------|----------------------------------------------------------------
`C-d`         | delete selected file (ask for confirmation)
`C-k`         | select previous file or directory
`C-<return>`  | open a `dired buffer`
`C-h`         | go to parent directory
`C-j`         | select next file or directory
`C-l`         | open the selected file
`C-S-j`       | go to next directory
`C-S-k`       | go to previous directory


#### NeoTree file tree

`Spacemacs` provides a quick and simple way to navigate in an unknown project
file tree with [NeoTree][neotree].

To toggle the `NeoTree` buffer press:

    <SPC> f t

In the `NeoTree` buffer:

Key Binding    |                 Description
---------------|----------------------------------------------------------------
`TAB` or `RET` | expand/open
`a`            | toggle stretch the buffer
`c`            | create a node
`d`            | delete a node
`g`            | refresh
`H`            | toggle hidden files
`K`            | kill corresponding buffer
`q` or `fd`    | hide `NeoTree` buffer
`r`            | rename a node

#### Bookmarks

Bookmarks can be set anywhere in a file. Bookmarks are persistent. They are very
useful to jump to/open a known project. `Spacemacs` used `helm-bookmarks` to
manage them.

Open an `helm` window with the current bookmarks by pressing:

    <SPC> h b

Then in the `helm-bookmarks` buffer:

Key Binding   |                 Description
--------------|----------------------------------------------------------------
`CTRL+d`      | delete the selected bookmark
`CTRL+e`      | edit the selected bookmark
`CTRL+f`      | toggle filename location
`CTRL+o`      | open the selected bookmark in another window

To save a new bookmark, just type the name of the bookmark and press `RET`.

### Searching

#### Persistent highlighting

`Spacemacs` uses `evil-search-highlight-persist` to keep the searched expression
highlighted until the next search. It is also possible to clear the
highlighting by pressing `<SPC> s c`.

#### Auto-highlight symbols

`Spacemacs` supports auto highlighting of the current symbol (provided by the
 [auto-highlight-symbol][auto-highlight] mode) and add a micro-state to it
 which makes it a very handy tool to have in your tool belt.

Key Binding   |                 Description
--------------|----------------------------------------------------------------
`<SPC> s e`   | edit all occurrences of the current symbol
`<SPC> t s`   | toggle the auto highlighting

Navigation between the highlighted symbols can be done with the commands:

Key Binding   | Description
--------------|------------------------------------------------------------
`*`           | initiate navigation micro-state
`<SPC> s n`   | go to next occurrence and initiate navigation micro-state
`<SPC> s N`   | go to previous occurrence and initiate navigation micro-state
`<SPC> s r b` | change range to `whole buffer`
`<SPC> s r d` | change range to `display area`
`<SPC> s r f` | change range to `function`
`<SPC> s R`   | change range to default (`whole buffer`)

In 'Spacemacs' highlight symbol micro-state:

Key Binding   | Description
--------------|------------------------------------------------------------
`e`           | edit occurrences
`n`           | go to next occurrence
`N`           | go to previous occurrence
`d`           | go to next definition occurrence
`D`           | go to previous definition occurrence
`r`           | change range (`function`, `display area`, `whole buffer`)
`R`           | go to home occurrence (reset position to starting occurrence)
Any other key | leave the navigation micro-state

The micro-state text in minibuffer display the following information:

    <M> [6/11]* press (n) or (N) to navigate, (R) for home symbol, (r) to change range

Where `<M> [x/y]*` is:
- M: the current range mode
  - `<B>`: whole buffer range
  - `<D>`: current display range
  - `<F>`: current function range
- `x`: the index of the current highlighted occurrence
- `y`: the total number of occurrences
- `*`: appears if there is at least one occurrence which is not currently
visible.

#### Visual Star

With [evil-visualstar][] you can search for the next occurrence of the current
selection.

It is pretty useful combined with the [expand-region](#region-selection)
bindings.

_Note:_ If the current state is not the `visual state` then pressing `*` uses
[auto-highlight-symbol](#auto-highlight-symbols) and its micro-state.

#### Listing symbols by semantic

Use `helm-semantic-or-imenu` command from `Helm` to quickly navigate between
the symbols in a buffer.

To list all the symbols of a buffer press:

    <SPC> s l

#### Helm-swoop

This is very similar to `moccur`, it displays a `helm` buffer with all the
occurrences of the word under point. You can then change the search query
in real-time and navigate between them easily.

You can even edit the occurrences directly in the `helm` buffer and apply
the modifications to the buffer.

Key Binding   |                    Description
--------------|----------------------------------------------------------------
`<SPC> s s`   | execute `helm-swoop`
`<SPC> s S`   | execute `helm-multi-swoop`
`<SPC> s C-s` | execute `helm-multi-swoop-all`

### Editing

#### Text manipulation commands

Text related commands (start with `x`):

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> x u`       | set the selected text to lower case
`<SPC> x U`       | set the selected text to upper case
`<SPC> x d w`     | delete trailing whitespaces
`<SPC> x g l`     | set languages used by translate commands
`<SPC> x g t`     | translate current word using Google Translate
`<SPC> x g T`     | reverse source and target languages
`<SPC> x m j`     | move down a line of text
`<SPC> x m k`     | move up a line of text
`<SPC> x t c`     | swap (transpose) the current character with the previous one
`<SPC> x t w`     | swap (transpose) the current word with the previous one
`<SPC> x t l`     | swap (transpose) the current line with the previous one
`<SPC> x w c`     | count the number of words in the selection region
`<SPC> x w C`     | count the number of occurrences per word in the select region

#### Change font size

The font size of the current buffer can be adjusted with the commands:

Key Binding   | Description
--------------|------------------------------------------------------------
`<SPC> x +`   | scale up the font and initiate the font scaling micro-state
`<SPC> x -`   | scale down the font and initiate the font scaling micro-state
`<SPC> x =`   | reset the font size (no scaling) and initiate the font scaling micro-state
`+`           | increase the font size
`-`           | decrease the font size
`=`           | reset the font size
Any other key | leave the font scaling micro-state

#### Increase/Decrease numbers

`Spacemacs` uses [evil-numbers][] to easily increase or increase numbers.

Key Binding   | Description
--------------|------------------------------------------------------------
`<SPC> n +`   | increase the number under point by one and initiate micro-state
`<SPC> n -`   | decrease the number under point by one and initiate micro-state

In micro-state:

Key Binding   | Description
--------------|------------------------------------------------------------
`+`           | increase the number under point by one
`-`           | decrease the number under point by one
Any other key | leave the micro-state

**Tips:** you can increase or decrease a value by more that once by using a
prefix argument (ie. `10 <SPC> n +` will add 10 to the number under point).

#### Spell checking

Spell checking commands start with `S`:

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> S c`       | list of corrections in a `helm` buffer
`<SPC> S d`       | change dictionary language
`<SPC> S n`       | go to the next spell check error


#### Region selection

Vi `Visual` modes are all supported by `evil`, `Spacemacs` adds another
`Visual` mode via the [expand-region][] mode.

Key Binding   |                 Description
--------------|----------------------------------------------------------------
`<SPC> v`     | initiate expand-region mode then...
`v`           | expand the region by one semantic unit
`V`           | contract the region by one semantic unit
`r`           | reset the region to initial selection
`ESC`         | leave expand-region mode

#### Region narrowing

The displayed text of a buffer can be narrowed with the commands
(start with `n`):

Key Binding   |                 Description
--------------|----------------------------------------------------------------
`<SPC> n f`   | narrow the buffer to the current function
`<SPC> n p`   | narrow the buffer to the visible page
`<SPC> n r`   | narrow the buffer to the selected text
`<SPC> n w`   | widen, i.e show the whole buffer again

#### Line formatting

`Spacemacs` replaces the default `J` Vi key binding (join current line with next
line) by a slightly more frequent action which is to `go to the line below point
and indent it`.

Join lines can still be performed with `<SPC> j k`

Line formatting commands start with `j`:

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`J`               | go to next line and indent it using auto-indent rules
`<SPC> j j`       | same as `J` but will split the current line at point
`<SPC> J`         | split a quoted string or s-expression in place
`<SPC> j J`       | split a quoted string or s-expression and auto-indent
`<SPC> j k`       | join the current line with the next line

Used together these key bindings are very powerful to quickly reformat the code.

#### Auto-completion

`Spacemacs` uses [auto-complete][] auto-completion engine.

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`C-j`             | select next candidate
`C-k`             | select previous candidate
`TAB`             | expand selection or select next candidate
`S-TAB`           | select previous candidate
`return`          | complete word, if word is already completed insert a carriage return


#### Commenting

Comments are handled by [evil-nerd-commenter][], it's bound to the following keys.

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> n c l`     | comment lines
`<SPC> n c t`     | comment to line
`<SPC> n c y`     | comment and yank
`<SPC> n c p`     | comment paragraphs
`<SPC> n c r`     | comment region
`<SPC> n c i`     | comment invert
`<SPC> n c c`     | comment operator


### Errors handling

`Spacemacs` uses [Flycheck][flycheck] to gives error feedback on the fly.
The checks are only performed at save time by default.

Errors management commands (star with `f` for `flycheck`):

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> f c`       | clear all errors
`<SPC> f l`       | display the `flycheck` list of errors/warnings
`<SPC> f n`       | go to the next `flycheck` error
`<SPC> f p`       | go to the previous flycheck error

Custom fringe bitmaps:

   Symbol                                                                                       | Description
:----------------------------------------------------------------------------------------------:|------------
![dot-error](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/dot-error.png)     | Error
![dot-warning](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/dot-warning.png) | warning
![dot-info](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/dot-info.png)       | Info

### Project management

Projects in `Spacemacs` are managed with [projectile][projectile]. In
`projectile` projects are defined implicitly, for instance the root of a
project is found when a `.git` repository or `.projectile` file is
encountered in the file tree.

The only bound key for `projectile` is `projectile-commander` which is:

    <SPC> p

`projectile commander` commands:

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`a`               | run `ack` on project
`A`               | run `ag` on project
`b`               | switch to project buffer
`d`               | find directory in project
`D`               | open project root in `dired`
`f`               | find file in project
`g`               | run `grep` on project
`h`               | find file in project using `helm`
`j`               | find a tag in project
`k`               | kill all project buffers
`o`               | run `multi-occur` on project
`R`               | regenerate the project's [e|g]tags
`r`               | replace a string in the project
`s`               | switch project
`T`               | find test files in project
`v`               | open project root in `vc-dir` or `magit`

### Working with Git

Git commands (start with `g`):

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> g c c`     | highlight regions by age of commits
`<SPC> g c C`     | clear highlights
`<SPC> g c t`     | highlight regions by last updated time
`<SPC> g s`       | open a `magit` status window
`<SPC> g m`       | display the last commit message of the current line
`<SPC> g t`       | launch the git time machine

- Highlight by age of commit or last update time is provided by
[smeargle][smeargle].
- Git time machine is provided by [git-timemachine][git-timemachine].
- Git last commit message per line is provided by
[git-messenger][git-messenger]

#### Magit

`Spacemacs` uses [magit][magit] to manage Git repositories.

To open a `status buffer`, type in a buffer of a Git repository:

    <SPC> g s

The buffer is opened in `Emacs state` but you can sill navigate up and down
with `k` and `j` respectively (should be like this in all `magit` buffers).

Here are the often used bindings inside a `status buffer`:

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`$`               | open `command output buffer`
`c c`             | open a `commit message buffer`
`b b`             | checkout a branch
`b c`             | create a branch
`b v`             | open the `branch manager buffer`
`f f`             | fetch changes
`F -r F`          | pull and rebase
`j`               | go down
`k`               | go up
`K`               | discard changes
`l l`             | open `log buffer`
`P P`             | push
`q`               | quit
`s`               | on a file or hunk in a diff: stage the file or hunk
`+`               | on a hunk: increase hunk size
`-`               | on a hunk: decrease hunk size
`S`               | stage all
`TAB`             | on a file: expand/collapse diff
`u`               | on a staged file: unstage
`U`               | unstage all staged files
`z z`             | stash changes

In a commit message buffer press `C-c C-c` to commit the changes with the
entered message. `C-c C-k` will discard the commit message.

**Note:** Sometimes you will be asked about reverting the commit buffer,
you can answer `y` with no issue.

#### Quick guide for recurring use cases in Magit

- Amend a commit:
  - `l l` to open `log buffer`
  - `c a` on the commit you want to amend
  - `C-c C-c` to submit the changes
- Squash last commit:
  - `l l` to open `log buffer`
  - `E` on the second to last commit, it opens the `rebase buffer`
  - `j` to put point on last commit
  - `i` to pass in `insert state`
  - `s` to squash it
  - `C-c C-c` to continue to the `commit message buffer`
  - `C-c C-c` again when you have finished to edit the commit message
- Force push a squashed commit:
  - in the `status buffer` you should see the new commit unpushed and the
  old commit unpulled
  - `P -f P` for force a push (**beware** usually it is not recommended to
  rewrite the history of a public repository, but if you are *sure* that you
  are the only one to work on a repository it is ok - i.e. in your fork).
- Add upstream remote (the parent repository you have forked):
  - `b v` to open the `branch manager buffer`
  - `a` to add a remote, type the name (i.e. `upstream`) and the URL
- Pull changes from upstream (the parent repository you have forked) and push:
  - `F -r C-u F` and choose `upstream` or the name you gave to it
  - `P P` to push the commit to `origin`

#### Git gutter bitmaps

`Spacemacs` has custom fringe bitmaps for
[git-gutter-fringe][git-gutter]:

   Symbol                                                                                    | Description
:-------------------------------------------------------------------------------------------:|-----------------
![git-new](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/git-new-line.png) | new line
![git-del](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/git-del-line.png) | at least one line has been deleted
![git-mod](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/git-mod-line.png) | modified line

### Editing Lisp code

Lisp navigation and edition is performed with a custom evil `lisp state`
provided by [evil-lisp-state][evil-lisp-state] package.

#### Philosophy

`evil-lisp-state` goal is to replace as much as possible the `normal state` in
lisp buffers.

To achieve this goal, this mode tries to keep the useful commands from the
`normal state` and add new commands (often with `shift` modifier) for
manipulating the data structure.

_Note: Be sure to try the key bindings '(' and ')'. I use them all the time
and it may be one of the best features of this mode._

#### Intuitive navigation model

`hjkl` behaves like in the default `normal state`.

**Next sexp on the same level (sibling)**
- `L` next sexp
- `H` previous sexp

**Change level (parent/children)**
- `J` go to next sexp one level down
- `K` go to previous one level up

And that's it! All these commands always put the point _at the beginning_ of
the sexp.

#### Key bindings maps

##### Regular normal state bindings

Key Binding   | Function
--------------|------------------------------------------------------------
`a`           | evil-append
`c`           | evil-change
`d`           | evil-delete
`h`           | next char
`i`           | evil-insert-state
`I`           | evil-insert-line
`j`           | next visual line
`k`           | previous visual line
`l`           | next char
`o`           | evil-insert-below
`O`           | evil-insert-above
`p`           | evil-past-after
`P`           | evil-past-before
`r`           | evil-replace
`C-r`         | undo-tree-redo
`u`           | undo-tree-undo
`x`           | evil-delete-char
`X`           | evil-delete-backward-char
`y`           | evil-yank
`ESC`         | evil-normal-state

##### Lisp specific bindings

_In this table we assume that `evil-lisp-state-backward-prefix` is set to
default `<tab>`_

Key Binding   | Function
--------------|------------------------------------------------------------
`(`           | insert sibling before sexp and switch to `insert state`
`)`           | insert sibling after sexp and switch to `insert state`
`$`           | sp-end-of-sexp
`0`           | sp-beginning-of-sexp
`A`           | sp-absorb-sexp
`b`           | sp-forward-barf-sexp
`<tab> b`     | sp-backward-barf-sexp
`C`           | sp-convolute-sexp
`Dd`          | sp-kill-hybrid-sexp
`Dx`          | sp-kill-sexp
`<tab> Dx`    | sp-backward-kill-sexp
`Ds`          | sp-kill-symbol
`<tab> Ds`    | sp-backward-kill-symbol
`Dw`          | sp-kill-word
`<tab> Dw`    | sp-backward-kill-word
`E$`          | evil-lisp-state-eval-sexp-end-of-line
`Ee`          | eval-last-sexp
`Ef`          | eval-defun
`gs`          | go to source of symbol under point
`gt`          | sp-transpose-sexp
`gT`          | sp-transpose-hybrid-sexp
`H`           | previous sexp at the same level
`J`           | next sexp one level down
`K`           | previous sexp one level up
`L`           | next sexp of the same level
`M`           | sp-join-sexp (think about `merge-sexp`)
`R`           | sp-raise-sexp
`s`           | sp-forward-slurp-sexp
`<tab> s`     | sp-backward-slurp-sexp
`S`           | sp-splice-sexp-killing-forward
`<tab> S`     | sp-splice-sexp-killing-backward
`w`           | wrap sexp
`W`           | unwrap sexp
`<tab> W`     | sp-backward-unwrap-sexp
`Y`           | sp-copy-sexp
`<tab> y`     | sp-backward-copy-sexp
`backspace`   | sp-backward-delete-char
`S-backspace` | sp-delete-char
`RET`         | indent next line
`S-RET`       | insert new line char and switch to `insert state`

**Reminder:**
`lisp state` is a [base state](#base-states) which means that leaving
the `insert state` when the previous state was `lisp` will set you back
in `lisp state`.
To go back to `normal state` press `<ESC>` or `fd` while in `lisp state`.

### Modes

`Spacemacs` tries to add more natural Vi key bindings to some modes or
simply add new leader key bindings.

Leader key bindings start with `m` because they are bindings related to
the current `major mode`.

#### Helm

`Spacemacs` add `hjkl` navigation to `helm` buffers:

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`CTRL+h`          | go to previous page
`CTRL+j`          | go to previous item
`CTRL+k`          | go to next item
`CTRL+l`          | go to next page

#### Erlang

`Spacemacs` uses [EDTS][edts] as an Erlang coding environment.

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> m d`       | show man page documentation
`<SPC> m e`       | go to next issue
`<SPC> m g`       | go to definition
`<SPC> m G`       | find a module in the current project
`<SPC> m h`       | open the header file under point
`<SPC> m l`       | find a function in the current module
`<SPC> m m`       | go to the macro definition under point
`<SPC> m r`       | go to the record definition under point

#### Ledger

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> m a`       | add a transaction
`<SPC> m d`       | delete current transaction

#### Org

In `org`, [evil-org-mode][evil-org-mode] is activated.

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`gh`              | outline-up-heading
`gj`              | org-forward-heading-same-level
`gk`              | org-backward-heading-same-level
`gl`              | outline-next-visible-heading
`t`               | org-todo
`T`               | org-insert-todo-heading nil
`H`               | org-beginning-of-line
`L`               | org-end-of-line
`;t`              | org-show-todo-tree
`o`               | always-insert-item
`O`               | org-insert-heading
`$`               | org-end-of-line
`^`               | org-beginning-of-line
`<`               | org-metaleft
`>`               | org-metaright
`;a`              | org-agenda`

#### Perforce

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> p 4 a`     | add a file in depot
`<SPC> p 4 d`     | delete a file in depot
`<SPC> p 4 D`     | p4-describe
`<SPC> p 4 e`     | checkout a file
`<SPC> p 4 r`     | rename a file
`<SPC> p 4 R`     | revert a file
`<SPC> p 4 S`     | submit CL

#### Python

##### Inferior REPL process

Start an iPython inferior REPL process with `<SPC> m i`.

Send code to inferior process commands:

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> m b`       | send buffer and keep code buffer focused
`<SPC> m B`       | send buffer and switch to REPL in insert mode
`<SPC> m f`       | send function and keep code buffer focused
`<SPC> m F`       | send function and switch to REPL in insert mode
`<SPC> m r`       | send region and keep code buffer focused
`<SPC> m R`       | send region and switch to REPL in insert mode
`CTRL+j`          | next item in REPL history
`CTRL+k`          | previous item in REPL history

##### Testing in Python

`Spacemacs` uses [nose][nose] as a test runner. An improved version of
[nose.el][nose.el] is shipped with `Spacemacs`, this version adds:
- windows support
- test suite support

The root of the project is detected with a `.git` directory or a `setup.cfg` file.

Test commands (start with `m t` or `m T`):

    No Debug      |                 Description
------------------|------------------------------------------------------------
<SPC> m t a       | launch all tests of the project
<SPC> m t f       | launch the current test under point
<SPC> m t m       | launch all tests of the current module
<SPC> m t s       | launch all tests of the current suite

     Debug        |                 Description
------------------|------------------------------------------------------------
<SPC> m T a       | launch all tests of the project in debug mode
<SPC> m T f       | launch the current test under point in debug mode
<SPC> m T m       | launch all tests of the current module in debug mode
<SPC> m T s       | launch all tests of the current suite in debug mode

##### Other Python commands

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> m d`       | open documentation in `firefox` using [pylookup][pylookup]
`<SPC> m g`       | go to definition using [emacs-jedi][jedi]
`<SPC> m p`       | add a breakpoint

#### JavaScript

[js2-mode][] will activate for all `*.js` files, along with
[tern-auto-complete][] which will provide the best JavaScript
completion currently available. Just make sure you have the [tern][]
NPM module installed.

Tern includes the following key bindings:

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`M-.`             | jump to the definition of the thing under the cursor.
`M-,`             | brings you back to last place you were when you pressed M-..
`C-c C-r`         | rename the variable under the cursor.
`C-c C-c`         | find the type of the thing under the cursor.
`C-c C-d`         | find docs of the thing under the cursor. Press again to open the associated URL (if any).

#### R (ESS)

**Important**:
In order to speed up the boot time of `Spacemacs`, `ESS` must be loaded
manually via the key binding:

    <SPC> e s s

##### Inferior REPL process

Start an `R` inferior REPL process with `<SPC> m i`.

Send code to inferior process commands:

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> m b`       | send buffer and keep code buffer focused
`<SPC> m B`       | send buffer and switch to REPL in insert mode
`<SPC> m f`       | send function and keep code buffer focused
`<SPC> m F`       | send function and switch to REPL in insert mode
`<SPC> m l`       | send line and keep code buffer focused
`<SPC> m L`       | send line and switch to REPL in insert mode
`<SPC> m r`       | send region and keep code buffer focused
`<SPC> m R`       | send region and switch to REPL in insert mode
`<SPC> m s`       | send region or line and step (debug)
`<SPC> m S`       | send function or paragraph and step (debug)
`CTRL+j`          | next item in REPL history
`CTRL+k`          | previous item in REPL history

##### Other R commands

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> m p`       | object introspection popup [ess-R-object-popup][ess-R-object-popup]
`<SPC> m v p`     | view data under point using [ess-R-data-view][ess-R-data-view] 
`<SPC> m v t`     | view table using [ess-R-data-view][ess-R-data-view] 

#### rcirc

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`CTRL+j`          | next item in command history
`CTRL+k`          | previous item in command history

## Tips

### Tips for Emacs users

If you came here with a pure Emacs background, here are some useful tips to get
you started.

1) As you may have notice, raw Emacs behavior is indeed available in Evil via the
`Emacs state`!

To start you could setup the `Emacs state` as the default one, pressing `fd`
quickly would bring you to `Normal state` and pressing `ESC` from there would
bring you back in `Emacs state`. This way you should never feel lost.

To do so add the following snippet to your `~/.spacemacs`:

```elisp
(defun dotspacemacs/config ()
  "This is were you can ultimately override default Spacemacs configuration.
This function is called at the very end of Spacemacs initialization."
  (setq evil-default-state 'emacs)
  (define-key evil-normal-state-map [escape] 'evil-emacs-state))
```

### Tips for Spacemacs advanced users

1) To Make `lisp state` the default state in `Emacs Lisp` buffers, insert in
your `~/.spacemacs` the following snippet:

```elisp
(defun dotspacemacs/config ()
  (add-hook 'emacs-lisp-mode-hook 'evil-lisp-state))
```

## TODO list

- Add support for [multiple-cursors][multiple-cursors] mode.

## Thank you

[Jokes aside](#contributions), thank you Richard for this great piece of software.

Thank you to the whole Emacs community from core developers to elisp hackers!

[evil]: https://gitorious.org/evil/pages/Home
[evil-leader]: https://github.com/cofi/evil-leader
[RSI]: http://en.wikipedia.org/wiki/Repetitive_strain_injury
[sacha_guide]: http://sachachua.com/blog/2013/05/how-to-learn-emacs-a-hand-drawn-one-pager-for-beginners/
[use-package]: https://github.com/jwiegley/use-package
[keychords]: http://www.emacswiki.org/emacs/KeyChord
[centered-cursor]: http://www.emacswiki.org/emacs/centered-cursor-mode.el
[ace-jump]: https://github.com/winterTTr/ace-jump-mode
[helm]: https://github.com/emacs-helm/helm
[popwin]: http://www.emacswiki.org/emacs/PopWin
[golden-ratio]: https://github.com/roman/golden-ratio.el
[solarized-theme]: https://github.com/bbatsov/solarized-emacs
[powerline]: https://github.com/milkypostman/powerline
[diminish]: http://www.emacswiki.org/emacs/DiminishedModes
[auto-complete]: https://github.com/auto-complete
[auto-highlight]: https://github.com/emacsmirror/auto-highlight-symbol
[e-project]: https://github.com/jrockway/eproject
[projectile]: https://github.com/bbatsov/projectile
[sp]: https://github.com/Fuco1/smartparens
[flycheck]: https://github.com/flycheck
[yasnippet]: https://github.com/capitaomorte/yasnippet
[expand-region]: https://github.com/magnars/expand-region.el
[multiple-cursors]: https://github.com/magnars/multiple-cursors.el
[keybindings]: https://github.com/syl20bnr/vimacs/blob/master/my-keybindings.el
[hswoop]: https://github.com/ShingoFukuyama/helm-swoop
[hcss]: https://github.com/ShingoFukuyama/helm-css-scss
[hyas]: https://github.com/emacs-helm/helm-c-yasnippet
[hthemes]: https://github.com/syohex/emacs-helm-themes
[projectile]: https://github.com/bbatsov/projectile
[hdescbinds]: https://github.com/emacs-helm/helm-descbinds
[hflyspell]: https://gist.github.com/cofi/3013327
[evil-little-word]: https://github.com/tarao/evil-plugins#evil-little-wordel
[evil-visualstar]: https://github.com/bling/evil-visualstar
[evil-exchange]: https://github.com/Dewdrops/evil-exchange
[evil-surround]: https://github.com/timcharper/evil-surround
[camelcasemotion.vim]: http://www.vim.org/scripts/script.php?script_id=1905
[vim-exchange]: https://github.com/tommcdo/vim-exchange
[vim-surround]: https://github.com/tpope/vim-surround
[evil-nerd-commenter]: https://github.com/redguardtoo/evil-nerd-commenter
[nerdcommenter]: https://github.com/scrooloose/nerdcommenter
[evil-org-mode]: https://github.com/edwtjo/evil-org-mode
[nose]: https://github.com/nose-devs/nose/
[nose.el]: https://github.com/syl20bnr/nose.el
[pylookup]: https://github.com/tsgates/pylookup
[jedi]: https://github.com/tkf/emacs-jedi
[edts]: https://github.com/tjarvstrand/edts
[ess-R-object-popup]: https://github.com/myuhe/ess-R-object-popup.el
[ess-R-data-view]: https://github.com/myuhe/ess-R-data-view.el
[monokai-theme]: https://github.com/oneKelvinSmith/monokai-emacs
[zenburn-theme]: https://github.com/bbatsov/zenburn-emacs
[git-gutter]: https://github.com/syohex/emacs-git-gutter-fringe
[magit]: http://magit.github.io/
[smeargle]: https://github.com/syohex/emacs-smeargle
[git-timemachine]: https://github.com/pidu/git-timemachine
[git-messenger]: https://github.com/syohex/emacs-git-messenger
[evil-lisp-state]: https://github.com/syl20bnr/evil-lisp-state
[ido-vertical-mode]: https://github.com/gempesaw/ido-vertical-mode.el
[emacs_live]: https://github.com/overtone/emacs-live
[issues]: https://github.com/syl20bnr/spacemacs/issues
[vundle]: https://github.com/gmarik/Vundle.vim
[anzu]: https://github.com/syohex/emacs-anzu
[js2-mode]: https://github.com/mooz/js2-mode
[tern-auto-complete]: https://github.com/marijnh/tern/blob/master/emacs/tern-auto-complete.el
[tern]: http://ternjs.net/
[themes-megapack]: https://github.com/syl20bnr/spacemacs/tree/master/contrib/themes-megapack
[guide-key]: https://github.com/kai2nenobu/guide-key
[guide-key-tip]: https://github.com/aki2o/guide-key-tip
[gitter]: https://gitter.im/syl20bnr/spacemacs
[CONTRIBUTE.md-PR]: https://github.com/syl20bnr/spacemacs/blob/master/CONTRIBUTE.md#pull-request-guidelines
[CONTRIBUTE.md-CL]: https://github.com/syl20bnr/spacemacs/blob/master/CONTRIBUTE.md#submitting-a-contribution-layer-upstream
[neotree]: https://github.com/jaypei/emacs-neotree
[nerdtree]: https://github.com/scrooloose/nerdtree
[evil-numbers]: https://github.com/cofi/evil-numbers
