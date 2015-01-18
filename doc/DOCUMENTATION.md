# Spacemacs Documentation

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Spacemacs Documentation](#spacemacs-documentation)
- [Philosophy](#philosophy)
    - [Easy](#easy)
    - [Consistency](#consistency)
    - [Crowd-Configured](#crowd-configured)
- [Goals](#goals)
- [Screenshots](#screenshots)
- [Who can benefit from this ?](#who-can-benefit-from-this-)
- [Configuration layers](#configuration-layers)
    - [Structure](#structure)
    - [Extensions and Packages](#extensions-and-packages)
        - [Declaration](#declaration)
        - [Initialization](#initialization)
        - [Exclusion](#exclusion)
    - [Packages synchronization (Vundle like feature)](#packages-synchronization-vundle-like-feature)
    - [Types of configuration layers](#types-of-configuration-layers)
    - [Submitting a configuration layer upstream](#submitting-a-configuration-layer-upstream)
    - [Example: Themes Megapack example](#example-themes-megapack-example)
    - [Managing private configuration layers](#managing-private-configuration-layers)
        - [Using the private directory](#using-the-private-directory)
        - [Using an external Git repository](#using-an-external-git-repository)
        - [Using a personal branch](#using-a-personal-branch)
- [Dotfile Configuration](#dotfile-configuration)
    - [Installation](#installation)
    - [Content](#content)
        - [Using configuration layers](#using-configuration-layers)
        - [Excluding packages](#excluding-packages)
        - [Hooks](#hooks)
        - [Custom variables](#custom-variables)
- [Using the package list buffer](#using-the-package-list-buffer)
    - [Update all the packages](#update-all-the-packages)
- [Main principles](#main-principles)
    - [Evil](#evil)
        - [States](#states)
    - [Evil leader](#evil-leader)
    - [Universal argument](#universal-argument)
    - [Micro-states](#micro-states)
- [Differences between Vim, Evil and Spacemacs](#differences-between-vim-evil-and-spacemacs)
    - [The vim-surround case](#the-vim-surround-case)
- [Evil plugins](#evil-plugins)
- [Color theme](#color-theme)
- [UI elements](#ui-elements)
    - [Toggles](#toggles)
    - [Mode-line](#mode-line)
        - [Flycheck integration](#flycheck-integration)
        - [Anzu integration](#anzu-integration)
        - [Battery status integration](#battery-status-integration)
        - [Powerline separators](#powerline-separators)
        - [Minor Modes](#minor-modes)
- [Font](#font)
- [Commands](#commands)
    - [Reserved prefix command for user](#reserved-prefix-command-for-user)
    - [Escaping](#escaping)
    - [Executing Vim, Emacs and shell commands](#executing-vim-emacs-and-shell-commands)
    - [Navigating](#navigating)
        - [Point/Cursor](#pointcursor)
            - [Smooth scrolling](#smooth-scrolling)
            - [Experimental insert state feature](#experimental-insert-state-feature)
        - [Vim motions with ace-jump mode](#vim-motions-with-ace-jump-mode)
        - [Window manipulation](#window-manipulation)
            - [Resizing windows](#resizing-windows)
            - [Reposition window](#reposition-window)
            - [Golden ratio](#golden-ratio)
        - [Buffers and Files](#buffers-and-files)
            - [Emacs and Spacemacs files](#emacs-and-spacemacs-files)
        - [Ido](#ido)
            - [Experimental Ido feature](#experimental-ido-feature)
        - [NeoTree file tree](#neotree-file-tree)
            - [NeoTree navigation](#neotree-navigation)
            - [Opening files with NeoTree](#opening-files-with-neotree)
            - [Other NeoTree key bindings](#other-neotree-key-bindings)
            - [NeoTree mode-line](#neotree-mode-line)
        - [Shells](#shells)
            - [Key bindings](#key-bindings)
            - [Staying in insert state](#staying-in-insert-state)
        - [Bookmarks](#bookmarks)
    - [Searching](#searching)
        - [Project Searching](#project-searching)
        - [Persistent highlighting](#persistent-highlighting)
        - [Stacking highlights](#stacking-highlights)
        - [Highlight current symbol](#highlight-current-symbol)
        - [Visual Star](#visual-star)
        - [Listing symbols by semantic](#listing-symbols-by-semantic)
        - [Helm-swoop](#helm-swoop)
    - [Editing](#editing)
        - [Text manipulation commands](#text-manipulation-commands)
        - [Smartparens Strict mode](#smartparens-strict-mode)
        - [Zooming](#zooming)
            - [Text](#text)
            - [Frame](#frame)
        - [Increase/Decrease numbers](#increasedecrease-numbers)
        - [Spell checking](#spell-checking)
        - [Region selection](#region-selection)
            - [Expand-region](#expand-region)
            - [Indent text object](#indent-text-object)
        - [Region narrowing](#region-narrowing)
        - [Line formatting](#line-formatting)
        - [Auto-completion](#auto-completion)
        - [Replacing text with iedit](#replacing-text-with-iedit)
            - [iedit states key bindings](#iedit-states-key-bindings)
                - [State transitions](#state-transitions)
                - [In iedit state](#in-iedit-state)
                - [In iedit-insert state](#in-iedit-insert-state)
            - [Examples](#examples)
        - [Commenting](#commenting)
        - [Deleting files](#deleting-files)
        - [Editing Lisp code](#editing-lisp-code)
            - [Key bindings maps](#key-bindings-maps)
                - [Regular normal state bindings](#regular-normal-state-bindings)
                - [Lisp specific bindings](#lisp-specific-bindings)
    - [Project management](#project-management)
    - [Registers](#registers)
    - [Errors handling](#errors-handling)
    - [Compiling](#compiling)
    - [Modes](#modes)
        - [Major Mode leader key](#major-mode-leader-key)
        - [Helm](#helm)
            - [Experimental Helm feature](#experimental-helm-feature)
        - [Ledger](#ledger)
        - [Org](#org)
        - [Python](#python)
        - [JavaScript](#javascript)
        - [rcirc](#rcirc)
        - [HTML and CSS](#html-and-css)
- [Emacs Server](#emacs-server)
    - [Connecting to the Emacs server](#connecting-to-the-emacs-server)
    - [Keeping the server alive](#keeping-the-server-alive)
    - [Troubleshoot](#troubleshoot)
        - [Loading fails](#loading-fails)
        - [I have no file ~/.spacemacs](#i-have-no-file-spacemacs)
- [Tips](#tips)
    - [evil-lisp-state as default state](#evil-lisp-state-as-default-state)
    - ["jk" to trigger evil leader](#jk-to-trigger-evil-leader)
- [Achievements](#achievements)
- [Thank you](#thank-you)

<!-- markdown-toc end -->

# Philosophy

Three core pillars: Easy, Consistency, "Crowd-Configured".

## Easy

`Spacemacs` organizes key bindings by mnemonic namespaces. If you are looking
for commands to operate on your buffer, they are right under <kbd>SPC b</kbd>,
if you want to operate on your project, then it is <kbd>SPC p</kbd>, etc...

`Spacemacs` comes with a dedicated major mode `spacemacs-mode`. Its goal is to
give useful feedbacks and perform maintenance tasks easily.

## Consistency

Similar functionalities should have the same key binding. For instance if you are
looking for the definition of a function, the binding is <kbd>SPC m g</kbd>,
`m` for `major mode` and `g` for `go to`. And no matter what is the major mode it
should be the same binding.

## Crowd-Configured

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

# Goals

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

# Screenshots

_Startup_
![spacemacs_startup](img/spacemacs-startup.png)

_Python_
![spacemacs_python](img/spacemacs-python.png)

_Terminal (urxvt)_
![spacemacs_urxvt](img/spacemacs-urxvt.png)

*Note: Even though screenshots are updated frequently, `Spacemacs` is evolving
quickly and the screenshots may not reflect exactly the current state of the
project.*

# Who can benefit from this ?

`Spacemacs` is first intended to be used by **Vim users** who want to go to the
next level by using Emacs.

It is also a good fit for people wanting to **lower the [risk of RSI][RSI]**
induced by the default Emacs key bindings.

Emacs users wanting to learn **a different way to edit files** or wanting to
learn Vim key bindings.

As a side note, if you are a programmer and you don't know Vim key bindings
yet, I deeply recommend you to learn the basics as recommended in
[Sacha Chua's one-page guide][sacha_guide] about how to learn Emacs.

# Configuration layers

_This part of Spacemacs is still in beta, the structure can change over
time. Refer to commit messages for more information in case of big changes._

## Structure

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

## Extensions and Packages

### Declaration

`Extensions` and `Packages` are declared in variables `<layer>-pre-extensions`,
`<layer>-post-extensions` and `<layer>-packages` where `<layer>` is the layer
name. `Pre-Extensions` are loaded before `Packages` and `Post-Extensions` are
loaded after `Packages`.

They are processed in alphabetical order so sometimes you'll have to use
some `eval-after-load` black magic.

Example:

```elisp
(defvar <layer>-packages
  '(
    package1
    package2
    )
```

### Initialization

To initialize an extension or a package `xxx`, define a function with this
format in `extensions.el` or `packages.el`:

```elisp
(defun <layer>/init-xxx ()
   ...body
)
```

It is common to define the body with the [use-package][use-package] macro.

### Exclusion

It is possible to exclude some packages from `Spacemacs` in a per layer basis.
This is useful when a configuration layer aims to replace a stock package
declared in the `Spacemacs` layer.

To do so add the package names to exclude to the variable
`<layer>-excluded-packages`.

Example:

```elisp
(defvar <layer>-excluded-packages
  '(
    package1
    )
```

## Packages synchronization (Vundle like feature)

`Spacemacs` features a synchronization engine for the ELPA packages. It means
that `Spacemacs` will auto-install the new packages in `<layer>-packages` lists
_and_ auto-delete orphan packages in your `elpa` directory.

It effectively makes `Spacemacs` to behave like [Vundle][vundle].

## Types of configuration layers

There are three types of configuration layers:
- core (this is the `Spacemacs` layer)
- private (in the `private` directory, they are ignored by Git)
- contrib (in the `contrib` directory, those layers are contributions shared by
the community and merged upstream).

## Submitting a configuration layer upstream

If you decide to provide a `contrib` configuration layer, please check
the contribution guidelines in [CONTRIBUTE.md][].

## Example: Themes Megapack example

This is a simple `contrib` configuration layer listing a bunch of themes,
you can find it [here][themes-megapack].

To install it, just add `themes-megapack` to your `~/.spacemacs` like so:

```elisp
dotspacemacs-configuration-layers '(themes-megapack)
```

You have now installed around 100 themes you are free to try with <kbd>SPC T h</kbd>
(helm-themes).

## Managing private configuration layers

`Spacemacs` configuration system is flexible enough to let you manage your
private layers in different ways.

### Using the private directory

Everything in the private directory is ignored by Git so it is a good place
to store private layers. There is a huge drawback to this approach though:
_your layers are not source controlled_.

### Using an external Git repository

This is the recommended way to manage your private layers.

The best approach is to store all your private layers into an external Git
repository. It is especially a good practice to store them in your `dotfiles`
repository if you have one. Put also your `~/.spacemacs` file in it.

Then you are free to symlink your layers into `~/emacs.d/private` _or_ let
them anywhere you want and reference the parent directory in the variable
`dotspacemacs-configuration-layer-path` of your `~/.spacemacs`.

Note that you could also have a dedicated repository for all your private
layers and then directly clone this repository in `~/.emacs.d/private`.

### Using a personal branch

The final main way to manage your private layers is to push them in a personal
branch that you keep up to date with upstream `master` or `develop`.

# Dotfile Configuration

User configuration can be stored in your `~/.spacemacs` file.

## Installation

`~/.spacemacs` is an optional file. If you want to use it you have to copy it
manually from the template file `~/.emacs.d/core/templates/.spacemacs.template`

```sh
$ cp ~/.emacs.d/core/templates/.spacemacs.template ~/.spacemacs
```

## Content

### Using configuration layers

To use a configuration layer, add it to the `dotspacemacs-configuration-layers`
variable of your `~/.spacemacs`.

For instance to add the configuration layer of [RMS](#thank-you):
```elisp
(setq-default dotspacemacs-configuration-layers '(rms))
```
If this layer does not exist you can still try another one in
[the `contrib` directory](https://github.com/syl20bnr/spacemacs/tree/master/contrib).

Configuration layers are expected to be stored in `~/.emacs.d/private` or
`~/.emacs.d/contrib`. But you are free to keep them somewhere else by declaring
additional paths where `Spacemacs` can look for configuration layers.
This is done by setting the list
`dotspacemacs-configuration-layer-path` in your `~/.spacemacs`:

```elisp
(setq-default dotspacemacs-configuration-layer-path '("~/.myconfig/"))
```

### Excluding packages

You can exclude packages you don't want to install with the variable
`dotspacemacs-excluded-packages`, this variable can exclude both packages and
extensions (see [Configuration layers](#configuration-layers) for more info
on packages and extensions).

For instance to disable the `rainbow-delimiters` package:
```elisp
(setq-default dotspacemacs-excluded-packages '(rainbow-delimiters))
```

When you exclude a package, `Spacemacs` will automatically delete it for you
the next time you launch Emacs. All the orphan dependencies are as well
delete automatically.

### Hooks

Two special functions of the `~/.spacemacs` file can be used to perform
configuration at the beginning and end of `Spacemacs` loading process.

- `dotspacemacs/init` is triggered at the very beginning of `Spacemacs`
loading.
- `dotspacemacs/config` is triggered at the very end of `Spacemacs` loading.

### Custom variables

Custom variables configuration from `M-x customize-group` which are
automatically saved by Emacs are stored at the end of your `~/.spacemacs`
file.

# Using the package list buffer

The package list buffer is where you can selectively update one or all
packages installed in your configuration as well as browse for all
available packages in the different Elpa repositories.

`Spacemacs` replaces the default package list buffer with [Paradox][].
Paradox enhances the package list buffer with better feedbacks, new
filters and Github information like the number of stars. Optionally you
can also star packages directly in the buffer.

**Important Note** Don't install new packages from the package list
buffer. If those packages are not referenced in a configuration layer
then `Spacemacs` will treat them as orphans during the next start of
Emacs and they will be deleted.

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>/</kbd>         | evil-search
<kbd>f k</kbd>       | filter by keywords
<kbd>f r</kbd>       | filter by regexp
<kbd>f u</kbd>       | display only installed package with updates available
<kbd>h</kbd>         | go left
<kbd>H</kbd>         | show help (not accurate)
<kbd>j</kbd>         | go down
<kbd>k</kbd>         | go up
<kbd>l</kbd>         | go right
<kbd>L</kbd>         | show last commits
<kbd>n</kbd>         | next search occurrence
<kbd>N</kbd>         | previous search occurrence
<kbd>o</kbd>         | open package homepage
<kbd>r</kbd>         | refresh
<kbd>S P</kbd>       | sort by package name
<kbd>S S</kbd>       | sort by status (installed, available, etc...)
<kbd>S *</kbd>       | sort by Github stars
<kbd>v</kbd>         | `visual state`
<kbd>V</kbd>         | `visual-line state`
<kbd>x</kbd>         | execute (action flags)

## Update all the packages

To update all the buffers:
- open paradox: <kbd>SPC a P</kbd>
- filter packages (optional): <kbd>f u</kbd>
- update all: <kbd>U x y</kbd>

When asked for old packages deletion hit `y`.

# Main principles

## Evil

`Spacemacs` uses the [evil][evil] mode to emulate Vim key bindings. It is a
very complete emulation, maybe the most advanced. In fact, Evil is much more
than just a Vim emulation. It has more states than Vim for instance.

### States

`Spacemacs` has 8 states:

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
                    modify it (see [Editing Lisp code](#editing-lisp-code))
- **Iedit** (red) - exclusive to `Spacemacs`, used to navigate between multiple
                    regions of text using `iedit`
                    (see [Replacing text with iedit](#replacing-text-with-iedit))
- **Iedit Insert** (red) - exclusive to `Spacemacs`, used to replace multiple
                   regions of text using `iedit`
                   (see [Replacing text with iedit](#replacing-text-with-iedit))

Note: Technically speaking there are also the `operator` and `replace` evil
states.

## Evil leader

`Spacemacs` heavily uses the [evil-leader][evil-leader] mode which brings the
Vim leader key to the Emacs world.

This leader key is commonly set to `,` by Vim users, in `Spacemacs` the leader
key is set on <kbd>SPC</kbd> (space bar, this is why the name `spacemacs`).
This key is the most accessible key on a keyboard and it is pressed with the
thumb which is a good choice to lower the risk of [RSI][RSI].

So with `Spacemacs` there is no need to remap your keyboard modifiers to
attempt to reduce the risk of RSI, every command can be executed very easily
while you are in `normal` mode by pressing the <kbd>SPC</kbd> leader key,
here are a few examples:

- Save a buffer: <kbd>SPC f s</kbd>
- Save all opened buffers: <kbd>SPC f S</kbd>
- Open (switch) to a buffer with `helm`: <kbd>SPC b s</kbd>

## Universal argument

The universal argument `C-u` is an important command in Emacs but it is also
a very handy Vim key binding to scroll up.

`Spacemacs` binds <kbd>C-u</kbd> to `scroll-up` and change the universal
argument binding to <kbd>SPC u</kbd>.

## Micro-states

`Spacemacs` defines a wide variety of `micro-states` (temporary overlay maps)
where it makes sense. This prevent from repetitive and tedious presses on the
<kbd>SPC</kbd> key.

When a `micro-state` is active, a documentation is displayed in the minibuffer.
Additional information may as well be displayed in the minibuffer.

[Auto-highlight-symbol micro-state](#auto-highlight-and-edition-of-symbols):
![spacemacs_ahs_micro_state](img/spacemacs-ahs-micro-state.png)

[Text scale micro-state](#change-font-size):
![spacemacs_scale_micro_state](img/spacemacs-scale-micro-state.png)

# Differences between Vim, Evil and Spacemacs

No doubt that `Evil` is one of the most advanced `Vim` emulation and you should
not see big difference between `Vim` and `Emacs`. I did not find any command I
used in Vim that I missed in Emacs with `Evil`.

Send a PR to add the differences you found in this section.

## The vim-surround case

There is one obvious visible difference though. It is not between `Evil` and
`Vim` but between `Spacemacs` and [vim-surround][]: the `surround` command is
on <kbd>S</kbd> in `vim-surround` whereas it is on <kbd>s</kbd> in `Spacemacs`.

This is something that can surprise some Vim users so let me explain why this is
the case:
- `s` and `c` do the same thing in `visual state`,
- `s` is only useful to delete _one_ character and add more than one character
which is a _very_ narrow use case,
- `c` accept motions and can do everything `s` can do in `normal state`,
- this is also true for `r` but `r` is more useful because it stays in
`normal state`.
- `surround` command is just a more powerful command that `s`

If you are not convinced, then here is the snippet to revert back to the default
`Vim + vim-surround` setup (add it to your `dotspacemacs/config` function or
your `~/.spacemacs`):

```elisp
(evil-define-key 'visual evil-surround-mode-map "s" 'evil-substitute)
(evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)
```

# Evil plugins

`Spacemacs` ships with the following evil plugins:

                 Mode                   |             Description
----------------------------------------|--------------------------------------
[evil-leader][]                         | vim leader that bring a new layer of keys in normal mode
[evil-indent-textobject][]              | add text object based on indentation level
[evil-visualstar][]                     | search for current selection with `*`
[evil-exchange][]                       | port of [vim-exchange][]
[evil-surround][]                       | port of [vim-surround][]
[evil-matchit][]                        | port of [matchit.vim][]
[evil-nerd-commenter][]                 | port of [nerdcommenter][]
[evil-search-highlight-persist][]       | emulation of hlsearch behavior
[evil-numbers][]                        | like C-a/C-x in vim
[evil-args][]                           | motions and text objects for arguments
[evil-jumper][]                         | jump list emulation
[NeoTree][neotree]                      | mimic [NERD Tree][nerdtree]

# Color theme

By default, `Spacemacs` uses the theme [solarized-light][solarized-theme].

It is possible to define your default theme in your `~/.spacemacs` with
the variable `dotspacemacs-default-theme`. For instance, to specify `zenburn`:

```elisp
(setq-default
 ;; Default theme applied at startup
 dotspacemacs-default-theme 'zenburn)
```

Some themes are supported by `Spacemacs`:
- [Solarized][solarized-theme]
- [Leuven][leuven-theme]
- [Monokai][monokai-theme]
- [Zenburn][zenburn-theme]

It is possible to set any other themes but their compatibility with `Spacemacs`
is not guaranteed (i.e. there may be some missing faces etc...).

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC T n</kbd>   | switch to next theme supported by `Spacemacs`.
<kbd>SPC h t</kbd>   | select a theme using a `helm` buffer.

**Note:** Due to the inner working of themes in Emacs, switching theme during
the same session may have some weird side effects. Although these side effects
should be pretty rare (especially when switching to a supported theme).

**Hint** If you are an `Org` user, [leuven-theme][] is amazing.

# UI elements

`Spacemacs` has a minimalistic and distraction free UI with a lot of subtle
customization which make it unique compared to other kits:
 - beautiful custom [powerline][powerline] mode-line
 [with color feedback](#flycheck-integration) according to current
 [Flycheck][flycheck]
 status
 - unicode symbols for minor mode lighters which appear in the mode-line
 - [custom fringe bitmaps](#errors-handling) and error feedbacks for
 [Flycheck][flycheck]
 - [custom fringe bitmaps](../contrib/git/README.md#git-gutter-bitmaps) for [git gutter][]
 - dedicated startup page with a mode aimed at easily managing `Spacemacs`

## Toggles

Some UI indicators can be toggled on and off (toggles start with `t`):

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC t 8</kbd>    | display a mark on the 80th column
<kbd>SPC t F</kbd>    | toggle frame fullscreen
<kbd>SPC t f</kbd>    | toggle display of the fringe
<kbd>SPC t i</kbd>    | toggle aggressive indent
<kbd>SPC t l</kbd>    | toggle truncate lines
<kbd>SPC t L</kbd>    | toggle visual lines
<kbd>SPC t M</kbd>    | toggle frame maximize
<kbd>SPC t n</kbd>    | show the absolute line numbers
<kbd>SPC t t</kbd>    | toggle frame transparency
<kbd>SPC t T</kbd>    | toggle tool bar
<kbd>SPC t U</kbd>    | toggle menu bar

## Mode-line

The mode line is an heavily customized [powerline][powerline] with the
following capabilities:
- show the window number
- color code for current state
- show the number of search occurrences via anzu
- toggle flycheck info
- toggle battery info
- toggle minor mode lighters

Reminder of the color codes for the states:

   Evil State      |       Color
-------------------|------------------
Normal             | Orange
Insert             | Green
Visual             | Grey
Emacs              | Blue
Motion             | Purple
Lisp               | Pink
Iedit/Iedit-Insert | Red

Some elements can be dynamically toggled:

    Key Binding        |                 Description
-----------------------|------------------------------------------------------------
<kbd>SPC t m m</kbd>   | toggle the minor mode lighters
<kbd>SPC t m b</kbd>   | toggle the battery status
<kbd>SPC t m f</kbd>   | toggle the flycheck info
<kbd>SPC t m v</kbd>   | toggle the new version lighter

### Flycheck integration

When [Flycheck][flycheck] minor mode is enabled, a new element appears showing
the number of errors, warnings and info.

![powerline-wave](img/powerline-wave.png)

### Anzu integration

[Anzu][anzu] shows the number of occurrence when performing a search. `Spacemacs`
integrates nicely the Anzu status by displaying it temporarily when `n` or `N` are
being pressed. See the `5/6` segment on the screenshot below.

![powerline-anzu](img/powerline-anzu.png)

### Battery status integration

[fancy-battery][] displays the percentage of total charge of the battery as
well as the time remaining to charge or discharge completely the battery.

A color code is used for the battery status:

 Battery State    |       Color
------------------|------------------
Charging          | Green
Discharging       | Orange
Critical          | Red

Note the these colors may vary depending on your theme.

### Powerline separators

It is possible to easily customize the `powerline separator` by setting the
`powerline-default-separator` variable in your `~./spacemacs`. For instance
if you want to set back the separator to the well-known `arrow` separator
add the following snippet to your configuration file:

```elisp
(defun dotspacemacs/config ()
  "This is were you can ultimately override default Spacemacs configuration.
This function is called at the very end of Spacemacs initialization."
  (setq powerline-default-separator 'arrow)
)
```

To save you the time to try all the possible separators provided by the
powerline, here is an exhaustive set of screenshots:

    Separator     |                 Screenshot
------------------|------------------------------------------------------------
`alternate`       | ![powerline-alternate](img/powerline-alternate.png)
`arrow`           | ![powerline-arrow](img/powerline-arrow.png)
`arrow-fade`      | ![powerline-arrow-fade](img/powerline-arrow-fade.png)
`bar`             | ![powerline-bar](img/powerline-bar.png)
`box`             | ![powerline-box](img/powerline-box.png)
`brace`           | ![powerline-brace](img/powerline-brace.png)
`butt`            | ![powerline-butt](img/powerline-butt.png)
`chamfer`         | ![powerline-chamfer](img/powerline-chamfer.png)
`contour`         | ![powerline-contour](img/powerline-contour.png)
`curve`           | ![powerline-curve](img/powerline-curve.png)
`rounded`         | ![powerline-rounded](img/powerline-rounded.png)
`roundstub`       | ![powerline-roundstub](img/powerline-roundstub.png)
`slant`           | ![powerline-slant](img/powerline-slant.png)
`wave`            | ![powerline-wave](img/powerline-wave.png)
`zigzag`          | ![powerline-zigzag](img/powerline-zigzag.png)
`nil`             | ![powerline-nil](img/powerline-nil.png)

### Minor Modes

`Spacemacs` uses [diminish][diminish] mode to reduce the size of minor mode
indicators:

The minor mode area can be toggled on and off with:

    <SPC> t m m

Unicode symbols are displayed by default. Setting the variable
`dotspacemacs-mode-line-unicode-symbols` to `nil` in your `~/.spacemacs` will
display ASCII characters instead (may be useful in terminal).

   Unicode   |   ASCII    |                    Mode
:-----------:|:----------:|----------------------------------------------------
`⊞`          | G          | [golden-ratio][golden-ratio] mode
`Ⓐ`          | A          | [auto-complete][auto-complete] mode
`Ⓒ`          | C          | [centered-cursor][centered-cursor] mode
`Ⓔ`          | E          | [evil-org][evil-org-mode] mode
`Ⓕ`          | F          | flycheck mode
`Ⓚ`          | K          | guide-key mode
`Ⓘ`          | I          | aggressive indent mode
`(Ⓟ)`        | (P)        | paredit mode
`Ⓢ`          | S          | flyspell mode
`(Ⓢ)`        | (S)        | [smartparens][sp] mode
`Ⓦ`          | W          | whitespace mode
`Ⓨ`          | Y          | [yasnippet][yasnippet] mode

# Font

The default font used by `Spacemacs` is [source code pro][] by Adobe. It is
recommended to install it on your system.

Basic font support is provided in `Spacemacs`, you can change the font and
its size using the function `spacemacs/set-font` in the `dotspacemacs/config`
function of your `~/.spacemacs`.

```elisp
(defun dotspacemacs/config ()
  (spacemacs/set-font "DejaVu Sans Mono" 10)
)
```

# Commands

Every sequences must be performed in `normal` mode.

## Reserved prefix command for user

<kbd>SPC o</kbd> is reserved for the user. Setting key bindings behind `<SPC> o`
is **guaranteed** to never conflict with `Spacemacs` defaults key bindings.

## Escaping

`Spacemacs` uses [evil-escape][] to easily switch between `insert state` and
`normal state` with the key chord `fd`.

The choice of `fd` was made to be able to use the same sequence to escape from
"everything" in Emacs:
- escape from all evil states to normal state
- escape from evil-lisp-state to normal state
- abort evil ex command
- quit minibuffer
- abort isearch
- quit magit buffers
- quit help buffers
- quit apropos buffers
- quit ert buffers
- quit undo-tree buffer
- quit paradox
- quit gist-list menu
- hide neotree buffer

This sequence can be customized in your `~/.spacemacs`, but `evil-escape`
is not guaranteed to work properly with sequences based on `h j k or l` so
it is recommended to avoid defining sequences like `jj` or `jk`.

Example to set it to `jn` (it is important to put it in `dotspacemacs/init`):

```elisp
(defun dotspacemacs/init ()
  (setq-default evil-escape-key-sequence (kbd "jn"))
)
```

## Executing Vim, Emacs and shell commands

    Command      |                 Key Binding
:---------------:|------------------------------------------------------------------
Vim (ex-command) | <kbd>`:`</kbd>
Emacs (M-x)      | <kbd>SPC :</kbd>
Shell            | <kbd>SPC !</kbd> or just <kbd>!</kbd>

The command key `:` can be easily changed with the variable
`dotspacemacs-command-key` of your `~/.spacemacs`. Note that is will change both
`:` and `SPC :` bindings to keep the symmetry between Vim and Emacs. A good
key can be `,` for example.

## Navigating

### Point/Cursor

Navigation is performed using the Vi key bindings `hjkl`.

Key Binding          |                 Description
---------------------|------------------------------------------------------------------
<kbd>`h`</kbd>       | move cursor left
<kbd>`j`</kbd>       | move cursor down
<kbd>`k`</kbd>       | move cursor up
<kbd>`l`</kbd>       | move cursor right
<kbd>`H`</kbd>       | move quickly up (10 lines at a time)
<kbd>`L`</kbd>       | move quickly down (10 lines at a time)
<kbd>SPC j h</kbd>   | go to the beginning of line (and set a mark at the previous location in the line)
<kbd>SPC j l</kbd>   | go to the end of line (and set a mark at the previous location in the line)
<kbd>SPC z z</kbd>   | lock the cursor at the center of the screen

#### Smooth scrolling

[smooth-scrolling]() prevent the point to jump when it reaches the top or
bottom of the screen. It is enabled by default.

On Windows, you may want to disable it. To disable the smooth scrolling set
the `dotspacemacs-smooth-scrolling` variable in your `~/.spacemacs` to `nil`:

```elisp
(setq-default dotspacemacs-smooth-scrolling t)
```

#### Experimental insert state feature

If `dotspacemacs-feature-toggle-leader-on-jk` is non nil, pressing `jk` while
in `insert state` will trigger the evil leader as if you pressed <kbd>SPC</kbd> in
normal mode.

### Vim motions with ace-jump mode

`Spacemacs` uses the `evil` integration of [ace-jump mode][ace-jump] which
enables the invocation of `ace-jump-mode` during motions.

It is useful for deleting visually a set of lines, try the following sequence
in a buffer containing some text:

    d <SPC> l

Key Binding          |                 Description
---------------------|------------------------------------------------------------------
<kbd>SPC SPC</kbd>   | initiate ace jump word mode
<kbd>SPC l</kbd>     | initiate ace jump line mode
<kbd>SPC `</kbd>     | go back to the previous location (before the jump)

Hint: you may change to char mode by `C-c C-c` in word mode.

### Window manipulation

Every window has a number displayed at the start of the mode-line and can
be quickly accessed using `<SPC> number`.

Key Binding         |                    Description
--------------------|----------------------------------------------------------------
<kbd>SPC 1</kbd>    | go to first window
<kbd>SPC 2</kbd>    | go to window number 2
<kbd>SPC 3</kbd>    | go to window number 3
<kbd>SPC 4</kbd>    | go to window number 4
<kbd>SPC 5</kbd>    | go to window number 5
<kbd>SPC 6</kbd>    | go to window number 6
<kbd>SPC 7</kbd>    | go to window number 7
<kbd>SPC 8</kbd>    | go to window number 8
<kbd>SPC 9</kbd>    | go to window number 9
<kbd>SPC 0</kbd>    | go to window number 10

Windows manipulation commands (start with `w`):

Key Binding                               |                 Description
------------------------------------------|----------------------------------------------------------------
<kbd>SPC w c</kbd>                        | close a window
<kbd>SPC w d</kbd>                        | toggle window dedication (dedicated window cannot be reused by a mode)
<kbd>SPC w H</kbd>                        | move window to the left
<kbd>SPC w J</kbd>                        | move window to the bottom
<kbd>SPC w K</kbd>                        | move window to the top
<kbd>SPC w L</kbd>                        | move window to the right
<kbd>SPC w m</kbd>                        | maximize/minimize a window
<kbd>SPC w M</kbd>                        | maximize/minimize a window, when maximized the buffer is centered
<kbd>SPC w o</kbd>                        | cycle and focus between frames
<kbd>SPC w p m</kbd>                      | open messages buffer in a popup window
<kbd>SPC w p p</kbd>                      | close the current sticky popup window
<kbd>SPC w r</kbd>                        | initiate window size micro-state
<kbd>SPC w R</kbd>                        | rotate windows clockwise
<kbd>SPC w s</kbd> or <kbd>SPC w /</kbd>  | horizontal split
<kbd>SPC w S</kbd>                        | horizontal split and focus new window
<kbd>SPC w u</kbd>                        | undo window layout (used to effectively undo a closed window)
<kbd>SPC w U</kbd>                        | redo window layout
<kbd>SPC w v</kbd> or  <kbd>SPC w -</kbd> | vertical split
<kbd>SPC w V</kbd>                        | vertical split and focus new window
<kbd>SPC w w</kbd>                        | cycle and focus between windows

#### Resizing windows

`Spacemacs` defines a micro-state to resize windows.

Key Binding         | Description
--------------------|------------------------------------------------------------
<kbd>SPC w S</kbd>  | initiate micro-state
<kbd>H</kbd>        | shrink window horizontally
<kbd>J</kbd>        | shrink window vertically
<kbd>K</kbd>        | enlarge window vertically
<kbd>L</kbd>        | enlarge window horizontally
Any other key       | leave the micro-state

The micro-state text in minibuffer display the following information:

    [WidthxHeight] Resize window: (H/L) shrink/enlarge horizontally, (J/K) shrink/enlarge vertically

#### Reposition window

Key Binding         | Description
--------------------|------------------------------------------------------------
<kbd>z f</kbd>      | Make current function or comments visible

`z f` tries to accommodate current function or comments into window as much as possible.

#### Golden ratio

If you resize windows like crazy you may want to give a try to [golden-ratio][].

`golden-ratio` resizes windows dynamically depending on whether they are
selected or not. By default `golden-ratio` is off.

The mode can be toggled on and off with:

    <SPC> t g

### Buffers and Files

`Spacemacs` uses `ido` for opening files since `ido` way to navigate
the file system is better than `helm` in my opinion (especially because `ido` can
remember the last selected directories and buffers, maybe helm can do this ?).
`ido` is also used to kill buffers.

Buffer manipulation commands (start with `b`):

Key Binding                               |              Description
------------------------------------------|----------------------------------------------------------------
<kbd>SPC b b</kbd> or <kbd>SPC TAB</kbd>  | switch to alternate buffer (switch back and forth)
<kbd>SPC b e</kbd>                        | erase the content of the buffer (ask for confirmation)
<kbd>SPC b k</kbd>                        | kill the current buffer
<kbd>SPC b K</kbd>                        | kill all buffers except the current one
<kbd>SPC b C-K</kbd>                      | kill all buffers matching the regexp
<kbd>SPC b m h</kbd>                      | move a buffer to the left
<kbd>SPC b m j</kbd>                      | move a buffer to the bottom
<kbd>SPC b m k</kbd>                      | move a buffer to the top
<kbd>SPC b m l</kbd>                      | move a buffer to the right
<kbd>SPC b n</kbd>                        | switch to next buffer
<kbd>SPC b p</kbd>                        | switch to previous buffer
<kbd>SPC b r</kbd>                        | rename the current buffer
<kbd>SPC b R</kbd>                        | revert the current buffer (reload from disk)
<kbd>SPC b s</kbd>                        | switch to a buffer using `helm`
<kbd>SPC b w</kbd>                        | toggle read-only (writable state)

Files manipulation commands (start with `f`):

Key Binding                               |                 Description
------------------------------------------|----------------------------------------------------------------
<kbd>SPC f d</kbd>                        | delete a file and the associated buffer (ask for confirmation)
<kbd>SPC f f</kbd>                        | open a file using `ido`
<kbd>SPC f j</kbd>                        | jump to the current buffer file in dired
<kbd>SPC f o</kbd>                        | open a file using the default external program
<kbd>SPC f s</kbd>                        | save a file
<kbd>SPC f S</kbd>                        | save all files
<kbd>SPC f r</kbd>                        | open a recent file with `helm`
<kbd>SPC f t</kbd>                        | toggle file tree side bar using [NeoTree][neotree]
<kbd>SPC f y</kbd>                        | show current file absolute path in the minibuffer

#### Emacs and Spacemacs files

Convenient key bindings are located under the prefix <kbd>SPC f e</kbd> to
quickly navigate between `Emacs` and `Spacemacs` specific files.

Key Binding                               |                 Description
------------------------------------------|----------------------------------------------------------------
<kbd>SPC f e c</kbd>                      | open `ido` in the `contrib` folder
<kbd>SPC f e d</kbd>                      | open the spacemacs dotfile (`~/.spacemacs`)
<kbd>SPC f e h</kbd>                      | discover `Spacemacs` layers and packages using `helm`
<kbd>SPC f e i</kbd>                      | open the all mighty `init.el`
<kbd>SPC f e s</kbd>                      | open `ido` in the `spacemacs` layer folder

### Ido

`Spacemacs` displays the `ido` minibuffer vertically thanks to the
[ido-vertical-mode][ido-vertical-mode].

Basic `ido` operations can be done with `Ctrl` key:

Key Binding             |                 Description
------------------------|----------------------------------------------------------------
<kbd>C-\<return\></kbd> | open a `dired buffer`
<kbd>M-\<return\></kbd> | open a `dired buffer` in terminal
<kbd>C-d</kbd>          | delete selected file (ask for confirmation)
<kbd>C-h</kbd>          | go to parent directory
<kbd>C-j</kbd>          | select next file or directory
<kbd>C-S-j</kbd>        | go to next directory
<kbd>C-k</kbd>          | select previous file or directory
<kbd>C-S-k</kbd>        | go to previous directory
<kbd>C-l</kbd>          | open the selected file
<kbd>C-n</kbd>          | next history element
<kbd>C-o</kbd>          | open selected file in other window
<kbd>C-p</kbd>          | previous history element
<kbd>C-s</kbd>          | open selected file in a vertically split window
<kbd>C-t</kbd>          | open selected file in a new frame
<kbd>C-v</kbd>          | open selected file in a horizontally split window

#### Experimental Ido feature

If `dotspacemacs-feature-toggle-leader-on-jk` is non nil, pressing `jk` while
in `ido` minibuffer will trigger the evil leader.

When evil leader is triggered the following commands are available:

Key Binding   |                 Description
--------------|----------------------------------------------------------------
<kbd>s</kbd>  | open selected file in a vertically split window
<kbd>t</kbd>  | open selected file in a new frame
<kbd>v</kbd>  | open selected file in a horizontally split window
<kbd>x</kbd>  | open selected file in other window

### NeoTree file tree

`Spacemacs` provides a quick and simple way to navigate in an unknown project
file tree with [NeoTree][neotree].

To toggle the `NeoTree` buffer press:

    <SPC> f t

The NeoTree window always has the number `0` so it does not shift the current
number of the other windows. To select the NeoTree window you then use
<kbd>SPC 0</kbd>.

#### NeoTree navigation

Navigation is centered on the `hjkl` with the hope to provide a fast navigation
experience like in [ranger][]:

Key Binding                      |                 Description
---------------------------------|----------------------------------------------------------------
<kbd>h</kbd>                     | collapse expanded directory or go to parent node
<kbd>H</kbd>                     | previous sibling
<kbd>j</kbd>                     | next file or directory
<kbd>J</kbd>                     | next expanded directory on level down
<kbd>k</kbd>                     | previous file or directory
<kbd>K</kbd>                     | parent directory, when reaching the root change it to parent directory
<kbd>l</kbd> or <kbd>RET</kbd>   | expand directory
<kbd>L</kbd>                     | next sibling

**Note:** The point is automatically set to the first letter of a node for a
smoother experience.

#### Opening files with NeoTree

By default a file is opened in the last active window. It is possible to choose
window number where to open a file by using a numeric argument, for instance
<kbd>2 l</kbd> or <kbd>2 RET</kbd> will open the current file in the windows 2.
It is also possible to open the file in a split window with <kbd>|</kbd> and
<kbd>-</kbd>:

Key Binding                       |                 Description
----------------------------------|----------------------------------------------------------------
<kbd>l</kbd> or <kbd>RET</kbd>    | open file in last active window
<kbd># l</kbd> or <kbd>2 RET</kbd>| open file in window number `#`
<kbd>|</kbd>                      | open file in an vertically split window
<kbd>-</kbd>                      | open file in an horizontally split window

#### Other NeoTree key bindings

Key Binding                      |                 Description
---------------------------------|----------------------------------------------------------------
<kbd>TAB</kbd>                   | toggle stretching of the buffer
<kbd>c</kbd>                     | create a node
<kbd>d</kbd>                     | delete a node
<kbd>g</kbd>                     | refresh
<kbd>s</kbd>                     | toggle showing of hidden files
<kbd>q</kbd> or <kbd>fd</kbd>    | hide `NeoTree` buffer
<kbd>r</kbd>                     | rename a node

#### NeoTree mode-line

The mode-line has the following format `[x/y] d (D:a, F:b)` where:
- `x` is the index of the current selected file or directory
- `y` the total number of items (file and directory) in the current directory
- `d` the name of the current directory
- `a` the number of directories in the current directory
- `b` the number of files in the current directory

### Shells

#### Key bindings

Key Binding         |                 Description
--------------------|----------------------------------------------------------------
<kbd>C-j</kbd>      | next item in history
<kbd>C-k</kbd>      | previous item in history
<kbd>SPC m h</kbd>  | browse history with `helm` (works in `eshell` and `shell`)

#### Staying in insert state

Navigating in shell buffers can be tricky because it is not possible to use the
leader in `insert state`. Switching back and forth between normal and insert
states can be tedious.

There are two solutions for this:
- use <kbd>C-o</kbd> then use the leader key
- enable the [leader on `jk`](#experimental-insert-state-feature)
experimental feature.

### Bookmarks

Bookmarks can be set anywhere in a file. Bookmarks are persistent. They are very
useful to jump to/open a known project. `Spacemacs` used `helm-bookmarks` to
manage them.

Open an `helm` window with the current bookmarks by pressing:

    <SPC> h b

Then in the `helm-bookmarks` buffer:

Key Binding        |                 Description
-------------------|----------------------------------------------------------------
<kbd>CTRL+d</kbd>  | delete the selected bookmark
<kbd>CTRL+e</kbd>  | edit the selected bookmark
<kbd>CTRL+f</kbd>  | toggle filename location
<kbd>CTRL+o</kbd>  | open the selected bookmark in another window

To save a new bookmark, just type the name of the bookmark and press `RET`.

## Searching

### Project Searching

Key Binding                           |                 Description
--------------------------------------|---------------------------------------------
<kbd>SPC /</kbd> or  <kbd>SPC a</kbd> | with [The Silver Searcher][ag]
<kbd>SPC A</kbd>                      | with `ack`
<kbd>SPC g</kbd>                      | with `grep`
<kbd>SPC h l</kbd>                    | show last helm popup

### Persistent highlighting

`Spacemacs` uses `evil-search-highlight-persist` to keep the searched expression
highlighted until the next search. It is also possible to clear the
highlighting by pressing <kbd>SPC s c</kbd> or executing the ex command `:noh`.

### Stacking highlights

With [hl-anything][] it is possible to highlight all occurrences of the word
under point. The highlights can be stacked.

Key Binding            |                 Description
-----------------------|----------------------------------------------------------------
<kbd>SPC h c</kbd>     | clear the highlightings
<kbd>SPC h g c</kbd>   | clear the highlightings globally (all opened buffers)
<kbd>SPC h h</kbd>     | highlight all occurrence of the word at point
<kbd>SPC h g h</kbd>   | highlight all occurrence of the word at point globally (all opened buffers)
<kbd>SPC h n</kbd>     | next highlighted occurrence
<kbd>SPC h N</kbd>     | previous highlighted occurrence
<kbd>SPC h p</kbd>     | toggle auto-highlight of the enclosing parenthesis
<kbd>SPC h r</kbd>     | restore saved highlights in the current buffer
<kbd>SPC h s</kbd>     | save current highlights

### Highlight current symbol

`Spacemacs` supports highlighting of the current symbol on demand (provided by
the [auto-highlight-symbol][auto-highlight] mode) and add a micro-state to
easily navigate and rename this symbol.

It is also possible to change the range of the navigation on the fly to:
- buffer
- function
- visible area

To initiate the highlighting of the current symbol under point press
<kbd>SPC s h</kbd>.

Navigation between the highlighted symbols can be done with the commands:

Key Binding            | Description
-----------------------|------------------------------------------------------------
<kbd>*</kbd>           | initiate navigation micro-state
<kbd>SPC s b</kbd>     | go to the last searched occurrence of the last highlighted symbol
<kbd>SPC s e</kbd>     | edit all occurrences of the current symbol(*)
<kbd>SPC s h</kbd>     | highlight the current symbol and all its occurrence within the current range
<kbd>SPC s R</kbd>     | change range to default (`whole buffer`)

In 'Spacemacs' highlight symbol micro-state:

Key Binding   | Description
--------------|------------------------------------------------------------
<kbd>e</kbd>  | edit occurrences (*)
<kbd>n</kbd>  | go to next occurrence
<kbd>N</kbd>  | go to previous occurrence
<kbd>d</kbd>  | go to next definition occurrence
<kbd>D</kbd>  | go to previous definition occurrence
<kbd>r</kbd>  | change range (`function`, `display area`, `whole buffer`)
<kbd>R</kbd>  | go to home occurrence (reset position to starting occurrence)
Any other key | leave the navigation micro-state

(*) using [iedit][] or the default implementation of `auto-highlight-symbol`

The micro-state text in minibuffer display the following information:

    <M> [6/11]* press (n/N) to navigate, (e) to edit, (r) to change range or (R) for reset

Where `<M> [x/y]*` is:
- M: the current range mode
  - `<B>`: whole buffer range
  - `<D>`: current display range
  - `<F>`: current function range
- `x`: the index of the current highlighted occurrence
- `y`: the total number of occurrences
- `*`: appears if there is at least one occurrence which is not currently
visible.

### Visual Star

With [evil-visualstar][] you can search for the next occurrence of the current
selection.

It is pretty useful combined with the [expand-region](#region-selection)
bindings.

_Note:_ If the current state is not the `visual state` then pressing `*` uses
[auto-highlight-symbol](#auto-highlight-symbols) and its micro-state.

### Listing symbols by semantic

Use `helm-semantic-or-imenu` command from `Helm` to quickly navigate between
the symbols in a buffer.

To list all the symbols of a buffer press:

    <SPC> s l

### Helm-swoop

This is very similar to `moccur`, it displays a `helm` buffer with all the
occurrences of the word under point. You can then change the search query
in real-time and navigate between them easily.

You can even edit the occurrences directly in the `helm` buffer and apply
the modifications to the buffer.

Key Binding            |                    Description
-----------------------|----------------------------------------------------------------
<kbd>SPC s s</kbd>     | execute `helm-swoop`
<kbd>SPC s S</kbd>     | execute `helm-multi-swoop`
<kbd>SPC s C-s</kbd>   | execute `helm-multi-swoop-all`

## Editing

### Text manipulation commands

Text related commands (start with `x`):

    Key Binding        |                 Description
-----------------------|------------------------------------------------------------
<kbd>SPC x u</kbd>     | set the selected text to lower case
<kbd>SPC x U</kbd>     | set the selected text to upper case
<kbd>SPC x d w</kbd>   | delete trailing whitespaces
<kbd>SPC x g l</kbd>   | set languages used by translate commands
<kbd>SPC x g t</kbd>   | translate current word using Google Translate
<kbd>SPC x g T</kbd>   | reverse source and target languages
<kbd>SPC x m j</kbd>   | move down a line of text
<kbd>SPC x m k</kbd>   | move up a line of text
<kbd>SPC x t c</kbd>   | swap (transpose) the current character with the previous one
<kbd>SPC x t w</kbd>   | swap (transpose) the current word with the previous one
<kbd>SPC x t l</kbd>   | swap (transpose) the current line with the previous one
<kbd>SPC x w c</kbd>   | count the number of words in the selection region
<kbd>SPC x w C</kbd>   | count the number of occurrences per word in the select region

### Smartparens Strict mode

[Smartparens][sp] comes with a strict mode which prevents deletion of
parenthesis if the result is unbalanced.

This mode can be frustrating for novices, this is why it is not enabled by
default.

It is possible to enable it easily for _all programming modes_ with the
variable `dotspacemacs-smartparens-strict-mode` of you `~/.spacemacs`.

```elisp
(setq-default dotspacemacs-smartparens-strict-mode t)
```

### Zooming

#### Text

The font size of the current buffer can be adjusted with the commands:

Key Binding            | Description
-----------------------|------------------------------------------------------------
<kbd>SPC z x +</kbd>   | scale up the font and initiate the font scaling micro-state
<kbd>SPC z x -</kbd>   | scale down the font and initiate the font scaling micro-state
<kbd>SPC z x =</kbd>   | reset the font size (no scaling) and initiate the font scaling micro-state
<kbd>+</kbd>           | increase the font size
<kbd>-</kbd>           | decrease the font size
<kbd>=</kbd>           | reset the font size
Any other key          | leave the font scaling micro-state

Note that _only_ the text of the current buffer is scaled, the other buffers,
the mode-line and the minibuffer are not affected. To zoom the whole content of
a frame use the `zoom frame` bindings (see next section).

#### Frame

You can zoom in and out the whole content of the frame with the commands:

Key Binding            | Description
-----------------------|------------------------------------------------------------
<kbd>SPC z f +</kbd>   | zoom in the frame content
<kbd>SPC z f -</kbd>   | zoom out the frame content
<kbd>SPC z f =</kbd>   | reset the frame content size
<kbd>+</kbd>           | zoom in
<kbd>-</kbd>           | zoom out
<kbd>=</kbd>           | reset zoom
Any other key          | leave the zoom frame micro-state

### Increase/Decrease numbers

`Spacemacs` uses [evil-numbers][] to easily increase or increase numbers.

Key Binding            | Description
-----------------------|------------------------------------------------------------
<kbd>SPC n +</kbd>     | increase the number under point by one and initiate micro-state
<kbd>SPC n -</kbd>     | decrease the number under point by one and initiate micro-state

In micro-state:

Key Binding   | Description
--------------|------------------------------------------------------------
<kbd>+</kbd>  | increase the number under point by one
<kbd>-</kbd>  | decrease the number under point by one
Any other key | leave the micro-state

**Tips:** you can increase or decrease a value by more that once by using a
prefix argument (ie. `10 SPC n +` will add 10 to the number under point).

### Spell checking

Spell checking commands start with `S`:

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC S c</kbd>   | list of corrections in a `helm` buffer
<kbd>SPC S d</kbd>   | change dictionary language
<kbd>SPC S n</kbd>   | go to the next spell check error


### Region selection

Vi `Visual` modes are all supported by `evil`.

#### Expand-region

`Spacemacs` adds another `Visual` mode via the [expand-region][] mode.

Key Binding        |                 Description
-------------------|----------------------------------------------------------------
<kbd>SPC v</kbd>   | initiate expand-region mode then...
<kbd>v</kbd>       | expand the region by one semantic unit
<kbd>V</kbd>       | contract the region by one semantic unit
<kbd>r</kbd>       | reset the region to initial selection
<kbd>ESC</kbd>     | leave expand-region mode

#### Indent text object

With [evil-indent-textobject] the following action can be performed in
`normal state`:
- <kbd>ii</kbd> - Inner Indentation: the surrounding textblock with the same
indentation
- <kbd>ai</kbd> - Above and Indentation: ii + the line above with a different
indentation
- <kbd>aI</kbd> - Above and Indentation+: ai + the line below with a different
indentation

Example (`|` is the point):

```elisp
(while (not done)
  (messa|ge "All work and no play makes Jack a dull boy."))
(1+ 41)
```

- <kbd>vii</kbd> will select the line with message
- <kbd>vai</kbd> will select the whole while loop
- <kbd>vaI</kbd> will select the whole fragment

### Region narrowing

The displayed text of a buffer can be narrowed with the commands
(start with `n`):

Key Binding            |                 Description
-----------------------|----------------------------------------------------------------
<kbd>SPC n f</kbd>     | narrow the buffer to the current function
<kbd>SPC n p</kbd>     | narrow the buffer to the visible page
<kbd>SPC n r</kbd>     | narrow the buffer to the selected text
<kbd>SPC n w</kbd>     | widen, i.e show the whole buffer again

### Line formatting

`Spacemacs` performs `go to the line below point and indent it`  with
<kbd>SPC j k</kbd>.
You may repeat this operation with `evil-repeat` if you need to indent many lines.

Line formatting commands start with `j`:

    Key Binding            |                 Description
---------------------------|------------------------------------------------------------
<kbd>J</kbd>               | join the current line with the next line
<kbd>SPC j j</kbd>         | same as <kbd>SPC j k</kbd> but will split the current line at point
<kbd>SPC J</kbd>           | split a quoted string or s-expression in place
<kbd>SPC j J</kbd>         | split a quoted string or s-expression and auto-indent
<kbd>SPC j k</kbd>         | go to next line and indent it using auto-indent rules

Used together these key bindings are very powerful to quickly reformat the code.

### Auto-completion

`Spacemacs` uses [auto-complete][] auto-completion engine.

    Key Binding    |                 Description
-------------------|------------------------------------------------------------
<kbd>C-j</kbd>     | select next candidate
<kbd>C-k</kbd>     | select previous candidate
<kbd>TAB</kbd>     | expand selection or select next candidate
<kbd>S-TAB</kbd>   | select previous candidate
<kbd>return</kbd>  | complete word, if word is already completed insert a carriage return

### Replacing text with iedit

`Spacemacs` uses the powerful [iedit][] mode through [evil-iedit-state][] to
quickly edit multiple occurrences of a symbol or selection.

`evil-iedit-state` defines two new evil states:
- `iedit state`
- `iedit-insert state`

The color code for these states is `red`.

`evil-iedit-state` has also a nice integration with [expand-region][] for quick
edition of the current selected text by pressing <kbd>e</kbd>.

#### iedit states key bindings

##### State transitions

    Key Binding    |       From         |          To
-------------------|:------------------:|:-------------------------:
<kbd>SPC s e</kbd> | normal or visual   | iedit
<kbd>e</kbd>       | expand-region      | iedit
<kbd>ESC</kbd>     | iedit              | normal
<kbd>C-g</kbd>     | iedit              | normal
<kbd>fd</kbd>      | iedit              | normal
<kbd>ESC</kbd>     | iedit-insert       | iedit
<kbd>C-g</kbd>     | iedit-insert       | normal
<kbd>fd</kbd>      | iedit-insert       | normal

To sum-up, in `iedit-insert state` you have to press <kbd>ESC</kbd> twice to
go back to the `normal state`. You can also at any time press <kbd>C-g</kbd>
or <kbd>fd</kbd> to return to `normal state`.

**Note:** evil commands which switch to `insert state` will switch in
`iedit-insert state`.

##### In iedit state

`iedit state` inherits from `normal state`, the following key bindings are
specific to `iedit state`.

    Key Binding   |                 Description
------------------|------------------------------------------------------------
<kbd>ESC</kbd>    | go back to `normal state`
<kbd>TAB</kbd>    | toggle current occurrence
<kbd>0</kbd>      | go to the beginning of the current occurrence
<kbd>$</kbd>      | go to the end of the current occurrence
<kbd>#</kbd>      | prefix all occurrences with an increasing number (<kbd>SPC u</kbd> to choose the starting number).
<kbd>A</kbd>      | go to the end of the current occurrence and switch to `iedit-insert state`
<kbd>D</kbd>      | delete the occurrences
<kbd>F</kbd>      | restrict the scope to the function
<kbd>gg</kbd>     | go to first occurrence
<kbd>G</kbd>      | go to last occurrence
<kbd>I</kbd>      | go to the beginning of the current occurrence and switch to `iedit-insert state`
<kbd>J</kbd>      | increase the edition scope by one line below
<kbd>K</kbd>      | increase the edition scope by one line above
<kbd>L</kbd>      | restrict the scope to the current line
<kbd>n</kbd>      | go to next occurrence
<kbd>N</kbd>      | go to previous occurrence
<kbd>p</kbd>      | replace occurrences with last yanked (copied) text
<kbd>S</kbd>      | (substitute) delete the occurrences and switch to `iedit-insert state`
<kbd>V</kbd>      | toggle visibility of lines with no occurrence
<kbd>U</kbd>      | Up-case the occurrences
<kbd>C-U</kbd>    | down-case the occurrences

**Note:** <kbd>0</kbd>, <kbd>$</kbd>, <kbd>A</kbd> and <kbd>I</kbd> have the
default Vim behavior when used outside of an occurrence.

##### In iedit-insert state

    Key Binding            |                 Description
---------------------------|------------------------------------------------------------
<kbd>ESC</kbd>             | go back to `iedit state`
<kbd>C-g</kbd>             | go back to `normal state`

#### Examples

- manual selection of several words then replace: <kbd>v w w SPC s e S "toto" ESC ESC</kbd>
- append text to a word on two lines: <kbd>v i w SPC s e J i "toto" ESC ESC</kbd>
- substitute symbol _with expand-region_: <kbd>SPC v v e S "toto" ESC ESC</kbd>
- replace symbol with yanked (copied) text _with expand region_: <kbd>SPC v e p ESC ESC</kbd>

### Commenting

Comments are handled by [evil-nerd-commenter][], it's bound to the following keys.

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC ;</kbd>     | comment operator
<kbd>SPC c i</kbd>   | comment invert
<kbd>SPC c l</kbd>   | comment lines
<kbd>SPC c p</kbd>   | comment paragraphs
<kbd>SPC c r</kbd>   | comment region
<kbd>SPC c t</kbd>   | comment to line
<kbd>SPC c y</kbd>   | comment and yank

**Tips:** To comment efficiently a block of line use the combo:

    <SPC> ; <SPC> l

### Deleting files

Deletion is configured to send deleted files to system trash.

On OS X the `trash` program is required. It can be installed with
[homebrew][] with the following command:

```sh
$ brew install trash
```

To disable the trash you can set the variable `delete-by-moving-to-trash`
to `nil` in your `~/.spacemacs`.

### Editing Lisp code

Edition of lisp code is provided by [evil-lisp-state][].

Every commands below will set the current state to `lisp state` where
the commands can be repeated without pressing on <kbd>SPC m</kbd>.

When in `lisp state` the color of the mode-line changes to pink.

Examples:
- to slurp three times while in normal state: <kbd>SPC m n n n</kbd>
- to wrap a symbol in parenthesis then slurping two times: <kbd>SPC m w n n</kbd>

#### hjkl keys for quickly editing lisp code

Evil Lisp state binds the most common commands on hjkl:

Key Binding         | Function
--------------------|------------------------------------------------------------
<kbd>SPC m h</kbd>  | previous symbol
<kbd>SPC m H</kbd>  | forward barf sexp (move the current symbol or sexp outside)
<kbd>SPC m j</kbd>  | next closing parenthesis
<kbd>SPC m J</kbd>  | wrap symbol with parenthesis (down one level)
<kbd>SPC m k</kbd>  | previous opening parenthesis
<kbd>SPC m K</kbd>  | unwrap current sexp (up one level)
<kbd>SPC m l</kbd>  | next symbol
<kbd>SPC m L</kbd>  | forward slurp sexp (move next outside sexp into current one)

So with just hjkl keys you can:
- navigate between symbols and sexps
- slurp and barf symbols and sexps
- wrap and unwrap symbols and sexps

**Notes:**
Slurping, barfing and wrapping are also bound on other keys, see below.

### Other commands:

Key Binding          | Function
---------------------|------------------------------------------------------------
<kbd>SPC m (</kbd>   | insert expression before (same level as current one)
<kbd>SPC m )</kbd>   | insert expression after (same level as current one)
<kbd>SPC m a</kbd>   | absorb expression
<kbd>SPC m b</kbd>   | forward barf expression
<kbd>SPC m B</kbd>   | backward barf expression
<kbd>SPC m c</kbd>   | convolute expression
<kbd>SPC m i</kbd>   | switch to `insert state`
<kbd>SPC m I</kbd>   | go to beginning of current expression and switch to `insert state`
<kbd>SPC m m</kbd>   | merge (join) expression
<kbd>SPC m n</kbd>   | forwared slurp expression
<kbd>SPC m N</kbd>   | backward slurp expression
<kbd>SPC m p</kbd>   | paste after
<kbd>SPC m P</kbd>   | paste before
<kbd>SPC m q</kbd>   | unwrap current expression and kill all symbols after point
<kbd>SPC m Q</kbd>   | unwrap current expression and kill all symbols before point
<kbd>SPC m r</kbd>   | raise expression (replace parent expression by current one)
<kbd>SPC m T</kbd>   | transpose expression
<kbd>SPC m u</kbd>   | undo
<kbd>SPC m C-r</kbd> | redo
<kbd>SPC m v</kbd>   | switch to `visual state`
<kbd>SPC m V</kbd>   | switch to `visual line state`
<kbd>SPC m C-v</kbd> | switch to `visual block state`
<kbd>SPC m w</kbd>   | wrap expression with parenthesis
<kbd>SPC m W</kbd>   | unwrap expression
<kbd>SPC m xs</kbd>  | delete symbol
<kbd>SPC m xw</kbd>  | delete word
<kbd>SPC m xx</kbd>  | delete expression
<kbd>SPC m y</kbd>   | copy expression

## Project management

Projects in `Spacemacs` are managed with [projectile][projectile]. In
`projectile` projects are defined implicitly, for instance the root of a
project is found when a `.git` repository or `.projectile` file is
encountered in the file tree.

`Helm` is used whenever it is possible.

To search in a project see [project searching](#project-searching).

`projectile` commands start with <kbd>p</kbd>:

    Key Binding     |                 Description
--------------------|------------------------------------------------------------
<kbd>SPC p /</kbd>  | run `ag`
<kbd>SPC p a</kbd>  | run `ag`
<kbd>SPC p A</kbd>  | run `ack`
<kbd>SPC p b</kbd>  | switch to project buffer
<kbd>SPC p d</kbd>  | find directory
<kbd>SPC p D</kbd>  | open project root in `dired`
<kbd>SPC p f</kbd>  | find file
<kbd>SPC p g</kbd>  | run `grep`
<kbd>SPC p h</kbd>  | find file using `helm`
<kbd>SPC p I</kbd>  | invalidate the projectile cache
<kbd>SPC p j</kbd>  | find a tag
<kbd>SPC p k</kbd>  | kill all project buffers
<kbd>SPC p o</kbd>  | run `multi-occur`
<kbd>SPC p R</kbd>  | regenerate the project's [e|g]tags
<kbd>SPC p r</kbd>  | replace a string
<kbd>SPC p s</kbd>  | switch project
<kbd>SPC p t</kbd>  | find tags
<kbd>SPC p T</kbd>  | find test files
<kbd>SPC p v</kbd>  | open project root in `vc-dir` or `magit`

## Registers

Access commands to the various registers start with `r`:

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC r e</kbd>   | show evil yank and named registers
<kbd>SPC r m</kbd>   | show marks register
<kbd>SPC r r</kbd>   | show helm register
<kbd>SPC r y</kbd>   | show kill ring

## Errors handling

`Spacemacs` uses [Flycheck][flycheck] to gives error feedback on the fly.
The checks are only performed at save time by default.

Errors management commands (star with `e`):

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC e c</kbd>   | clear all errors
<kbd>SPC e f</kbd>   | toggle flycheck
<kbd>SPC e l</kbd>   | display the `flycheck` list of errors/warnings
<kbd>SPC e n</kbd>   | go to the next error
<kbd>SPC e p</kbd>   | go to the previous error

Custom fringe bitmaps:

   Symbol                           | Description
:----------------------------------:|------------
![dot-error](img/dot-error.png)     | Error
![dot-warning](img/dot-warning.png) | warning
![dot-info](img/dot-info.png)       | Info

## Compiling

`Spacemacs` binds a few commands to support compiling a project.

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC c c</kbd>   | use `helm-make` via projectile
<kbd>SPC c C</kbd>   | compile
<kbd>SPC c r</kbd>   | recompile

## Modes

### Major Mode leader key

Key bindings specific to the current `major mode` start with <kbd>SPC m</kbd>.
For convenience a shortcut key called the major mode leader key is set by
default on <kbd>,</kbd> which saves one precious keystroke.

It is possible to change the major mode leader key by defining the variable
`dotspacemacs-major-mode-leader-key` in your `~/.spacemacs`. For example to
setup the key on tabulation:

```elisp
(setq-default dotspacemacs-major-mode-leader-key "<tab>")
```

### Helm

`Spacemacs` add `hjkl` navigation to `helm` buffers:

    Key Binding   |                 Description
------------------|------------------------------------------------------------
<kbd>CTRL+h</kbd> | go to previous page
<kbd>CTRL+j</kbd> | go to previous item
<kbd>CTRL+k</kbd> | go to next item
<kbd>CTRL+l</kbd> | go to next page

#### Experimental Helm feature

If `dotspacemacs-feature-toggle-leader-on-jk` is non nil, pressing `jk` while
in `helm` buffer will trigger the evil leader.

When evil leader is triggered the following commands are available:

Key Binding   |                 Description
--------------|----------------------------------------------------------------
<kbd>1</kbd>  | execute action 0
<kbd>2</kbd>  | execute action 1
<kbd>3</kbd>  | execute action 2
<kbd>4</kbd>  | execute action 3
<kbd>5</kbd>  | execute action 4
<kbd>6</kbd>  | execute action 5
<kbd>7</kbd>  | execute action 6
<kbd>8</kbd>  | execute action 7
<kbd>9</kbd>  | execute action 8
<kbd>0</kbd>  | execute action 9
<kbd>a</kbd>  | toggle action selection menu

### Ledger

    Key Binding    |                 Description
-------------------|------------------------------------------------------------
<kbd>SPC m a</kbd> | add a transaction
<kbd>SPC m d</kbd> | delete current transaction

### Org

In `org`, [evil-org-mode][] is activated.

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m a</kbd>    | org-agenda
<kbd>SPC m A</kbd>    | org-archive-subtree
<kbd>SPC m c</kbd>    | org-capture
<kbd>SPC m C</kbd>    | evil-org-recompute-clocks
<kbd>SPC m d</kbd>    | org-deadline
<kbd>SPC m e</kbd>    | org-export-dispatch
<kbd>SPC m i</kbd>    | org-clock-in
<kbd>SPC m l</kbd>    | evil-org-open-links
<kbd>SPC m m</kbd>    | org-ctrl-c-ctrl-c
<kbd>SPC m o</kbd>    | org-clock-out
<kbd>SPC m r</kbd>    | org-refile
<kbd>SPC m s</kbd>    | org-schedule
<kbd>SPC m t</kbd>    | org-show-todo-tree
<kbd>gh</kbd>         | outline-up-heading
<kbd>gj</kbd>         | org-forward-heading-same-level
<kbd>gk</kbd>         | org-backward-heading-same-level
<kbd>gl</kbd>         | outline-next-visible-heading
<kbd>t</kbd>          | org-todo
<kbd>T</kbd>          | org-insert-todo-heading nil
<kbd>H</kbd>          | org-beginning-of-line
<kbd>L</kbd>          | org-end-of-line
<kbd>o</kbd>          | always-insert-item
<kbd>O</kbd>          | org-insert-heading
<kbd>$</kbd>          | org-end-of-line
<kbd>^</kbd>          | org-beginning-of-line
<kbd><</kbd>          | org-metaleft
<kbd>></kbd>          | org-metaright
<kbd>TAB</kbd>        | org-cycle
<kbd>M-l</kbd>        | org-metaright
<kbd>M-h</kbd>        | org-metaleft
<kbd>M-k</kbd>        | org-metaup
<kbd>M-j</kbd>        | org-metadown
<kbd>M-L</kbd>        | org-shiftmetaright
<kbd>M-H</kbd>        | org-shiftmetaleft
<kbd>M-K</kbd>        | org-shiftmetaup
<kbd>M-J</kbd>        | org-shiftmetadown
<kbd>M-o</kbd>        | org-insert-heading+org-metaright
<kbd>M-t</kbd>        | org-insert-todo-heading nil+ org-metaright

### Python

Writing python code with spacemacs is supported by python contribution. Please see
[python contribution][python-contrib] documentation for detail.

### JavaScript

More featured JavaScript support is provided by the javascript contribution. Please see
[javascript contribution][javascript-contrib] documentation for detail.

### rcirc

    Key Binding   |                 Description
------------------|------------------------------------------------------------
<kbd>CTRL+j</kbd> | next item in command history
<kbd>CTRL+k</kbd> | previous item in command history

### HTML and CSS

HTML contribution provides support for editing HTML, CSS, Scss and Less files. Please see
[html contribution][html-contrib] documentation for detail.

# Emacs Server

`Spacemacs` starts a server at launch. This server is killed whenever you close
your Emacs windows.

## Connecting to the Emacs server

TODO

## Keeping the server alive

It is possible to keep the server alive when you close Emacs by setting the
variable `dotspacemacs-persistent-server` to `t` in your `~./spacemacs`.

```elisp
(setq-default dotspacemacs-persistent-server t)
```

When this variable is set to `t`, the only way to quit Emacs _and_ kill the
server is to use the following bindings:

    Key Binding    |                 Description
-------------------|------------------------------------------------------------
<kbd>SPC q q</kbd> | Quit Emacs and kill the server
<kbd>SPC q s</kbd> | Save the buffers, quit Emacs and kill the server

## Troubleshoot

### Loading fails

If during the first boot of Emacs nothing seems to happen or if the
installation seems to abort prematurely, you can check for an error message
by opening the `*Warning*` buffer:

    C-x b warning RET

_('C-x b' means 'Ctrl + x then b' and 'RET' means 'return')_

Then you can copy/paste the error in a [Github issue][issues], thank you.

### I have no file ~/.spacemacs

You have to manually copy the `~/.emacs.d/core/templates/.spacemacs.template`
file to `~/.spacemacs`

# Tips

## evil-lisp-state as default state

To Make `lisp state` the default state in `Emacs Lisp` buffers, insert in
your `~/.spacemacs` the following snippet:

```elisp
(defun dotspacemacs/config ()
  (add-hook 'emacs-lisp-mode-hook 'evil-lisp-state))
```

## "jk" to trigger evil leader

It is possible to activate an experimental feature which allows to trigger the
evil leader in `insert state`, in `ido` minibuffer and in `helm` buffers.

To activate it, set `dotspacemacs-feature-toggle-leader-on-jk` to `t`.

```elisp
(setq-default dotspacemacs-feature-toggle-leader-on-jk t)
```

More info on this feature:
- [insert state](#experimental-insert-state-feature)
- [helm](#experimental-helm-feature)
- [ido](#experimental-ido-feature)

# Achievements

Achievements                                         | Account
-----------------------------------------------------|------------------------
[First contribution][1st-contrib]                    | [trishume][]
[First contribution layer][1st-clayer]               | [trishume][]
[First blog article on Spacemacs][1st-article]       | [Wolfy87][]
[First contributed banner][1st-cbanner]              | [chrisbarrett][]
[100th issue (PR)][100th-issue]                      | [danielwuz][]
[200th issue (question)][200th-issue]                | [justrajdeep][]
[300th issue (PR)][300th-issue]                      | [danielwuz][]
[400th issue (PR)][400th-issue]                      | [CestDiego][]
[100th pull request][100th-PR]                       | [bru][]
[200th pull request][200th-PR]                       | [smt][]
PR gunner (8 PRs in a row)                           | [ralesi][]
100th star                                           | [Jackneill][]
200th star                                           | [jb55][]
400th star                                           | [dbohdan][]

# Thank you

[Jokes aside](#using-configuration-layers), thank you Richard for this great
piece of software.

Thank you to all the contributors and the whole Emacs community from core
developers to elisp hackers!

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
[ag]: https://github.com/ggreer/the_silver_searcher
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
[iedit]: https://github.com/tsdh/iedit
[evil-iedit-state]: https://github.com/syl20bnr/evil-iedit-state
[evil-indent-textobject]: https://github.com/cofi/evil-indent-textobject
[evil-visualstar]: https://github.com/bling/evil-visualstar
[evil-exchange]: https://github.com/Dewdrops/evil-exchange
[evil-surround]: https://github.com/timcharper/evil-surround
[camelcasemotion.vim]: http://www.vim.org/scripts/script.php?script_id=1905
[vim-exchange]: https://github.com/tommcdo/vim-exchange
[vim-surround]: https://github.com/tpope/vim-surround
[evil-nerd-commenter]: https://github.com/redguardtoo/evil-nerd-commenter
[nerdcommenter]: https://github.com/scrooloose/nerdcommenter
[evil-matchit]: https://github.com/redguardtoo/evil-matchit
[matchit.vim]: http://www.vim.org/scripts/script.php?script_id=39
[source code pro]: https://github.com/adobe-fonts/source-code-pro
[evil-escape]: https://github.com/syl20bnr/evil-escape
[evil-args]: https://github.com/wcsmith/evil-args
[evil-jumper]: https://github.com/bling/evil-jumper
[evil-numbers]: https://github.com/cofi/evil-numbers
[evil-org-mode]: https://github.com/edwtjo/evil-org-mode
[evil-lisp-state]: https://github.com/syl20bnr/evil-lisp-state
[git-gutter]: https://github.com/syohex/emacs-git-gutter-fringe
[nose]: https://github.com/nose-devs/nose/
[nose.el]: https://github.com/syl20bnr/nose.el
[pylookup]: https://github.com/tsgates/pylookup
[jedi]: https://github.com/tkf/emacs-jedi
[ess-R-object-popup]: https://github.com/myuhe/ess-R-object-popup.el
[ess-R-data-view]: https://github.com/myuhe/ess-R-data-view.el
[leuven-theme]: https://github.com/fniessen/emacs-leuven-theme
[monokai-theme]: https://github.com/oneKelvinSmith/monokai-emacs
[zenburn-theme]: https://github.com/bbatsov/zenburn-emacs
[ido-vertical-mode]: https://github.com/gempesaw/ido-vertical-mode.el
[issues]: https://github.com/syl20bnr/spacemacs/issues
[vundle]: https://github.com/gmarik/Vundle.vim
[anzu]: https://github.com/syohex/emacs-anzu
[javascript-contrib]: https://github.com/syl20bnr/spacemacs/tree/master/contrib/lang/javascript
[themes-megapack]: https://github.com/syl20bnr/spacemacs/tree/master/contrib/themes-megapack
[python-contrib]: https://github.com/syl20bnr/spacemacs/tree/master/contrib/lang/python
[html-contrib]: https://github.com/syl20bnr/spacemacs/tree/master/contrib/lang/html
[guide-key]: https://github.com/kai2nenobu/guide-key
[guide-key-tip]: https://github.com/aki2o/guide-key-tip
[gitter]: https://gitter.im/syl20bnr/spacemacs
[CONTRIBUTE.md]: https://github.com/syl20bnr/spacemacs/blob/master/doc/CONTRIBUTE.md
[neotree]: https://github.com/jaypei/emacs-neotree
[nerdtree]: https://github.com/scrooloose/nerdtree
[anaconda-mode]: https://github.com/proofit404/anaconda-mode
[1st-contrib]: https://github.com/syl20bnr/spacemacs/pull/19
[1st-clayer]: https://github.com/syl20bnr/spacemacs/commit/e802027d75d0c0aed55539b0da2dfa0df94dfd39
[1st-article]: http://oli.me.uk/2014/11/06/spacemacs-emacs-vim/
[1st-cbanner]: https://github.com/syl20bnr/spacemacs/commit/7b44a56263049482ed540ed6815a295633ffe9d1
[100th-issue]: https://github.com/syl20bnr/spacemacs/pull/100
[200th-issue]: https://github.com/syl20bnr/spacemacs/pull/200
[300th-issue]: https://github.com/syl20bnr/spacemacs/pull/300
[400th-issue]: https://github.com/syl20bnr/spacemacs/pull/400
[100th-PR]: https://github.com/syl20bnr/spacemacs/pull/228
[200th-PR]: https://github.com/syl20bnr/spacemacs/pull/418
[trishume]:https://github.com/trishume
[Wolfy87]:https://github.com/Wolfy87
[danielwuz]:https://github.com/danielwuz
[CestDiego]:https://github.com/CestDiego
[chrisbarrett]:https://github.com/chrisbarrett
[justrajdeep]:https://github.com/justrajdeep
[dbohdan]:https://github.com/dbohdan
[bru]:https://github.com/bru
[smt]:https://github.com/smt
[ralesi]:https://github.com/ralesi
[Jackneill]:https://github.com/Jackneill
[jb55]:https://github.com/jb55
[use-package]: https://github.com/jwiegley/use-package
[Paradox]: https://github.com/Bruce-Connor/paradox
[fancy-battery]: https://github.com/lunaryorn/fancy-battery.el
[MacType]: https://code.google.com/p/mactype/
