# Spacemacs Documentation

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Spacemacs Documentation](#spacemacs-documentation)
- [Core Pillars](#core-pillars)
    - [Mnemonic](#mnemonic)
    - [Discoverability](#discoverability)
    - [Consistency](#consistency)
    - [Crowd-Configured](#crowd-configured)
- [Goals](#goals)
- [Screenshots](#screenshots)
- [Who can benefit from this ?](#who-can-benefit-from-this-)
- [Update and Rollback](#update-and-rollback)
    - [Update Spacemacs repository](#update-spacemacs-repository)
    - [Update packages](#update-packages)
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
    - [Synchronization of dotfile changes](#synchronization-of-dotfile-changes)
    - [Content](#content)
        - [Using configuration layers](#using-configuration-layers)
        - [Setting configuration layers variables](#setting-configuration-layers-variables)
        - [Excluding packages](#excluding-packages)
        - [Hooks](#hooks)
        - [Custom variables](#custom-variables)
- [Main principles](#main-principles)
    - [Evil](#evil)
        - [States](#states)
    - [Evil leader](#evil-leader)
    - [Universal argument](#universal-argument)
    - [Micro-states](#micro-states)
- [Differences between Vim, Evil and Spacemacs](#differences-between-vim-evil-and-spacemacs)
    - [The vim-surround case](#the-vim-surround-case)
- [Evil plugins](#evil-plugins)
- [Spacemacs UI](#spacemacs-ui)
    - [Graphical UI](#graphical-ui)
        - [Color themes](#color-themes)
        - [Font](#font)
        - [Graphical UI Toggles](#graphical-ui-toggles)
        - [Mouse usage](#mouse-usage)
        - [Mode-line](#mode-line)
            - [Flycheck integration](#flycheck-integration)
            - [Anzu integration](#anzu-integration)
            - [Battery status integration](#battery-status-integration)
            - [Powerline separators](#powerline-separators)
            - [Minor Modes](#minor-modes)
- [Commands](#commands)
    - [Vim key bindings](#vim-key-bindings)
        - [Escaping](#escaping)
        - [Executing Vim and Emacs ex/M-x commands](#executing-vim-and-emacs-exm-x-commands)
        - [Leader key](#leader-key)
    - [Reserved prefix command for user](#reserved-prefix-command-for-user)
    - [Helm](#helm)
        - [Helm micro-state](#helm-micro-state)
    - [Discovering](#discovering)
        - [Key bindings](#key-bindings)
            - [Guide-key](#guide-key)
            - [Helm describe key bindings](#helm-describe-key-bindings)
        - [Getting help](#getting-help)
        - [Available layers](#available-layers)
            - [Available packages in Spacemacs](#available-packages-in-spacemacs)
            - [New packages from ELPA repositories](#new-packages-from-elpa-repositories)
        - [Toggles](#toggles)
    - [Navigating](#navigating)
        - [Point/Cursor](#pointcursor)
            - [Smooth scrolling](#smooth-scrolling)
        - [Vim motions with ace-jump mode](#vim-motions-with-ace-jump-mode)
            - [ace-link mode](#ace-link-mode)
        - [Window manipulation](#window-manipulation)
            - [Window manipulation key bindings](#window-manipulation-key-bindings)
            - [Window manipulation micro-state](#window-manipulation-micro-state)
            - [Golden ratio](#golden-ratio)
        - [Buffers and Files](#buffers-and-files)
            - [Buffers manipulation key bindings](#buffers-manipulation-key-bindings)
            - [Buffers manipulation manipulation micro-state](#buffers-manipulation-manipulation-micro-state)
            - [Files manipulations key bindings](#files-manipulations-key-bindings)
            - [Emacs and Spacemacs files](#emacs-and-spacemacs-files)
        - [Ido](#ido)
        - [Ido micro-state](#ido-micro-state)
        - [NeoTree file tree](#neotree-file-tree)
            - [NeoTree navigation](#neotree-navigation)
            - [Opening files with NeoTree](#opening-files-with-neotree)
            - [Other NeoTree key bindings](#other-neotree-key-bindings)
            - [NeoTree mode-line](#neotree-mode-line)
        - [Shells](#shells)
            - [Key bindings](#key-bindings)
            - [Staying in insert state](#staying-in-insert-state)
        - [Bookmarks](#bookmarks)
        - [DocView mode](#docview-mode)
    - [Searching](#searching)
        - [With an external tool](#with-an-external-tool)
            - [Searching in an arbitrary directory](#searching-in-an-arbitrary-directory)
            - [Searching in a project](#searching-in-a-project)
            - [Searching the web](#searching-the-web)
        - [Persistent highlighting](#persistent-highlighting)
        - [Stacking highlights](#stacking-highlights)
        - [Highlight current symbol](#highlight-current-symbol)
        - [Visual Star](#visual-star)
        - [Listing symbols by semantic](#listing-symbols-by-semantic)
        - [Helm-swoop](#helm-swoop)
    - [Editing](#editing)
        - [Paste text](#paste-text)
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
        - [Replacing text with iedit](#replacing-text-with-iedit)
            - [iedit states key bindings](#iedit-states-key-bindings)
                - [State transitions](#state-transitions)
                - [In iedit state](#in-iedit-state)
                - [In iedit-insert state](#in-iedit-insert-state)
            - [Examples](#examples)
        - [Replacing text in several files](#replacing-text-in-several-files)
        - [Commenting](#commenting)
        - [Deleting files](#deleting-files)
        - [Editing Lisp code](#editing-lisp-code)
            - [Lisp Key Bindings](#lisp-key-bindings)
                - [Lisp state key bindings](#lisp-state-key-bindings)
                - [Emacs lisp specific key bindings](#emacs-lisp-specific-key-bindings)
    - [Managing projects](#managing-projects)
    - [Registers](#registers)
    - [Errors handling](#errors-handling)
    - [Compiling](#compiling)
    - [Modes](#modes)
        - [Major Mode leader key](#major-mode-leader-key)
        - [Helm](#helm)
        - [Python](#python)
        - [JavaScript](#javascript)
        - [HTML and CSS](#html-and-css)
- [Emacs Server](#emacs-server)
    - [Connecting to the Emacs server](#connecting-to-the-emacs-server)
    - [Keeping the server alive](#keeping-the-server-alive)
    - [Troubleshoot](#troubleshoot)
        - [Loading fails](#loading-fails)
        - [I have no file ~/.spacemacs](#i-have-no-file-spacemacs)
- [Tips](#tips)
    - [evil-lisp-state as default state](#evil-lisp-state-as-default-state)
- [Achievements](#achievements)
    - [Issues](#issues)
    - [Merged Pull Requests](#merged-pull-requests)
    - [Stars and forks](#stars-and-forks)
    - [Specials](#specials)
- [Thank you](#thank-you)

<!-- markdown-toc end -->

# Core Pillars

Four core pillars: Mnemonic, Discoverability, Consistency, "Crowd-Configured".

If any of these core pillars is violated open an issue and we'll fix it.

## Mnemonic

`Spacemacs` organizes key bindings by mnemonic namespaces as much as possible.
If you are looking for commands to operate on your buffer, they are right under
<kbd>SPC b</kbd>, if you want to operate on your project, then it is
<kbd>SPC p</kbd>, etc...

## Discoverability

`Spacemacs` comes with a dedicated major mode `spacemacs-mode`. Its goal is to
give useful feedbacks and easily perform maintenance tasks.

It also comes with dedicated [helm][] sources to quickly find layers, packages
and more.

[guide-key][] is enabled by default, it will display all the available key
bindings in a dedicated popup buffer.

## Consistency

Similar functionalities should have the same key binding no matter which major
is currently active. For instance if you are looking for the definition of a
function, the binding is <kbd>SPC m g g</kbd>, `m` for `major mode` and `g g`
for `go to thing at point`. No matter what is the major mode it is the same
binding to perform this action.

This is also true for the documentation, each configuration layer comes with
an associated `README.md` file with the same base layout.

The consistency core pillar is supported by a convention file:
[CONVENTIONS.md][]

## Crowd-Configured

By defining an very light structure called `configuration layer` which is easy
to understand, `Spacemacs` makes it easy to contribute additional support.

The conventions in [CONVENTIONS.md][] make it easy to get the spacemacs way
and keep consistency even if there are a lot of contributions.

`Crowd-configuration` is the most powerful pillar of `Spacemacs`. Anybody can
submit upstream improvements to configuration layers or a whole new one. Any
user can easily and directly use this layer by adding it to a list in a
dotfile. It is even possible to exclude _any_ unwanted packages.

# Goals

- **Bring the power of modal editing** to the powerful Emacs editing platform.

- Integrate nicely with `Evil` states (`Vim` modes): `Spacemacs` tries to
**keep your fingers on the home row** as much as possible, no matter the mode
you are in.

- **Crowd-configured**: Contribute easily your improvements and new
configuration layers.

- **Minimalistic and nice graphical UI**, keep your available screen space for
what matters: your text files.

- **Mnemonic and consistent key bindings** which should be easier to learn
and remember and be the same in all major modes.

- **Fast boot time**, everything is lazy-loaded.

- **Lower the risk of RSI** by heavily using the space bar instead of modifiers.

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
induced by the default Emacs key bindings (this is an assumption, there is no
official studies to prove this).

Emacs users wanting to learn **a different way to edit files** or wanting to
learn Vim key bindings.

As a side note, if you are a programmer and you don't know Vim key bindings
yet, I deeply recommend you to learn the basics as recommended in
[Sacha Chua's one-page guide][sacha_guide] about how to learn Emacs.

# Update and Rollback

For now it is still needed to update the `Spacemacs` repository manually.

## Update Spacemacs repository

Close Emacs and update the git repository:

   ```sh
   git pull --rebase
   git submodule sync; git submodule update
   ```

**Note** It is recommended to update the packages first, see next session.

## Update packages

To update `Spacemacs` press <kbd>RET</kbd> (enter) or click on the link
`[Update]` in the startup page under the banner then restart Emacs.

If anything goes wrong you should be able to rollback the update by pressing
<kbd>RET</kbd> or clicking on the `[Rollback]` link next to the `[Update]`
link and choose a rollback slot (sorted by date).

# Configuration layers

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

It effectively makes `Spacemacs` behave like [Vundle][vundle].

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
(setq-default dotspacemacs-configuration-layers '(themes-megapack))
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

## Synchronization of dotfile changes

To apply the modifications made in `~/.spacemacs` press <kbd>SPC m c c</kbd>.
It will re-execute the `Spacemacs` initialization process.

**Note:** A synchronization re-execute the functions `dotspacemacs/init` and
`dotspacemacs/config`. Depending on the content of this functions you may
encounter some unwanted side effects. For instance if you use a toggle in
`dotspacemac/config` to enable some behavior, this behavior will be turned off
whenever the dotfile is re-synchronize. To avoid these side-effects it is
recommended to use `setq` expressions instead of toggle functions.
It is also possible to _skip_ the execution of `dotspacemacs/config` with the
universal argument (<kbd>SPC u SPC m c c</kbd>).

## Content

### Using configuration layers

To use a configuration layer, add it to the `dotspacemacs-configuration-layers`
variable of your `~/.spacemacs`.

For instance to add the configuration layer of [RMS](#thank-you):
```elisp
(setq-default dotspacemacs-configuration-layers '(rms))
```
If this layer does not exist you can still try another one in
[the `contrib` directory](../contrib).

Configuration layers are expected to be stored in `~/.emacs.d/private` or
`~/.emacs.d/contrib`. But you are free to keep them somewhere else by declaring
additional paths where `Spacemacs` can look for configuration layers.
This is done by setting the list
`dotspacemacs-configuration-layer-path` in your `~/.spacemacs`:

```elisp
(setq-default dotspacemacs-configuration-layer-path '("~/.myconfig/"))
```

### Setting configuration layers variables

Some configuration layers have configuration variables to enable specific
support. For instance the [git layer][] has several configuration variables,
they can be set directly in the `dotspacemacs-configuration-layers` like this:

```elisp
;; List of configuration layers to load.
dotspacemacs-configuration-layers '(company-mode
                                    (git :variables
                                         git-magit-status-fullscreen t
                                         git-enable-github-support t
                                         git-gutter-use-fringe t)
                                    smex)
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

# Main principles

## Evil

`Spacemacs` uses the [evil][evil] mode to emulate Vim key bindings. It is a
very complete emulation, maybe the most advanced. In fact, Evil is much more
than just a Vim emulation. It has more states than Vim for instance.

### States

`Spacemacs` has 9 states:

State        | Color       | Description
-------------|-------------|--------------------------------------------------------
normal       | orange      | like the `normal mode of Vim`, used to execute and combine commands
insert       | green       | like the `insert mode of Vim`, used to actually insert text
visual       | gray        | like the `visual mode of Vim`, used to make text selection
motion       | purple      | exclusive to `Evil`, used to navigate read only buffers
emacs        | blue        | exclusive to `Evil`, using this state is like using a regular Emacs without Vim
evilified    | light brown | exclusive to `Spacemacs`, this is an `emacs state` modified to bring Vim navigation, selection and search.
lisp         | pink        | exclusive to `Spacemacs`, used to navigate Lisp code and modify it (more [info](#editing-lisp-code))
iedit        | red         | exclusive to `Spacemacs`, used to navigate between multiple regions of text using `iedit` (more [info](#replacing-text-with-iedit))
iedit-insert | red         | exclusive to `Spacemacs`, used to replace multiple regions of text using `iedit` (more [info](#replacing-text-with-iedit))

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
where it makes sense. This prevents one from doing repetitive and tedious presses on the
<kbd>SPC</kbd> key.

When a `micro-state` is active, a documentation is displayed in the minibuffer.
Additional information may as well be displayed in the minibuffer.

[Auto-highlight-symbol micro-state](#auto-highlight-and-edition-of-symbols):
![spacemacs_ahs_micro_state](img/spacemacs-ahs-micro-state.png)

[Text scale micro-state](#change-font-size):
![spacemacs_scale_micro_state](img/spacemacs-scale-micro-state.png)

# Differences between Vim, Evil and Spacemacs

- The `,` key does "repeat last `f`, `t`, `F`, or `T` command in opposite
direction in `Vim`, but in `Spacemacs` it the major mode specific leader
key by default (which can be set on another key binding in the dotfile).

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
- `surround` command is just a more powerful command than `s`

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

# Spacemacs UI

 `Spacemacs` has unique UI elements to make the Emacs experience even
 more enjoyable:
 - dedicated startup page with a mode aimed at easily managing `Spacemacs`
 - dedicated helm source via `helm-spacemacs`
 - a [guide-key][] buffer

## Graphical UI

`Spacemacs` has a minimalistic and distraction free graphical UI:
- custom [powerline][powerline] mode-line
[with color feedback](#flycheck-integration) according to current
 [Flycheck][flycheck] status
 - unicode symbols for minor mode lighters which appear in the mode-line
 - [custom fringe bitmaps](#errors-handling) and error feedbacks for
 [Flycheck][flycheck]
 - [custom fringe bitmaps](../contrib/git/README.md#git-gutter-bitmaps) for
 git gutter (available in [git layer][])

### Color themes

By default, `Spacemacs` uses the theme [solarized-light][solarized-theme].

It is possible to define your default themes in your `~/.spacemacs` with
the variable `dotspacemacs-themes`. For instance, to specify `leuven` and
`zenburn` (high contrast theme and low contrast theme):

```elisp
(setq-default dotspacemacs-themes '(leuven zenburn))
```

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC T n</kbd>   | switch to next theme listed in `dotspacemacs-themes`.
<kbd>SPC T h</kbd>   | select a theme using a `helm` buffer.

**Note:** Due to the inner working of themes in Emacs, switching theme during
the same session may have some weird side effects. Although these side effects
should be pretty rare.

**Hint** If you are an `Org` user, [leuven-theme][] is amazing ;-)

### Font

The default font used by `Spacemacs` is [source code pro][] by Adobe. It is
recommended to install it on your system.

To change the default font set the variable `dotspacemacs-default-font` in
your `.spacemacs` file.

By default its value is:

```elisp
(setq-default dotspacemacs-default-font '("Source Code Pro"
                                          :size 13
                                          :weight normal
                                          :width normal
                                          :powerline-scale 1.1))
```

The properties should be pretty straightforward, it is possible to set any
valid property of a [font-spec][]:
- `:family` Font family or fontset (a string).
- `:width` Relative character width. This should be one of the symbols:
  - ultra-condensed
  - extra-condensed
  - condensed
  - semi-condensed
  - normal
  - semi-expanded
  - expanded
  - extra-expanded
  - ultra-expanded
- `:height` The height of the font. In the simplest case, this is an integer in
units of 1/10 point.
- `:weight` Font weight—one of the symbols (from densest to faintest):
  - ultra-bold
  - extra-bold
  - bold
  - semi-bold
  - normal
  - semi-light
  - light
  - extra-light
  - ultra-light
- `:slant` Font slant—one of the symbols:
  - italic
  - oblique
  - normal
  - reverse-italic
  - reverse-oblique
- `:size` The font size—either a non-negative integer that specifies the pixel
size, or a floating-point number that specifies the point size.
- `:adstyle` Additional typographic style information for the font, such as
‘sans’. The value should be a string or a symbol.
- `:registry` The charset registry and encoding of the font, such as
‘iso8859-1’. The value should be a string or a symbol.
- `:script` The script that the font must support (a symbol).

The special property `:powerline-scale` is `Spacemacs` specific and it is for
quick tweaking of the mode-line height in order to avoid crappy rendering of
the separators like on the following screenshot (default value is 1.1).

_Ugly separators_

![ugly-separators](img/crappy-powerline-separators.png)

### Graphical UI Toggles

Some graphical UI indicators can be toggled on and off
(toggles start with `t` and `T`):

    Key Binding         |                 Description
------------------------|------------------------------------------------------------
<kbd>SPC t ~</kbd>      | display `~` in the fringe on empty lines
<kbd>SPC t c</kbd>      | display the fill column (by default the fill column is set to 80)
<kbd>SPC t h h</kbd>    | toggle highlight of the current line
<kbd>SPC t h i</kbd>    | toggle highlight indentation levels
<kbd>SPC t h c</kbd>    | toggle highlight indentation current column
<kbd>SPC t i</kbd>      | toggle indentation guide at point
<kbd>SPC t l</kbd>      | toggle truncate lines
<kbd>SPC t L</kbd>      | toggle visual lines
<kbd>SPC t n</kbd>      | show the absolute line numbers

    Key Binding         |                 Description
------------------------|------------------------------------------------------------
<kbd>SPC T F</kbd>      | toggle frame fullscreen
<kbd>SPC T f</kbd>      | toggle display of the fringe
<kbd>SPC T m</kbd>      | toggle menu bar
<kbd>SPC T M</kbd>      | toggle frame maximize
<kbd>SPC T t</kbd>      | toggle tool bar
<kbd>SPC T T</kbd>      | toggle frame transparency

**Note** These toggles are all available via the `helm-spacemacs` interface
(press <kbd>SPC fe h</kbd> to display the `helm-spacemacs` buffer).

### Mouse usage

There are some added mouse features set for the line number margin (if shown):

- single click in line number margin visually selects the entire line
- drag across line number margin visually selects the region
- double click in line number margin visually select the current code block

### Mode-line

The mode line is a heavily customized [powerline][powerline] with the
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
<kbd>SPC t m b</kbd>   | toggle the battery status
<kbd>SPC t m c</kbd>   | toggle the `org` task clock (available in `org` layer)
<kbd>SPC t m f</kbd>   | toggle the flycheck info (available in `syntax-checking` layer)
<kbd>SPC t m m</kbd>   | toggle the minor mode lighters
<kbd>SPC t m n</kbd>   | toggle the cat! (if `colors` layer is declared in your dotfile)
<kbd>SPC t m p</kbd>   | toggle the point character position
<kbd>SPC t m v</kbd>   | toggle the new version lighter

#### Flycheck integration

When [Flycheck][flycheck] minor mode is enabled, a new element appears showing
the number of errors, warnings and info.

![powerline-wave](img/powerline-wave.png)

#### Anzu integration

[Anzu][anzu] shows the number of occurrence when performing a search. `Spacemacs`
integrates nicely the Anzu status by displaying it temporarily when `n` or `N` are
being pressed. See the `5/6` segment on the screenshot below.

![powerline-anzu](img/powerline-anzu.png)

#### Battery status integration

[fancy-battery][] displays the percentage of total charge of the battery as
well as the time remaining to charge or discharge completely the battery.

A color code is used for the battery status:

 Battery State    |       Color
------------------|------------------
Charging          | Green
Discharging       | Orange
Critical          | Red

Note the these colors may vary depending on your theme.

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

#### Minor Modes

`Spacemacs` uses [diminish][diminish] mode to reduce the size of minor mode
indicators:

The minor mode area can be toggled on and off with:

    <SPC> t m m

Unicode symbols are displayed by default. Setting the variable
`dotspacemacs-mode-line-unicode-symbols` to `nil` in your `~/.spacemacs` will
display ASCII characters instead (may be useful in terminal if you
cannot set an appropriate font).

The letters displayed in the mode-line correspond to the key bindings used
to toggle them.

Some toggle have two flavors: local and glocal. The global version of the
toggle can be reached using the `control` key.

Key Binding          |   Unicode   |   ASCII    |                    Mode
---------------------|:-----------:|:----------:|----------------------------------------------------
<kbd>SPC t -</kbd>   | `⊝`        | -          | [centered-cursor][] mode
<kbd>SPC t C--</kbd> |            |            | global
<kbd>SPC t a</kbd>   | `ⓐ`        | a          | auto-completion
<kbd>SPC t c</kbd>   | `ⓒ`        | c          | fill-column-indicator mode
`none`               | `ⓔ`        | e          | [evil-org][evil-org-mode] mode
<kbd>SPC t f</kbd>   | `ⓕ`        | f          | flycheck mode
<kbd>SPC t F</kbd>   | `Ⓕ`        | F          | auto-fill mode
<kbd>SPC t g</kbd>   | `ⓖ`        | g          | [golden-ratio][] mode
<kbd>SPC t k</kbd>   | `Ⓖ`        | G          | guide-key mode
<kbd>SPC t i</kbd>   | `ⓘ`        | i          | indentation guide
<kbd>SPC t C-i</kbd> |             |            | global
<kbd>SPC t I</kbd>   | `Ⓘ`        | I          | aggressive indent mode
<kbd>SPC t p</kbd>   | `ⓟ`        | p          | [smartparens][sp] mode
<kbd>SPC t C-p</kbd> |             |            | global
<kbd>SPC t s</kbd>   | `ⓢ`        | s          | flyspell mode
<kbd>SPC t w</kbd>   | `ⓦ`        | w          | whitespace mode
<kbd>SPC t C-w</kbd> |             |            | global
<kbd>SPC t y</kbd>   | `ⓨ`        | y          | [yasnippet][yasnippet] mode

# Commands

## Vim key bindings

`Spacemacs` is based on `Vim` modal user interface to navigate and edit text.
If you are not familiar with the `Vim` way of editing text you can try the
 [evil tutor][] lessons by pressing <kbd>SPC h T</kbd> at any time.

### Escaping

`Spacemacs` uses [evil-escape][] to easily switch between `insert state` and
`normal state` by quickly pressing the `fd` keys.

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

This sequence can be customized in your `~/.spacemacs`. Example to set it
to `jj` (it is important set the variable in `dotspacemacs/init`):

```elisp
(defun dotspacemacs/init ()
  (setq-default evil-escape-key-sequence "jj"))
```

**Note:** Although `jj` or `jk` are popular choices of vim users, these key
sequences are not optimal for `Spacemacs`. Indeed it is very easy in
`visual state` to press quickly `jj` and inadvertently escape to `normal state`.

**Important Note** One caveat of `evil-escape` is that you must not use it
while recording macros. Use `escape` key instead.

### Executing Vim and Emacs ex/M-x commands

    Command      |                 Key Binding
:---------------:|------------------------------------------------------------------
Vim (ex-command) | <kbd>`:`</kbd>
Emacs (M-x)      | <kbd>SPC :</kbd>

The command key `:` can be easily changed with the variable
`dotspacemacs-command-key` of your `~/.spacemacs`. Note that is will change both
`:` and `SPC :` bindings to keep the symmetry between Vim and Emacs. A good
key can be `,` for example.

### Leader key

On top of `Vim` modes (modes are called states in `Spacemacs`) there is a
special key called the leader key which once pressed gives a whole new
keyboard layer. The leader key is by default <kbd>SPC</kbd> (space).
It is possible to change this key with the variable `dotspacemacs-leader-key`.

## Reserved prefix command for user

<kbd>SPC o</kbd> is reserved for the user. Setting key bindings behind
`<SPC> o` is **guaranteed** to never conflict with `Spacemacs` defaults key
bindings.

**Example:**
Put `(evil-leader/set-key "oc" 'org-capture)` inside `dotspacemacs/config`
in your `~/.spacemacs` file, to be able to use <kbd>SPC o c</kbd> to run
org mode capture.

## Helm

`Spacemacs` is powered by [Helm][helm-link] which is an incremental completion
and selection narrowing framework.

`Helm` is the central control tower of `Spacemacs`, it is used to manage
buffers, projects, search results, configuration layers, toggles and more...

Mastering `Helm` will make you a `Spacemacs` power user. Do not hesitate
to read the [Helm documentation wiki][helm-doc].

### Helm micro-state

`Spacemacs` defines a [micro-state](#micro-states) for `Helm` to make it
work like [Vim's Unite][] plugin.

Initiate the micro-state with <kbd>C-SPC</kbd> while in a `Helm` buffer.
Use <kbd>C-SPC</kbd> again to exit from the micro-state.

Key Binding           | Description
----------------------|------------------------------------------------------------
<kbd>C-SPC</kbd>      | initiate or leave the micro-state
<kbd>TAB</kbd>        | switch to actions page and leave the micro-state
<kbd>1</kbd>          | execute action 0
<kbd>2</kbd>          | execute action 1
<kbd>3</kbd>          | execute action 2
<kbd>4</kbd>          | execute action 3
<kbd>5</kbd>          | execute action 4
<kbd>6</kbd>          | execute action 5
<kbd>7</kbd>          | execute action 6
<kbd>8</kbd>          | execute action 7
<kbd>9</kbd>          | execute action 8
<kbd>0</kbd>          | execute action 9
<kbd>a</kbd>          | switch to actions page
<kbd>g</kbd>          | go to first candidate
<kbd>G</kbd>          | go to last candidate
<kbd>h</kbd>          | go to previous source
<kbd>j</kbd>          | select next candidate
<kbd>k</kbd>          | select previous candidate
<kbd>l</kbd>          | go to next source
<kbd>t</kbd>          | mark current candidate
<kbd>T</kbd>          | mark all candidates
<kbd>v</kbd>          | execute persistent action

## Discovering

### Key bindings

#### Guide-key

An help buffer is displayed each time the <kbd>SPC</kbd> key is pressed in
normal mode. It lists the available key bindings and their associated
commands.

By default the [guide-key][] buffer will be displayed quickly after the key
has been pressed. You can change the delay by setting the variable
`dotspacemacs-guide-key-delay` to your liking (the value is in second).

#### Helm describe key bindings

It is possible to search for specific key bindings by pressing
<kbd>SPC ?</kbd>.

To narrow the list to some key bindings using the leader key type a pattern
like this regular expression:

    SPC\ b

which would list all `buffer` related bindings.

### Getting help

`Describe functions` are powerful Emacs introspection commands to get
information about functions, variables, modes etc. These commands are bound
thusly:

Key Binding          |                 Description
---------------------|------------------------------------------------------------------
<kbd>SPC h d c</kbd> | describe current character under point
<kbd>SPC h d f</kbd> | describe a function
<kbd>SPC h d k</kbd> | describe a key
<kbd>SPC h d m</kbd> | describe current modes
<kbd>SPC h d p</kbd> | describe a package
<kbd>SPC h d t</kbd> | describe a theme
<kbd>SPC h d v</kbd> | describe a variable

Other help key bindings

Key Binding          |                 Description
---------------------|------------------------------------------------------------------
<kbd>SPC h i</kbd>   | search in info pages with the symbol at point
<kbd>SPC h m</kbd>   | search available man pages

### Available layers

All layers can be easily discovered via `helm-spacemacs` accessible with
<kbd>SPC f e h</kbd>.

The following helm actions are available:
- default: open the layer `README.md`
- 2nd: open the layer `packages.el`
- 3nd: open the layer `extensions.el`

#### Available packages in Spacemacs

`helm-spacemacs` also lists all the packages available in `Spacemacs`.
The entry format is `(layer) packages`. If you type `flycheck` you'll
be able to see all the layers where `flycheck` is used.

The following helm actions are available on packages:
- default: go the package init function

#### New packages from ELPA repositories

`package-list-packages` is where you can browse for all available packages
in the different Elpa repositories. It is possible to upgrade packages
from there but it is not recommended, use the `[Update]` link on the
`Spacemacs` startup page instead.

`Spacemacs` proposes to use [Paradox][] instead of `package-list-packages`
to list available ELPA packages.
Paradox enhances the package list buffer with better feedbacks, new
filters and Github information like the number of stars. Optionally you
can also star packages directly in the buffer.

**Important Note 1** Installing a new package from `Paradox` won't make it
persistent. To install a package persistently you have to add it explicitly
to a configuration layer.

**Important Note 2** Don't _update_ your packages from `Paradox` or
`package-list-packages` because they don't support the rollback feature of
Spacemacs.

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

### Toggles

`helm-spacemacs` is also a central place to discover the available toggles.
To display only the toggles source press <kbd>C-l</kbd> (or in
[Helm micro-state](#helm-micro-state) you can press just <kbd>l</kbd>).

The following helm actions are available on packages:
- default: toggle on/off

**Tips** Use <kbd>SPC h l</kbd> to resume the last helm session. It is
handy to quickly toggle on and off a toggle.

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

#### ace-link mode

Similar to `ace-jump-mode`, [ace-link][ace-link] allows one to jump to any link in
`help-mode` and `info-mode` with two key strokes.

Key Binding          |                 Description
---------------------|------------------------------------------------------------------
<kbd>o</kbd>         | initiate ace link mode in `help-mode` and `info-mode`

### Window manipulation

#### Window manipulation key bindings

Every window has a number displayed at the start of the mode-line and can
be quickly accessed using `<SPC> number`.

Key Binding         |                    Description
--------------------|----------------------------------------------------------------
<kbd>SPC 1</kbd>    | go to window number 1
<kbd>SPC 2</kbd>    | go to window number 2
<kbd>SPC 3</kbd>    | go to window number 3
<kbd>SPC 4</kbd>    | go to window number 4
<kbd>SPC 5</kbd>    | go to window number 5
<kbd>SPC 6</kbd>    | go to window number 6
<kbd>SPC 7</kbd>    | go to window number 7
<kbd>SPC 8</kbd>    | go to window number 8
<kbd>SPC 9</kbd>    | go to window number 9
<kbd>SPC 0</kbd>    | go to window number 0

Windows manipulation commands (start with `w`):

Key Binding                               |                 Description
------------------------------------------|----------------------------------------------------------------
<kbd>SPC w =</kbd>                        | balance split windows
<kbd>SPC w b</kbd>                        | force the focus back to the minibuffer (usefull with `helm` popups)
<kbd>SPC w c</kbd>                        | close a window
<kbd>SPC w C</kbd>                        | delete another window using [ace-delete-window][ace-window]
<kbd>SPC w d</kbd>                        | toggle window dedication (dedicated window cannot be reused by a mode)
<kbd>SPC w h</kbd>                        | move to window on the left
<kbd>SPC w H</kbd>                        | move window to the left
<kbd>SPC w j</kbd>                        | move to window below
<kbd>SPC w J</kbd>                        | move window to the bottom
<kbd>SPC w k</kbd>                        | move to window above
<kbd>SPC w K</kbd>                        | move window to the top
<kbd>SPC w l</kbd>                        | move to window on the right
<kbd>SPC w L</kbd>                        | move window to the right
<kbd>SPC w m</kbd>                        | maximize/minimize a window (maximize is equivalent to delete otehr windows)
<kbd>SPC w M</kbd>                        | maximize/minimize a window, when maximized the buffer is centered
<kbd>SPC w o</kbd>                        | cycle and focus between frames
<kbd>SPC w p m</kbd>                      | open messages buffer in a popup window
<kbd>SPC w p p</kbd>                      | close the current sticky popup window
<kbd>SPC w R</kbd>                        | rotate windows clockwise
<kbd>SPC w s</kbd> or <kbd>SPC w /</kbd>  | horizontal split
<kbd>SPC w S</kbd>                        | horizontal split and focus new window
<kbd>SPC w u</kbd>                        | undo window layout (used to effectively undo a closed window)
<kbd>SPC w U</kbd>                        | redo window layout
<kbd>SPC w v</kbd> or  <kbd>SPC w -</kbd> | vertical split
<kbd>SPC w V</kbd>                        | vertical split and focus new window
<kbd>SPC w w</kbd>                        | cycle and focus between windows
<kbd>SPC w W</kbd>                        | select window using [ace-window][ace-window]

#### Window manipulation micro-state

A convenient window manipulation micro-state allows to perform most of the
actions listed above. The micro-state allows additional actions as well like
window resizing.

Key Binding         | Description
--------------------|------------------------------------------------------------
<kbd>SPC w .</kbd>  | initiate micro-state
<kbd>?</kbd>        | display the full documentation in minibuffer
<kbd>0</kbd>        | go to window number 0
<kbd>1</kbd>        | go to window number 1
<kbd>2</kbd>        | go to window number 2
<kbd>3</kbd>        | go to window number 3
<kbd>4</kbd>        | go to window number 4
<kbd>5</kbd>        | go to window number 5
<kbd>6</kbd>        | go to window number 6
<kbd>7</kbd>        | go to window number 7
<kbd>8</kbd>        | go to window number 8
<kbd>9</kbd>        | go to window number 9
<kbd>-</kbd>        | vertical split
<kbd>/</kbd>        | horizontal split
<kbd>[</kbd>        | shrink window horizontally
<kbd>]</kbd>        | enlarge window horizontally
<kbd>{</kbd>        | shrink window vertically
<kbd>}</kbd>        | enlarge window vertically
<kbd>c</kbd>        | close window
<kbd>C</kbd>        | close other windows
<kbd>g</kbd>        | toggle `golden-ratio` on and off
<kbd>h</kbd>        | go to window on the left
<kbd>j</kbd>        | go to window below
<kbd>k</kbd>        | go to window above
<kbd>l</kbd>        | go to window on the right
<kbd>H</kbd>        | move window to the left
<kbd>J</kbd>        | move window to the bottom
<kbd>K</kbd>        | move bottom to the top
<kbd>L</kbd>        | move window to the right
<kbd>o</kbd>        | focus other frame
<kbd>R</kbd>        | rotate windows
<kbd>s</kbd>        | horizontal split
<kbd>S</kbd>        | horizontal split and focus new window
<kbd>u</kbd>        | undo window layout (used to effectively undo a closed window)
<kbd>U</kbd>        | redo window layout
<kbd>v</kbd>        | vertical split
<kbd>V</kbd>        | horizontal split and focus new window
<kbd>w</kbd>        | focus other window
Any other key       | leave the micro-state

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

#### Buffers manipulation key bindings

Buffer manipulation commands (start with `b`):

Key Binding                               |              Description
------------------------------------------|----------------------------------------------------------------
<kbd>SPC b 0</kbd>                        | move to the beginning of buffer (useful in `emacs state` buffers)
<kbd>SPC b $</kbd>                        | move to the end of buffer (useful in `emacs state` buffers)
<kbd>SPC b b</kbd> or <kbd>SPC TAB</kbd>  | switch to alternate buffer (switch back and forth)
<kbd>SPC b d</kbd>                        | kill the current buffer (does not delete the visited file)
<kbd>SPC b e</kbd>                        | erase the content of the buffer (ask for confirmation)
<kbd>SPC b h</kbd>                        | open `*spacemacs*` home buffer
<kbd>SPC b k</kbd>                        | kill the current buffer
<kbd>SPC b K</kbd>                        | kill all buffers except the current one
<kbd>SPC b C-K</kbd>                      | kill all buffers matching the regexp
<kbd>SPC b m h</kbd>                      | move a buffer to the left
<kbd>SPC b m j</kbd>                      | move a buffer to the bottom
<kbd>SPC b m k</kbd>                      | move a buffer to the top
<kbd>SPC b m l</kbd>                      | move a buffer to the right
<kbd>SPC b M</kbd>                        | swap windows using [ace-swap-window][ace-window]
<kbd>SPC b n</kbd>                        | switch to next buffer
<kbd>SPC b p</kbd>                        | switch to previous buffer
<kbd>SPC b r</kbd>                        | rename the current buffer
<kbd>SPC b R</kbd>                        | revert the current buffer (reload from disk)
<kbd>SPC b s</kbd>                        | switch to a buffer using `helm`
<kbd>SPC b w</kbd>                        | toggle read-only (writable state)
<kbd>z f</kbd>                            | Make current function or comments visible in buffer as much as possible

#### Buffers manipulation manipulation micro-state

A convenient buffer manipulation micro-state allows to quickly cycles through
the opened buffer and kill them.

Key Binding         | Description
--------------------|------------------------------------------------------------
<kbd>SPC b .</kbd>  | initiate micro-state
<kbd>K</kbd>        | kill current buffer
<kbd>n</kbd>        | go to next buffer (avoid special buffers)
<kbd>N</kbd>        | go to previous buffer (avoid special buffers)
Any other key       | leave the micro-state

#### Files manipulations key bindings

Files manipulation commands (start with `f`):

Key Binding                               |                 Description
------------------------------------------|----------------------------------------------------------------
<kbd>SPC f D</kbd>                        | delete a file and the associated buffer (ask for confirmation)
<kbd>SPC f f</kbd>                        | open a file using `ido`
<kbd>SPC f F</kbd>                        | open a file under point using `helm`
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
<kbd>C-k</kbd>          | select previous file or directory
<kbd>C-l</kbd>          | open the selected file
<kbd>C-n</kbd>          | select next file or directory
<kbd>C-o</kbd>          | open selected file in other window
<kbd>C-p</kbd>          | select previous file or directory
<kbd>C-s</kbd>          | open selected file in a vertically split window
<kbd>C-t</kbd>          | open selected file in a new frame
<kbd>C-v</kbd>          | open selected file in a horizontally split window
<kbd>C-S-h</kbd>        | go to previous directory
<kbd>C-S-j</kbd>        | next history element
<kbd>C-S-k</kbd>        | previous history element
<kbd>C-S-l</kbd>        | go to next directory
<kbd>C-S-n</kbd         | next history element
<kbd>C-S-p</kbd         | previous history element

### Ido micro-state

`Spacemacs` defines a [micro-state](#micro-states) for `ido`.

Initiate the micro-state with <kbd>C-SPC</kbd> while in a `ido` buffer.
Use <kbd>C-SPC</kbd> again to exit from the micro-state.

Key Binding           | Description
----------------------|------------------------------------------------------------
<kbd>C-SPC</kbd>      | initiate or leave the micro-state
<kbd>?</kbd>          | display help
<kbd>e</kbd>          | open dired
<kbd>h</kbd>          | delete backward or parent directory
<kbd>j</kbd>          | next match
<kbd>J</kbd>          | sub directory
<kbd>k</kbd>          | previous match
<kbd>K</kbd>          | parent directory
<kbd>l</kbd>          | select match
<kbd>n</kbd>          | next directory in history
<kbd>o</kbd>          | open in other window
<kbd>p</kbd>          | previous directory in history
<kbd>s</kbd>          | open in a new horizontal split
<kbd>t</kbd>          | open in other frame
<kbd>v</kbd>          | open in a new vertical split

### NeoTree file tree

`Spacemacs` provides a quick and simple way to navigate in an unknown project
file tree with [NeoTree][neotree].

To toggle the `NeoTree` buffer press <kbd>SPC f t</kbd> or <kbd>SPC p t</kbd>
(the latter open NeoTree with the root set to the projectile project root).

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
<kbd>R</kbd>                     | make a directory the root directory

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
<kbd>\|</kbd>                     | open file in an vertically split window
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
states can be tedious. The solution to this is to use <kbd>C-o</kbd> then use
the leader key. <kbd>C-o</kbd> set the next key to be evaluated in
`normal state`.

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

### DocView mode

`doc-view-mode` is a built-in major mode to view DVI, PostScript (PS), PDF,
OpenDocument, and Microsoft Office documents.

Key Binding        |                 Description
-------------------|----------------------------------------------------------------
<kbd>/</kbd>       | search forward
<kbd>?</kbd>       | search backward
<kbd>+</kbd>       | enlarge
<kbd>-</kbd>       | shrink
<kbd>gg</kbd>      | go to first page
<kbd>G</kbd>       | go to last page
<kbd>gt</kbd>      | go to page number
<kbd>h</kbd>       | previous page
<kbd>H</kbd>       | adjust to height
<kbd>j</kbd>       | next line
<kbd>k</kbd>       | previous line
<kbd>K</kbd>       | kill proc and buffer
<kbd>l</kbd>       | next page
<kbd>n</kbd>       | go to next search occurrence
<kbd>N</kbd>       | go to previous search occurrence
<kbd>P</kbd>       | fit page to window
<kbd>r</kbd>       | revert
<kbd>W</kbd>       | adjust to width
<kbd>C-d</kbd>     | scroll down
<kbd>C-k</kbd>     | kill proc
<kbd>C-u</kbd>     | scroll up
<kbd>C-c C-c</kbd> | toggle display text and image display
<kbd>C-c C-t</kbd> | open new buffer with doc's text contents

## Searching


### With an external tool

`Spacemacs` can be interfaced with different search utilities:
- ack
- grep
- [ag][]
- [pt][]

**Note** `ag` and `pt` are optimized to be used in a source control repository
but they can be used in an arbitrary directory as well.

#### Searching in an arbitrary directory

To use these utilities in one or several arbitrary directories:

Key Binding               |                 Description
--------------------------|---------------------------------------------
<kbd>SPC s /</kbd>        | execute the first found utility in this order `pt`, `ag`, `ack` and `grep`
<kbd>SPC s a</kbd>        | `ag`
<kbd>SPC s g</kbd>        | `grep`
<kbd>SPC s k</kbd>        | `ack`
<kbd>SPC s p</kbd>        | `pt`

**Note** Use the universal argument to change the search list of
<kbd>SPC s /</kbd> to `ack` and `grep` (does not look for `ag` or `pt`).

**Note** It is also possible to search in several directories at once by
marking them in the helm buffer.

#### Searching in a project

To use these utilities in a project using `projectile`:

Key Binding               |                 Description
--------------------------|---------------------------------------------
<kbd>SPC /</kbd>          | execute the first found utility in this order `pt`, `ag`, `ack` and `grep`
<kbd>SPC p s a</kbd>      | `ag`
<kbd>SPC p s g</kbd>      | `grep`
<kbd>SPC p s k</kbd>      | `ack`
<kbd>SPC p s p</kbd>      | `pt`

**Pro Tip** Use <kbd>SPC h l</kbd> to bring back the last helm session.

#### Searching the web

Key Binding               |                 Description
--------------------------|---------------------------------------------
<kbd>SPC s w g</kbd>      | Get Google suggestions in emacs. Opens Google results in Browser.
<kbd>SPC s w w</kbd>      | Get Wikipedia suggestions in emacs. Opens Wikipedia page in Browser.

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
the [auto-highlight-symbol][auto-highlight] mode) and adds a micro-state to
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
<kbd>*</kbd>           | initiate navigation micro-state on current symbol and jump forwards
<kbd>#</kbd>           | initiate navigation micro-state on current symbol and jump backwards
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

### Paste text

Whenever you paste some text a `paste` micro-state is initiated. Pressing
<kbd>p</kbd> again will replace the pasted text with the previous
yanked (copied) text on the kill ring.

For example if you copy `foo` and `bar` then press <kbd>p</kbd> the text `bar`
will be pasted, pressing <kbd>p</kbd> again will replace `bar` with `foo`.

Key Binding                   |                    Description
------------------------------|----------------------------------------------------------------
<kbd>p</kbd> or <kbd>P</kbd>  | paste the text before or after point and initiate the `paste` micro-state
<kbd>p</kbd>                  | in micro-state: replace paste text with the previously copied one
<kbd>P</kbd>                  | in micro-state: replace paste text with the next copied one
<kbd>.</kbd>                  | paste the same text and leave the micro-state
Any other key                 | leave the micro-state

This micro-state can be disabled by setting
`dotspacemacs-enable-paste-micro-state` to `nil` in `~/.spacemacs`.

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

### Replacing text in several files

Replacing an occurrence of text in several files can be performed via
[helm-ag][].

Say you want to replace all `foo` occurrences by `bar` in your current project:
- initiate a search with <kbd>SPC /</kbd>
- enter in edit mode with <kbd>C-c C-e</kbd>
- go to the occurrence and enter in `iedit state` with <kbd>SPC s e</kbd>
- edit the occurrences then leave the `iedit state`
- press <kbd>C-c C-c</kbd>

**Note** in Spacemacs, `helm-ag` despite its name works with `ack` and `pt` as
well.

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

Commands will set the current state to `lisp state` where
different commands combo can be repeated without pressing on
<kbd>SPC m</kbd>.

When in `lisp state` the color of the mode-line changes to pink.

Examples:
- to slurp three times while in normal state: <kbd>SPC k 3 n</kbd>
- to wrap a symbol in parenthesis then slurping two times: <kbd>SPC k w 2 n</kbd>

**Note** The `lisp state` commands are available in _any_ modes! Try it out.

#### Lisp Key Bindings

##### Lisp state key bindings

These commands automatically switch to `lisp state`.

Key Binding          | Function
---------------------|------------------------------------------------------------
<kbd>SPC k %</kbd>   | evil jump item
<kbd>SPC k :</kbd>   | ex command
<kbd>SPC k (</kbd>   | insert expression before (same level as current one)
<kbd>SPC k )</kbd>   | insert expression after (same level as current one)
<kbd>SPC k $</kbd>   | go to the end of current sexp
<kbd>SPC k ` k</kbd> | hybrid version of kill sexp (can be used in non lisp dialects)
<kbd>SPC k ` p</kbd> | hybrid version of push sexp (can be used in non lisp dialects)
<kbd>SPC k ` s</kbd> | hybrid version of slurp sexp (can be used in non lisp dialects)
<kbd>SPC k ` t</kbd> | hybrid version of transpose sexp (can be used in non lisp dialects)
<kbd>SPC k 0</kbd>   | go to the beginning of current sexp
<kbd>SPC k a</kbd>   | absorb expression
<kbd>SPC k b</kbd>   | forward barf expression
<kbd>SPC k B</kbd>   | backward barf expression
<kbd>SPC k c</kbd>   | convolute expression
<kbd>SPC k ds</kbd>  | delete symbol
<kbd>SPC k Ds</kbd>  | backward delete symbol
<kbd>SPC k dw</kbd>  | delete word
<kbd>SPC k Dw</kbd>  | backward delete word
<kbd>SPC k dx</kbd>  | delete expression
<kbd>SPC k Dx</kbd>  | backward delete expression
<kbd>SPC k e</kbd>   | unwrap current expression and kill all symbols after point
<kbd>SPC k E</kbd>   | unwrap current expression and kill all symbols before point
<kbd>SPC k h</kbd>   | previous symbol
<kbd>SPC k H</kbd>   | go to previous sexp
<kbd>SPC k i</kbd>   | switch to `insert state`
<kbd>SPC k I</kbd>   | go to beginning of current expression and switch to `insert state`
<kbd>SPC k j</kbd>   | next closing parenthesis
<kbd>SPC k J</kbd>   | join expression
<kbd>SPC k k</kbd>   | previous opening parenthesis
<kbd>SPC k l</kbd>   | next symbol
<kbd>SPC k L</kbd>   | go to next sexp
<kbd>SPC k p</kbd>   | paste after
<kbd>SPC k P</kbd>   | paste before
<kbd>SPC k r</kbd>   | raise expression (replace parent expression by current one)
<kbd>SPC k s</kbd>   | forwared slurp expression
<kbd>SPC k S</kbd>   | backward slurp expression
<kbd>SPC k t</kbd>   | transpose expression
<kbd>SPC k u</kbd>   | undo
<kbd>SPC k U</kbd>   | got to parent sexp backward
<kbd>SPC k C-r</kbd> | redo
<kbd>SPC k v</kbd>   | switch to `visual state`
<kbd>SPC k V</kbd>   | switch to `visual line state`
<kbd>SPC k C-v</kbd> | switch to `visual block state`
<kbd>SPC k w</kbd>   | wrap expression with parenthesis
<kbd>SPC k W</kbd>   | unwrap expression
<kbd>SPC k y</kbd>   | copy expression

##### Emacs lisp specific key bindings

Key Binding          | Function
---------------------|------------------------------------------------------------
<kbd>SPC m e $</kbd> | go to end of line and evaluate last sexp
<kbd>SPC m e b</kbd> | evaluate buffer
<kbd>SPC m e c</kbd> | evaluate current form (a `def` or a `set`)
<kbd>SPC m e e</kbd> | evaluate last sexp
<kbd>SPC m e f</kbd> | evaluate current defun
<kbd>SPC m e l</kbd> | go to end of line and evaluate last sexp
<kbd>SPC m e r</kbd> | evaluate region

Key Binding          | Function
---------------------|------------------------------------------------------------
<kbd>SPC m g g</kbd> | go to definition
<kbd>SPC m h h</kbd> | describe elisp thing at point (show documentation)
<kbd>SPC m t b</kbd> | execute buffer tests
<kbd>SPC m t q</kbd> | ask for test function to execute

## Managing projects

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
<kbd>SPC p !</kbd>  | run shell command in root
<kbd>SPC p &</kbd>  | run async shell command in root
<kbd>SPC p a</kbd>  | run `ag`
<kbd>SPC p A</kbd>  | run `ack`
<kbd>SPC p b</kbd>  | switch to project buffer
<kbd>SPC p c</kbd>  | compile project using `projectile`
<kbd>SPC p d</kbd>  | find directory
<kbd>SPC p D</kbd>  | open project root in `dired`
<kbd>SPC p f</kbd>  | find file
<kbd>SPC p g</kbd>  | run `grep`
<kbd>SPC p h</kbd>  | find file using `helm`
<kbd>SPC p I</kbd>  | invalidate the projectile cache
<kbd>SPC p k</kbd>  | kill all project buffers
<kbd>SPC p o</kbd>  | run `multi-occur`
<kbd>SPC p p</kbd>  | switch project
<kbd>SPC p R</kbd>  | regenerate the project's [e|g]tags
<kbd>SPC p r</kbd>  | replace a string
<kbd>SPC p s</kbd>  | see [search in project](#searching-in-a-project)
<kbd>SPC p t</kbd>  | open `NeoTree` in `projectile` root
<kbd>SPC p T</kbd>  | find test files
<kbd>SPC p v</kbd>  | open project root in `vc-dir` or `magit`
<kbd>SPC p y</kbd>  | find tags

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

### Python

Writing python code with spacemacs is supported by python contribution. Please
see [python contribution][python-contrib] documentation for detail.

### JavaScript

More featured JavaScript support is provided by the javascript
contribution. Please see [javascript contribution][javascript-contrib]
documentation for detail.

### HTML and CSS

HTML contribution provides support for editing HTML, CSS, Scss and Less
files. Please see [html contribution][html-contrib] documentation for detail.

# Emacs Server

`Spacemacs` starts a server at launch. This server is killed whenever you close
your Emacs windows.

## Connecting to the Emacs server

You can open a file in Emacs from the terminal using `emacsclient`. Use
`emacsclient -c` to open the file in Emacs GUI. Use `emacsclient -t` to open
the file in Emacs within the terminal.

If you want your Linux/OS X system to use Emacs by default for any prompt, use
`export EDITOR="emacsclient -c"` in your shell configuration.

Note that if you're on OS X, you may have to refer to the emacsclient that comes
with your GUI Emacs, e.g. `export
EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c"`.

Tip: Remember to use `:qw` or `C-x #` after you are done editing the file
in Emacs.

See [Emacs as a Server][] in the official Emacs manual for more details.

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
<kbd>SPC q q</kbd> | Quit Emacs and kill the server, prompt for changed buffers to save
<kbd>SPC q Q</kbd> | Quit Emacs and kill the server, lose all unsaved changes.
<kbd>SPC q s</kbd> | Save the buffers, quit Emacs and kill the server
<kbd>SPC q z</kbd> | Kill the current frame

## Troubleshoot

### Loading fails

If any errors happen during the loading the mode-line will turn red and the
errors should appear inline in the startup buffer. Spacemacs should still be
usable, if it is not the case then restart Emacs with `emacs --debug-init` and
open a [Github issue][issues] with the backtrace.

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

# Achievements

## Issues

Achievements                                         | Account
-----------------------------------------------------|------------------------
[100th issue (PR)][100th-issue]                      | [danielwuz][]
[200th issue (question)][200th-issue]                | [justrajdeep][]
[300th issue (PR)][300th-issue]                      | [danielwuz][]
[400th issue (PR)][400th-issue]                      | [CestDiego][]
[500th issue (PR)][500th-issue]                      | [bjarkevad][]
[600th issue (PR)][600th-issue]                      | [bjarkevad][]
[700th issue (enhancement)][700th-issue]             | [jcpetkovich][]
[800th issue (PR)][800th-issue]                      | [ryansroberts][]
[900th issue (PR)][900th-issue]                      | [jcpetkovich][]
[1000th issue (PR)][1000th-issue]                    | [tuhdo][]

## Merged Pull Requests

Achievements                                         | Account
-----------------------------------------------------|------------------------
[100th pull request][100th-PR]                       | [bru][]
[200th pull request][200th-PR]                       | [smt][]
[300th pull request][300th-PR]                       | [BrianHicks][]
[400th pull request][400th-PR]                       | [cpaulik][]

## Stars and forks

Achievements                                         | Account
-----------------------------------------------------|------------------------
100th fork                                           | [balajisivaraman][]
200th fork                                           | [alcol80][]
100th star                                           | [Jackneill][]
200th star                                           | [jb55][]
400th star                                           | [dbohdan][]
600th star                                           | [laat][]
700th star                                           | [kendall][]
800th star                                           | [urso][]
900th star                                           | [luisgerhorst][]

## Specials

Achievements                                         | Account
-----------------------------------------------------|------------------------
[First contribution][1st-contrib]                    | [trishume][]
[First contribution layer][1st-clayer]               | [trishume][]
[First blog article on Spacemacs][1st-article]       | [Wolfy87][]
[First contributed banner][1st-cbanner]              | [chrisbarrett][]
The Gunner (made 18 PRs in a row)                    | [ralesi][]
The Saint (unlocked the holy-mode)                   | [trishume][]
The Artist (made the spacemacs logo)                 | [nashamri][]
The Meme Master (made the doge banner)               | [chrisbarrett][]

# Thank you

[Jokes aside](#using-configuration-layers), thank you Richard for this great
piece of software.

Thank you to all the contributors and the whole Emacs community from core
developers to elisp hackers!

[CONVENTIONS.md]: ./CONVENTIONS.md
[evil]: https://gitorious.org/evil/pages/Home
[evil-leader]: https://github.com/cofi/evil-leader
[RSI]: http://en.wikipedia.org/wiki/Repetitive_strain_injury
[sacha_guide]: http://sachachua.com/blog/2013/05/how-to-learn-emacs-a-hand-drawn-one-pager-for-beginners/
[use-package]: https://github.com/jwiegley/use-package
[keychords]: http://www.emacswiki.org/emacs/KeyChord
[centered-cursor]: http://www.emacswiki.org/emacs/centered-cursor-mode.el
[ace-jump]: https://github.com/winterTTr/ace-jump-mode
[ace-link]: https://github.com/abo-abo/ace-link
[ace-window]: https://github.com/abo-abo/ace-window
[helm-link]: https://github.com/emacs-helm/helm
[helm-doc]: https://github.com/emacs-helm/helm/wiki
[helm-ag]: https://github.com/syohex/emacs-helm-ag
[popwin]: http://www.emacswiki.org/emacs/PopWin
[golden-ratio]: https://github.com/roman/golden-ratio.el
[solarized-theme]: https://github.com/bbatsov/solarized-emacs
[powerline]: https://github.com/milkypostman/powerline
[font-spec]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Low_002dLevel-Font.html
[diminish]: http://www.emacswiki.org/emacs/DiminishedModes
[auto-complete]: https://github.com/auto-complete
[auto-highlight]: https://github.com/emacsmirror/auto-highlight-symbol
[e-project]: https://github.com/jrockway/eproject
[projectile]: https://github.com/bbatsov/projectile
[sp]: https://github.com/Fuco1/smartparens
[ag]: https://github.com/ggreer/the_silver_searcher
[pt]: https://github.com/monochromegane/the_platinum_searcher
[flycheck]: https://github.com/flycheck
[yasnippet]: https://github.com/capitaomorte/yasnippet
[expand-region]: https://github.com/magnars/expand-region.el
[multiple-cursors]: https://github.com/magnars/multiple-cursors.el
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
[evil-lisp-state]: https://github.com/syl20bnr/evil-lisp-state
[Vim's Unite]: https://github.com/Shougo/unite.vim
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
[javascript-contrib]: ../contrib/lang/javascript
[themes-megapack]: ../contrib/themes-megapack
[python-contrib]: ../contrib/lang/python
[git layer]: ../contrib/git
[html-contrib]: ../contrib/lang/html
[guide-key]: https://github.com/kai2nenobu/guide-key
[guide-key-tip]: https://github.com/aki2o/guide-key-tip
[gitter]: https://gitter.im/syl20bnr/spacemacs
[CONTRIBUTE.md]: ./CONTRIBUTE.md
[neotree]: https://github.com/jaypei/emacs-neotree
[nerdtree]: https://github.com/scrooloose/nerdtree
[anaconda-mode]: https://github.com/proofit404/anaconda-mode
[1st-contrib]: https://github.com/syl20bnr/spacemacs/pull/19
[1st-clayer]: https://github.com/syl20bnr/spacemacs/commit/e802027d75d0c0aed55539b0da2dfa0df94dfd39
[1st-article]: http://oli.me.uk/2014/11/06/spacemacs-emacs-vim/
[1st-cbanner]: https://github.com/syl20bnr/spacemacs/commit/7b44a56263049482ed540ed6815a295633ffe9d1
[100th-issue]: https://github.com/syl20bnr/spacemacs/pull/100
[200th-issue]: https://github.com/syl20bnr/spacemacs/issues/200
[300th-issue]: https://github.com/syl20bnr/spacemacs/pull/300
[400th-issue]: https://github.com/syl20bnr/spacemacs/pull/400
[500th-issue]: https://github.com/syl20bnr/spacemacs/pull/500
[600th-issue]: https://github.com/syl20bnr/spacemacs/pull/600
[700th-issue]: https://github.com/syl20bnr/spacemacs/pull/700
[800th-issue]: https://github.com/syl20bnr/spacemacs/pull/800
[900th-issue]: https://github.com/syl20bnr/spacemacs/pull/900
[1000th-issue]: https://github.com/syl20bnr/spacemacs/pull/1000
[100th-PR]: https://github.com/syl20bnr/spacemacs/pull/228
[200th-PR]: https://github.com/syl20bnr/spacemacs/pull/418
[300th-PR]: https://github.com/syl20bnr/spacemacs/pull/617
[400th-PR]: https://github.com/syl20bnr/spacemacs/pull/806
[trishume]:https://github.com/trishume
[nashamri]: https://github.com/nashamri
[Wolfy87]:https://github.com/Wolfy87
[danielwuz]:https://github.com/danielwuz
[CestDiego]:https://github.com/CestDiego
[bjarkevad]:https://github.com/bjarkevad
[jcpetkovich]:https://github.com/jcpetkovich
[tuhdo]:https://github.com/tuhdo
[BrianHicks]:https://github.com/BrianHicks
[cpaulik]: https://github.com/cpaulik
[chrisbarrett]:https://github.com/chrisbarrett
[justrajdeep]:https://github.com/justrajdeep
[dbohdan]:https://github.com/dbohdan
[laat]:https://github.com/laat
[ryansroberts]:https://github.com/laat
[kendall]:https://github.com/kendall
[urso]:https://github.com/urso
[luisgerhorst]:https://github.com/luisgerhorst
[bru]:https://github.com/bru
[smt]:https://github.com/smt
[ralesi]:https://github.com/ralesi
[alcol80]:https://github.com/alcol80
[balajisivaraman]:https://github.com/balajisivaraman
[Jackneill]:https://github.com/Jackneill
[jb55]:https://github.com/jb55
[use-package]: https://github.com/jwiegley/use-package
[Paradox]: https://github.com/Bruce-Connor/paradox
[fancy-battery]: https://github.com/lunaryorn/fancy-battery.el
[MacType]: https://code.google.com/p/mactype/
[Emacs as a Server]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html
