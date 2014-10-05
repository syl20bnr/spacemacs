# Spacemacs Starter Kit

    ┏━━━┓
    ┃┏━┓┃ Welcome to
    ┃┗━━┓╋╋╋╋┏━━┓╋╋╋╋┏━━┓╋╋╋╋┏━━┓╋╋╋╋┏━━┓╋╋╋╋┏┓┏┓╋╋╋╋┏━━┓╋╋╋╋┏━━┓╋╋╋╋┏━━┓ b
    ┗━━┓┃┏━━┓┃┏┓┃┏━━┓┃┏┓┃┏━━┓┃┏━┛┏━━┓┃┃━┫┏━━┓┃┗┛┃┏━━┓┃┏┓┃┏━━┓┃┏━┛┏━━┓┃━━┫ e
    ┃┗━┛┃┗━━┛┃┗┛┃┗━━┛┃┏┓┃┗━━┛┃┗━┓┗━━┛┃┃━┫┗━━┛┃┃┃┃┗━━┛┃┏┓┃┗━━┛┃┗━┓┗━━┛┣━━┃ t
    ┗━━━┛╋╋╋╋┃┏━┛╋╋╋╋┗┛┗┛╋╋╋╋┗━━┛╋╋╋╋┗━━┛╋╋╋╋┗┻┻┛╋╋╋╋┗┛┗┛╋╋╋╋┗━━┛╋╋╋╋┗━━┛ a
    ╋╋╋╋╋╋╋╋╋┃┃[The best editor is neither Emacs nor Vim, it's Emacs+Vim]
    ╋╋╋╋╋╋╋╋╋┗┛


**Quick Install:**

    git clone --recursive http://github.com/syl20bnr/spacemacs .emacs.d

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Spacemacs Starter Kit](#spacemacs-starter-kit)
    - [Goals](#goals)
    - [Screenshots](#screenshots)
    - [Who can benefit from this ?](#who-can-benefit-from-this-)
    - [Prerequisites](#prerequisites)
    - [Install](#install)
    - [Configuration layers](#configuration-layers)
        - [Structure](#structure)
        - [Extensions and Packages initialization](#extensions-and-packages-initialization)
    - [Contributions](#contributions)
    - [Main principles](#main-principles)
        - [Evil](#evil)
        - [Evil leader](#evil-leader)
        - [Micro-states](#micro-states)
    - [UI tweaks](#ui-tweaks)
    - [Commands](#commands)
        - [Return to normal mode](#return-to-normal-mode)
        - [Executing Vim and Emacs commands](#executing-vim-and-emacs-commands)
        - [Key bindings help](#key-bindings-help)
        - [Included Evil plugins](#included-evil-plugins)
        - [About helm](#about-helm)
        - [Navigation](#navigation)
            - [Point/Cursor](#pointcursor)
            - [Vim motions with ace-jump mode](#vim-motions-with-ace-jump-mode)
            - [Buffers and Files](#buffers-and-files)
            - [Bookmarks](#bookmarks)
        - [Window manipulation](#window-manipulation)
            - [Golden ratio](#golden-ratio)
        - [Text manipulation commands](#text-manipulation-commands)
        - [Change font size](#change-font-size)
        - [Spell checking](#spell-checking)
        - [Region selection](#region-selection)
        - [Region narrowing](#region-narrowing)
        - [Auto highlight and edition of symbols](#auto-highlight-and-edition-of-symbols)
        - [Color theme](#color-theme)
        - [UI elements](#ui-elements)
            - [Mode-line](#mode-line)
            - [Toggles](#toggles)
        - [Minor Modes](#minor-modes)
        - [Line formatting](#line-formatting)
        - [Errors handling](#errors-handling)
        - [Project management](#project-management)
        - [Modes](#modes)
            - [Helm](#helm)
            - [Erlang](#erlang)
            - [Ledger](#ledger)
            - [Lisp](#lisp)
            - [Magit](#magit)
            - [Org](#org)
            - [Perforce](#perforce)
            - [Python](#python)
                - [Inferior REPL process](#inferior-repl-process)
                - [Testing in Python](#testing-in-python)
                - [Other Python commands](#other-python-commands)
            - [R (ESS)](#r-ess)
                - [Inferior REPL process](#inferior-repl-process)
                - [Other R commands](#other-r-commands)
            - [rcirc](#rcirc)
    - [TODO list](#todo-list)
    - [Thank you](#thank-you)

<!-- markdown-toc end -->

## Goals

- The main goal of `Spacemacs` is to **bring the power of Vim modal editing
to the powerful Emacs editing platform**.

- Slick integration with `Evil` states (`Vim` modes) with custom
`micro-states` (temporary overlay map): `Spacemacs` tries to **keep your
fingers on the home row** as much as possible, no matter the mode you are in.

- **Community driven configuration** based on a configuration layer system
*(work in progress)*. Contribute your own personal layer upstream and
everybody can use it.

- **Minimalistic and nice custom UI**, keep your available screen space for
what matters: your text files.

- **Mnemonic and consistent key bindings** which should be easier to learn
and remember.

- **Fast boot time**, some time has been invested to make 'Spacemacs' quick
to load.

- **Lower the risk of RSI** by using the space key as much as possible.

- Hopefully, if it's not already the case: Ɛ> **make you love modal editing!** <3

## Screenshots

![spacemacs_startup](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/spacemacs-startup.png)
![spacemacs_python](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/spacemacs-python.png)

*Note: Even though screenshots are updated frequently, `Spacemacs` is evolving
quickly and the screenshots may not reflect exactly the current state of the
project.*

## Who can benefit from this ?

`Spacemacs` is first intended to be used by **Vim users** who want to go to the
next level by using Emacs.

It is also a good fit for people wanting to **lower the [risk of RSI][RSI]**
induced by the default Emacs key bindings. 

People wanting to learn **a different way to edit files** or wanting to learn
Vim key bindings.

As a note side, if you are a programmer and you don't know Vim key bindings
yet, I deeply recommend you to learn the basics as recommended in
[Sacha Chua's one-page guide][sacha_guide] about how to learn Emacs.

## Prerequisites

`Spacemacs` is compatible with Emacs 24.3 and above and should boot on all
the major OSes where this version can be installed.

Some packages require external tools to work, a list of all dependencies will
be provided in this read me. _Stay tuned._

## Install

Backup your current `.emacs.d`, clone the repo _with the submodules_ and
you are good to go:

    cd ~
    mv .emacs.d .emacs.bak
    git clone --recursive http://github.com/syl20bnr/spacemacs .emacs.d

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
repository, and `Extensions` are elisp code from git submodules, they can also
be directly stored in this git repository (maybe we call this: site elisp
code ??).

### Extensions and Packages initialization

`Extensions` and `Packages` are listed in variables `<layer>-pre-extensions`,
`<layer>-post-extensions` and `<layer>-packages` where `<layer>` is the layer
name. `Pre-Extensions` are loaded before `Packages` and `Post-Extensions` are
loaded after `Packages`.

They are processed in alphabetical order so sometimes you'll have to use
some `after-eval-load` black magic.

To initialize an extension or a package `xxx`, define a function with this
format in `extensions.el` or `packages.el`:

```elisp
(defun <layer>/init-xxx ()
   ...body
)
```

## Contributions

`Spacemacs` leverages the configuration layers in order to make it possible for
you to share your own layer with other `Spacemacs` users.

To use a contribution layer, add it to the `dotspacemacs-configuration-layers`
variable of your `~/.spacemacs`

For instance to add the configuration layer of [RMS](#thank-you) just do:
```elisp
(defvar dotspacemacs-configuration-layers '(rms)
  "List of contribution to load."
)
```
Oh, you don't find this configuration layer ? So sad, well you can try mine:
[syl20bnr](https://github.com/syl20bnr/spacemacs/tree/master/contrib/syl20bnr)

Of course, all pull requests are welcome for all parts of `Spacemacs`.

## Main principles

### Evil

`Spacemacs` uses the [evil][evil] mode to emulate Vim key bindings. It is a
very complete emulation (the most complete I've seen yet).

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

### Micro-states

`Spacemacs` defines a wide variety of `micro-states` (temporary overlay maps)
where it makes sense. This prevent from repetitive and tedious presses on the
`SPC` key.

When a `micro-state` is active, a documentation is displayed in the minibuffer.
Additional information may as well be displayed in the minibuffer.

Auto-highlight-symbol micro-state:
![spacemacs_ahs_micro_state](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/spacemacs-ahs-micro-state.png)

Text scale micro-state:
![spacemacs_scale_micro_state](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/spacemacs-scale-micro-state.png)

## UI tweaks

`Spacemacs` has a minimalistic and distraction free UI with a lot of subtle
customizations which make it unique compared to other Emacs kits:
 - custom [powerline][powerline] mode-line [with color feedback](#mode-line)
 according to current [Flycheck][flycheck] status
 - unicode symbols for minor mode lighters which appear in the mode-line
 - [custom fringe bitmaps](#errors-handling) and error feedbacks for
 [Flycheck][flycheck]
 - custom fringe bitmaps for [git gutter][git-gutter].
 - dedicated startup page with a mode aimed at easily managing `Spacemacs`

## Commands

Every sequences must be performed in `normal` mode.

### Return to normal mode

`ESC` is the default key to return to normal mode. This is one of the main
design flaw in Vim key bindings because the `ESC` key is very far from the
home row.

The popular way to avoid this is to replace `ESC` by `jj` pressed rapidly.
`Spacemacs` **initialy** proposed to press `fd` instead of `jj`. But since
`jj` is one of the most used combination `Spacemacs` now **defaults to `jj`**

This sequence can be customized in your `~/.spacemacs`, for instance to
revert back to the initial configuration using `fd` add this to your file:

```elisp
(defun dotspacemacs/init ()
  "User initialization for Spacemacs. This function is called at the very startup."
  (defvar spacemacs-normal-state-sequence '(?f . ?d))
  (defvar spacemacs-normal-state-sequence-delay 0.1)
)
```

### Executing Vim and Emacs commands

Vim commands are executed as usual with the `:` key.

To execute an Emacs command press `<SPC>` (space) before:

    <SPC> :

### Key bindings help

A list of all the key bindings can be accessed by pressing:

    <SPC> ?

To narrow the list to `Spacemacs` specific key bindings set the pattern to `SPC`

### Included Evil plugins

`Spacemacs` ships with the following evil plugins:

                 Mode                   |                          Description
----------------------------------------|--------------------------------------
[evil-leader][evil-leader]              | vim leader that bring a new layer of keys in normal mode
[evil-little-word][evil-plugin01]       | port of [camelcasemotion.vim][vim-plugin01]
[evil-operator-comment][evil-plugin01]  | comment/uncomment with `CC`
[evil-visualstar][evil-plugin03]        | search for current selection with `*`
[evil-exchange][evil-plugin05]          | port of [vim-exchange][vim-plugin04]
[surround][evil-plugin04]               | port of [surround.vim][vim-plugin03]

### About helm

`Spacemacs` tries to use [helm][helm] as much as possible.
[helm][helm] is coupled to [popwin][popwin] so `helm` window always appears in
a new temporary windows at the bottom.

The following `helm` modes are installed with `Spacemacs`:

Key Binding | Mode                                    | Description
------------|-----------------------------------------|------------------------
`<SPC> h s` | [helm-swoop][hswoop]                    | search for occurrences within a file and edit the result
`<SPC> h c` | [helm-css-scss][hcss]                   | for quick navigation in CSS
`<SPC> h y` | [helm-c-yasnippet][hyas]                | select snippets
`<SPC> h t` | [helm-themes][hthemes]                  | select a theme
`<SPC> p f` | [helm-projectile][projectile]           | select files within a projectile project
`<SPC> ?`   | [helm-descbinds][hdescbinds]            | show key bindings
`<SPC> s c` | [cofi/helm-flyspell-correct][hflyspell] | choose a corrected word

### Navigation

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
`<SPC> f y`   | show current file absolute path in the minibuffer

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

### Window manipulation

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
`<SPC> w p`   | close the current sticky popup window
`<SPC> w r`   | rotate windows clockwise
`<SPC> w R`   | rotate windows counter-clockwise
`<SPC> w u`   | undo window layout (used to effectively undo a close window)
`<SPC> w U`   | redo window layout
`<SPC> w v`   | split a window vertically
`<SPC> w w`   | cycle and focus between windows

#### Golden ratio

Split windows can be dynamically resized depending on whether they are selected
or not. Resizing is performed by the [golden-ratio][golden-ratio] mode.
By default `golden-ratio` if off.

The mode can be toggled on and off with:

    <SPC> t g

### Text manipulation commands

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

### Change font size

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

### Spell checking

Spell checking commands start with `s`:

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> s c`       | list of corrections in a `helm` buffer
`<SPC> s d`       | change dictionary language
`<SPC> s n`       | go to the next spell check error


### Region selection

Vi `Visual` modes are all supported by `evil`, `Spacemacs` adds another
`Visual` mode via the [expand-region][expand-region] mode.

Key Binding   |                 Description
--------------|----------------------------------------------------------------
`<SPC> v`     | initiate expand-region mode then...
`v`           | expand the region by one semantic unit
`V`           | contract the region by one semantic unit
`r`           | reset the region to initial selection
`ESC`         | leave expand-region mode

### Region narrowing

The displayed text of a buffer can be narrowed with the commands
(start with `n`):

Key Binding   |                 Description
--------------|----------------------------------------------------------------
`<SPC> n f`   | narrow the buffer to the current function
`<SPC> n p`   | narrow the buffer to the visible page
`<SPC> n r`   | narrow the buffer to the selected text
`<SPC> n w`   | widen, i.e show the whole buffer again

### Auto highlight and edition of symbols

`Spacemacs` supports auto highlighting of the current word (provided by the
 [auto-highlight-symbol][auto-highlight] mode).

Key Binding   |                 Description
--------------|----------------------------------------------------------------
`<SPC> h e`   | edit all occurrences of the current word
`<SPC> t h`   | toggle the auto highlighting

Navigation between the highlighted symbols can be done with the commands:

Key Binding   | Description
--------------|------------------------------------------------------------
`<SPC> h h`   | initiate navigation micro-state
`<SPC> h n`   | go to next occurrence and initiate navigation micro-state
`<SPC> h N`   | go to previous occurrence and initiate navigation micro-state
`<SPC> h c b` | change range to `whole buffer`
`<SPC> h c d` | change range to `display area`
`<SPC> h c f` | change range to `function`
`<SPC> h C`   | change range to default (`whole buffer`)

In 'Spacemacs' highlight symbol micro-state:

Key Binding   | Description
--------------|------------------------------------------------------------
`c`           | change scope (`function`, `display area`, `whole buffer`)
`e`           | edit occurrences
`n`           | go to next occurrence
`N`           | go to previous occurrence
`d`           | go to next definition occurrence
`D`           | go to previous definition occurrence
`h`           | go to home occurrence (go to starting occurrence)
Any other key | leave the navigation micro-state

The micro-state text in minibuffer display the following information:

    <M> [6/11]* press (n) or (N) to navigate, (h) for home symbol, (c) to change scope

Where <M> [x/y]* is:
- M: the current range mode
  - <B>: whole buffer range
  - <D>: current display range
  - <F>: current function range
- x: the index of the current highlighted occurrence
- y: the total number of occurrences
- * (star): appears if there is at least one occurrence which is not currently
visible

### Color theme

By default, `Spacemacs` uses the theme [Solarized][solarized-theme].

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> c t`       | cycle between `Spacemacs` themes
`<SPC> h t`       | select a theme using a `helm` buffer

`Spacemacs` available themes:
- [Solarized][solarized-theme]
- [Monokai][monokai-theme]
- [Zenburn][zenburn-theme]

### UI elements

#### Mode-line

The mode line is a [powerline][powerline] customized to show the window
number and the colorized Evil current state.

The color codes for modes are:

   Evil State     |       Color
------------------|------------------
Normal            | Orange
Insert            | Green
Visual            | Grey
Emacs             | Red
Motion            | Purple

When [Flycheck][flycheck] minor mode is enabled, the mode line color changes
according to the current flycheck state:

![flycheck-error](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/flycheck-error.png)
![flycheck-warning](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/flycheck-warning.png)
![flycheck-info](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/doc/flycheck-info.png)

#### Toggles

Some UI indicators can be toggled on and off (toggles start with `t`):

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> t 8`       | display a mark on the 80th column
`<SPC> t F`       | toggle display of the fringe
`<SPC> t n`       | show the absolute line numbers

### Minor Modes

`Spacemacs` uses [diminish][diminish] mode to reduce the size of minor mode
indicators:

The minor mode area can be toggled on and off with:

    <SPC> t m

   Lighter   |                              Mode
-------------|-----------------------------------------------------------------
⊞            | [golden-ratio][golden-ratio] mode
Ⓐ            | [auto-complete][auto-complete] mode
Ⓗ            | [auto-highlight-symbol][auto-highlight] mode
Ⓒ            | [centered-cursor][centered-cursor] mode
eⓅ           | [e-project][e-project] mode
Ⓟ            | [projectile][projectile] mode
Ⓕ            | flymake mode
Ⓢ            | flyspell mode
(Ⓢ)          | [smartparens][sp] mode
(Ⓟ)          | paredit mode
Ⓨ            | [yasnippet][yasnippet] mode

**Note:** in terminal the regular indicators are used instead of the utf-8
ones.

### Line formatting

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

Projects in `Spacemacs` are managed with [projectile][projectile].
So projects are defined implicitly, for instance the root of a project
is found when a `.git` repository or `.projectile` file is encountered
in the file tree.

Projects management commands (start with `p`):

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> p b`       | switch to a buffer of the project 
`<SPC> p C`       | invalidate the cache of `projectile`
`<SPC> p d`       | open a `dired` buffer at the root of the project
`<SPC> p f`       | open a file of the project using `helm`
`<SPC> p F`       | find a file if the project using `ido`
`<SPC> p k`       | kill all the buffers of the project
`<SPC> p g`       | grep search in the project
`<SPC> p r`       | replace a string in the files of the project

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

#### Lisp

**TODO**

#### Magit

`Spacemacs` add `hjkl` navigation support for the following magit modes:
- branch manager
- commit
- log
- process
- status

**Note:** in `status` mode only `j` and `k` are remapped (to go down and up).
Press `K` instead of `k` to discard changes to an item.

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

## TODO list

- Add support for [multiple-cursors][multiple-cursors] mode.

## Thank you

Jokes aside, thank you Richard for this great piece of software.

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
[evil-plugin01]: https://github.com/tarao/evil-plugins
[evil-plugin02]: https://github.com/redguardtoo/evil-matchit
[evil-plugin03]: https://github.com/bling/evil-visualstar
[evil-plugin04]: https://github.com/timcharper/evil-surround
[evil-plugin05]: https://github.com/Dewdrops/evil-exchange
[vim-plugin01]: http://www.vim.org/scripts/script.php?script_id=1905
[vim-plugin02]: http://www.vim.org/scripts/script.php?script_id=39
[vim-plugin03]: http://www.vim.org/scripts/script.php?script_id=1697
[vim-plugin04]: https://github.com/tommcdo/vim-exchange
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
