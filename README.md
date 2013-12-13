# Spacemacs

_The best editor is not *Emacs* nor *Vim*, the best editor is
*Emacs+Vim* !_

**Table of Contents**  *generated with [DocToc](http://doctoc.herokuapp.com/)*

- [Goal](#goal)
- [Who can benefit from this configuration files set ?](#who-can-benefit-from-this-configuration-files-set-)
- [Install](#install)
- [Packages and Extensions Organization](#packages-and-extensions-organization)
- [How it works](#how-it-works)
- [How to switch to normal mode efficiently ?](#how-to-switch-to-normal-mode-efficiently-)
- [How to use it ?](#how-to-use-it-)
    - [Executing Vim and Emacs commands](#executing-vim-and-emacs-commands)
    - [Key bindings help](#key-bindings-help)
    - [About helm](#about-helm)
    - [Navigation (point/cursor)](#navigation-pointcursor)
        - [ace-jump mode](#ace-jump-mode)
    - [Navigation (buffers/files)](#navigation-buffersfiles)
    - [Window manipulation](#window-manipulation)
    - [Additional text manipulation commands](#additional-text-manipulation-commands)
    - [Spell checking](#spell-checking)
    - [Color theme](#color-theme)
    - [UI elements](#ui-elements)
    - [Minor Modes](#minor-modes)
    - [Formatting](#formatting)
    - [Errors handling](#errors-handling)
    - [Various](#various)
        - [expand-region mode](#expand-region-mode)
        - [narrow region](#narrow-region)
        - [auto-highlight-symbol mode](#auto-highlight-symbol-mode)
        - [and more...](#and-more)
- [TODO list](#todo-list)

## Goal

These configuration files try to bring *the power of Vim modal editing* to
*the powerful Emacs editing platform* and some more power with the `SPC` key
as a leader key.

To achieve this, `Spacemacs` uses two modes heavily:
- [evil mode][evil]
- [evil-leader mode][evil-leader]


## Who can benefit from this configuration files set ?

`Spacemacs` is first intended to be used by Vim users who want to go to the
next level by using Emacs.

It is also a good fit for people wanting to lower the [risk of RSI][RSI]
induced by the default Emacs key bindings. 

People wanting to learn a new way to edit files or wanting to learn Vim
key bindings.

As a note side, if you are a programmer and you don't know Vim key bindings
yet, I deeply recommend you to learn the basics as recommended in
[Sacha Chua's one-page guide][sacha_guide] about how to learn Emacs.

## Install

`Spacemacs` is only tested with `Emacs 24.3.1`.

Backup your current `.emacs.d`.
Clone this repository in your home folder:

    cd ~
    mv .emacs.d .emacs.bak
    git clone http://github.com/syl20bnr/spacemacs .emacs.d

Get the sub-modules:

    cd ~/.emacs.d
    git submodule init
    git submodule update

## Packages and Extensions Organization

Modes/libraries are separated into two categories:
- package: packages installed from `package.el` compliant repositories.
- extension: modes directly fetched from git repositories (as sub-modules)

There are two types of extensions:
- pre-extensions: loaded before packages
- post-extensions: loaded after packages

Modes/libraries are listed in the following three files depending whether they
are packages, pre-extensions or post-extensions:
- packages.el
- pre-extensions.el 
- post-extensions.el

A package or extension has a corresponding `init-....el` file responsible for
its initialization, those files are stored respectively in the `init-package`
and `init-extension` directories.

`init-....el` files lazy load the modes by using the [use-package][use-package]
macro.

## How it works

`Spacemacs` uses the `evil` mode to emulate Vim key bindings. This is a very
complete emulation (the most complete I've seen yet).

`Spacemacs` heavily uses the `evil-leader` mode which brings the Vim leader key to
the Emacs world.

This leader key is commonly set to `,` by Vim users, in `Spacemacs` the leader key
is set on `SPC` (space bar, this is why `spacemacs`). This key is the most
accessible key on a keyboard and it is pressed with the thumb which is a good
choice to lower the risk of [RSI][RSI].

So with `Spacemacs` there is no need to remap your keyboard modifiers to attempt to
reduce the risk of RSI, every command can be executed very easily while you are
in `normal` mode by pressing the `SPC` leader key, here are a few examples:

Save a buffer:

    <SPC> f s

Save all opened buffers:

    <SPC> f S

Open (switch) to a buffer with `helm`:

    <SPC> b s

## How to switch to normal mode efficiently ?

One of the main design flaw in Vim key bindings is that you often have to press
the `ESC` key to return to `normal` mode and `ESC` key is very far from the
home row.

The popular way to avoid this is to replace `ESC` by `jj` pressed rapidly. In
`Spacemacs` you have to press `fd` quickly by default.

`fd` also works to quit minibuffer prompts.

Note that [keychords.el][keychords] mode is not used to achieve this, the
reasons for this is latency and the fact that keychords wants you to press
several keys almost at the same time which is something very difficult to master
correctly on a keyboard. `Spacemacs` has a special function called `fd-trigger` to
handle the `fd` key sequence and fix the above keychords issues.

## How to use it ?

Every sequences must be performed in `normal` mode.

### Executing Vim and Emacs commands

Vim commands are execute as usual with the `:` key.
To execute an Emacs command:

    <SPC> :

### Key bindings help

A list of all the key bindings can be accessed by pressing:

    <SPC> ?

To narrow the list to `Spacemacs` specific key bindings set the pattern to `SPC`

### About helm

`Spacemacs` tries to use [helm][helm] as much as possible.
[helm][helm] is coupled to [popwin][popwin] so `helm` window always appears in
a new temporary windows at the bottom.

The following `helm` modes are installed with `Spacemacs`:
- [helm-swoop][hswoop] (<SPC> h s): A very cool mode to search for occurrences
within a file and edit the result.
- [helm-css-scss][hcss] (<SPC> h c): for quick navigation in CSS.
- [helm-c-yasnippet][hyas] (<SPC> a y): select snippets.
- [helm-themes][hthemes] (<SPC> h t): select a theme
- [helm-projectile][projectile] (<SPC> p f): select files within a projectile
project.
- [helm-descbinds][hdescbinds] (<SPC> ?): show key bindings.
- [cofi/helm-flyspell-correct][hflyspell] (<SPC> s c): choose a corrected word.

### Navigation (point/cursor)

Navigation is performed using the Vi key bindings `hjkl`.

The current line of the cursor is always at the center of the buffer.
This effect is achieved with [centered-cursor][centered-cursor] mode.
It can be toggled on and off with:

    <SPC> z z

To move quickly up a buffer:

    H

To move quickly down a buffer:

    L

Go to beginning/end of line:

    <SPC> j h
    <SPC> j l

#### [ace-jump mode][ace-jump]

I often use `ace-jump` mode so this key binding does not require the evil
leader, just press:

    ,

To go back to the previous location (pop mark):

    <SPC> ,

### Navigation (buffers/files)

`Spacemacs` uses `ido` for opening files since `ido` way to navigate
the file system is better than `helm` in my opinion (especially because `ido` can
remember the last selected directories and buffers, maybe helm can do this ?).
`ido` is also used to kill buffers.

Buffer manipulation commands start with `b` and file manipulation with `f`.

Open a file:

    <SPC> f f

Save a file:

    <SPC> f s

Switch to a buffer:

    <SPC> b s

Kill a buffer:

    <SPC> b k

Kill all other buffer:

    <SPC> b K

Rename a buffer:

    <SPC> b r

Delete a buffer (well a file...):

    <SPC> b d

Move a buffer to the left:

    <SPC> b m h

### Window manipulation

Window manipulation commands start with `w`.
Split windows are dynamically resized depending on whether they are selected or
not. Resizing is performed by the [golden-ratio][golden-ratio] mode.

Every window has a number displayed at the start of the mode-line and can
be accessed using `<SPC> number`. For instance to go to the window number 2,
you can press:

    <SPC> 2

Split a window horizontally:

    <SPC> w b

Split a window vertically:

    <SPC> w v

Close a window:

    <SPC> w c

Undo close window:

    <SPC> w u

Toggle window dedication (dedicated window cannot be used by a mode):

    <SPC> w d

Rotate windows clockwise or counter-clockwise:

    <SPC> w r
    <SPC> w R

Maximize/minimize a window:

    <SPC> w m

### Additional text manipulation commands

Text related commands start with `x`.

To move a line of text up or down:

    <SPC> x m k
    <SPC> x m j

To swap (transpose) two letters/words/lines:

    <SPC> x t c
    <SPC> x t w
    <SPC> x t l

To make the selected text upper case or lower case:

    <SPC> x U
    <SPC> x u

To translate a word at point with `google translate`:

    <SPC> x g t


### Spell checking

Spell checking commands start with `s`.

Correct word at point with `helm`:

    <SPC> s c

Go to the next spell check error:

    <SPC> s n

Change dictionary language:

    <SPC> s d

### Color theme

`Spacemacs` uses this [Solarized theme][solarized-theme].
It is possible to cycle between the light and dark themes with.

    <SPC> c t

### UI elements

The mode line is a [powerline][powerline] customized to show the window
number and to colorize the current editing mode.

Some UI indicators can be toggled on and off:

Fill column indicator for 80 columns wide buffers:

    <SPC> t 8

I suggest to not always display it since it slow down Emacs.

Toggle line numbers:

    <SPC> t n

Idem, I suggest to display this only if required for performance reason.

Toggle fringe mode:

    <SPC> t f

### Minor Modes

`Spacemacs` uses [diminish][diminish] mode to reduce the size of minor mode
indicators:

- Ⓐ -> [auto-complete][auto-complete] mode
- Ⓗ -> [auto-highlight-symbol][auto-highlight] mode
- Ⓒ -> [centered-cursor][centered-cursor] mode
- eⓅ -> [e-project][e-project] mode
- Ⓟ -> [projectile][projectile] mode
- Ⓕ -> flymake mode
- Ⓢ -> flyspell mode
- (Ⓢ) -> [smartparens][sp] mode
- (Ⓟ) -> paredit mode
- Ⓨ -> [yasnippet][yasnippet] mode

The minor mode area can be toggled on and off with:

    <SPC> t m

Note that in terminal the regular indicators are used instead of the utf-8
ones.

### Formatting

`Spacemacs` leverage `paredit` in all major modes by using [smartparens][sp] mode.

To join the current line with the next line:

    <SPC> j k

To split the current line at point and auto-indent:

    <SPC> j j

To auto-indent the line below the current line and jump to it:

    <SPC> j i

`<SPC> j k`, `<SPC> j j` and `<SPC> j i` used together are very powerful.

To split a quoted string (ie. `"Hello Emacs Hello Vim"` to `"Hello Emacs"` and
`"Hello Vim"`, but nobody wants to do that!):

    <SPC> l CTRL+j

Ahah a modifier! It should be the only one.

### Errors handling

`Spacemacs` uses [Flycheck][flycheck] to gives error feedback on the fly.
The checks are only performed at save time by default.

To go to the next/previous flycheck error:

    <SPC> f n
    <SPC> f p

To display the list of errors/warnings:

    <SPC> f l

Could be great to turn this command into a toggle.
By the way `l` is for `lisp`.

### Various

#### [expand-region][expand-region] mode

initiate expand-region with:

    <SPC> v

to expand region:

    v

to contract:

    V

to reset:

    r

#### narrow region

narrow to a region:

    <SPC> n r

widen a region:

    <SPC> n w


#### [auto-highlight-symbol][auto-highlight] mode

toggle the mode:

    <SPC> t h

edit all highlighted symbols:

    <SPC> h e

#### and more...

There are sets of key bindings for different major modes I'm using, the
convention is to start the major mode key sequences by `M`.

See [my-keybindings.el][keybindings] to explore for more key bindings.

## TODO list

- Add a way to easily share/activate/deactivate sets of key bindings for major
or minor modes key bindings (maybe by leveraging [use-package][use-package] ?).
- Add support for [multiple-cursors][multiple-cursors] mode.

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
