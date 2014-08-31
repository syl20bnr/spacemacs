# Spacemacs

_The best editor is not *Emacs* nor *Vim*, the best editor is
*Emacs+Vim* !_

**Table of Contents**  *generated with [DocToc](http://doctoc.herokuapp.com/)*

- [Goal](#goal)
- [Who can benefit from this ?](#who-can-benefit-from-this-)
- [Install](#install)
- [Packages and Extensions Organization](#packages-and-extensions-organization)
- [How it works](#how-it-works)
- [How to switch to normal mode efficiently ?](#how-to-switch-to-normal-mode-efficiently-)
- [Evil plugins](#evil-plugins)
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
*the powerful Emacs editing platform*.

To achieve this, `Spacemacs` uses two modes heavily:
- [evil mode][evil]
- [evil-leader mode][evil-leader]


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

## Install

`Spacemacs` is only tested with `Emacs 24.3.1`.

    cd ~
    mv .emacs.d .emacs.bak
    git clone http://github.com/syl20bnr/spacemacs .emacs.d
    cd .emacs.d
    mkdir .desktop

Get the sub-modules:

    cd ~/.emacs.d
    git submodule init
    git submodule update

## Packages and Extensions Organization

Modes/libraries are separated into two categories:
- **package:** packages installed from `package.el` compliant repositories.
- **extension:** modes directly fetched from git repositories (as sub-modules)

There are two types of extensions:
- **pre-extensions:** loaded before packages
- **post-extensions:** loaded after packages

Modes/libraries are listed in the following three files depending whether they
are packages, pre-extensions or post-extensions, in order of loading:
- pre-extensions.el 
- packages.el
- post-extensions.el

A package or extension may have a corresponding `init-....el` file responsible
for its initialization, those files are stored respectively in the
`init-package` and `init-extension` directories.

`init-....el` files lazy load the modes by using the [use-package][use-package]
macro.

## How it works

`Spacemacs` uses the `evil` mode to emulate Vim key bindings. It is a very
complete emulation (the most complete I've seen yet).

`Spacemacs` heavily uses the `evil-leader` mode which brings the Vim leader key
to the Emacs world.

This leader key is commonly set to `,` by Vim users, in `Spacemacs` the leader
key is set on `SPC` (space bar, this is why the name `spacemacs`). This key is
the most accessible key on a keyboard and it is pressed with the thumb which is
a good choice to lower the risk of [RSI][RSI].

So with `Spacemacs` there is no need to remap your keyboard modifiers to attempt to
reduce the risk of RSI, every command can be executed very easily while you are
in `normal` mode by pressing the `SPC` leader key, here are a few examples:

- Save a buffer: `<SPC> f s`
- Save all opened buffers: `<SPC> f S`
- Open (switch) to a buffer with `helm`: `<SPC> b s`

## How to switch to normal mode efficiently ?

One of the main design flaw in Vim key bindings is that you often have to press
the `ESC` key to return to `normal` mode and `ESC` key is very far from the
home row.

The popular way to avoid this is to replace `ESC` by `jj` pressed rapidly. In
`Spacemacs` you have to press `fd` quickly by default.

`fd` also works to quit minibuffer prompts.

**Note:** For those who know about [keychords.el][keychords] mode. This mode is
not used to achieve this, the reasons for this is latency and the fact that
keychords wants you to press several keys almost at the same time which is
something very difficult to master correctly on a keyboard. `Spacemacs` has a
special function called `fd-trigger` to handle the `fd` key sequence and fix
the above keychords issues.

## Evil plugins

`Spacemacs` ships with the following evil plugins:

                 Mode                   |                          Description
----------------------------------------|---------------------------------------------------------
[evil-leader][evil-leader]              | vim leader that bring a new layer of keys in normal mode
[evil-little-word][evil-plugin01]       | port of [camelcasemotion.vim][vim-plugin01]
[evil-operator-comment][evil-plugin01]  | comment/uncomment with `CC`
[evil-matchit][evil-plugin02]           | port of [matchit.vim][vim-plugin02]
[evil-visualstar][evil-plugin03]        | search for current selection with `*`
[evil-exchange][evil-plugin05]          | port of [vim-exchange][vim-plugin04]
[surround][evil-plugin04]               | port of [surround.vim][vim-plugin03]

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

Mode                                    | Key Binding | Description
----------------------------------------|-------------|---------------------------------------------------------
[helm-swoop][hswoop]                    | `<SPC> h s` | search for occurrences within a file and edit the result
[helm-css-scss][hcss]                   | `<SPC> h c` | for quick navigation in CSS
[helm-c-yasnippet][hyas]                | `<SPC> a y` | select snippets
[helm-themes][hthemes]                  | `<SPC> h t` | select a theme
[helm-projectile][projectile]           | `<SPC> p f` | select files within a projectile project
[helm-descbinds][hdescbinds]            | `<SPC> ?`   | show key bindings
[cofi/helm-flyspell-correct][hflyspell] | `<SPC> s c` | choose a corrected word

### Navigation (point/cursor)

Navigation is performed using the Vi key bindings `hjkl`.

Key Binding |                              Description
------------|----------------------------------------------------------------------------------
`h`         | move cursor left
`j`         | move cursor down
`k`         | move cursor up
`l`         | move cursor right
`H`         | move quickly up (10 lines at a time)
`L`         | move quickly down (10 lines at a time)
`<SPC> j h` | go to the beginning of line (and set a mark at the previous location in the line)
`<SPC> j l` | go to the end of line (and set a mark at the previous location in the line)
`<SPC> z z` | lock the cursor at the center of the screen 

#### [ace-jump mode][ace-jump]

Key Binding |                              Description
------------|----------------------------------------------------------------------------------
`,`         | initiate ace jump
`<SPC> ,`   | go back to the previous location (before the jump)

### Navigation (buffers/files)

`Spacemacs` uses `ido` for opening files since `ido` way to navigate
the file system is better than `helm` in my opinion (especially because `ido` can
remember the last selected directories and buffers, maybe helm can do this ?).
`ido` is also used to kill buffers.

Buffer manipulation commands (start with `b`):

Key Binding   |                              Description
--------------|-----------------------------------------------------------------------
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

Key Binding   |                              Description
--------------|-----------------------------------------------------------------------
`<SPC> f f`   | open a file using `ido`
`<SPC> f i`   | open your `init.el` file
`<SPC> f s`   | save a file
`<SPC> f S`   | save all files
`<SPC> f y`   | show current file absolute path in the mini buffer

### Window manipulation

Split windows are dynamically resized depending on whether they are selected or
not. Resizing is performed by the [golden-ratio][golden-ratio] mode.

Every window has a number displayed at the start of the mode-line and can
be quickly accessed using `<SPC> number`.

Key Binding   |                    Description
--------------|---------------------------------------------------
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

Key Binding   |                              Description
--------------|-----------------------------------------------------------------------
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

### Additional text manipulation commands

Text related commands (start with `x`):

    Key Binding   |                              Description
------------------|-----------------------------------------------------------------------
`<SPC> x +`       | increase text font size
`<SPC> x -`       | decrease text font size
`<SPC> x =`       | reset text font size
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
[evil-plugin01]: https://github.com/tarao/evil-plugins
[evil-plugin02]: https://github.com/redguardtoo/evil-matchit
[evil-plugin03]: https://github.com/bling/evil-visualstar
[evil-plugin04]: https://github.com/timcharper/evil-surround
[evil-plugin05]: https://github.com/Dewdrops/evil-exchange
[vim-plugin01]: http://www.vim.org/scripts/script.php?script_id=1905
[vim-plugin02]: http://www.vim.org/scripts/script.php?script_id=39
[vim-plugin03]: http://www.vim.org/scripts/script.php?script_id=1697
[vim-plugin04]: https://github.com/tommcdo/vim-exchange

