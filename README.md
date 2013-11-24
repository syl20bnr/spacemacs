# Vimacs

The best editor is not *Emacs* nor *Vim*, the best editor is
*Vimacs, Emacs+Vim* !

## Goal

These configuration files try to bring *the power of Vim modal editing* to
*the powerful Emacs editing platform* to finally "build" the *Vimacs editor*.

To achieve this, `Vimacs` uses two modes heavily:
- [evil mode][evil]
- [evil-leader mode][evil-leader]

## Who can benefit from this configuration files set ?

Vimacs is first intended to be used by Vim users who wants to go to the next
level by using Emacs.

It is also a good fit for people wanting to lower the [risks of RSI][RSI]
induced by the default Emacs key bindings. 

People wanting to learn a new way to edit files or wanting to learn Vim.

As a note side, if you are a programmer and you don't know Vim key bindings
yet, I deeply recommend you to learn the basics as recommended in
[Sacha Chua's one-page guide][sacha_guide] about how to learn Emacs.

## Install

Vimacs is only tested with `Emacs 24.3.1`.

Backup your current `.emacs.d`.
Clone this repository in your home folder:

    cd ~
    mv .emacs.d .emacs.bak
    git clone http://github.com/syl20bnr/vimacs .emacs.d

Get the submodules:

    cd ~/.emacs.d
    git submodule init
    git submodule update

## Packages and Extensions Organization

Modes/libraries are separated into two categories:
- package: packages installed from `package.el` compliant repositories.
- extension: modes directly fetched from git repositories (as submodules)

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

With Vimacs there is no need to remap your keyboard modifiers to lower the
risks of RSI, every command can be executed very easily  while you are in
`normal` mode:

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
Vimacs you have to press `fd` quickly by default.

`fd` also works to quit minibuffer prompts.

Note that [keychords.el][keychords] mode is not used to achieve this, the
reasons for this is latency and the fact that keychords wants you to press
several keys almost at the same time which is something very difficult to master
correctly on a keyboard. Vimacs has a special function called `fd-trigger` to
handle the `fd` key sequence and fix the above keychords issues.

## How to use it ?

Every sequences must be performed in `normal` mode.

### Executing Vim and Emacs commands

Vim commands are execute as usual with the `:` key.
To execute an Emacs command:

    <SPC> :

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

#### [ace-jump mode][ace-jump]

I often use `ace-jump` mode so this key binding does not require the evil
leader, just press:

    ,

To go back to the previous location (pop mark):

    <SPC> ,

### Navigation (buffers/files)

Vimacs uses [helm][helm] coupled to [popwin][popwin] to handle buffer
navigation.

Note: Vimacs also uses `ido` for opening files since `ido` way to navigate
the file system is better than `helm` (especially because `ido` can
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

To swap two letters/words/lines:

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

Vimacs uses this [Solarized theme][solarized-theme].
It is possible to switch between the light and dark themes.

Activate daylight theme:

    <SPC> t t d

Activate night theme:

    <SPC> t t n

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

Vimacs uses [diminish][diminish] mode to reduce the size of minor mode
indicators:

- Ⓐ -> [auto-complete][auto-complete] mode
- Ⓗ -> [auto-highlight-symbol][auto-highlight-mode] mode
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

Vimacs leverage `paredit` in all major modes by using [smartparens][sp] mode.

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

Vimacs uses [Flycheck][flycheck] to gives error feedback on the fly.
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

See `my-keybindings.el` for more keybindings.

## TODO list

- Add a way to easily share/activate/deactivate sets of key bindings for major
or minor modes key bindings (maybe by leveraging [use-package][use-package] ?).
- Add support for [multiple-cursors][multiple-cursors] mode.

## Are there any other configuration files like Vimacs ?

Yes there is:
- [Bin Chen's configuration files][bin_chen_dotemacs]

Let me now if you know other great Vimacs configurations and I'll list them
here.


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
[bin_chen_dotemacs]: https://github.com/redguardtoo/emacs.d
