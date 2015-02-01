# geeknote contribution layer for Spacemacs

## Description

Geeknote is a command-line utility that gives access to Evernote
functionality via the terminal. It allows users to write notes
in markdown, and sync it to Evernote.

geeknote.el is a wrapper for some of the most used geeknote commands.
By default, geeknote.el doesn't have key bindings defined. This
contribution layer provides key bindings for all of geeknote.el's
exposed features.

## Install

The command `geeknote` is expected to be present in your `$PATH`. To
obtain this utility, please refer to the official geeknote
[documentation](http://www.geeknote.me/documentation/).

geeknote.el relies on having a correctly setup geeknote editor. To set
this up, run the following command in your terminal after successfully
installing geeknote:

```
$ geeknote settings --editor "emacsclient"
```

If you would prefer to customize the geeknote command to be used
such as specifying the path to the geeknote python script, please
refer to the geeknote.el [documentation](https://github.com/avendael/emacs-geeknote).

Finally, add this configuration layour to your `~/.spacemacs`:

```elisp
(setq-default dotspacemacs-configuration-layers '(geeknote)
  "List of contribution to load."
)
```

## Key Bindings

Key Binding           |                 Description
----------------------|------------------------------------------------
<kbd>SPC a g c</kbd>  | create a new note
<kbd>SPC a g e</kbd>  | edit an existing note
<kbd>SPC a g f</kbd>  | find a note using a keyword
<kbd>SPC a g s</kbd>  | show an existing note
<kbd>SPC a g r</kbd>  | remove an existing note
