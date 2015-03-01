# Evernote contribution layer for Spacemacs

![evernote](img/evernote.png) _with_ ![geeknote](img/geeknote.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Evernote contribution layer for Spacemacs](#evernote-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [geeknote](#geeknote)
        - [geeknote.el](#geeknoteel)
    - [Key Bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer group together packages to work with [Evernote][].

It uses the non official Evernote command line [geeknote][] which allows
users to write notes in markdown, and sync them.

[geeknote.el][] is a wrapper for some of the most used `geeknote` commands.
By default, `geeknote.el` doesn't have key bindings defined. This
contribution layer provides key bindings for all of geeknote.el's
exposed features.

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(evernote))
```

### geeknote

The command `geeknote` is expected to be present in your `$PATH`. To
obtain this utility, please refer to the official geeknote
[documentation][geeknote-doc].

### geeknote.el

`geeknote.el` relies on having a correctly setup geeknote editor. To set
this up, run the following command in your terminal after successfully
installing `geeknote`:

```
$ geeknote settings --editor "emacsclient"
```

If you would prefer to customize the geeknote command to be used
such as specifying the path to the geeknote python script, please
refer to the `geeknote.el` [documentation][geeknote.el].

## Key Bindings

Key Binding           |                 Description
----------------------|------------------------------------------------
<kbd>SPC a e c</kbd>  | create a new note
<kbd>SPC a e e</kbd>  | edit an existing note
<kbd>SPC a e f</kbd>  | find a note using a keyword
<kbd>SPC a e s</kbd>  | show an existing note
<kbd>SPC a e r</kbd>  | remove an existing note
<kbd>SPC a e m</kbd>  | move a note to a different notebook

[Evernote]: https://evernote.com/
[geeknote]: http://www.geeknote.me
[geeknote-doc]: http://www.geeknote.me/documentation/
[geeknote.el]: https://github.com/avendael/emacs-geeknote
