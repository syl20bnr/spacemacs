# Helm Gtags contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Helm Gtags contribution layer for Spacemacs](#helm-gtags-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Features](#features)
    - [Install](#install)
        - [GNU Global](#gnu-global)
    - [Usage](#usage)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

`helm-gtags` and `ggtags` are clients for GNU Global. GNU Global is a source
code tagging system that allows querying symbol locations in source code, such
as definitions or references.

## Features

- Select any tag in a project retreived by gtags
- Resume previous helm-gtags sesssion
- Jump to a location based on context
- Find definitions
- Find references
- Present tags in current function only
- Create a tag databas
- Jump to definitions in file
- Show stack of visited locations
- Manually update tag database
- Jump to next location in context stack
- Jump to previous location in context stack
- Jump to a file in tag database

## Install

### GNU Global

You can install `helm-gtags` from the software repository of your OS. For example, in Ubuntu:

```shell-script
sudo apt-get install global
```

To use `helm-gtags`, you first have to install GNU Global: [Download link][gnu-global-download].

Download the latest tar.gz archive, then run these commands:

```shell-script
tar xvf global-6.4.tar.gz
cd global-6.4
./configure
make
sudo make install
```

[gnu-global-download]: https://www.gnu.org/software/global/download.html

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(gtags))
```

## Usage

Before using the helm-gtags, remember to create a GTAGS database by the following methods:

- From within Emacs, runs the command `helm-gtags-create-tags`, which is bound
  to `SPC m g c`. If the language is not directly supported by GNU Global, you
  can choose `ctags` or `pygment` as a backend to generate tag database.

- From inside terminal, runs gtags at your project root in terminal:

```shell-script
cd /path/to/project/root
gtags
```

If the language is not directly supported by `gtags` but `ctags`, use this command instead:

```shell-script
gtags --gtagslabel=ctags
```

### Eldoc integration

This layer also integrates `ggtags` for its Eldoc feature. That means, when
writing code, you can look at the minibuffer (at the bottom) and see variable
and function definition of the symbol the cursor is on. However, this feature is
only activated for programming modes that are not one of these languages:

- C mode
- C++ mode
- Common Lisp
- Emacs Lisp
- Python
- Ruby-mode

Since these modes have better Eldoc integration already.

## Key bindings

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m g s</kbd>  | select any tag in a project retreived by gtags
<kbd>SPC m g R</kbd>  | resume previous helm-gtags sesssion
<kbd>SPC m g g</kbd>  | jump to a location based on context
<kbd>SPC m g d</kbd>  | find definitions
<kbd>SPC m g r</kbd>  | find references
<kbd>SPC m g i</kbd>  | present tags in current function only
<kbd>SPC m g c</kbd>  | create a tag databas
<kbd>SPC m g l</kbd>  | jump to definitions in file
<kbd>SPC m g S</kbd>  | show stack of visited locations
<kbd>SPC m g u</kbd>  | manually update tag database
<kbd>SPC m g n</kbd>  | jump to next location in context stack
<kbd>SPC m g p</kbd>  | jump to previous location in context stack
<kbd>SPC m g f</kbd>  | jump to a file in tag database
