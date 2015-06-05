# Cscope contribution layer for Spacemacs

![logo](img/cscope.jpg)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Cscope contribution layer for Spacemacs](#cscope-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [Cscope](#cscope)
        - [PyCscope](#pycscope)
    - [Usage](#usage)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer provides bindings for using [Cscope](http://cscope.sourceforge.net) and
[PyCscope](https://github.com/portante/pycscope) in Spacemacs.
Cscope provides indexing and searching capabilities for C and C++ code. PyCscope
extends these capabilities for Python code as well.
See [here](https://github.com/OpenGrok/OpenGrok/wiki/Comparison-with-Similar-Tools)
for a comparison between Cscope and other similar tools (such as gtags).

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(cscope))
```

### Cscope

Install Cscope through your package manager, or download it from the
[website](http://cscope.sourceforge.net/#downloads) and build it from source.

From package manager (for example, Ubuntu):
```bash
sudo apt-get install cscope
```

From source:
```bash
tar xvf cscope-15.8b
cd cscope-15.8b
./configure
make
sudo make install
```

### PyCscope

Install PyCscope through pip:
```bash
pip install pycscope
```

## Usage

Before using any helm-cscope commands, remember to create a Cscope index file.
Do it by running the command `cscope-index-files` for C and C++ projects, or the
command `cscope/run-pycscope` for Python projects, bound to <kbd>SPC m g i</kbd>.

## Key bindings

    Key Binding      |              Description
---------------------|------------------------------------------------
<kbd>SPC m g c</kbd> | find which functions are called by a function
<kbd>SPC m g C</kbd> | find where a function is called
<kbd>SPC m g d</kbd> | find global definition of a symbol
<kbd>SPC m g e</kbd> | search regular expression
<kbd>SPC m g f</kbd> | find a file
<kbd>SPC m g F</kbd> | find which files include a file
<kbd>SPC m g i</kbd> | create Cscope index
<kbd>SPC m g r</kbd> | find references of a symbol
<kbd>SPC m g x</kbd> | search text
