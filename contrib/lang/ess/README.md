# R (ESS) contribution layer for Spacemacs

![logo](img/r.jpg)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [R (ESS) contribution layer for Spacemacs](#r-ess-contribution-layer-for-spacemacs)
    - [Install](#install)
    - [Key Bindings](#key-bindings)
        - [Inferior REPL process](#inferior-repl-process)
        - [Helpers](#helpers)
    - [Options](#options)

<!-- markdown-toc end -->

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(ess))
```

**Important**:
In order to speed up the boot time of `Spacemacs`, `ESS` must be loaded
manually via the key binding: <kbd>SPC e s s</kbd>

## Key Bindings

### Inferior REPL process

Start an `R` inferior REPL process with <kbd>SPC m s i</kbd>.

Send code to inferior process commands:

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC m c c</kbd> | send knitr/sweave chunk and keep buffer focused
<kbd>SPC m c C</kbd> | send knitr/sweave chunk and switch to REPL in insert mode
<kbd>SPC m c d</kbd> | send knitr/sweave chunk and step to next chunk
<kbd>SPC m c m</kbd> | mark knitr/sweave chunk around point
<kbd>SPC m c n</kbd> | next knitr/sweave chunk
<kbd>SPC m c N</kbd> | previous knitr/sweave chunk
<kbd>SPC m s b</kbd> | send buffer and keep code buffer focused
<kbd>SPC m s B</kbd> | send buffer and switch to REPL in insert mode
<kbd>SPC m s d</kbd> | send region or line and step (debug)
<kbd>SPC m s D</kbd> | send function or paragraph and step (debug)
<kbd>SPC m s i</kbd> | start an inferior REPL process
<kbd>SPC m s l</kbd> | send line and keep code buffer focused
<kbd>SPC m s L</kbd> | send line and switch to REPL in insert mode
<kbd>SPC m s r</kbd> | send region and keep code buffer focused
<kbd>SPC m s R</kbd> | send region and switch to REPL in insert mode
<kbd>SPC m s t</kbd> | send thing at point (function) and keep code buffer focused
<kbd>SPC m s T</kbd> | send thing at point (function) and switch to REPL in insert mode
<kbd>CTRL+j</kbd>    | next item in REPL history
<kbd>CTRL+k</kbd>    | previous item in REPL history

### Helpers

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m h d</kbd>  | view data under point using [ess-R-data-view][ess-R-data-view] 
<kbd>SPC m h i</kbd>  | object introspection popup [ess-R-object-popup][ess-R-object-popup]
<kbd>SPC m h t</kbd>  | view table using [ess-R-data-view][ess-R-data-view] 

## Options

`ess-smart-equals` is enabled by default. In order to disable it, set in your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '((ess :variables
                                                       ess-enable-smart-equals nil)))
```
