# R (ESS) contribution layer for Spacemacs

![logo](img/r.jpg)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [R (ESS) contribution layer for Spacemacs](#r-ess-contribution-layer-for-spacemacs)
    - [Install](#install)
    - [Key Bindings](#key-bindings)
        - [Inferior REPL process](#inferior-repl-process)
        - [Other R commands](#other-r-commands)

<!-- markdown-toc end -->

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(ess)
  "List of contribution to load."
)
```

## Key Bindings

**Important**:
In order to speed up the boot time of `Spacemacs`, `ESS` must be loaded
manually via the key binding:

    <SPC> e s s

### Inferior REPL process

Start an `R` inferior REPL process with <kbd>SPC m i</kbd>.

Send code to inferior process commands:

    Key Binding     |                 Description
--------------------|------------------------------------------------------------
<kbd>SPC m b</kbd>  | send buffer and keep code buffer focused
<kbd>SPC m B</kbd>  | send buffer and switch to REPL in insert mode
<kbd>SPC m d</kbd>  | send region or line and step (debug)
<kbd>SPC m D</kbd>  | send function or paragraph and step (debug)
<kbd>SPC m l</kbd>  | send line and keep code buffer focused
<kbd>SPC m L</kbd>  | send line and switch to REPL in insert mode
<kbd>SPC m r</kbd>  | send region and keep code buffer focused
<kbd>SPC m R</kbd>  | send region and switch to REPL in insert mode
<kbd>SPC m t</kbd>  | send thing at point (function) and keep code buffer focused
<kbd>SPC m T</kbd>  | send thing at point (function) and switch to REPL in insert mode
<kbd>CTRL+j</kbd>   | next item in REPL history
<kbd>CTRL+k</kbd>   | previous item in REPL history

### Other R commands

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m p</kbd>    | object introspection popup [ess-R-object-popup][ess-R-object-popup]
<kbd>SPC m v p</kbd>  | view data under point using [ess-R-data-view][ess-R-data-view] 
<kbd>SPC m v t</kbd>  | view table using [ess-R-data-view][ess-R-data-view] 
