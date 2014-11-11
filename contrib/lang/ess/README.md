# R (ESS) contribution layer for Spacemacs

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(defvar dotspacemacs-configuration-layers '(ess)
  "List of contribution to load."
)
```

## Key Bindings

**Important**:
In order to speed up the boot time of `Spacemacs`, `ESS` must be loaded
manually via the key binding:

    <SPC> e s s

### Inferior REPL process

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

### Other R commands

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> m p`       | object introspection popup [ess-R-object-popup][ess-R-object-popup]
`<SPC> m v p`     | view data under point using [ess-R-data-view][ess-R-data-view] 
`<SPC> m v t`     | view table using [ess-R-data-view][ess-R-data-view] 
