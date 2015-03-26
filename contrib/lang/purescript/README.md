# Purescript contribution layer for Spacemacs

![logo](img/purescript-logo.png)

## Description

This layer provides basic Purescript editing support for spacemacs:
- purescript-mode
- psci

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(purescript))
```

## Key bindings

Key Binding      | Description
-----------------|------------------------------------------------------------
`<SPC> m p l`    | Equivalent of `:m /path/to/current/module/file.purs` - Load <file> for importing
`<SPC> m p i`    | Equivalent of `:i your.current.module.name` - Import <module> for use in PSCI
`<SPC> m p p r`  | Load or reload files defined in the project file .psci
`<SPC> m p r`    | Equivalent of `:r` - Reset
`<SPC> m p q`    | Equivalent of `:q` - Quit
`<SPC> m p p`    | Launch a psci console buffer
`<SPC> m i f`    | Format imports
`<SPC> m i a`    | Align imports
`<SPC> m i n`    | Navigate to the imports
`<SPC> m i r`    | Return to where you were editing before navigating to imports
