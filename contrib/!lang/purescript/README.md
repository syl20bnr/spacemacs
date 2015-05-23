# Purescript contribution layer for Spacemacs

![logo](img/purescript-logo.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Purescript contribution layer for Spacemacs](#purescript-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer provides basic Purescript editing support for spacemacs
thanks to the following packages:
- [purescript-mode][]
- [psci][]

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(purescript))
```

## Key bindings

### Purescript

#### Imports

Key Binding      | Description
-----------------|------------------------------------------------------------
`<SPC> m i =`    | Format imports
`<SPC> m i \``   | Return to where you were editing before navigating to imports
`<SPC> m i a`    | Align imports
`<SPC> m i n`    | Navigate to the imports

### REPL

[psci][] provides a very basic REPL for purescript. The following key
bindings are available:

Key Binding      | Description
-----------------|------------------------------------------------------------
`<SPC> m s b`    | Equivalent of `:m /path/to/current/module/file.purs` - Load <file> for importing
`<SPC> m s i`    | Launch a psci console buffer
`<SPC> m s m`    | Equivalent of `:i your.current.module.name` - Import <module> for use in PSCI
`<SPC> m s p`    | Load or reload files defined in the project file .psci

[purescript-mode]: https://github.com/dysinger/purescript-mode
[psci]: https://github.com/ardumont/emacs-psci
