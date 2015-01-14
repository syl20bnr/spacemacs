# Lua contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Lua contribution layer for Spacemacs](#lua-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key Bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer adds support for editing Lua.

Features:
- Editing lua files using [lua-mode][]
- Live evaluation of buffer contents in lua interpreter

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(lua)
  "List of contribution to load."
)
```

## Key Bindings

### commands

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC m d</kbd>   | lookup thing at point in lua documentation
<kbd>SPC m l</kbd>   | evaluate buffer contents

[lua-mode]: https://github.com/immerrr/lua-mode 
