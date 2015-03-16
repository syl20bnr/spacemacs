# Floobits integration layer for Spacemacs

![floobits](img/floobits.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Floobits integration layer for Spacemacs](#floobits-integration-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
    - [Key Bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer adds support for peer programming tool [floobits][].

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(floobits))
```

## Key Bindings

    Key Binding            |                 Description
---------------------------|------------------------------------------------------------
<kbd>SPC P c</kbd>         | Clears all mirrored highlights.
<kbd>SPC P d</kbd>         | Load the .floorc.json file for floobits configuration.
<kbd>SPC P f</kbd>         | Follow a users changes. This also toggles follow mode.
<kbd>SPC P j</kbd>         | Join an existing floobits workspace.
<kbd>SPC P l</kbd>         | Leave the current workspace.
<kbd>SPC P R</kbd>         | Create a workspace and populate it with the contents of the directory, DIR (or make it).
<kbd>SPC P s</kbd>         | Summon everyone in the workspace to your cursor position. 
<kbd>SPC P t</kbd>         | Toggle following of recent changes.
<kbd>SPC P U</kbd>         | Create a workspace and populate it with the contents of the directory, DIR (or make it).

[floobits]: https://github.com/Floobits/floobits-emacs
