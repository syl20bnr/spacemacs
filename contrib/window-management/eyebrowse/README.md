# Eyebrowse contribution layer for Spacemacs

![logo](img/eyebrowse.gif) ![i3wm](img/i3wm.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Eyebrowse contribution layer for Spacemacs](#eyebrowse-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [Removing additional help](#removing-additional-help)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer adds [i3wm][] like workspaces thanks to the [eyebrowse][] package.

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(eyebrowse))
```

### Removing additional help

Once you know the key bindings to navigate between the workspaces you
may want to disable the exhaustive help in the workspace micro-state.
Set the variable `eyebrowse-display-help` to `nil`

```elisp
(setq-default dotspacemacs-configuration-layers
  '((eyebrowse :variables eyebrowse-display-help nil)))
```

## Key bindings

Key Binding                               | Description
------------------------------------------|-------------------------------------
<kbd>SPC W 1</kbd>                        | create or switch to workspace 1
<kbd>SPC W 2</kbd>                        | create or switch to workspace 2
<kbd>SPC W 3</kbd>                        | create or switch to workspace 3
<kbd>SPC W 4</kbd>                        | create or switch to workspace 4
<kbd>SPC W 5</kbd>                        | create or switch to workspace 5
<kbd>SPC W 6</kbd>                        | create or switch to workspace 6
<kbd>SPC W 7</kbd>                        | create or switch to workspace 7
<kbd>SPC W 8</kbd>                        | create or switch to workspace 8
<kbd>SPC W 9</kbd>                        | create or switch to workspace 9
<kbd>SPC W 0</kbd>                        | create or switch to workspace 0
<kbd>SPC W TAB</kbd>                      | switch to last active workspace
<kbd>SPC W c</kbd>                        | close current workspace
<kbd>SPC W n</kbd>                        | switch to next workspace
<kbd>SPC W N</kbd> or <kbd>SPC W p</kbd>  | create or switch to workspace 0

[i3wm]: https://i3wm.org/
[eyebrowse]: https://github.com/wasamasa/eyebrowse
