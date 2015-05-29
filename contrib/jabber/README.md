<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Jabber contribution layer for Spacemacs](#jabber-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)
        - [-](#-)

<!-- markdown-toc end -->
# Jabber contribution layer for Spacemacs

![logo](img/jabber-logo.gif)


## Description

This layer adds keybindings for jabber.el. jabber.el is a Jabber client for Emacs

## Install

To use this contribution layer add it to your `~/.spacemacs`

```elisp
(set-default dotspacemacs-configuration-layers '(jabber))
```

## Key bindings

Key Binding         | Description
--------------------|-------------------------------
<kbd>SPC a j </kbd> | Connect all accounts

#### Jabber Roster

Key Binding             | Description
------------------------|--------------------------------
<kbd>SPC J a</kbd>      | Jabber send presence
<kbd>SPC J b</kbd>      | Jabber get browse
<kbd>SPC J d</kbd>      | Jabber disconnect
<kbd>SPC J e</kbd>      | Jabber roster edit action at point
<kbd>SPC J g</kbd>      | Jabber display roster
<kbd>SPC J i</kbd>      | Jabber get disco items
<kbd>SPC J j</kbd>      | Jabber muc join
<kbd>SPC J o</kbd>      | Jabber roster toggle offline display
<kbd>SPC J q</kbd>      | bury buffer
<kbd>SPC J s</kbd>      | Jabber send subscription request
<kbd>SPC J v</kbd>      | Jabber get version
<kbd>SPC J RET</kbd>  | Jabber roster ret action at point
