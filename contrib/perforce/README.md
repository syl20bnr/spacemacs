# Perforce contribution layer for Spacemacs

![logo](img/p4.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Perforce contribution layer for Spacemacs](#perforce-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer adds support for [Perforce][] (p4).

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(perforce)
  "List of contribution to load."
)
```

You'll have to install the `p4`` command line, [download page][].

Don't forget to setup the environment variables:
- `P4_PORT`
- `P4_CLIENT`
- `P4_USER`
- `P4_PASSWD`

## Key bindings

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC p 4 a</kbd> | add a file in depot
<kbd>SPC p 4 d</kbd> | delete a file in depot
<kbd>SPC p 4 D</kbd> | p4-describe
<kbd>SPC p 4 e</kbd> | checkout a file
<kbd>SPC p 4 r</kbd> | rename a file
<kbd>SPC p 4 R</kbd> | revert a file
<kbd>SPC p 4 S</kbd> | submit CL

[Perforce]: http://www.perforce.com/
[download page]: http://www.perforce.com/downloads
