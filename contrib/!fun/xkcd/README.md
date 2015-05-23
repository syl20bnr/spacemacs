# xkcd contribution layer for Spacemacs

![logo_xkcd](img/xkcd.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [xkcd contribution layer for Spacemacs](#xkcd-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key Bindings](#key-bindings)
        - [Tell me how to spawn the xkcd greatness!!](#tell-me-how-to-spawn-the-xkcd-greatness)
        - [Okay, what now](#okay-what-now)

<!-- markdown-toc end -->

## Description

This layer adds a [xkcd][] navigation mode using [emacs-xkcd][].

Features:
- Load a random xkcd
- Show the text in the modeline
- Open explanation and current comic in browser
- Cache the comics in `.cache/xkcd`

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(xkcd))
```

## Key Bindings

### Tell me how to spawn the xkcd greatness!!

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC a x</kbd>    | Open xkcd mode

### Okay, what now

You can now move through the comics with these

    Key Binding               |                 Description
------------------------------|------------------------------------------------------------
<kbd>e</kbd>                  | Open explanation in the browser
<kbd>j</kbd> or <kbd>l</kbd>  | Next comic
<kbd>k</kbd> or <kbd>h</kbd>  | Previous comic
<kbd>o</kbd>                  | Open the browser
<kbd>q</kbd>                  | Quit the buffer
<kbd>r</kbd>                  | Random comic
<kbd>t</kbd>                  | Show alternate text in the modeline

[xkcd]: http://xkcd.com/
[emacs-xkcd]: https://github.com/vibhavp/emacs-xkcd
