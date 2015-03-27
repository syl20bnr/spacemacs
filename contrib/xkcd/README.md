# xkcd contribution layer for Spacemacs
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [xkcd contribution layer for Spacemacs](#xkcd-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key Bindings](#key-bindings)
        - [Tell me how to spawn the xkcd greatness!!](#tell-me-how-to-spawn-the-xkcd-greatness)
        - [Okay, what now](#okay-what-now)

<!-- markdown-toc end -->

![logo_xkcd](img/xkcd.png)


## Description

This layer adds a XKCD navigation mode. (it just enables [this package](https://github.com/vibhavp/emacs-xkcd))

Features:
- Load a random xkcd
- Show the text in the modeline
- Open explanation and current comic in browser
- Cache the comics in .cache/xkcd

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

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>j</kbd>          | Next comic
<kbd>k</kbd>          | Previous comic
<kbd>r</kbd>          | Random comic
<kbd>t</kbd>          | Show alternate text in the modeline
<kbd>o</kbd>          | Open the browser
<kbd>e</kbd>          | Open explanation in the browser
<kbd>q</kbd>          | Quit the buffer
