# dash contribution layer for Spacemacs

![logo](img/dash.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [dash contribution layer for Spacemacs](#dash-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Dash](#dash)
    - [Key bindings](#key-bindings)
    - [TODO](#todo)
        - [Check zeal](#check-zeal)
        - [Check helm-dash](#check-helm-dash)

<!-- markdown-toc end -->

**This layer works only on OS X for the moment**

## Description

[dash][] is a great tool for quick access to various sets of documentation.

[dash-at-point][] is the package used to integrate `dash` in Emacs. It will try
to intelligently guess specific docsets to use based off of your current mode.

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(dash))
```

### Dash

You have to install [dash][] on your machine.

It is recommended to set the `HUD mode` in your Dash application preferences
when using this layer. 

See the documentation [dash-at-point][dash-at-point-usage] for more information
on customizing specific docsets for modes.

## Key bindings

    Key Binding     |                 Description
--------------------|---------------------------------------------------------
<kbd>SPC d d</kbd>  | Lookup thing at point in Dash
<kbd>SPC d D</kbd>  | Lookup thing at point in Dash within a specified Docset

### helm-dash
dash-at-point is linked to the GUI app and is only available for OSX. On linux, 
[helm-dash](https://github.com/areina/helm-dash) is used instead. It requires no app.
You can use `dash/helm-dash-docset-newpath` to set the location path of your docsets.

## TODO

### Check zeal

[zeal][] is an open source alternative to dash with Emacs integration available.

[dash]: http://kapeli.com/dash
[dash-at-point]: https://github.com/stanaka/dash-at-point
[dash-at-point-usage]: https://github.com/stanaka/dash-at-point#Usage
[helm-dash]: https://github.com/areina/helm-dash
[zeal]: http://zealdocs.org/
