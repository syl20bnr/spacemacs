# dash contribution layer for Spacemacs

![logo](img/dash.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [dash contribution layer for Spacemacs](#dash-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)
    - [Improving](#improving)

<!-- markdown-toc end -->

## Description

Dash is great for instant quick access to documentation. Unfortunately it is
*only available on OSX* at the moment. Recommended to set 'HUD mode' in your
Dash application preferences when using this. dash-at-point will try to
intelligently guess specific docsets to use based off of your current mode. See
the documentation [dash-at-point](https://github.com/stanaka/dash-at-point#Usage)
for more information on customizing specific docsets for modes.

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(dash)
  "List of contribution to load."
)
```

## Key bindings

    Key Binding     |                 Description
--------------------|------------------------------------------------------------
<kbd>SPC d d</kbd>  | Lookup thing at point in Dash
<kbd>SPC m D</kbd>  | Lookup thing at point in Dash within a specified Docset

## Improving

dash-at-point is linked the GUI app and is only available for OSX. Another project,
[helm-dash](https://github.com/areina/helm-dash) is in the works. It appears to
currently only be available for linux though, but working towards mac and windows
support. It would be great to supplement or even replace dash-at-point considering
that it doesn't require paying for the app, and integrates with helm.

[dash]: http://kapeli.com/dash
[dash-at-point]: https://github.com/stanaka/dash-at-point
[helm-dash]: https://github.com/areina/helm-dash
