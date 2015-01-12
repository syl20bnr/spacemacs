# YCMD

This contrib layer adds [emacs-ycmd](https://github.com/abingham/emacs-ycmd) support.
In order to use this layer you must have a local [ycmd](https://github.com/Valloric/ycmd#building) installation
and must set the `ycmd-server-command` variable to reflect the path to that installation. See the emacs-ycmd
Readme for more instructions on this.

This package also requires the `company-mode` layer in order to get actual completion rather than just flychecking and
key bindings.

## Key Bindings

Adds the `SPC m g` go to definition binding to `c++-mode` as well as `SPC m G` for the more imprecise but faster version.

## Configuration

By default this layer only activates ycmd for `c++-mode` so company can provide completion that is better than mere keyword
completion for all the modes it would otherwise add itself to. If you want ycmd in all sorts of other modes then put this
snippet in your dotspacemacs init function:
```elisp
(setq ycmd/all-the-modes t)
```
otherwise you might just want to add it for specific languages like:
```elisp
(add-hook 'c++-mode-hook 'ycmd-mode)
```
