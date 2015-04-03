# Device Tree layer for Spacemacs

[Device Tree](http://devicetree.org/Main_Page) is a data structure and syntax
for describing hardware used by Linux and other kernels. This layer adds support
for [`dts-mode`](https://github.com/bgamari/dts-mode) which provides basic
syntax highlighting for the Device Tree source format.

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(device-tree))
```
