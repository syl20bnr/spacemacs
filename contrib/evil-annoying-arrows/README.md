# evil-annoying-arrows-layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [evil-annoying-arrows-layer for Spacemacs](#evil-annoying-arrows-layer-for-spacemacs)
    - [Description](#description)
        - [Layer](#layer)

<!-- markdown-toc end -->

## Description
This package warns you when you are using the arrow keys too much, and instead suggests alternative motions to move more efficiently.

By setting `evil-annoying-arrows-super-annoying-mode` a new buffer will open with the warning, instead of just showing it in the minibuffer.

The number of keypresses before `evil-annoying-arrows` complains can be set with `evil-annoying-arrows-too-far-count`. The default is 10.

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(evil-annoying-arrows))
```
