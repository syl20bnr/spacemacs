# C/C++ contribution layer for Spacemacs

![cc++](ccpp.jpg)
![cmake](cmake.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [C/C++ contribution layer for Spacemacs](#cc-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)

<!-- markdown-toc end -->

## Description

This layer adds configuration for C/C++ language as well support for [CMake][]
scripts.

**This layer is not adapted for Spacemacs, it needs you, C/C++ experts, to
improve it and make it consistent with the Spacemacs experience.**

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(c-c++)
  "List of contribution to load."
)
```

[CMake]: http://www.cmake.org/
