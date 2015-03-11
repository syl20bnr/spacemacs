# C/C++ contribution layer for Spacemacs

![cc++](img/ccpp.jpg)
![cmake](img/cmake.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [C/C++ contribution layer for Spacemacs](#cc-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)

<!-- markdown-toc end -->

## Description

This layer adds configuration for C/C++ language as well support for [CMake][]
scripts.

## Features

- Support syntax checking with Clang.
- Display function or variable definition at the bottom.
- Display current function cursor is in at the top.
- Support common refactoring with [semantic-refactor][]. See [this page][demos]
for demonstration of refactoring features.

**This layer is not fully adapted for Spacemacs, it needs you, C/C++ experts, to
improve it and make it consistent with the Spacemacs experience.**

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(c-c++))
```

** Note: ** [semantic-refactor][] is only available for Emacs 24.4+

[CMake]: http://www.cmake.org/
[semantic-refactor]: https://github.com/tuhdo/semantic-refactor
[demos]: https://github.com/tuhdo/semantic-refactor/blob/master/srefactor-demos/demos.org
