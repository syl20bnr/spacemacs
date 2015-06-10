# Version-Control contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Version-Control contribution layer for Spacemacs](#version-control-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
    - [Key Bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layers adds general configuration for [Emacs VC][].
It should work with all VC backends such as Git, Mercurial, Bazaar, SVN, etc...

Features:
- highlights uncommitted changes in the fringe or margin with [diff-hl][]

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(version-control))
```

## Key Bindings

    Key Binding   |                 Description
------------------|------------------------------------------------------------
<kbd>ghg</kbd>    | Go to hunk
<kbd>ghN</kbd>    | Previous hunk
<kbd>ghn</kbd>    | Next hunk
<kbd>ghr</kbd>    | Revert hunk

[Emacs VC]: http://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html
[diff-hl]: https://github.com/dgutov/diff-hl
