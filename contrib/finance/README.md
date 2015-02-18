# Finance contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Finance contribution layer for Spacemacs](#finance-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)
        - [Ledger](#ledger)

<!-- markdown-toc end -->

## Description

This layer adds finance related packages:
- [ledger][] support via [ledger-mode][]

![ledger-mode-img](img/ledger.png)

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(finance))
```

## Key bindings

### Ledger

    Key Binding    |                 Description
-------------------|------------------------------------------------------------
<kbd>SPC m a</kbd> | add a transaction
<kbd>SPC m d</kbd> | delete current transaction


[ledger]: https://github.com/ledger/ledger
[ledger-mode]: https://github.com/ledger/ledger/tree/next/lisp

