# TypeScript contribution layer for Spacemacs

![logo](img/TypeScript.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [TypeScript contribution layer for Spacemacs](#typescript-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Prerequisites](#prerequisites)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This adds support for TypeScript editing via [typescript-tools](https://github.com/clausreinke/typescript-tools) and [emacs-tss](https://github.com/aki2o/emacs-tss).

These provide:
- syntax coloring
- error highlighting
- autocompletion
- jump-to-definition

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(typescript))
```

### Prerequisites

You'll need [typescript-tools](https://github.com/clausreinke/typescript-tools) and fairly obviously also the TypeScript compiler:

```
$ npm install typescript
$ git clone git://github.com/clausreinke/typescript-tools.git
$ cd typescript-tools/
$ npm install -g
```

## Key bindings

Key Binding   | Description
--------------|------------------------------------------------------------
`<SPC> m h`   | Show popup help (with type info)
`<SPC> m d`   | Jump to definition
`<SPC> m c f` | Flymake check (is done automatically on save)
