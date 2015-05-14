# Rust contribution layer for Spacemacs

![logo](img/rust.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Rust contribution layer for Spacemacs](#rust-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)

<!-- markdown-toc end -->


## Description

This layer aims to support [Rust][] development in Spacemacs.

For now only basic support for [Cargo][] is provided.

## Install

### Layer

To use this layer, add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(rust))
```

### Cargo

[Cargo][] is a project management command line tool for Rust.
Installation instructions can be found on the main page of [Cargo][].

## Key bindings

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC m c c</kbd> | compile project with Cargo
<kbd>SPC m c x</kbd> | compile and execute project with Cargo
<kbd>SPC m t a</kbd> | execute all tests with Cargo

[Rust]: http://www.rust-lang.org/
[Cargo]: http://doc.crates.io/index.html
