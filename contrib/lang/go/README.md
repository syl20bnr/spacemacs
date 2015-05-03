# go contribution layer for Spacemacs

![go](img/go.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [go contribution layer for Spacemacs](#go-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Pre-requisites](#pre-requisites)
        - [Layer](#layer)
    - [Working with Go](#working-with-go)
        - [Go commands (start with `m`):](#go-commands-start-with-m)
        - [Go Oracle](#go-oracle)

<!-- markdown-toc end -->

## Description

This layer adds extensive support for go.

Features:
- gofmt on file save
- Auto-completion using [go-autocomplete](https://github.com/nsf/gocode/tree/master/emacs)
- Source analysis using [go-oracle](http://golang.org/s/oracle-user-manual)

## Install

### Pre-requisites

You will need gocode:

```
go get -u github.com/nsf/gocode
```

Make sure that `gocode` executable is in your PATH.

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(go))
```

## Working with Go

### Go commands (start with `m`):

    Key Binding            |                 Description
---------------------------|------------------------------------------------------------
<kbd>SPC m d p</kbd>       | godoc at point
<kbd>SPC m i g</kbd>       | goto imports
<kbd>SPC m i a</kbd>       | add import
<kbd>SPC m i r</kbd>       | remove unused import
<kbd>SPC m p b</kbd>       | go-play buffer
<kbd>SPC m p r</kbd>       | go-play region
<kbd>SPC m p d</kbd>       | download go-play snippet
<kbd>SPC m t p</kbd>       | run "go test" for the current package
<kbd>SPC m g</kbd>         | go jump to definition


### Go Oracle

    Key Binding            |                 Description
---------------------------|------------------------------------------------------------
<kbd>SPC m o o</kbd>       | go-oracle set analysis scope
<kbd>SPC m o <</kbd>       | go-oracle show possible callers
<kbd>SPC m o ></kbd>       | go-oracle show call targets
<kbd>SPC m o c</kbd>       | go-oracle show channel sends/receives
<kbd>SPC m o d</kbd>       | go-oracle show definition
<kbd>SPC m o f</kbd>       | go-oracle show free variables
<kbd>SPC m o g</kbd>       | go-oracle show callgraph
<kbd>SPC m o i</kbd>       | go-oracle show implements relation
<kbd>SPC m o p</kbd>       | go-oracle show what the select expression points to
<kbd>SPC m o r</kbd>       | go-oracle show all references to object
<kbd>SPC m o s</kbd>       | go-oracle show callstack
<kbd>SPC m o t</kbd>       | go-oracle describe selected syntax, kind, type and methods
