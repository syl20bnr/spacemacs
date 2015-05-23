# Vagrant contribution layer for Spacemacs

![vagrant](img/vagrant.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Vagrant contribution layer for Spacemacs](#vagrant-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [Vagrant](#vagrant)
        - [Testing](#testing)
    - [Keybindings](#keybindings)

<!-- markdown-toc end -->

## Description

This layer adds support for working with Vagrant using [vagrant.el][] and
[vagrant-tramp][].

Features:
 - manage boxes (under the <kbd>SPC V</kbd> prefix)
 - remote editing on Vagrant boxes via Tramp

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(ruby vagrant))
```

**Note** Since vagrant files are written in `ruby` it is recommended
to install the `ruby` layer as well.

### Vagrant

Follow the [Installing Vagrant][] and [Getting Started][] guides in
Vagrant's documentation.

### Testing

If you'd like to test this layer out in a simple way (for example to
make sure you have Vagrant configured correctly) there is a [Vagrantfile][]
in this directory.

## Keybindings

Key Binding        | Description
-------------------|-----------------------------------------------------------------------------------------------
<kbd>SPC V D</kbd> | destroy a box
<kbd>SPC V e</kbd> | edit the `Vagrantfile`
<kbd>SPC V H</kbd> | halt (shut down) a box
<kbd>SPC V p</kbd> | (re)provision a box that is already up
<kbd>SPC V r</kbd> | resume a suspended box (you can also use `SPC V V` for this)
<kbd>SPC V s</kbd> | view the status of running boxes in the current project
<kbd>SPC V S</kbd> | suspend a box
<kbd>SPC V t</kbd> | start a `vagrant-tramp-term` session - after start, edit files at `/vagrant:box_name:filename`
<kbd>SPC V V</kbd> | bring up a Vagrant box

[vagrant.el]: https://github.com/ottbot/vagrant.el
[vagrant-tramp]: https://github.com/dougm/vagrant-tramp
[Installing Vagrant]: http://docs.vagrantup.com/v2/installation/index.html
[Getting Started]: http://docs.vagrantup.com/v2/getting-started/index.html
[Vagrantfile]: Vagrantfile
