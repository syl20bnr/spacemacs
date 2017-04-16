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
# Vagrant contribution layer for Spacemacs

![vagrant](img/vagrant.png)

## Description

This layers adds support for working with Vagrant using
[vagrant.el](https://github.com/ottbot/vagrant.el) and
[vagrant-tramp](https://github.com/dougm/vagrant-tramp)

Features:

 - manage boxes (under the `SPC V` prefix)
 - remote editing on Vagrant boxes via tramp

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(vagrant))
```

### Vagrant

Follow the
[Installing Vagrant](http://docs.vagrantup.com/v2/installation/index.html)
and
[Getting Started](http://docs.vagrantup.com/v2/getting-started/index.html)
guides in Vagrant's documentation.

### Testing

If you'd like to test this layer out in a simple way (for example to
make sure you have Vagrant configured correctly) there is a
[Vagrantfile](Vagrantfile) in this directory.

## Keybindings

Key Binding | Description
------------|-----------------------------------------------------------------------------------------------
`SPC V V`   | bring up a Vagrant box
`SPC V p`   | (re)provision a box that is already up
`SPC V D`   | destroy a box
`SPC V s`   | view the status of running boxes in the current project
`SPC V S`   | suspend a box
`SPC V r`   | resume a suspended box (you can also use `SPC V V` for this)
`SPC V H`   | halt (shut down) a box
`SPC V e`   | edit the `Vagrantfile`
`SPC V t`   | start a `vagrant-tramp-term` session - after start, edit files at `/vagrant:box_name:filename`
