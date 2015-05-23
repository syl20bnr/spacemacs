# Ansible contribution layer for Spacemacs

![ansible](img/ansible.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Ansible contribution layer for Spacemacs](#ansible-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer adds support for Ansible-flavored YAML buffers.

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(ansible))
```

## Key bindings

Key Binding   | Description
--------------|------------------------------------------------------------
`<SPC> m a ?` | looks up documentation using [`ansible-doc`][ansible-doc]

[ansible-doc]: https://github.com/lunaryorn/ansible-doc.el
