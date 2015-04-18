# Deft configuration layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Deft configuration layer for Spacemacs](#deft-configuration-layer-for-spacemacs)
    - [Description](#description)
        - [Differences from default](#differences-from-default)
    - [Install](#install)
        - [Layer](#layer)
        - [Configuration](#configuration)
    - [Key Bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

[Deft][] is an Emacs mode inspired by `Notational Velocity` for quickly
browsing and creating notes.

### Differences from default

This layer sets the default to use filenames for note titles as well as
`org-mode` for editing. The default extension is still `.txt` though.

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(deft))
```

### Configuration

By default deft tries to put notes in `~/.deft` but you can change
this like so in your `dotspacemacs/config` function:

```
(setq deft-directory "~/Dropbox/notes")
```

## Key Bindings

Key Binding         |                 Description
--------------------|------------------------------------------------------------------
<kbd>SPC a n  </kbd>| Open Deft (works globally)
<kbd>SPC m d  </kbd>| Delete selected note
<kbd>SPC m r  </kbd>| Rename selected note
<kbd>SPC m i  </kbd>| Toggle to regex search
<kbd>SPC m n  </kbd>| Create new file with filter text

[Deft]: http://jblevins.org/projects/deft/
