# Perspectives contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Perspectives contribution layer for Spacemacs](#perspectives-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [Custom Perspective Macro](#custom-perspective-macro)
    - [Predefined custom perspectives](#predefined-custom-perspectives)
        - [-](#-)
        - [Org-agenda custom perspective](#org-agenda-custom-perspective)
        - [RCIRC custom perspective](#rcirc-custom-perspective)
    - [Key Bindings](#key-bindings)
        - [Custom Perspectives Key Bindings](#custom-perspectives-key-bindings)

<!-- markdown-toc end -->

## Description

This contrib layer sets up perspective-mode. And also defines custom
perspectives with a macro so that you don't have to do more steps.

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(perspectives))
```

### Custom Perspective Macro

If you want to add a new custom-persp (for example if you want to have
IRC on its own perspective or maybe calendar or gnus) you have to use
the macro `custom-persp` as follows:

```elisp
(defun custom-persp/<persp-function-name> ()
(interactive)
(custom-persp "<name-to-be-shown-in-the-modeline>"
    (... stuff to be done in the persp activating a major mode like twittering or whatever ...)))
```

You can check out the layer's packages.el to see some examples of the
custom-perspectives. if you define something like this you may be able
to define a perspective with a layout.

```elisp
(defun custom-persp/c++-project ()
(interactive)
(custom-persp "c++"
    (progn
        (find-file "~/path/to/first/file.cpp")
        (split-window-right)
        (find-file "~/path/to/second/file.cpp")
        (... do more stuff but be careful not to destroy the universe ...)
    )))
```

Then you just need to add a keybinding to your custom persp, we use
<kbd>SPC L o [your key here]</kbd> (Perspective open <key>)

```elisp
(evil-leader/set-key "Lo<your key here>" 'custom-persp/<persp-function-name>))
```

## Predefined custom perspectives

#### Per project custom perpsective

As the name suggests, this persp-projectile mode creates a new perspective
once you switch to a new project with `<SPC> p s`. It must be said that in the
current implementation in order for this to work you must first open a
custom-perspective like `SPC L o e` to go to the init.el in the spacemacs.

```elisp 
(setq-default dotspacemacs-configuration-layers '(
  (perspectives :variables
                perspective-enable-persp-projectile t)))
```

#### Org-agenda custom perspective

Here we define a custom perspective that adds items to your org-agenda if you
do not know what that is check the
[docs](https://www.gnu.org/software/emacs/manual/html_node/org/Agenda-commands.html).

The cool part is that you can have many org files with todos in the agenda and
with one simple command you can gather all the todos from all the agenda files
you have and show them in a single buffer. (in evil the command starts with `;
a`)

#### RCIRC custom perspective

Now you can also open rcirc in a new layer to keep all the chat buffers in one
perspective isolated from your work buffers.

You will have to use the perspective layer as well as the rcirc layer:

```elisp
(setq-default dotspacemacs-configuration-layers '(rcirc
                                                  perspectives))
```

## Key Bindings

Prefix command for perspective commands is <kbd>SPC L</kbd> (for Layout).

Key Binding           |                 Description
----------------------|------------------------------------------------
<kbd>SPC L A</kbd>    | Add current buffer to the current perspective
<kbd>SPC L c</kbd>    | Close a perspective (kill it)
<kbd>SPC L k</kbd>    | Remove current buffer from the current perspective
<kbd>SPC L n</kbd>    | Next perspective
<kbd>SPC L p</kbd>    | Previous perspective
<kbd>SPC L r</kbd>    | Rename current perspective
<kbd>SPC L s</kbd>    | Switch to a perspective

### Custom Perspectives Key Bindings

Key Binding           |                 Description
----------------------|------------------------------------------------
<kbd>SPC L o e</kbd>  | Emacs custom perspective
<kbd>SPC L o i</kbd>  | RCIRC custom perspective (needs the rcirc layer enabled)
<kbd>SPC L o o</kbd>  | Org custom perspective
