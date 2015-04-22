# OS X contribution layer for Spacemacs

![applogo](img/apple.png)![osxlogo](img/osx.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [OS X contribution layer for Spacemacs](#os-x-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Philosophy](#philosophy)
    - [Install](#install)
    - [Key Bindings](#key-bindings)
    - [Future Work](#future-work)

<!-- markdown-toc end -->

## Description

Spacemacs is not just emacs+vim. It can have OS X keybindings too!
This layer globally defines common OS X keybindings. ⌘ is set to
`super` and ⌥ is set to `meta`.

On OS X GUI applications do not inherit shell variables from a
user's login shell session but this layer makes sure it does. This
makes Emacs correctly discover packages installed by Homebrew.

Aside from that, there's nothing much, really.

## Philosophy

While this layer enables common OS X bindings, it does not implement
OS X navigation keybindings. Spacemacs is meant to be used with evil,
and we encourage you to do so :)

## Install

To use this configuration layer, add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(osx)
  ;; List of contribution to load.
)
```

By default only `$PATH` and `$MANPATH` are taken from your shell
session. Any additional environment variables that you would like to
also keep can be copied through calls to `exec-path-from-shell-copy-env`
or by passing a list to `exec-path-from-shell-copy-envs`.

```elisp
(defun dotspacemacs/config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (exec-path-from-shell-copy-env "PYENV_ROOT")
  (exec-path-from-shell-copy-envs '("RBENV_ROOT" "ANDROID_HOME"))
)
```

## Key Bindings

    Key Binding   |       Description
------------------|------------------------------------------------------------
<kbd>⌘ q</kbd>    | Quit
<kbd>⌘ v</kbd>    | Paste
<kbd>⌘ c</kbd>    | Copy
<kbd>⌘ x</kbd>    | Cut
<kbd>⌘ w</kbd>    | Close window
<kbd>⌘ z</kbd>    | Undo
<kbd>⌘ Z</kbd>    | Redo
<kbd>⌃ ⌘ f</kbd>  | Toggle fullscreen

## Future Work

- Allow user to choose from either `hyper` or `super` as ⌘. This is an option that is supported cross-platform.
- Configurable option to keep the OS X and spacemacs clipboards separate
