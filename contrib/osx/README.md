# OSX contribution layer for Spacemacs

## Description

Spacemacs is not just emacs+vim. It can have OSX keybindings too! 
This layer globally defines common OSX keybindings. ⌘ is set to
`super` and ⌥ is set to `meta`. Aside from that, there's nothing
much, really.

## Philosophy

While this layer enables common OSX bindings, it does not implement
OSX navigation keybindings. Spacemacs is meant to be used with evil,
and we encourage you to do so :)

## Install

To use this configuration layer, add it to your `~/.spacemacs`

```
(setq-default dotspacemacs-configuration-layers '(osx)
  "List of contribution to load."
)
```

## Key Bindings

- ⌘ q: Quit
- ⌘ v: Paste
- ⌘ c: Copy
- ⌘ x: Cut
- ⌘ w: Close window
- ⌘ z: Undo
- ⌘ Z: Redo
- ⌃ ⌘ f: Toggle fullscreen

## Future Work

- Allow user to choose from either `hyper` or `super` as ⌘. This is an option that is supported cross-platform.
- Configurable option to keep the OSX and spacemacs clipboards separate
