# Games contribution layer for Spacemacs

![logo](img/games.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Games contribution layer for Spacemacs](#games-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)
        - [2048](#2048)
        - [Tetris](#tetris)

<!-- markdown-toc end -->


## Description

This layer allows you to play evilified games in spacemacs.
The games available now are:

- 2048-game
- Tetris

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(games))
```

## Key bindings

To run a game:

Key Binding         | Description
--------------------|------------------------------------------------------
<kbd>SPC a G</kbd>  | Open an `helm` buffer to select a game

Possible helm actions:
- run (default)
- quit
- reset

### 2048

Key Binding   | Description
--------------|------------------------------------------------------------
<kbd>h</kbd>  | Move the tiles left
<kbd>j</kbd>  | Move the tiles down
<kbd>k</kbd>  | Move the tiles up
<kbd>l</kbd>  | Move the tiles right

### Tetris

Key Binding   | Description
--------------|------------------------------------------------------------
<kbd>h</kbd>  | Move block to the left
<kbd>i</kbd>  | Rotate block counter-clockwise
<kbd>j</kbd>  | Move block to the bottom
<kbd>k</kbd>  | Rotate block clockwise
<kbd>l</kbd>  | Move block to the right
<kbd>n</kbd>  | Start a new game
<kbd>p</kbd>  | Pause the game
<kbd>q</kbd>  | Quit the game
