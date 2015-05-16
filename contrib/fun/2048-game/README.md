# 2048 game contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [2048 game contribution layer for Spacemacs](#2048-game-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)
        - [Start the game](#start-the-game)
        - [How to play](#how-to-play)

<!-- markdown-toc end -->


## Description

This layer allows you to play 2048 game and waste your time.

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(2048-game))
```

## Key bindings

### Start the game

Key Binding   | Description
--------------|------------------------------------------------------------
`<SPC> a 2`   | Starts the game

### How to play

Key Binding   | Description
--------------|------------------------------------------------------------
`j`           | Move the tiles down
`k`           | Move the tiles up
`h`           | Move the tiles left
`l`           | Move the tiles right
