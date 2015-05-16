# Games contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Games contribution layer for Spacemacs](#games-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)
        - [Start 2048-game](#start-2048-game)
            - [How to play](#how-to-play)

<!-- markdown-toc end -->


## Description

This layer allows you to play evilified games in spacemacs.
The games available now are:

* 2048-game

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(games))
```

## Key bindings

### Start 2048-game

Key Binding   | Description
--------------|------------------------------------------------------------
`<SPC> a G 2` | Starts the 2048-game

#### How to play

Key Binding   | Description
--------------|------------------------------------------------------------
`j`           | Move the tiles down
`k`           | Move the tiles up
`h`           | Move the tiles left
`l`           | Move the tiles right
