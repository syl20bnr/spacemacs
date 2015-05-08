# Spotify contribution layer for Spacemacs

![logo](img/spotify.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Spotify contribution layer for Spacemacs](#spotify-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer adds key bindings for controlling Spotify from inside Emacs.

## Install

To use this contribution layer add it to your `~/.spacemacs`

```elisp
(set-default dotspacemacs-configuration-layers '(spotify))
```

## Key bindings

Key Binding            | Description
-----------------------|-------------------------------
<kbd>SPC a m s p</kbd> | Play or pause Spotify
<kbd>SPC a m s n</kbd> | Go to the next track
<kbd>SPC a m s N</kbd> | Go to the previous track
<kbd>SPC a m s g</kbd> | Search for a new track
<kbd>SPC a m s Q</kbd> | Quit Spotify
