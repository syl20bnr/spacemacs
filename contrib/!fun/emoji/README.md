# Emoji contribution layer for Spacemacs

:blue_heart::green_heart::heart::gift_heart::heartbeat::heartpulse::purple_heart::sparkling_heart::yellow_heart:

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Emoji contribution layer for Spacemacs](#emoji-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)
        - [Emoji dedicated buffer](#emoji-dedicated-buffer)

<!-- markdown-toc end -->

## Description

This layer adds support for Emoji emoticons from [emoji-cheat-sheet.com][].

Features:
- browse Emoji in a dedicated buffer
- display Emoji images in buffer
- insert one or several Emoji with an helm front-end

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(emoji))
```

## Key bindings

Key Binding        | Description
-------------------|------------------------------------------------------------
<kbd>SPC a E</kbd> | open a dedicated buffer to browse Emoji
<kbd>SPC i e</kbd> | insert Emoji via an helm buffer

### Emoji dedicated buffer

Key Binding        | Description
-------------------|------------------------------------------------------------
<kbd>RET</kbd>     | copy current Emoji code
<kbd>q</kbd>       | quit

[emoji-cheat-sheet.com]: http://www.emoji-cheat-sheet.com/
