# ERC contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [ERC contribution layer for Spacemacs](#erc-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Features](#features)
    - [Install](#install)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

Layer for [ERC IRC chat.](http://www.emacswiki.org/emacs/ERC)

## Features

- Highlight nicks (using [erc-hl-nicks](https://github.com/leathekd/erc-hl-nicks))
- Image inline support (using [erc-image](https://github.com/kidd/erc-image.el))
- Logging to ~/.emacs.d/.cache/erc-logs and ViewLogMode for viewing logs (using [erc-view-log](https://github.com/Niluge-KiWi/erc-view-log))
- YouTube videos Thumbnails inline (using [erc-yt](https://github.com/yhvh/erc-yt))
- Social Graph for ERC messages (using [erc-social-graph](https://github.com/vibhavp/erc-social-graph))

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(erc))
```

*NOTE:* If you're on OS X, it's recommended to install the
[terminal-notifier gem](https://github.com/alloy/terminal-notifier) so that you get notifications
via the OS X Notification Center.

## Key bindings

Key Binding   | Description
--------------|------------------------------------------------------------
`<SPC> a i e` | Starts ERC
`<SPC> a i E` | Starts ERC via TLS/SSL
`<SPC> a i i` | Switch to next active ERC buffer
`<SPC> m b`   | Switch between ERC buffers
`<SPC> m d`   | Interactively input a user action and send it to IRC.
`<SPC> m D`   | Draw Social Graph using [erc-social-graph](https://github.com/vibhavp/erc-social-graph) (needs graphviz to be installed)
`<SPC> m j`   | Join a channel, executes the /join command
`<SPC> m n`   | Run "/names #channel" in the current channel.
`<SPC> m l`   | Run the /list command
`<SPC> m p`   | Part from the channel
`<SPC> m q`   | Quit server

**Note** If you want to connect securely to an IRC server, you must run
`erc-tls` command on <kbd>SPC a i E</kbd> instead of the `erc` command.
