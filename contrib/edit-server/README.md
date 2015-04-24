# edit-server contribution layer for Spacemacs

![logo](img/chrome.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [edit-server contribution layer for Spacemacs](#edit-server-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Configuration](#configuration)

<!-- markdown-toc end -->

## Description

This layer provides [edit-server](http://melpa.org/#/edit-server) - a server that responds to edit requests from Chrome. The package is required by the [Edit with Emacs](https://chrome.google.com/webstore/detail/edit-with-emacs/ljobjlafonikaiipfkggjbhkghgicgoh) Google Chrome extension.

More information can be found on [Emacs Wiki](http://www.emacswiki.org/emacs/Edit_with_Emacs).

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(edit-server))
```

The server is configured to start when the editor is opened.

## Configuration

You can hook into `edit-server`'s provided hooks in `dotspacemacs/config` in your `.spacemacs` file.

```elisp
(defun dotspacemacs/config ()
  ;; Open github text areas as markdown
  (add-hook 'edit-server-start-hook
            (lambda ()
              (when (string-match "github.com" (buffer-name))
                (markdown-mode))))
)
```
