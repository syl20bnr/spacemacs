# chrome contribution layer for Spacemacs

![logo](img/chrome.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [chrome contribution layer for Spacemacs](#chrome-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [Chrome extension](#chrome-extension)
    - [Configuration](#configuration)

<!-- markdown-toc end -->

## Description

This layer provides some integration with the Google Chrome browser.

Feature:
- Edit text boxes with Emacs using [edit-server][]

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(edit-server))
```

### Chrome extension

[edit-server][] is a server that responds to edit requests sent Chrome via the
Google Chrome extension [Edit with Emacs][]. You have to install this extension.

More information can be found on [Emacs Wiki][].

The edit server is configured to start automatically when Spacemacs starts.

## Configuration

You can hook into `edit-server`'s provided hooks in `dotspacemacs/config` in
your `.spacemacs` file.

```elisp
(defun dotspacemacs/config ()
  ;; Open github text areas as markdown
  (add-hook 'edit-server-start-hook
            (lambda ()
              (when (string-match "github.com" (buffer-name))
                (markdown-mode))))
)
```

[edit-server]: http://melpa.org/#/edit-server
[Edit with Emacs]: https://chrome.google.com/webstore/detail/edit-with-emacs/ljobjlafonikaiipfkggjbhkghgicgoh
[Emacs Wiki]: http://www.emacswiki.org/emacs/Edit_with_Emacs
