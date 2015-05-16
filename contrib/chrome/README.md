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
(setq-default dotspacemacs-configuration-layers '(chrome))
```

### Chrome extension

[edit-server][] is a server that responds to edit requests sent Chrome via the
Google Chrome extension [Edit with Emacs][]. You have to install this extension.

More information can be found on [Emacs Wiki][].

The edit server is configured to start automatically when Spacemacs starts.

## Configuration

Use `edit-server-url-major-mode-alist` to choose a major mode initialization
function based on `edit-server-url`, or fall back to
`edit-server-default-major-mode` that has a current value of `markdown-mode`.

```elisp
(defun dotspacemacs/config ()
;; Open github text areas as org buffers
;; currently they are opened as markdown
  (setq edit-server-url-major-mode-alist
      '(("github\\.com" . org-mode)))
)
```

Or 

[edit-server]: http://melpa.org/#/edit-server
[Edit with Emacs]: https://chrome.google.com/webstore/detail/edit-with-emacs/ljobjlafonikaiipfkggjbhkghgicgoh
[Emacs Wiki]: http://www.emacswiki.org/emacs/Edit_with_Emacs
