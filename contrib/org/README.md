# Org contribution layer for Spacemacs

![logo](img/org.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Org contribution layer for Spacemacs](#org-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [Different bullets](#different-bullets)
    - [Key bindings](#key-bindings)
        - [Org with evil-org-mode](#org-with-evil-org-mode)
        - [Pomodoro](#pomodoro)
        - [Org-repo-todo](#org-repo-todo)

<!-- markdown-toc end -->

## Description

This layer enables [org mode][] for Spacemacs.

**Features:**
- Vim inspired key bindings are provided by [evil-org-mode][]
- Nicer bullet via [org-bullets][]
- A [pomodoro method][] integration via [org-pomodoro][]
- TODO capture via [org-repo-todo][]

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(org))
```

### Different bullets

You can tweak the bullets displayed in the org buffer in the function
`dotspacemacs/config` of your dotfile by setting the variable
`org-bullets-bullet-list`. By default the list is set to `("◉" "○" "✸" "✿")`.

```elisp
(setq org-bullets-bullet-list '("■" "◆" "▲" "▶"))
```

## Key bindings

### Org with evil-org-mode

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m a</kbd>    | org-agenda
<kbd>SPC m A</kbd>    | org-archive-subtree
<kbd>SPC m c</kbd>    | org-capture
<kbd>SPC m C</kbd>    | evil-org-recompute-clocks
<kbd>SPC m d</kbd>    | org-deadline
<kbd>SPC m e</kbd>    | org-export-dispatch
<kbd>SPC m f</kbd>    | org-set-effort
<kbd>SPC m i</kbd>    | org-clock-in
<kbd>SPC m l</kbd>    | evil-org-open-links
<kbd>SPC m m</kbd>    | org-ctrl-c-ctrl-c
<kbd>SPC m o</kbd>    | org-clock-out
<kbd>SPC m q</kbd>    | org-clock-cancel
<kbd>SPC m r</kbd>    | org-refile
<kbd>SPC m s</kbd>    | org-schedule
<kbd>SPC m t</kbd>    | org-show-todo-tree

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>TAB</kbd>        | org-cycle
<kbd>$</kbd>          | org-end-of-line
<kbd>^</kbd>          | org-beginning-of-line
<kbd><</kbd>          | org-metaleft
<kbd>></kbd>          | org-metaright
<kbd>gh</kbd>         | outline-up-heading
<kbd>gj</kbd>         | org-forward-heading-same-level
<kbd>gk</kbd>         | org-backward-heading-same-level
<kbd>gl</kbd>         | outline-next-visible-heading
<kbd>t</kbd>          | org-todo
<kbd>T</kbd>          | org-insert-todo-heading nil
<kbd>H</kbd>          | org-beginning-of-line
<kbd>L</kbd>          | org-end-of-line
<kbd>o</kbd>          | always-insert-item
<kbd>O</kbd>          | org-insert-heading

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>M-l</kbd>        | org-metaright
<kbd>M-h</kbd>        | org-metaleft
<kbd>M-k</kbd>        | org-metaup
<kbd>M-j</kbd>        | org-metadown
<kbd>M-L</kbd>        | org-shiftmetaright
<kbd>M-H</kbd>        | org-shiftmetaleft
<kbd>M-K</kbd>        | org-shiftmetaup
<kbd>M-J</kbd>        | org-shiftmetadown
<kbd>M-o</kbd>        | org-insert-heading+org-metaright
<kbd>M-t</kbd>        | org-insert-todo-heading nil+ org-metaright

### Pomodoro

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m p</kbd>    | starts a pomodoro

### Org-repo-todo

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC C t</kbd>    | ort/capture-todo
<kbd>SPC C T</kbd>    | ort/capture-todo-check
<kbd>SPC m g t</kbd>  | ort/goto-todos


[org mode]: http://orgmode.org/
[evil-org-mode]: https://github.com/edwtjo/evil-org-mode
[org-pomodoro]: https://github.com/lolownia/org-pomodoro
[pomodoro method]: http://pomodorotechnique.com/
[org-bullets]: https://github.com/sabof/org-bullets
[org-repo-todo]: https://github.com/waymondo/org-repo-todo
