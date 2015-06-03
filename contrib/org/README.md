# Org contribution layer for Spacemacs

![logo](img/org.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Org contribution layer for Spacemacs](#org-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [Github support](#github-support)
        - [Different bullets](#different-bullets)
    - [Key bindings](#key-bindings)
        - [Org with evil-org-mode](#org-with-evil-org-mode)
            - [Element insertion](#element-insertion)
            - [Org emphasize](#org-emphasize)
        - [Pomodoro](#pomodoro)
        - [Presentation](#presentation)
        - [Org-repo-todo](#org-repo-todo)

<!-- markdown-toc end -->

## Description

This layer enables [org mode][] for Spacemacs.

**Features:**
- Vim inspired key bindings are provided by [evil-org-mode][]
- Nicer bullet via [org-bullets][]
- A [pomodoro method][] integration via [org-pomodoro][]
- TODO capture via [org-repo-todo][]
- presentation mode via [org-present][]

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(org))
```

### Github support

To install Github related extensions like [ox-gfm][] to export to Github
flavored markdown set the variable `org-enable-github-support` to `t`.

```elisp
(setq-default dotspacemacs-configuration-layers '(
  (org :variables
       org-enable-github-support t)))
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

    Key Binding                                       |          Description
------------------------------------------------------|------------------------------
<kbd>SPC m '</kbd>                                    | org-edit-special
<kbd>SPC m a</kbd>                                    | org-agenda
<kbd>SPC m b</kbd>                                    | org-tree-to-indirect-buffer
<kbd>SPC m A</kbd>                                    | org-archive-subtree
<kbd>SPC m c</kbd>                                    | org-capture
<kbd>SPC m C</kbd>                                    | evil-org-recompute-clocks
<kbd>SPC m d</kbd>                                    | org-deadline
<kbd>SPC m e</kbd>                                    | org-export-dispatch
<kbd>SPC m f</kbd>                                    | org-set-effort
<kbd>SPC m I</kbd>                                    | org-clock-in
<kbd>SPC m j</kbd>                                    | helm-org-in-buffer-headings
<kbd>SPC m n</kbd>                                    | org-narrow-to-subtree
<kbd>SPC m N</kbd>                                    | widen
<kbd>SPC m <dotspacemacs-major-mode-leader-key></kbd> | org-ctrl-c-ctrl-c
<kbd>SPC m o</kbd>                                    | evil-org-open-links
<kbd>SPC m O</kbd>                                    | org-clock-out
<kbd>SPC m q</kbd>                                    | org-clock-cancel
<kbd>SPC m R</kbd>                                    | org-refile
<kbd>SPC m s</kbd>                                    | org-schedule
<kbd>SPC m T</kbd>                                    | org-show-todo-tree

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
<kbd>O</kbd>          | org-open-above

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

#### Element insertion

    Key Binding             |                 Description
----------------------------|------------------------------------------------------------
<kbd>SPC m h i</kbd>        | org-insert-heading-after-current
<kbd>SPC m h I</kbd>        | org-insert-heading
<kbd>SPC m i f</kbd>        | org-insert-footnote
<kbd>SPC m i l</kbd>        | org-insert-link

#### Org emphasize

    Key Binding             |                 Description
----------------------------|------------------------------------------------------------
<kbd>SPC m x b</kbd>        | make region bold
<kbd>SPC m x c</kbd>        | make region code
<kbd>SPC m x i</kbd>        | make region italic
<kbd>SPC m x r</kbd>        | clear region emphasis
<kbd>SPC m x s</kbd>        | make region strike-through
<kbd>SPC m x u</kbd>        | make region underline
<kbd>SPC m x v</kbd>        | make region verbose

### Pomodoro

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m p</kbd>    | starts a pomodoro

### Presentation

org-present must be activated explicitly by typing: <kbd>SPC : org-present</kbd>

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>h</kbd>          | previous slide
<kbd>l</kbd>          | next slide
<kbd>q</kbd>          | quit

### Org-repo-todo

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC C c</kbd>    | org-capture
<kbd>SPC C t</kbd>    | ort/capture-todo
<kbd>SPC C T</kbd>    | ort/capture-todo-check
<kbd>SPC m g t</kbd>  | ort/goto-todos

[org mode]: http://orgmode.org/
[evil-org-mode]: https://github.com/edwtjo/evil-org-mode
[org-pomodoro]: https://github.com/lolownia/org-pomodoro
[pomodoro method]: http://pomodorotechnique.com/
[org-bullets]: https://github.com/sabof/org-bullets
[org-repo-todo]: https://github.com/waymondo/org-repo-todo
[org-present]: https://github.com/rlister/org-present
