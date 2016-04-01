# git contribution layer for Spacemacs

![logo](git.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [git contribution layer for Spacemacs](#git-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Working with Git](#working-with-git)
        - [Magit](#magit)
        - [Quick guide for recurring use cases in Magit](#quick-guide-for-recurring-use-cases-in-magit)
        - [Git gutter bitmaps](#git-gutter-bitmaps)

<!-- markdown-toc end -->

## Description

This layers adds extensive support for [git][].

Features:
- git repository management the indispensable [magit][] package
- [git-flow][] add-on for magit.
- quick in buffer history browsing with [git-timemachine][]
- quick in buffer last commit message per line with [git-messenger][]
- colorize buffer line by age of commit with [smeargle][]
- git gutter in fringe with [git-gutter][]

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(git)
  "List of contribution to load."
)
```

Of course if your OS does not ship with git (!) you'll have to install it
on your machine, [download page][].

## Working with Git

Git commands (start with `g`):

    Key Binding            |                 Description
---------------------------|------------------------------------------------------------
<kbd>SPC g c c</kbd>       | highlight regions by age of commits
<kbd>SPC g c C</kbd>       | clear highlights
<kbd>SPC g c t</kbd>       | highlight regions by last updated time
<kbd>SPC g s</kbd>         | open a `magit` status window
<kbd>SPC g m</kbd>         | display the last commit message of the current line
<kbd>SPC g t</kbd>         | launch the git time machine

- Highlight by age of commit or last update time is provided by
[smeargle][].
- Git time machine is provided by [git-timemachine][].
- Git last commit message per line is provided by
[git-messenger][]

### Magit

`Spacemacs` uses [magit][] to manage Git repositories.

To open a `status buffer`, type in a buffer of a Git repository:

    <SPC> g s

`hjkl` navigation is enabled in all Magit buffers. The default Magit keys
on `hjkl` are remapped on `HJKL`.

Here are the often used bindings inside a `status buffer`:

    Key Binding   |                 Description
------------------|------------------------------------------------------------
<kbd>/</kbd>      | evil-search
<kbd>$</kbd>      | open `command output buffer`
<kbd>c c</kbd>    | open a `commit message buffer`
<kbd>b b</kbd>    | checkout a branch
<kbd>b c</kbd>    | create a branch
<kbd>b v</kbd>    | open the `branch manager buffer`
<kbd>f f</kbd>    | fetch changes
<kbd>F -r F</kbd> | pull and rebase
<kbd>h</kbd>      | go left
<kbd>j</kbd>      | go down
<kbd>C-j</kbd     | goto next magit section
<kbd>k</kbd>      | go up
<kbd>K</kbd>      | discard changes
<kbd>C-k</kbd     | goto previous magit section
<kbd>l</kbd>      | go right
<kbd>L l</kbd>    | open `log buffer`
<kbd>n</kbd>      | next search occurrence
<kbd>C-n</kbd     | goto next magit section
<kbd>N</kbd>      | previous search occurrence
<kbd>P P</kbd>    | push
<kbd>C-p</kbd     | goto previous magit section
<kbd>q</kbd>      | quit
<kbd>s</kbd>      | on a file or hunk in a diff: stage the file or hunk
<kbd>+</kbd>      | on a hunk: increase hunk size
<kbd>-</kbd>      | on a hunk: decrease hunk size
<kbd>S</kbd>      | stage all
<kbd>TAB</kbd>    | on a file: expand/collapse diff
<kbd>u</kbd>      | on a staged file: unstage
<kbd>U</kbd>      | unstage all staged files
<kbd>v</kbd>      | `visual state`
<kbd>V</kbd>      | `visual-line state`
<kbd>C-v</kbd>    | revert item at point
<kbd>z z</kbd>    | stash changes

In a commit message buffer press `C-c C-c` to commit the changes with the
entered message. `C-c C-k` will discard the commit message.

**Note:** Sometimes you will be asked about reverting the commit buffer,
you can answer `y` with no issue.

### Quick guide for recurring use cases in Magit

- Amend a commit:
  - `L l` to open `log buffer`
  - `c a` on the commit you want to amend
  - `C-c C-c` to submit the changes
- Squash last commit:
  - `L l` to open `log buffer`
  - `E` on the second to last commit, it opens the `rebase buffer`
  - `j` to put point on last commit
  - `i` to pass in `insert state`
  - `s` to squash it
  - `C-c C-c` to continue to the `commit message buffer`
  - `C-c C-c` again when you have finished to edit the commit message
- Force push a squashed commit:
  - in the `status buffer` you should see the new commit unpushed and the
  old commit unpulled
  - `P -f P` for force a push (**beware** usually it is not recommended to
  rewrite the history of a public repository, but if you are *sure* that you
  are the only one to work on a repository it is ok - i.e. in your fork).
- Add upstream remote (the parent repository you have forked):
  - `b v` to open the `branch manager buffer`
  - `a` to add a remote, type the name (i.e. `upstream`) and the URL
- Pull changes from upstream (the parent repository you have forked) and push:
  - `F -r C-u F` and choose `upstream` or the name you gave to it
  - `P P` to push the commit to `origin`

### Git gutter bitmaps

`Spacemacs` has custom fringe bitmaps for
[git-gutter-fringe][git-gutter]:

   Symbol                        | Description
:-------------------------------:|-----------------
![git-new](git-new-line.png) | new line
![git-del](git-del-line.png) | at least one line has been deleted
![git-mod](git-mod-line.png) | modified line


[git]: http://git-scm.com/
[download page]: http://git-scm.com/downloads
[git-gutter]: https://github.com/syohex/emacs-git-gutter-fringe
[magit]: http://magit.github.io/
[git-flow]: https://github.com/jtatarik/magit-gitflow
[smeargle]: https://github.com/syohex/emacs-smeargle
[git-timemachine]: https://github.com/pidu/git-timemachine
[git-messenger]: https://github.com/syohex/emacs-git-messenger
