# git contribution layer for Spacemacs

![git](img/git.png) ![github](img/github.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [git contribution layer for Spacemacs](#git-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Working with Git](#working-with-git)
        - [Magit](#magit)
        - [Quick guide for recurring use cases in Magit](#quick-guide-for-recurring-use-cases-in-magit)
        - [Git gutter](#git-gutter)
    - [Github support](#github-support)
        - [magit-gh-pulls](#magit-gh-pulls)
        - [gist.el](#gistel)
        - [github-browse-file](#github-browse-file)

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

This layer also provides support for Github with:
- [magit-gh-pulls][]: handy `magit` add-on to manage Github pull requests.
- [gist.el][]: full-featured mode to browse and post Githug gists.

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(git)
  "List of contribution to load."
)
```

To enable the Github support set the variable `git-enable-github-support`
to `t` in your `dotspacemacs/init` function.

```elisp
(defun dotspacemacs/init ()
  (setq-default git-enable-github-support t)
)
```

Of course if your OS does not ship with git (!) you'll have to install it
on your machine, [download page][].

## Working with Git

Git commands (start with `g`):

    Key Binding            |                 Description
---------------------------|------------------------------------------------------------
<kbd>SPC g b</kbd>         | open a `magit` blame
<kbd>SPC g C</kbd>         | commit changes
<kbd>SPC g h c</kbd>       | clear highlights
<kbd>SPC g h h</kbd>       | highlight regions by age of commits
<kbd>SPC g h t</kbd>       | highlight regions by last updated time
<kbd>SPC g l</kbd>         | open a `magit` log
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

### Git gutter

With [git-gutter][] it is possible to navigate between hunks, stage them and
revert them.

Git gutter hunks commands start with `gh`:

    Key Binding     |                 Description
--------------------|------------------------------------------------------------
<kbd>g h n</kbd>    | next hunk in buffer
<kbd>g h N</kbd>    | previous hunk in buffer
<kbd>g h r</kbd>    | revert current hunk
<kbd>g h s</kbd>    | stage current hunk

`Spacemacs` has custom fringe bitmaps for [git-gutter-fringe][git-gutter]:

           Symbol            | Description
:---------------------------:|-----------------
![git-new](git-new-line.png) | new line
![git-del](git-del-line.png) | at least one line has been deleted
![git-mod](git-mod-line.png) | modified line

## Github support

### magit-gh-pulls

In a `magit status` buffer (<kbd>SPC g s</kbd>):

    Key Binding     |                 Description
--------------------|------------------------------------------------------------
<kbd># g g</kbd>    | get a list of all PRs in the current repository
<kbd># g f</kbd>    | fetch the commits associated to the current PR
<kbd># g b</kbd>    | create a branch for the current PR
<kbd># g m</kbd>    | merge the PR with current branch

Note that `magit-gh-pulls` will try to fast-forward the PRs whenever it is
possible.

### gist.el

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC g g b</kbd>  | create a public gist with the buffer content
<kbd>SPC g g B</kbd>  | create a private gist with the buffer content
<kbd>SPC g g l</kbd>  | open the gist list buffer
<kbd>SPC g g r</kbd>  | create a public gist with the region content
<kbd>SPC g g R</kbd>  | create a private gist with the region content

In the gist list buffer:

    Key Binding              |                 Description
-----------------------------|------------------------------------------------------------
<kbd>/</kbd>                 | evil search
<kbd>+</kbd>                 | add buffer to gist
<kbd>-</kbd>                 | remove file for gist
<kbd>b</kbd> or <kbd>o</kbd> | open current gist in browser
<kbd>f</kbd>                 | fetch current gist
<kbd>g</kbd>                 | refresh the list
<kbd>h</kbd>                 | go left
<kbd>j</kbd>                 | go down
<kbd>k</kbd>                 | go up
<kbd>K</kbd>                 | kill current gist
<kbd>l</kbd>                 | go right
<kbd>n</kbd>                 | next search occurrence
<kbd>N</kbd>                 | next previous occurrence
<kbd>v</kbd>                 | `visual state`
<kbd>V</kbd>                 | `visual-line state`
<kbd>y</kbd>                 | print URL and copy it

[git]: http://git-scm.com/
[download page]: http://git-scm.com/downloads
[git-gutter]: https://github.com/syohex/emacs-git-gutter-fringe
[magit]: http://magit.github.io/
[git-flow]: https://github.com/jtatarik/magit-gitflow
[smeargle]: https://github.com/syohex/emacs-smeargle
[git-timemachine]: https://github.com/pidu/git-timemachine
[git-messenger]: https://github.com/syohex/emacs-git-messenger
[magit-gh-pulls]: https://github.com/sigma/magit-gh-pulls
[gist.el]: https://github.com/defunkt/gist.el

### github-browse-file

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC g f b</kbd>  | browse to file on github

