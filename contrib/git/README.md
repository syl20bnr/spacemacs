# git contribution layer for Spacemacs

![git](img/git.png) ![github](img/github.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [git contribution layer for Spacemacs](#git-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [Github support](#github-support)
        - [Magit status fullscreen](#magit-status-fullscreen)
        - [Magit auto-complete](#magit-auto-complete)
        - [Magit SVN plugin](#magit-svn-plugin)
        - [Git gutter](#git-gutter)
        - [Git](#git)
    - [Working with Git](#working-with-git)
        - [Magit](#magit)
        - [Commit message edition buffer](#commit-message-edition-buffer)
        - [Interactive rebase buffer](#interactive-rebase-buffer)
        - [Quick guide for recurring use cases in Magit](#quick-guide-for-recurring-use-cases-in-magit)
        - [Git time machine](#git-time-machine)
        - [Git gutter](#git-gutter)
    - [Github support](#github-support)
        - [magit-gh-pulls](#magit-gh-pulls)
        - [gist.el](#gistel)
        - [Browse files](#browse-files)

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
- [github-browse-file][] and [git-link][]: quickly browse github URL in your
browser.

New to Magit? Checkout the [official intro][].

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(git))
```

### Github support

To enable the Github support set the variable `git-enable-github-support`
to `t` in your `dotspacemacs/init` function.

```elisp
(defun dotspacemacs/init ()
  (setq-default git-enable-github-support t)
)
```

### Magit status fullscreen

To display the `magit status` buffer in fullscreen set the variable
`git-magit-status-fullscreen` to `t` in your `dotspacemacs/init` function.

```elisp
(defun dotspacemacs/init ()
  (setq-default git-magit-status-fullscreen t)
)
```

### Magit auto-complete

Magit auto-complete feature is enabled. For this feature to work best you
have to setup your Git repository directory in your `dotspacemacs/config`
function, this is the folder where you keep all your git-controlled projects
(the path should end up with a `/` to respect Emacs conventions):

```elisp
(setq magit-repo-dirs '("~/repos/"))
```

For more information, see [Magit-User-Manual#Status][]

### Magit SVN plugin

For convenience the magit SVN plugin can be activated directly in the Git
layer by setting the variable `git-enable-magit-svn-plugin` to `t`.

```elisp
(defun dotspacemacs/init ()
  (setq-default git-enable-magit-svn-plugin t)
)
```

### Git gutter

In graphical environment `Spacemacs` will display [git-gutter][] icons in
the fringe by default.

It is possible to disable the usage of the fringe by setting the variable
`git-gutter-use-fringe` to `nil` in the `dotspacemacs/init` function:

```elisp
(defun dotspacemacs/init ()
  (setq-default git-gutter-use-fringe nil)
)
```

### Git

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
on `hjkl` (if they exist) are remapped on `HJKL`.

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
<kbd>N</kbd>      | previous search occurrence _or_ SVN sub-menu
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

### Commit message edition buffer

In a commit message buffer press <kbd>C-c C-c</kbd> or <kbd>SPC m c c</kbd>
to commit the changes with the entered message.
Pressing <kbd>C-c C-k</kbd> or <kbd>SPC m k</kbd> will discard the commit
message.

    Key Binding       |                 Description
----------------------|--------------------------------------------------------
<kbd>h</kbd>          | go left
<kbd>j</kbd>          | go down
<kbd>k</kbd>          | go up
<kbd>l</kbd>          | go right
<kbd>SPC m c c</kbd>  | commit
<kbd>SPC m k</kbd>    | abort

### Interactive rebase buffer

    Key Binding       |                 Description
----------------------|--------------------------------------------------------
<kbd>c</kbd>          | pick
<kbd>e</kbd>          | edit
<kbd>f</kbd>          | fixup
<kbd>h</kbd>          | go left
<kbd>j</kbd>          | go down
<kbd>J</kbd>          | move line down
<kbd>k</kbd>          | go up
<kbd>K</kbd>          | move line up
<kbd>C-k</kbd>        | kill line
<kbd>l</kbd>          | go right
<kbd>r</kbd>          | reword
<kbd>s</kbd>          | squash
<kbd>u</kbd>          | undo
<kbd>x</kbd>          | execute
<kbd>y</kbd>          | insert
<kbd>SPC m c c</kbd>  | rebase
<kbd>SPC m k</kbd>    | abort

### Quick guide for recurring use cases in Magit

- Amend a commit:
  - `L l` to open `log buffer`
  - `c a` on the commit you want to amend
  - `C-c C-c` to submit the changes
- Squash last commit:
  - `L l` to open `log buffer`
  - `E` on the second to last commit, it opens the `rebase buffer`
  - `j` to put point on last commit
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

### Git time machine

[git-timemachine] allows to quickly browse the commits of the current buffer.

    Key Binding     |                 Description
--------------------|------------------------------------------------------------
<kbd>SPC g t</kbd>  | start git timemachine and initiate micro-state
<kbd>c</kbd>        | show current commit
<kbd>n</kbd>        | show next commit
<kbd>N</kbd>        | show previous commit
<kbd>p</kbd>        | show previous commit
<kbd>q</kbd>        | leave micro-state and git timemachine
<kbd>Y</kbd>        | copy current commit hash

### Git gutter

With [git-gutter][] it is possible to navigate between hunks, stage them and
revert them.

Git gutter hunks commands start with `gh`:

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC g h n</kbd>  | next hunk in buffer
<kbd>SPC g h N</kbd>  | previous hunk in buffer
<kbd>SPC g h r</kbd>  | revert current hunk
<kbd>SPC g h s</kbd>  | stage current hunk

`Spacemacs` has custom fringe bitmaps for [git-gutter-fringe][git-gutter]:

           Symbol                | Description
:-------------------------------:|-----------------
![git-new](img/git-new-line.png) | new line
![git-del](img/git-del-line.png) | at least one line has been deleted
![git-mod](img/git-mod-line.png) | modified line

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

### Browse files

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC g f b</kbd>  | browse to file on github
<kbd>SPC g f c</kbd>  | browse to file on github/bitbucket/etc (on current line at commit)
<kbd>SPC g f C</kbd>  | only copy the generated link on the kill ring
<kbd>SPC g f l</kbd>  | browse to file on github/bitbucket/etc (on current line position)
<kbd>SPC g f L</kbd>  | only copy the generated link on the kill ring

**Notes**
- You can use the universal argument `SPC u` to select a remote repository.
- You can use `git-link` on a region.
- When the link is opened, the URL is also copied in the kill ring, you can
override this behavior by setting the variable `git-link-open-in-browser` to
`nil`.

[git]: http://git-scm.com/
[download page]: http://git-scm.com/downloads
[git-gutter]: https://github.com/syohex/emacs-git-gutter-fringe
[magit]: http://magit.github.io/
[official intro]: https://magit.github.io/master/magit.html#Introduction
[Magit-User-Manual#Status]: https://magit.github.io/master/magit.html#Status
[git-flow]: https://github.com/jtatarik/magit-gitflow
[smeargle]: https://github.com/syohex/emacs-smeargle
[git-timemachine]: https://github.com/pidu/git-timemachine
[git-messenger]: https://github.com/syohex/emacs-git-messenger
[magit-gh-pulls]: https://github.com/sigma/magit-gh-pulls
[gist.el]: https://github.com/defunkt/gist.el
[git-link]: https://github.com/sshaw/git-link
[github-browse-file]: https://github.com/osener/github-browse-file
