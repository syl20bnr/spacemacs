# github contribution layer for Spacemacs

![github](https://raw.githubusercontent.com/syl20bnr/spacemacs/master/contrib/github/github.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [github contribution layer for Spacemacs](#github-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)
        - [magit-gh-pulls](#magit-gh-pulls)
        - [gist.el](#gistel)

<!-- markdown-toc end -->

## Description

This layer installs packages to interface Github services or features:
- [magit-gh-pulls][]: handy `magit` add-on to manage Github pull requests.
- [gist.el][]: full-featured mode to browse and post Githug gists.

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(defvar dotspacemacs-configuration-layers '(github)
  "List of contribution to load."
)
```

## Key bindings

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

 [magit-gh-pulls]: https://github.com/sigma/magit-gh-pulls
 [gist.el]: https://github.com/defunkt/gist.el
