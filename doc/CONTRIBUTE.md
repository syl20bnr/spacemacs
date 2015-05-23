# Contribute to Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Contribute to Spacemacs](#contribute-to-spacemacs)
    - [Pull Request Guidelines](#pull-request-guidelines)
    - [Submitting a configuration layer](#submitting-a-configuration-layer)
    - [Submitting a banner](#submitting-a-banner)
    - [Credits](#credits)
        - [License](#license)
        - [File header](#file-header)
        - [Author of a contribution layer](#author-of-a-contribution-layer)
        - [Contributor of a contribution layer](#contributor-of-a-contribution-layer)

<!-- markdown-toc end -->

## Pull Request Guidelines

`Spacemacs` branch model is inspired from the [git-flow][] model: You'll have
to submit your contributions and fixes within a pull-request to apply against
the `develop` branch.

_PR = pull request_

**Guidelines:**

1) Ideally and for _simple_ PRs:

- branch from `develop`
- one topic per PR
- one commit per PR
  - if you have several commits on different topics, close the PR and create
  one PR per topic
  - if you still have several commits, squash them into only one commit
- rebase your PR branch on top of upstream `develop` before submitting the PR

Those PRs are _fast-forwarded_ whenever it's possible and _cherry-picked_
otherwise (most likely they will be cherry-picked).

2) For complex pull requests:

- squash only the commits with uninteresting changes like typos, syntax fixes,
etc... and keep the important and _isolated_ steps in different commits.

Those PRs are _merged_ and explicitly _not fast-forwarded_.
    
**Getting Help:**
If you have any question on this process, join the [gitter chatroom][gitter]
and ask your questions there. It will be a pleasure to help you to contribute!

## Submitting a configuration layer

Contributed configuration layers are stored in the `contrib` folder.
The `contrib` folder also contains categories prefixed with `!` to put your
layers in. For example a layer for a language would go in the `contrib/!lang` folder.

It is recommended to join a `README.md` file with your layer:
- ideally this file should document the packages of your layer as well as
the key bindings associated with them,
- a template is provided in 
`~/.emacs.d/core/templates/layer-README.template`, use it as much as possible,
- another good practice is to start from the `README.md` of an existing layer,
- add a TOC at the start of the file with the command
`markdown-toc/generate-toc`,
- if a logo exists for the layer you can add it at the top of the `README.md`
before the TOC. The maximum recommended height is 200 pixels.

## Submitting a banner 

The startup banner is by default randomly chosen among a pool of banners each
time `Spacemacs` starts. Banners are located in directory
`~/.emacs.d/core/banners`.

If you have some ASCII skills you can submit your artwork!

You are free to choose a reasonable height size but the width size should be
around 75 characters.

## Credits

### License

The license is GPLv3 for all parts specific to `Spacemacs`, this includes:
- the initialization and core files
- all the layer files.

For files not belonging to `Spacemacs` like extensions and libraries, refer
to the header file. Those files should not have an empty header, please
report any file imported in `Spacemacs` without a proper header.

### File header

Template:

```elisp
;;; extensions.el --- NAME Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
```

### Author of a contribution layer

In the file header:
- change `NAME` to the name of the layer,
- change the default author name `Sylvain Benner` to your name,
- do not remove the line: `;; Copyright (c) 2012-2014 Sylvain Benner`
- modify the second copyright line by replacing the default name and dates,
**keep** `& Contributors` in this line,
- other lines should not be modified

### Contributor of a contribution layer

You should not modify any header file. A very cool way to show your
contributions will be available in Spacemacs at some point, _Stay Tuned_.

[git-flow]: http://nvie.com/posts/a-successful-git-branching-model/
[gitter]: https://gitter.im/syl20bnr/spacemacs
