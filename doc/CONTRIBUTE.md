# Contribute to Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Contribute to Spacemacs](#contribute-to-spacemacs)
    - [Pull Request Guidelines](#pull-request-guidelines)
    - [Submitting a contribution layer](#submitting-a-contribution-layer)
    - [Submitting a banner](#submitting-a-banner)

<!-- markdown-toc end -->

## Pull Request Guidelines

`Spacemacs` branch model is inspired from the [git-flow][] model: You'll have
to submit your contributions and fixes within a pull-request to apply against
the `develop` branch.

_PR = pull request_

**Guidelines:**

1) Ideally and for simple PRs:

- branch from `develop` only
- one topic per PR
- one commit per PR
  - if you have several commits on different topics, close the PR and create
  one PR per topic
  - if you still have several commits, squash them into only one commit
- rebase your PR branch on top of upstream `develop` before submitting the PR

Those PRs are _fast-forwarded_ whenever it's possible and _cherry-picked_
otherwise.

2) For complex pull requests:

- squash only the commits with uninteresting changes like typos, syntax fixes,
etc... and keep the important steps in different commits.

Those PRs are _merged_.
    
**Getting Help:**
If you have any question on this process, join the [gitter chatroom][gitter]
and ask your questions there. It will be a pleasure to help you to contribute!

## Submitting a contribution layer

It is recommended to join a `README.md` file with your layer, ideally this file
should document the packages of your layer as well as the key bindings
associated with them.

You will find a template in `~/.emacs.d/core/templates/layer-README.template`.

Another good practice is to start from the `README.md` of an existing layer.

## Submitting a banner 

The startup banner is randomly chosen among a pool of banners each time
`Spacemacs` starts. Banners are located in directory
`~/.emacs.d/core/banners`.

If you have some ASCII skills you can submit your artwork!

You are free to choose a reasonable height size but the recommended width
size is 75 characters to keep the code simple.

[git-flow]: http://nvie.com/posts/a-successful-git-branching-model/
[gitter]: https://gitter.im/syl20bnr/spacemacs
