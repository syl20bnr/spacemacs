# Contribute to Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Contribute to Spacemacs](#contribute-to-spacemacs)
    - [Pull Request Guidelines](#pull-request-guidelines)
    - [Submitting a contribution layer upstream](#submitting-a-contribution-layer-upstream)

<!-- markdown-toc end -->

## Pull Request Guidelines

`Spacemacs` uses the `git-flow` model, so you'll have to submit your
contributions and fixes within a pull-request to apply against the `develop`
branch.

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
and ask your questions there. Do not hesitate to ask your questions even the
simplest one, it will be a pleasure to help you to contribute!

## Submitting a contribution layer upstream

It is recommended to join a `README.md` file with your layer, ideally this file
should document the packages of your layer as well as the key bindings
associated with them. 

To submit your contribution layer follow the above
[guidelines](#pull-request-guidelines) for pull requests.

**Note:** by submitting a configuration layer you become the maintainer of it.
