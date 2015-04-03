# Auto-Completion configuration layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Auto-Completion configuration layer for Spacemacs](#auto-completion-configuration-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Company variables](#company-variables)
    - [Key Bindings](#key-bindings)
        - [Company](#company)
        - [Auto-complete](#auto-complete)

<!-- markdown-toc end -->

## Description

This layer provides auto-completion to Spacemacs.
The following front-ends are supported:
- [company][]
- [auto-complete][]

**Notes***
- `company` is the most supported and preferred front-end in Spacemacs.
- For a given language, Spacemacs supports one and only one front-end.

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(auto-completion))
```

### Company variables

To use tab instead of enter to complete your selection,
`dotspacemacs/init` set `auto-completion-use-tab-instead-of-enter` to
`t`, for example:

``` elisp
(setq-default dotspacemacs-configuration-layers
  '(auto-completion :variables
                    auto-completion-use-tab-instead-of-enter t))
```

To enable docstring tooltips set `auto-completion-enable-company-help-tooltip`
to `t`

``` elisp
(setq-default dotspacemacs-configuration-layers
  '(auto-completion :variables
                    auto-completion-enable-company-help-tooltip t))
```

## Key Bindings

### Company

    No Debug         |                 Description
---------------------|------------------------------------------------------------
<kbd>C-j</kbd>       | go down in company dropdown menu
<kbd>C-k</kbd>       | go up in company dropdown menu
<kbd>C-/</kbd>       | search in company dropdown
<kbd>C-M-/</kbd>     | filter the company dropdown menu
<kbd>C-d</kbd>       | open minibuffer with documentation of thing at point in company dropdown

### Auto-complete
 
    Key Binding    |                 Description
-------------------|------------------------------------------------------------
<kbd>C-j</kbd>     | select next candidate
<kbd>C-k</kbd>     | select previous candidate
<kbd>TAB</kbd>     | expand selection or select next candidate
<kbd>S-TAB</kbd>   | select previous candidate
<kbd>return</kbd>  | complete word, if word is already completed insert a carriage return

[company]: http://company-mode.github.io/
[auto-complete]: http://auto-complete.org/
