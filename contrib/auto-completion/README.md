# Auto-Completion configuration layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Auto-Completion configuration layer for Spacemacs](#auto-completion-configuration-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Company variables](#company-variables)
    - [Configure](#configure)
        - [Enable company or auto-complete globally](#enable-company-or-auto-complete-globally)
        - [Replacing company by auto-complete](#replacing-company-by-auto-complete)
        - [Add auto-completion in a layer](#add-auto-completion-in-a-layer)
    - [Key Bindings](#key-bindings)
        - [Company](#company)
        - [Auto-complete](#auto-complete)

<!-- markdown-toc end -->

## Description

This layer provides auto-completion to Spacemacs.

The following completion engines are supported:
- [company][]
- [auto-complete][]

## Install

To use this configuration layer add it to your `~/.spacemacs`

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

## Configure

### Enable company or auto-complete globally

By default Spacemacs enables auto-completion explicitly for each supported
major-mode, it means that `company` and `auto-complete` are not enabled
globally, it allows more flexibility to choose an auto-completion engine
for a given mode.

You may want to enable company globally to get auto-completion
everywhere even in the modes which are not configured by Spacemacs. To do
so, you just have to add `(global-company-mode)` in the
`dotspacemacs/config` function of your dotfile.

Note that if you want to enable `auto-complete` globally you will have to
disable `company` first, see the next section to do so.

### Replacing company by auto-complete

You can disable `company` by adding it to the `dotspacemacs-excluded-packages`
variable, then you are free to enable `auto-complete` globally.

### Add auto-completion in a layer

Here is an example to add `company` auto-completion to python buffer:

In `config.el`:

```elisp
;; Define the buffer local company backend variable
(spacemacs|defvar-company-backends python-mode)
```

In `packages.el`:

```elisp
;; Add the relevant packages to the layer
(defvar python-packages
  '(...
    company
    company-anaconda
    ...))

;; Configure the packages
(when (configuration-layer/layer-usedp 'auto-completion)

  ;; Hook company to python-mode
  (defun python/post-init-company ()
    (spacemacs|add-company-hook python-mode))

  ;; Add the backend to the major-mode specific backend list
  (defun python/init-company-anaconda ()
    (use-package company-anaconda
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :init (push 'company-anaconda company-backends-python-mode))))
```

## Key Bindings

### Company

    Key Binding      |                 Description
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
