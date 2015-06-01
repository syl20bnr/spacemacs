# Auto-Completion configuration layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Auto-Completion configuration layer for Spacemacs](#auto-completion-configuration-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Configuration](#configuration)
        - [Key bindings](#key-bindings)
        - [Tooltips](#tooltips)
        - [Sort results by usage](#sort-results-by-usage)
        - [Enable company or auto-complete globally](#enable-company-or-auto-complete-globally)
        - [Replacing company by auto-complete](#replacing-company-by-auto-complete)
        - [Add auto-completion in a layer](#add-auto-completion-in-a-layer)
    - [Key Bindings](#key-bindings)
        - [Company](#company)
        - [Auto-complete](#auto-complete)
        - [Yasnippet](#yasnippet)
        - [Auto-yasnippet](#auto-yasnippet)

<!-- markdown-toc end -->

## Description

This layer provides auto-completion to Spacemacs.

The following completion engines are supported:
- [company][]
- [auto-complete][]

Snippets are supported via [yasnippet][] and [auto-yasnippet][].

This layer also configures `hippie-expand`.

## Install

To use this configuration layer add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(auto-completion))
```

## Configuration

### Key bindings

You can customize the user experience of auto-completion with the following
layer variables:

`auto-completion-return-key-behavior` set the action to perform when the
<kbd>RET</kbd> key is pressed, the possible values are:
- `complete` completes with the current selection
- `nil` does nothing

`auto-completion-tab-key-behavior` set the action to perform when the
<kbd>TAB</kbd> key is pressed, the possible values are:
- `complete` completes with the current selection
- `cycle` completes the common prefix and cycle between candidates
- `nil` does nothing

`auto-completion-complete-with-key-sequence` is a string of two characters
denoting a key sequence that will perform a `complete` action if the sequence
as been entered quickly enough. If its value is `nil` then the feature is
disabled.

The default configuration of the layer is:

```elisp
(setq-default dotspacemacs-configuration-layers '(
  (auto-completion :variables
                   auto-completion-return-key-behavior 'complete
                   auto-completion-tab-key-behavior 'cycle
                   auto-completion-complete-with-key-sequence nil)
                   ))
```

`"jk"` is a good candidate for `auto-completion-complete-with-key-sequence` if
you don't use it already.

### Tooltips

To enable docstring tooltips set `auto-completion-enable-help-tooltip` to `t`

``` elisp
(setq-default dotspacemacs-configuration-layers
  '(auto-completion :variables
                    auto-completion-enable-help-tooltip t))
```

### Sort results by usage

To enable sorting auto-completion results by their usage frequency set
`auto-completion-enable-sort-by-usage` to `t`.
This feature is provided by the [company-statistics][] package when `company`
is used.
The variable has no effect when `auto-complete` is used.

```elisp
(setq-default dotspacemacs-configuration-layers
  '(auto-completion :variables
                    auto-completion-enable-sort-by-usage t))
```

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
(setq python-packages
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

### Yasnippet

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>M-/<kbd>        | Expand a snippet if text before point is a prefix of a snippet
<kbd>SPC i s</kbd>   | List all current yasnippets for inserting

### Auto-yasnippet

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC i S c</kbd> | create a snippet from an active region
<kbd>SPC i S e</kbd> | Expand the snippet just created with <kbd>SPC i y</kbd>
<kbd>SPC i S w</kbd> | Write the snippet inside `private/snippets` directory for future sessions

[company]: http://company-mode.github.io/
[auto-complete]: http://auto-complete.org/
[yasnippet]: https://github.com/capitaomorte/yasnippet
[auto-yasnippet]: https://github.com/abo-abo/auto-yasnippet
[company-statistics]: https://github.com/company-mode/company-statistics
