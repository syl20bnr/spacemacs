# evil-escape
[![MELPA](http://melpa.org/packages/evil-escape-badge.svg)](http://melpa.org/#/evil-escape)
[![MELPA Stable](http://stable.melpa.org/packages/evil-escape-badge.svg)](http://stable.melpa.org/#/evil-escape)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [evil-escape](#evil-escape)
    - [Install](#install)
    - [Usage](#usage)
    - [Customization](#customization)
        - [Key sequence](#key-sequence)
        - [Delay between keys](#delay-between-keys)
    - [Limitations](#limitations)

<!-- markdown-toc end -->

Customizable key sequence to escape from insert state and everything else in
Emacs.

Press quickly `fd` (or the 2-keys sequence of your choice) to:

- escape from all stock evil states to normal state
- escape from evil-lisp-state to normal state
- escape from evil-iedit-state to normal state
- abort evil ex command
- quit minibuffer
- abort isearch
- quit magit buffers
- quit help buffers
- quit apropos buffers
- quit ert buffers
- quit undo-tree buffer
- quit paradox
- quit gist-list menu
- hide neotree buffer

And more to come !

Contributions to support more buffers are _very welcome_:
**Escape Everything !**

## Install

The package _will be available soon_ in [MELPA][].

If you have MELPA in `package-archives`, use

    M-x package-install RET evil-escape RET

If you don't, open `evil-escape.el` in Emacs and call
`package-install-from-buffer`.

## Usage

To toggle the `evil-escape` mode globally:

    M-x evil-escape-mode

## Customization

### Key sequence

The key sequence can be customized with the variable `evil-escape-key-sequence`.
For instance to change it for `jk`:

```elisp
(setq-default evil-escape-key-sequence "jk")
```

**Note:** The variable `evil-escape-key-sequence` must be set before requiring
`evil-escape`.

### Delay between keys

The delay between the two key presses can be customized with the variable
`evil-escape-delay`. The default value is `0.1`. If your key sequence is
composed with the two same characters it is recommended to set the delay to
`0.2`.

```elisp
(setq-default evil-escape-delay 0.2)
```

**Note:** The variable `evil-escape-delay` must be set before requiring
`evil-escape`.

## Limitations

Due to the current implementation only the following characters can be used
as the first character of the key sequence:

|   Keys     |
|:----------:|
|<kbd>b</kbd>|
|<kbd>B</kbd>|
|<kbd>e</kbd>|
|<kbd>E</kbd>|
|<kbd>f</kbd>|
|<kbd>F</kbd>|
|<kbd>G</kbd>|
|<kbd>h</kbd>|
|<kbd>H</kbd>|
|<kbd>j</kbd>|
|<kbd>k</kbd>|
|<kbd>l</kbd>|
|<kbd> </kbd>|
|<kbd>K</kbd>|
|<kbd>L</kbd>|
|<kbd>M</kbd>|
|<kbd>n</kbd>|
|<kbd>N</kbd>|
|<kbd>t</kbd>|
|<kbd>T</kbd>|
|<kbd>w</kbd>|
|<kbd>W</kbd>|
|<kbd>{</kbd>|
|<kbd>}</kbd>|
|<kbd>#</kbd>|
|<kbd>%</kbd>|
|<kbd>`</kbd>|
|<kbd>'</kbd>|
|<kbd>(</kbd>|
|<kbd>)</kbd>|
|<kbd>*</kbd>|
|<kbd>,</kbd>|
|<kbd>/</kbd>|
|<kbd>;</kbd>|
|<kbd>?</kbd>|
|<kbd>|</kbd>|
|<kbd>^</kbd>|
|<kbd>+</kbd>|
|<kbd>_</kbd>|
|<kbd>-</kbd>|

[MELPA]: http://melpa.org/
