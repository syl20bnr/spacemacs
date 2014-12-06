# iedit contribution layer for Spacemacs

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [iedit contribution layer for Spacemacs](#iedit-contribution-layer-for-spacemacs)
    - [Install](#install)
    - [Key bindings](#key-bindings)
        - [State transitions](#state-transitions)
        - [In `iedit state`](#in-iedit-state)
        - [In `iedit-insert state`](#in-iedit-insert-state)
    - [Examples](#examples)
    - [Tips](#tips)

<!-- markdown-toc end -->

This layer replaces [auto-highlight-symbol][] back end by [iedit][].

It comes with two new evil states:
- iedit state
- iedit-insert state

These states color code is `red`.

It has also a nice integration with [expand-region][] for quick edit
of the current selected text by pressing <kbd>e</kbd>.

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(defvar dotspacemacs-configuration-layers '(iedit)
  "List of contribution to load."
)
```

## Key bindings

### State transitions

To `iedit state`:
- <kbd>SPC s e</kbd> initiates the `iedit state` in normal mode.
- <kbd>e</kbd> initiates the `iedit state` while expanding region.
- <kbd>ESC</kbd> in `iedit-insert state` returns to `iedit state`.

To `iedit-insert state`
- evil commands which switch to `insert state` will switch in
`iedit-insert state`.

To `normal state`
- <kbd>ESC</kbd> in `iedit state` returns to `normal state`.
- <kbd>ESC</kbd> in `iedit-insert state` returns to `iedit state`.
- <kbd>C-g</kbd> returns to normal mode
- <kbd>fd</kbd> returns to normal mode (if `evil-escape` is enabled)

To sum-up, in `iedit-insert state` you have to press <kbd>ESC</kbd> twice to
go back to the `normal state`. You can also at any time press <kbd>C-g</kbd>
or <kbd>fd</kbd> to return to `normal state`.

### In `iedit state`

`iedit state` inherits from `normal state`, the following key bindings are
specific to `iedit state`.

    Key Binding   |                 Description
------------------|------------------------------------------------------------
<kbd>ESC</kbd>    | go back to `normal state`
<kbd>#</kbd>      | prefix all occurrences with an increasing number (<kbd>SPC u</kbd> to choose the starting number).
<kbd>d</kbd>      | delete the occurrences
<kbd>D</kbd>      | down-case the occurrences
<kbd>F</kbd>      | restrict the scope to the function
<kbd>gg</kbd>     | go to first occurrence
<kbd>G</kbd>      | go to last occurrence
<kbd>I</kbd>      | toggle case-sensitivity
<kbd>J</kbd>      | increase the edition scope by one line below
<kbd>K</kbd>      | increase the edition scope by one line above
<kbd>L</kbd>      | restrict the scope to the current line
<kbd>n</kbd>      | go to next occurrence
<kbd>N</kbd>      | go to previous occurrence
<kbd>p</kbd>      | replace occurrences with last yanked (copied) text
<kbd>S</kbd>      | (substitute) delete the occurrences and switch to `iedit-insert state`
<kbd>V</kbd>      | toggle visibility of lines with no occurrence
<kbd>U</kbd>      | Up-case the occurrences

### In `iedit-insert state`

    Key Binding            |                 Description
---------------------------|------------------------------------------------------------
<kbd>ESC</kbd>             | go back to `iedit state`
<kbd>C-g</kbd>             | go back to `normal state`

## Examples

- manual selection of several words then replace: <kbd>v w w SPC s e S "toto" ESC ESC</kbd>
- append text to a word on two lines: <kbd>v i w SPC s e J i "toto" ESC ESC</kbd>
- substitute symbol _with expand-region_: <kbd>SPC v v e S "toto" ESC ESC</kbd>
- replace symbol with yanked (copied) text _with expand region_: <kbd>SPC v e p ESC ESC</kbd>

## Todo

- Make `A` append to the end of the edited region.
- Make `I` insert to the beginning of the edited region.
- Make `0` go to the beginning of the edited region.
- Make `$` go to the end of the edited region.

[auto-highlight-symbol]: https://github.com/gennad/auto-highlight-symbol
[iedit]: https://github.com/tsdh/iedit
[expand-region]: https://github.com/magnars/expand-region.el
