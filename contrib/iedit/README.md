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

- <kbd>SPC s e</kbd> initiates the `iedit state` in normal mode.
- <kbd>e</kbd> initiates the `iedit state` while expanding region.
- <kbd>i</kbd> in `iedit state` triggers the `iedit-insert state`.
- <kbd>ESC</kbd> in `iedit state` returns to `normal state`.
- <kbd>ESC</kbd> in `iedit-insert state` returns to `iedit state`.
- <kbd>Shift-Return</kbd> in `iedit-insert state` ends the edition and returns to `normal state`.

### In `iedit state`

    Key Binding                 |                 Description
--------------------------------|------------------------------------------------------------
<kbd>#</kbd>                    | prefix all occurrences with an increasing number
<kbd>b</kbd>                    | make all occurrences blank
<kbd>B</kbd>                    | toggle buffering (use it for large buffers with a lof of occurrences)
<kbd>c</kbd> or <kbd>r</kbd>    | delete the occurrences and switch to `iedit-insert state`
<kbd>d</kbd>                    | delete the occurrences
<kbd>D</kbd>                    | down-case the occurrences
<kbd>f</kbd>                    | restrict the scope to the function
<kbd>"gg"</kbd>                 | go to first occurrence
<kbd>G</kbd>                    | go to last occurrence
<kbd>i</kbd>                    | switch to `iedit-insert state`
<kbd>I</kbd>                    | toggle case-sensitivity
<kbd>j</kbd>                    | increase the edition scope by one line below
<kbd>k</kbd>                    | increase the edition scope by one line above
<kbd>l</kbd>                    | restrict the scope to the current line
<kbd>n</kbd>                    | go to next occurrence
<kbd>N</kbd>                    | go to previous occurrence
<kbd>v</kbd>                    | toggle visibility of lines with no occurrence
<kbd>u</kbd>                    | undo (for convenience)
<kbd>U</kbd>                    | Up-case the occurrences

### In `iedit-insert state`

    Key Binding            |                 Description
---------------------------|------------------------------------------------------------
<kbd>Shift Return</kbd>    | end edition
<kbd>ESC</kbd>             | go back to `iedit state`

## Examples

- manual selection of several words then replace: <kbd>v w w SPC s e r "toto" Shift-Return</kbd>
- replace symbol _with expand-region_: <kbd>SPC v v e r "toto" Shift-Return</kbd>
- append text to a word on two lines: <kbd>SPC v i w SPC s e j i "toto" Shift-Return</kbd>

## Tips

The first <kbd>N</kbd> put the cursor at the beginning of the edited selection.

[auto-highlight-symbol]: https://github.com/gennad/auto-highlight-symbol
[iedit]: https://github.com/tsdh/iedit
[expand-region]: https://github.com/magnars/expand-region.el
