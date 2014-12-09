# Spacemacs conventions

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Spacemacs conventions](#spacemacs-conventions)
    - [Code guidelines](#code-guidelines)
        - [Spacemacs core and layer](#spacemacs-core-and-layer)
        - [All layers](#all-layers)
    - [Key bindings conventions](#key-bindings-conventions)
        - [Prefix reserved to the user](#prefix-reserved-to-the-user)
        - [Prefix reserved to the current major mode](#prefix-reserved-to-the-current-major-mode)
        - [Interactions with REPLs](#interactions-with-repls)
        - [Interactions with Tests](#interactions-with-tests)
            - [For all languages](#for-all-languages)
            - [Depending on the language](#depending-on-the-language)
        - [Debugging](#debugging)
        - [Evilify buffers](#evilify-buffers)
        - [Navigation in `insert state` buffers](#navigation-in-insert-state-buffers)

<!-- markdown-toc end -->

## Code guidelines

### Spacemacs core and layer

Function names follow these conventions:
- `spacemacs/xxx` is an interactive function called `xxx`
- `spacemacs//xxx` is a private function called `xxx` (implementation details)
- `spacemacs|xxx` is a _macro_ called `xxx`

Variables follow these conventions:
- `spacemacs-xxx` is a variable
- `spacemacs--xxx` is a private variable (implementation details)

### All layers

A package is initialized in a function with name `<layer>/init-xxx` where:
- `<layer>` is the layer name
- `xxx` is the package name

## Key bindings conventions

### Prefix reserved to the user

`<SPC> o` must not be used by any layer. It is reserved for the user.

### Prefix reserved to the current major mode

`<SPC> m` is reserved for the current major mode.

### Interactions with REPLs

A lot of languages can interact with a REPL. To help keeping a consistent
behavior between those languages the following conventions should be
followed:
- `<SPC> m s` is the prefix for sending code.
- lower case key bindings keep the focus on the current buffer
- upper case key bindings move the focus to the REPL buffer

    Key           |                 Description
------------------|------------------------------------------------------------
<kbd>m s b</kbd>  | send buffer
<kbd>m s B</kbd>  | send buffer and switch to REPL
<kbd>m s d</kbd>  | first key to send buffer and switch to REPL to debug (step)
<kbd>m s D</kbd>  | second key to send buffer and switch to REPL to debug (step)
<kbd>m s f</kbd>  | send function
<kbd>m s F</kbd>  | send function and switch to REPL
<kbd>m s l</kbd>  | send line
<kbd>m s L</kbd>  | send line and switch to REPL
<kbd>m s r</kbd>  | send region
<kbd>m s R</kbd>  | send region and switch to REPL

Note: we don't distinguish between the file and the buffer.

### Interactions with Tests

A lot of languages have their own test frameworks. These frameworks share
common actions that we can unite under the same key bindings:
- `<SPC> m t` is the prefix for test execution.
- `<SPC> m T` is the prefix for test execution in debug mode (if supported).

#### For all languages

    Key           |                 Description
------------------|------------------------------------------------------------
<kbd>m t a</kbd>  | execute all the tests of the current project
<kbd>m t b</kbd>  | execute all the tests of the current buffer
<kbd>m t f</kbd>  | execute the current test (function)

Note: we don't distinguish between the file and the buffer. We can implement
an auto-save of the buffer before executing the tests.

#### Depending on the language

    Key           |                 Description
------------------|------------------------------------------------------------
<kbd>m t m</kbd>  | execute the tests of the current module
<kbd>m t s</kbd>  | execute the tests of the current suite

Note that there are overlaps, depending on the language we will choose one
or more bindings for the same thing

### Debugging

    Key           |                 Description
------------------|------------------------------------------------------------
<kbd>m b</kbd>    | toggle a breakpoint

**TBD**

### Evilify buffers

`Spacemacs` offers convenient functions to _evilify_ a buffer.
_Evilifying_ a buffer is to:
- add `hjkl` navigation
- add incremental search with `/`, `n` and `N`
- add `visual state` and `visual line state`
- activate evil-leader key
- fix all bindings shadows by the above additions

To fix the shadowed bindings we capitalize them, for instance:
shadowed `h` is transposed to `H`, if `H` is taken then it is
transposed to `C-h` and so on...

Example of _evilified_ buffers are `magit status`, `paradox buffer`.

The related functions are:
- `spacemacs/activate-evil-leader-for-maps` and `spacemacs/activate-evil-leader-for-map`
- `spacemacs/evilify`

### Navigation in `insert state` buffers

Navigation in buffers like `Helm` and `ido` which are in `insert state` should
be performed with <kbd>C-j</kbd> and <kdb>C-k</kbd> for vertical movements.

History navigation in shells or REPLs buffers should be bound as well to
<kbd>C-j</kbd> and <kdb>C-k</kbd>.
