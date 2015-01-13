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
        - [Navigation between items](#navigation-between-items)
        - [In buffer evaluation of code](#in-buffer-evaluation-of-code)
        - [Interactions with REPLs](#interactions-with-repls)
        - [Interactions with Tests](#interactions-with-tests)
            - [For all languages](#for-all-languages)
            - [Depending on the language](#depending-on-the-language)
        - [Building and Compilation](#building-and-compilation)
        - [Debugging](#debugging)
        - [Code navigation](#code-navigation)
        - [Getting Help or Documentation](#getting-help-or-documentation)
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

<kbd>SPC o</kbd> must not be used by any layer. It is reserved for the user.

### Prefix reserved to the current major mode

<kbd>SPC m</kbd> is reserved for the current major mode. Three keys bindings
are not an issue (ie. <kbd>SPC m h d</kbd>) since <kbd>SPC m</kbd> can be
accessed via <kbd>,</kbd>.

### Navigation between items

To be consistent with the Vim way, <kbd>n</kbd> and <kbd>N</kbd> are favored
over Emacs <kbd>n</kbd> and <kbd>p</kbd>.

Ideally a micro-state should be provided to smooth the navigation experience.
A micro-state allows to repeat key bindings without entering each time the
prefix commands.
More info on micro-states in the [documentation](DOCUMENTATION.md#micro-states).

### In buffer evaluation of code

Live evaluation of code is under the prefix `<SPC> e`.

    Key           |                 Description
------------------|------------------------------------------------------------
<kbd>e $</kbd>    | put the point at the end of the line and evaluate
<kbd>e b</kbd>    | evaluate buffer
<kbd>e e</kbd>    | evaluate last expression
<kbd>e f</kbd>    | evaluate function
<kbd>e l</kbd>    | evaluate line
<kbd>e r</kbd>    | evaluate region


### Interactions with REPLs

A lot of languages can interact with a REPL. To help keeping a consistent
behavior between those languages the following conventions should be
followed:
- `<SPC> m s` is the prefix for sending code. This allows fast
interaction with the REPL whenever it is possible (don't forget that `,` is
a shortcut for `<SPC> m`).
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
<kbd>m t t</kbd>  | execute the current test (thing at point, function)

Note: we don't distinguish between the file and the buffer. We can implement
an auto-save of the buffer before executing the tests.

#### Depending on the language

    Key           |                 Description
------------------|------------------------------------------------------------
<kbd>m t m</kbd>  | execute the tests of the current module
<kbd>m t s</kbd>  | execute the tests of the current suite

Note that there are overlaps, depending on the language we will choose one
or more bindings for the same thing

### Building and Compilation

The base prefix for major mode specific compilation is <kbd>SPC m c</kbd>.

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>m c c</kbd>     | compile
<kbd>m c r</kbd>     | recompile

### Debugging

The base prefix for debugging commands is <kbd>SPC d</kbd>.

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>m d a</kbd>     | abandon current process
<kbd>m d b</kbd>     | toggle a breakpoint
<kbd>m d c</kbd>     | continue current process
<kbd>m d d</kbd>     | start debug process
<kbd>m d n</kbd>     | next breakpoint
<kbd>m d N</kbd>     | previous breakpoint

Notes:
- Ideally a micro-state for breakpoint navigation should be provided.
- If there is no toggle breakpoint function, then it should be implemented at
the spacemacs level and ideally the function should be proposed as a patch
upstream (major mode repository).

### Code navigation

    Key           |                 Description
------------------|------------------------------------------------------------
<kbd>m g</kbd>    | go to definition of thing under point

### Getting Help or Documentation

The base prefix for help commands is <kbd>SPC h</kbd>. Documentation is
considered as an help command.

    Key           |                 Description
------------------|------------------------------------------------------------
<kbd>m h d</kbd>  | documentation of thing under point

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
be performed with <kbd>C-j</kbd> and <kbd>C-k</kbd> bindings for vertical
movements.

    Key         |                 Description
----------------|------------------------------------------------------------
<kbd>C-j</kbd>  | go down
<kbd>C-k</kbd>  | go up

History navigation in shells or REPLs buffers should be bound as well to
<kbd>C-j</kbd> and <kbd>C-k</kbd>.

    Key         |                 Description
----------------|------------------------------------------------------------
<kbd>C-j</kbd>  | next item in history
<kbd>C-k</kbd>  | previous item in history
