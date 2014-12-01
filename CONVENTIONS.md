# Spacemacs conventions

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Spacemacs conventions](#spacemacs-conventions)
    - [Code guidelines](#code-guidelines)
        - [Spacemacs core and layer](#spacemacs-core-and-layer)
        - [All layers](#all-layers)
    - [Key bindings conventions](#key-bindings-conventions)
        - [Prefix reserved to the user](#prefix-reserved-to-the-user)
        - [Navigation in `insert state` buffers](#navigation-in-insert-state-buffers)
        - [Interactions with REPLs](#interactions-with-repls)

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

### Navigation in `insert state` buffers

Navigation in buffers like `Helm` and `ido` which are in `insert state` should
be performed with <kbd>C-j</kbd> and <kdb>C-k</kbd> for vertical movements.

History navigation in shells or REPLs buffers should be bound as well to
<kbd>C-j</kbd> and <kdb>C-k</kbd>.

### Interactions with REPLs

A lot of languages can interact with a REPL. To help keeping a consistent
behavior between those languages the following conventions should be
followed:
- lower case key bindings keep the focus on the current buffer
- upper case key bindings move the focus to the REPL buffer

    Key           |                 Description
------------------|------------------------------------------------------------
<kbd>b</kbd>      | evaluate buffer
<kbd>B</kbd>      | evaluate buffer and switch to REPL
<kbd>f</kbd>      | evaluate function
<kbd>F</kbd>      | evaluate function and switch to REPL
<kbd>l</kbd>      | evaluate line
<kbd>L</kbd>      | evaluate line and switch to REPL
<kbd>r</kbd>      | evaluate region
<kbd>R</kbd>      | evaluate region and switch to REPL

