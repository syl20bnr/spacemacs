# Spacemacs conventions

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Spacemacs conventions](#spacemacs-conventions)
    - [Code guidelines](#code-guidelines)
        - [Spacemacs core and layer](#spacemacs-core-and-layer)
        - [All layers](#all-layers)
    - [Key bindings conventions](#key-bindings-conventions)
        - [Reserved prefix](#reserved-prefix)
            - [User prefix](#user-prefix)
            - [Major mode prefix](#major-mode-prefix)
            - [Micro-state](#micro-state)
        - [Evilify buffers](#evilify-buffers)
        - [Navigation](#navigation)
            - [n and N](#n-and-n)
            - [Code Navigation](#code-navigation)
            - [`insert state` buffers](#insert-state-buffers)
        - [Evaluation](#evaluation)
        - [REPLs](#repls)
            - [Send code](#send-code)
            - [In terminal](#in-terminal)
        - [Building and Compilation](#building-and-compilation)
        - [Debugging](#debugging)
        - [Plain Text Markup Languages](#plain-text-markup-languages)
            - [Headers](#headers)
            - [Insertion of common elements](#insertion-of-common-elements)
            - [Text manipulation](#text-manipulation)
            - [Movement in normal mode](#movement-in-normal-mode)
            - [Promotion, Demotion and element movement](#promotion-demotion-and-element-movement)
            - [Table editing](#table-editing)
        - [Tests](#tests)
            - [All languages](#all-languages)
            - [Language specific](#language-specific)
        - [Refactoring](#refactoring)
        - [Help or Documentation](#help-or-documentation)

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

### Reserved prefix

#### User prefix

<kbd>SPC o</kbd> must not be used by any layer. It is reserved for the user.

#### Major mode prefix

<kbd>SPC m</kbd> is reserved for the current major mode. Three keys bindings
are not an issue (ie. <kbd>SPC m h d</kbd>) since <kbd>SPC m</kbd> can be
accessed via <kbd>,</kbd>.

#### Micro-state

Whenever possible a micro-state should be enabled with <kbd>M-SPC</kbd> and
<kbd>s-M-SPC</kbd>. We need the latter bindings on OS X since <kbd>M-SPC</kbd>
is used by the OS for spotlight.

For instance micro-states dedicated to special buffers like `helm` or `ido`
buffers are good candidates to be put on <kbd>M-SPC</kbd> and
<kbd>s-M-SPC</kbd>.

It is recommended to add <kbd>q</kbd> to leave the micro-state.

### Evilify buffers

`Spacemacs` offers convenient functions to _evilify_ a buffer.
_Evilifying_ a buffer is to set the `evilified state` as the default
state for the major mode of the buffer.

The `evilified state` is derived from the `emacs state` and modify the
map to:
- add `hjkl` navigation
- add incremental search with `/`, `n` and `N`
- add `visual state` and `visual line state`
- add yank (copy) with `y`
- activate evil-leader key

Setting the `evilified state` to a mode is done by calling the macro `evilify`
which takes optional parameters to fix the key bindings shadowed by the above
modifications.

To fix the shadowed bindings we capitalize them, for instance:
shadowed `h` is transposed to `H`, if `H` is taken then it is
transposed to `C-h` and so on...

Example of _evilified_ buffers are `magit status`, `paradox buffer`.

### Navigation

#### n and N

To be consistent with the Vim way, <kbd>n</kbd> and <kbd>N</kbd> are favored
over Emacs <kbd>n</kbd> and <kbd>p</kbd>.

Ideally a micro-state should be provided to smooth the navigation experience.
A micro-state allows to repeat key bindings without entering each time the
prefix commands.
More info on micro-states in the [documentation](DOCUMENTATION.md#micro-states).

#### Code Navigation

The prefix for going to something is `<SPC> m g`.

    Key           |                 Description
------------------|------------------------------------------------------------
<kbd>m g a</kbd>  | go to alternate file (i.e. `.h <--> .cpp`)
<kbd>m g g</kbd>  | go to things under point
<kbd>m g G</kbd>  | go to things under point in other window
<kbd>m g t</kbd>  | go to corresponding test file if any

#### `insert state` buffers

Navigation in buffers like `Helm` and `ido` which are in `insert state` should
be performed with <kbd>C-j</kbd> and <kbd>C-k</kbd> bindings for vertical
movements.

    Key         |                 Description
----------------|------------------------------------------------------------
<kbd>C-j</kbd>  | go down
<kbd>C-k</kbd>  | go up

### Evaluation

Live evaluation of code is under the prefix `<SPC> m e`.

    Key           |                 Description
------------------|------------------------------------------------------------
<kbd>m e $</kbd>  | put the point at the end of the line and evaluate
<kbd>m e b</kbd>  | evaluate buffer
<kbd>m e e</kbd>  | evaluate last expression
<kbd>m e f</kbd>  | evaluate function
<kbd>m e l</kbd>  | evaluate line
<kbd>m e r</kbd>  | evaluate region

### REPLs

#### Send code

A lot of languages can interact with a REPL. To help keeping a consistent
behavior between those languages the following conventions should be
followed:
- `<SPC> m s` is the prefix for sending code. This allows fast
interaction with the REPL whenever it is possible
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
<kbd>m s i</kbd>  | start/switch to REPL inferior process
<kbd>m s l</kbd>  | send line
<kbd>m s L</kbd>  | send line and switch to REPL
<kbd>m s r</kbd>  | send region
<kbd>m s R</kbd>  | send region and switch to REPL

Note: we don't distinguish between the file and the buffer.

#### In terminal

History navigation in shells or REPLs buffers should be bound as well to
<kbd>C-j</kbd> and <kbd>C-k</kbd>.

    Key         |                 Description
----------------|------------------------------------------------------------
<kbd>C-j</kbd>  | next item in history
<kbd>C-k</kbd>  | previous item in history
<kbd>C-l</kbd>  | clear screen
<kbd>C-r</kbd>  | search backward in history

### Building and Compilation

The base prefix for major mode specific compilation is <kbd>SPC m c</kbd>.

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>m c b</kbd>     | compile buffer
<kbd>m c c</kbd>     | compile
<kbd>m c r</kbd>     | clean and compile

Note: we don't distinguish between the file and the buffer. We can implement
an auto-save of the buffer before compiling the buffer.

### Debugging

The base prefix for debugging commands is <kbd>SPC d</kbd>.

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>m d a</kbd>     | abandon current process
<kbd>m d b</kbd>     | toggle a breakpoint
<kbd>m d B</kbd>     | clear all breakpoints
<kbd>m d c</kbd>     | continue
<kbd>m d d</kbd>     | start debug session
<kbd>m d i</kbd>     | inspect value at point
<kbd>m d l</kbd>     | local variables
<kbd>m d n</kbd>     | next
<kbd>m d r</kbd>     | run
<kbd>m d s</kbd>     | step

Notes:
- Ideally a micro-state for breakpoint navigation should be provided.
- If there is no toggle breakpoint function, then it should be implemented at
the spacemacs level and ideally the function should be proposed as a patch
upstream (major mode repository).

### Plain Text Markup Languages

For layers supporting markup languages please follow the following keybindings
whenever applicable.

#### Headers

All header functionality should be grouped under <kbd>SPC m h</kbd>

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>m h i</kbd>     | Insert a header 
<kbd>m h I</kbd>     | Insert a header alternative method (if existing) 
<kbd>m h 1..10</kbd> | Insert a header of level 1..10 (if possible)

#### Insertion of common elements

Insertion of common elements like links or footnotes should be grouped
under <kbd>SPC m i</kbd>

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>m i f</kbd>     | Insert footnote
<kbd>m i i</kbd>     | Insert image
<kbd>m i l</kbd>     | Insert link
<kbd>m i u</kbd>     | Insert url
<kbd>m i w</kbd>     | Insert wiki-link

#### Text manipulation

Manipulation of text regions should be grouped under <kbd>SPC m r</kbd>

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>m x b</kbd>     | Make region bold
<kbd>m x c</kbd>     | Make region code
<kbd>m x i</kbd>     | Make region italic
<kbd>m x q</kbd>     | Quote a region
<kbd>m x r</kbd>     | Remove formatting from region
<kbd>m x s</kbd>     | Make region strike-through
<kbd>m x u</kbd>     | Make region underlined
<kbd>m x v</kbd>     | Make region verbose

#### Movement in normal mode

In normal mode Vim style movement should be enabled with these keybindings:

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>g h</kbd>       | Move up one level in headings
<kbd>g j</kbd>       | Move to next heading on same level
<kbd>g k</kbd>       | Move to previous heading on same level
<kbd>g l</kbd>       | Move down one level in headings

#### Promotion, Demotion and element movement

Promotion, demotion and movement of headings or list elements (whatever
is possible) should be enabled with the following keys in any mode

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>M-h</kbd>       | Promote heading by one level
<kbd>M-j</kbd>       | Move element down
<kbd>M-k</kbd>       | Move element up
<kbd>M-l</kbd>       | Demote heading by one level

#### Table editing

If table specific commands are available the they are grouped under the
<kbd>SPC m t</kbd> group.

### Tests

A lot of languages have their own test frameworks. These frameworks share
common actions that we can unite under the same key bindings:
- `<SPC> m t` is the prefix for test execution.
- `<SPC> m T` is the prefix for test execution in debug mode (if supported).

#### All languages

    Key           |                 Description
------------------|------------------------------------------------------------
<kbd>m t a</kbd>  | execute all the tests of the current project
<kbd>m t b</kbd>  | execute all the tests of the current buffer
<kbd>m t t</kbd>  | execute the current test (thing at point, function)

Note: we don't distinguish between the file and the buffer. We can implement
an auto-save of the buffer before executing the tests of buffer.

#### Language specific

    Key           |                 Description
------------------|------------------------------------------------------------
<kbd>m t m</kbd>  | execute the tests of the current module
<kbd>m t s</kbd>  | execute the tests of the current suite

Note that there are overlaps, depending on the language we will choose one
or more bindings for the same thing

### Refactoring

Refactoring prefix is <kbd>SPC m r</kbd>.

### Help or Documentation

The base prefix for help commands is <kbd>SPC h</kbd>. Documentation is
considered as an help command.

    Key           |                 Description
------------------|------------------------------------------------------------
<kbd>m h h</kbd>  | documentation of thing under point
<kbd>m h r</kbd>  | documentation of selected region
