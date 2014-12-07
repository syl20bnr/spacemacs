# JavaScript contribution layer for Spacemacs

## Features

- **Smart Code Folding**
- **Refactoring**
- **Auto-completion**
- **Documentation Lookup**

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(defvar dotspacemacs-configuration-layers '(js2-mode)
  "List of contribution to load."
)
```

You will also need to install `tern` to use the auto-completion and
documentation features:

```shell
$ npm install -g tern
```

## Key Bindings

### js2-mode

[Improved JavaScript editing mode for GNU Emacs](https://github.com/mooz/js2-mode).

    Key Binding   |                 Description
------------------|------------------------------------------------------------
<SPC> m w         | toggle js2-mode warnings and errors
<SPC> m z c       | hide element
<SPC> m z o       | show element
<SPC> m z r       | show all element
<SPC> m z e       | toggle hide/show element
<SPC> m z F       | toggle hide functions
<SPC> m z C       | toggle hide comments

### js2-refactor

Refactoring is done using [js2-refactor](https://github.com/magnars/js2-refactor.el).  
Bindings should match the plain emacs assignments.

    Key Binding   |                 Description
------------------|------------------------------------------------------------
<SPC> x m j       | move line down, while keeping commas correctly placed
<SPC> x m k       | move line up, while keeping commas correctly placed
<SPC> m k         | deletes to the end of the line, but does not cross semantic boundaries
<SPC> m r 3 i     | converts ternary operator to if-statement
<SPC> m r a g     | creates a `/* global */` annotation if it is missing, and adds var to point to it
<SPC> m r a o     | replaces arguments to a function call with an object literal of named arguments
<SPC> m r b a     | moves the last child out of current function, if-statement, for-loop or while-loop
<SPC> m r c a     | converts a multiline array to one line
<SPC> m r c o     | converts a multiline object literal to one line
<SPC> m r c u     | converts a multiline function to one line (expecting semicolons as statement delimiters)
<SPC> m r e a     | converts a one line array to multiline
<SPC> m r e f     | extracts the marked expressions into a new named function
<SPC> m r e m     | extracts the marked expressions out into a new method in an object literal
<SPC> m r e o     | converts a one line object literal to multiline
<SPC> m r e u     | converts a one line function to multiline (expecting semicolons as statement delimiters)
<SPC> m r e v     | takes a marked expression and replaces it with a var
<SPC> m r i g     | creates a shortcut for a marked global by injecting it in the wrapping immediately invoked function expression
<SPC> m r i p     | changes the marked expression to a parameter in a local function
<SPC> m r i v     | replaces all instances of a variable with its initial value
<SPC> m r l p     | changes a parameter to a local var in a local function
<SPC> m r l t     | adds a console.log statement for what is at point (or region)
<SPC> m r r v     | renames the variable on point and all occurrences in its lexical scope
<SPC> m r s l     | moves the next statement into current function, if-statement, for-loop, while-loop
<SPC> m r s s     | splits a `String`
<SPC> m r s v     | splits a `var` with multiple vars declared into several `var` statements
<SPC> m r t f     | toggle between function declaration and function expression
<SPC> m r u w     | replaces the parent statement with the selected region
<SPC> m r v t     | changes local `var a` to be `this.a` instead
<SPC> m r w i     | wraps the entire buffer in an immediately invoked function expression
<SPC> m r w l     | wraps the region in a for-loop

### tern

[Tern](http://ternjs.net/) is used as an auto-completion backend and for documentation features.

    Key Binding   |                 Description
------------------|------------------------------------------------------------
<SPC> m .       | jump to the definition of the thing under the cursor
<SPC> m ,       | brings you back to last place you were when you pressed M-..
<SPC> m f       | jump to definition for the given name
<SPC> m c       | rename variable under the cursor using tern
<SPC> m t       | find the type of the thing under the cursor
<SPC> m g       | find docs of the thing under the cursor. Press again to open the associated URL (if any)
