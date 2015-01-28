# C# contribution layer for Spacemacs

![logo](img/csharp.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [C# contribution layer for Spacemacs](#c-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Packages Included](#packages-included)
    - [Install](#install)
    - [Key Bindings](#key-bindings)
        - [Tests](#tests)
        - [Navigation](#navigation)
        - [Type information](#type-information)
        - [Refactoring](#refactoring)
        - [Solution/Project manipulation](#solutionproject-manipulation)
        - [Compilation](#compilation)
        - [Code manipulation](#code-manipulation)
        - [OmniSharp server interaction](#omnisharp-server-interaction)

<!-- markdown-toc end -->

## Description

This layer adds experimental support for C# language using [OmniSharp](https://github.com/OmniSharp/omnisharp-emacs).
The layer requires the OmniSharp server, refer to the install section.

## Packages Included

- [OmniSharp-emacs](https://github.com/OmniSharp/omnisharp-emacs)
- [OmniSharp-server](https://github.com/OmniSharp/omnisharp-server)

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(csharp)
  "List of contribution to load."
)
```

- Compile the OmniSharp server, following the instructions found here: https://github.com/OmniSharp/omnisharp-server

- Then do one of:

    a) In `.spacemacs` add the path to OmniSharp to an Emacs variable `(setq omnisharp-server-executable-path "/PATH/TO/OMNISHARP/OmniSharpServer")`

    b) Ensure that the binary `OmniSharp` is available in Emacs' `exec-path`.

OmniSharp should now automatically load and start a server when you open a `.cs` file.

## Limitations
It's currently not possible to create C# solutions outside of IDEs such as MonoDevelop,
it's therefore recommended that you install a C# IDE and use that for creating a solution.

Debugging is possible using [SDB](https://github.com/mono/sdb), or by using an IDE.

## Key Bindings

### Tests

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
                "mta" | Run all tests in project
                "mtb" | Run all tests in current file/fixture
                "mtt" | Run test under cursor

### Navigation

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
                "mgg" |   Go to definition
                "mgG" |   Go to definition in other window
                "mgu" |   Find usages of symbol under cursor using Helm
                "mgs" |   Find symbols using Helm
                "mgi" |   Find implementations
                "mgr" |   Go to region
                "mgm" |   Go to solution member
                "mgM" |   Go to solution member in other window
                "mgf" |   Go to solution file
                "mgF" |   Go to solution file then member

### Type information

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
                 "mt" | Get type information for symbol under cursor
                 "mT" | Get type information for symbol under cursor and put it into kill-ring

### Refactoring

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
                "mrm" | Rename symbol under cursor
                "mrr" | Refactor symbol under cursor

### Solution/Project manipulation

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
                "mpa" | Add the current file to solution
                "mpA" | Add files selected in dired to solution
                "mpr" | Remove the current file from solution
                "mpR" | Removed files selected in dired from solution
                "mpl" | Add reference to solution

### Compilation

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
                 "mb" | Build the solution

### Code manipulation

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
                 "mo" | Auto complete overrides
                 "mi" | Fix usings/imports
                 "m=" | Format the current buffer

### OmniSharp server interaction

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
                "mss" | Start the OmniSharp server
                "msS" | Stop the OmniSharp server
                "msr" | Reload the solution
