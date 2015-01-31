# C# contribution layer for Spacemacs

![logo](img/csharp.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [C# contribution layer for Spacemacs](#c-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Packages Included](#packages-included)
    - [Install](#install)
        - [Prerequisites](#prerequisites)
            - [APS.NET 5](#apsnet-5)
            - [curl](#curl)
            - [Omnisharp server](#omnisharp-server)
    - [Caveats](#caveats)
    - [Key Bindings](#key-bindings)
        - [Tests](#tests)
        - [Navigation](#navigation)
        - [Refactoring](#refactoring)
        - [Solution/Project manipulation](#solutionproject-manipulation)
        - [Compilation](#compilation)
        - [Code manipulation](#code-manipulation)
        - [OmniSharp server interaction](#omnisharp-server-interaction)

<!-- markdown-toc end -->

## Description

This layer adds experimental support for C# language using [OmniSharp][].

## Packages Included

- [OmniSharp-emacs][]
- [OmniSharp-server][]

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(csharp)
  "List of contribution to load."
)
```

### Prerequisites

#### APS.NET 5

Follow the instructions for your platform [here][aspnet].

#### curl

You'll need [curl][] to be able to use the Omnisharp package. Use your favorite
package manager to install it (on Windows you can use [chocolatey][]).

Be sure to make the `curl` binary available to your PATH environment variable,
or set the variable `omnisharp--curl-executable-path` in your dotfile function
`dotspacemacs-config`:

```elisp
(setq-default omnisharp--curl-executable-path "/PATH/TO/CURL/curl")
```

#### Omnisharp server

You have to compile the OmniSharp server following the instructions which can
be found [here][server_install].

Don't forget to add the server binary directory to your system PATH environment
variable. The full path the binary can also be directly referenced in the
variable `omnisharp-server-executable-path` (put this in your
`dotspacemacs-config` function):

```elisp
(setq-default omnisharp-server-executable-path "/PATH/TO/OMNISHARP/OmniSharpServer")
```

OmniSharp should now automatically load and start a server when you open a
`.cs` file.

## Caveats

- It's currently not possible to create a C# solution outside of an IDE such as
[MonoDevelop][], it's therefore recommended that you install it to create your
solutions.

- Debugging is possible using [SDB][].

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

[server_install]: https://github.com/OmniSharp/omnisharp-server
[aspnet]: https://github.com/aspnet/home#getting-started
[OmniSharp]: https://github.com/OmniSharp/omnisharp-emacs
[OmniSharp-emacs]: https://github.com/OmniSharp/omnisharp-emacs
[OmniSharp-server]: https://github.com/OmniSharp/omnisharp-server
[MonoDevelop]: http://www.monodevelop.com/
[SDB]: https://github.com/mono/sdb
[curl]: http://curl.haxx.se/
[chocolatey]: https://chocolatey.org/
