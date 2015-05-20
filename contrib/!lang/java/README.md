# Java contribution layer for Spacemacs

## Description

This layer adds support for the Java language using the [Eclim](http://eclim.org)
client/server.

## Layer Installation
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Java contribution layer for Spacemacs](#java-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Layer Installation](#layer-installation)
    - [Eclim](#eclim)
        - [Installation](#installation)
        - [Usage](#usage)
    - [Key bindings](#key-bindings)
        - [Eclim key bindings in java-mode](#eclim-key-bindings-in-java-mode)
            - [Project management](#project-management)
            - [Maven](#maven)
            - [Goto](#goto)
            - [Refactoring](#refactoring)
            - [Documentation, Find](#documentation-find)
            - [Problems](#problems)
            - [Tests](#tests)
        - [Eclim keybindings in problems buffer](#eclim-keybindings-in-problems-buffer)
        - [Eclim keybindings in projects buffer](#eclim-keybindings-in-projects-buffer)

<!-- markdown-toc end -->

Add this layer to your `~/.spacemacs`.

```elisp
(setq-default dotspacemacs-configuration-layers '(java))
```

and also in `~/.spacemacs` set eclipse and eclim paths:

```elisp
(custom-set-variables
  '(eclim-eclipse-dirs '("~/opt/eclipse"))
  '(eclim-executable "~/opt/eclipse/eclim"))
```

## Eclim

[Eclim]() provides the ability to access Eclipse code editing features (code completion, searching, code validation, and many more) via the command line or a local network connection, allowing those features to be integrated with your favorite editor.

### Installation
For installation check [official page](http://eclim.org/install.html#download)

### Usage
Currently you have to have eclimd already started. This layer doesn't try to control eclimd in no way although there's that option in `emacs-eclim` itself.

This layer uses `company` as a completion framework. If you want to use `auto-complete` I encourage you to make it configurable.

## Key bindings

### Eclim key bindings in java-mode

#### Project management

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m p j</kbd>  | Information about project
<kbd>SPC m p o</kbd>  | Open project
<kbd>SPC m p b</kbd>  | Build project
<kbd>SPC m p d</kbd>  | Delete project
<kbd>SPC m p g</kbd>  | Open file in current project
<kbd>SPC m p i</kbd>  | Import project
<kbd>SPC m p c</kbd>  | Create project
<kbd>SPC m p k</kbd>  | Close project
<kbd>SPC m p s</kbd>  | Open project management buffer
<kbd>SPC m p u</kbd>  | Update project

#### Maven

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m m p</kbd>  | Run one already goal from list
<kbd>SPC m m R</kbd>  | Run one maven goal
<kbd>SPC m m r</kbd>  | Run maven goals
<kbd>SPC m m t</kbd>  | Run maven test
<kbd>SPC m m i</kbd>  | Run maven clean install
<kbd>SPC m m I</kbd>  | Run maven install

#### Goto

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m g g</kbd> or <kbd> M-.</kbd> | go to declaration
<kbd>M-,</kbd>        | jump back from go to declaration/definition
<kbd>SPC m g t</kbd>  | go to type definition

#### Refactoring

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m r f</kbd>  | Format file
<kbd>SPC m r r</kbd>  | Rename symbol
<kbd>SPC m r i</kbd>  | optimize imports

#### Documentation, Find

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m h h</kbd>  | show documentation for symbol at point
<kbd>SPC m h u</kbd>  | show usages for symbol at point
<kbd>SPC m f f</kbd>  | general find in project

#### Problems

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m e o</kbd>  | open buffer with problems
<kbd>SPC m e b</kbd>  | open buffer with problems
<kbd>SPC m e a</kbd>  | set all problems for next/prev action
<kbd>SPC m e e</kbd>  | set only errors for next/prev action
<kbd>SPC m e w</kbd>  | set warnings for next/prev action
<kbd>SPC m e f</kbd>  | set only current file for next/prev action
<kbd>SPC m e n</kbd>  | set all problems for next/prev action
<kbd>SPC m e n</kbd>  | go to next problem
<kbd>SPC m e p</kbd>  | go to previous problem
<kbd>SPC m e c</kbd>  | show options with problem corrections

#### Tests

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m t t</kbd>  | run JUnit tests for current method or current file or project
<kbd>SPC m t T</kbd>  | run maven test phase


### Eclim keybindings in problems buffer

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>a</kbd>          | show all problems
<kbd>e</kbd>          | show only errors
<kbd>g</kbd>          | refresh problems
<kbd>q</kbd>          | quit
<kbd>w</kbd>          | show only warnings
<kbd>f</kbd>          | show problems only for current file
<kbd>RET</kbd>        | go to problem place

### Eclim keybindings in projects buffer

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>N</kbd>          | create new project
<kbd>m</kbd>          | mark current project
<kbd>M</kbd>          | mark all projects
<kbd>u</kbd>          | unmark current project
<kbd>U</kbd>          | unmark all projects
<kbd>o</kbd>          | open project
<kbd>c</kbd>          | go to problem place
<kbd>i</kbd>          | info about current project
<kbd>I</kbd>          | import existing project into the workspace
<kbd>RET</kbd>        | go to current project
<kbd>D</kbd>          | delete project
<kbd>p</kbd>          | update project
<kbd>g</kbd>          | refresh buffer
<kbd>R</kbd>          | rename current project
<kbd>q</kbd>          | quit
