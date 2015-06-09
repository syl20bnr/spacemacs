# Elixir contribution layer for Spacemacs

![logo_elixir](img/elixir.png) with ![logo_alchemist](img/alchemist.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Elixir contribution layer for Spacemacs](#elixir-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)
        - [Refcard](#refcard)
        - [Help](#help)
        - [Mix](#mix)
        - [Project](#project)
        - [Evaluation in place](#evaluation-in-place)
        - [REPL interactions](#repl-interactions)
        - [Tests](#tests)
        - [Compile](#compile)
        - [Execute](#execute)
        - [Code Definition Jump](#code-definition-jump)

<!-- markdown-toc end -->

## Description

This layer adds support for [Elixir][].

[Alchemist][] brings the Elixir tooling to Emacs and comes with a bunch of
features like:
- Powerful IEx integration
- Mix integration
- Compile & Execution of Elixir code
- Inline code evaluation
- Documentation lookup
- Definition lookup
- Smart code completion
- Elixir project management
- Integration with [company-mode](http://company-mode.github.io/)

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(elixir))
```

## Key bindings

### Refcard

You find and overview of all the key-bindings on the [Alchemist-Refcard][].

### Help

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m h :</kbd>  | Run custom search for help
<kbd>SPC m h h</kbd>  | Show help of the current expression
<kbd>SPC m h H</kbd>  | Toggle through search history
<kbd>SPC m h r</kbd>  | Show help for current region

### Mix

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m m :</kbd>  | Prompt for a `mix` command
<kbd>SPC m m c</kbd>  | Compile the whole application
<kbd>SPC m m h</kbd>  | Show help for a specific `mix` command
<kbd>SPC m m x</kbd>  | Run the given expression in the Elixir application context

### Project

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m p t</kbd>  | Open project test directory and list all test files.
<kbd>SPC m g t</kbd>  | Toggle between a file and its tests in the current window.
<kbd>SPC m g T</kbd>  | Toggle between a file and its tests in other window.

### Evaluation in place

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m e b</kbd>  | Evaluate buffer
<kbd>SPC m e B</kbd>  | Evaluate buffer and insert result
<kbd>SPC m e l</kbd>  | Evaluate current line
<kbd>SPC m e L</kbd>  | Evaluate current line and insert result
<kbd>SPC m e r</kbd>  | Evaluate region
<kbd>SPC m e R</kbd>  | Evaluate region and insert result

### REPL interactions

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m s i</kbd>  | Start an `iex` inferior process
<kbd>SPC m s I</kbd>  | Start an IEx process with mix (`iex -S mix`)
<kbd>SPC m s l</kbd>  | Send current line to REPL buffer
<kbd>SPC m s L</kbd>  | Send current line to REPL buffer and focus it in `insert state`
<kbd>SPC m s r</kbd>  | Send region to REPL buffer
<kbd>SPC m s R</kbd>  | Send region to REPL buffer and focus it in `insert state`
<kbd>SPC m s c</kbd>  | Compiles the current buffer in the IEx process.

### Tests

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m g t</kbd>  | Open the test file for current buffer
<kbd>SPC m t a</kbd>  | Run all the tests
<kbd>SPC m t b</kbd>  | Run all the tests from current buffer
<kbd>SPC m t t</kbd>  | Run test under point

### Compile

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m c :</kbd>  | Run a custom compile command with `elixirc`
<kbd>SPC m c b</kbd>  | Compile the current buffer with elixirc. `elixirc`
<kbd>SPC m c f</kbd>  | Compile the given filename with `elixirc`


### Execute

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m x :</kbd>  | Run a custom execute command with `elixir`
<kbd>SPC m x b</kbd>  | Run the current buffer through `elixir`
<kbd>SPC m x f</kbd>  | Run `elixir` with the given filename

### Code Definition Jump

    Key Binding     |                 Description
--------------------|------------------------------------------------------------
<kbd>SPC m g g</kbd>  | Jump to the elixir expression definition at point.
<kbd>SPC m ,</kbd>  | Pop back to where <kbd>SPC m g g</kbd> was last invoked.

[Elixir]: http://elixir-lang.org/
[Alchemist]: https://github.com/tonini/alchemist.el
[Alchemist-Refcard]: alchemist-refcard.pdf?raw=true
