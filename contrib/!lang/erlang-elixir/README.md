# Erlang/Elixir contribution layer for Spacemacs

![logo_erlang](img/erlang.png)

![logo_elixir](img/elixir.png) with ![logo_alchemist](img/alchemist.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Erlang/Elixir contribution layer for Spacemacs](#erlangelixir-contribution-layer-for-spacemacs)
    - [Install](#install)
    - [Erlang](#erlang)
        - [Enable EDTS](#enable-edts)
            - [EDTS Key bindings](#edts-key-bindings)
    - [Elixir](#elixir)
        - [Elixir Key bindings](#elixir-key-bindings)
            - [Compilation](#compilation)
            - [Elixir Help](#elixir-help)
            - [Elixir Mix](#elixir-mix)
            - [Elixir Evaluation in place](#elixir-evaluation-in-place)
            - [Elixir REPL interactions](#elixir-repl-interactions)
            - [Elixir Tests](#elixir-tests)
            - [Elixir Execute](#elixir-execute)

<!-- markdown-toc end -->

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(erlang-elixir))
```

## Erlang

### Enable EDTS

[EDTS][] is disabled by default, to enable it add the following snippet to
the `dotspacemacs/init` function of your `~/.spacemacs` file.

```elisp
(setq spacemacs-erlang-elixir-use-edts t)
```

Note that `EDTS` is disable on Windows.

#### EDTS Key bindings

    Key Binding     |                 Description
--------------------|------------------------------------------------------------
<kbd>SPC m d</kbd>  | show man page documentation
<kbd>SPC m e</kbd>  | go to next issue
<kbd>SPC m g</kbd>  | go to definition
<kbd>SPC m G</kbd>  | find a module in the current project
<kbd>SPC m h</kbd>  | open the header file under point
<kbd>SPC m l</kbd>  | find a function in the current module
<kbd>SPC m m</kbd>  | go to the macro definition under point
<kbd>SPC m r</kbd>  | go to the record definition under point

## Elixir

### Elixir Key bindings

#### Compilation

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m c b</kbd>  | Compile current buffer

#### Elixir Help

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m h :</kbd>  | Run custom search for help
<kbd>SPC m h h</kbd>  | Show help of the current expression
<kbd>SPC m h H</kbd>  | Toggle through search history
<kbd>SPC m h r</kbd>  | Show help for current region

#### Elixir Mix

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m m :</kbd>  | Prompt for a `mix` command
<kbd>SPC m m c</kbd>  | Compile the whole application
<kbd>SPC m m h</kbd>  | Show help for a specific `mix` command
<kbd>SPC m m i</kbd>  | Start `iex` in the context of an Elixir project
<kbd>SPC m m x</kbd>  | Run the given expression in the Elixir application context

#### Elixir Evaluation in place

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m e b</kbd>  | Evaluate buffer
<kbd>SPC m e B</kbd>  | Evaluate buffer and insert result
<kbd>SPC m e l</kbd>  | Evaluate current line
<kbd>SPC m e L</kbd>  | Evaluate current line and insert result
<kbd>SPC m e r</kbd>  | Evaluate region
<kbd>SPC m e R</kbd>  | Evaluate region and insert result

#### Elixir REPL interactions

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m s i</kbd>  | Start an `iex` inferior process
<kbd>SPC m s l</kbd>  | Send current line to REPL buffer
<kbd>SPC m s L</kbd>  | Send current line to REPL buffer and focus it in `insert state`
<kbd>SPC m s r</kbd>  | Send region to REPL buffer
<kbd>SPC m s R</kbd>  | Send region to REPL buffer and focus it in `insert state`


#### Elixir Tests

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m g t</kbd>  | Open the test file for current buffer
<kbd>SPC m t a</kbd>  | Run all the tests
<kbd>SPC m t b</kbd>  | Run all the tests from current buffer
<kbd>SPC m t t</kbd>  | Run test under point

#### Elixir Execute

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m x :</kbd>  | Run a custom execute command with `elixir`
<kbd>SPC m x b</kbd>  | Run the current buffer through `elixir`
<kbd>SPC m x f</kbd>  | Run `elixir` with the given filename


[EDTS]: https://github.com/tjarvstrand/edts
