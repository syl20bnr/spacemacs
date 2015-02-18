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

[EDTS]: https://github.com/tjarvstrand/edts
