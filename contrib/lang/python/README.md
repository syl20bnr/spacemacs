# Python contribution layer for Spacemacs

## Features

- **Auto-completion**
- **Code Navigation**
- **Virtual Environment**
- **Documentation Lookup**
- **Running Tests**

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(defvar dotspacemacs-configuration-layers '(python)
  "List of contribution to load."
)
```

## Key Bindings

### Inferior REPL process

Start an (i)Python inferior REPL process with `<SPC> m i`.  If
`ipython` is available in system executable search paths, `ipython`
will be used to launch python shell; otherwise, default `python`
interpreter will be used.  You may change your system executable
search path by activating a virtual environment.

Send code to inferior process commands:

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> m s b`     | send buffer and keep code buffer focused
`<SPC> m s B`     | send buffer and switch to REPL in insert mode
`<SPC> m s f`     | send function and keep code buffer focused
`<SPC> m s F`     | send function and switch to REPL in insert mode
`<SPC> m s r`     | send region and keep code buffer focused
`<SPC> m s R`     | send region and switch to REPL in insert mode
`CTRL+j`          | next item in REPL history
`CTRL+k`          | previous item in REPL history

### Testing in Python

`Spacemacs` uses [nose][nose] as a test runner. An improved version of
[nose.el][nose.el] is shipped with `Spacemacs`, this version adds:
- windows support
- test suite support

The root of the project is detected with a `.git` directory or a `setup.cfg` file.

Test commands (start with `m t` or `m T`):

    No Debug      |                 Description
------------------|------------------------------------------------------------
<SPC> m t a       | launch all tests of the project
<SPC> m t b       | launch all tests of the current buffer (same as module)
<SPC> m t f       | launch the current test (function)
<SPC> m t m       | launch all tests of the current module
<SPC> m t s       | launch all tests of the current suite

     Debug        |                 Description
------------------|------------------------------------------------------------
<SPC> m T a       | launch all tests of the project in debug mode
<SPC> m T b       | launch all tests of the current buffer (module) in debug mode
<SPC> m T f       | launch the current test (function) in debug mode
<SPC> m T m       | launch all tests of the current module in debug mode
<SPC> m T s       | launch all tests of the current suite in debug mode

### Other Python commands

    Key Binding   |                 Description
------------------|------------------------------------------------------------
`<SPC> m d`       | quick documentation using anaconda
`<SPC> m D`       | open documentation in `firefox` using [pylookup][pylookup]
`<SPC> m g`       | go to definition using `anaconda-mode-goto` (`C-o` to jump back)
`<SPC> m b`       | toggle a breakpoint
`<SPC> m v`       | activate a virtual environment with [pyvenv][pyvenv]

## Screenshots

[anaconda-mode]: https://github.com/proofit404/anaconda-mode
[pyvenv]: https://github.com/jorgenschaefer/pyvenv
[pylookup]: https://github.com/tsgates/pylookup
[nose]: https://github.com/nose-devs/nose/
[nose.el]: https://github.com/syl20bnr/nose.el
