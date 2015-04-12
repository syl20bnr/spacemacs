# Python contribution layer for Spacemacs

![logo_python](img/python.png) ![logo_django](img/django.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Python contribution layer for Spacemacs](#python-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [Anaconda dependencies](#anaconda-dependencies)
    - [Key Bindings](#key-bindings)
        - [Inferior REPL process](#inferior-repl-process)
        - [Running Python Script in shell](#running-python-script-in-shell)
        - [Testing in Python](#testing-in-python)
        - [Other Python commands](#other-python-commands)
        - [Django](#django)
            - [Fabric](#fabric)
            - [Files](#files)
            - [Interactive](#interactive)
            - [Server](#server)
            - [South/Syncdb](#southsyncdb)
            - [Test](#test)

<!-- markdown-toc end -->

## Description

This layer adds support for the Python language.

Features:
- Auto-completion using [anaconda-mode][]
- Code Navigation using  [anaconda-mode][]
- Documentation Lookup using  [anaconda-mode][] and [pylookup][]
- Test Runner using [nose.el][]
- Virtual Environment using [pyvenv][] and [pyenv][]
- semantic mode is enabled
- Django support via [pony-mode][]

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(python))
```

### Anaconda dependencies

`anaconda-mode` tries to install the dependencies itself but sometimes
it does not work and you may encounter the following message when
opening a python buffer:

    Blocking call to accept-process-output with quit inhibited!!

To fix this, install the `anaconda-mode` [dependencies][anaconda-deps] by hand:

    pip install  jedi==0.8.1 json-rpc==1.8.1 service_factory==0.1.2

Source: https://github.com/proofit404/anaconda-mode#issues

## Key Bindings

### Inferior REPL process

Start a Python or iPython inferior REPL process with <kbd>SPC m s i</kbd>.
If `ipython` is available in system executable search paths, `ipython`
will be used to launch python shell; otherwise, default `python`
interpreter will be used.  You may change your system executable
search path by activating a virtual environment.

Send code to inferior process commands:

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m s b</kbd>  | send buffer and keep code buffer focused
<kbd>SPC m s B</kbd>  | send buffer and switch to REPL in insert mode
<kbd>SPC m s f</kbd>  | send function and keep code buffer focused
<kbd>SPC m s F</kbd>  | send function and switch to REPL in insert mode
<kbd>SPC m s i</kbd>  | start inferior REPL process
<kbd>SPC m s r</kbd>  | send region and keep code buffer focused
<kbd>SPC m s R</kbd>  | send region and switch to REPL in insert mode
<kbd>CTRL+j</kbd>     | next item in REPL history
<kbd>CTRL+k</kbd>     | previous item in REPL history

### Running Python Script in shell

To run a Python script like you would in the shell press <kbd>SPC m c c</kbd>
to start the Python script in comint mode. This is useful when working with
multiple Python files since the REPL does not reload changes made in other
modules.

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m c c</kbd>  | Execute current file in a comint shell
<kbd>SPC m c C</kbd>  | Execute current file in a comint shell and switch to it in `insert state`

**Note** With the universal argument <kbd>SPC u</kbd> you can enter a new
compilation command.

### Testing in Python

`Spacemacs` uses [nose][nose] as a test runner. An improved version of
[nose.el][nose.el] is shipped with `Spacemacs`, this version adds:
- windows support
- test suite support

The root of the project is detected with a `.git` directory or a `setup.cfg` file.

Test commands (start with <kbd>m t</kbd> or <kbd>m T</kbd>):

    No Debug         |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC m t a</kbd> | launch all tests of the project
<kbd>SPC m t b</kbd> | launch all tests of the current buffer (same as module)
<kbd>SPC m t m</kbd> | launch all tests of the current module
<kbd>SPC m t s</kbd> | launch all tests of the current suite
<kbd>SPC m t t</kbd> | launch the current test (function)

     Debug           |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC m T a</kbd> | launch all tests of the project in debug mode
<kbd>SPC m T b</kbd> | launch all tests of the current buffer (module) in debug mode
<kbd>SPC m T m</kbd> | launch all tests of the current module in debug mode
<kbd>SPC m T s</kbd> | launch all tests of the current suite in debug mode
<kbd>SPC m T t</kbd> | launch the current test (function) in debug mode

### Other Python commands

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m d b</kbd>  | toggle a breakpoint
<kbd>SPC m g g</kbd>  | go to definition using `anaconda-mode-goto` (<kbd>C-o</kbd> to jump back)
<kbd>SPC m h d</kbd>  | look for documentation using `helm-pydoc`
<kbd>SPC m h h</kbd>  | quick documentation using anaconda
<kbd>SPC m h H</kbd>  | open documentation in `firefox` using [pylookup][pylookup]
<kbd>SPC m v</kbd>    | activate a virtual environment with [pyenv][pyenv]
<kbd>SPC m V</kbd>    | activate a virtual environment with [pyvenv][pyvenv]

### Django

Django related key bindings uses [pony-mode][] and are behind the prefix
<kbd>SPC m j</kbd>.

Configuration options for pony-mode are documented at
[deadpansincerity.com](http://www.deadpansincerity.com/docs/pony/configuration.html)

Manage Django with <kbd>SPC m j m</kbd>.

#### Fabric

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>mjaf</kbd>       | Run a fabric command
<kbd>mjad</kbd>       | Deploy project with `fab deploy`

#### Files

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>mjfs</kbd>       | Open the `settings.py` for this project
<kbd>mjfc</kbd>       | Interactively display a setting value in the minibuffer
<kbd>mjft</kbd>       | Jump to template at point
<kbd>mjfr</kbd>       | Jump to the view file that the URL resolves to (experimental)

#### Interactive

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>mjid</kbd>       | Run interpreter for this project's default database as an inferior process
<kbd>mjis</kbd>       | Open a Python shell with the current pony project's context loaded. If the project has the django_extras package installed, then use the excellent `shell_plus` command. Otherwise, fall back to `manage.py shell`

#### Server

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>mjrd</kbd>       | Stop the dev server
<kbd>mjro</kbd>       | Open a tab at the dev server
<kbd>mjrr</kbd>       | Restart the dev server (works better with django_extras/werkzeug)
<kbd>mjru</kbd>       | Start or open the dev server
<kbd>mjrt</kbd>       | Open a second server with a "throwaway" host/port

#### South/Syncdb

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>mjsc</kbd>       | Convert an existing app to south
<kbd>mjsh</kbd>       | Create migration for modification
<kbd>mjsi</kbd>       | Run the initial south migration for an app
<kbd>mjsm</kbd>       | Migrate an app
<kbd>mjss</kbd>       | Run syncdb on the current project

#### Test

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>mjtd</kbd>       | Move down the traceback one level
<kbd>mjte</kbd>       | Go to the file and line of the last stack trace in a test buffer
<kbd>mjto</kbd>       | Open the file in a traceback at the line specified
<kbd>mjtt</kbd>       | Run the test(s) given by `command`
<kbd>mjtu</kbd>       | Move up the traceback one level


[anaconda-mode]: https://github.com/proofit404/anaconda-mode
[pyvenv]: https://github.com/jorgenschaefer/pyvenv
[pyenv]: https://github.com/yyuu/pyenv
[pylookup]: https://github.com/tsgates/pylookup
[nose]: https://github.com/nose-devs/nose/
[nose.el]: https://github.com/syl20bnr/nose.el
[pony-mode]: https://github.com/davidmiller/pony-mode
[anaconda-deps]: https://github.com/proofit404/anaconda-mode/blob/master/requirements.txt
