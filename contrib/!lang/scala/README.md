# Scala contribution layer for Spacemacs

![logo_scala](img/scala.png) ![logo_ensime](img/ensime.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Scala contribution layer for Spacemacs](#scala-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Layer Installation](#layer-installation)
    - [Ensime](#ensime)
        - [Installation](#installation)
        - [Usage](#usage)
    - [Scalastyle](#scalastyle)
    - [Key bindings](#key-bindings)
        - [Ensime key bindings](#ensime-key-bindings)
            - [Search](#search)
            - [sbt](#sbt)
            - [Typecheck](#typecheck)
            - [Debug](#debug)
            - [Errors](#errors)
            - [Goto](#goto)
            - [Documentation, Inspect](#documentation-inspect)
            - [Server](#server)
            - [Refactoring](#refactoring)
            - [Tests](#tests)
            - [REPL](#repl)

<!-- markdown-toc end -->

## Description

This layer adds support for the Scala language using the excellent [ENSIME][]
client/server.

## Layer Installation

Add this contribution to your `~/.spacemacs`.

```elisp
(setq-default dotspacemacs-configuration-layers '(scala))
```

## Ensime

[ENSIME][] provides IDE-like features, such as refactoring, incremental
compilation and project-wide type-checking.

ENSIME requires a configuration file at the root of each Scala project. It
provides an SBT plugin to generate these files.

### Installation

1. Configure the ENSIME sbt plugin by adding the following to
   `~/.sbt/0.13/plugins/plugins.sbt`:

   ```scala
   resolvers += Resolver.sonatypeRepo("snapshots")

   addSbtPlugin("org.ensime" % "ensime-sbt" % "0.1.5-SNAPSHOT")
   ```

2. Run `sbt` at the shell to download and install the plugin.

See the [ENSIME quickstart][] guide for further information.

### Usage

1. Create a `.ensime` file at the root of your Scala project using `sbt
   gen-ensime` at the shell.

2. Run `M-x ensime` within Emacs to start an ENSIME session.

Each Scala project uses a dedicated ENSIME session, so you only need to run `M-x
ensime` once per project. Any Scala files you create or visit within the project
will automatically use ENSIME for the remainder of your editing session.

## Scalastyle

[Scalastyle][] provides style-checking and linting. The Emacs functionality is
provided by Flycheck.

To use scalastyle,

1. Download the [scalastyle jar][] and put it somewhere sensible
2. Customise the `flycheck-scalastyle-jar` variable and set it to the path of
   the jar.

See the [flycheck documentation][] for up-to-date configuration instructions.

## Key bindings

### Ensime key bindings

#### Search

    Key Binding     |                 Description
--------------------|------------------------------------------------------------
<kbd>SPC m /</kbd>  | incremental search using `ensime-scalex` major mode
<kbd>SPC m ?</kbd>  | incremental search in all live buffers

#### sbt

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m b c</kbd>  | compile command
<kbd>SPC m b C</kbd>  | clean command
<kbd>SPC m b i</kbd>  | switch to sbt shell
<kbd>SPC m b p</kbd>  | package command
<kbd>SPC m b r</kbd>  | run command

#### Typecheck

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m c t</kbd>  | type check the current file
<kbd>SPC m c T</kbd>  | type check all the open buffers

#### Debug

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m d A</kbd>  | Attach to a remote debugger
<kbd>SPC m d b</kbd>  | set breakpoint
<kbd>SPC m d B</kbd>  | clear breakpoint
<kbd>SPC m d C</kbd>  | clear all breakpoints
<kbd>SPC m d c</kbd>  | continue
<kbd>SPC m d d</kbd>  | start a debug session
<kbd>SPC m d i</kbd>  | inspect value at point
<kbd>SPC m d l</kbd>  | list local variables
<kbd>SPC m d n</kbd>  | next
<kbd>SPC m d o</kbd>  | step out
<kbd>SPC m d q</kbd>  | quit
<kbd>SPC m d r</kbd>  | run
<kbd>SPC m d s</kbd>  | step
<kbd>SPC m d t</kbd>  | backtrace

**Note** These key bindings need a micro-state, PR welcome :-)

#### Errors

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m e e</kbd>  | print error at point
<kbd>SPC m e l</kbd>  | show all errors and warnings
<kbd>SPC m e s</kbd>  | switch to buffer containing the stack trace parser

#### Goto

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m g g</kbd>  | go to definition
<kbd>SPC m g i</kbd>  | go to implementation
<kbd>SPC m g t</kbd>  | go to test

#### Documentation, Inspect

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m h h</kbd>  | show documentation for symbol at point
<kbd>SPC m h u</kbd>  | show uses for symbol at point
<kbd>SPC m h t</kbd>  | print type at point
<kbd>SPC m i i</kbd>  | inspect type at point
<kbd>SPC m i I</kbd>  | inspect type in other frame
<kbd>SPC m i p</kbd>  | inspect project package

#### Server

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m n F</kbd>  | reload open files
<kbd>SPC m n s</kbd>  | start ensime server
<kbd>SPC m n S</kbd>  | regenerate the `.ensime` and restart the ensime server

#### Refactoring

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m r f</kbd>  | format source
<kbd>SPC m r d</kbd>  | get rid of an intermediate variable (`ensime-refactor-inline-local`)
<kbd>SPC m r D</kbd>  | get rid of an intermediate variable (`ensime-undo-peek`)
<kbd>SPC m r i</kbd>  | organize imports
<kbd>SPC m r m</kbd>  | extract a range of code into a method
<kbd>SPC m r r</kbd>  | rename a symbol project wide
<kbd>SPC m r t</kbd>  | import type at point
<kbd>SPC m r v</kbd>  | extract a range of code into a variable
<kbd>SPC m z</kbd>    | expand/contract region

#### Tests

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m t a</kbd>  | test command (sbt)
<kbd>SPC m t r</kbd>  | test quick command (sbt)
<kbd>SPC m t t</kbd>  | test only (sbt)

#### REPL

    Key Binding       |                 Description
----------------------|------------------------------------------------------------
<kbd>SPC m s a</kbd>  | ask for a file to be loaded in the REPL
<kbd>SPC m s b</kbd>  | send buffer to the REPL
<kbd>SPC m s B</kbd>  | send buffer to the REPL and focus the REPL buffer in `insert state`
<kbd>SPC m s i</kbd>  | start or switch to the REPL inferior process
<kbd>SPC m s r</kbd>  | send region to the REPL
<kbd>SPC m s R</kbd>  | send region to the REPL and focus the REPL buffer in `insert state`

[ENSIME quickstart]: https://github.com/ensime/ensime-server/wiki/Quick-Start-Guide#installing-the-ensime-sbt-plugin
[ENSIME]: https://github.com/ensime
[Scalastyle]: http://flycheck.readthedocs.org/en/latest/guide/languages.html#el.flycheck-checker.scala-scalastyle
[dotensime]: https://github.com/ensime/ensime-server/wiki/Example-Configuration-File
[flycheck documentation]: http://flycheck.readthedocs.org/en/latest/guide/languages.html#el.flycheck-checker.scala-scalastyle
[sbt-plugin]: https://github.com/ensime/ensime-server/wiki/Quick-Start-Guide#installing-the-ensime-sbt-plugin
[sbt]: http://www.scala-sbt.org/
[scalastyle jar]: https://oss.sonatype.org/content/repositories/releases/org/scalastyle/scalastyle_2.11/0.6.0/
