# Scala contribution layer for Spacemacs

![logo](img/scala.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Description](#description)
- [Install](#install)
- [Ensime](#ensime)
    - [Installation](#installation)
    - [Usage](#usage)
- [Scalastyle](#scalastyle)

<!-- markdown-toc end -->

## Description

This layer adds support for the Scala language using the excellent [ENSIME][]
client/server.

## Install

Add this contribution to your `~/.spacemacs`.

```elisp
(setq-default dotspacemacs-configuration-layers '(scala)
  "List of contribution to load."
)
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

[ENSIME quickstart]: https://github.com/ensime/ensime-server/wiki/Quick-Start-Guide#installing-the-ensime-sbt-plugin
[ENSIME]: https://github.com/ensime
[Scalastyle]: http://flycheck.readthedocs.org/en/latest/guide/languages.html#el.flycheck-checker.scala-scalastyle
[dotensime]: https://github.com/ensime/ensime-server/wiki/Example-Configuration-File
[flycheck documentation]: http://flycheck.readthedocs.org/en/latest/guide/languages.html#el.flycheck-checker.scala-scalastyle
[sbt-plugin]: https://github.com/ensime/ensime-server/wiki/Quick-Start-Guide#installing-the-ensime-sbt-plugin
[sbt]: http://www.scala-sbt.org/
[scalastyle jar]: https://oss.sonatype.org/content/repositories/releases/org/scalastyle/scalastyle_2.11/0.6.0/
