# Scala contribution layer for Spacemacs

![logo](img/scala.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Scala contribution layer for Spacemacs](#scala-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Ensime](#ensime)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer adds support for the Scala language using the excellent [ENSIME][]
client/server.

**This layer is not adapted for Spacemacs, it needs you, Scala experts, to
improve it and make it consistent with the Spacemacs experience.**

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(scala)
  "List of contribution to load."
)
```

### Ensime

There is nothing to install for ENSIME to work but you have to create a
`.ensime` file at the root of your project.

This project can be created manually using the [template here][dotensime],
but it is recommended to use the [ENSIME sbt-plugin][sbt-plugin] if your
project uses [SBT][].

## Key bindings

**TODO**

[ENSIME]: https://github.com/ensime
[dotensime]: https://github.com/ensime/ensime-server/wiki/Example-Configuration-File
[sbt]: http://www.scala-sbt.org/
[sbt-plugin]: https://github.com/ensime/ensime-server/wiki/Quick-Start-Guide#installing-the-ensime-sbt-plugin
