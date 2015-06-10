# Perforce contribution layer for Spacemacs

![logo](img/p4.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Perforce contribution layer for Spacemacs](#perforce-contribution-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
    - [Key bindings](#key-bindings)

<!-- markdown-toc end -->

## Description

This layer adds support for [Perforce][] (p4).

## Install

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(perforce))
```

You'll have to install the `p4`` command line, [download page][].

Don't forget to setup the environment variables:
- `P4_PORT`
- `P4_CLIENT`
- `P4_USER`
- `P4_PASSWD`

## Key bindings

    Key Binding      |                 Description
---------------------|------------------------------------------------------------
<kbd>SPC p 4 a</kbd> | add a file in depot
<kbd>SPC p 4 d</kbd> | delete a file in depot
<kbd>SPC p 4 D</kbd> | p4-describe
<kbd>SPC p 4 e</kbd> | checkout a file
<kbd>SPC p 4 r</kbd> | revert a file
<kbd>SPC p 4 R</kbd> | refresh content of an file. 'sync -f'
<kbd>SPC p 4 S</kbd> | submit CL
<kbd>SPC p 4 b</kbd> | create, modify, or delete a branch view specification
<kbd>SPC p 4 B</kbd> | display list of branch specifications
<kbd>SPC p 4 c</kbd> | create or edit a client workspace specification and its view
<kbd>SPC p 4 C</kbd> | display list of pending and submitted CL
<kbd>SPC p 4 E</kbd> | change the filetype of an open file or move it to another CL
<kbd>SPC p 4 @</kbd> | p4-depot-find-file
<kbd>SPC p 4 f</kbd> | list revision history of files
<kbd>SPC p 4 F</kbd> | list files in the depot
<kbd>SPC p 4 G</kbd> | display current perforce client name
<kbd>SPC p 4 g</kbd> | synchronize client with depot
<kbd>SPC p 4 h</kbd> | p4-help
<kbd>SPC p 4 H</kbd> | list revisions most recently synced to the current workspace
<kbd>SPC p 4 i</kbd> | display client/server information
<kbd>SPC p 4 I</kbd> | integrate one set of files into another
<kbd>SPC p 4 j</kbd> | create or edit a job (defect) specification
<kbd>SPC p 4 J</kbd> | display list of all jobs
<kbd>SPC p 4 l</kbd> | create or edit a label specification
<kbd>SPC p 4 L</kbd> | display list of defined labels
<kbd>SPC p 4 :</kbd> | apply label to the contents of the client workspace
<kbd>SPC p 4 m</kbd> | move files from one location to another
<kbd>SPC p 4 o</kbd> | list open files and display file status
<kbd>SPC p 4 p</kbd> | write a depot file to a buffer
<kbd>SPC p 4 P</kbd> | p4-set-p4-port
<kbd>SPC p 4 q</kbd> | quit window
<kbd>SPC p 4 y</kbd> | resolve integrations and updates to workspace files
<kbd>SPC p 4 s</kbd> | identify differences between workspace and depot
<kbd>SPC p 4 t</kbd> | toggle perfore server check when opening files.
<kbd>SPC p 4 u</kbd> | create or edit user specification
<kbd>SPC p 4 U</kbd> | list perforce users
<kbd>SPC p 4 v</kbd> | emacs perforce integration version
<kbd>SPC p 4 V</kbd> | p4 blame
<kbd>SPC p 4 w</kbd> | show how file names are mapped by client view
<kbd>SPC p 4 x</kbd> | delete a file from the depot
<kbd>SPC p 4 X</kbd> | mark jobs done by specific CL
<kbd>SPC p 4 z</kbd> | p4-reconcile
<kbd>SPC p 4 =</kbd> | p4 diff
<kbd>SPC p 4 +</kbd> | p4 diff on all opened files
<kbd>SPC p 4 -</kbd> | p4 ediff

[Perforce]: http://www.perforce.com/
[download page]: http://www.perforce.com/downloads
