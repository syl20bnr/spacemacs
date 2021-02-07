# Private directory for local packages

The content of this directory is ignored by Git.

This is the place to store the local packages that you define in the
`dotspacemacs-additional-packages` variable of your dotfile.

Additional packages can be added using the same recipe as for [adding packages
to layers](https://develop.spacemacs.org/doc/LAYERS.html#packagesel) i.e.:

- For a local package:
  - Load the file explicitly, using the full path to the file, by placing a
  `(load "~/.emacs.d/private/local/package-name")` within the body of the
  `dotspacemacs/user-config` function of your dotspacemacs file.
  - Alternatively create a directory with the name of the package in the
  `.emacs.d/private/local` directory, and add that directory to the load-path
  variable by adding `(some-package :location local)` to the list
  `dotspacemacs-additional-packages` within the `dotspacemacs/layers` function
  of your dotspacemacs file. After placing your package file into this
  package-directory the file can be loaded, without requiring the full path, by
  placing a `(require 'package-name)` within the body of the
  `dotspacemacs/user-config` function of your dotspacemacs file.

- If the package is on (M)ELPA simply add the package name to the list
  `dotspacemacs-additional-packages` in your dotspacemacs file

- For a package hosted on github the recipe for github packages can be used i.e. add

```
(some-package :location (recipe
                         :fetcher github
                         :repo "some/repo"))
```

to the list `dotspacemacs-additional-packages` in your dotspacemacs file.
