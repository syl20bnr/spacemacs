# Private directory for local packages

The content of this directory is ignored by Git.

This is the place to store the local packages that you define in
the `dotspacemacs-additional-packages` variable of your dotfile.

Additional packages can be added using the same recipe as for [adding packages to layers](https://develop.spacemacs.org/doc/LAYERS.html#packagesel) i.e.:

- if the package is on (M)ELPA simply add the package name to the list

- for a local package add `(some-package :location local)` to the list and load or
  require it explicitly in the user/config section. The first part only adds the
  package directory (i.e. .emacs.d/private/local/package-dir/) to the load path
  so that the .el file should be placed inside the package directory. If the
  package provides a feature it can be loaded with require, otherwise it must be
  loaded using load.

- for a package hosted on github the recipe for github packages can be used i.e. add

```
(some-package :location (recipe
:fetcher github
                             :repo "some/repo"))
```

to the list. This will reinstall the package on every startup, this can be
prevented by installing the package locally as described in the previous point.
