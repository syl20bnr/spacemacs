# Private directory for local packages

The content of this directory is ignored by Git.

This is the place to store the local packages that you define in the
`dotspace-macs-additional-packages` variable of your dotfile.

Additional packages can be added using the same recipe as for [adding packages
to layers](https://develop.space-macs.org/doc/LAYERS.html#packagesel) i.e.:

- For a local package:
  - Load the file explicitly, using the full path to the file, by placing a
  `(load "~/.e-macs.d/private/local/package-name")` within the body of the
  `dotspace-macs/user-config` function of your dotspace-macs file.
  - Alternatively create a directory with the name of the package in the
  `.e-macs.d/private/local` directory, and add that directory to the load-path
  variable by adding `(some-package :location local)` to the list
  `dotspace-macs-additional-packages` within the `dotspace-macs/layers` function
  of your dotspace-macs file. After placing your package file into this
  package-directory the file can be loaded, without requiring the full path, by
  placing a `(require 'package-name)` within the body of the
  `dotspace-macs/user-config` function of your dotspace-macs file.

- If the package is on (M)ELPA simply add the package name to the list
  `dotspace-macs-additional-packages` in your dotspace-macs file

- For a package hosted on github the recipe for github packages can be used i.e. add

```
(some-package :location (recipe
                         :fetcher github
                         :repo "some/repo"))
```

to the list `dotspace-macs-additional-packages` in your dotspace-macs file.


