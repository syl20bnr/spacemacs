# Company Mode

In order to use this you should disable the default `auto-complete` package plus everything that depends on it:

```elisp
(setq-default dotspacemacs-excluded-packages
  '(auto-complete ac-ispell tern-auto-complete auto-complete-clang enslime edts))
```

## Clang Fanciness

In `funcs.el` there are some fancy improvements to `company-clang`.
It includes a hook to load a projects `.clang_complete` file, which is just a text
file with one clang flag per line, a format also used by other text editor clang plugins.

Not only does this allow proper autocomplete on projects with extra includes and flags,
but I also hooked it into flycheck so that the error messages don't complain about missing
header files and skip the actual problems.

Basically, Spacemacs now has better Clang/C++ than any other Emacs config.

## Maintainer

This contrib layer was written by and should be maintained by @trishume, everyone else is
welcome to contribute.
