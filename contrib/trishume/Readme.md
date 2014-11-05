# Tristan Hume's Contrib Layer

Mostly consists of support for additional languages including:
LaTeX, Idris, OpenSCAD, Julia, Arduino, QML, Lua

Also adds smooth scrolling and Helm Ag support.

## Company Auctex

Along with other things this layer includes company-auctex, it's only enabled if you also enable my other contrib layer `company-mode`.
The autocompletion it offers is great and almost works, unlike auto-complete-auctex which lags so hard it's worthless.

The problem is the almost. There are two issues that need to be fixed for it to be nice:

- https://github.com/capitaomorte/yasnippet/issues/537 To allow you to delete expanded macro braces, this will need to be merged.
- https://github.com/alexeyr/company-auctex/pull/3 For completing environments like `align*` properly, this will need to be merged.

Currently I've manually applied both of these patches to the folders in my `elpa` directory, which isn't a good solution.
If the patches aren't merged in a reasonable amount of time I'll add the patched forks/branches as extension submodules.
