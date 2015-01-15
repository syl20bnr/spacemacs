# Auctex Layer

Includes support for Auctex including some bindings, sane defaults, and manual loading for better startup times.

## Company Auctex

Along with other things this layer includes company-auctex, it's only enabled if you also enable my other contrib layer `company-mode`.

## Keybindings

Key Binding         |                 Description
--------------------|------------------------------------------------------------------
<kbd>SPC m b  </kbd>| build and view
<kbd>SPC m e  </kbd>| insert LaTeX environment
<kbd>SPC m c  </kbd>| close LaTeX environment
<kbd>SPC m i  </kbd>| insert `\item`
<kbd>SPC m f  </kbd>| insert LaTeX font - full bindings here: [AUCTeX documentation](https://www.gnu.org/software/auctex/manual/auctex/Font-Specifiers.html)
<kbd>SPC m C  </kbd>| TeX command on master file
<kbd>SPC m p r <kbd>| preview region
<kbd>SPC m p b</kbd>| preview buffer
<kbd>SPC m p d</kbd>| preview document
<kbd>SPC m p e</kbd>| preview environment
<kbd>SPC m p s</kbd>| preview section
<kbd>SPC m p p</kbd>| preview at point
<kbd>SPC m p f</kbd>| cache preamble for preview
<kbd>SPC m p c</kbd>| clear previews
<kbd>SPC m *</kbd>  | TeX documentation, can be very slow

## Maintainer

This layer was created by and is maintained by @trishume, ping me in the Gitter chat if you have questions. Feel free to submit
PRs for this layer though if you have improvements.
