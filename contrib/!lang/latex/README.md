# LaTeX Layer for Spacemacs

![logo](img/latex.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [LaTeX Layer for Spacemacs](#latex-layer-for-spacemacs)
    - [Description](#description)
    - [Install](#install)
        - [Layer](#layer)
        - [Auto-completion](#auto-completion)
        - [Previewing](#previewing)
        - [Build command](#build-command)
        - [Auto-fill](#auto-fill)
    - [Keybindings](#keybindings)
        - [RefTeX](#reftex)
    - [Maintainer](#maintainer)

<!-- markdown-toc end -->

## Description

This layer adds support for LaTeX files with [AucTeX][].

**Features:**
- auto-completion with [company-auctex][]
- tags navigation on <kbd>%</kbd> with [evil-matchit][]
- labels, references, citations and index entries management with [RefTeX][]

## Install

### Layer

To use this contribution add it to your `~/.spacemacs`

```elisp
(setq-default dotspacemacs-configuration-layers '(auctex))
```

### Auto-completion

Add the layer `auto-completion` to the variable
`dotspacemacs-configuration-layers` of your dotfile `~/.spacemacs`.

### Previewing

To perform full-document previews (that is, aside from the inline previewing
under <kbd>SPC m p</kbd>), add the following to your `.spacemacs`
under `dotspacemacs/config`:

```elisp
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
```

Then when you open up a compiled PDF, the preview will update automatically
when you recompile.

### Build command

A build command can be specific via the layer variable `latex-build-command`.

The default value is `"LaTeX"`. If `"LatexMk"` is specified, the appropriate
`LatexMk` configuration will be applied.

```elisp
dotspacemacs-configuration-layers '(
  (auctex :variables latex-build-command "LatexMk"))
```

### Auto-fill

To enable auto-fill set the variable `latex-enable-auto-fill` to `t`.

The variable `latex-nofill-env` provide the list of environment names where
`auto-fille-mode` will be inhibited.

```elisp
dotspacemacs-configuration-layers '(
  (auctex :variables latex-enable-auto-fill t))
```

## Keybindings

Key Binding         |                 Description
--------------------|------------------------------------------------------------------
<kbd>SPC m *</kbd>  | TeX documentation, can be very slow
<kbd>SPC m b</kbd>  | build
<kbd>SPC m c</kbd>  | close LaTeX environment
<kbd>SPC m C</kbd>  | TeX command on master file
<kbd>SPC m e</kbd>  | insert LaTeX environment
<kbd>SPC m f</kbd>  | insert LaTeX font - full bindings here: [AUCTeX documentation][AUCTeX Font]
<kbd>SPC m i</kbd>  | insert `\item`
<kbd>SPC m p r<kbd> | preview region
<kbd>SPC m p b</kbd>| preview buffer
<kbd>SPC m p d</kbd>| preview document
<kbd>SPC m p e</kbd>| preview environment
<kbd>SPC m p s</kbd>| preview section
<kbd>SPC m p p</kbd>| preview at point
<kbd>SPC m p f</kbd>| cache preamble for preview
<kbd>SPC m p c</kbd>| clear previews
<kbd>SPC m v  </kbd>| view


### RefTeX

Key Binding            |                 Description
-----------------------|------------------------------------------------------------------
<kbd>SPC m r c</kbd>   | reftex-citation
<kbd>SPC m r g</kbd>   | reftex-grep-document
<kbd>SPC m r i</kbd>   | reftex-index-selection-or-word
<kbd>SPC m r I</kbd>   | reftex-display-index
<kbd>SPC m r C-i</kbd> | reftex-index
<kbd>SPC m r l</kbd>   | reftex-label
<kbd>SPC m r p</kbd>   | reftex-index-phrase-selection-or-word
<kbd>SPC m r P</kbd>   | reftex-index-visit-phrases-buffer
<kbd>SPC m r r</kbd>   | reftex-reference
<kbd>SPC m r s</kbd>   | reftex-search-document
<kbd>SPC m r t</kbd>   | reftex-toc
<kbd>SPC m r T</kbd>   | reftex-toc-recenter
<kbd>SPC m r v</kbd>   | reftex-view-crossref

## Maintainer

This layer was created by and is maintained by @trishume, ping me in the Gitter
chat if you have questions. Feel free to submit PRs for this layer though if
you have improvements.

[AUCTex]: https://savannah.gnu.org/projects/auctex/
[AUCTeX Font]: https://www.gnu.org/software/auctex/manual/auctex/Font-Specifiers.html
[RefTeX]: http://www.gnu.org/software/emacs/manual/html_node/reftex/index.html
[evil-matchit]: https://github.com/redguardtoo/evil-matchit
