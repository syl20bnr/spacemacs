# Solarized for Emacs

Solarized for Emacs is an Emacs port of the [Solarized theme for vim](http://ethanschoonover.com/solarized),
developed by Ethan Schoonover.

Solarized for Emacs is tested only under Emacs 24, but should be
working under Emacs 23 as well. The theme is implemented in terms of
customizations and `deftheme` and does not require the
`color-theme-package`.

# Installation


Solarized for Emacs is available for installation via the
[MELPA](http://melpa.milkbox.net) and
[Marmalade](http://marmalade-repo.org/) `package.el`
repositories. Assuming you've set one of the them up (I recommend
MELPA) you can install solarized like this:

`M-x package-install solarized-theme`

Afterwards - business as usual, just load one of the theme variants with `M-x
load-theme`.

(If you want to install manually that procedure is briefly documentet in the
FAQ at the end of this document.)

# Customisations

## Theme specific settings
If you don't like low-contrast modeline or fringe, you can `customize` them
either by doing `M-x customize-group solarized` or setting the values using
elisp code:


```lisp
;; make the fringe stand out from the background
(setq solarized-distinct-fringe-background t)

;; make the modeline high contrast
(setq solarized-high-contrast-mode-line t)

;; Use less bolding
(setq solarized-use-less-bold t)

;; Use more italics
(setq solarized-use-more-italic t)

;; Use less colors for indicators such as git:gutter, flycheck and similar.
(setq solarized-emphasize-indicators nil)

```
Note that these need to be set **before** `load-theme` is invoked for Solarized.

## Underline position setting for X

If you are using emacs under X you might like the following setting which puts
the underline below the
[font bottomline instead of the baseline](https://publib.boulder.ibm.com/infocenter/pseries/v5r3/topic/com.ibm.aix.graPHIGS/doc/phigstrf/figures/afma5rbd.jpg).

Ihmo it enhances the general readability and also it fits well with the default
`solarized-high-contrast-mode-line` setting which uses an slightly emphazised
underline for the modeline to create one horisontal window border in the same
manner as the vertical border.

```lisp
(setq x-underline-at-descent-line t)
```

# Bugs & Improvements

Please, report any problems that you find on the projects integrated
issue tracker. If you've added some improvements and you want them
included upstream don't hesitate to send me a patch or even better - a
GitHub pull request.

# FAQ

## Stand-alone manual installation

Save the following files in a folder that's on your Emacs' `load-path`:

* [dash.el](https://raw.githubusercontent.com/magnars/dash.el/master/dash.el) - [dash](https://github.com/magnars/dash.el), a modern list library for Emacs
* [solarized.el](https://raw.githubusercontent.com/bbatsov/solarized-emacs/master/solarized.el) - the solarized theme

Save the following files into `~/.emacs.d/themes`:

* [solarized-light-theme.el](https://raw.githubusercontent.com/bbatsov/solarized-emacs/master/solarized-light-theme.el) 
* [solarized-dark-theme.el](https://raw.githubusercontent.com/bbatsov/solarized-emacs/master/solarized-dark-theme.el)

Add this your `.emacs.d`:

```lisp
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
```

Now you can load the theme with the interactive function `load-theme`.


# Contributors

- [Thomas Frössman](http://t.jossystem.se)

(Add yourself to the list)
