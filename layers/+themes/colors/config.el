;;; config.el --- Colors Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar colors-colorize-identifiers nil
  "If `variables' colorize variables, if `all' colorize all identifiers in
programming language buffers.")

(defvar colors-enable-nyan-cat-progress-bar nil
  "If non nil all nyan cat packages are enabled (for now only `nyan-mode').")

(defvar colors-default-rainbow-identifiers-sat 80
  "The rainbow-identifiers saturation value to use for themes that lack per-theme settings.")

(defvar colors-default-rainbow-identifiers-light 45
  "The rainbow-identifiers lightness value to use for themes that lack per-theme settings.")

(defvar colors-theme-identifiers-sat&light
  '(
    (doom-molokai . (50 95))
    (doom-one . (50 90))
    (flatland . (40 80))
    (gotham . (45 60))
    (gruvbox . (55 90))
    (jazz . (50 55))
    (leuven . (95 40))
    (material . (70 100))
    (material-light . (70 30))
    (monokai . (55 60))
    (sanityinc-tomorrow-blue . (40 95))
    (sanityinc-tomorrow-eighties . (30 85))
    (solarized-dark . (65 55))
    (solarized-light . (60 55))
    (spacemacs-dark . (45 70))
    (spacemacs-light . (60 45))
    (subatomic . (35 90))
    (subatomic256 . (30 85))
    (zenburn . (40 65))
    )
  "alist of theme symbols and pair of saturation and lightness values.")
