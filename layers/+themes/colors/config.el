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

(defvar colors-theme-identifiers-sat&light
  '((jazz . (50 55))
    (gotham . (45 60))
    (leuven . (100 40))
    (material . (95 105))
    (monokai . (55 60))
    (solarized-dark . (65 55))
    (solarized-light . (60 55))
    (spacemacs-light . (65 45))
    (spacemacs-dark . (125 100))
    (zenburn . (40 65)))
  "alist of theme symbols and pair of saturation and lightness values.")
