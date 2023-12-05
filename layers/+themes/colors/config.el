;;; config.el --- Colors Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; Variables

(defvar colors-colorize-identifiers nil
  "If `variables' colorize variables, if `all' colorize all identifiers in
programming language buffers.")

(defvar colors-enable-nyan-cat-progress-bar nil
  "If non nil all nyan cat packages are enabled (for now only `nyan-mode').")

;; IMPORTANT INFORMATION FOR CONTRIBUTORS:
;; Do not edit these defaults. Let the user change them if they really want to.
;; These defaults are carefully balanced statistical averages of all colors suitable
;; for both light and dark themes, and perfected to work for 90% of all themes.
;; It's the BEST out-of-the-box experience we can offer without forcing all users
;; to write their own individual per-theme optimizations for ALL of their themes...

(defvar colors-default-rainbow-identifiers-sat 50
  "The rainbow-identifiers saturation value to use for themes that lack per-theme settings.")

(defvar colors-default-rainbow-identifiers-light 65
  "The rainbow-identifiers lightness value to use for themes that lack per-theme settings.")

;; PER-THEME CONTRIBUTIONS WELCOME:
;; Guideline: Aim at a saturation and lightness level that matches the average
;; look of the theme, so that it looks balanced against the theme's own colors.

(defvar colors-theme-identifiers-sat&light
  '(
    (doom-molokai . (45 80))
    (doom-one . (45 80))
    (flatland . (50 80))
    (gotham . (45 60))
    (gruvbox . (55 80))
    (jazz . (50 55))
    (leuven . (95 40))
    (material . (55 90))
    (material-light . (70 35))
    (monokai . (55 60))
    (sanityinc-tomorrow-blue . (40 95))
    (sanityinc-tomorrow-eighties . (30 80))
    (solarized-dark . (65 55))
    (solarized-light . (60 55))
    (spacemacs-dark . (45 70))
    (spacemacs-light . (60 45))
    (subatomic . (35 90))
    (subatomic256 . (30 85))
    (zenburn . (40 65))
    )
  "alist of theme symbols and pair of saturation and lightness values.")
