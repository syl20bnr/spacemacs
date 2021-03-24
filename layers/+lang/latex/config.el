;;; config.el --- Latex Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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


;; variables

;; Even though AUCTeX uses TeX-latex-mode rather than latex-mode, major-mode
;; will still be bound to 'latex-mode (since AUCTeX uses an advice to override
;; latex-mode with TeX-latex-mode), so the keymap's name should use the
;; lowercase form, since bind-map uses the value of major-mode...
(spacemacs|define-jump-handlers latex-mode dumb-jump-go)
;; ...but AUCTeX runs LaTeX-mode-hook rather than latex-mode-hook, so:
(add-hook 'LaTeX-mode-hook #'spacemacs//init-jump-handlers-latex-mode)

(defvar latex-backend nil
  "The backend to use for IDE features.
Possible values are `lsp' and `company-auctex'.
If `nil' then 'company-auctex` is the default backend unless `lsp' layer is used")

(defvar latex-build-command (if (executable-find "latexmk") "LatexMk" "LaTeX")
  "The default command to use with `SPC m b'")

(defvar latex-build-engine (if (and (executable-find "xetex")
                                    (or (configuration-layer/layer-used-p 'chinese)
                                        (configuration-layer/layer-used-p 'japanese)))
                               'xetex
                             'default)
  "The default TeX engine to use with `SPC m b'. It's set to `xetex' when `xetex' is found on PATH
and either `chinese' layer or `japanese' layer is in use.

Allowed values are defined in `TeX-engine-alist'. The default allowed values are:
* `default'
* `luatex'
* `omega'
* `xetex'")

(defvar latex-enable-auto-fill t
  "Whether to use auto-fill-mode or not in tex files.")

(defvar latex-enable-folding nil
  "Whether to use `TeX-fold-mode' or not in tex/latex buffers.")

(defvar latex-enable-magic nil
  "Whether to enable \"magic\" symbols in the buffer.")

(defvar latex-nofill-env '("equation"
                           "equation*"
                           "align"
                           "align*"
                           "tabular"
                           "tabular*"
                           "tabu"
                           "tabu*"
                           "tikzpicture")
  "List of environment names in which `auto-fill-mode' will be inhibited.")

(defvar latex-refresh-preview nil
  "Whether non-nil, refresh the preview buffer when file changes.")
