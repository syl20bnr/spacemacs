;;; config.el --- Spacemacs Bootstrap Layer configuration File
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
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


;; Thanks to `editorconfig-emacs' for many of these
(defvar spacemacs--indent-variable-alist
  ;; Note that derived modes must come before their sources
  '(((awk-mode c-mode c++-mode java-mode
      idl-mode java-mode objc-mode pike-mode) . c-basic-offset)
    (groovy-mode . groovy-indent-offset)
    (python-mode . python-indent-offset)
    (cmake-mode . cmake-tab-width)
    (coffee-mode . coffee-tab-width)
    (cperl-mode . cperl-indent-level)
    (css-mode . css-indent-offset)
    (elixir-mode . elixir-smie-indent-basic)
    ((emacs-lisp-mode lisp-mode) . lisp-indent-offset)
    (enh-ruby-mode . enh-ruby-indent-level)
    (erlang-mode . erlang-indent-level)
    (go-mode . go-tab-width)
    (js2-mode . js2-basic-offset)
    (js3-mode . js3-indent-level)
    ((js-mode json-mode) . js-indent-level)
    (latex-mode . (LaTeX-indent-level tex-indent-basic))
    (livescript-mode . livescript-tab-width)
    (mustache-mode . mustache-basic-offset)
    (nxml-mode . nxml-child-indent)
    (perl-mode . perl-indent-level)
    (puppet-mode . puppet-indent-level)
    (ruby-mode . ruby-indent-level)
    (rust-mode . rust-indent-offset)
    (scala-mode . scala-indent:step)
    (sgml-mode . sgml-basic-offset)
    (sh-mode . sh-basic-offset)
    (typescript-mode . typescript-indent-level)
    (web-mode . web-mode-markup-indent-offset)
    (yaml-mode . yaml-indent-offset))
  "An alist where each key is either a symbol corresponding
to a major mode, a list of such symbols, or the symbol t,
acting as default. The values are either integers, symbols
or lists of these.")

(defvar vim-style-remap-Y-to-y$
  (spacemacs|dotspacemacs-backward-compatibility
   dotspacemacs-remap-Y-to-y$ nil)
  "If non nil `Y' is remapped to `y$' in Evil states.")

(defvar vim-style-retain-visual-state-on-shift
  (spacemacs|dotspacemacs-backward-compatibility
   dotspacemacs-retain-visual-state-on-shift t)
  "If non-nil, the shift mappings `<' and `>' retain visual state
if used there.")

(defvar vim-style-visual-line-move-text
  (spacemacs|dotspacemacs-backward-compatibility
   dotspacemacs-visual-line-move-text nil)
  "If non-nil, J and K move lines up and down when in visual mode.")

(defvar vim-style-enable-undo-region nil
  "If non-nil, `u' is remapped to `undo' in visual state.
Otherwise, in visual state `u' downcases visually selected text.")

(defvar vim-style-ex-substitute-global
  (spacemacs|dotspacemacs-backward-compatibility
   dotspacemacs-ex-substitute-global nil)
  "If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.")

;; State cursors
(defvar spacemacs-evil-cursors '(("normal" "DarkGoldenrod2" box)
                                 ("insert" "chartreuse3" (bar . 2))
                                 ("emacs" "SkyBlue2" box)
                                 ("hybrid" "SkyBlue2" (bar . 2))
                                 ("replace" "chocolate" (hbar . 2))
                                 ("evilified" "LightGoldenrod3" box)
                                 ("visual" "gray" (hbar . 2))
                                 ("motion" "plum3" box)
                                 ("lisp" "HotPink1" box)
                                 ("iedit" "firebrick1" box)
                                 ("iedit-insert" "firebrick1" (bar . 2)))
  "Colors assigned to evil states with cursor definitions.
To add your own, use `spacemacs/add-evil-curosr'.")
