;;; config.el --- Space-macs Bootstrap Layer configuration File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; Thanks to `editorconfig-e-macs' for many of these
(defvar space-macs--indent-variable-alist
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
    ((e-macs-lisp-mode lisp-mode) . lisp-indent-offset)
    (enh-ruby-mode . enh-ruby-indent-level)
    (erlang-mode . erlang-indent-level)
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
  (space-macs|dotspace-macs-backward-compatibility
   dotspace-macs-remap-Y-to-y$ nil)
  "If non nil `Y' is remapped to `y$' in Evil states.")

(defvar vim-style-retain-visual-state-on-shift
  (space-macs|dotspace-macs-backward-compatibility
   dotspace-macs-retain-visual-state-on-shift t)
  "If non-nil, the shift mappings `<' and `>' retain visual state
if used there.")

(defvar vim-style-visual-line-move-text
  (space-macs|dotspace-macs-backward-compatibility
   dotspace-macs-visual-line-move-text nil)
  "If non-nil, J and K move lines up and down when in visual mode.")

(defvar vim-style-ex-substitute-global
  (space-macs|dotspace-macs-backward-compatibility
   dotspace-macs-ex-substitute-global nil)
  "If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.")

;; State cursors
(defvar space-macs-evil-cursors '(("normal" "DarkGoldenrod2" box)
                                 ("insert" "chartreuse3" (bar . 2))
                                 ("e-macs" "SkyBlue2" box)
                                 ("hybrid" "SkyBlue2" (bar . 2))
                                 ("replace" "chocolate" (hbar . 2))
                                 ("evilified" "LightGoldenrod3" box)
                                 ("visual" "gray" (hbar . 2))
                                 ("motion" "plum3" box)
                                 ("lisp" "HotPink1" box)
                                 ("iedit" "firebrick1" box)
                                 ("iedit-insert" "firebrick1" (bar . 2)))
  "Colors assigned to evil states with cursor definitions.
To add your own, use `space-macs/add-evil-curosr'.")


