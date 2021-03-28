;;; config.el --- Haskell Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Bjarke Vad Andersen <bjarke.vad90@gmail.com>
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

(defconst spacemacs--haskell-modes '(haskell-mode haskell-literate-mode))

(spacemacs|define-jump-handlers haskell-mode haskell-mode-jump-to-def-or-tag)
(spacemacs|define-jump-handlers haskell-literate-mode haskell-mode-jump-to-def-or-tag)

(defvar haskell-completion-backend (if (configuration-layer/layer-used-p 'lsp) 'lsp 'dante)
  "Completion backend used by company.
Available options are `dante' and `lsp'.
If nil then `dante' is the default backend unless `lsp' layer is used.")

;; TODO: verify whether `stylish-haskell' breaks flycheck highlighting
;; reported by taksuyu 2015-10-06
(defvar haskell-formatter-backend (cond
                                   (haskell-enable-hindent 'hindent)
                                   ((executable-find "brittany") 'brittany)
                                   ((executable-find "ormolu") 'ormolu)
                                   ((and (executable-find "fourmolu")
                                         (eq haskell-completion-backend 'lsp))
                                    'fourmolu)
                                   ((and (executable-find "floskell")
                                         (eq haskell-completion-backend 'lsp))
                                    'floskell))
  "Formatter backend for `haskell-mode' and `haskell-literate-mode'.
Valid values are: `brittany', `hindent', `ormolu', `stylish-haskell',
`fourmolu', `floskell' and nil. `fourmolu' and `floskell' are valid only when
`haskell-completion-backend' is `lsp'.
For backward compatibility, defaults to `hindent' when `haskell-enable-hindent'
is set.
Otherwise, use the first formatter found in `brittany', `ormolu', `fourmolu' and
and `floskell'.
`stylish-haskell' is never used as default due to past report of issues.")

(defvar haskell-enable-cmm-mode nil
  "When non-nil, enable cmm-mode for C-- language.")

(defvar haskell-enable-hlint t
  "When non-nil, enable hlint integration.
When `haskell-completion-backend' is `lsp', use its builtin support.
When it's `dante', use `hlint-refactor-mode'.")

;; This should be deprecated at some point, use haskell-formatter-backend instead
(defvar haskell-enable-hindent nil
  "When non-nil, use hindent as formatter.")
