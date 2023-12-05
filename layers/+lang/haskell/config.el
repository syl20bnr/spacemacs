;;; config.el --- Haskell Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
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

(setq haskell-modes '(haskell-mode haskell-literate-mode))

(spacemacs|define-jump-handlers haskell-mode haskell-mode-jump-to-def-or-tag)

(defvar haskell-completion-backend (if (configuration-layer/layer-used-p 'lsp) 'lsp 'dante)
  "Completion backend used by company.
Available options are `dante' and `lsp'.
If `nil' then `dante' is the default backend unless `lsp' layer is used.")

(defvar haskell-enable-hindent nil
  "Formatting with hindent; If t hindent is enabled.")
