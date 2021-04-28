;;; config.el --- Julia Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Adam Beckmeyer <adam_git@thebeckmeyers.xyz>
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

(spacemacs|define-jump-handlers julia-mode)

;; ess-mode is what the majority of people developing julia in emacs currently use
(defvar julia-mode-enable-ess nil
  "If non-nil, enable ESS in julia-mode buffers and disable julia-repl.")

;; disabled by default since most won't have lsp-mode working
(defvar julia-backend (when (configuration-layer/layer-used-p 'lsp) 'lsp)
  "Set to 'lsp to enable use of LanguageServer.jl")
