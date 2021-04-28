;;; config.el --- Clojure Layer configuration File for Spacemacs
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


;; Variables

(spacemacs|define-jump-handlers clojure-mode)
(spacemacs|define-jump-handlers clojurec-mode)
(spacemacs|define-jump-handlers clojurescript-mode)
(spacemacs|define-jump-handlers clojurex-mode)
(spacemacs|define-jump-handlers cider-repl-mode)

(defvar clojure-enable-fancify-symbols nil
  "If non-nil, the `fancify-symbols' function is enabled.")

(defvar clojure-enable-sayid nil
  "If non-nil, the Sayid Clojure debugger is enabled.")

(defvar clojure-enable-clj-refactor nil
  "If non-nil, the clj-refactor is enabled.")

(defvar clojure-enable-linters nil
  "If non-nil, enable clojure linters.")

(defvar clojure-backend (if (configuration-layer/layer-used-p 'lsp) 'lsp 'cider)
  "The backend to use for IDE features.
Possible values are `lsp' and `cider'.
If `nil' then 'cider` is the default backend unless `lsp' layer is used")
