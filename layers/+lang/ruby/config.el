;;; config.el --- Ruby Layer configuration File for Spacemacs
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


;; Variables

(spacemacs|define-jump-handlers enh-ruby-mode)
(spacemacs|define-jump-handlers ruby-mode)

(defvar ruby-backend (if (configuration-layer/layer-used-p 'lsp) 'lsp 'robe)
  "Defines the backend for IDE features, defaulting to robe.
Possible values are `robe', and `lsp'.
If `nil' then `robe' is the default backend unless `lsp' layer is used.")

(defvar ruby-enable-enh-ruby-mode nil
  "If non-nil, use `enh-ruby-mode' package instead of the built-in Ruby Mode.")

(defvar ruby-version-manager nil
  "If non nil, defines the Ruby version manager.
Possible values are `rbenv', `rvm' or `chruby'.)")

(defvar ruby-test-runner 'ruby-test
  "Test runner to use. Possible values are `ruby-test', `minitest' or `rspec'.")

(defvar ruby-highlight-debugger-keywords t
  "If non-nil, enable highlight for debugger keywords.")

(defvar ruby-prettier-on-save nil
  "Use prettier and run on buffer save")
