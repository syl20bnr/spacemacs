;;; config.el --- Gleam layer config file for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Qynn Schwaab <qynn@riseup.net>
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


(spacemacs|define-jump-handlers gleam-mode)

(defvar gleam-format-on-save nil
  "If non-nil, automatically run gleam-format before save. Default is nil.")

(defvar gleam-enable-lsp (not (null (configuration-layer/layer-used-p 'lsp)))
  "Whether to enable gleam-lsp. Default is nil unless `lsp' layer is used.")

(defvar gleam-target 'erlang
  "The platform to target for `gleam-run-command'.
Possible values are `erlang' or `javascript'. Default is `erlang'.")

(defvar gleam-run-scope 'project
  "The scope for `gleam-run-command'.
Possible values are `project', `module' or `nil'. Default is `project'.")

(defvar gleam-runtime 'nodejs
  "The runtime to target when `gleam-run-target' is set to `javascript'.
Possible values are `nodejs', `deno' or `bun'. Default is `nodejs'.")

(defvar gleam-build-command "gleam build"
  "Gleam build command. Default is \"gleam build\".")

(defvar gleam-run-command "gleam run"
  "Gleam run command. Default is \"gleam run\".")

(defvar gleam-test-command "gleam test"
  "Gleam test command. Default is \"gleam test\".")
