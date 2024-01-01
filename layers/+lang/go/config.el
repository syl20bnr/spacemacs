;;; config.el --- Go Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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

(spacemacs|define-jump-handlers go-mode godef-jump)

(spacemacs|defc go-backend (if (configuration-layer/layer-used-p 'lsp) 'lsp 'go-mode)
  "The backend to use for IDE features.
Possible values are `lsp' and `go-mode'.
If not set then `go-mode' is the default backend unless `lsp' layer is used."
  '(choice (const lsp) (const go-mode)) nil t)

(spacemacs|defc go-use-gocheck-for-testing nil
  "If using gocheck for testing when running the tests -check.f will be used instead of -run to specify the test that will be ran. Gocheck is mandatory for testing suites."
  'boolean nil t)

(spacemacs|defc go-use-testify-for-testing nil
  "If using testify for testing when running the tests -testify.m will be used instead of -run to specify the test that will be ran. Testify is mandatory for testing suites."
  'boolean nil t)

(spacemacs|defc go-format-before-save nil
  "Use gofmt before save. Set to non-nil to enable gofmt before saving. Default is nil."
  'boolean nil t)

(spacemacs|defc go-tab-width 8
  "Set the `tab-width' in Go mode. Default is 8."
  'integer nil #'integerp)

(spacemacs|defc go-use-golangci-lint nil
  "Use `golangci-lint' if the variable has non-nil value."
  'boolean nil t)

(spacemacs|defc go-test-buffer-name "*go test*"
  "Name of the buffer for go test output. Default is *go test*."
  'string nil t)

(spacemacs|defc go-use-test-args ""
  "Additional arguments to be supplied to `go test` during runtime."
  'string nil t)

(spacemacs|defc go-test-verbose nil
  "Control verbosity of `go test` output"
  'boolean nil t)

(spacemacs|defc go-run-args ""
  "Additional arguments to by supplied to `go run` during runtime."
  'string nil t)

(spacemacs|defc go-run-command "go run"
  "Go run command. Default is `go run`."
  'string nil t)

(spacemacs|defc go-test-command "go test"
  "Go test command. Default is `go test`."
  'string nil t)

(spacemacs|defc go-generate-command "go generate"
  "Go generate command. Default is `go generate`."
  'string nil t)

(spacemacs|defc go-generate-buffer-name "*go gen*"
  "Name of the buffer for go generate output. Default is *go gen*."
  'string nil t)

(spacemacs|defc go-dap-mode 'dap-dlv-go
  "Go dap mode. This variable defines which kind of dap integration will be used.

Default is `dap-dlv-mode' which is completely self-contained.
Alternatively the depreciated vscode integration can be used, to do so set this variable to `dap-go'.
Remember that the legacy integration requires vscode extensions to work properly."
  '(choice (const dap-dlv-go) (const dap-go)) nil t)
