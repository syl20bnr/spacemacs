;;; funcs.el --- Go Layer functions File for Spacemacs
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


(defun spacemacs//go-setup-backend ()
  "Conditionally setup go backend"
  (when (eq go-backend 'lsp)
    (spacemacs//go-setup-backend-lsp)))

(defun spacemacs//go-setup-company ()
  "Conditionally setup go company based on backend"
  (pcase go-backend
    ('go-mode (spacemacs|add-company-backends
                :backends company-go
                :modes go-mode
                :variables company-go-show-annotation t
                :append-hooks nil
                :call-hooks t))
    ('lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes go-mode))))

(defun spacemacs//go-setup-eldoc ()
  "Conditionally setup go eldoc based on backend"
  (when (eq go-backend 'go-mode)
    (go-eldoc-setup)))

(defun spacemacs//go-setup-dap ()
  "Conditionally setup go DAP integration."
  ;; currently DAP is only available using LSP
  (when (eq go-backend 'lsp)
    (require 'dap-go)
    (dap-go-setup)))

(defun spacemacs//go-setup-format ()
  "Conditionally setup format on save."
  (if go-format-before-save
      (add-hook 'before-save-hook 'gofmt-before-save)
    (remove-hook 'before-save-hook 'gofmt-before-save)))


;; lsp

(defun spacemacs//go-setup-backend-lsp ()
  "Setup lsp backend"
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        ;; without setting lsp-diagnostics-provider to :none
        ;; golangci-lint errors won't be reported
        (when go-use-golangci-lint
          (message "[go] Setting lsp-diagnostics-provider :none to enable golangci-lint support.")
          (setq-local lsp-diagnostics-provider :none))
        (lsp-deferred))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))


;; flycheck

(defun spacemacs//go-enable-flycheck-golangci-lint ()
  "Enable `flycheck-golangci-linter' and disable overlapping `flycheck' linters."
  (setq flycheck-disabled-checkers '(go-gofmt
                                     go-golint
                                     go-vet
                                     ;; go-build
                                     ;; go-test
                                     go-errcheck
                                     go-staticcheck
                                     go-unconvert))
  (flycheck-golangci-lint-setup)

  ;; Make sure to only run golangci after go-build
  ;; to ensure we show at least basic errors in the buffer
  ;; when golangci fails. Make also sure to run go-test if possible.
  ;; See #13580 for details
  (flycheck-add-next-checker 'go-build '(warning . golangci-lint) t)
  (flycheck-add-next-checker 'go-test '(warning . golangci-lint) t)

  ;; Set basic checkers explicitly as flycheck will
  ;; select the better golangci-lint automatically.
  ;; However if it fails we require these as fallbacks.
  (cond ((flycheck-may-use-checker 'go-test) (flycheck-select-checker 'go-test))
        ((flycheck-may-use-checker 'go-build) (flycheck-select-checker 'go-build))))


;; run

(defun spacemacs/go-run-tests (args)
  (interactive)
  (compilation-start (concat go-test-command " " (when go-test-verbose "-v ") args " " go-use-test-args)
                     nil (lambda (n) go-test-buffer-name) nil))

(defun spacemacs/go-run-package-tests ()
  (interactive)
  (spacemacs/go-run-tests ""))

(defun spacemacs/go-run-package-tests-nested ()
  (interactive)
  (spacemacs/go-run-tests "./..."))

(defun spacemacs/go-run-test-current-function ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (move-end-of-line nil)
        (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
        (spacemacs/go-run-tests
         (cond (go-use-testify-for-testing (concat "-run='Test" (match-string-no-properties 2) "' -testify.m='" (match-string-no-properties 3) "'"))
               (go-use-gocheck-for-testing (concat "-check.f='" (match-string-no-properties 3) "$'"))
               (t (concat "-run='" (match-string-no-properties 3) "$'")))))
    (message "Must be in a _test.go file to run go-run-test-current-function")))

(defun spacemacs/go-run-test-current-suite ()
  (interactive)
  (if (string-match "_test\.go" buffer-file-name)
      (if (or go-use-testify-for-testing go-use-gocheck-for-testing)
          (let ((test-method (if go-use-gocheck-for-testing
                                 "-check.f='"
                               "-run='Test")))
            (save-excursion
              (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
              (spacemacs/go-run-tests (concat test-method (match-string-no-properties 2) "'"))))
        (message "Testify or Gocheck is needed to test the current suite"))
    (message "Must be in a _test.go file to run go-test-current-suite")))

(defun spacemacs/go-run-main ()
  (interactive)
  (shell-command
   (concat go-run-command " . " go-run-args)))

;; misc
(defun spacemacs/go-packages-gopkgs ()
  "Return a list of all Go packages, using `gopkgs'."
  (sort (process-lines "gopkgs") #'string<))

(defun spacemacs//go-set-tab-width ()
  "Set the tab width."
  (when go-tab-width
    (setq-local tab-width go-tab-width)))
