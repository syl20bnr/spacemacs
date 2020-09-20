;;; funcs.el --- Go Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//go-backend ()
  "Returns selected backend."
  (if go-backend
      go-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'go-mode))))

(defun spacemacs//go-setup-backend ()
  "Conditionally setup go backend"
  (pcase (spacemacs//go-backend)
    ('lsp (spacemacs//go-setup-backend-lsp))))

(defun spacemacs//go-setup-company ()
  "Conditionally setup go company based on backend"
  (pcase (spacemacs//go-backend)
    ('go-mode (spacemacs|add-company-backends
                :backends company-go
                :modes go-mode
                :variables company-go-show-annotation t
                :append-hooks nil
                :call-hooks t))
    (`lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes go-mode))))

(defun spacemacs//go-setup-eldoc ()
  "Conditionally setup go eldoc based on backend"
  (pcase (spacemacs//go-backend)
    ('go-mode (go-eldoc-setup))))

(defun spacemacs//go-setup-dap ()
  "Conditionally setup go DAP integration."
  ;; currently DAP is only available using LSP
  (pcase (spacemacs//go-backend)
    (`lsp (require 'dap-go)
          (dap-go-setup))))


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
        (lsp))
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
   (format (concat go-run-command " %s %s")
           (shell-quote-argument (or (file-remote-p (buffer-file-name (buffer-base-buffer)) 'localname)
                                     (buffer-file-name (buffer-base-buffer))))
           go-run-args)))


;; misc

(defun spacemacs/go-packages-gopkgs ()
  "Return a list of all Go packages, using `gopkgs'."
  (sort (process-lines "gopkgs") #'string<))

(defun spacemacs//go-set-tab-width ()
  "Set the tab width."
  (when go-tab-width
    (setq-local tab-width go-tab-width)))
