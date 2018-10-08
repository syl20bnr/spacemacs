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

(defun spacemacs//go-set-tab-width ()
  "Set the tab width."
  (when go-tab-width
    (setq-local tab-width go-tab-width)))

(defun spacemacs//go-setup-backend ()
  "Conditionally setup go backend"
  (pcase go-backend
    ('lsp (spacemacs//go-setup-backend-lsp))))

(defun spacemacs//go-setup-company ()
  "Conditionally setup go company based on backend"
  (pcase go-backend
    ('go-mode (spacemacs//go-setup-company-go))
    ('lsp (spacemacs//go-setup-company-lsp))))

(defun spacemacs//go-setup-company-go ()
  (spacemacs|add-company-backends
    :backends company-go
    :modes go-mode
    :variables company-go-show-annotation t
    :append-hooks nil
    :call-hooks t)
  (company-mode))

(defun spacemacs//go-setup-backend-lsp ()
  "Setup lsp backend"
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (require 'lsp-go)
        (lsp-go-enable))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//go-setup-company-lsp ()
  "Setup lsp auto-completion"
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (spacemacs|add-company-backends
          :backends company-lsp
          :modes go-mode
          :append-hooks nil
          :call-hooks t)
        (company-mode))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//go-enable-gometalinter ()
   "Enable `flycheck-gometalinter' and disable overlapping `flycheck' linters."
   (setq flycheck-disabled-checkers '(go-gofmt
                                      go-golint
                                      go-vet
                                      go-build
                                      go-test
                                      go-errcheck))
   (flycheck-gometalinter-setup))

(defun spacemacs//go-enable-golangci-lint ()
  "Enable `flycheck-golangci-lint' and disable overlapping `flycheck' linters."
  (setq flycheck-disabled-checkers '(go-gofmt
                                     go-golint
                                     go-vet
                                     go-build
                                     go-test
                                     go-errcheck))
  (flycheck-golangci-lint-setup))

(defun spacemacs/go-run-tests (args)
  (interactive)
  (compilation-start (concat "go test " args " " go-use-test-args)
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
      (let ((test-method (if go-use-gocheck-for-testing
                             "-check.f"
                           "-run")))
        (save-excursion
          (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
          (spacemacs/go-run-tests (concat test-method "='" (match-string-no-properties 2) "$'"))))
    (message "Must be in a _test.go file to run go-run-test-current-function")))

(defun spacemacs/go-run-test-current-suite ()
  (interactive)
  (if (string-match "_test\.go" buffer-file-name)
      (if go-use-gocheck-for-testing
          (save-excursion
            (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?Test[[:alnum:]_]+(.*)")
            (spacemacs/go-run-tests (concat "-check.f='" (match-string-no-properties 2) "'")))
        (message "Gocheck is needed to test the current suite"))
    (message "Must be in a _test.go file to run go-test-current-suite")))

(defun spacemacs/go-run-main ()
  (interactive)
  (shell-command
   (format "go run %s"
           (shell-quote-argument (or (file-remote-p (buffer-file-name (buffer-base-buffer)) 'localname)
                                     (buffer-file-name (buffer-base-buffer)))))))
(defun spacemacs/go-packages-gopkgs ()
  "Return a list of all Go packages, using `gopkgs'."
  (sort (process-lines "gopkgs") #'string<))
