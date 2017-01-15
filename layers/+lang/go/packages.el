;;; packages.el --- Go Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq go-packages
      '(
        (company-go :toggle (configuration-layer/package-usedp 'company))
        flycheck
        (flycheck-gometalinter :toggle (and go-use-gometalinter
                                            (configuration-layer/package-usedp
                                             'flycheck)))
        ggtags
        helm-gtags
        exec-path-from-shell
        go-eldoc
        go-mode
        go-guru
        go-rename
        popwin
        ))


(defun go/post-init-popwin ()
  (push (cons go-test-buffer-name '(:dedicated t :position bottom :stick t :noselect t :height 0.4))
        popwin:special-display-config))

(defun go/init-company-go ()
  (use-package company-go
    :defer t
    :init
    (spacemacs|add-company-backends
      :backends company-go
      :modes go-mode
      :variables company-go-show-annotation t)))

(defun go/post-init-flycheck ()
  (spacemacs/enable-flycheck 'go-mode))

(defun go/pre-init-exec-path-from-shell ()
  (spacemacs|use-package-add-hook exec-path-from-shell
    :pre-config
    (dolist (var '("GOPATH" "GO15VENDOREXPERIMENT") exec-path-from-shell-variables)
      (unless (or (member var exec-path-from-shell-variables) (getenv var))
        (push var exec-path-from-shell-variables)))))

(defun go/init-go-mode()
  (use-package go-mode
    :defer t
    :init
    (progn
      (defun spacemacs//go-set-tab-width ()
        "Set the tab width."
        (setq-local tab-width go-tab-width))
      (add-hook 'go-mode-hook 'spacemacs//go-set-tab-width))
    :config
    (progn
      (add-hook 'before-save-hook 'gofmt-before-save)

      (defun spacemacs/go-run-tests (args)
        (interactive)
        (compilation-start (concat "go test " args) nil (lambda (n) go-test-buffer-name) nil))

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
                  (shell-quote-argument (buffer-file-name (buffer-base-buffer))))))

      (spacemacs/declare-prefix-for-mode 'go-mode "me" "playground")
      (spacemacs/declare-prefix-for-mode 'go-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'go-mode "mh" "help")
      (spacemacs/declare-prefix-for-mode 'go-mode "mi" "imports")
      (spacemacs/declare-prefix-for-mode 'go-mode "mt" "test")
      (spacemacs/declare-prefix-for-mode 'go-mode "mx" "execute")
      (spacemacs/set-leader-keys-for-major-mode 'go-mode
        "hh" 'godoc-at-point
        "ig" 'go-goto-imports
        "ia" 'go-import-add
        "ir" 'go-remove-unused-imports
        "eb" 'go-play-buffer
        "er" 'go-play-region
        "ed" 'go-download-play
        "xx" 'spacemacs/go-run-main
        "ga" 'ff-find-other-file
        "gc" 'go-coverage
        "tt" 'spacemacs/go-run-test-current-function
        "ts" 'spacemacs/go-run-test-current-suite
        "tp" 'spacemacs/go-run-package-tests
        "tP" 'spacemacs/go-run-package-tests-nested))))

(defun go/init-go-eldoc()
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(defun go/init-go-guru()
  (spacemacs/declare-prefix-for-mode 'go-mode "mf" "guru")
  (spacemacs/set-leader-keys-for-major-mode 'go-mode
    "fd" 'go-guru-describe
    "ff" 'go-guru-freevars
    "fi" 'go-guru-implements
    "fc" 'go-guru-peers
    "fr" 'go-guru-referrers
    "fj" 'go-guru-definition
    "fp" 'go-guru-pointsto
    "fs" 'go-guru-callstack
    "fe" 'go-guru-whicherrs
    "f<" 'go-guru-callers
    "f>" 'go-guru-callees
    "fo" 'go-guru-set-scope))

(defun go/init-go-rename()
  (use-package go-rename
    :init
    (spacemacs/declare-prefix-for-mode 'go-mode "mr" "rename")
    (spacemacs/set-leader-keys-for-major-mode 'go-mode "rn" 'go-rename)))

(defun go/init-flycheck-gometalinter()
  (use-package flycheck-gometalinter
    :defer t
    :init
    (add-hook 'go-mode-hook 'spacemacs//go-enable-gometalinter t)))

(defun go/post-init-ggtags ()
  (add-hook 'go-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun go/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'go-mode))
