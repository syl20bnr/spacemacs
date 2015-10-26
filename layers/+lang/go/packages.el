(setq go-packages
      '(
        company
        company-go
        flycheck
        go-mode
        go-eldoc
        ))

(defun go/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'go-mode))

(defun go/init-go-mode()
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "GO15VENDOREXPERIMENT"))

  (use-package go-mode
    :defer t
    :config
    (progn
      (add-hook 'before-save-hook 'gofmt-before-save)

      (defun spacemacs/go-run-tests (args)
        (interactive)
        (save-selected-window
          (async-shell-command (concat "go test " args))))

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
                  (re-search-backward "^func[ ]+([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\(Test[[:alnum:]]+\\)(.*)")
                  (spacemacs/go-run-tests (concat test-method "='" (match-string-no-properties 2) "'"))))
          (message "Must be in a _test.go file to run go-run-test-current-function")))

      (defun spacemacs/go-run-test-current-suite ()
        (interactive)
        (if (string-match "_test\.go" buffer-file-name)
            (if go-use-gocheck-for-testing
                (save-excursion
                    (re-search-backward "^func[ ]+([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\(Test[[:alnum:]]+\\)(.*)")
                    (spacemacs/go-run-tests (concat "-check.f='" (match-string-no-properties 1) "'")))
              (message "Gocheck is needed to test the current suite"))
          (message "Must be in a _test.go file to run go-test-current-suite")))

      (defun spacemacs/go-run-main ()
        (interactive)
        (shell-command
          (format "go run %s"
                  (shell-quote-argument (buffer-file-name)))))

      (evil-leader/set-key-for-mode 'go-mode
        "mhh" 'godoc-at-point
        "mig" 'go-goto-imports
        "mia" 'go-import-add
        "mir" 'go-remove-unused-imports
        "meb" 'go-play-buffer
        "mer" 'go-play-region
        "med" 'go-download-play
        "mxx" 'spacemacs/go-run-main
        "mga" 'ff-find-other-file
        "mgg" 'godef-jump
        "mtt" 'spacemacs/go-run-test-current-function
        "mts" 'spacemacs/go-run-test-current-suite
        "mtp" 'spacemacs/go-run-package-tests
        "mtP" 'spacemacs/go-run-package-tests-nested))))

(defun go/init-go-eldoc()
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun go/post-init-company ()
    (spacemacs|add-company-hook go-mode))

  (defun go/init-company-go ()
    (use-package company-go
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :init
      (push 'company-go company-backends-go-mode))))
