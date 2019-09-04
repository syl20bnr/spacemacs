;;; packages.el --- Go Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq go-packages
      '(
        company
        (company-go :requires company)
        counsel-gtags
        flycheck
        (flycheck-golangci-lint :toggle (and go-use-golangci-lint
                                             (configuration-layer/package-used-p
                                              'flycheck)))
        ggtags
        helm-gtags
        go-eldoc
        go-fill-struct
        go-gen-test
        go-guru
        go-impl
        go-mode
        go-rename
        go-tag
        godoctor
        popwin))

(defun go/init-company-go ()
  (use-package company-go
    :defer t
    :init (spacemacs|add-company-backends
            :backends company-go
            :modes go-mode
            :variables company-go-show-annotation t)))

(defun go/post-init-company ()
  (add-hook 'go-mode-local-vars-hook #'spacemacs//go-setup-company))

(defun go/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'go-mode))

(defun go/post-init-flycheck ()
  (spacemacs/enable-flycheck 'go-mode))

(defun go/init-flycheck-golangci-lint ()
  (use-package flycheck-golangci-lint
    :defer t
    :init (add-hook 'go-mode-hook 'spacemacs//go-enable-flycheck-golangci-lint t)))

(defun go/post-init-ggtags ()
  (add-hook 'go-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun go/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'go-mode))

(defun go/init-go-eldoc ()
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(defun go/init-go-fill-struct ()
  (use-package go-fill-struct
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'go-mode
            "rs" 'go-fill-struct)))

(defun go/init-go-gen-test()
  (use-package go-gen-test
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'go-mode "mtg" "generate")
      (spacemacs/set-leader-keys-for-major-mode 'go-mode
        "tgg" 'go-gen-test-dwim
        "tgf" 'go-gen-test-exported
        "tgF" 'go-gen-test-all))))

(defun go/init-go-guru ()
  (use-package go-impl
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'go-mode "mf" "guru")
      (spacemacs/set-leader-keys-for-major-mode 'go-mode
        "f<" 'go-guru-callers
        "f>" 'go-guru-callees
        "fc" 'go-guru-peers
        "fd" 'go-guru-describe
        "fe" 'go-guru-whicherrs
        "ff" 'go-guru-freevars
        "fi" 'go-guru-implements
        "fj" 'go-guru-definition
        "fo" 'go-guru-set-scope
        "fp" 'go-guru-pointsto
        "fr" 'go-guru-referrers
        "fs" 'go-guru-callstack))))

(defun go/init-go-impl()
  (use-package go-impl
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'go-mode
            "ri" 'go-impl)))

(defun go/init-go-mode()
  (use-package go-mode
    :defer t
    :init
    (progn
      ;; get go packages much faster
      (setq go-packages-function 'spacemacs/go-packages-gopkgs)
      (add-hook 'go-mode-hook 'spacemacs//go-set-tab-width)
      (add-hook 'go-mode-local-vars-hook
                #'spacemacs//go-setup-backend)
      (dolist (value '(lsp go-mode))
        (add-to-list 'safe-local-variable-values
                     (cons 'go-backend value)))
      (spacemacs|add-toggle go-test-verbose
        :documentation "Enable verbose test output."
        :status go-test-verbose
        :on (setq go-test-verbose t)
        :off (setq go-test-verbose nil)
        :evil-leader-for-mode (go-mode . "tv")))
    :config
    (progn
      (when go-format-before-save
        (add-hook 'before-save-hook 'gofmt-before-save))
      (spacemacs/declare-prefix-for-mode 'go-mode "me" "playground")
      (spacemacs/declare-prefix-for-mode 'go-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'go-mode "mh" "help")
      (spacemacs/declare-prefix-for-mode 'go-mode "mi" "imports")
      (spacemacs/declare-prefix-for-mode 'go-mode "mr" "refactoring")
      (spacemacs/declare-prefix-for-mode 'go-mode "mt" "test")
      (spacemacs/declare-prefix-for-mode 'go-mode "mT" "toggle")
      (spacemacs/declare-prefix-for-mode 'go-mode "mx" "execute")
      (spacemacs/set-leader-keys-for-major-mode 'go-mode
        "="  'gofmt
        "eb" 'go-play-buffer
        "ed" 'go-download-play
        "er" 'go-play-region
        "ga" 'ff-find-other-file
        "gc" 'go-coverage
        "hh" 'godoc-at-point
        "ia" 'go-import-add
        "ig" 'go-goto-imports
        "ir" 'go-remove-unused-imports
        "tP" 'spacemacs/go-run-package-tests-nested
        "tp" 'spacemacs/go-run-package-tests
        "ts" 'spacemacs/go-run-test-current-suite
        "tt" 'spacemacs/go-run-test-current-function
        "xx" 'spacemacs/go-run-main))))

(defun go/init-go-rename ()
  (use-package go-rename
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'go-mode
            "rN" 'go-rename)))

(defun go/init-go-tag ()
  (use-package go-tag
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'go-mode
            "rf" 'go-tag-add
            "rF" 'go-tag-remove)))

(defun go/init-godoctor ()
  (use-package godoctor
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'go-mode
            "rd" 'godoctor-godoc
            "re" 'godoctor-extract
            "rn" 'godoctor-rename
            "rt" 'godoctor-toggle)))

(defun go/post-init-popwin ()
  (push (cons go-test-buffer-name '(:dedicated t :position bottom :stick t :noselect t :height 0.4))
        popwin:special-display-config))
