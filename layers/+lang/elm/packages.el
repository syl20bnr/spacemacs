;;; packages.el --- elm Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst elm-packages
  '(
    company
    elm-mode
    elm-test-runner
    flycheck
    (flycheck-elm :requires flycheck)
    popwin
    smartparens))

(defun elm/post-init-company ()
  (space-macs//elm-setup-company))

(defun elm/post-init-flycheck ()
  (space-macs/enable-flycheck 'elm-mode))

(defun elm/init-flycheck-elm ()
  "Initialize flycheck-elm"
  (use-package flycheck-elm
    :defer t
    :init (add-hook 'flycheck-mode-hook 'flycheck-elm-setup t)))

(defun elm/init-elm-mode ()
  "Initialize elm-mode"
  (use-package elm-mode
    :mode ("\\.elm\\'" . elm-mode)
    :defer t
    :init
    (progn
      (space-macs/register-repl 'elm-mode 'elm-repl-load "elm")
      (add-hook 'elm-mode-hook 'space-macs//elm-setup-backend))
    :config
    (progn
      ;; Bind non-lsp keys
      (when (eq (space-macs//elm-backend) 'company-elm)
        (space-macs/set-leader-keys-for-major-mode 'elm-mode
          ;; format
          "=b" 'elm-format-buffer
          ;; oracle
          "hh" 'elm-oracle-doc-at-point
          "ht" 'elm-oracle-type-at-point)

        ;; Bind prefixes
        (dolist (x '(("m=" . "format")
                     ("mh" . "help")
                     ("mg" . "goto")
                     ("mr" . "refactor")))
          (space-macs/declare-prefix-for-mode 'elm-mode (car x) (cdr x))))

      ;; Bind general keys
      (space-macs/set-leader-keys-for-major-mode 'elm-mode
        ;; refactoring
        "ri" 'elm-sort-imports
        ;; repl
        "'"  'elm-repl-load
        "si" 'elm-repl-load
        "sf" 'elm-repl-push-decl
        "sF" 'space-macs/elm-repl-push-decl-focus
        "sr" 'elm-repl-push
        "sR" 'space-macs/elm-repl-push-focus
        ;; make
        "cb" 'elm-compile-buffer
        "cB" 'space-macs/elm-compile-buffer-output
        "cm" 'elm-compile-main
        ;; reactor
        "Rn" 'elm-preview-buffer
        "Rm" 'elm-preview-main
        ;; package
        "pi" 'elm-import
        "pc" 'elm-package-catalog
        "pd" 'elm-documentation-lookup)

      ;; Bind prefixes
      (dolist (x '(("mp" . "package")
                   ("mc" . "compile")
                   ("mR" . "reactor")
                   ("ms" . "repl")))
        (space-macs/declare-prefix-for-mode 'elm-mode (car x) (cdr x)))

      (evilified-state-evilify elm-package-mode elm-package-mode-map
        "g" 'elm-package-refresh
        "v" 'elm-package-view
        "m" 'elm-package-mark
        "u" 'elm-package-unmark
        "x" 'elm-package-install
        "q" 'quit-window))))

(defun elm/init-elm-test-runner ()
  (use-package elm-test-runner
    :after elm-mode
    :init
    (progn
      (space-macs/declare-prefix-for-mode 'elm-mode "mt" "test")
      (space-macs/set-leader-keys-for-major-mode 'elm-mode
        "tb" 'elm-test-runner-run
        "td" 'elm-test-runner-run-directory
        "tp" 'elm-test-runner-run-project
        "tr" 'elm-test-runner-rerun
        "tw" 'elm-test-runner-watch
        "t TAB" 'elm-test-runner-toggle-test-and-target))))

(defun elm/pre-init-popwin ()
  (space-macs|use-package-add-hook popwin
    :post-config
    (push '("*elm*" :tail t :noselect t) popwin:special-display-config)
    (push '("*elm-make*" :tail t :noselect t) popwin:special-display-config)))

(defun elm/post-init-smartparens ()
  (if dotspace-macs-smartparens-strict-mode
      (add-hook 'elm-mode-hook #'smartparens-strict-mode)
    (add-hook 'elm-mode-hook #'smartparens-mode)))


