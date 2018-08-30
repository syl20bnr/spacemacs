;;; packages.el --- elm Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq elm-packages
    '(
      company
      elm-mode
      elm-test-runner
      flycheck
      (flycheck-elm :requires flycheck)
      popwin
      smartparens
      ))

(defun elm/post-init-company ()
  (spacemacs|add-company-backends :backends company-elm :modes elm-mode)
  (add-hook 'elm-mode-hook 'elm-oracle-setup-completion))

(defun elm/post-init-flycheck ()
  (add-hook 'elm-mode-hook 'flycheck-mode))

(defun elm/init-flycheck-elm ()
  "Initialize flycheck-elm"
  (use-package flycheck-elm
    :defer t
    :init (add-hook 'flycheck-mode-hook 'flycheck-elm-setup t)))

(defun elm/init-elm-mode ()
  "Initialize elm-mode"
  (use-package elm-mode
    :mode ("\\.elm\\'" . elm-mode)
    :init
    (progn
      (spacemacs/register-repl 'elm-mode 'elm-repl-load "elm")

      (defun spacemacs/init-elm-mode ()
        "Disable electric-indent-mode and let indentation cycling feature work"
        (if (fboundp 'electric-indent-local-mode)
            (electric-indent-local-mode -1)))

      (add-hook 'elm-mode-hook 'spacemacs/init-elm-mode))
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'elm-mode
        ;; format
        "=b" 'elm-mode-format-buffer
        ;; make
        "cb" 'elm-compile-buffer
        "cB" 'spacemacs/elm-compile-buffer-output
        "cm" 'elm-compile-main
        ;; oracle
        "hh" 'elm-oracle-doc-at-point
        "ht" 'elm-oracle-type-at-point
        ;; refactoring
        "ri" 'elm-sort-imports
        ;; repl
        "'"  'elm-repl-load
        "si" 'elm-repl-load
        "sf" 'elm-repl-push-decl
        "sF" 'spacemacs/elm-repl-push-decl-focus
        "sr" 'elm-repl-push
        "sR" 'spacemacs/elm-repl-push-focus
        ;; reactor
        "Rn" 'elm-preview-buffer
        "Rm" 'elm-preview-main
        ;; package
        "pi" 'elm-import
        "pc" 'elm-package-catalog
        "pd" 'elm-documentation-lookup)

      (dolist (x '(("m=" . "format")
                   ("mc" . "compile")
                   ("mh" . "help")
                   ("mp" . "package")
                   ("mR" . "reactor")
                   ("mr" . "refactor")
                   ("ms" . "repl")))
        (spacemacs/declare-prefix-for-mode 'elm-mode (car x) (cdr x)))

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
      (spacemacs/declare-prefix-for-mode 'elm-mode "mt" "test")
      (spacemacs/set-leader-keys-for-major-mode 'elm-mode
        "tb" 'elm-test-runner-run
        "td" 'elm-test-runner-run-directory
        "tp" 'elm-test-runner-run-project
        "tr" 'elm-test-runner-rerun
        "tw" 'elm-test-runner-watch
        "t TAB" 'elm-test-runner-toggle-test-and-target
        ))))

(defun elm/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*elm*" :tail t :noselect t) popwin:special-display-config)
    (push '("*elm-make*" :tail t :noselect t) popwin:special-display-config)))

(defun elm/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
      (add-hook 'elm-mode-hook #'smartparens-strict-mode)
    (add-hook 'elm-mode-hook #'smartparens-mode)))
