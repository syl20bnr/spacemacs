;;; packages.el --- Agda2 Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Oliver Charles <ollie@ocharles.org.uk>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq agda-packages
      '((agda :location local)
        company
        golden-ratio))

(defun agda/post-init-company ()
  (spacemacs|add-company-hook agda2-mode)
  (push 'company-capf company-backends-agda2-mode))

(defun agda/init-agda ()
  (if (and (eq 'use-helper agda-mode-path)
           (not (executable-find "agda-mode")))
      (spacemacs-buffer/warning
       (concat "Couldn't find `agda-mode', make sure it is "
               "available in your PATH or check the installation "
               "instructions in the README file."))

    (when (eq 'use-helper agda-mode-path)
      (setq agda-mode-path (let ((coding-system-for-read 'utf-8))
                             (shell-command-to-string "agda-mode locate"))))

    (use-package agda2-mode
      :defer t
      :init (when agda-mode-path (load-file agda-mode-path))
      (progn
        (mapc
         (lambda (x) (add-to-list 'face-remapping-alist x))
         '((agda2-highlight-datatype-face              . font-lock-type-face)
           (agda2-highlight-function-face              . font-lock-type-face)
           (agda2-highlight-inductive-constructor-face . font-lock-function-name-face)
           (agda2-highlight-keyword-face               . font-lock-keyword-face)
           (agda2-highlight-module-face                . font-lock-constant-face)
           (agda2-highlight-number-face                . nil)
           (agda2-highlight-postulate-face             . font-lock-type-face)
           (agda2-highlight-primitive-type-face        . font-lock-type-face)
           (agda2-highlight-record-face                . font-lock-type-face))))
      :config
      (progn
        (spacemacs|define-transient-state goal-navigation
          :title "Goal Navigation Transient State"
          :doc "\n[_f_] next [_b_] previous [_q_] quit"
          :bindings
          ("f" agda2-next-goal)
          ("b" agda2-previous-goal)
          ("q" nil :exit t))
        (spacemacs/set-leader-keys-for-major-mode 'agda2-mode
          "f" 'spacemacs/goal-navigation-transient-state/agda2-next-goal
          "b" 'spacemacs/goal-navigation-transient-state/agda2-previous-goal)

        (spacemacs/set-leader-keys-for-major-mode 'agda2-mode
          "?"   'agda2-show-goals
          "."   'agda2-goal-and-context-and-inferred
          ","   'agda2-goal-and-context
          "="   'agda2-show-constraints
          "SPC" 'agda2-give
          "a"   'agda2-auto
          "c"   'agda2-make-case
          "d"   'agda2-infer-type-maybe-toplevel
          "e"   'agda2-show-context
          "gG"  'agda2-go-back
          "h"   'agda2-helper-function-type
          "l"   'agda2-load
          "n"   'agda2-compute-normalised-maybe-toplevel
          "p"   'agda2-module-contents-maybe-toplevel
          "r"   'agda2-refine
          "s"   'agda2-solveAll
          "t"   'agda2-goal-type
          "w"   'agda2-why-in-scope-maybe-toplevel
          "xc"  'agda2-compile
          "xd"  'agda2-remove-annotations
          "xh"  'agda2-display-implicit-arguments
          "xq"  'agda2-quit
          "xr"  'agda2-restart)))))

(defun idris/pre-init-golden-ratio ()
  (spacemacs|use-package-add-hook golden-ratio
    :post-config
    (add-to-list 'golden-ratio-exclude-buffer-names
                 "*Agda information*")))
