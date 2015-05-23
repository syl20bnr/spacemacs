;;; extensions.el --- Agda2 Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2015 Oliver Charles & Contributors
;;
;; Author: Oliver Charles <ollie@ocharles.org.uk>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar agda-post-extensions '(agda))

(defun agda/init-agda ()
  (unless (executable-find "agda-mode")
    (spacemacs-buffer/warning
     (concat "Agda not detected, be sure that Agda binaries are "
             "available in your PATH or check the installation "
             "instructions in the README file.")))

  (use-package agda2-mode
    :if (executable-find "agda-mode")
    :defer t
    :init (load-file (let ((coding-system-for-read 'utf-8))
                       (shell-command-to-string "agda-mode locate")))
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
       (agda2-highlight-record-face                . font-lock-type-face)))
    :config
    (progn
      (spacemacs|define-micro-state goal-navigation
        :doc "[f] next [b] previous [q]uit"
        :execute-binding-on-enter t
        :evil-leader-for-mode
        (agda2-mode . "mf")
        (agda2-mode . "mb")
        :bindings
        ("f" agda2-next-goal)
        ("b" agda2-previous-goal)
        ("q" nil :exit t))

      (evil-leader/set-key-for-mode 'agda2-mode
        "m?" 'agda2-show-goals
        "m." 'agda2-goal-and-context-and-inferred
        "m," 'agda2-goal-and-context
        "m=" 'agda2-show-constraints
        "m <SPC>" 'agda2-give
        "ma" 'agda2-auto
        "mc" 'agda2-make-case
        "md" 'agda2-infer-type-maybe-toplevel
        "me" 'agda2-show-context
        "mgg" 'agda2-goto-definition-keyboard
        "mgG" 'agda2-go-back
        "mh" 'agda2-helper-function-type
        "ml" 'agda2-load
        "mn" 'agda2-compute-normalised-maybe-toplevel
        "mo" 'agda2-module-contents-maybe-toplevel
        "mr" 'agda2-refine
        "ms" 'agda2-solveAll
        "mt" 'agda2-goal-type
        "mw" 'agda2-why-in-scope-maybe-toplevel
        "mxc" 'agda2-compile
        "mxd" 'agda2-remove-annotations
        "mxh" 'agda2-display-implicit-arguments
        "mxq" 'agda2-quit
        "mxr" 'agda2-restart)

      (eval-after-load 'golden-ratio
        '(add-to-list 'golden-ratio-exclude-buffer-names
                      "*Agda information*")))))
