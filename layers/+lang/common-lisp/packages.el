;;; packages.el --- Common Lisp Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq common-lisp-packages
      '(slime))

(defun common-lisp/init-slime ()
  (use-package slime
    :commands slime-mode
    :init
    (progn
      (setq slime-contribs '(slime-fancy
                             slime-indentation
                             slime-sbcl-exts
                             slime-scratch)
            inferior-lisp-program "sbcl")
      ;; enable fuzzy matching in code buffer and SLIME REPL
      (setq slime-complete-symbol*-fancy t)
      (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
      ;; enabel smartparen in code buffer and SLIME REPL
      ;; (add-hook 'slime-repl-mode-hook #'smartparens-strict-mode)
      (defun slime/disable-smartparens ()
        (smartparens-strict-mode -1)
        (turn-off-smartparens-mode))
      (add-hook 'slime-repl-mode-hook #'slime/disable-smartparens)
      (spacemacs/add-to-hooks 'slime-mode '(lisp-mode-hook)))
    :config
    (progn
      (slime-setup)
      (dolist (m `(,slime-mode-map ,slime-repl-mode-map))
        (define-key m [(tab)] 'slime-fuzzy-complete-symbol))
      ;; TODO: Add bindings for the SLIME debugger?
      (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
        "cc" 'slime-compile-file
        "cC" 'slime-compile-and-load-file
        "cl" 'slime-load-file
        "cf" 'slime-compile-defun
        "cr" 'slime-compile-region
        "cn" 'slime-remove-notes

        "eb" 'slime-eval-buffer
        "ef" 'slime-eval-defun
        "eF" 'slime-undefine-function
        "ee" 'slime-eval-last-sexp
        "er" 'slime-eval-region

        "gg" 'slime-inspect-definition
        "gb" 'slime-pop-find-definition-stack
        "gn" 'slime-next-note
        "gN" 'slime-previous-note

        "ha" 'slime-apropos
        "hA" 'slime-apropos-all
        "hd" 'slime-disassemble-symbol
        "hh" 'slime-describe-symbol
        "hH" 'slime-hyperspec-lookup
        "hp" 'slime-apropos-package
        "ht" 'slime-toggle-trace-fdefinition
        "hT" 'slime-untrace-all
        "h<" 'slime-who-calls
        "h>" 'slime-calls-who
        ;; TODO: Add key bindings for who binds/sets globals?
        "hr" 'slime-who-references
        "hm" 'slime-who-macroexpands
        "hs" 'slime-who-specializes

        "ma" 'slime-macroexpand-all
        "mo" 'slime-macroexpand-1

        "se" 'slime-eval-last-expression-in-repl
        "si" 'slime
        "sq" 'slime-quit-lisp

        "tf" 'slime-toggle-fancy-trace))))
