;;; packages.el --- Common Lisp Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
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
      (evil-leader/set-key-for-mode 'lisp-mode
        "mcc" 'slime-compile-file
        "mcC" 'slime-compile-and-load-file
        "mcl" 'slime-load-file
        "mcf" 'slime-compile-defun
        "mcr" 'slime-compile-region
        "mcn" 'slime-remove-notes

        "meb" 'slime-eval-buffer
        "mef" 'slime-eval-defun
        "meF" 'slime-undefine-function
        "mee" 'slime-eval-last-sexp
        "mer" 'slime-eval-region

        "mgg" 'slime-inspect-definition
        "mgb" 'slime-pop-find-definition-stack
        "mgn" 'slime-next-note
        "mgN" 'slime-previous-note

        "mha" 'slime-apropos
        "mhA" 'slime-apropos-all
        "mhd" 'slime-disassemble-symbol
        "mhh" 'slime-describe-symbol
        "mhH" 'slime-hyperspec-lookup
        "mhp" 'slime-apropos-package
        "mht" 'slime-toggle-trace-fdefinition
        "mhT" 'slime-untrace-all
        "mh<" 'slime-who-calls
        "mh>" 'slime-calls-who
        ;; TODO: Add key bindings for who binds/sets globals?
        "mhr" 'slime-who-references
        "mhm" 'slime-who-macroexpands
        "mhs" 'slime-who-specializes

        "mma" 'slime-macroexpand-all
        "mmo" 'slime-macroexpand-1

        "mse" 'slime-eval-last-expression-in-repl
        "msi" 'slime
        "msq" 'slime-quit-lisp

        "mtf" 'slime-toggle-fancy-trace))))
