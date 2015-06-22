;;; packages.el --- slime Layer packages File for Spacemacs
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

(setq slime-packages
  '(slime))

(defun slime/init-slime ()
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
      (add-to-hooks 'slime-mode '(lisp-mode-hook scheme-mode-hook)))
    :config
    (progn
      (slime-setup)
      (dolist (m `(,slime-mode-map ,slime-repl-mode-map))
        (define-key m [(tab)] 'slime-fuzzy-complete-symbol))
      (dolist (m '(lisp-mode
                   scheme-mode))
        (evil-leader/set-key-for-mode m
          "mcc" 'slime-compile-file
          "mcC" 'slime-compile-and-load-file
          "mcf" 'slime-compile-defun
          "mcr" 'slime-compile-region

          "meb" 'slime-eval-buffer
          "mef" 'slime-eval-defun
          "mee" 'slime-eval-last-sexp
          "mer" 'slime-eval-region

          "mgg" 'slime-inspect-definition
          "mgn" 'slime-next-note
          "mgN" 'slime-previous-note
          "mgp" 'slime-previous-note

          "mha" 'slime-apropos
          "mhd" 'slime-disassemble-symbol
          "mhh" 'slime-describe-function
          "mhH" 'slime-hyperspec-lookup

          "mse" 'slime-eval-last-expression-in-repl
          "msi" 'slime
          "msq" 'slime-quit-lisp

          "mtf" 'slime-toggle-fancy-trace)))))
