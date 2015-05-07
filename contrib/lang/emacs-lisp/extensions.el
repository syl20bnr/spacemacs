;;; extensions.el --- Emacs Lisp Layer Extensions File for Spacemacs
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

(setq emacs-lisp-post-extensions
      '(emacs-builtin-emacs-lisp))

(defun emacs-lisp/init-emacs-builtin-emacs-lisp ()

  (evil-leader/set-key-for-mode 'emacs-lisp-mode
    "me$" 'lisp-state-eval-sexp-end-of-line
    "meb" 'eval-buffer
    "mec" 'spacemacs/eval-current-form
    "mee" 'eval-last-sexp
    "mer" 'spacemacs/eval-region
    "mef" 'eval-defun
    "mel" 'lisp-state-eval-sexp-end-of-line
    "m,"  'lisp-state-toggle-lisp-state
    "mtb" 'spacemacs/ert-run-tests-buffer
    "mtq" 'ert)

  ;; company support
  (push 'company-capf company-backends-emacs-lisp-mode)
  (spacemacs|add-company-hook emacs-lisp-mode))

