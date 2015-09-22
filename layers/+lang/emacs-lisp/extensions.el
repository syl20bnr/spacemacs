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

  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (spacemacs/declare-prefix-for-mode mode "me" "eval")
    (spacemacs/declare-prefix-for-mode mode "mt" "tests")
    (evil-leader/set-key-for-mode mode
      "me$" 'lisp-state-eval-sexp-end-of-line
      "meb" 'eval-buffer
      "mee" 'eval-last-sexp
      "mer" 'eval-region
      "mef" 'eval-defun
      "mel" 'lisp-state-eval-sexp-end-of-line
      "m,"  'lisp-state-toggle-lisp-state
      "mtb" 'spacemacs/ert-run-tests-buffer
      "mtq" 'ert))

  (unless (configuration-layer/package-usedp 'smartparens)
    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (evil-leader/set-key-for-mode mode
        "mec" 'spacemacs/eval-current-form)))

  ;; company support
  (push 'company-capf company-backends-emacs-lisp-mode)
  (spacemacs|add-company-hook emacs-lisp-mode))

