;;; packages.el --- Emacs Lisp Layer packages File for Spacemacs
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

(setq emacs-lisp-packages
      '(
        company
        eldoc
        elisp-slime-nav
        (emacs-lisp :location built-in)
        evil
        flycheck
        ielm
        macrostep
        semantic
        smartparens
        srefactor
        ))

(use-package ielm
  :config
  (progn
    (defun ielm-indent-line ()
      (interactive)
      (let ((current-point (point)))
        (save-restriction
          (narrow-to-region (search-backward-regexp "^ELISP>") (goto-char current-point))
          (lisp-indent-line))))
    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (spacemacs/declare-prefix-for-mode mode "ms" "ielm")
      (evil-leader/set-key-for-mode mode
        "msi" 'ielm))))

(defun emacs-lisp/post-init-company ()
  (spacemacs|add-company-hook ielm-mode)
  (push '(company-files company-capf) company-backends-ielm-mode))

(defun emacs-lisp/post-init-eldoc ()
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(defun emacs-lisp/init-elisp-slime-nav ()
  ;; Elisp go-to-definition with M-. and back again with M-,
  (use-package elisp-slime-nav
    :defer t
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
      (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
        (spacemacs/declare-prefix-for-mode mode "mg" "find-symbol")
        (spacemacs/declare-prefix-for-mode mode "mh" "help")
        (evil-leader/set-key-for-mode mode
          "mgg" 'elisp-slime-nav-find-elisp-thing-at-point
          "mhh" 'elisp-slime-nav-describe-elisp-thing-at-point)))))

(defun emacs-lisp/init-emacs-lisp ()
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
  ;; company support
  (push 'company-capf company-backends-emacs-lisp-mode)
  (spacemacs|add-company-hook emacs-lisp-mode))

(defun emacs-lisp/init-macrostep ()
  (use-package macrostep
    :defer t
    :mode ("\\*.el\\'" . emacs-lisp-mode)
    :init
    (progn
      (spacemacs|define-micro-state macrostep
        :doc "[e] expand [c] collapse [n/N] next/previous [q] quit"
        :disable-evil-leader t
        :persistent t
        :evil-leader-for-mode (emacs-lisp-mode . "mdm")
        :bindings
        ("e" macrostep-expand)
        ("c" macrostep-collapse)
        ("n" macrostep-next-macro)
        ("N" macrostep-prev-macro)
        ("q" macrostep-collapse-all :exit t)))))

(defun emacs-lisp/post-init-evil ()
  (spacemacs/add-to-hook 'emacs-lisp-mode
                         '(lambda ()
                            (spacemacs|define-text-object ";"
                                                          "elisp-comment"
                                                          ";; "
                                                          ""))))

(defun emacs-lisp/post-init-flycheck ()
  ;; Make flycheck recognize packages in loadpath
  ;; i.e (require 'company) will not give an error now
  (setq flycheck-emacs-lisp-load-path 'inherit))

(defun emacs-lisp/post-init-semantic ()
  (semantic/enable-semantic-mode 'emacs-lisp-mode)
  (eval-after-load 'semantic
    '(semantic-default-elisp-setup)))

(defun emacs-lisp/post-init-srefactor ()
  (add-hook 'emacs-lisp-mode-hook 'spacemacs/lazy-load-srefactor)
  (use-package srefactor-lisp
    :commands (srefactor-lisp-format-buffer
               srefactor-lisp-format-defun
               srefactor-lisp-format-sexp
               srefactor-lisp-one-line)
    :init
    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (spacemacs/declare-prefix-for-mode mode "m=" "srefactor")
      (evil-leader/set-key-for-mode mode
        "m=b" 'srefactor-lisp-format-buffer
        "m=d" 'srefactor-lisp-format-defun
        "m=o" 'srefactor-lisp-one-line
        "m=s" 'srefactor-lisp-format-sexp))))

(defun emacs-lisp/post-init-smartparens ()
  (if (version< emacs-version "24.4")
      (ad-disable-advice 'preceding-sexp 'around 'evil)
    (advice-remove 'elisp--preceding-sexp 'evil--preceding-sexp))

  (defun spacemacs/eval-current-form-sp (&optional arg)
    "Call `eval-last-sexp' after moving out of one level of
parentheses. Will exit any strings and/or comments first.
Requires smartparens because all movement is done using
`sp-up-sexp'. An optional ARG can be used which is passed to
`sp-up-sexp' to move out of more than one sexp."
    (interactive "p")
    (require 'smartparens)
    (save-excursion
      (let ((max 10))
        (while (and (> max 0)
                    (sp-point-in-string-or-comment))
          (decf max)
          (sp-up-sexp)))
      (sp-up-sexp arg)
      (call-interactively 'eval-last-sexp)))

  (defun spacemacs/eval-current-symbol-sp ()
    "Call `eval-last-sexp' on the symbol underneath the
point. Requires smartparens because all movement is done using
`sp-forward-symbol'."
    (interactive)
    (require 'smartparens)
    (save-excursion
      (sp-forward-symbol)
      (call-interactively 'eval-last-sexp)))

  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (evil-leader/set-key-for-mode mode
      "mec" 'spacemacs/eval-current-form-sp
      "mes" 'spacemacs/eval-current-symbol-sp)))
