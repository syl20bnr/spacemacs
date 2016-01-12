;;; packages.el --- Emacs Lisp Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq emacs-lisp-packages
      '(
        auto-compile
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
      (spacemacs/set-leader-keys-for-major-mode mode
        "si" 'ielm))))

(defun emacs-lisp/post-init-company ()
  (spacemacs|add-company-hook ielm-mode)
  (push '(company-files company-capf) company-backends-ielm-mode))

(defun emacs-lisp/post-init-eldoc ()
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(defun emacs-lisp/init-auto-compile ()
  (use-package auto-compile
    :defer t
    :diminish (auto-compile-mode . "")
    :init
    (progn
      (setq auto-compile-display-buffer nil
            ;; lets spaceline manage the mode-line
            auto-compile-use-mode-line nil
            auto-compile-mode-line-counter t)
      (add-hook 'emacs-lisp-mode-hook 'auto-compile-mode))
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
        "cl" 'auto-compile-display-log))))

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
        (spacemacs/set-leader-keys-for-major-mode mode
          "gg" 'elisp-slime-nav-find-elisp-thing-at-point
          "hh" 'elisp-slime-nav-describe-elisp-thing-at-point)))))

(defun emacs-lisp/init-emacs-lisp ()
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (spacemacs/declare-prefix-for-mode mode "mc" "compile")
    (spacemacs/declare-prefix-for-mode mode "me" "eval")
    (spacemacs/declare-prefix-for-mode mode "mt" "tests")
    (spacemacs/set-leader-keys-for-major-mode mode
      "cc" 'emacs-lisp-byte-compile
      "e$" 'lisp-state-eval-sexp-end-of-line
      "eb" 'eval-buffer
      "ee" 'eval-last-sexp
      "er" 'eval-region
      "ef" 'eval-defun
      "el" 'lisp-state-eval-sexp-end-of-line
      ","  'lisp-state-toggle-lisp-state
      "tb" 'spacemacs/ert-run-tests-buffer
      "tq" 'ert))
  ;; company support
  (push 'company-capf company-backends-emacs-lisp-mode)
  (spacemacs|add-company-hook emacs-lisp-mode))

(defun emacs-lisp/init-macrostep ()
  (use-package macrostep
    :defer t
    :mode ("\\*.el\\'" . emacs-lisp-mode)
    :init
    (progn
      (evil-define-key 'normal macrostep-keymap "q" 'macrostep-collapse-all)
      (spacemacs|define-micro-state macrostep
        :doc "[e] expand [c] collapse [n/N] next/previous [q] quit"
        :disable-evil-leader t
        :persistent t
        :evil-leader-for-mode (emacs-lisp-mode . "dm")
        :bindings
        ("e" macrostep-expand)
        ("c" macrostep-collapse)
        ("n" macrostep-next-macro)
        ("N" macrostep-prev-macro)
        ("q" macrostep-collapse-all :exit t)))))

(defun emacs-lisp/post-init-evil ()
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (spacemacs|define-text-object ";" "elisp-comment" ";; " ""))))

(defun emacs-lisp/post-init-flycheck ()
  ;; Don't activate flycheck by default in elisp
  ;; because of too much false warnings
  ;; (spacemacs/add-flycheck-hook 'emacs-lisp-mode-hook)

  ;; Make flycheck recognize packages in loadpath
  ;; i.e (require 'company) will not give an error now
  (setq flycheck-emacs-lisp-load-path 'inherit))

(defun emacs-lisp/post-init-semantic ()
  (semantic/enable-semantic-mode 'emacs-lisp-mode)
  (with-eval-after-load 'semantic
    (semantic-default-elisp-setup)))

(defun emacs-lisp/post-init-srefactor ()
  (add-hook 'emacs-lisp-mode-hook 'spacemacs/lazy-load-srefactor)
  (use-package srefactor-lisp
    :commands (srefactor-lisp-format-buffer
               srefactor-lisp-format-defun
               srefactor-lisp-format-sexp
               srefactor-lisp-one-line)
    :init
    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (spacemacs/declare-prefix-for-mode mode "=" "srefactor")
      (spacemacs/set-leader-keys-for-major-mode mode
        "=b" 'srefactor-lisp-format-buffer
        "=d" 'srefactor-lisp-format-defun
        "=o" 'srefactor-lisp-one-line
        "=s" 'srefactor-lisp-format-sexp))))

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
    (spacemacs/set-leader-keys-for-major-mode mode
      "ec" 'spacemacs/eval-current-form-sp
      "es" 'spacemacs/eval-current-symbol-sp)))
