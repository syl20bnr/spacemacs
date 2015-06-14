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
        eldoc
        elisp-slime-nav
        evil
        ielm
        macrostep
        semantic
        smartparens
        srefactor
        ))

(use-package ielm
  :config
  (defun ielm-indent-line ()
    (interactive)
    (let ((current-point (point)))
      (save-restriction
        (narrow-to-region (search-backward-regexp "^ELISP>") (goto-char current-point))
        (lisp-indent-line)))))

(defun emacs-lisp/post-init-eldoc ()
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(defun emacs-lisp/init-elisp-slime-nav ()
  ;; Elisp go-to-definition with M-. and back again with M-,
  (use-package elisp-slime-nav
    :defer t
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
      (evil-leader/set-key-for-mode 'emacs-lisp-mode
        "mgg" 'elisp-slime-nav-find-elisp-thing-at-point
        "mhh" 'elisp-slime-nav-describe-elisp-thing-at-point))))

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
  (add-to-hook 'emacs-lisp-mode
               '(lambda ()
                  (spacemacs|define-text-object ";"
                                                "elisp-comment"
                                                ";; "
                                                ""))))

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
    (evil-leader/set-key-for-mode 'emacs-lisp-mode
      "m=b" 'srefactor-lisp-format-buffer
      "m=d" 'srefactor-lisp-format-defun
      "m=o" 'srefactor-lisp-one-line
      "m=s" 'srefactor-lisp-format-sexp
      )))
