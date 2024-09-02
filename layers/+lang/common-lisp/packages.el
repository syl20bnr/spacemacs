;;; packages.el --- Common Lisp Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defconst common-lisp-packages
  '(
    auto-highlight-symbol
    (common-lisp-snippets :requires yasnippet)
    evil
    evil-cleverparens
    evil-collection
    ggtags
    counsel-gtags
    helm
    rainbow-identifiers
    slime
    (slime-company :requires company)))


(defun common-lisp/post-init-auto-highlight-symbol ()
  (with-eval-after-load 'auto-highlight-symbol
    (add-to-list 'ahs-plugin-bod-modes 'lisp-mode)))

(defun common-lisp/init-common-lisp-snippets ())

(defun common-lisp/post-init-evil ()
  (define-advice slime-last-expression (:around (f &rest args) evil)
    "In normal-state or motion-state, last sexp ends at point."
    (if (and (not evil-move-beyond-eol)
             (or (evil-normal-state-p) (evil-motion-state-p)))
        (save-excursion
          (unless (or (eobp) (eolp)) (forward-char))
          (apply f args))
      (apply f args))))

(defun common-lisp/pre-init-evil-cleverparens ()
  (spacemacs|use-package-add-hook evil-cleverparens
    :pre-init
    (progn
      (add-to-list 'evil-lisp-safe-structural-editing-modes 'common-lisp-mode)
      (add-to-list 'evil-lisp-safe-structural-editing-modes 'lisp-mode))))

(defun common-lisp/pre-init-evil-collection ()
  (when (spacemacs//support-evilified-buffer-p)
    (add-to-list 'spacemacs-evil-collection-allowed-list 'slime)))

(defun common-lisp/post-init-helm ()
  (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
    "sI" 'spacemacs/helm-slime))

(defun common-lisp/post-init-ggtags ()
  (add-hook 'common-lisp-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun common-lisp/post-init-counsel-gtags nil)

(defun common-lisp/post-init-rainbow-identifiers ()
  (add-hook 'lisp-mode-hook #'colors//rainbow-identifiers-ignore-keywords))

(defun common-lisp/pre-init-slime-company ()
  (spacemacs|use-package-add-hook slime
    :pre-config
    (progn
      (setq slime-company-completion 'fuzzy)
      (add-to-list 'slime-contribs 'slime-company))))
(defun common-lisp/init-slime-company ())

(defun common-lisp/init-slime ()
  (use-package slime
    :commands slime-mode
    :init
    (spacemacs/register-repl 'slime 'slime)
    (setq slime-contribs '(slime-asdf
                           slime-fancy
                           slime-indentation
                           slime-sbcl-exts
                           slime-scratch)
          inferior-lisp-program "sbcl")
    ;; enable fuzzy matching in code buffer and SLIME REPL
    (setq slime-complete-symbol*-fancy t)
    (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
    (add-hook 'slime-repl-mode-hook #'spacemacs//deactivate-smartparens)
    (spacemacs/add-to-hooks 'slime-mode '(lisp-mode-hook))
    :config
    (slime-setup)
    (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
      "'" 'spacemacs/slime-repl

      "cc" 'slime-compile-file
      "cC" 'slime-compile-and-load-file
      "cl" 'slime-load-file
      "cf" 'slime-compile-defun
      "cr" 'slime-compile-region
      "cn" 'slime-remove-notes

      "eb" 'slime-eval-buffer
      "ef" 'slime-eval-defun
      "eF" 'slime-undefine-function
      "ee" 'slime-eval-last-expression
      "el" 'spacemacs/slime-eval-sexp-end-of-line
      "er" 'slime-eval-region

      "gb" 'slime-pop-find-definition-stack
      "gn" 'slime-next-note
      "gN" 'slime-previous-note

      "ha" 'slime-apropos
      "hA" 'slime-apropos-all
      "hd" 'slime-disassemble-symbol
      "hh" 'slime-describe-symbol
      "hH" 'slime-hyperspec-lookup
      "hi" 'slime-inspect-definition
      "hp" 'slime-apropos-package
      "ht" 'slime-toggle-trace-fdefinition
      "hT" 'slime-untrace-all
      "h<" 'slime-who-calls
      "h>" 'slime-calls-who
      "hS" 'slime-who-sets
      "hb" 'slime-who-binds
      "hr" 'slime-who-references
      "hm" 'slime-who-macroexpands
      "hs" 'slime-who-specializes

      "ma" 'slime-macroexpand-all
      "mo" 'slime-macroexpand-1

      "se" 'slime-eval-last-expression-in-repl
      "si" 'slime
      "sq" 'slime-quit-lisp

      "tf" 'slime-toggle-fancy-trace

      ;; Add key bindings for custom eval functions
      "ec" 'spacemacs/cl-eval-current-form-sp
      "eC" 'spacemacs/cl-eval-current-form
      "es" 'spacemacs/cl-eval-current-symbol-sp)

    ;; prefix names for which-key
    (mapc (lambda (x)
            (spacemacs/declare-prefix-for-mode 'lisp-mode (car x) (cdr x)))
          '(("mh" . "help")
            ("me" . "eval")
            ("ms" . "repl")
            ("mc" . "compile")
            ("mg" . "nav")
            ("mm" . "macro")
            ("mt" . "toggle")))))
