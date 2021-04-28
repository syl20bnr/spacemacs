;;; packages.el --- Scheme Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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


(defconst scheme-packages
  '(company
    evil-cleverparens
    geiser
    ggtags
    counsel-gtags
    helm-gtags
    (geiser-chez    :toggle (memq 'chez    scheme-implementations))
    (geiser-chibi   :toggle (memq 'chibi   scheme-implementations))
    (geiser-chicken :toggle (memq 'chicken scheme-implementations))
    (geiser-gambit  :toggle (memq 'gambit  scheme-implementations))
    (geiser-gauche  :toggle (memq 'gauche  scheme-implementations))
    (geiser-guile   :toggle (memq 'guile   scheme-implementations))
    (geiser-kawa    :toggle (memq 'kawa    scheme-implementations))
    (geiser-mit     :toggle (memq 'mit     scheme-implementations))
    (geiser-racket  :toggle (memq 'racket  scheme-implementations))))


(defun scheme/post-init-company ()
  ;; Geiser provides completion as long as company mode is loaded.
  (spacemacs|add-company-backends :modes scheme-mode :backends geiser-company-backend))

(defun scheme/pre-init-evil-cleverparens ()
  (spacemacs|use-package-add-hook evil-cleverparens
    :pre-init
    (add-to-list 'evil-lisp-safe-structural-editing-modes 'scheme-mode)))

(defun scheme/init-geiser ()
  (use-package geiser
    :commands run-geiser
    :init (spacemacs/register-repl 'geiser 'run-geiser "geiser")
    :config
    (progn
      ;; prefixes
      (spacemacs/declare-prefix-for-mode 'scheme-mode "mc" "compiling")
      (spacemacs/declare-prefix-for-mode 'scheme-mode "mg" "navigation")
      (spacemacs/declare-prefix-for-mode 'scheme-mode "mh" "documentation")
      (spacemacs/declare-prefix-for-mode 'scheme-mode "mi" "insertion")
      (spacemacs/declare-prefix-for-mode 'scheme-mode "mm" "macroexpansion")
      (spacemacs/declare-prefix-for-mode 'scheme-mode "ms" "repl")
      ;; key bindings
      (spacemacs/set-leader-keys-for-major-mode 'scheme-mode
        "'"  'geiser-mode-switch-to-repl
        ","  'lisp-state-toggle-lisp-state

        "cc" 'geiser-compile-current-buffer
        "cp" 'geiser-add-to-load-path

        "eb" 'geiser-eval-buffer
        "ee" 'geiser-eval-last-sexp
        "ef" 'geiser-eval-definition
        "el" 'lisp-state-eval-sexp-end-of-line
        "er" 'geiser-eval-region

        "gb" 'geiser-pop-symbol-stack
        "gm" 'geiser-edit-module
        "gn" 'next-error
        "gN" 'previous-error

        "hh" 'geiser-doc-symbol-at-point
        "hd" 'geiser-doc-look-up-manual
        "hm" 'geiser-doc-module
        "h<" 'geiser-xref-callers
        "h>" 'geiser-xref-callees

        "il" 'geiser-insert-lambda

        "me" 'geiser-expand-last-sexp
        "mf" 'geiser-expand-definition
        "mx" 'geiser-expand-region

        "si" 'geiser-mode-switch-to-repl
        "sb" 'geiser-eval-buffer
        "sB" 'geiser-eval-buffer-and-go
        "sf" 'geiser-eval-definition
        "sF" 'geiser-eval-definition-and-go
        "se" 'geiser-eval-last-sexp
        "sr" 'geiser-eval-region
        "sR" 'geiser-eval-region-and-go
        "ss" 'geiser-set-scheme)

      (evil-define-key 'normal 'geiser-repl-mode-map
      "gj" 'geiser-repl-next-prompt
      "gk" 'geiser-repl-previous-prompt)

      (spacemacs/set-leader-keys-for-major-mode 'geiser-repl-mode
        "q" 'geiser-repl-exit
        "c" 'geiser-repl-clear-buffer
        "i" 'geiser-repl-interrupt
        "m" 'geiser-repl-import-module
        "u" 'geiser-repl-unload-function
        "h" 'geiser-doc-symbol-at-point))))

(defun scheme/post-init-ggtags ()
  (add-hook 'scheme-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun scheme/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'scheme-mode))

(defun scheme/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'scheme-mode))

(defun scheme/init-geiser-chez ()
  (use-package geiser-chez
    :defer t))

(defun scheme/init-geiser-chibi ()
  (use-package geiser-chibi
    :defer t))

(defun scheme/init-geiser-chicken ()
  (use-package geiser-chicken
    :defer t))

(defun scheme/init-geiser-gambit ()
  (use-package geiser-gambit
    :defer t))

(defun scheme/init-geiser-gauche ()
  (use-package geiser-gauche
    :defer t))

(defun scheme/init-geiser-guile ()
  (use-package geiser-guile
    :defer t))

(defun scheme/init-geiser-kawa ()
  (use-package geiser-kawa
    :defer t))

(defun scheme/init-geiser-mit ()
  (use-package geiser-mit
    :defer t))

(defun scheme/init-geiser-racket ()
  (use-package geiser-racket
    :defer t))
