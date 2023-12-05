;;; packages.el --- Idris Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Timothy Jones <tim@zmthy.net>
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


(setq idris-packages
      '(
        company
        idris-mode
        golden-ratio
        popwin
        ))

(defun idris/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes idris-mode idris-repl-mode))

(defun idris/init-idris-mode ()
  (use-package idris-mode
    :defer t
    :init (spacemacs/register-repl 'idris-mode 'idris-repl "idris")
    :config
    (defun spacemacs/idris-load-file-and-focus (&optional set-line)
      "Pass the current buffer's file to the REPL and switch to it in
`insert state'."
      (interactive "p")
      (idris-load-file set-line)
      (idris-pop-to-repl)
      (evil-insert-state))

    (defun spacemacs/idris-load-forward-line-and-focus ()
      "Pass the next line to REPL and switch to it in `insert state'."
      (interactive)
      (idris-load-forward-line)
      (idris-pop-to-repl)
      (evil-insert-state))

    (defun spacemacs/idris-load-backward-line-and-focus ()
      "Pass the previous line to REPL and switch to it in `insert state'."
      (interactive)
      (idris-load-backward-line)
      (idris-pop-to-repl)
      (evil-insert-state))

    ;; prefix
    (spacemacs/declare-prefix-for-mode 'idris-mode "mb" "idris/build")
    (spacemacs/declare-prefix-for-mode 'idris-mode "mi" "idris/editing")
    (spacemacs/declare-prefix-for-mode 'idris-mode "mh" "idris/documentation")
    (spacemacs/declare-prefix-for-mode 'idris-mode "ms" "idris/repl")
    (spacemacs/declare-prefix-for-mode 'idris-mode "mm" "idris/term")

    (spacemacs/set-leader-keys-for-major-mode 'idris-mode
      ;; Shorthands: rebind the standard evil-mode combinations to the local
      ;; leader for the keys not used as a prefix below.
      "c" 'idris-case-dwim
      "d" 'idris-add-clause
      "l" 'idris-make-lemma
      "p" 'idris-proof-search
      "r" 'idris-load-file
      "t" 'idris-type-at-point
      "w" 'idris-make-with-block

      ;; ipkg.
      "bc" 'idris-ipkg-build
      "bC" 'idris-ipkg-clean
      "bi" 'idris-ipkg-install
      "bp" 'idris-open-package-file

      ;; Interactive editing.
      "ia" 'idris-proof-search
      "ic" 'idris-case-dwim
      "ie" 'idris-make-lemma
      "im" 'idris-add-missing
      "ir" 'idris-refine
      "is" 'idris-add-clause
      "iw" 'idris-make-with-block

      ;; Documentation.
      "ha" 'idris-apropos
      "hd" 'idris-docs-at-point
      "hs" 'idris-type-search
      "ht" 'idris-type-at-point

      ;; Active term manipulations.
      "mn" 'idris-normalise-term
      "mi" 'idris-show-term-implicits
      "mh" 'idris-hide-term-implicits
      "mc" 'idris-show-core-term

      ;; REPL
      "'"  'idris-repl
      "sb" 'idris-load-file
      "sB" 'spacemacs/idris-load-file-and-focus
      "si" 'idris-repl
      "sn" 'idris-load-forward-line
      "sN" 'spacemacs/idris-load-forward-line-and-focus
      "sp" 'idris-load-backward-line
      "sP" 'spacemacs/idris-load-backward-line-and-focus
      "ss" 'idris-pop-to-repl
      "sq" 'idris-quit))

  ;; To suppress auto-indentation
  (add-to-list 'spacemacs-indent-sensitive-modes 'idris-mode)

  ;; To bind TAB to the indentation command for all Idris buffers
  (add-hook 'idris-mode-hook 'turn-on-idris-simple-indent)

  ;; open special buffers in motion state so they can be closed with ~q~
  (evil-set-initial-state 'idris-compiler-notes-mode 'motion)
  (evil-set-initial-state 'idris-hole-list-mode 'motion)
  (evil-set-initial-state 'idris-info-mode 'motion))

(defun idris/pre-init-golden-ratio ()
  (spacemacs|use-package-add-hook golden-ratio
    :post-config
    (dolist (x '("*idris-notes*" "*idris-holes*" "*idris-info*"))
      (add-to-list 'golden-ratio-exclude-buffer-names x))))

(defun idris/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*idris-notes*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          popwin:special-display-config)
    (push '("*idris-holes*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          popwin:special-display-config)
    (push '("*idris-info*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          popwin:special-display-config)))
