;;; packages.el --- Idris Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Timothy Jones <git@zmthy.io>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq idris-packages '(idris-mode
                       popwin))

(defun idris/init-idris-mode ()
  (use-package idris-mode
    :defer t
    :config
    (progn
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

      (spacemacs/set-leader-keys-for-major-mode 'idris-mode
        ;; Shorthands: rebind the standard evil-mode combinations to the local
        ;; leader for the keys not used as a prefix below.
        "c" 'idris-case-split
        "d" 'idris-add-clause
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
        "ic" 'idris-case-split
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
        "sb" 'idris-load-file
        "sB" 'spacemacs/idris-load-file-and-focus
        "si" 'idris-ensure-process-and-repl-buffer
        "sn" 'idris-load-forward-line
        "sN" 'spacemacs/idris-load-forward-line-and-focus
        "sp" 'idris-load-backward-line
        "sP" 'spacemacs/idris-load-backward-line-and-focus
        "ss" 'idris-pop-to-repl)))

  ;; open special buffers in motion state so they can be closed with ~q~
  (evil-set-initial-state 'idris-compiler-notes-mode 'motion)
  (evil-set-initial-state 'idris-hole-list-mode 'motion)
  (evil-set-initial-state 'idris-info-mode 'motion))

(defun idris/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*idris-notes*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          popwin:special-display-config)
    (push '("*idris-holes*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          popwin:special-display-config)
    (push '("*idris-info*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          popwin:special-display-config)))
