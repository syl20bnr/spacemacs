;;; packages.el --- Idris Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2015 Timothy Jones
;;
;; Author: Timothy Jones <git@zmthy.io>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq idris-packages '(idris-mode))

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

      (evil-leader/set-key-for-mode 'idris-mode
        ;; Shorthands: rebind the standard evil-mode combinations to the local
        ;; leader for the keys not used as a prefix below.
        "mc" 'idris-case-split
        "md" 'idris-add-clause
        "mp" 'idris-proof-search
        "mr" 'idris-load-file
        "mt" 'idris-type-at-point
        "mw" 'idris-make-with-block

        ;; ipkg.
        "mbc" 'idris-ipkg-build
        "mbC" 'idris-ipkg-clean
        "mbi" 'idris-ipkg-install
        "mbp" 'idris-open-package-file

        ;; Interactive editing.
        "mia" 'idris-proof-search
        "mic" 'idris-case-split
        "mie" 'idris-make-lemma
        "mim" 'idris-add-missing
        "mir" 'idris-refine
        "mis" 'idris-add-clause
        "miw" 'idris-make-with-block

        ;; Documentation.
        "mha" 'idris-apropos
        "mhd" 'idris-docs-at-point
        "mhs" 'idris-type-search
        "mht" 'idris-type-at-point

        ;; Active term manipulations.
        "mmn" 'idris-normalise-term
        "mmi" 'idris-show-term-implicits
        "mmh" 'idris-hide-term-implicits
        "mmc" 'idris-show-core-term

        ;; REPL
        "msb" 'idris-load-file
        "msB" 'spacemacs/idris-load-file-and-focus
        "msi" 'idris-ensure-process-and-repl-buffer
        "msn" 'idris-load-forward-line
        "msN" 'spacemacs/idris-load-forward-line-and-focus
        "msp" 'idris-load-backward-line
        "msP" 'spacemacs/idris-load-backward-line-and-focus
        "mss" 'idris-pop-to-repl))))
