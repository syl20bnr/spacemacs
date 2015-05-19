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
    :init
    (progn
      ;; Ensure that the idris-define-evil-keys function has been defined, but
      ;; not used, so that it can be overridden below.
      (require 'idris-keys)

      ;; Replace Idris' existing evil bindings with Spacemacs-style bindings.
      (defun idris-define-evil-keys ()
        "Define Spacemacs-style keys for evil-mode."
        (evil-leader/set-key-for-mode 'idris-mode
          ;; Shorthands: rebind the standard evil-mode combinations to the local
          ;; leader for the keys not used as a prefix below.
          "mc" 'idris-case-split
          "md" 'idris-add-clause
          "mp" 'idris-proof-search
          "mr" 'idris-load-file
          "mt" 'idris-type-at-point
          "mw" 'idris-make-with-block

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

          ;; File loading.
          "mlb" 'idris-load-file
          "mln" 'idris-load-forward-line
          "mlp" 'idris-load-backward-line
          "mlN" 'idris-load-backward-line

          ;; Active term manipulations.
          "mmn" 'idris-normalise-term
          "mmi" 'idris-show-term-implicits
          "mmh" 'idris-hide-term-implicits
          "mmc" 'idris-show-core-term

          ;; ipkg.
          "mbb" 'idris-ipkg-build
          "mbc" 'idris-ipkg-clean
          "mbi" 'idris-ipkg-install
          "mbp" 'idris-open-package-file

          ;; Inferior mode switch.
          "ms" 'idris-pop-to-repl)

        (evil-leader/set-key-for-mode 'idris-ipkg-mode
          "mf" 'idris-ipkg-insert-field)))

    :config
    ;; Idris also uses the idris-packages variable, so we clear it out in Idris
    ;; buffers with a hook.
    (add-hook 'idris-mode-hook (lambda () (setq idris-packages nil)))))
