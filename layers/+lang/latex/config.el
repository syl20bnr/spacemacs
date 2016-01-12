;;; packages.el --- Latex Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

;; Company-mode LaTeX-backend
(spacemacs|defvar-company-backends LaTeX-mode)

(defvar latex-build-command (if (executable-find "latexmk") "LatexMk" "LaTeX")
  "The default command to use with `SPC m b'")

(defvar latex-enable-auto-fill t
  "Whether to use auto-fill-mode or not in tex files.")

(defvar latex-enable-folding nil
  "Whether to use `TeX-fold-mode' or not in tex/latex buffers.")

(defvar latex-nofill-env '("equation"
                           "equation*"
                           "align"
                           "align*"
                           "tabular"
                           "tikzpicture")
  "List of environment names in which `auto-fill-mode' will be inhibited.")

;; Command prefixes
;; no supported
;; (setq auctex/key-binding-prefixes '())
;; (push (cons "mp" "LaTeX Preview") auctex/key-binding-prefixes)
;; (push (cons "mr" "RefTeX") auctex/key-binding-prefixes)
;; (mapc (lambda (x) (spacemacs/declare-prefix (car x) (cdr x)))
;;       auctex/key-binding-prefixes)

