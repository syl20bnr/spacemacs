;;; packages.el --- faust layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author:  Bart Brouns <bart@magnetophon.nl>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst faust-packages
  '(company
    faustine
    yasnippet))

(defun faust/post-init-company ()
  (spacemacs|add-company-backends :modes faustine-mode))

(defun faust/init-faustine ()
  (use-package faustine-mode
    :defer t
    :mode "\\.\\(dsp\\|lib\\)\\'"
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'faustine-mode
        "b" 'faustine-build
        "B" 'faustine-build-all
        "c" 'faustine-syntax-check
        "d" 'faustine-diagram
        "D" 'faustine-diagram-all
        "h" 'faustine-online-doc
        "i" 'prog-indent-sexp
        "m" 'faustine-mdoc
        "o" 'faustine-toggle-output-buffer
        "r" 'faustine-run
        "s" 'faustine-source-code ))))

(defun faust/post-init-yasnippet ()
  (add-hook 'faust-mode-hook 'spacemacs/load-yasnippet))
