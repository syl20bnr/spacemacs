;;; packages.el --- Asciidoc Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2015 Mark Safronov & Contributors
;;
;; Author: Mark Safronov <hijarian@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar asciidoc-packages
  '(adoc-mode)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun asciidoc/init-adoc-mode ()
  (use-package adoc-mode
    :mode (("\\.adoc?$" . adoc-mode))
    :defer t))
