;;; config.el --- Perl5 Layer config File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Troy Hinckley <troyhinckley@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(space-macs|define-jump-handlers cperl-mode)

(defvar perl5-perltidy-executable "perltidy"
  "Location of perltidy executable.")

(defvar perl5-perltidy-options '()
  "Command line options to pass to perltidy")

(defvar perl5-backend nil
  "The backend to use for IDE features.
Possible values are `lsp' and `company-plsense'.
If `nil' then 'company-plsense` is the default backend unless `lsp' layer is used")


