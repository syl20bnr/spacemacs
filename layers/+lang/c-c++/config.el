;;; config.el --- C/C++ Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(defvar c-c++-enable-clang-support nil
  "If non nil Clang related packages and configuration are enabled.")

(defvar c-c++-enable-clang-format-on-save nil
  "If non-nil, automatically format code with ClangFormat on
  save. Clang support has to be enabled for this to work.")

(spacemacs|define-jump-handlers c++-mode)
(spacemacs|define-jump-handlers c-mode)

(defvar c-c++-default-mode-for-headers 'c-mode
  "Default mode to open header files. Can be `c-mode' or `c++-mode'.")
