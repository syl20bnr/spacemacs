;;; config.el --- Clojure Layer configuration File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; Variables

(space-macs|define-jump-handlers clojure-mode)
(space-macs|define-jump-handlers clojurec-mode)
(space-macs|define-jump-handlers clojurescript-mode)
(space-macs|define-jump-handlers clojurex-mode)
(space-macs|define-jump-handlers cider-repl-mode)

(defvar clojure-enable-fancify-symbols nil
  "If non-nil, the `fancify-symbols' function is enabled.")

(defvar clojure-enable-sayid nil
  "If non-nil, the Sayid Clojure debugger is enabled.")

(defvar clojure-enable-clj-refactor nil
  "If non-nil, the clj-refactor is enabled.")

(defvar clojure-enable-linters nil
  "If non-nil, enable clojure linters.")

(defvar clojure-backend nil
  "The backend to use for IDE features.
Possible values are `lsp' and `cider'.
If `nil' then 'cider` is the default backend unless `lsp' layer is used")


