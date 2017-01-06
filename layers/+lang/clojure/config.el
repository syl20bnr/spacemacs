;;; config.el --- Clojure Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; ---------------------------------------------------------------------------
;; Prefixes
;; ---------------------------------------------------------------------------

;; Variables

(spacemacs|defvar-company-backends cider-mode)
(spacemacs|defvar-company-backends cider-repl-mode)

(spacemacs|define-jump-handlers clojure-mode)
(spacemacs|define-jump-handlers clojurec-mode)
(spacemacs|define-jump-handlers clojurescript-mode)
(spacemacs|define-jump-handlers clojurex-mode)
(spacemacs|define-jump-handlers cider-repl-mode)

(defvar clojure-enable-fancify-symbols nil
  "If non nil the `fancify-symbols' function is enabled.")
