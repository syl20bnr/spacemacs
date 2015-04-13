;;; config.el --- Clojure Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
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

(defvar clojure-enable-fancify-symbols nil
  "If non nil the `fancify-symbols' function is enabled.")

;; not supported for now
;; (setq clojure/key-binding-prefixes '(("md" . "documentation")
;;                                      ("me" . "evaluation")
;;                                      ("mg" . "goto")
;;                                      ("mr" . "refactor")
;;                                      ("mt" . "test")))
;; (mapc (lambda (x) (spacemacs/declare-prefix (car x) (cdr x)))
;;             clojure/key-binding-prefixes)
