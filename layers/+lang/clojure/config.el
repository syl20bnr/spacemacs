;;; config.el --- Clojure Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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

(setq clojure/key-binding-prefixes '(("md" . "debug")
                                     ("me" . "evaluation")
                                     ("mg" . "goto")
                                     ("mh" . "documentation")
                                     ("mr" . "refactor")
                                     ("mra" . "add")
                                     ("mrc" . "cycle/clean")
                                     ("mrd" . "destructure")
                                     ("mre" . "extract/expand")
                                     ("mrf" . "find/function")
                                     ("mrh" . "hotload")
                                     ("mri" . "introduce/inline")
                                     ("mrm" . "move")
                                     ("mrp" . "project/promote")
                                     ("mrr" . "remove/rename/replace")
                                     ("mrs" . "show/sort/stop")
                                     ("mrt" . "thread")
                                     ("mru" . "unwind/update")
                                     ("ms" . "repl")
                                     ("mt" . "test")
                                     ("mT" . "toggle")
                                     ("mf" . "format")))

(dolist (mode '(clojure-mode
                clojurec-mode
                clojurescript-mode
                clojurex-mode
                cider-repl-mode))
  (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                     mode (car x) (cdr x)))
        clojure/key-binding-prefixes))
