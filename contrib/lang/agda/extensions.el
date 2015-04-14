;;; extensions.el --- Agda2 Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2015 Oliver Charles & Contributors
;;
;; Author: Oliver Charles <ollie@ocharles.org.uk>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar agda-pre-extensions '(agda))

(defun agda/init-agda ()
  (custom-set-faces
   '(agda2-highlight-datatype-face ((t (:inherit font-lock-type-face))))
   '(agda2-highlight-function-face ((t (:inherit font-lock-type-face))))
   '(agda2-highlight-inductive-constructor-face ((t (:inherit font-lock-function-name-face))))
   '(agda2-highlight-keyword-face ((t (:inherit font-lock-keyword-face))))
   '(agda2-highlight-module-face ((t (:inherit font-lock-constant-face))))
   '(agda2-highlight-number-face ((t nil)))
   '(agda2-highlight-postulate-face ((t (:inherit font-lock-type-face))))
   '(agda2-highlight-primitive-type-face ((t (:inherit font-lock-type-face))))
   '(agda2-highlight-record-face ((t (:inherit font-lock-type-face)))))

  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate")))

  (evil-leader/set-key-for-mode 'agda2-mode
    "ml" 'agda2-load
    "ma" 'agda2-auto
    "mr" 'agda2-refine
    "ms" 'agda2-solveAll
    "mt" 'agda2-goal-type
    "mc" 'agda2-make-case
    "mf" 'agda2-next-goal
    "m?" 'agda2-show-goals
    "me" 'agda2-show-context
    "mb" 'agda2-previous-goal
    "m," 'agda2-goal-and-context
    "m=" 'agda2-show-constraints
    "mh" 'agda2-helper-function-type
    "md" 'agda2-infer-type-maybe-toplevel
    "mw" 'agda2-why-in-scope-maybe-toplevel
    "m." 'agda2-goal-and-context-and-inferred
    "mo" 'agda2-module-contents-maybe-toplevel
    "mn" 'agda2-compute-normalised-maybe-toplevel))
