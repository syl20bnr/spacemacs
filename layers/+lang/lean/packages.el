;;; packages.el --- Lean Layer packages File for Spacemacs
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
(setq lean-packages '(mmm-mode f dash-functional
                      (lean-mode :location (recipe
                                            :fetcher github
                                            :repo leanprover/lean
                                            :files ("src/emacs/*.el")))))
(defun lean/init-f ()
  (use-package f
    :defer t))

(defun lean/init-dash-functional ()
  (use-package dash-functional
    :defer t))

(defun lean/init-mmm-mode ()
  (use-package mmm-mode
    :defer t))

(defun lean/init-lean-mode ()
  (use-package lean-mode
    :defer t
    :config
    (evil-leader/set-key-for-mode 'lean-mode
      "L" 'lean-std-exe
      "R" 'lean-server-restart-process
      "d" 'lean-eldoc-documentation-function
      "f" 'lean-fill-placeholder
      "gu" 'lean-generate-tags
      "gg" 'lean-find-tag
      "o" 'lean-set-option
      "c" 'lean-eval-cmd
      "," 'lean-show-goal-at-pos
      "k" 'quail-show-key
      "i" 'lean-show-id-keyword-info
      "t" 'lean-show-type)))
