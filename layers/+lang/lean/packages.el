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
(setq lean-packages '(mmm-mode 
                      f 
                      dash-functional
                      (lean-mode :location (recipe
                                            :fetcher github
                                            :repo leanprover/lean
                                            :files ("src/emacs/*.el")))))
(defun lean/init-f ())

(defun lean/init-dash-functional ())

(defun lean/init-mmm-mode ())

(defun lean/init-lean-mode ()
  (use-package lean-mode
    :defer t
    :config
    (progn
    (spacemacs/set-leader-keys-for-major-mode 'lean-mode
      "cc" 'lean-std-exe
      "R" 'lean-server-restart-process
      "hh" 'lean-eldoc-documentation-function
      "f" 'lean-fill-placeholder
      "gu" 'lean-generate-tags
      "gg" 'lean-find-tag
      "ef" 'lean-set-option
      "ec" 'lean-eval-cmd
      "," 'lean-show-goal-at-pos
      "hk" 'quail-show-key
      "hi" 'lean-show-id-keyword-info
      "ht" 'lean-show-type))))
