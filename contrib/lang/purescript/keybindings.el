;;; keybindings.el --- Purescript Layer keybindings File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2015 Ryan L. Bell & Contributors
;;
;; Author: Ryan L. Bell
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(evil-leader/set-key-for-mode 'purescript-mode
;; -- ----- purescript-mode bindings ------------------
  "mia"  'purescript-align-imports
  "mif"  'purescript-mode-format-imports
  "min"  'purescript-navigate-imports
  "mir"  'purescript-navigate-imports-return

;; ------- inferior-psci-mode bindings ---------------
  "mpl"  'psci/load-current-file!
  "mpi"  'psci/load-module!
  "mppr" 'psci/load-project-modules!
  "mpr"  'psci/reset!
  "mpq"  'psci/quit!
  "mpp"  'psci
  )
