;;; packages.el --- TidalCycles Layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Riccardo Binetti <rbino@gmx.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst tidalcycles-packages
  '(tidal))

(defun tidalcycles/init-tidal ()
  (use-package tidal
    :defer t
    :mode ("\\.tidal\\'" . tidal-mode)
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'tidal-mode "mt" "tidal")
      (spacemacs/declare-prefix-for-mode 'tidal-mode "mr" "run")
      (spacemacs/declare-prefix-for-mode 'tidal-mode "ms" "silence")
      (spacemacs/declare-prefix-for-mode 'tidal-mode "mm" "mute")
      (spacemacs/declare-prefix-for-mode 'tidal-mode "mo" "solo")
      (spacemacs/set-leader-keys-for-major-mode 'tidal-mode
        "RET" 'tidal-run-multiple-lines
        "ts" 'tidal-start-haskell
        "tq" 'tidal-quit-haskell
        "r1" 'tidal-run-d1
        "r2" 'tidal-run-d2
        "r3" 'tidal-run-d3
        "r4" 'tidal-run-d4
        "r5" 'tidal-run-d5
        "r6" 'tidal-run-d6
        "r7" 'tidal-run-d7
        "r8" 'tidal-run-d8
        "r9" 'tidal-run-d9
        "s1" 'tidal-stop-d1
        "s2" 'tidal-stop-d2
        "s3" 'tidal-stop-d3
        "s4" 'tidal-stop-d4
        "s5" 'tidal-stop-d5
        "s6" 'tidal-stop-d6
        "s7" 'tidal-stop-d7
        "s8" 'tidal-stop-d8
        "s9" 'tidal-stop-d9
        "mu" 'tidal-unmute-all
        "ou" 'tidal-unsolo-all))))
