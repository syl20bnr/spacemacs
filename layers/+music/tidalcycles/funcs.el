;;; funcs.el --- TidalCycles Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Riccardo Binetti <rbino@gmx.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun tidal-unmute-all ()
  "Unmute all orbits"
  (interactive)
  (tidal-send-string ":{")
  (tidal-send-string " mapM_ (unmute) [1,2,3,4,5,6,7,8,9,10,11,12]")
  (tidal-send-string ":}")
  )

(defun tidal-unsolo-all ()
  "Unsolo all orbits"
  (interactive)
  (tidal-send-string ":{")
  (tidal-send-string " mapM_ (unsolo) [1,2,3,4,5,6,7,8,9,10,11,12]")
  (tidal-send-string ":}")
  )
