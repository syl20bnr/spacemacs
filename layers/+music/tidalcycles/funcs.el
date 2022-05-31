;;; funcs.el --- TidalCycles Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Riccardo Binetti <rbino@gmx.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


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
