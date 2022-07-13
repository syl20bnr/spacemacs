;;; funcs.el --- evil-better-jumper Layer Functions File for spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanh@gmail.com>
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


(defun evil-better-jumper/set-jump-a (orig-fn &rest args)
  "Set a jump point and ensure ORIG-FN doesn't set any new jump points."
  (better-jumper-set-jump (if (markerp (car args)) (car args)))
  (let ((evil--jumps-jumping t)
        (better-jumper--jumping t))
    (apply orig-fn args)))

;; (defun evil-better-jumper/set-jump-maybe-a (orig-fn &rest args)
;;   "Set a jump point if ORIG-FN returns non-nil."
;;   (let ((origin (point-marker))
;;         (result
;;          (let* ((evil--jumps-jumping t)
;;                 (better-jumper--jumping t))
;;            (apply orig-fn args))))
;;     (unless result
;;       (with-current-buffer (marker-buffer origin)
;;         (better-jumper-set-jump
;;          (if (markerp (car args))
;;              (car args)
;;            origin))))
;;     result))

;; (defun evil-better-jumper/set-jump-h ()
;;   "Run `better-jumper-set-jump' but return nil, for short-circuiting hooks."
;;   (better-jumper-set-jump)
;;   nil)
