;;; holy-mode.el --- Enter the church of Emacs

;; Copyright (C) 2014-2015 syl20bnr
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: convenience editing
;; Created: 18 Mar 2015
;; Version: 1.00
;; Package-Requires: ((emacs "24") (evil "1.0.9"))
;; URL: https://github.com/syl20bnr/spacemacs

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defvar holy-mode-modes-to-disable-alist
  `((evil-mode . 1)
    (hybrid-mode . -1)
    (evil-escape-mode . ,(when (boundp 'evil-escape-mode) 1 -1)))
  "Alist of modes that should be disabled when activating
`holy-mode'. The cdr in each cell stores the state of the mode
before it was disabled.")

;;;###autoload
(define-minor-mode holy-mode
  "Global minor mode to repulse the evil from spacemacs.

`evil-mode' and other minor modes in
`holy-mode-modes-to-disable-alist' are turned off."
  :global t
  :lighter " holy"
  :group 'spacemacs
  (if holy-mode
      (progn
        (dolist (mode holy-mode-modes-to-disable-alist)
          (when (boundp (car mode)) (funcall (car mode) -1)))
        (setq cursor-type 'box)
        (set-cursor-color "SkyBlue2")
        (when (fboundp 'spacemacs//helm-hjkl-navigation)
          (spacemacs//helm-hjkl-navigation nil)))
    (when (fboundp 'spacemacs//helm-hjkl-navigation)
      (spacemacs//helm-hjkl-navigation t))
    (dolist (mode holy-mode-modes-to-disable-alist)
      (when (boundp (car mode))
        (funcall (car mode) (cdr mode))))))

(provide 'holy-mode)
