;;; vi-tilde-fringe.el --- Displays tildes in the fringe on empty lines a la Vi.

;; Copyright (C) 2014 syl20bnr
;;
;;;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: emulation
;; Created: 27 Oct 2014
;; Version: 1.0
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/syl20bnr/vi-tilde-fringe

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

;; Usage
;; -----

;; To toggle the mode locally:
;; M-x vi-tilde-fringe-mode
;;
;; To toggle the mode globally:
;; M-x global-vi-tilde-fringe-mode
;;
;; To turn it on automatically only for programming modes:
;; (add-hook 'prog-mode-hook 'vi-tilde-fringe-mode)
;;
;; Customization
;; -------------
;;
;; Open the customization group buffer:
;; M-x customize-group RET vi-tilde-fringe RET
;;
;; There you can change the bitmap array or the face of the symbol drawn in
;; the fringe. By default the symbol is a tilde :-) and its face simply
;; inherits from `default'.

;;; Code:

(defgroup vi-tilde-fringe nil
  "Vi tilde fringe customizations."
  :group 'emulations
  :prefix 'vi-tilde-fringe-)

(defface vi-tilde-fringe-face '((t (:inherit 'default)))
    "Color for vi tilde displayed in the fringe when line is empty."
    :group 'vi-tilde-fringe)

(defcustom vi-tilde-fringe-bitmap-array
  [#b00000000
   #b00000000
   #b00000000
   #b01110001
   #b11011011
   #b10001110
   #b00000000
   #b00000000]
  "Bitmap array drawn in the fringe."
  :type 'sexp)

;;;###autoload
(define-minor-mode vi-tilde-fringe-mode
  "Buffer-local minor mode to display tildes in the fringe when the line is
empty."
  :lighter " ~"
  :group 'emulations
  (if vi-tilde-fringe-mode
      (progn
        (define-fringe-bitmap 'vi-tilde-fringe-bitmap
          vi-tilde-fringe-bitmap-array nil nil 'center)
        (setq indicate-empty-lines t)
        (add-to-list 'fringe-indicator-alist
                     '(empty-line . vi-tilde-fringe-bitmap)))
    (setq indicate-empty-lines nil)))

;;;###autoload
(define-globalized-minor-mode global-vi-tilde-fringe-mode vi-tilde-fringe-mode
  vi-tilde-fringe-mode--turn-on
  :group 'vi-tilde-fringe)

(defun vi-tilde-fringe-mode--turn-on ()
  (unless (minibufferp)
    (vi-tilde-fringe-mode +1)))

(provide 'vi-tilde-fringe)

;;; vi-tilde-fringe.el ends here

