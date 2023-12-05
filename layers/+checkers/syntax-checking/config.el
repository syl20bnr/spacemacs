;;; config.el --- Syntax Checking Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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

;; Variables

(spacemacs|defc syntax-checking-enable-by-default t
  "If non-nil enable syntax-checking by default in `prog-mode'."
  '(boolean))

(spacemacs|defc syntax-checking-enable-tooltips t
  "If non-nil display tooltips when hovering on errors."
  '(boolean))

(spacemacs|defc syntax-checking-auto-hide-tooltips nil
  "Auto hide tooltips after the given number of seconds.
If non-positive or nil, do not hide tooltip."
  '(number))

;; a small circle used for flycheck-indication-mode
(define-fringe-bitmap 'syntax-checking--fringe-indicator
  (vector #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00011100
          #b00111110
          #b00111110
          #b00111110
          #b00011100
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000))

(define-obsolete-variable-alias 'syntax-checking-use-original-bitmaps
  'syntax-checking-indication-symbol "July 2022"
  "If non-nil, use the original bitmaps from flycheck.")

(spacemacs|defc syntax-checking-indication-symbol
  '(syntax-checking--fringe-indicator . nil)
  "The fringe bitmap or margin symbol used for `flycheck-indication-mode'.

The value is a cons cell (BITMAP . MARGIN-STR), in which a nil value means the
default indicator is chosen by `flycheck'.

BITMAP      is a bitmap symbol displayed in left or right fringe, which defaults
            to a small circle as defined in `syntax-checking--fringe-indicator'.
MARGIN-STR  is a string to be displayed in the margin (Defaults to nil).

Note only one of BITMAP and MARGIN-STR is used, which is dictated by
`flycheck-indication-mode'."
  '(cons symbol string))

(spacemacs|defc syntax-checking-use-standard-error-navigation nil
  "If non-nil hook into emacs standard error navigation."
  '(boolean))

(spacemacs|defc syntax-checking-window-position 'bottom
  "Popup window position."
  '(choice :tag "Position"
           (const :tag "Bottom" bottom)
           (const :tag "Top" top)
           (const :tag "Left" left)
           (const :tag "Right" right)))

(spacemacs|defc syntax-checking-window-width 0.3
  "Popup window width in columns (int) or as percentage (float)."
  '(choice :tag "Width"
           (integer :tag "Width")
           (float :tag "Width (%)")))

(spacemacs|defc syntax-checking-window-height 0.3
  "Popup window height in columns (int) or as percentage (float)."
  '(choice :tag "Width"
           (integer :tag "Width")
           (float :tag "Width (%)")))

;; internals
(defvar syntax-checking--buffer-config
  (list "^\\*Flycheck.+\\*$"
        :regexp t
        :dedicated t
        :position syntax-checking-window-position
        :width syntax-checking-window-width
        :height syntax-checking-window-height
        :stick t
        :noselect t)
  "Internal syntax checking window position config.")
