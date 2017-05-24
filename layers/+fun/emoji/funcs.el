;;; funcs.el --- Emoji Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; From https://github.com/dunn/company-emoji/README.md for Linux, or on
;; Mac OS X and using the Cocoa version of Emacs
(defun spacemacs//set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (cond
   ((spacemacs/system-is-mac)
    (set-fontset-font t 'symbol
                      (font-spec :family "Apple Color Emoji")
                      frame 'prepend))
   ((spacemacs/system-is-linux)
    (set-fontset-font t 'symbol
                      (font-spec :family "Symbola")
                      frame 'prepend))))

(defun spacemacs//set-emoji-font-for-current-frame ()
  "Adjust the font settings of current frame so Emacs can display emoji
properly."
  (spacemacs//set-emoji-font (selected-frame)))
