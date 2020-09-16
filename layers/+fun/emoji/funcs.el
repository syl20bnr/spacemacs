;;; funcs.el --- Emoji Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; From https://github.com/dunn/company-emoji/README.md for Linux, or on
;; macOS and using the Cocoa version of Emacs
(defun spacemacs//set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (when (fboundp 'set-fontset-font)
    (cond
     ((spacemacs/system-is-mac)
      (set-fontset-font t 'symbol
                        (font-spec :family "Apple Color Emoji")
                        frame 'prepend))
     ((spacemacs/system-is-linux)
      (set-fontset-font t 'symbol
                        (font-spec :family "Symbola")
                        frame 'prepend)))))

(defun spacemacs//set-emoji-font-for-current-frame ()
  "Adjust the font settings of current frame so Emacs can display emoji
properly."
  (spacemacs//set-emoji-font (selected-frame)))

(defun spacemacs/delay-emoji-cheat-sheet-hook ()
  "Work-around for org buffers."
  ;; we need to wait for org buffer to be fully loaded before
  ;; calling the emoji mode.
  ;; If we directly call the emoji mode at hook runtime then some
  ;; text properties are not applied correctly.
  (run-at-time 0.1 nil 'emoji-cheat-sheet-plus-display-mode))
