;;; config.el --- Colors Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar chinese-default-input-method 'pinyin
  "The default chiense input method. Can be `wubi` or `pinyin`.")

;; Set the monospaced font size when mixed Chinese and English words
(defun spacemacs//set-monospaced-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
                      (format   "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))

;; If the Hiragino Sans GB font is not found in your system, you could call this
;; method in dotspacemacs/config function with a different Chinese font name.
(when (system-is-mac)
  (spacemacs//set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 14 16))
