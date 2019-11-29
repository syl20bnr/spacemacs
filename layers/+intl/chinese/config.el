;;; config.el --- Chinese Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar chinese-default-input-method 'pinyin
  "The default chinese input method. Can be `wubi` or `pinyin`.")

(defvar chinese-enable-youdao-dict nil
  "Enable YouDao Dict translation service.")

(defvar chinese-enable-avy-pinyin t
  "Enable ace-pinyin in avy-goto-char")

(defvar chinese-enable-fcitx nil
  "Enable fcitx to help writing Chinese in Evil mode.")

(defvar chinese-use-fcitx5 nil
  "Enable fcitx5 instead fcitx4.")

(defvar chinese-fcitx-use-dbus nil
  "Use dbus interface for fcitx.el.")

;; Set the monospaced font size when mixed Chinese and English words
(defun spacemacs//set-monospaced-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
                      (format   "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))

;; If the Hiragino Sans GB font is not found in your system, you could call this
;; method in dotspacemacs/user-config function with a different Chinese font name.
;; If you are using macOS, you could put the following code in your dotspacemacs/user-config function.
;; (when (spacemacs/system-is-mac)
;;   (spacemacs//set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 14 16))
