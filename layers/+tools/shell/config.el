;;; config.el --- shell configuration File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Emacs built-in variables

;; move point to the end of buffer on new output
(setq comint-move-point-for-output t)

;; allow moving around the buffer in emacs >= 26.1 in evil's normal mode
(setq term-char-mode-point-at-process-mark nil)

;; Variables
(defvar shell-default-shell (if (spacemacs/system-is-mswindows)
                                'eshell
                              'ansi-term)
  "Default shell to use in Spacemacs. Possible values are `eshell' (default),
`shell', `term', `ansi-term', `multi-term' and `vterm'.")

(defvar shell-default-position 'bottom
  "Position of the shell. Possible values are `top', `bottom', `full',
  `left' and `right'.")

(defvar shell-default-height 30
  "Height in percents for the shell window.")

(defvar shell-default-width 30
  "Width in percents for the shell window.")

(defvar shell-default-term-shell shell-file-name
  "Default shell to use in `term', `ansi-term' and `vterm' shells.")

(defvar shell-enable-smart-eshell nil
  "If non-nil then `em-smart' is enabled. `em-smart' allows to quickly review
  commands, modify old commands or enter a new one.")

(defvar shell-protect-eshell-prompt t
  "If non-nil then eshell's prompt is protected. This means that
  movement to the prompt is inhibited like for `comint-mode'
  prompts and the prompt is made read-only")

(defvar shell-default-full-span t
  "If non-nil, the `shell' buffer spans full width of a frame.")

(defvar close-window-with-terminal nil
  "If non-nil, the window is closed when the terminal is stopped.
  This is only applied to `term' and `ansi-term' modes.")

(defvar spacemacs-vterm-history-file-location nil
  "Bash history full file name.")
