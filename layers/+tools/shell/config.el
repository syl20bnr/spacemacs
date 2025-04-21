;;; config.el --- shell configuration File for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
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


;; Emacs built-in variables

;; move point to the end of buffer on new output
(setq comint-move-point-for-output t)

;; allow moving around the buffer in emacs >= 26.1 in evil's normal mode
(setq term-char-mode-point-at-process-mark nil)

;; Variables
(defvar shell-default-shell (if (spacemacs/system-is-mswindows)
                                'eshell
                              'ansi-term)
  "Default shell to use in Spacemacs.

Possible values are `ansi-term' (default for Linux/macOS),
`eshell' (default for windows), `shell', `term', `eat', `vterm',
`multi-term' and `multi-vterm'.")

(spacemacs|defc shell-default-position 'bottom
  "Position of the shell. Possible values are `top', `bottom', `full',
  `left' and `right'."
  '(choice (const top) (const bottom) (const full) (const left) (const right)))

(spacemacs|defc shell-default-height 30
  "Height in percents for the shell window."
  'integer)

(defvar shell-default-width 30
  "Width in percents for the shell window.")

(defvar shell-default-term-shell shell-file-name
  "Default shell to use in `term', `ansi-term' and `vterm' shells.")

(defvar shell-enable-smart-eshell nil
  "If non-nil then `em-smart' is enabled.

`em-smart' allows to quickly review commands, modify old commands
or enter a new one.")

(defvar shell-protect-eshell-prompt t
  "If non-nil then eshell's prompt is protected.

This means that movement to the prompt is inhibited like for
`comint-mode' prompts and the prompt is made read-only")

(defvar shell-default-full-span t
  "If non-nil, the `shell' buffer spans full width of a frame.")

(define-obsolete-variable-alias
  'close-window-with-terminal
  'shell-close-window-with-terminal
  "2025-02-17")

(defvar shell-close-window-with-terminal nil
  "If non-nil, the window is closed when the terminal is stopped.
This is only applied to `term' and `ansi-term' modes.")

(defvar shell-enable-vterm-support t
  "If non-nil, enable the `vterm' and `multi-vterm' packages.'

These packages require dynamic module support in your Emacs, as
well as cmake and libtool.  See the layer README for details.")

(defvar spacemacs-vterm-history-file-location nil
  "Bash history full file name.")
