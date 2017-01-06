;;; config.el --- shell configuration File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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

;; Add shell buffers to useful buffers list
(push "\\*\\(ansi-term\\|eshell\\|shell\\|terminal\.\+\\)\\(-[0-9]+\\)?\\*"
      spacemacs-useful-buffers-regexp)

;; Variables

(spacemacs|defvar-company-backends eshell-mode)

(defvar shell-default-shell (if (eq window-system 'w32)
                                'eshell
                              'ansi-term)
  "Default shell to use in Spacemacs. Possible values are `eshell', `shell',
`term' and `ansi-term'.")

(defvar shell-default-position 'bottom
  "Position of the shell. Possible values are `top', `bottom', `full',
`left' and `right'.")

(defvar shell-default-height 30
  "Height in percents for the shell window.")

(defvar shell-default-term-shell shell-file-name
  "Default shell to use in `term' and `ansi-term' shells.")

(defvar shell-enable-smart-eshell nil
  "If non-nil then `em-smart' is enabled. `em-smart' allows to quickly review
commands, modify old commands or enter a new one.")

(defvar shell-protect-eshell-prompt t
  "If non-nil then eshell's prompt is protected. This means that
movement to the prompt is inhibited like for `comint-mode'
prompts and the prompt is made read-only")

(defvar shell-default-full-span t
  "If non-nil, the `shell' buffer spans full width of a frame.")
