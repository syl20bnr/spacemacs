;;; config.el --- proportional Layer configuration
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Johannes Goslar <jogo@kronberger-spiele.de>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar proportional-font "DejaVu Sans"
  "Default proportional-font to activate.")

(defvar proportional-monospace-font "DejaVu Sans Mono"
  "Default proportional-font to activate.")

(add-to-list 'default-frame-alist '(font . proportional-font))
(set-frame-font proportional-font)
(set-fontset-font "fontset-default" 'symbol proportional-font)
(setq variable-pitch '((t :family proportional-font)))
