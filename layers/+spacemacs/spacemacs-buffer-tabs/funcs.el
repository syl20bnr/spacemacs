
;;; funcs.el --- Spacemacs Buffer Tabs Layer functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3



(defvar spacemacs--buffer-tabs-ts-full-hint-toggle nil
  "Toggle the state of the buffer-tabs transient state documentation.

Initial value is t so full hint will be shown by default. This is
to preserve the behavior before hint toggle was implemented for
the buffer-tabs transient state.")

(defvar spacemacs--buffer-tabs-ts-full-hint nil
  "Display full buffer-tabs transient state documentation.")

(defun spacemacs//buffer-tabs-ts-toggle-hint ()
  "Toggle the full hint docstring for the buffer-tabs transient state."
  (interactive)
  (setq spacemacs--buffer-tabs-ts-full-hint-toggle
        (not spacemacs--buffer-tabs-ts-full-hint-toggle)))

(defun spacemacs//buffer-tabs-ts-hint ()
  "Return a condensed/full hint for the buffer-tabs transient state"
  (concat
   " "
   (if spacemacs--buffer-tabs-ts-full-hint-toggle
       spacemacs--buffer-tabs-ts-full-hint
     (concat "[" (propertize "?" 'face 'hydra-face-red) "] toggle help"))))


