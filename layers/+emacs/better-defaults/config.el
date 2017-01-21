;;; config.el --- Better Emacs Defaults Layer configuration variables File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Thomas de Beauchêne <thomas.de.beauchene@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar better-defaults-move-to-beginning-of-code-first t
  "when t, first stroke of C-a will move the cursor to the beginning of code.
When nil, first stroke will go to the beginning of line.
Subsequent strokes will toggle between beginning of line and beginning of code.")

(defvar better-defaults-move-to-end-of-code-first nil
  "when t, first stroke of C-e will move the cursor to the end of code (before comments).
When nil, first stroke will go to the end of line (after comments).
Subsequent strokes will toggle between end of line and end of code.")
