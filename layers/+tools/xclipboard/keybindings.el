;;; keybindings.el --- xclipboard layer keybindings file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Google Inc. & Contributors
;;
;; Author: Charles Weill <weill@google.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(evil-leader/set-key "x y" 'xclipboard/copy)
(evil-leader/set-key "x p" 'xclipboard/paste)
