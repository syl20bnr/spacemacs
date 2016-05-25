;;; config.el --- Ivy Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; smex is handled by the `ivy' layer and we don't want
;; to use the ownership mechanism of layers because it is dependent
;; on the order of layer declaration
(configuration-layer/remove-layer 'smex)


;; Variables

(defvar spacemacs--counsel-commands
  '(("ag" . "ag --nocolor --nogroup %s %S .")
    ("pt" . "pt -e --nocolor --nogroup %s %S .")
    ("ack" . "ack --nocolor --nogroup %s %S .")
    ("grep" . "grep -nrP %s %S ."))
  "Alist of search commands and their corresponding commands
with options to run in the shell.")

(defvar spacemacs--counsel-search-max-path-length 30
  "Truncate the current path in counsel search if it is longer
than this amount.")

(defvar spacemacs--counsel-initial-number-cand 100)

(defvar spacemacs--ivy-file-actions
  '(("f" find-file-other-frame "other frame")
    ("w" find-file-other-window "other window")
    ("v" spacemacs/find-file-vsplit "in vertical split")
    ("s" spacemacs/find-file-split "in horizontal split")
    ("l" find-file-literally "literally"))
  "Default ivy actions for files.")

(defvar spacemacs--ivy-grep-actions
  (loop for j in spacemacs--ivy-file-actions
        for key = (nth 0 j)
        for func = (nth 1 j)
        for desc = (nth 2 j)
        collect `(,key (lambda (x) (spacemacs//counsel-with-git-grep (quote ,func) x)) ,desc))
  "Default ivy actions to be used with git-grep output.")
