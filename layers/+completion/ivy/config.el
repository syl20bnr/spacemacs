;;; config.el --- Ivy Layer Configuration File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3



;; Layer Variables

(defvar ivy-enable-advanced-buffer-information nil
  "If non-nil, enable `ivy-rich' which adds information on buffers.")

(defvar ivy-ret-visits-directory nil
  "If non-nil, swap `RET' and `C-j' so that `RET' goes into directory like Ido.")


;; Private Variables

(defvar space-macs--counsel-commands
  '(;; --line-number forces line numbers (disabled by default on windows)
    ;; no --vimgrep because it adds column numbers that wgrep can't handle
    ;; see https://github.com/syl20bnr/space-macs/pull/8065
    ("rg" . "rg --smart-case --no-heading --color never --line-number --max-columns 150 %s %S .")
    ("ag" . "ag --nocolor --nogroup %s %S .")
    ("pt" . "pt -e --nocolor --nogroup %s %S .")
    ("ack" . "ack --nocolor --nogroup %s %S .")
    ("grep" . "grep -nrP %s %S ."))
  "An alist of search commands and their corresponding commands
with options to run in the shell.")

(defvar space-macs--counsel-search-max-path-length 30
  "Truncate the current path in counsel search if it is longer
than this amount.")

(defvar space-macs--counsel-initial-number-cand 100)

(defvar space-macs--ivy-file-actions
  '(("f" find-file-other-frame "other frame")
    ("j" find-file-other-window "other window")
    ("v" space-macs/find-file-vsplit "in vertical split")
    ("s" space-macs/find-file-split "in horizontal split")
    ("l" find-file-literally "literally")
    ("d" space-macs/delete-file-confirm "delete file")
    ("r" space-macs/rename-file "rename file"))
  "Default ivy actions for files.")

(defvar space-macs--ivy-grep-actions
  (cl-loop for j in space-macs--ivy-file-actions
        for key = (nth 0 j)
        for func = (nth 1 j)
        for desc = (nth 2 j)
        collect `(,key (lambda (x) (space-macs//counsel-with-git-grep (quote ,func) x)) ,desc))
  "Default ivy actions to be used with git-grep output.")


