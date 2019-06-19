;;; config.el --- Ivy Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3



;; Layer Variables

(defvar ivy-enable-advanced-buffer-information nil
  "If non-nil, enable `ivy-rich' which adds information on buffers.")

(defvar ivy-ret-visits-directory nil
  "If non-nil, swap `RET' and `C-j' so that `RET' goes into directory like Ido.")


;; Private Variables

(defvar spacemacs--counsel-commands
  '(;; --line-number forces line numbers (disabled by default on windows)
    ;; no --vimgrep because it adds column numbers that wgrep can't handle
    ;; see https://github.com/syl20bnr/spacemacs/pull/8065
    ("rg" . "rg --smart-case --no-heading --color never --line-number --max-columns 150 %s %S .")
    ("ag" . "ag --nocolor --nogroup %s %S .")
    ("pt" . "pt -e --nocolor --nogroup %s %S .")
    ("ack" . "ack --nocolor --nogroup %s %S .")
    ("grep" . "grep -nrP %s %S ."))
  "An alist of search commands and their corresponding commands
with options to run in the shell.")

(defvar spacemacs--counsel-search-max-path-length 30
  "Truncate the current path in counsel search if it is longer
than this amount.")

(defvar spacemacs--counsel-initial-number-cand 100)

(defvar spacemacs--ivy-file-actions
  '(("f" find-file-other-frame "other frame")
    ("j" find-file-other-window "other window")
    ("v" spacemacs/find-file-vsplit "in vertical split")
    ("s" spacemacs/find-file-split "in horizontal split")
    ("l" find-file-literally "literally")
    ("d" spacemacs/delete-file-confirm "delete file")
    ("r" spacemacs/rename-file "rename file"))
  "Default ivy actions for files.")

(defvar spacemacs--ivy-grep-actions
  (cl-loop for j in spacemacs--ivy-file-actions
        for key = (nth 0 j)
        for func = (nth 1 j)
        for desc = (nth 2 j)
        collect `(,key (lambda (x) (spacemacs//counsel-with-git-grep (quote ,func) x)) ,desc))
  "Default ivy actions to be used with git-grep output.")
