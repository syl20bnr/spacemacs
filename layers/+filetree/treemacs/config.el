;;; packages.el --- treemacs Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Alexander Miller <alexanderm@web.de>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar treemacs-use-follow-mode t
  "When non-nil use `treemacs-follow-mode'.")

(defvar treemacs-use-filewatch-mode t
  "When non-nil use `treemacs-filewatch-mode'.")

(defvar treemacs-use-collapsed-directories (if (executable-find "python") 3 0)
  "Number of directories to collapse with `treemacs-collapse-dirs'.
Must be a number.")

(defvar treemacs-use-git-mode
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t) 'extended)
    (`(t . _) 'simple))
  "Type of git integration for `treemacs-git-mode'.
There are 2 possible values:
1) simple, which highlights only files based on their git status, and is
   slightly faster
2) extended, which highlights both files and directories, but requires python")
