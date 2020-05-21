;;; config.el --- Treemacs Layer configuration File for Spacemacs
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

(defvar treemacs-use-scope-type 'Frames
  "Determines the scope of treemacs buffers and workspaces.
Possible values are:
 - `Frames' - to scope treemacs to the current frame
 - `Perspectives' - to scope treemacs in conjunction with `persp-mode'.")

(defvar treemacs-use-git-mode
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t) 'deferred)
    (`(t . _) 'simple))
  "Type of git integration for `treemacs-git-mode'.
There are 2 possible values:
1) simple, which highlights only files based on their git status, and is
   slightly faster
2) extended, which highlights both files and directories, but requires python
3) deferred, which is the same is extended, but delays highlighting for improved
   performance")

(defvar treemacs-lock-width nil
  "When non-nil the treemacs window will not be manually resizable by default.")

(defvar treemacs-use-icons-dired t
  "When non-nil use `treemacs-icons-dired'")
