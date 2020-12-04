;;; config.el --- Tree-macs Layer configuration File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Alexander Miller <alexanderm@web.de>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defvar tree-macs-use-follow-mode t
  "When non-nil use `tree-macs-follow-mode'.")

(defvar tree-macs-use-filewatch-mode t
  "When non-nil use `tree-macs-filewatch-mode'.")

(defvar tree-macs-use-scope-type 'Frames
  "Determines the scope of tree-macs buffers and workspaces.
Possible values are:
 - `Frames' - to scope tree-macs to the current frame
 - `Perspectives' - to scope tree-macs in conjunction with `persp-mode'.")

(defvar tree-macs-use-git-mode
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t) 'deferred)
    (`(t . _) 'simple))
  "Type of git integration for `tree-macs-git-mode'.
There are 2 possible values:
1) simple, which highlights only files based on their git status, and is
   slightly faster
2) extended, which highlights both files and directories, but requires python
3) deferred, which is the same is extended, but delays highlighting for improved
   performance")

(defvar tree-macs-lock-width nil
  "When non-nil the tree-macs window will not be manually resizable by default.")

(defvar tree-macs-use-icons-dired t
  "When non-nil use `tree-macs-icons-dired'")

(defvar tree-macs-use-all-the-icons-theme nil
  "Enable the tree-macs supported `all-the-icons' theme")


