;;; evil-collection-gited.el --- Evil bindings for Gited -*- lexical-binding: t -*-

;; Copyright (C) 2022 Earl Hyatt

;; Author: Earl Hyatt <okamsn@protonmail.com>
;; Maintainer: Earl Hyatt <okamsn@protonmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3") (evil "1.2") (gited "0.6"))
;; Keywords: evil, git, tools, vc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for Gited.

;;; Code:
(require 'gited nil t)
(require 'evil-collection)
(declare-function gited-summary "gited")
(declare-function gited-hide-details-mode "gited")

(defconst evil-collection-gited-maps '(gited-mode-map))

;;;###autoload
(defun evil-collection-gited-setup ()
  "Set up `evil' bindings for `gited'."
  (evil-collection-set-readonly-bindings 'gited-mode-map)
  (evil-collection-define-key 'normal 'gited-mode-map
    "g?" 'gited-summary
    "(" 'gited-hide-details-mode
    "gr" 'gited-update
    (kbd "C-c C-c") 'gited-commit
    (kbd "C-c C-b") 'gited-list-branches

    ;; Movement
    "j" 'gited-next-line
    "k" 'gited-prev-line
    (kbd "M-{") 'gited-prev-marked-branch
    (kbd "M-}") 'gited-next-marked-branch
    "G" 'gited-goto-last-branch
    "gg" 'gited-goto-first-branch
    "gj" 'gited-next-marked-branch
    "gk" 'gited-prev-marked-branch
    "gl" 'tabulated-list-next-column
    "gh" 'tabulated-list-previous-column
    "]]" 'gited-next-marked-branch
    "[[" 'gited-prev-marked-branch
    "<" 'gited-prev-marked-branch
    ">" 'gited-next-marked-branch
    " " 'gited-next-line
    [?\S-\ ] 'gited-prev-line
    [remap next-line] 'gited-next-line
    [remap previous-line] 'gited-prev-line
    "J" 'gited-goto-branch

    "gA" 'gited-move-to-end-of-author
    "ga" 'gited-move-to-author
    "gB" 'gited-move-to-end-of-branchname
    "gb" 'gited-move-to-branchname
    "gD" 'gited-move-to-end-of-date
    "gd" 'gited-move-to-date

    ;; Operations on Things at Point
    "=" 'gited-diff
    "A" 'gited-add-patched-files
    "a" 'gited-apply-patch
    "B" 'gited-bisect
    "C" 'gited-copy-branch
    "c" 'gited-checkout-branch
    "d" 'gited-flag-branch-deletion
    "e" 'gited-extract-patches
    "F" 'gited-pull ; Similar to Magit binding. Don't conflict with "f".
    "gR" 'gited-change-current-remote-rep

    "gs" 'gited-status             ; These "g"-prefixed operations open
    "gL" 'gited-log                ; new windows.
    "gN" 'gited-log-last-n-commits ;
    "gO" 'gited-origin             ; Use "go" to visit branch source.
    "gc" 'gited-show-commit        ;

    "M" 'gited-merge-branch
    "O" 'tabulated-list-sort
    "o" 'tabulated-list-sort
    "P" 'gited-push                ; Only branches.
    "p" 'gited-set-object-upstream ; Pushes branches and tags.
    "R" 'gited-rename-branch
    "r" 'gited-reset-branch
    "s" nil                ; Don't conflict with `tabulated-list-sort' on "S".
    "ss" 'gited-stash
    "sA" 'gited-stash-pop
    "sa" 'gited-stash-apply
    "sb" 'gited-stash-branch
    "sD" 'gited-delete-all-stashes
    "sd" 'gited-stash-drop
    "Y" 'gited-copy-branchname-as-kill

    (kbd "RET") 'gited-visit-branch-sources
    "go" 'gited-visit-branch-sources

    ;; Operations on Marked and Flagged Branches/Tags
    "D" 'gited-do-delete
    "K" 'gited-do-kill-lines
    "T" 'gited-do-sync-with-trunk
    "x" 'gited-do-flagged-delete

    ;; Marks
    "*" nil
    "*%" 'gited-mark-branches-regexp
    "*m" 'gited-mark
    "*u" 'gited-unmark
    "*?" 'gited-unmark-all-branches
    "*!" 'gited-unmark-all-marks
    (kbd "* <delete>") 'gited-unmark-backward
    (kbd "* C-n") 'gited-next-marked-branch
    (kbd "* C-p") 'gited-prev-marked-branch
    "*t" 'gited-toggle-marks
    "*l" 'gited-mark-local-tags
    "*N" 'gited-number-marked

    "U" 'gited-unmark-all-marks
    "m" 'gited-mark
    "t" 'gited-toggle-marks
    "~" 'gited-toggle-marks
    "u" 'gited-unmark
    (kbd "<delete>") 'gited-unmark-backward

    ;; Searching for marks
    "%" nil
    ;; "%d" No flag for deletion.
    "%t" 'gited-mark-branches-by-date
    "%c" 'gited-mark-branches-containing-commit
    "%g" 'gited-mark-branches-containing-regexp
    "%m" 'gited-mark-branches-regexp
    "%M" 'gited-mark-merged-branches
    "%U" 'gited-mark-unmerged-branches

    ;; Tags instead of thumbnails. Only usable when listing tags.  Ideally,
    ;; these would have a separate map that would be used when displaying tags.
    (kbd "C-t a") 'gited-tag-add
    (kbd "C-t d") 'gited-tag-delete
    (kbd "C-t D") 'gited-remote-tag-delete
    (kbd "C-t F") 'gited-fetch-remote-tags))

(provide 'evil-collection-gited)
;;; evil-collection-gited.el ends here
