;;; evil-collection-ibuffer.el --- Evil bindings for IBuffer -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, ibuffer, tools

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
;; Evil bindings for IBuffer.

;;; Code:
(require 'evil-collection)
(require 'ibuffer)

(defconst evil-collection-ibuffer-maps '(ibuffer-mode-map))

;;;###autoload
(defun evil-collection-ibuffer-setup ()
  "Set up `evil' bindings for `ibuffer'."
  (evil-set-initial-state 'ibuffer-mode 'normal)

  (evil-collection-define-key 'normal 'ibuffer-mode-map
    (kbd "C-d") (if evil-want-C-d-scroll
                    'evil-scroll-down
                  'ibuffer-mark-for-delete-backwards))

  (evil-collection-define-key 'normal 'ibuffer-mode-map
    (kbd "=") 'ibuffer-diff-with-file
    (kbd "J") 'ibuffer-jump-to-buffer
    (kbd "M-g") 'ibuffer-jump-to-buffer
    (kbd "M-s a C-s") 'ibuffer-do-isearch
    (kbd "M-s a M-C-s") 'ibuffer-do-isearch-regexp
    (kbd "M-s a C-o") 'ibuffer-do-occur

    ;; mark
    (kbd "m") 'ibuffer-mark-forward
    (kbd "~") 'ibuffer-toggle-marks
    (kbd "u") 'ibuffer-unmark-forward
    (kbd "DEL") 'ibuffer-unmark-backward
    (kbd "M-DEL") 'ibuffer-unmark-all
    (kbd "* *") 'ibuffer-mark-special-buffers
    (kbd "* c") 'ibuffer-change-marks
    (kbd "U") 'ibuffer-unmark-all-marks
    (kbd "* M") 'ibuffer-mark-by-mode
    (kbd "* m") 'ibuffer-mark-modified-buffers
    (kbd "* u") 'ibuffer-mark-unsaved-buffers
    (kbd "* s") 'ibuffer-mark-special-buffers
    (kbd "* r") 'ibuffer-mark-read-only-buffers
    (kbd "* /") 'ibuffer-mark-dired-buffers
    (kbd "* e") 'ibuffer-mark-dissociated-buffers
    (kbd "* h") 'ibuffer-mark-help-buffers
    (kbd "* z") 'ibuffer-mark-compressed-file-buffers
    (kbd ".") 'ibuffer-mark-old-buffers

    (kbd "d") 'ibuffer-mark-for-delete
    (kbd "x") 'ibuffer-do-kill-on-deletion-marks

    ;; immediate operations
    (kbd "gj") 'ibuffer-forward-line
    (kbd "gk") 'ibuffer-backward-line

    (kbd "}") 'ibuffer-forward-next-marked
    (kbd "{") 'ibuffer-backwards-next-marked
    (kbd "M-}") 'ibuffer-forward-next-marked
    (kbd "M-{") 'ibuffer-backwards-next-marked

    (kbd "gR") 'ibuffer-redisplay
    (kbd "gr") 'ibuffer-update

    "`" 'ibuffer-switch-format
    "-" 'ibuffer-add-to-tmp-hide
    "+" 'ibuffer-add-to-tmp-show
    "X" 'ibuffer-bury-buffer
    (kbd ",") 'ibuffer-toggle-sorting-mode
    (kbd "o i") 'ibuffer-invert-sorting
    (kbd "o a") 'ibuffer-do-sort-by-alphabetic
    (kbd "o v") 'ibuffer-do-sort-by-recency
    (kbd "o s") 'ibuffer-do-sort-by-size
    (kbd "o f") 'ibuffer-do-sort-by-filename/process
    (kbd "o m") 'ibuffer-do-sort-by-major-mode

    (kbd "s RET") 'ibuffer-filter-by-mode
    (kbd "s m") 'ibuffer-filter-by-used-mode
    (kbd "s M") 'ibuffer-filter-by-derived-mode
    (kbd "s n") 'ibuffer-filter-by-name
    (kbd "s *") 'ibuffer-filter-by-starred-name
    (kbd "s f") 'ibuffer-filter-by-filename
    (kbd "s b") 'ibuffer-filter-by-basename
    (kbd "s .") 'ibuffer-filter-by-file-extension
    (kbd "s <") 'ibuffer-filter-by-size-lt
    (kbd "s >") 'ibuffer-filter-by-size-gt
    (kbd "s i") 'ibuffer-filter-by-modified
    (kbd "s v") 'ibuffer-filter-by-visiting-file
    (kbd "s c") 'ibuffer-filter-by-content
    (kbd "s e") 'ibuffer-filter-by-predicate

    (kbd "s r") 'ibuffer-switch-to-saved-filters
    (kbd "s a") 'ibuffer-add-saved-filters
    (kbd "s x") 'ibuffer-delete-saved-filters
    (kbd "s d") 'ibuffer-decompose-filter
    (kbd "s s") 'ibuffer-save-filters
    (kbd "s p") 'ibuffer-pop-filter
    (kbd "s <up>") 'ibuffer-pop-filter
    (kbd "s !") 'ibuffer-negate-filter
    (kbd "s t") 'ibuffer-exchange-filters
    (kbd "s TAB") 'ibuffer-exchange-filters
    (kbd "s o") 'ibuffer-or-filter
    (kbd "s |") 'ibuffer-or-filter
    (kbd "s &") 'ibuffer-and-filter
    (kbd "s g") 'ibuffer-filters-to-filter-group
    (kbd "s P") 'ibuffer-pop-filter-group
    (kbd "s S-<up>") 'ibuffer-pop-filter-group
    (kbd "s D") 'ibuffer-decompose-filter-group
    (kbd "s /") 'ibuffer-filter-disable

    (kbd "C-j") 'ibuffer-forward-filter-group
    (kbd "M-n") 'ibuffer-forward-filter-group
    (kbd "]]") 'ibuffer-forward-filter-group
    "\t" 'ibuffer-forward-filter-group
    (kbd "M-p") 'ibuffer-backward-filter-group
    (kbd "C-k") 'ibuffer-backward-filter-group
    (kbd "[[") 'ibuffer-backward-filter-group
    [backtab] 'ibuffer-backward-filter-group
    (kbd "M-j") 'ibuffer-jump-to-filter-group
    (kbd "gx") 'ibuffer-kill-line
    (kbd "C-y") 'ibuffer-yank
    (kbd "s S") 'ibuffer-save-filter-groups
    (kbd "s R") 'ibuffer-switch-to-saved-filter-groups
    (kbd "s X") 'ibuffer-delete-saved-filter-groups
    (kbd "s \\") 'ibuffer-clear-filter-groups

    (kbd "% n") 'ibuffer-mark-by-name-regexp
    (kbd "% m") 'ibuffer-mark-by-mode-regexp
    (kbd "% f") 'ibuffer-mark-by-file-name-regexp
    (kbd "% g") 'ibuffer-mark-by-content-regexp
    (kbd "% L") 'ibuffer-mark-by-locked

    (kbd "C-t") 'ibuffer-visit-tags-table

    (kbd "|") 'ibuffer-do-shell-command-pipe
    (kbd "!") 'ibuffer-do-shell-command-file
    (kbd "t") 'ibuffer-toggle-marks
    ;; marked operations
    (kbd "A") 'ibuffer-do-view
    (kbd "D") 'ibuffer-do-delete
    (kbd "E") 'ibuffer-do-eval
    (kbd "F") 'ibuffer-do-shell-command-file
    (kbd "I") 'ibuffer-do-query-replace-regexp
    (kbd "H") 'ibuffer-do-view-other-frame
    (kbd "N") 'ibuffer-do-shell-command-pipe-replace
    (kbd "M") 'ibuffer-do-toggle-modified
    (kbd "O") 'ibuffer-do-occur
    (kbd "P") 'ibuffer-do-print
    (kbd "Q") 'ibuffer-do-query-replace
    (kbd "R") 'ibuffer-do-rename-uniquely
    (kbd "S") 'ibuffer-do-save
    (kbd "T") 'ibuffer-do-toggle-read-only
    (kbd "r") 'ibuffer-do-replace-regexp
    (kbd "V") 'ibuffer-do-revert
    (kbd "W") 'ibuffer-do-view-and-eval

    (kbd "K") 'ibuffer-do-kill-lines
    (kbd "yf") 'ibuffer-copy-filename-as-kill
    (kbd "yb") 'ibuffer-copy-buffername-as-kill

    (kbd "RET") 'ibuffer-visit-buffer
    (kbd "go") 'ibuffer-visit-buffer-other-window
    (kbd "C-o") 'ibuffer-visit-buffer-other-window-noselect
    (kbd "M-o") 'ibuffer-visit-buffer-1-window
    (kbd "gv") 'ibuffer-do-view
    (kbd "gV") 'ibuffer-do-view-horizontally

    ;; Quit
    "q" 'quit-window
    "ZZ" 'quit-window
    "ZQ" 'quit-window))

(provide 'evil-collection-ibuffer)
;;; evil-collection-ibuffer.el ends here
