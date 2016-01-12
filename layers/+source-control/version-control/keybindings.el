;;; keybindings.el --- Version control keybindings
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar version-control--ms-doc-toggle 0
  "Display a short doc when nil, full doc otherwise.")

(defvar version-control--ms-documentation-long
  "
  [?]          toggle this help
  [t]          toggle diff signs in margin
Hunk Commands
  [n]          move to next hunk
  [N] [p]      move to previous hunk
  [r]          revert hunk
  [s]          stage hunk
  [h]          show hunk
Magit Commands
  [w]          stage changes in current file
  [u]          unstage changes in current file
  [c]          commit with popup
  [C]          direct commit
  [D]          diff popup
  [P]          push popup
  [f]          fetch popup
  [F]          pull popup
  [l]          log popup")

(defvar version-control--ms-documentation-short
  "[n] next [N] [p] previous [r] revert [s] stage [h] show-hunk [w] stage-file [u] unstage-file [t] toggle margin [q] quit")

(defun version-control//ms-doc ()
  "Return the docstring for the layouts micro-state."
  (if (equal 1 version-control--ms-doc-toggle)
      version-control--ms-documentation-long
    version-control--ms-documentation-short))

(spacemacs|define-micro-state vcs
  :doc (version-control//ms-doc)
  :disable-evil-leader t
  :use-minibuffer t
  :evil-leader "g."
  :on-enter (version-control/enable-margin)
  :bindings
  ("?" version-control/show-help)
  ("C" magit-commit :exit t)
  ("d" magit-ediff-popup :exit t)
  ("D" magit-diff-unstaged :exit t)
  ("F" magit-pull-popup :exit t)
  ("P" magit-push-popup :exit t)
  ("c" magit-commit-popup :exit t)
  ("f" magit-fetch-popup :exit t)
  ("l" magit-log-popup :exit t)
  ("u" magit-unstage-file)
  ("w" magit-stage-file)
  ("n" version-control/next-hunk)
  ("N" version-control/previous-hunk)
  ("p" version-control/previous-hunk)
  ("r" version-control/revert-hunk)
  ("s" version-control/stage-hunk)
  ("h" version-control/show-hunk)
  ("t" spacemacs/toggle-version-control-margin)
  ("q" nil :exit t))

