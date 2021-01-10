;;; config.el --- tabs configuration File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar tabs-navigation nil
  "*Specify the scope of cyclic navigation through tabs.
The following scopes are possible:

- `tabs'
    Navigate through visible tabs only.
- `groups'
    Navigate through tab groups only.
- nil
    Navigate through visible tabs, then through tab groups.")

(defvar tabs-gray-out-unselected nil
  "When non nil, enable gray icons for unselected buffer.")

(defvar tabs-height 22
  "The height of tab")

(defvar tabs-show-icons t
  "When non nil, show icon from all-the-icons")

(defvar tabs-set-modified-marker t
  "When non nil, display a marker when buffer is modified")

(defvar tabs-modified-marker "⚠"
  "Display appearance of modified marker if enabled")

(defvar tabs-show-navigation-buttons nil
  "When non-nil, show buttons for backward/forward tabs")

(defvar tabs-style "bar"
  "Style of tab; available values are \"bar\", \"alternate\", \"box\", \"chamfer\", \"rounded\", \"slant\", \"wave\", \"zigzag\" ")

(defvar tabs-group-by-project t
  "When non-nil, group tabs by projectile project. Default t.
   If non-nil calls (centaur-tabs-group-by-projectile-project)
   Otherwise calls (centaur-tabs-group-buffer-groups)")

(defvar tabs-headline-match t
  "When non-nil, make headline use centaur-tabs-default-face. Default t. Calls (centaur-tabs-headline-match)")

(defvar tabs-set-bar 'left
  "When non-nil, display a bar to show currently selected tab.
  There are three options:
  - 'left: displays the bar at the left of the currently selected tab.
  - 'under: displays the bar under the currently selected tab.
  - 'over: displays the bar over the currently selected tab.")
