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

(defvaralias 'tabs-navigation 'centaur-tabs-cycle-scope
  "*Specify the scope of cyclic navigation through tabs.
The following scopes are possible:

- `tabs'
    Navigate through visible tabs only.
- `groups'
    Navigate through tab groups only.
- nil
    Navigate through visible tabs, then through tab groups.")

(defvaralias 'tabs-gray-out-unselected 'centaur-tabs-gray-out-icons)

(defvaralias 'tabs-height 'centaur-tabs-height)

(defvaralias 'tabs-show-icons 'centaur-tabs-set-icons)

(defvaralias 'tabs-set-modified-marker 'centaur-tabs-set-modified-marker)

(defvaralias 'tabs-modified-marker 'centaur-tabs-modified-marker)

(defvaralias 'tabs-show-navigation-buttons 'centaur-tabs-show-navigation-buttons)

(defvaralias 'tabs-style 'centaur-tabs-style
  "Style of tab.
Available values are \"bar\", \"alternate\", \"box\",
\"chamfer\", \"rounded\", \"slant\", \"wave\", \"zigzag\" ")

(defvaralias 'tabs-set-bar 'centaur-tabs-set-bar)

(defcustom tabs-group-by-project t
  "When non-nil, group tabs by projectile project.
Default t. If non-nil calls (tabs-group-by-projectile-project)
Otherwise calls (tabs-group-buffer-groups)"
:type '(boolean)
:group 'tabs)

(defcustom tabs-headline-match t
  "When non-nil, make headline use tabs-default-face. Default t.
Calls (tabs-headline-match)"
:type '(boolean)
:group 'tabs)

(defcustom tabs-auto-hide nil
  "If non-nil hide tabs automatically after TABS-AUTO-HIDE-DELAY seconds."
  :type '(boolean)
  :group 'tabs)

(defcustom tabs-auto-hide-delay 2
  "Tabs auto hide delay in seconds."
  :type '(float)
  :group 'tabs)
