;;; config.el --- Spacemacs Layouts Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; Variables

(defvar spacemacs-layouts-directory
  (expand-file-name (concat spacemacs-cache-directory "layouts/"))
  "Save layouts in this directory.")

(defvar layouts-enable-autosave nil
  "If true, saves perspectives to file per `layouts-autosave-delay'")

(defvar layouts-autosave-delay 900
  "Delay in seconds between each layouts auto-save.")

(defvar spacemacs--layouts-ts-full-hint-toggle nil
  "Toggle display of layouts transient-state documentation.")

(defvar spacemacs--workspaces-ts-full-hint-toggle nil
  "Toggle display of workspaces transient-state documentation.")

(defvar spacemacs--last-selected-layout dotspacemacs-default-layout-name
  "Previously selected layout.")

(defvar spacemacs--custom-layout-alist nil
  "List of custom layouts with their bound keys.
 Do not modify directly, use provided `spacemacs|define-custom-layout'")

(defvar spacemacs--layouts-autosave-timer nil
  "Timer for layouts auto-save.")

(defvar spacemacs-generic-layout-names
  '(("zebra" "zucchini" "zen" "yellow" "yeti" "yard") ; grab-bag
    ("baboon" "banana" "blue")                        ; 2nd layout
    ("crab" "cabbage" "crayon")                       ; 3rd
    ("deer" "doughnut" "door")                        ; 4th
    ("elephant" "eggplant" "extreme")                 ; 5th
    ("falcon" "fig" "fjord")                          ; 6th
    ("gnu" "garlic" "guardian")                       ; 7th
    ("horse" "honey" "hallelujah")                    ; 8th
    ("iguana" "ice-cream" "internet")                 ; 9th
    ("jellyfish" "jalapeno" "jolt"))                  ; 10th (aka 0th)
  "Names for auto-generated layout names.
Used by `spacemacs//generate-layout-name'.

Must be a list with 10 entries, where each entry is a list of
names. The 2nd list contains possible names for the 2nd
layout (or 10th) layout, the 3rd list contains names for the 3rd
layout, the 4th for the 4th, and so on until the 10th (aka layout
number 0). The first list is sepcial - it is a grab-bag for names
in case none of the regular names can be used for a new layout.")

(defvar spacemacs-layouts-restricted-functions
  '(spacemacs/window-split-double-columns
    spacemacs/window-split-triple-columns
    spacemacs/window-split-grid)
  "List of functions to be wrapped by `with-persp-buffer-list'")

(defvar spacemacs-layouts-restrict-spc-tab nil
  "If `t' then `SPC-TAB' will be limited to the current layout's buffers.")

(defvar layouts-enable-local-variables t
  "Allow variables to be specified as layout-local (value local to a particular layout).")

(defvar spacemacs--layout-local-variables nil
  "List of variables that will be local to the current layout.")

(defvar spacemacs--layout-local-map (spacemacs-ht-create)
  "Map of layouts to their local variable values.")
