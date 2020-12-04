;;; config.el --- Space-macs Layouts Layer configuration File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; Variables

(defvar space-macs-layouts-directory
  (expand-file-name (concat space-macs-cache-directory "layouts/"))
  "Save layouts in this directory.")

(defvar layouts-enable-autosave nil
  "If true, saves perspectives to file per `layouts-autosave-delay'")

(defvar layouts-autosave-delay 900
  "Delay in seconds between each layouts auto-save.")

(defvar space-macs--layouts-ts-full-hint-toggle nil
  "Toggle display of layouts transient-state documentation.")

(defvar space-macs--workspaces-ts-full-hint-toggle nil
  "Toggle display of workspaces transient-state documentation.")

(defvar space-macs--last-selected-layout dotspace-macs-default-layout-name
  "Previously selected layout.")

(defvar space-macs--custom-layout-alist nil
  "List of custom layouts with their bound keys.
 Do not modify directly, use provided `space-macs|define-custom-layout'")

(defvar space-macs--layouts-autosave-timer nil
  "Timer for layouts auto-save.")

(defvar space-macs-generic-layout-names
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
Used by `space-macs//generate-layout-name'.

Must be a list with 10 entries, where each entry is a list of
names. The 2nd list contains possible names for the 2nd
layout (or 10th) layout, the 3rd list contains names for the 3rd
layout, the 4th for the 4th, and so on until the 10th (aka layout
number 0). The first list is sepcial - it is a grab-bag for names
in case none of the regular names can be used for a new layout.")

(defvar space-macs-layouts-restricted-functions
  '(space-macs/window-split-double-columns
    space-macs/window-split-triple-columns
    space-macs/window-split-grid)
  "List of functions to be wrapped by `with-persp-buffer-list'")

(defvar space-macs-layouts-restrict-spc-tab nil
  "If `t' then `SPC-TAB' will be limited to the current layout's buffers.")

(defvar layouts-enable-local-variables t
  "Allow variables to be specified as layout-local (value local to a particular layout).")

(defvar space-macs--layout-local-variables nil
  "List of variables that will be local to the current layout.")

(defvar space-macs--layout-local-map (ht-create)
  "Map of layouts to their local variable values.")


