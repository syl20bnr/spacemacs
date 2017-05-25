;;; config.el --- Spacemacs Layouts Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar spacemacs-layouts-directory
  (expand-file-name (concat spacemacs-cache-directory "layouts/"))
  "Save layouts in this directory.")

(defvar layouts-enable-autosave nil
  "If true, saves perspectives to file per `layouts-autosave-delay'")

(defvar layouts-autosave-delay 900
  "Delay in seconds between each layouts auto-save.")

(defvar spacemacs--ts-full-hint-toggle 0
  "Toggle display of transient states documentations.")

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

