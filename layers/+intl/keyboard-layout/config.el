;;; config.el --- keyboard-layout Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Fabien Dubosson <fabien.dubosson@gmail.com>
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


;;------------------------------------------------------------------------------
;; PUBLIC VARIABLES
;;------------------------------------------------------------------------------

(defvar kl-layout 'dvorak
  "The keyboard-layout to use. Possible values are `bepo', `dvp',
`dvorak', `workman', `neo', `colemak-neio-literal', `colemak-neio-inverted',
 `colemak-hnei' and `colemak-jkhl'.")

(defvar kl-enabled-configurations nil
  "If non nil, `keyboard-layout' will enable configurations only
for the passed list of symbols. Configurations that are also in
`kl-disabled-configurations' will not be loaded.")

(defvar kl-disabled-configurations nil
  "If non nil, `keyboard-layout' will disable configurations for
the passed list of symbols. This list takes priority over
`kl-enabled-configurations', so they will not be loaded in any
case.")

;;------------------------------------------------------------------------------
;; PRIVATE VARIABLES
;;------------------------------------------------------------------------------

(defvar kl--base-rebinding-maps
  '((bepo
     . (("c" . "h")
        ("t" . "j")
        ("s" . "k")
        ("r" . "l")
        ;;
        ("h" . "r")
        ("j" . "t")
        ("k" . "s")
        ("l" . "c")))
    (dvp
     . (("d" . "h")
        ("h" . "j")
        ("t" . "k")
        ("n" . "l")
        ;;
        ("j" . "d")
        ("k" . "t")
        ("l" . "n")))
    (dvorak
     . (("h" . "h")
        ("t" . "j")
        ("n" . "k")
        ("s" . "l")
        ;;
        ("h" . "h")
        ("j" . "t")
        ("k" . "n")
        ("l" . "s")))
    (workman
     . (("y" . "h")
        ("n" . "j")
        ("e" . "k")
        ("o" . "l")
        ;;
        ("h" . "y")
        ("j" . "n")
        ("k" . "e")
        ("l" . "o")))
    (neo
     . (("s" . "h")
        ("n" . "j")
        ("r" . "k")
        ("t" . "l")
        ;;
        ("l" . "s")
        ("j" . "n")
        ("h" . "r")
        ("k" . "t")))
    (colemak-neio-literal
     . (("n" . "h")
        ("e" . "j")
        ("i" . "k")
        ("o" . "l")
        ;;
        ("h" . "n")
        ("j" . "e")
        ("k" . "i")   ;;
        ("l" . "o"))) ;; easier access to "New Line Mode"
    (colemak-neio-inverted
     . (("n" . "h")
        ("e" . "j")
        ("i" . "k")
        ("o" . "l")
        ;;
        ("h" . "n")
        ("j" . "e")
        ("l" . "i")   ;; easier access to "Insert Mode"
        ("k" . "o"))) ;;
    (colemak-hnei
     . (("h" . "h")
        ("n" . "j")
        ("e" . "k")
        ("i" . "l")
        ;;
        ("h" . "h")
        ("j" . "n")
        ("k" . "e")
        ("l" . "i")))
    (colemak-jkhl
     . (("j" . "h")
        ("k" . "j")
        ("h" . "k")
        ("l" . "l")
        ;;
        ("h" . "j")
        ("j" . "k")
        ("k" . "h")
        ("l" . "l"))))
  "The base rebinding map. Dots should be read as `will behave
as'. It should be a bidirectional mapping, i.e. all present keys
should be once in each column.")

(defvar kl--rebinding-maps
  (mapcar (lambda (map) `(,(car map) . ,(kl//generate-full-rebinding-map (cdr map))))
          kl--base-rebinding-maps)
  "The full rebinding map. Dots should be read as `will behave as'.")

(with-eval-after-load 'evil
  (defvar kl--all-evil-states
    (list evil-normal-state-map
          evil-visual-state-map
          evil-insert-state-map
          evil-emacs-state-map
          evil-motion-state-map)
    "The list of all evil states.")

  (defvar kl--all-evil-states-but-insert
    (list evil-normal-state-map
          evil-visual-state-map
          evil-motion-state-map)
    "The list of all evil states except insert."))
