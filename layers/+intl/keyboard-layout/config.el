;;; config.el --- keyboard-layout Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Fabien Dubosson <fabien.dubosson@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;------------------------------------------------------------------------------
;; PUBLIC VARIABLES
;;------------------------------------------------------------------------------

(defvar kl-layout 'dvorak
  "The keyboard-layout to use. Possible values are `dvorak' and `bepo'.")

(defvar kl-enabled-configurations nil
  "If non nil, `keyboard-layout' will enable configurations only
for the passed list of symbols. Configurations that are also in
`kl-disabled-configurations' will not be loaded.")

(defvar kl-disabled-configurations nil
  "If non nil, `keyboard-layout' will disable configurations for
the passed list of symbols. This list takes priority over
`kl-enabled-configurations', so they will not be loaded in
any case.")

;;------------------------------------------------------------------------------
;; PRIVATE VARIABLES
;;------------------------------------------------------------------------------

(defvar kl--base-rebinding-maps
  '((bepo . (("c" . "h")
             ("t" . "j")
             ("s" . "k")
             ("r" . "l")
             ;;
             ("h" . "r")
             ("j" . "t")
             ("k" . "s")
             ("l" . "c")))
    (dvorak . (("h" . "h")
               ("t" . "j")
               ("n" . "k")
               ("s" . "l")
               ;;
               ("h" . "h")
               ("j" . "t")
               ("k" . "n")
               ("l" . "s"))))
  "The base rebinding map. Dots should be read as `will behave
  as'. It should be a bidirectional mapping, i.e. all present
  keys should be once in each column.")

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
