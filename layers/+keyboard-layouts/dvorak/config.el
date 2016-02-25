;;; config.el --- dvorak Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Alejandro Catalina <alecatfel@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This functions have been taken from bepo layer, courtesy of
;; Fabien Dubosson <fabien.dubosson@gmail.com>>

;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;------------------------------------------------------------------------------
;; PUBLIC VARIABLES
;;------------------------------------------------------------------------------

(defvar dvorak-set-disabled-configurations nil
  "If non nil, `dvorak' will disable configurations only for the
  passed list of symbols.")

(defvar dvorak-set-enabled-configurations nil
  "If non nil, `dvorak' will enable configurations only for the passed list of
symbols. Configurations that are also in `dvorak-set-disabled-configurations' will
not be loaded.")

;;------------------------------------------------------------------------------
;; PRIVATE VARIABLES
;;------------------------------------------------------------------------------

(defvar dvorak--base-rebinding-map
  '(("h" . "h")
    ("t" . "j")
    ("n" . "k")
    ("s" . "l")
    ;;
    ("h" . "h")
    ("j" . "t")
    ("k" . "n")
    ("l" . "s"))
  "The base dvorak's rebinding map. Dots should be read as `will
  behave as'. It should be a bidirectional mapping, i.e. all
  present keys should be once in each column.")

(defvar dvorak--rebinding-map
  (mapcan (lambda (binding)
            (let ((key1 (car binding))
                  (key2 (cdr binding)))
              (append
               (list  (cons (upcase key1) (upcase key2))
                      (cons key1 key2))
               (mapcar
                (lambda (modifier)
                  (cons (concat modifier key1) (concat modifier key2)))
                '("" "C-" "M-" "C-S-")))))
          dvorak--base-rebinding-map)
  "The full dvorak's rebinding map. Dots should be read as `will behave as'.")

(defvar dvorak--all-evil-states
  (list evil-normal-state-map
        evil-visual-state-map
        evil-insert-state-map
        evil-emacs-state-map
        evil-motion-state-map)
  "The list of all evil states.")

(defvar dvorak--all-evil-states-but-insert
  (list evil-normal-state-map
        evil-visual-state-map
        evil-motion-state-map)
  "The list of all evil states except insert.")
