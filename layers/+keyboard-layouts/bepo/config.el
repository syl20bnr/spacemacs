;;; config.el --- bepo Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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

(defvar bepo-set-enabled-configurations nil
  "If non nil, `bepo' will enable configurations only for the passed list of
symbols. Configurations that are also in `bepo-set-disabled-configurations' will
not be loaded.")

(defvar bepo-set-disabled-configurations nil
  "If non nil, `bepo' will disable configurations for the passed list of
symbols. This list takes priority over `bepo-set-enabled-configurations', so
they will not be loaded in any case.")

;;------------------------------------------------------------------------------
;; PRIVATE VARIABLES
;;------------------------------------------------------------------------------

(defvar bepo--base-rebinding-map
  '(("c" . "h")
    ("t" . "j")
    ("s" . "k")
    ("r" . "l")
    ;;
    ("h" . "r")
    ("j" . "t")
    ("k" . "s")
    ("l" . "c"))
  "The base bepo's rebinding map. Dots should be read as `will
  behave as'. It should be a bidirectional mapping, i.e. all
  present keys should be once in each column.")

(defvar bepo--rebinding-map
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
          bepo--base-rebinding-map)
  "The full bepo's rebinding map. Dots should be read as `will behave as'.")

(defvar bepo--all-evil-states
  (list evil-normal-state-map
        evil-visual-state-map
        evil-insert-state-map
        evil-emacs-state-map
        evil-motion-state-map)
  "The list of all evil states.")

(defvar bepo--all-evil-states-but-insert
  (remove evil-insert-state-map bepo--all-evil-states)
  "The list of all evil states except insert.")
