;;; packages.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst emacs-info-packages
  '((info :location built-in)))

(defun emacs-info/init-info ()
  (use-package info
    :defer t
    :init
    (progn
      (spacemacs|define-transient-state emacs-info
        :title "Emacs Info transient state"
        :foreign-keys run
        :doc
"
 [_q_] quit "
        :bindings
        ("n" Info-next)
        ("p" Info-prev)
        ("e" Info-scroll-up)
        ("y" Info-scroll-down)
        ("<tab>" Info-next-reference)
        ("S-<tab>" Info-prev-reference)
        ("h" Info-help)
        ("l" Info-history-back)
        ("r" Info-history-forward)
        ("d" Info-directory)
        ("u" Info-up)
        ("L" Info-history)
        ("s" Info-search)
        ("S" Info-search-case-sensitively)
        ("i" Info-index)
        ("I" Info-virtual-index)
        ("a" info-apropos)
        ("m" Info-menu)
        ("gd" Info-goto-node)
        ("gm" Info-menu)
        ("gt" Info-top-node)
        ("gT" Info-toc)
        ("gf" Info-follow-reference)
        ("gj" Info-next)
        ("gk" Info-prev)

        ("q" nil :exit t)
        :config
        (evilified-state-evilify-map Info-mode-map
          :mode Info-mode
          :bindings
          (kbd "M-SPC" 'spacemacs/emacs-info-transient-state/body)))
      (define-key Info-mode-map (kbd "M-SPC") 'spacemacs/emacs-info-transient-state/body))))
