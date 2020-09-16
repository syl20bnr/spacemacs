;;; packages.el --- PDF Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner
;; Copyright (c) 2020-2020 Sylvain Benner & Contributors
;;
;; Author: André Peric Tavares <andre.peric.tavares@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq pdf-packages '(pdf-tools))

(defun pdf/init-pdf-tools ()
  (use-package pdf-tools
    :defer t
    :mode (("\\.pdf\\'" . pdf-view-mode))
    :config
    (progn
      (pdf-tools-install)

      (spacemacs|define-transient-state pdf-tools
        :title "PDF-tools Transient State"
        :on-enter (setq which-key-inhibit t)
        :on-exit (setq which-key-inhibit nil)
        :evil-leader-for-mode (pdf-view-mode . ".")
        :doc
        "
 Navigation^^^^                Scale/Fit^^                    Annotations^^       Actions^^           Other^^
 ----------^^^^--------------- ---------^^------------------  -----------^^------ -------^^---------- -----^^---
 [_j_/_k_] scroll down/up      [_W_] fit to width             [_al_] list         [_s_] search         [_q_] quit
 [_h_/_l_] scroll left/right   [_H_] fit to height            [_at_] text         [_O_] outline
 [_d_/_u_] pg down/up          [_P_] fit to page              [_aD_] delete       [_p_] print
 [_J_/_K_] next/prev pg        [_m_] slice using mouse        [_am_] markup       [_o_] open link
 [_0_/_$_] full scroll l/r     [_b_] slice from bounding box  ^^                  [_r_] revert
 [_[_/_]_] history back/for    [_R_] reset slice              ^^                  [_t_] attachments
 ^^^^                          [_zr_] reset zoom              ^^                  [_n_] night mode
 "
        :bindings
        ;; Navigation
        ("j"  pdf-view-next-line-or-next-page)
        ("k"  pdf-view-previous-line-or-previous-page)
        ("l"  image-forward-hscroll)
        ("h"  image-backward-hscroll)
        ("J"  pdf-view-next-page)
        ("K"  pdf-view-previous-page)
        ("u"  pdf-view-scroll-down-or-previous-page)
        ("d"  pdf-view-scroll-up-or-next-page)
        ("0"  image-bol)
        ("$"  image-eol)
        ("["  pdf-history-backward)
        ("]"  pdf-history-forward)
        ;; Scale/Fit
        ("W"  pdf-view-fit-width-to-window)
        ("H"  pdf-view-fit-height-to-window)
        ("P"  pdf-view-fit-page-to-window)
        ("m"  pdf-view-set-slice-using-mouse)
        ("b"  pdf-view-set-slice-from-bounding-box)
        ("R"  pdf-view-reset-slice)
        ("zr" pdf-view-scale-reset)
        ;; Annotations
        ("aD" pdf-annot-delete)
        ("at" pdf-annot-attachment-dired :exit t)
        ("al" pdf-annot-list-annotations :exit t)
        ("am" pdf-annot-add-markup-annotation)
        ;; Actions
        ("s" pdf-occur :exit t)
        ("O" pdf-outline :exit t)
        ("p" pdf-misc-print-document :exit t)
        ("o" pdf-links-action-perform :exit t)
        ("r" pdf-view-revert-buffer)
        ("t" pdf-annot-attachment-dired :exit t)
        ("n" pdf-view-midnight-minor-mode)
        ;; Other
        ("q" nil :exit t))

      (spacemacs/declare-prefix-for-mode 'pdf-view-mode "ma" "annotations")
      (spacemacs/declare-prefix-for-mode 'pdf-view-mode "mf" "fit")
      (spacemacs/declare-prefix-for-mode 'pdf-view-mode "ms" "slice/search")
      (spacemacs/set-leader-keys-for-major-mode 'pdf-view-mode
        ;; Slicing image
        "sm" 'pdf-view-set-slice-using-mouse
        "sb" 'pdf-view-set-slice-from-bounding-box
        "sr" 'pdf-view-reset-slice
        ;; Annotations
        "aD" 	'pdf-annot-delete
        "at" 	'pdf-annot-attachment-dired
        "ah" 	'pdf-annot-add-highlight-markup-annotation
        "al" 	'pdf-annot-list-annotations
        "am" 	'pdf-annot-add-markup-annotation
        "ao" 	'pdf-annot-add-strikeout-markup-annotation
        "as" 	'pdf-annot-add-squiggly-markup-annotation
        "at" 	'pdf-annot-add-text-annotation
        "au" 	'pdf-annot-add-underline-markup-annotation
        ;; Fit image to window
        "fw" 'pdf-view-fit-width-to-window
        "fh" 'pdf-view-fit-height-to-window
        "fp" 'pdf-view-fit-page-to-window
        ;; Other
        "ss" 'pdf-occur
        "p" 'pdf-misc-print-document
        "O" 'pdf-outline
        "n" 'pdf-view-midnight-minor-mode)

      (evil-define-key 'visual pdf-view-mode-map "y" 'pdf-view-kill-ring-save)

      ;; TODO: Make `/', `?' and `n' work like in Evil
      (evilified-state-evilify pdf-view-mode pdf-view-mode-map
        ;; Navigation
        "0"  'image-bol
        "$"  'image-eol
        "j"  'pdf-view-next-line-or-next-page
        "k"  'pdf-view-previous-line-or-previous-page
        "l"  'image-forward-hscroll
        "h"  'image-backward-hscroll
        "J"  'pdf-view-next-page
        "K"  'pdf-view-previous-page
        "gg"  'pdf-view-first-page
        "G"  'pdf-view-last-page
        "gt"  'pdf-view-goto-page
        "gl"  'pdf-view-goto-label
        "u" 'pdf-view-scroll-down-or-previous-page
        "d" 'pdf-view-scroll-up-or-next-page
        (kbd "C-u") 'pdf-view-scroll-down-or-previous-page
        (kbd "C-d") 'pdf-view-scroll-up-or-next-page
        (kbd "``")  'pdf-history-backward
        "["  'pdf-history-backward
        "]"  'pdf-history-forward
        "'" 'pdf-view-jump-to-register
        ;; Search
        "/" 'isearch-forward
        "?" 'isearch-backward
        ;; Actions
        "r"   'pdf-view-revert-buffer
        "o"   'pdf-links-action-perform
        "O"   'pdf-outline
        "zr"  'pdf-view-scale-reset)
      (evilified-state-evilify pdf-outline-buffer-mode pdf-outline-buffer-mode-map
        "-"                'negative-argument
        "j"                'next-line
        "k"                'previous-line
        "gk"               'outline-backward-same-level
        "gj"               'outline-forward-same-level
        (kbd "<backtab>")  'show-all
        "gh"               'pdf-outline-up-heading
        "gg"               'beginning-of-buffer
        "G"                'pdf-outline-end-of-buffer
        "TAB"              'outline-toggle-children
        "RET"              'pdf-outline-follow-link
        (kbd "M-RET")      'pdf-outline-follow-link-and-quit
        "f"                'pdf-outline-display-link
        [mouse-1]          'pdf-outline-mouse-display-link
        "o"                'pdf-outline-select-pdf-window
        "``"               'pdf-outline-move-to-current-page
        "''"               'pdf-outline-move-to-current-page
        "Q"                'pdf-outline-quit-and-kill
        "q"                'quit-window
        "F"                'pdf-outline-follow-mode)
      (evilified-state-evilify pdf-annot-list-mode pdf-annot-list-mode-map
        "f"                'pdf-annot-list-display-annotation-from-id
        "d"                'tablist-flag-forward
        "x"                'tablist-do-flagged-delete
        "u"                'tablist-unmark-forward
        "q"                'tablist-quit)
      (evilified-state-evilify pdf-occur-buffer-mode pdf-occur-buffer-mode-map
        "q"              'tablist-quit
        "g"              'pdf-occur-revert-buffer-with-args
        "r"              'pdf-occur-revert-buffer-with-args
        "*"              'spacemacs/enter-ahs-forward
        "?"              'evil-search-backward))))
