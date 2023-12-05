;;; packages.el --- PDF Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner
;; Copyright (c) 2020-2023 Sylvain Benner & Contributors
;;
;; Author: André Peric Tavares <andre.peric.tavares@gmail.com>
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


(setq pdf-packages '(pdf-tools
                     pdf-view-restore))

(defun pdf/init-pdf-tools ()
  (use-package pdf-tools
    :defer t
    :mode (("\\.pdf\\'" . pdf-view-mode))
    :init
    (spacemacs//pdf-tools-setup-transient-state)
    :config
    (pdf-tools-install)

    (spacemacs/declare-prefix-for-mode 'pdf-view-mode "ma" "annotations")
    (spacemacs/declare-prefix-for-mode 'pdf-view-mode "mf" "fit")
    (spacemacs/declare-prefix-for-mode 'pdf-view-mode "ms" "slice/search")
    (spacemacs/set-leader-keys-for-major-mode 'pdf-view-mode
      ;; Slicing image
      "sm" 'pdf-view-set-slice-using-mouse
      "sb" 'pdf-view-set-slice-from-bounding-box
      "sr" 'pdf-view-reset-slice
      ;; Annotations
      "aD"  'pdf-annot-delete
      "at"  'pdf-annot-attachment-dired
      "ah"  'pdf-annot-add-highlight-markup-annotation
      "al"  'pdf-annot-list-annotations
      "am"  'pdf-annot-add-markup-annotation
      "ao"  'pdf-annot-add-strikeout-markup-annotation
      "as"  'pdf-annot-add-squiggly-markup-annotation
      "at"  'pdf-annot-add-text-annotation
      "au"  'pdf-annot-add-underline-markup-annotation
      ;; Fit image to window
      "fw" 'pdf-view-fit-width-to-window
      "fh" 'pdf-view-fit-height-to-window
      "fp" 'pdf-view-fit-page-to-window
      ;; Other
      "ss" 'pdf-occur
      "p" 'pdf-misc-print-document
      "O" 'pdf-outline
      "n" 'pdf-view-midnight-minor-mode)

    (evil-define-key 'visual pdf-view-mode-map
      "y" 'pdf-view-kill-ring-save
      (kbd "<C-down-mouse-1>") 'pdf-view-mouse-extend-region
      (kbd "<M-down-mouse-1>") 'pdf-view-mouse-set-region-rectangle
      (kbd "<down-mouse-1>")  'pdf-view-mouse-set-region)

    ;; TODO: Make `/', `?' and `n' work like in Evil
    (evilified-state-evilify-map pdf-view-mode-map
      :mode  pdf-view-mode
      :eval-after-load pdf-view
      :bindings
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

    (evilified-state-evilify-map pdf-outline-buffer-mode-map
      :mode  pdf-outline-buffer-mode
      :eval-after-load pdf-outline
      :bindings
      "-"                'negative-argument
      "j"                'next-line
      "k"                'previous-line
      "gk"               'outline-backward-same-level
      "gj"               'outline-forward-same-level
      (kbd "<backtab>")  (if (version< emacs-version "28.0")
                             'outline-show-all
                           'outline-cycle-buffer)
      "gh"               'pdf-outline-up-heading
      "gg"               'beginning-of-buffer
      "G"                'pdf-outline-end-of-buffer
      (kbd "<tab>")      'outline-toggle-children
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
    (evilified-state-evilify-map pdf-annot-list-mode-map
      :mode  pdf-annot-list-mode
      :eval-after-load pdf-annot
      :bindings
      "f"                'pdf-annot-list-display-annotation-from-id
      "d"                'tablist-flag-forward
      "x"                'tablist-do-flagged-delete
      "u"                'tablist-unmark-forward
      "q"                'tablist-quit)
    (evilified-state-evilify-map pdf-occur-buffer-mode-map
      :mode  pdf-occur-buffer-mode
      :eval-after-load pdf-occur
      :bindings
      "q"              'tablist-quit
      "g"              'pdf-occur-revert-buffer-with-args
      "r"              'pdf-occur-revert-buffer-with-args
      "*"              'spacemacs/enter-ahs-forward
      "?"              'evil-search-backward)
    (spacemacs/declare-prefix-for-mode 'pdf-occur-buffer-mode "mt" "toggles")
    (spacemacs/set-leader-keys-for-major-mode 'pdf-occur-buffer-mode
      "tf" 'next-error-follow-minor-mode)))

(defun pdf/init-pdf-view-restore ()
  (use-package pdf-view-restore
    :after pdf-tools
    :defer t
    :init
    (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)))
