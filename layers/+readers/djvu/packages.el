;;; packages.el --- Djvu Layer Packages File for Spacemacs
;;
;; Copyright (c) 2021-2023 Sylvain Benner & Contributors
;;
;; Author: Daniel Laurens Nicolai <dalanicolai@gmail.com>
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

(defconst djvu-packages
  '(djvu
    tablist
    (djvu3 :location (recipe
                      :fetcher github
                      :repo "dalanicolai/djvu3"))))

(defun djvu/init-djvu ()
  (use-package djvu
    :defer t
    :magic ("%DJVU" . djvu-read-mode)))

(defun djvu/init-tablist ()
  (use-package tablist
    :defer t))

(defun djvu/init-djvu3 ()
  (use-package djvu3
    :after djvu
    :init
    (add-to-list 'spacemacs-large-file-modes-list 'djvu-read-mode t)
    (add-hook 'djvu-read-mode-hook 'spacemacs//djvu-set-imenu-create-index-function)
    (add-hook 'djvu-read-mode-hook 'spacemacs//djvu-set-imenu-goto-function)
    :config
    (advice-add 'djvu-find-file :after #'spacemacs/djvu-advise-image-toggle)
    (evilified-state-evilify-map djvu-read-mode-map
      :mode  djvu-read-mode
      :bindings
      "j" 'spacemacs/djvu-scroll-up-or-next-page
      "k" 'spacemacs/djvu-scroll-down-or-previous-page
      "J" 'djvu-next-page
      "K" 'djvu-prev-page
      "g" 'djvu-goto-page
      "/" 'spacemacs/djvu-fast-search
      "n" 'spacemacs/djvu-re-search-forward-continue
      "H" 'djvu-history-backward
      "L" 'djvu-history-forward
      "c" 'spacemacs/djvu-toggle-semi-continuous-scrolling
      "d" 'djvu-toggle-invert
      "r" 'djvu-revert-buffer
      "q" 'djvu-kill-doc)
    (define-key djvu-image-mode-map "s" 'image-save)
    (spacemacs/declare-prefix-for-mode 'djvu-read-mode "mb" "buffers")
    (spacemacs/set-leader-keys-for-major-mode 'djvu-read-mode
      "s" 'djvu-occur
      "h" 'djvu-keyboard-annot
      "d" 'djvu-toggle-invert
      "i" 'djvu-image-toggle
      "bs" 'djvu-switch-shared
      "bo" 'djvu-switch-outline
      "bt" 'djvu-switch-text
      "ba" 'djvu-switch-annot
      "bb" 'djvu-switch-bookmarks)
    ;; for some reason can not use dolist here
    (define-key djvu-read-mode-map [remap save-buffer] 'djvu-save)
    (define-key djvu-script-mode-map [remap save-buffer] 'djvu-save)
    (define-key djvu-outline-mode-map [remap save-buffer] 'djvu-save)
    (spacemacs/set-leader-keys-for-major-mode 'djvu-script-mode
      "r" 'djvu-switch-read
      "s" 'djvu-switch-shared
      "o" 'djvu-switch-outline
      "t" 'djvu-switch-text
      "a" 'djvu-switch-annot
      "b" 'djvu-switch-bookmarks)
    (evilified-state-evilify-map djvu-outline-mode-map
      :mode  djvu-outline-mode
      :bindings
      "r" 'djvu-revert-buffer
      "q" 'djvu-quit-window)
    (evilified-state-evilify-map djvu-occur-mode-map
      :mode djvu-occur-mode
      :bindings
      (kbd "C-j") 'spacemacs/djvu-occur-next-entry-and-follow
      (kbd "C-k") 'spacemacs/djvu-occur-previous-entry-and-follow
      "r" 'tablist-revert)))
