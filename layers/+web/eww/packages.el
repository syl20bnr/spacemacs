;;; packages.el --- EWW Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Colton Kopsa <coljamkop@gmail.com>
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

(defconst eww-packages
  '(
    (eww :location built-in)
    texfrag))
;; (ace-link :location elpa)
;; (helm-net :location elpa)

(defun eww/init-eww ()
  (use-package eww
    :defer t
    :init
    (spacemacs//eww-setup-transient-state)
    (spacemacs/declare-prefix "awe" "eww")
    (spacemacs/set-leader-keys "awee" 'eww)
    (spacemacs/set-leader-keys "awew" 'eww-switch-to-buffer)
    :config
    (progn
      (define-key eww-link-keymap "f" 'eww-follow-link)
      (define-key eww-link-keymap "F" (lambda () (interactive) (eww-follow-link 2)))
      (let ((mode 'eww-mode))
        (spacemacs/declare-prefix-for-mode mode "mv" "view")
        (spacemacs/declare-prefix-for-mode mode "ml" "list")
        (spacemacs/set-leader-keys-for-major-mode mode
          "s" 'helm-google-suggest
          "S" 'browse-web
          "t" 'spacemacs/eww-render-latex
          "r" 'eww-reload
          "p" 'eww-previous-url
          "n" 'eww-next-url
          "h" 'eww-list-histories
          "d" 'eww-download
          "a" 'eww-add-bookmark
          "lb" 'eww-list-buffers
          "lo" 'eww-list-bookmarks
          "vx" 'eww-browse-with-external-browser
          "vf" 'eww-toggle-fonts
          "vr" 'eww-readable
          "vs" 'eww-view-source)
        (evil-define-key 'normal eww-mode-map
          (kbd "C-o") 'eww-back-url
          (kbd "C-i") 'eww-forward-url
          "<" 'eww-back-url
          ">" 'eww-forward-url
          "[" 'eww-previous-url
          "]" 'eww-next-url
          "L" 'spacemacs/eww-jump-next-buffer
          "H" 'spacemacs/eww-jump-previous-buffer
          (kbd "C-j") 'shr-next-link
          (kbd "C-k") 'shr-previous-link
          "o" 'ace-link-eww
          "+" 'zoom-frm-in
          "-" 'zoom-frm-out
          "=" 'zoom-frm-unzoom))

      (let ((mode 'eww-history-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "f" 'eww-history-browse)
        (evil-define-key 'normal eww-history-mode-map "f" 'eww-history-browse
          "q" 'quit-window))

      (let ((mode 'eww-bookmark-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "d" 'eww-bookmark-kill
          "y" 'eww-bookmark-yank
          "f" 'eww-bookmark-browse)
        (evil-define-key 'normal eww-bookmark-mode-map
          "q" 'quit-window
          "f" 'eww-bookmark-browse
          "d" 'eww-bookmark-kill
          "y" 'eww-bookmark-yank))

      (let ((mode 'eww-buffers-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "f" 'eww-buffer-select
          "d" 'eww-buffer-kill
          "n" 'eww-buffer-show-next
          "p" 'eww-buffer-show-previous)
        (evil-define-key 'normal eww-buffers-mode-map
          "q" 'quit-window
          "f" 'eww-buffer-select
          "d" 'eww-buffer-kill
          "n" 'eww-buffer-show-next
          "p" 'eww-buffer-show-previous)))))

(defun eww/init-texfrag ()
  (use-package texfrag
    :defer t))
