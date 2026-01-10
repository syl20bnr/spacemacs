;;; packages.el --- dirvish Layer packages File for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2026 Sylvain Benner & Contributors
;;
;; Author: Jaehyun Yeom
;; URL: https://github.com/jaeyeom
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


(defconst dirvish-packages
  '(
    all-the-icons
    (dired :location built-in)
    dirvish
    golden-ratio
    nerd-icons))

(defun dirvish//set-leader-keys ()
  "Set leader key bindings for dirvish.
Uses same prefix as ranger (atr) so bindings seamlessly replace ranger
when dirvish shadows it.  Note: SPC j d and SPC j D are already bound to
`dired-jump' and `dired-jump-other-window' in spacemacs-defaults, which
will use dirvish when `dirvish-override-dired-mode' is active."
  (spacemacs/declare-prefix "atr" "dirvish")
  (spacemacs/set-leader-keys
    "atrr" 'dirvish
    "atrd" 'dirvish-dwim
    "atrs" 'dirvish-side
    "atrq" 'dirvish-quick-access
    "atrf" 'dirvish-fd))

(defun dirvish/init-dirvish ()
  (use-package dirvish
    :commands (dirvish dirvish-side dirvish-dwim dirvish-quick-access
                       dirvish-fd dirvish-override-dired-mode)
    :init
    (dirvish//set-leader-keys)
    ;; Enable dirvish globally - required for dirvish to work properly
    (dirvish-override-dired-mode)
    :config
    ;; Add icons attribute based on dotspacemacs-default-icons-font
    ;; We don't override dirvish-attributes or dirvish-mode-line-format
    ;; to respect user customizations via M-x customize
    (when-let ((icons-font dotspacemacs-default-icons-font))
      (eval-after-load icons-font
        `(add-to-list 'dirvish-attributes ',icons-font)))))

(defun dirvish/post-init-dired ()
  "Configure dired with dirvish keybindings."
  ;; Enable dired-omit-mode if configured (before use-package dired)
  (when dirvish-enable-dired-omit
    (add-hook 'dired-mode-hook #'dired-omit-mode))

  (use-package dired
    :defer t
    :config
    ;; Be sure to override dired bindings
    (dirvish//set-leader-keys)

    ;; Evilified dired keybindings for dirvish
    (evilified-state-evilify-map dired-mode-map
      :mode dired-mode
      :bindings
      ;; Navigation
      "h"         'dired-up-directory
      "l"         'dirvish/dired-find-file-smart
      "gr"        'revert-buffer

      ;; Dirvish-specific
      "q"         'dirvish-quit
      "/"         'dirvish-narrow
      (kbd "TAB") 'dirvish-subtree-toggle
      "f"         'dirvish-layout-toggle
      "gf"        'dirvish-layout-toggle
      "gt"        'dirvish-layout-switch
      "gd"        'dirvish-dispatch
      "gl"        'dirvish-ls-switches-menu

      ;; Dired enhancements
      "i"         'dired-toggle-read-only
      "I"         'dired-maybe-insert-subdir
      "g$"        'dired-hide-subdir
      "g?"        'dired-summary
      "gj"        'dired-next-dirline
      "gk"        'dired-prev-dirline
      "gG"        'dired-do-chgrp
      "gO"        'dired-find-file-other-window
      (kbd "C-l") 'recenter-top-bottom)))

(defun dirvish/post-init-all-the-icons ()
  "all-the-icons is initialized by spacemacs-visual layer.
Dirvish uses it for file icons in `dirvish-attributes'.")

(defun dirvish/post-init-nerd-icons ()
  "nerd-icons is initialized by spacemacs-visual layer.
Dirvish uses it for file icons in `dirvish-attributes'.")

(defun dirvish/post-init-golden-ratio ()
  (with-eval-after-load 'golden-ratio
    (add-to-list 'golden-ratio-exclude-modes "dirvish-mode")))
