;;; packages.el --- ranger Layer packages File for Spacemacs  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Rich Alesi
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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


(defconst ranger-packages
  '(
    (dired :location built-in)
    (dirvish :toggle (eq ranger-override-dired 'dirvish))
    golden-ratio
    (ranger :toggle (not (eq ranger-override-dired 'dirvish)))))

(defun ranger//set-leader-keys ()
  (spacemacs/declare-prefix "atr" "ranger/dirvish")
  (if (eq ranger-override-dired 'dirvish)
      (spacemacs/set-leader-keys
        "atrr" 'ranger/dirvish-full-layout
        "atrd" 'dirvish
        "atrs" 'dirvish-side
        "atrq" 'dirvish-quick-access
        "atrf" 'dirvish-fd)
    (spacemacs/set-leader-keys
      "atrr" 'ranger
      "atrd" 'deer
      "jD" 'deer-jump-other-window
      "jd" 'deer)))

(defun ranger/init-dirvish ()
  (use-package dirvish
    :commands (dirvish dirvish-side dirvish-dwim dirvish-quick-access
                       dirvish-fd dirvish-override-dired-mode
                       dirvish-layout-toggle dirvish-layout-switch
                       dirvish-dispatch dirvish-ls-switches-menu
                       dirvish-narrow dirvish-subtree-toggle dirvish-quit)
    :init
    (ranger//set-leader-keys)
    (when (eq ranger-override-dired 'dirvish)
      (setq dirvish-default-layout nil)
      (dirvish-override-dired-mode 1))
    :config
    (ranger//apply-override-dired)
    ;; Add icons attribute based on dotspacemacs-default-icons-font.
    ;; We don't override dirvish-attributes or dirvish-mode-line-format
    ;; to respect user customizations via M-x customize.
    (when-let* ((icons-font dotspacemacs-default-icons-font))
      (with-eval-after-load icons-font
        (add-to-list 'dirvish-attributes icons-font)))))

(defun ranger/init-ranger ()
  (use-package ranger
    :commands (ranger deer deer-jump-other-window ranger-override-dired-mode)
    :init
    (ranger//set-leader-keys)

    (when (memq ranger-enter-with-minus '(deer ranger))
      (define-key evil-motion-state-map (kbd "-") ranger-enter-with-minus))

    ;; set up image-dired to allow picture resize
    (setq image-dired-dir (concat spacemacs-cache-directory "image-dir"))
    (unless (file-directory-p image-dired-dir)
      (make-directory image-dired-dir))
    :config
    (when (memq 'helm dotspacemacs-configuration-layers)
      (require 'helm))
    (define-key ranger-mode-map (kbd "-") 'ranger-up-directory)
    (ranger//apply-override-dired)))

(defun ranger/post-init-dired ()
  ;; Be sure to override dired bindings
  (ranger//set-leader-keys)
  (when dirvish-enable-dired-omit
    (add-hook 'dired-mode-hook #'dired-omit-mode))
  (use-package dired
    :defer t
    :config
    (when (eq ranger-override-dired 'dirvish)
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
        (kbd "C-l") 'recenter-top-bottom))))

(defun ranger/post-init-golden-ratio ()
  (with-eval-after-load 'golden-ratio
    (add-to-list 'golden-ratio-exclude-modes "ranger-mode")
    (add-to-list 'golden-ratio-exclude-modes "dirvish-mode")))
