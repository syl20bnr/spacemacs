;;; packages.el --- Nav-flash Layer Packages File for Spacemacs.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
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


(defconst nav-flash-packages
  '(nav-flash))

(defun nav-flash/init-nav-flash()
  :defer t
  :init
  (spacemacs/add-to-hooks #'nav-flash/blink-cursor-maybe
                          '(imenu-after-jump-hook
                            better-jumper-post-jump-hook
                            counsel-grep-post-action-hook
                            xref-after-jump-hook
                            dumb-jump-after-jump-hook)
                          t)
  (spacemacs/add-to-hooks #'nav-flash/delayed-blink-cursor-h
                          '(eyebrowse-post-window-switch-hook)
                          t)
  ;; `org'
  (add-hook 'org-follow-link-hook #'nav-flash/delayed-blink-cursor-h)

  ;; (add-hook 'persp-activated-functions #'nav-flash/delayed-blink-cursor-h)

  ;; `saveplace'
  (advice-add #'save-place-find-file-hook :after #'nav-flash/blink-cursor-maybe)

  ;; `evil'
  (advice-add #'evil-window-top    :after #'nav-flash/blink-cursor-maybe)
  (advice-add #'evil-window-middle :after #'nav-flash/blink-cursor-maybe)
  (advice-add #'evil-window-bottom :after #'nav-flash/blink-cursor-maybe)

  ;; Bound to `ga' for evil users
  (advice-add #'what-cursor-position :after #'nav-flash/blink-cursor-maybe)

  ;; misc
  (dolist (command '(scroll-up-command
                     scroll-down-command
                     recenter-top-bottom
                     other-window
                     other-frame
                     switch-to-buffer
                     winum-select-window-by-number
                     pop-tag-mark
                     ;; persp-switch
                     spacemacs/alternate-buffer
                     spacemacs/jump-to-definition))
    (advice-add command :after #'nav-flash/blink-cursor-maybe))

  ;; persp and eyebrowse
  (advice-add 'persp-switch :after #'nav-flash/delayed-blink-cursor-h)

  :config
  ;; emacs 27 extend face
  (when (fboundp 'set-face-extend)
    (with-eval-after-load "nav-flash"
      (set-face-extend 'nav-flash-face t))))
