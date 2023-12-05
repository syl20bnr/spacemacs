;;; packages.el --- emoji Layer Packages File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
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


(defconst emoji-packages
  '(
    emoji-cheat-sheet-plus
    emojify
    (company-emoji :requires company)))

(defun emoji/init-emoji-cheat-sheet-plus ()
  (use-package emoji-cheat-sheet-plus
    :commands (emoji-cheat-sheet-plus-insert
               emoji-cheat-sheet-plus-buffer
               emoji-cheat-sheet-plus-display-mode)
    :init
    (spacemacs/set-leader-keys "afe" 'emoji-cheat-sheet-plus-buffer)
    (spacemacs/set-leader-keys "ie" 'emoji-cheat-sheet-plus-insert)
    :config
    (evilified-state-evilify-map emoji-cheat-sheet-plus-buffer-mode-map
      :mode emoji-cheat-sheet-plus-buffer-mode
      :bindings "<RET>" 'emoji-cheat-sheet-plus-echo-and-copy)
    (spacemacs|hide-lighter emoji-cheat-sheet-plus-display-mode)))

(defun emoji/init-emojify ()
  (use-package emojify
    :defer t
    :init
    (setq emojify-emojis-dir (concat spacemacs-cache-directory "emojify/"))))

(defun emoji/init-company-emoji ()
  (use-package company-emoji
    :defer t
    :init
    ;; For when Emacs is started in GUI mode:
    (spacemacs//set-emoji-font nil)
    ;; Hook for when a frame is created with emacsclient
    (spacemacs|do-after-display-system-init
     (spacemacs//set-emoji-font-for-current-frame))
    (spacemacs|add-company-backends
     :backends company-emoji
     :modes text-mode)
    :config
    (advice-add 'emoji-cheat-sheet-plus--insert-selection :after #'spacemacs/emoji-insert-and-possibly-complete)))
