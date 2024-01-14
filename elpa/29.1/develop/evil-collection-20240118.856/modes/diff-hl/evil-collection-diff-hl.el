;;; evil-collection-diff-hl.el --- Evil bindings for diff-hl -*- lexical-binding: t -*-

;; Copyright (C) 2021 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, diff, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for diff-hl.
;;
;; All functions used below are autoloaded except for
;; `diff-hl-inline-popup--popup-down' et al commands.
;;
;; When users invoke `diff-hl-show-hunk' (it invokes
;; `diff-hl-show-hunk-function' under the hood), `diff-hl-inline-popup.el' or
;; `diff-hl-show-hunk-posframe.el' will be loaded eventually.
;;
;; So there is no
;;
;;   (require 'diff-hl nil t)
;;
;; below.

;;; Code:
(require 'evil-collection)

(defconst evil-collection-diff-hl-maps '(diff-hl-show-hunk-map
                                         diff-hl-inline-popup-transient-mode-map))

;;;###autoload
(defun evil-collection-diff-hl-setup()
  "Set up `evil' bindings for `diff-hl'."
  (add-hook 'diff-hl-inline-popup-transient-mode-hook 'evil-normalize-keymaps)

  (evil-collection-define-key 'normal 'diff-hl-inline-popup-transient-mode-map
    ;; quit
    "q" 'diff-hl-inline-popup-hide
    (kbd "<escape>") 'diff-hl-inline-popup-hide

    ;; motion
    "j" 'diff-hl-inline-popup--popup-down
    "k" 'diff-hl-inline-popup--popup-up
    (kbd "C-f") 'diff-hl-inline-popup--popup-pagedown
    (kbd "C-b") 'diff-hl-inline-popup--popup-pageup)

  ;; Actually `diff-hl-inline-popup-transient-mode-map' will inherit it by
  ;; `set-keymap-parent'.
  (evil-collection-define-key 'normal 'diff-hl-show-hunk-map
    ;; Keep it the same as the overlay shows.
    "p" 'diff-hl-show-hunk-previous
    "n" 'diff-hl-show-hunk-next
    "c" 'diff-hl-show-hunk-copy-original-text
    "r" 'diff-hl-show-hunk-revert-hunk
    "S" 'diff-hl-show-hunk-stage-hunk))

(provide 'evil-collection-diff-hl)
;;; evil-collection-diff-hl.el ends here
