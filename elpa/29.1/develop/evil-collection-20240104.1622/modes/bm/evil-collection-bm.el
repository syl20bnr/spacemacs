;;; evil-collection-bm.el --- Evil bindings for bm -*- lexical-binding: t -*-

;; Copyright (C) 2020 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, bm, tools

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
;; Evil bindings for bm.

;;; Code:
(require 'evil-collection)
(require 'bm nil t)

(defconst evil-collection-bm-maps '(bm-show-mode-map))

;;;###autoload
(defun evil-collection-bm-setup ()
  "Set up `evil' bindings for `bm'."
  (evil-set-initial-state 'bm-show-mode 'normal)

  (evil-collection-define-key 'normal 'bm-show-mode-map
    "q" 'bm-show-quit-window
    "gr" 'revert-buffer
    "g?" 'describe-mode

    "j" 'bm-show-next
    "k" 'bm-show-prev

    ;; open
    (kbd "RET") 'bm-show-goto-bookmark
    (kbd "S-<return>") 'bm-show-bookmark))


(provide 'evil-collection-bm)
;;; evil-collection-bm.el ends here
