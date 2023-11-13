;;; evil-collection-youtube-dl.el --- Evil bindings for youtube-dl -*- lexical-binding: t -*-

;; Copyright (C) 2018 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>, Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, youtube, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for youtube-dl.

;;; Code:
(require 'evil-collection)
(require 'youtube-dl nil t)

(defvar youtube-dl-list-mode-map)

(defconst evil-collection-youtube-dl-maps '(youtube-dl-list-mode-map))

;;;###autoload
(defun evil-collection-youtube-dl-setup ()
  "Set up `evil' bindings for `youtube-dl'."

  (evil-collection-set-readonly-bindings 'youtube-dl-list-mode-map)
  (evil-collection-define-key 'normal 'youtube-dl-list-mode-map
    (kbd "S-SPC") 'scroll-down-command

    "gr" 'youtube-dl-list-redisplay

    "g?" 'describe-mode

    "p" 'youtube-dl-list-toggle-pause
    "s" 'youtube-dl-list-toggle-slow
    "S" 'youtube-dl-list-toggle-slow-all
    "y" 'youtube-dl-list-yank

    "i" 'youtube-dl-list-log
    "I" 'youtube-dl-list-kill-log

    "a" 'youtube-dl
    "d" 'youtube-dl-list-kill

    "[[" 'youtube-dl-list-priority-down
    "]]" 'youtube-dl-list-priority-up
    "gj" 'youtube-dl-list-priority-down
    "gk" 'youtube-dl-list-priority-up
    (kbd "C-j") 'youtube-dl-list-priority-down
    (kbd "C-k") 'youtube-dl-list-priority-up))

(provide 'evil-collection-youtube-dl)
;;; evil-collection-youtube-dl.el ends here
