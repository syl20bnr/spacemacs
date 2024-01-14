;;; evil-collection-zmusic.el --- Bindings for `zmusic' -*- lexical-binding: t -*-

;; Copyright (C) 2020 Carla Cao

;; Author: Carla Cao <ccao001@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, emacs, tools, minimalist, presentation

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
;;; Bindings for`zmusic'.

;;; Code:
(require 'evil-collection)
(require 'zmusic nil t) 

(defvar zmusic-mode-map) 

(defconst evil-collection-zmusic-maps '(zmusic-mode-map))

;;;###autoload
(defun evil-collection-zmusic-setup ()
  "Set up `evil' bindings for `zmusic'."
  (evil-collection-define-key 'normal 'zmusic-mode-map
    (kbd "RET") 'zmusic/toggle-play
    "j" 'zmusic/next-beat
    "k" 'zmusic/previous-beat
    "h" 'zmusic/backward-degree
    "l" 'zmusic/forward-degree
    "d" 'zmusic/kill-beat))

(provide 'evil-collection-zmusic)
;;; evil-collection-zmusic.el ends here
