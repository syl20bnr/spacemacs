;;; evil-collection-daemons.el --- Evil Bindings for Daemons -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: Jay Kamat <jaygkamat@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, daemons, tools

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
;; Evil bindings for `daemons' from the `daemons.el' package.

;;; Code:
(require 'daemons nil t)
(require 'evil-collection)

(defconst evil-collection-daemons-maps '(daemons-mode-map
                                         daemons-output-mode-map))

;;;###autoload
(defun evil-collection-daemons-setup ()
  "Set up `evil' bindings for `daemons'."
  (evil-collection-define-key '(normal visual) 'daemons-mode-map
    (kbd "RET") 'daemons-status-at-point
    "s" 'daemons-start-at-point
    "S" 'daemons-stop-at-point
    "r" 'daemons-reload-at-point
    "R" 'daemons-restart-at-point

    "gr" 'revert-buffer

    "q" 'quit-window
    "ZZ" 'quit-window
    "ZQ" 'quit-window)

  ;; Functions are available in daemons-output-mode-map as well
  (evil-collection-define-key '(normal visual) 'daemons-output-mode-map
    (kbd "RET") 'daemons-status-at-point
    "s" 'daemons-start-at-point
    "S" 'daemons-stop-at-point
    "r" 'daemons-reload-at-point
    "R" 'daemons-restart-at-point

    "q" 'quit-window
    "ZZ" 'quit-window
    "ZQ" 'quit-window)

  (evil-set-initial-state 'daemons-mode 'normal)
  (evil-set-initial-state 'daemons-output-mode 'normal))

(provide 'evil-collection-daemons)
;;; evil-collection-daemons.el ends here
