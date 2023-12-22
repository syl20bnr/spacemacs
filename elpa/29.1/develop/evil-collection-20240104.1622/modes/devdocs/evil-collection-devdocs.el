;;; evil-collection-devdocs.el --- Evil bindings for devdocs -*- lexical-binding: t -*-

;; Copyright (C) 2021 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, tools

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
;; Evil bindings for devdocs.

;;; Code:
(require 'evil-collection)
(require 'devdocs nil t)

(defconst evil-collection-devdocs-maps '(devdocs-mode-map))

;;;###autoload
(defun evil-collection-devdocs-setup ()
  "Set up `evil' bindings for `devdocs'."
  (evil-set-initial-state 'devdocs-mode 'normal)
  (evil-collection-set-readonly-bindings 'devdocs-mode-map)

  (evil-collection-define-key 'normal 'devdocs-mode-map
    ;; motion
    (kbd "SPC")         'scroll-up-command
    (kbd "S-SPC")       'scroll-down-command
    (kbd "<tab>")       'forward-button
    (kbd "<backtab>")   'backward-button

    "[[" 'devdocs-previous-page
    "]]" 'devdocs-next-page
    "gk" 'devdocs-previous-page
    "gj" 'devdocs-next-page

    "g." 'devdocs-goto-target

    ;; history
    (kbd "C-p") 'devdocs-go-back
    (kbd "C-n") 'devdocs-go-forward

    ;; copy
    "C" 'devdocs-copy-url

    ;; search
    "s" 'devdocs-lookup))

(provide 'evil-collection-devdocs)
;;; evil-collection-devdocs.el ends here
