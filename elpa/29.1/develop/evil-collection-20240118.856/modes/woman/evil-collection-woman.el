;;; evil-collection-woman.el --- Evil bindings for WoMan -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, woman, tools

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
;; Evil bindings for `woman'.

;;; Code:
(require 'evil-collection)
(evil-collection-require 'man)
(require 'woman)

(defconst evil-collection-woman-maps '(woman-mode-map))

;;;###autoload
(defun evil-collection-woman-setup ()
  "Set up `evil' bindings for `woman'."
  (evil-set-initial-state 'woman-mode 'normal)
  (evil-collection-define-key 'normal 'woman-mode-map
    (kbd "]]") 'WoMan-next-manpage
    (kbd "[[") 'WoMan-previous-manpage

    ;; goto
    ;; "gm" 'woman

    ;; refresh
    "gr" 'woman-reformat-last-file))

(provide 'evil-collection-woman)
;;; evil-collection-woman.el ends here
