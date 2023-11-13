;;; evil-collection-vdiff.el --- Evil bindings for vdiff -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Evgeni Kolev <evgenysw@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, vdiff, tools

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
;; Evil bindings for vdiff.el https://github.com/justbur/emacs-vdiff

;;; Code:
(require 'vdiff nil t)
(require 'evil-collection)

(defconst evil-collection-vdiff-maps '(vdiff-mode-map
                                       vdiff-3way-mode-map))

;;;###autoload
(defun evil-collection-vdiff-setup ()
  "Set up `evil' bindings for `vdiff-mode'."
  (add-hook 'vdiff-mode-hook 'evil-normalize-keymaps)
  (add-hook 'vdiff-3way-mode-hook 'evil-normalize-keymaps)

  (dolist (keymap evil-collection-vdiff-maps)
    (evil-collection-define-key 'normal keymap
      "]c" 'vdiff-next-hunk
      "[c" 'vdiff-previous-hunk)

    ;; define `do' (diff obtain) and `dp' (diff put) bindings
    (evil-collection-define-operator-key 'delete keymap
      "o" 'vdiff-receive-changes
      "p" 'vdiff-send-changes)))

(provide 'evil-collection-vdiff)

;;; evil-collection-vdiff.el ends here
