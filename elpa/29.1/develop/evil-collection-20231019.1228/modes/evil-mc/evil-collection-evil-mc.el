;;; evil-collection-evil-mc.el --- Bindings for evil-mc -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.2
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, emacs, tools

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
;;; Bindings for evil-mc.

;;; Code:
(require 'evil-collection)
(require 'evil-mc nil t)
(eval-when-compile (require 'subr-x)) ; `if-let*' and `when-let*'

(defvar evil-mc-key-map)
(defvar evil-mc-cursors-map)
(defconst evil-collection-evil-mc-maps '(evil-mc-key-map evil-mc-cursors-map))

;;;###autoload
(defun evil-collection-evil-mc-setup ()
  "Set up `evil' bindings for evil-mc."

  (setcdr evil-mc-cursors-map nil)

  (define-key evil-mc-cursors-map (kbd "a") 'evil-mc-make-all-cursors)
  (define-key evil-mc-cursors-map (kbd "q") 'evil-mc-undo-all-cursors)
  (define-key evil-mc-cursors-map (kbd "u") 'evil-mc-undo-last-added-cursor)

  (define-key evil-mc-cursors-map (kbd "C-p") 'evil-mc-pause-cursors)
  (define-key evil-mc-cursors-map (kbd "C-r") 'evil-mc-resume-cursors)
  (define-key evil-mc-cursors-map (kbd "C-m") 'evil-mc-make-cursor-here)

  (define-key evil-mc-cursors-map (kbd "0") 'evil-mc-make-and-goto-first-cursor)
  (define-key evil-mc-cursors-map (kbd "$") 'evil-mc-make-and-goto-last-cursor)

  (define-key evil-mc-cursors-map (kbd "I") 'evil-mc-make-cursor-in-visual-selection-beg)
  (define-key evil-mc-cursors-map (kbd "A") 'evil-mc-make-cursor-in-visual-selection-end)

  (define-key evil-mc-cursors-map (kbd "o") 'evil-mc-make-cursor-move-next-line)
  (define-key evil-mc-cursors-map (kbd "O") 'evil-mc-make-cursor-move-prev-line)

  (define-key evil-mc-cursors-map (kbd "n") 'evil-mc-make-and-goto-next-match)
  (define-key evil-mc-cursors-map (kbd "N") 'evil-mc-make-and-goto-prev-match)
  (define-key evil-mc-cursors-map (kbd "C-n") 'evil-mc-skip-and-goto-next-match)
  (define-key evil-mc-cursors-map (kbd "C-S-n") 'evil-mc-skip-and-goto-prev-match)
  (define-key evil-mc-cursors-map (kbd "M-n") 'evil-mc-make-and-goto-next-cursor)
  (define-key evil-mc-cursors-map (kbd "M-N") 'evil-mc-make-and-goto-prev-cursor)
  (define-key evil-mc-cursors-map (kbd "C-u") 'evil-mc-skip-and-goto-next-cursor)
  (define-key evil-mc-cursors-map (kbd "C-S-u") 'evil-mc-skip-and-goto-prev-cursor)

  (setcdr evil-mc-key-map nil)
  (evil-collection-define-key '(normal visual) 'evil-mc-key-map (kbd "g.") evil-mc-cursors-map)

  ;; https://github.com/gabesoft/evil-mc/issues/70
  (add-hook 'evil-mc-after-cursors-deleted
            (lambda ()
              (setq evil-was-yanked-without-register t))))

(provide 'evil-collection-evil-mc)
;;; evil-collection-evil-mc.el ends here
