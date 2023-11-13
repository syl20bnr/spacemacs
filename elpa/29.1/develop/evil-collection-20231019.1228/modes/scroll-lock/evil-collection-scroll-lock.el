;;; evil-collection-scroll-lock.el --- Bindings for `scroll-lock' -*- lexical-binding: t -*-

;; Copyright (C) 2021 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, elisp, lisp

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
;;; Bindings for `scroll-lock'.

;;; Code:
(require 'scroll-lock)
(require 'evil-collection)

(declare-function scroll-lock-next-line "scroll-lock")
(declare-function scroll-lock-previous-line "scroll-lock")
(declare-function scroll-lock-forward-paragraph "scroll-lock")
(declare-function scroll-lock-backward-paragraph "scroll-lock")

(defconst evil-collection-scroll-lock-maps '(scroll-lock-mode-map))

(evil-define-motion evil-collection-scroll-lock-next-line (count)
  "Scroll down COUNT lines keeping point fixed."
  :type line
  (let (line-move-visual)
    (scroll-lock-next-line (or count 1))))

(evil-define-motion evil-collection-scroll-lock-previous-line (count)
  "Scroll up COUNT lines keeping point fixed."
  :type line
  (let (line-move-visual)
    (scroll-lock-previous-line (or count 1))))

(evil-define-motion evil-collection-scroll-lock-forward-paragraph (count)
  "Scroll down COUNT paragraphs keeping point fixed."
  :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob count)
  (scroll-lock-forward-paragraph count)
  (unless (eobp) (forward-line)))

(evil-define-motion evil-collection-scroll-lock-backward-paragraph (count)
  "Scroll up COUNT paragraphs keeping point fixed."
  :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob (- (or count 1)))
  (unless (eobp) (forward-line))
  (scroll-lock-backward-paragraph count)
  (unless (bobp) (forward-line -1)))

;;;###autoload
(defun evil-collection-scroll-lock-setup ()
  "Set up `evil' bindings for `scroll-lock'."
  (evil-collection-define-key 'normal 'scroll-lock-mode-map
    [remap evil-next-line] 'evil-collection-scroll-lock-next-line
    [remap evil-previous-line] 'evil-collection-scroll-lock-previous-line
    [remap evil-forward-paragraph]
    'evil-collection-scroll-lock-forward-paragraph
    [remap evil-backward-paragraph]
    'evil-collection-scroll-lock-backward-paragraph)

  (evil-add-command-properties
   #'evil-collection-scroll-lock-forward-paragraph :jump t)
  (evil-add-command-properties
   #'evil-collection-scroll-lock-backward-paragraph :jump t)

  (add-hook 'scroll-lock-mode-hook 'evil-normalize-keymaps))

(provide 'evil-collection-scroll-lock)
;;; evil-collection-scroll-lock.el ends here
