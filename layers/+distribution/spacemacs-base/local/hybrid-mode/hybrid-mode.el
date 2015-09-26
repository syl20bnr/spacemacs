;;; hybrid-mode.el --- Put one foot in the church of Emacs

;; Copyright (C) 2014-2015 syl20bnr
;;
;; Author: Justin Burkett <justin@burkett.cc>, Chris Ewald <chrisewald@gmail.com>
;; Keywords: convenience editing
;; Created: 12 Aug 2015
;; Version: 1.00
;; Package-Requires: ((emacs "24") (evil "1.0.9"))
;; URL: https://github.com/syl20bnr/spacemacs

;; This file is not part of GNU Emacs.

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

;;; Code:

(require 'evil)

(evil-define-state hybrid
  "Emacs/insert state for hybrid mode."
  :tag " <H> "
  :cursor (bar . 2)
  :message "-- HYBRID --"
  :entry-hook (evil-start-track-last-insertion)
  :exit-hook (evil-cleanup-insert-state evil-stop-track-last-insertion)
  :input-method t
  (cond
   ((evil-hybrid-state-p)
    (add-hook 'pre-command-hook #'evil-insert-repeat-hook)
    (unless (eq evil-want-fine-undo t)
      (evil-start-undo-step t)))
   (t
    (remove-hook 'pre-command-hook #'evil-insert-repeat-hook)
    (setq evil-insert-repeat-info evil-repeat-info)
    (evil-set-marker ?^ nil t)
    (unless (eq evil-want-fine-undo t)
      (evil-end-undo-step t (eq evil-want-fine-undo 'fine)))
    (when evil-move-cursor-back
      (when (or (evil-normal-state-p evil-next-state)
                (evil-motion-state-p evil-next-state))
        (evil-move-cursor-back))))))

(define-key evil-hybrid-state-map [escape] 'evil-normal-state)
(setf (symbol-function 'hybrid-mode--evil-insert-state-backup)
      (symbol-function 'evil-insert-state))

;; Override stock evil function `evil-insert-state-p'
(defun evil-insert-state-p (&optional state)
  "Whether the current state is insert.
\(That is, whether `evil-state' is either `evil-insert-state' or
 `evil-hybrid-state'.)"
  (and evil-local-mode
       (memq (or state evil-state) '(insert hybrid))))

;;;###autoload
(define-minor-mode hybrid-mode
  "Global minor mode to replaces the `evil-insert-state' keymap
with `evil-hybrid-state-map'."
  :global t
  :lighter " hybrid"
  :group 'spacemacs
  (if hybrid-mode
      (setf (symbol-function 'evil-insert-state)
            (symbol-function 'evil-hybrid-state))
    (setf (symbol-function 'evil-insert-state)
          (symbol-function 'hybrid-mode--evil-insert-state-backup))))

(provide 'hybrid-mode)
