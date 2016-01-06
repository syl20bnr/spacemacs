;;; hybrid-mode.el --- Put one foot in the church of Emacs

;; Copyright (C) 2012-2016 Sylvain Benner & Contributors
;;
;; Authors: Justin Burkett <justin@burkett.cc>
;;          Chris Ewald <chrisewald@gmail.com>
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

(defvar hybrid-mode-default-state-backup evil-default-state
  "Backup of `evil-default-state'.")

(defcustom hybrid-mode-default-state 'normal
  "Value of `evil-default-state' for hybrid-mode. Set to hybrid
to start in hybrid state (emacs bindings) by default."
  :group 'spacemacs
  :type 'symbol)

(defvar hybrid-mode-insert-cursor evil-hybrid-state-cursor)
(defvar hybrid-mode-insert-cursor-backup evil-insert-state-cursor)

;;;###autoload
(define-minor-mode hybrid-mode
  "Global minor mode to allow emacs bindings in `evil-insert-state'."
  :global t
  :lighter " hybrid"
  :group 'spacemacs
  (if hybrid-mode
      (progn
        (setq hybrid-mode-default-state-backup evil-default-state
              evil-default-state hybrid-mode-default-state
              evil-insert-state-cursor hybrid-mode-insert-cursor)
        (put 'spacemacs-insert-face 'face-alias 'spacemacs-hybrid-face)
        ;; using this function to set the variable triggers the defcustom :set
        ;; property which actually does the work of removing the bindings.
        (customize-set-variable 'evil-disable-insert-state-bindings t))
    (setq evil-default-state hybrid-mode-default-state-backup
          evil-insert-state-cursor hybrid-mode-insert-cursor-backup)
    (put 'spacemacs-insert-face 'face-alias nil)
    (customize-set-variable 'evil-disable-insert-state-bindings nil)))

(provide 'hybrid-mode)
