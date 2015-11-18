;;; holy-mode.el --- Enter the church of Emacs

;; Copyright (C) 2014-2015 syl20bnr
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: convenience editing
;; Created: 18 Mar 2015
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

(defcustom holy-mode-allow-esc-to-normal-state nil
  "When non-nil bind ESC to `evil-normal-state' in emacs state
when holy-mode is active. This will also make commands in normal
state that usually enter insert state enter emacs state instead.
This option is for those who want to be in emacs state most of
the time but still want the option of easily going into evil's
normal state."
  :group 'spacemacs
  :type 'boolean)

(defadvice evil-insert-state (around benedictus-dominus disable)
  "Preparing the holy water flasks."
  (evil-emacs-state))

;;;###autoload
(define-minor-mode holy-mode
  "Global minor mode to repulse the evil from spacemacs.

The `insert state' is replaced by the `emacs state'."
  :global t
  :lighter " holy"
  :group 'spacemacs
  (if holy-mode
      (in-nominus-patris-et-filii-et-sipritus-sancti)
    (amen)))

(defun in-nominus-patris-et-filii-et-sipritus-sancti ()
  "Enter the church of Emacs (wash your hands)."
  ;; make all buffers' initial state emacs
  (push '("." . emacs) evil-buffer-regexps)
  ;; this is correct, but not really necessary given previous line
  (setq evil-default-state 'emacs)
  (when holy-mode-allow-esc-to-normal-state
    ;; allow to return to `normal state' with escape
    (define-key evil-emacs-state-map [escape] 'evil-normal-state)
    ;; replace `insert state' by `emacs state'
    (ad-enable-advice 'evil-insert-state 'around 'benedictus-dominus)
    (ad-activate 'evil-insert-state))
  ;; helm navigation
  (when (fboundp 'spacemacs//helm-hjkl-navigation)
    (spacemacs//helm-hjkl-navigation nil))
  ;; initiate `emacs state' and enter the church
  (holy-mode//update-states-for-current-buffers))

(defun amen ()
  "May the force be with you my son (or not)."
  ;; restore defaults
  (setq evil-buffer-regexps (delete '("." . emacs) evil-buffer-regexps))
  (setq evil-default-state 'normal)
  (when holy-mode-allow-esc-to-normal-state
    ;; restore key bindings
    (define-key evil-emacs-state-map [escape] nil)
    ;; restore `insert state'
    (ad-disable-advice 'evil-insert-state 'around 'benedictus-dominus)
    (ad-activate 'evil-insert-state))
  ;; restore helm navigation
  (when (fboundp 'spacemacs//helm-hjkl-navigation)
    (spacemacs//helm-hjkl-navigation t))
  ;; restore the states
  (holy-mode//update-states-for-current-buffers t))

(defun holy-mode//update-states-for-current-buffers (&optional arg)
  "Update the active state in all current buffers.
ARG non nil means that the editing style is `vim'."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      ;; switch to holy-mode
      (when (not arg)
        (evil-emacs-state))
      ;; disable holy-mode
      (when (and arg (eq 'emacs evil-state))
        (cond
         ((memq major-mode evil-evilified-state-modes) (evil-evilified-state))
         ((memq major-mode evil-motion-state-modes) (evil-motion-state))
         (t (evil-normal-state)))))))
