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

(defvar holy-mode-normal-state-modes-backup nil
  "Backup of `evil-normal-state-modes'.")

(defvar holy-mode-motion-state-modes-backup nil
  "Backup of `evil-motion-state-modes'.")

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
  ;; transfert all modes defaulting to `evilified state' to
  ;; `emacs state'
  (setq evil-evilified-state-modes nil)
  (mapc (lambda (x) (push x evil-emacs-state-modes))
        spacemacs-core-evilified-state--modes)
  ;; allow to return to `normal state' with escape
  (define-key evil-emacs-state-map [escape] 'evil-normal-state)
  ;; replace `insert state' by `emacs state'
  (ad-enable-advice 'evil-insert-state 'around 'benedictus-dominus)
  (ad-activate 'evil-insert-state)
  ;; start all buffers in `emacs state'
  (setq evil-default-state 'emacs)
  (setq holy-mode-normal-state-modes-backup evil-normal-state-modes)
  (setq evil-normal-state-modes nil)
  (setq holy-mode-motion-state-modes-backup evil-motion-state-modes)
  (setq evil-motion-state-modes nil)
  ;; helm navigation
  (when (fboundp 'spacemacs//helm-hjkl-navigation)
    (spacemacs//helm-hjkl-navigation nil))
  ;; initiate `emacs state' and enter the church
  (holy-mode//update-states-for-current-buffers))

(defun amen ()
  "May the force be with you my son (or not)."
  ;; restore default `evilified state'
  (mapc (lambda (x) (delq x evil-emacs-state-modes))
        spacemacs-core-evilified-state--modes)
  (setq evil-evilified-state-modes spacemacs-core-evilified-state--modes)
  ;; restore `insert state'
  (ad-disable-advice 'evil-insert-state 'around 'benedictus-dominus)
  (ad-activate 'evil-insert-state)
  ;; restore `normal state'
  (setq evil-default-state 'normal)
  ;; restore per mode default states
  (setq evil-normal-state-modes holy-mode-normal-state-modes-backup)
  (setq evil-motion-state-modes holy-mode-motion-state-modes-backup)
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
      (when (and (not arg) (or (eq 'evilified evil-state)
                         (eq 'normal evil-state)))
        (evil-emacs-state))
      ;; disable holy-mode
      (when (and arg (eq 'emacs evil-state))
        (cond
         ((memq major-mode evil-evilified-state-modes) (evil-evilified-state))
         ((memq major-mode evil-motion-state-modes) (evil-motion-state))
         (t (evil-normal-state)))))))
