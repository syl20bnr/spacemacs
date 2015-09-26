;;; core-evilified-state.el --- A minimalistic evil state

;; Copyright (C) 2014, 2015 syl20bnr
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: convenience editing evil spacemacs
;; Created: 22 Mar 2015
;; Version: 1.0
;; Package-Requires: ((evil "1.0.9") (evil-leader "0.4.3"))

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

;; Define a `evilified' evil state inheriting from `emacs' state and
;; setting a minimalist list of Vim key bindings (like navigation, search, ...)

;;; Code:

(require 'evil)

(defvar spacemacs-core-evilified-state--modes nil
  "List of all evilified modes.")

(defvar spacemacs-core-evilified-state--visual-state-map evil-visual-state-map
  "Evil visual state map backup.")

(defvar spacemacs-core-evilified-state--evil-surround nil
  "Evil surround mode variable backup.")
(make-variable-buffer-local 'spacemacs-core-evilified-state--evil-surround)

(evil-define-state evilified
  "Evilified state.
 Hybrid `emacs state' with carrefully selected Vim key bindings.
 See spacemacs conventions for more info."
  :tag " <Ev> "
  :enable (emacs)
  :message "-- EVILIFIED BUFFER --"
  :cursor box)

(add-hook 'evil-evilified-state-entry-hook 'spacemacs//evilified-state-on-entry)
(add-hook 'evil-evilified-state-exit-hook 'spacemacs//evilified-state-on-exit)

(add-hook 'evil-visual-state-entry-hook 'spacemacs//evilified-state-on-entry)
(add-hook 'evil-visual-state-exit-hook 'spacemacs//evilified-state-on-exit)

(defun spacemacs//evilify-pre-command-hook ()
  (let ((map (get-char-property (point) 'keymap)))
    (when (and map (assq 'evilified-state map))
      (let* ((submap (cdr (assq 'evilified-state map)))
             (command (when (and submap (eq 1 (length (this-command-keys))))
                        (lookup-key submap (this-command-keys)))))
        (when command
          (setq this-command command))))))

(defun spacemacs//evilified-state-on-entry ()
  "Setup evilified state."
  (add-hook 'pre-command-hook 'spacemacs//evilify-pre-command-hook nil 'local)
  (when (bound-and-true-p evil-surround-mode)
    (make-local-variable 'evil-surround-mode)
    (evil-surround-mode -1))
  (setq-local evil-normal-state-map (cons 'keymap nil))
  (setq-local evil-visual-state-map (cons 'keymap (list (cons ?y 'evil-yank)))))

(defun spacemacs//evilified-state-on-exit ()
  "Clean evilified-state"
  (remove-hook 'pre-command-hook 'spacemacs//evilify-pre-command-hook 'local))

;; default key bindings for all evilified buffers
(define-key evil-evilified-state-map (kbd dotspacemacs-leader-key)
  evil-leader--default-map)
(define-key evil-evilified-state-map "/" 'evil-search-forward)
(define-key evil-evilified-state-map ":" 'evil-ex)
(define-key evil-evilified-state-map "h" 'evil-backward-char)
(define-key evil-evilified-state-map "j" 'evil-next-visual-line)
(define-key evil-evilified-state-map "k" 'evil-previous-visual-line)
(define-key evil-evilified-state-map "l" 'evil-forward-char)
(define-key evil-evilified-state-map "n" 'evil-search-next)
(define-key evil-evilified-state-map "N" 'evil-search-previous)
(define-key evil-evilified-state-map "v" 'evil-visual-char)
(define-key evil-evilified-state-map "V" 'evil-visual-line)
(define-key evil-evilified-state-map "gg" 'evil-goto-first-line)
(define-key evil-evilified-state-map "G" 'evil-goto-line)
(define-key evil-evilified-state-map (kbd "C-f") 'evil-scroll-page-down)
(define-key evil-evilified-state-map (kbd "C-b") 'evil-scroll-page-up)
(define-key evil-evilified-state-map (kbd "C-d") 'evil-scroll-down)
(define-key evil-evilified-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-evilified-state-map (kbd "C-z") 'evil-emacs-state)

(provide 'core-evilified-state)

;;; core-evilified-state.el ends here
