;;; evil-evilified-state.el --- A minimalistic evil state

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

(defvar evil-evilified-state--modes nil
  "List of all evilified modes.")

(evil-define-state evilified
  "Evilified state.
 Hybrid `emacs state' with carrefully selected Vim key bindings.
 See spacemacs conventions for more info."
  :tag " <Ev> "
  :enable (emacs)
  :message "-- EVILIFIED BUFFER --"
  :cursor box
  (when (evil-evilified-state-p)
    (when (bound-and-true-p evil-surround-mode)
      (evil-surround-mode -1))
    (setq-local evil-normal-state-map (cons 'keymap nil))
    (setq-local evil-visual-state-map (cons 'keymap nil))
    (add-hook 'evil-visual-state-entry-hook
              (lambda () (interactive)
                (local-set-key evil-visual-state-map "y" 'evil-yank))
              nil 'local)))

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
(define-key evil-evilified-state-map "y" 'evil-yank)

(defmacro evilify (mode map &rest body)
  "Set `evilified state' as default for MODE.

BODY is a list of additional key bindings to apply for the given MAP in
`evilified state'."
  (let ((defkey (when body `(evil-define-key 'evilified ,map ,@body))))
    `(progn (unless ,(null mode)
              (unless (memq ',mode evil-evilified-state--modes)
                (push ',mode evil-evilified-state--modes))
              (unless (or (bound-and-true-p holy-mode)
                          (memq ',mode evil-evilified-state-modes))
                (delq ',mode evil-emacs-state-modes)
                (push ',mode evil-evilified-state-modes)))
            (unless ,(null defkey) (,@defkey)))))

(provide 'evil-evilified-state)

;;; evil-evilified-state.el ends here
