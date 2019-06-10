;;; evil-unimpaired.el --- Pairs of handy bracket mappings.

;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: evil, vim-unimpaired, spacemacs
;; Version: 0.1
;; Package-Requires: ((dash "2.12.0") (f "0.18.0"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is a port of vim-unimpaired https://github.com/tpope/vim-unimpaired
;; `evil-unimpaired' provides pairs of handy bracket mappings to quickly navigate
;; to previous/next thing and more.

;;; Code:

(require 'dash)
(require 'f)

(defun evil-unimpaired//find-relative-filename (offset)
  (when buffer-file-name
    (let* ((directory (f-dirname buffer-file-name))
           (files (f--files directory (not (s-matches? "^\\.?#" it))))
           (index (+ (-elem-index buffer-file-name files) offset))
           (file (and (>= index 0) (nth index files))))
      (when file
        (f-expand file directory)))))

(defun evil-unimpaired/previous-file ()
  (interactive)
  (-if-let (filename (evil-unimpaired//find-relative-filename -1))
      (find-file filename)
    (user-error "No previous file")))

(defun evil-unimpaired/next-file ()
  (interactive)
  (-if-let (filename (evil-unimpaired//find-relative-filename 1))
      (find-file filename)
    (user-error "No next file")))

(defun evil-unimpaired/paste-above ()
  (interactive)
  (setq this-command 'evil-paste-after)
  (evil-insert-newline-above)
  (evil-paste-after 1))

(defun evil-unimpaired/paste-below ()
  (interactive)
  (setq this-command 'evil-paste-after)
  (evil-insert-newline-below)
  (evil-paste-after 1))

(defun evil-unimpaired/insert-space-above (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun evil-unimpaired/insert-space-below (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

(defun evil-unimpaired/next-frame ()
  (interactive)
  (select-frame-set-input-focus (next-frame)))

(defun evil-unimpaired/previous-frame ()
  (interactive)
  (select-frame-set-input-focus (previous-frame)))

;; from tpope's unimpaired
(define-key evil-normal-state-map (kbd "[ SPC")
  'evil-unimpaired/insert-space-above)
(define-key evil-normal-state-map (kbd "] SPC")
  'evil-unimpaired/insert-space-below)
(define-key evil-normal-state-map (kbd "[ e") 'move-text-up)
(define-key evil-normal-state-map (kbd "] e") 'move-text-down)
(define-key evil-visual-state-map (kbd "[ e") ":move'<--1")
(define-key evil-visual-state-map (kbd "] e") ":move'>+1")
;; (define-key evil-visual-state-map (kbd "[ e") 'move-text-up)
;; (define-key evil-visual-state-map (kbd "] e") 'move-text-down)
;; navigation
(define-key evil-motion-state-map (kbd "[ b") 'previous-buffer)
(define-key evil-motion-state-map (kbd "] b") 'next-buffer)
(define-key evil-motion-state-map (kbd "[ f") 'evil-unimpaired/previous-file)
(define-key evil-motion-state-map (kbd "] f") 'evil-unimpaired/next-file)
(define-key evil-motion-state-map (kbd "] l") 'spacemacs/next-error)
(define-key evil-motion-state-map (kbd "[ l") 'spacemacs/previous-error)
(define-key evil-motion-state-map (kbd "] q") 'spacemacs/next-error)
(define-key evil-motion-state-map (kbd "[ q") 'spacemacs/previous-error)
(define-key evil-motion-state-map (kbd "[ t") 'evil-unimpaired/previous-frame)
(define-key evil-motion-state-map (kbd "] t") 'evil-unimpaired/next-frame)
(define-key evil-motion-state-map (kbd "[ w") 'previous-multiframe-window)
(define-key evil-motion-state-map (kbd "] w") 'next-multiframe-window)
;; select pasted text
(define-key evil-normal-state-map (kbd "g p") (kbd "` [ v ` ]"))
;; paste above or below with newline
(define-key evil-normal-state-map (kbd "[ p") 'evil-unimpaired/paste-above)
(define-key evil-normal-state-map (kbd "] p") 'evil-unimpaired/paste-below)

(provide 'evil-unimpaired)
;;; evil-unimpaired.el ends here.
