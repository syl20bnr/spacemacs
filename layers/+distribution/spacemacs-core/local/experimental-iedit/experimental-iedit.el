;;; experimental-iedit.el --- iedit experimentations.

;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: helm, spacemacs
;; Version: 0.1
;; Package-Requires: ((helm "1.5"))

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
;; This package adds a convenient way to discover Spacemacs configuration
;; layers thanks to helm.

;;; Code:

(require 'iedit)
(require 'evil-iedit-state)

(define-key evil-iedit-state-map (kbd "H") 'ex-iedit-move-overlays-backward)
(define-key evil-iedit-state-map (kbd "J") 'ex-iedit-move-overlays-next-line)
(define-key evil-iedit-state-map (kbd "K") 'ex-iedit-move-overlays-previous-line)
(define-key evil-iedit-state-map (kbd "L") 'ex-iedit-move-overlays-forward)
(define-key evil-iedit-state-map (kbd "C-S-h") '(lambda () (interactive) (ex-iedit-expand-overlays -1)))
(define-key evil-iedit-state-map (kbd "C-S-l") '(lambda () (interactive) (ex-iedit-expand-overlays 1)))
(define-key evil-iedit-state-map (kbd "w") '(lambda () (interactive) (ex-iedit-expand-overlays 'evil-forward-word-begin t)))
(define-key evil-iedit-state-map (kbd "gj") 'ex-iedit-toggle-point-overlay-next-line)
(define-key evil-iedit-state-map (kbd "gk") 'ex-iedit-toggle-point-overlay-previous-line)

(defun iedit-toggle-selection ()
  "Override default iedit function to be able to add arbitrary overlays.

It will toggle the overlay under point or create an overlay of one character."
  (interactive)
  (iedit-barf-if-buffering)
  (let ((ov (iedit-find-current-occurrence-overlay)))
    (if ov
        (iedit-restrict-region (overlay-start ov) (overlay-end ov) t)
      (save-excursion
        (push (iedit-make-occurrence-overlay (point) (1+ (point)))
              iedit-occurrences-overlays))
      (setq iedit-mode
            (propertize
             (concat " Iedit:" (number-to-string
                                (length iedit-occurrences-overlays)))
             'face 'font-lock-warning-face))
      (force-mode-line-update))))

(defun ex-iedit-move-overlays (func)
    (dolist (o iedit-occurrences-overlays)
    (let* ((start (overlay-start o))
            (end (overlay-end o))
            (len (- end start)))
        (save-excursion
        (goto-char (overlay-start o))
        (ignore-errors (call-interactively func))
        (move-overlay o (point) (+ (point) len))))))

(defun ex-iedit-expand-overlays (arg &optional from-end)
  (when (iedit-find-current-occurrence-overlay)
    (dolist (o iedit-occurrences-overlays)
      (let ((start (overlay-start o))
            (end (overlay-end o)))
        (save-excursion
          (if from-end
              (goto-char end)
            (goto-char start))
          (cond
           ((numberp arg)
            (when (> (+ (- end start) arg) 0)
              (move-overlay o start (+ arg end))))
           ((functionp arg)
            (ignore-errors (call-interactively arg))
            (move-overlay o start (point)))))))))

(defun ex-iedit-move-overlays-forward ()
    (interactive)
    (ex-iedit-move-overlays 'evil-forward-char))

(defun ex-iedit-move-overlays-backward ()
    (interactive)
    (ex-iedit-move-overlays 'evil-backward-char))

(defun ex-iedit-move-overlays-next-line ()
    (interactive)
    (ex-iedit-move-overlays 'evil-next-line))

(defun ex-iedit-move-overlays-previous-line ()
    (interactive)
    (ex-iedit-move-overlays 'evil-previous-line))

(defun ex-iedit-toggle-point-overlay-next-line (count)
    (interactive "p")
    (unless (iedit-find-current-occurrence-overlay)
    (iedit-toggle-selection))
    (let ((i (when count count 1)))
    (save-excursion
        (evil-next-line)
        (while (and (> i 0)
                    (iedit-find-current-occurrence-overlay))
        (1- i)
        (evil-next-line))
        (message "i %s" i)
        (when (not (iedit-find-current-occurrence-overlay))
        (dotimes (j (1+(- count i)))
            (iedit-toggle-selection)
            (evil-next-line))))))

(defun ex-iedit-toggle-point-overlay-previous-line (count)
    (interactive "p")
    (save-excursion
    (dotimes (i count)
        (evil-previous-line)
        (iedit-toggle-selection))))

 (provide 'experimental-iedit)
