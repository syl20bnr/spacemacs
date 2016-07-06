;;; centered-buffer-mode.el --- Minor mode for centering buffers.

;; Copyright (C) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; Keywords: centering buffer minor-mode
;; Created: 1 July 2016
;; Version: 1.00
;; Package-Requires: ((emacs "24.4"))
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

;;; Code:
(require 'face-remap)

(defcustom spacemacs-centered-buffer-mode-min-fringe-width 50
  "Minimal fringe width."
  :type 'integer
  :group 'editing-basics)

(defcustom spacemacs-centered-buffer-mode-safety-gap-width 20
  "Add extra width to the modified buffer to make sure
that differed modifications won't cause an overflow."
  :type 'integer
  :group 'editing-basics)

(defcustom spacemacs-centered-buffer-mode-fringe-color "black"
  "Color of the fringes."
  :type 'color
  :group 'editing-basics)

(defvar-local spacemacs--centered-buffer-mode-origin-buffer nil)
(defvar-local spacemacs--centered-buffer-mode-indirect-buffer nil)
(defvar-local spacemacs--centered-buffer-mode-text-pixel-size nil)
(defvar spacemacs--centered-buffer-mode-indirect-buffers (list))

(define-minor-mode spacemacs-centered-buffer-mode
  "Minor mode to center buffer in its window."
  :init-value nil
  :group 'editing-basics
  (if spacemacs-centered-buffer-mode
      (let* ((window (selected-window))
             (origin-buffer (window-buffer window))
             (indirect-buffer
              (if (buffer-live-p
                   spacemacs--centered-buffer-mode-indirect-buffer)
                  spacemacs--centered-buffer-mode-indirect-buffer
                (setq spacemacs--centered-buffer-mode-indirect-buffer
                      (make-indirect-buffer origin-buffer
                                            (format "%s(centered)"
                                                    origin-buffer)
                                            t)))))
        ;; Mode will be applied to the indirect buffer.
        (setq spacemacs-centered-buffer-mode
              nil
              spacemacs--centered-buffer-mode-indirect-buffer
              indirect-buffer)
        (when (derived-mode-p 'org-mode)
          (setq-local org-startup-folded nil)
          (outline-show-all))
        (switch-to-buffer indirect-buffer nil t)
        (with-mode-disabled page-break-lines-mode
          (let* ((fringe-w (spacemacs//centered-buffer-calc-fringe
                            window)))
            (if (> fringe-w spacemacs-centered-buffer-mode-min-fringe-width)
                (progn
                  ;; Fix visual glitch.
                  (spacemacs/toggle-line-numbers)
                  (spacemacs/toggle-line-numbers)
                  ;;
                  (setq spacemacs--centered-buffer-mode-indirect-buffers
                        (append (list indirect-buffer)
                                spacemacs--centered-buffer-mode-indirect-buffers)
                        spacemacs--centered-buffer-mode-origin-buffer origin-buffer
                        spacemacs-centered-buffer-mode t
                        indicate-empty-lines nil
                        fringes-outside-margins t
                        left-fringe-width fringe-w
                        right-fringe-width fringe-w
                        ;; looks better with some margin.
                        left-margin-width (if (or (not left-margin-width)
                                                  (= left-margin-width 0))
                                              1
                                            left-margin-width)
                        spacemacs--centered-buffer-mode-text-pixel-size
                        (car (window-text-pixel-size window)))
                  (face-remap-add-relative 'fringe :background
                                           spacemacs-centered-buffer-mode-fringe-color)
                  (set-window-buffer window indirect-buffer)
                  (add-hook 'buffer-list-update-hook
                            'spacemacs//centered-buffer-buffer-list-update-fringes)
                  (add-hook 'window-configuration-change-hook
                            'spacemacs//centered-buffer-buffer-list-update-fringes))
              (setq spacemacs--centered-buffer-mode-origin-buffer nil)
              (set-buffer origin-buffer)
              (kill-buffer indirect-buffer)
              (setq spacemacs--centered-buffer-mode-indirect-buffer nil)
              (when (called-interactively-p 'any)
                (message "Not enough space to center the buffer!"))))))
    (let* ((window (selected-window))
           (origin-buffer spacemacs--centered-buffer-mode-origin-buffer)
           (indirect-buffer (window-buffer window)))
      (setq spacemacs--centered-buffer-mode-origin-buffer nil)
      (when (buffer-live-p origin-buffer)
        (switch-to-buffer origin-buffer nil t)
        (setq spacemacs--centered-buffer-mode-indirect-buffer nil))
      (when (buffer-live-p indirect-buffer)
        (dolist (window (get-buffer-window-list indirect-buffer 2))
          (set-window-buffer window origin-buffer))
        (kill-buffer indirect-buffer)))))

(defun spacemacs//centered-buffer-calc-fringe (&optional window text-pixel-size)
  "Calculate fringe width for `spacemacs-centered-buffer-mode'.
Uses text-pixel-size if provided, otherwise calculates it with `window-pixel-width'."
  (-(/ (- (window-pixel-width window)
          (or text-pixel-size
              (car (window-text-pixel-size window))))
       2)
    (if (bound-and-true-p org-indent-mode) 40
      spacemacs-centered-buffer-mode-safety-gap-width)))

(defun spacemacs//centered-buffer-buffer-list-update-fringes ()
  "Used in `spacemacs//centered-buffer-buffer-list-update-hook' and
`spacemacs//centered-buffer-buffer-window-configuration-change-hook'."
  (dolist (frame (frame-list))
    (when (frame-live-p frame)
      (dolist (window (window-list frame 2))
        (when (and (buffer-local-value 'spacemacs--centered-buffer-mode-origin-buffer
                                       (window-buffer window))
                   ;; Might be needed because
                   ;; (spacemacs-centered-buffer-mode -1) kills buffers.
                   (buffer-live-p (window-buffer window)))
          (let ((fringe-w (spacemacs//centered-buffer-calc-fringe
                           window
                           spacemacs--centered-buffer-mode-text-pixel-size)))
            (if (> fringe-w spacemacs-centered-buffer-mode-min-fringe-width)
                (set-window-fringes window fringe-w fringe-w t)
              (spacemacs-centered-buffer-mode -1)))))))
  ;; Prevent premature evaluation.
  (when spacemacs--centered-buffer-mode-indirect-buffers
    (spacemacs//centered-buffer-prune-indirect-buffer-list)))

(defun spacemacs//centered-buffer-prune-indirect-buffer-list ()
  "Remove indirect buffer from the `spacemacs--centered-buffer-mode-indirect-buffers'
if it not displayed. Disables `centered-buffer-mode' hooks
if `spacemacs--centered-buffer-mode-indirect-buffers' has no elements left(nil)."
  (dolist (buffer spacemacs--centered-buffer-mode-indirect-buffers)
   (unless (and (buffer-live-p buffer)
                (window-live-p (get-buffer-window buffer t)))
      (delete buffer 'spacemacs--centered-buffer-mode-indirect-buffers)
      (let ((origin-buffer (buffer-local-value
                            'spacemacs--centered-buffer-mode-origin-buffer
                            buffer)))
        (when (buffer-live-p origin-buffer)
          (with-current-buffer origin-buffer
            (setq spacemacs--centered-buffer-mode-indirect-buffers nil))))
      (kill-buffer buffer)))
  (unless spacemacs--centered-buffer-mode-indirect-buffers
    (remove-hook 'buffer-list-update-hook
                 'spacemacs//centered-buffer-buffer-list-update-fringes)
    (remove-hook 'window-configuration-change-hook
                 'spacemacs//centered-buffer-buffer-list-update-fringes)))

(defmacro with-mode-disabled (mode &rest body)
  "Evaluate BODY with MODE disabled."
  (declare (indent 1) (debug t))
  `(if (not (bound-and-true-p ,mode))
       (progn ,@body)
     (,mode -1)
     ,@body
     (,mode +1)))

(provide 'centered-buffer-mode)

;;; centered-buffer-mode.el ends here
