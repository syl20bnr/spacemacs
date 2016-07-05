;;; centered-buffer-mode.el --- Minor mode for centering buffers.

;; Copyright (C) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: centering
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

(defvar-local spacemacs--centered-buffer-mode-origin-buffer nil)
(defvar-local spacemacs--centered-buffer-mode-indirect-buffer nil)
(defvar-local spacemacs--centered-buffer-mode-text-pixel-size nil)
(defvar spacemacs--centered-buffer-mode-min-fringe-width 50)

(define-minor-mode spacemacs-centered-buffer-mode
  "Minor mode to center the current buffer.

Hide line number and empty lines indicator(~) if
called interactively."
  :init-value nil
  :group 'editing-basics
  :lighter "-||-"
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
        (spacemacs/toggle-line-numbers-off)
        (let* ((fringe-w (spacemacs//centered-buffer-calc-fringe
                          window)))
          (if (> fringe-w spacemacs--centered-buffer-mode-min-fringe-width)
              (progn
                (setq spacemacs--centered-buffer-mode-origin-buffer origin-buffer
                      spacemacs-centered-buffer-mode t
                      fringes-outside-margins t
                      left-fringe-width fringe-w
                      right-fringe-width fringe-w
                      indicate-empty-lines nil
                      spacemacs--centered-buffer-mode-text-pixel-size
                      (car (window-text-pixel-size window)))
                (face-remap-add-relative 'fringe :background "black")
                (set-window-buffer window indirect-buffer)
                (read-only-mode t)
                (add-hook 'buffer-list-update-hook
                          'spacemacs//centered-buffer-buffer-list-update-fringes)
                (add-hook 'window-configuration-change-hook
                          'spacemacs//centered-buffer-buffer-list-update-fringes))
            (setq spacemacs--centered-buffer-mode-origin-buffer nil)
            (set-buffer origin-buffer)
            (kill-buffer indirect-buffer)
            (setq spacemacs--centered-buffer-mode-indirect-buffer nil)
            (when (called-interactively-p 'any)
              (message "Not enough space to center the buffer!")))))
    (let* ((window (selected-window))
           (origin-buffer spacemacs--centered-buffer-mode-origin-buffer)
           (indirect-buffer (window-buffer window)))
      (setq spacemacs--centered-buffer-mode-origin-buffer nil)
      (switch-to-buffer origin-buffer nil t)
      (setq spacemacs--centered-buffer-mode-indirect-buffer nil)
      (dolist (window (get-buffer-window-list indirect-buffer 2))
        (set-window-buffer window origin-buffer))
      (kill-buffer indirect-buffer))))

(defun spacemacs//centered-buffer-calc-fringe (&optional window text-pixel-size)
  "Calculate fringe width for `spacemacs-centered-buffer-mode'.
Uses text-pixel-size if provided, otherwise calculates it with `window-pixel-width'."
  (-(/ (- (window-pixel-width window)
          (or text-pixel-size
              (car (window-text-pixel-size window))))
       2)
    ;; 20 * 2 extra pixels for the `org-indent' shenanigans
    ;; and 20 as a safe base.
    (if (bound-and-true-p org-indent-mode) 40 20)))

(defun spacemacs//centered-buffer-buffer-list-update-fringes ()
  "Used in `spacemacs//centered-buffer-buffer-list-update-hook' and
`spacemacs//centered-buffer-buffer-window-configuration-change-hook'."
  (dolist (window (window-list))
    (when (and (buffer-local-value 'spacemacs--centered-buffer-mode-origin-buffer
                                   (window-buffer window))
               (buffer-live-p (window-buffer window)))
      (with-selected-window window
        (let ((fringe-w (spacemacs//centered-buffer-calc-fringe
                         window
                         spacemacs--centered-buffer-mode-text-pixel-size)))
          (if (> fringe-w spacemacs--centered-buffer-mode-min-fringe-width)
              (set-window-fringes window fringe-w fringe-w t)
            (spacemacs-centered-buffer-mode -1)))))))

(provide 'centered-buffer-mode)

;;; centered-buffer-mode.el ends here
