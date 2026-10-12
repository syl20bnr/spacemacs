;;; funcs.el --- Treemacs Layer functions File for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defun spacemacs/treemacs-project-toggle ()
  "Toggle and add the current project to treemacs if not already added."
  (interactive)
  (if (eq (treemacs-current-visibility) 'visible)
      (delete-window (treemacs-get-local-window))
    (let ((path (projectile-ensure-project (projectile-project-root)))
          (name (projectile-project-name)))
      (unless (treemacs-current-workspace)
        (treemacs--find-workspace))
      (treemacs-do-add-project-to-workspace path name)
      (treemacs-select-window))))

(defun spacemacs/treemacs-setup-width-lock ()
  "Setup the width lock of treemacs buffer based on
`treemacs-lock-width'."
  (interactive)
  (unless (eq (not treemacs--width-is-locked)
              (not treemacs-lock-width))
    (treemacs-without-messages
     (treemacs-toggle-fixed-width))))

(defun spacemacs/treemacs-preserved-width (frame)
  "Return the treemacs width to preserve for FRAME.
Fall back to `treemacs-width' until a width has been recorded."
  (or (frame-parameter frame 'spacemacs-treemacs-preserved-width)
      treemacs-width))

(defun spacemacs/treemacs-on-frame-resize (frame)
  "Auto-hide/restore treemacs and maintain its width on FRAME resize."
  (when treemacs-auto-hide-min-width
    (let* ((current-frame-width (frame-width frame))
           (last-frame-width (frame-parameter
                              frame 'spacemacs-treemacs-last-frame-width))
           (width-changed (not (and last-frame-width
                                    (= last-frame-width current-frame-width))))
           (treemacs-window (treemacs-get-local-window))
           (preserved-width (spacemacs/treemacs-preserved-width frame)))
      (set-frame-parameter frame 'spacemacs-treemacs-last-frame-width
                           current-frame-width)
      (when treemacs-window
        (if width-changed
            (with-selected-window treemacs-window
              (treemacs--set-width preserved-width))
          (setq preserved-width (window-width treemacs-window))
          (set-frame-parameter frame 'spacemacs-treemacs-preserved-width
                               preserved-width)))
      (if (< (- current-frame-width preserved-width)
             treemacs-auto-hide-min-width)
          (when treemacs-window
            (if width-changed
                (unless (frame-parameter frame 'spacemacs-treemacs-manual-override)
                  (set-frame-parameter frame 'spacemacs-treemacs-auto-hidden t)
                  (delete-window treemacs-window))
              (set-frame-parameter frame 'spacemacs-treemacs-manual-override t)))
        (set-frame-parameter frame 'spacemacs-treemacs-manual-override nil)
        (when (frame-parameter frame 'spacemacs-treemacs-auto-hidden)
          (set-frame-parameter frame 'spacemacs-treemacs-auto-hidden nil)
          (unless treemacs-window
            (save-selected-window
              (treemacs)
              (when-let* ((restored-window (treemacs-get-local-window)))
                (with-selected-window restored-window
                  (treemacs--set-width preserved-width))))))))))

(defun spacemacs/treemacs-manual-open (&rest _)
  "Prevent automatic re-hiding of treemacs when manually opened in a narrow frame."
  (when treemacs-auto-hide-min-width
    (let ((frame (selected-frame)))
      (when (< (- (frame-width frame) (spacemacs/treemacs-preserved-width frame))
               treemacs-auto-hide-min-width)
        (set-frame-parameter frame 'spacemacs-treemacs-manual-override t)))))
