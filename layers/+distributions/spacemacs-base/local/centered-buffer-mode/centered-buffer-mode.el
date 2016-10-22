;;; centered-buffer-mode.el --- Minor mode for centering buffers.

;; Copyright (C) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; Keywords: centering buffer minor-mode
;; Created: 1 July 2016
;; Version: 1.01
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

(defgroup spacemacs-centered-buffer-mode nil "Minor mode to center buffer in its window."
  :group 'convenience)

(defcustom spacemacs-centered-buffer-mode-min-fringe-width 50
  "Minimal fringe width."
  :type 'integer
  :group 'spacemacs-centered-buffer-mode)

(defcustom spacemacs-centered-buffer-mode-safety-gap-width 20
  "Add extra width to the modified buffer to make sure
that differed modifications won't cause an overflow."
  :type 'integer
  :group 'spacemacs-centered-buffer-mode)

(defcustom spacemacs-centered-buffer-mode-fringe-color (face-background 'default)
  "Color of the fringes."
  :type 'color
  :group 'spacemacs-centered-buffer-mode)

(defvar-local spacemacs--centered-buffer-mode-origin-buffer nil)
(defvar-local spacemacs--centered-buffer-mode-indirect-buffer nil)
(defvar-local spacemacs--centered-buffer-mode-text-pixel-size nil)
(defvar spacemacs-centered-buffer-mode-default-fringe-color (face-background 'fringe))
(defvar spacemacs--centered-buffer-mode-indirect-buffers (list))

(define-minor-mode spacemacs-centered-buffer-mode
  "Minor mode to center buffer in its window."
  :init-value nil
  :group 'spacemacs-centered-buffer-mode
  (if spacemacs-centered-buffer-mode
      (if (not (window-dedicated-p))
          (spacemacs//centered-buffer-mode-enable-branch (called-interactively-p 'any))
        (setq spacemacs-centered-buffer-mode nil)
        (when (called-interactively-p 'any)
          (message "Can't center in dedicated window!")))
    (spacemacs//centered-buffer-mode-disable-branch)))

(defun spacemacs//centered-buffer-mode-enable-branch (interact)
  "Used it `spacemacs-centered-buffer-mode'.
Assume to be called interactively when INTERACT has non nil value."
  ;; Mode will be applied to the indirect buffer.
  (setq spacemacs-centered-buffer-mode nil)
  ;; Don't run if the mode is enabled(we are in the indirect buffer).
  (unless spacemacs--centered-buffer-mode-origin-buffer
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
      (spacemacs//centered-buffer-mode-buffer-fringr-color-toggle origin-buffer t)
      (setq  spacemacs--centered-buffer-mode-indirect-buffer
             indirect-buffer
             spacemacs-centered-buffer-mode-default-fringe-color
             (face-background 'fringe))
      (when (derived-mode-p 'org-mode)
        (setq-local org-startup-folded nil)
        (outline-show-all))
      (switch-to-buffer indirect-buffer nil t)
      (with-mode-disabled
          page-break-lines-mode
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
                (advice-add 'previous-buffer
                            :before
                            #'spacemacs//centered-buffer-mode-prev-next-buffer-advice)
                (advice-add 'next-buffer
                            :before
                            #'spacemacs//centered-buffer-mode-prev-next-buffer-advice)
                (add-hook 'after-change-functions
                          #'spacemacs//centered-buffer-after-change-function
                          nil
                          t)
                (add-hook 'buffer-list-update-hook
                          'spacemacs//centered-buffer-buffer-list-update-fringes)
                (add-hook 'window-configuration-change-hook
                          'spacemacs//centered-buffer-buffer-list-update-fringes))
            (setq spacemacs--centered-buffer-mode-origin-buffer nil)
            (set-buffer origin-buffer)
            (spacemacs//centered-buffer-mode-buffer-fringr-color-toggle origin-buffer nil)
            (kill-buffer indirect-buffer)
            (setq spacemacs--centered-buffer-mode-indirect-buffer nil)
            (when interact
              (message "Not enough space to center the buffer!"))))))))

(defun spacemacs//centered-buffer-mode-prev-next-buffer-advice ()
  "Disables `spacemacs-centered-buffer-mode' when `spacemacs/previous-buffer'
or `spacemacs/next-buffer' is called."
  (when (bound-and-true-p spacemacs-centered-buffer-mode)
    (spacemacs-centered-buffer-mode -1)))

(defun spacemacs//centered-buffer-mode-disable-branch ()
  "Used in `spacemacs-centered-buffer-mode'."
  ;; Don't run if the mode is disabled(we are not in the indirect buffer).
  (when spacemacs--centered-buffer-mode-origin-buffer
    (let* ((window (selected-window))
           (origin-buffer spacemacs--centered-buffer-mode-origin-buffer)
           (indirect-buffer (window-buffer window)))
      (setq spacemacs--centered-buffer-mode-origin-buffer nil)
      (switch-to-buffer origin-buffer nil t)
      (spacemacs//centered-buffer-mode-buffer-fringr-color-toggle origin-buffer nil)
      (setq spacemacs--centered-buffer-mode-indirect-buffer nil)
      (when (buffer-live-p indirect-buffer)
        (dolist (window (get-buffer-window-list indirect-buffer 2))
          (set-window-buffer window origin-buffer))
        (kill-buffer indirect-buffer)))))

(defun spacemacs//centered-buffer-mode-buffer-fringr-color-toggle (buffer flag)
  "Change fringe color of the BUFFER if FLAG has non-nil value.
Revert changes Otherwise."
  (with-current-buffer buffer
    (if flag
        (face-remap-add-relative 'fringe :background
                                 spacemacs-centered-buffer-mode-fringe-color)
      (face-remap-reset-base 'fringe)
      (face-remap-add-relative 'fringe
                               :background
                               spacemacs-centered-buffer-mode-default-fringe-color))))

(defun spacemacs//centered-buffer-calc-fringe (&optional window text-pixel-size)
  "Calculate fringe width for `spacemacs-centered-buffer-mode'.
Uses text-pixel-size if provided, otherwise calculates it with `window-pixel-width'."
  (-(/ (- (window-pixel-width window)
          (or text-pixel-size
              (car (window-text-pixel-size window))))
       2)
    (if (bound-and-true-p org-indent-mode) 40
      spacemacs-centered-buffer-mode-safety-gap-width)))

(defun spacemacs//centered-buffer-buffer-update-window-fringes (window)
  "Update fringe width of WINDOW if it displays `centered-buffer-mode' buffer."
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
        (spacemacs-centered-buffer-mode -1)
        (when spacemacs--centered-buffer-mode-indirect-buffers
          (spacemacs//centered-buffer-prune-indirect-buffer-list))))))

(defun spacemacs//centered-buffer-buffer-list-update-fringes ()
  "Update fringe width of all `centered-buffer-mode' fringes."
  (dolist (frame (frame-list))
    (when (frame-live-p frame)
      (dolist (window (window-list frame 2))
        (spacemacs//centered-buffer-buffer-update-window-fringes window)))))

(defun spacemacs//centered-buffer-after-change-function (begin end length)
  "Reduce `centered-buffer-mode' fringe width in case of buffer content overflow."
  (dolist (window (get-buffer-window-list (current-buffer) 2 t))
    (save-excursion
      (let* ((min-pos (progn (goto-char begin)
                             (point-at-bol)))
             (max-pos (progn (goto-char end)
                             (point-at-eol)))
             (updated-segment-max-width (car (window-text-pixel-size window min-pos max-pos))))
        (when (> updated-segment-max-width
                 spacemacs--centered-buffer-mode-text-pixel-size)
          (setq spacemacs--centered-buffer-mode-text-pixel-size updated-segment-max-width)
          (spacemacs//centered-buffer-buffer-update-window-fringes window))))))

(defun spacemacs//centered-buffer-prune-indirect-buffer-list ()
  "Remove indirect buffer from the `spacemacs--centered-buffer-mode-indirect-buffers'
if the buffer hasn't at least one live window. Disables `centered-buffer-mode' hooks
and advices if `spacemacs--centered-buffer-mode-indirect-buffers' has no elements left to
minimize the performance hit when the mode isn't used."
  (dolist (buffer spacemacs--centered-buffer-mode-indirect-buffers)
    (unless (or (and (buffer-live-p buffer)
                     (window-live-p (get-buffer-window buffer t)))
                (not spacemacs--centered-buffer-mode-indirect-buffers))
      (let ((origin-buffer (buffer-local-value
                            'spacemacs--centered-buffer-mode-origin-buffer
                            buffer)))
        (when (ignore-errors (kill-buffer buffer))
          (setq spacemacs--centered-buffer-mode-indirect-buffers
                (delete buffer spacemacs--centered-buffer-mode-indirect-buffers))
          (when (buffer-live-p origin-buffer)
            (spacemacs//centered-buffer-mode-buffer-fringr-color-toggle origin-buffer nil)
            (with-current-buffer origin-buffer
              (setq spacemacs--centered-buffer-mode-indirect-buffers nil)))))))
  ;; Remove hooks and advices when they are not needed anymore.
  (unless spacemacs--centered-buffer-mode-indirect-buffers
    (advice-remove 'previous-buffer
                   #'spacemacs//centered-buffer-mode-prev-next-buffer-advice)
    (advice-remove 'next-buffer
                   #'spacemacs//centered-buffer-mode-prev-next-buffer-advice)
    (remove-hook 'after-change-functions
                 #'spacemacs//centered-buffer-after-change-function)
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
