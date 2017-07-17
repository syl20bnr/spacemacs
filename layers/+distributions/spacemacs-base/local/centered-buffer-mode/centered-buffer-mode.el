;;; centered-buffer-mode.el --- Centering minor mode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; Keywords: centering buffer minor-mode
;; Created: 1 July 2016
;; Version: 1.02
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

(defgroup spacemacs-centered-buffer-mode nil
  "Minor mode to center buffer in its window."
  :group 'convenience)

(defcustom spacemacs-centered-buffer-mode-min-fringe-width 50
  "Minimal fringe width. `centered-buffer-mode' will be disabled
if one of windows displaying it becomes too narrow."
  :type 'integer
  :group 'spacemacs-centered-buffer-mode)

(defcustom spacemacs-centered-buffer-mode-min-content-width 400
  "Minimal width of the centered buffer content.
Prevents `centered-buffer-mode' buffers from becoming too thin."
  :type 'integer
  :group 'spacemacs-centered-buffer-mode)

(defcustom spacemacs-centered-buffer-mode-max-content-width 800
  "Maximal width of the centered buffer content before it stops stretching."
  :type 'integer
  :group 'spacemacs-centered-buffer-mode)

(defcustom spacemacs-centered-buffer-mode-safety-gap-width 20
  "Add extra width to the modified buffer to make sure
that differed modifications won't cause an overflow."
  :type 'integer
  :group 'spacemacs-centered-buffer-mode)

(defcustom spacemacs-centered-buffer-mode-fringe-color
  (face-background 'default)
  "Color of the fringes."
  :type 'color
  :group 'spacemacs-centered-buffer-mode)

(defcustom spacemacs-centered-buffer-mode-deferred-update nil
  "Defer buffer re-centering (may improve input latency)."
  :type 'boolean
  :group 'spacemacs-centered-buffer-mode)

(defcustom spacemacs-centered-buffer-mode-only-grow nil
  "Ignore reduction of the centered buffer width.
NOTE: Can be slow with huge buffers."
  :type 'boolean
  :group 'spacemacs-centered-buffer-mode)

(defcustom spacemacs-centered-buffer-mode-visual-line-mode t
  "Enable `visual-line-mode' when `centered-buffer-mode' is enabled."
  :type 'boolean
  :group 'spacemacs-centered-buffer-mode)

(defcustom spacemacs-centered-buffer-mode-mark-origin-as-useless t
  "Mark original buffer as useless when centering."
  :type 'boolean
  :group 'spacemacs-centered-buffer-mode)

(defvar-local spacemacs--centered-buffer-origin-buffer nil)
(defvar-local spacemacs--centered-buffer-indirect-buffer nil)
(defvar-local spacemacs--centered-buffer-text-pixel-size nil)
(defvar spacemacs--centered-buffer-adviced-fontify-fs (list))
(defvar spacemacs-centered-buffer-mode-default-fringe-color (face-background
                                                             'fringe))
(defvar spacemacs--centered-buffer-indirect-buffers (list))
(defvar-local spacemacs--centered-buffer-on-buffer-updated-timer nil)
(defvar-local spacemacs--centered-buffer-update-all-timer nil)
(defvar-local spacemacs--centered-buffer-fontify-region-min-pos nil)
(defvar-local spacemacs--centered-buffer-fontify-region-max-pos nil)
(defvar-local spacemacs--centered-buffer-origin-buffer-old-name nil)

(define-minor-mode spacemacs-centered-buffer-mode
  "Minor mode to center buffer in its window."
  :init-value nil
  :group 'spacemacs-centered-buffer-mode
  (if (display-graphic-p)
      (if spacemacs-centered-buffer-mode
          (if (not (window-dedicated-p))
              (spacemacs//centered-buffer-enable-branch
               (called-interactively-p 'any))
            (setq spacemacs-centered-buffer-mode nil)
            (when (called-interactively-p 'any)
              (message "Can't center in dedicated window!")))
        (spacemacs//centered-buffer-disable-branch))
    (message "centered buffer mode doesn't work in terminal :(")))

(defun spacemacs//centered-buffer-enable-branch (interact)
  "Used it `spacemacs-centered-buffer-mode'.
Assume to be called interactively when INTERACT has non nil value."
  ;; Mode will be applied to the indirect buffer.
  (setq spacemacs-centered-buffer-mode nil)
  ;; Don't run if the mode is enabled(we are in the indirect buffer).
  (unless spacemacs--centered-buffer-origin-buffer
    (when spacemacs-centered-buffer-mode-visual-line-mode
      (visual-line-mode +1))
    (spacemacs//centered-buffer-mode-mark-buff-as-origin nil)
    (let* ((window (selected-window))
           (ori-buff (window-buffer window))
           (origin-buffer-name (buffer-name ori-buff))
           (origin-buffer-new-name
            (spacemacs//centered-buffer-mode-mark-buff-as-origin t))
           (origin-buffer (window-buffer window))
           (indirect-buffer
            (if (buffer-live-p
                 spacemacs--centered-buffer-indirect-buffer)
                spacemacs--centered-buffer-indirect-buffer
              (setq spacemacs--centered-buffer-indirect-buffer
                    (make-indirect-buffer origin-buffer
                                          (format "%s(centered)"
                                                  origin-buffer-name)
                                          t)))))
      (spacemacs//centered-buffer-buffer-fringr-color-toggle origin-buffer t)
      (setq  spacemacs--centered-buffer-indirect-buffer
             indirect-buffer
             spacemacs-centered-buffer-mode-default-fringe-color
             (face-background 'fringe))
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
                (setq spacemacs--centered-buffer-indirect-buffers
                      (append (list indirect-buffer)
                              spacemacs--centered-buffer-indirect-buffers)
                      spacemacs--centered-buffer-origin-buffer origin-buffer
                      buffer-file-name (buffer-file-name origin-buffer)
                      spacemacs-centered-buffer-mode t
                      indicate-empty-lines nil
                      fringes-outside-margins t
                      left-fringe-width fringe-w
                      right-fringe-width fringe-w
                      spacemacs--centered-buffer-on-buffer-updated-timer nil
                      spacemacs--centered-buffer-update-all-timer nil
                      ;; looks better with some margin.
                      left-margin-width (if (or (not left-margin-width)
                                                (= left-margin-width 0))
                                            1
                                          left-margin-width)
                      spacemacs--centered-buffer-text-pixel-size
                      (spacemacs//centered-buffer-window-text-pixel-size
                       window))
                (face-remap-add-relative
                 'fringe
                 :background
                 spacemacs-centered-buffer-mode-fringe-color)
                (set-window-buffer window indirect-buffer)
                (spacemacs//centered-buffer-toggle-hooks t))
            (setq spacemacs--centered-buffer-origin-buffer nil)
            (set-buffer origin-buffer)
            (spacemacs//centered-buffer-mode-mark-buff-as-origin nil)
            (spacemacs//centered-buffer-buffer-fringr-color-toggle origin-buffer
                                                                   nil)
            (with-current-buffer indirect-buffer
              (set-buffer-modified-p nil))
            (kill-buffer indirect-buffer)
            (setq spacemacs--centered-buffer-indirect-buffer nil)
            (when interact
              (message "Not enough space to center the buffer!"))))))))

(defun spacemacs//centered-buffer-mode-mark-buff-as-origin (mark?)
  "If MARK? has non-nil value make sure that the `current-buffer' name has
\"*\" prefix and suffix. Restore old name if MARK? has nil value."
  (let* ((prefix "*")
         (suffix "*")
         (buff (current-buffer))
         (buff-name (buffer-name buff)))
    (if (not mark?)
        (rename-buffer (or spacemacs--centered-buffer-origin-buffer-old-name
                           buff-name))
      (unless (or (and (string-prefix-p prefix buff-name)
                       (string-suffix-p suffix buff-name))
                  (not spacemacs-centered-buffer-mode-mark-origin-as-useless))
        (rename-buffer (concat prefix buff-name suffix)))
      (setq spacemacs--centered-buffer-origin-buffer-old-name buff-name))))

(defun spacemacs//centered-buffer-on-buffer-update-internal (buffer)
  "Calls `spacemacs//centered-buffer-update-fringes' on the buffer
using `spacemacs--centered-buffer-fontify-region-min-pos' and
`spacemacs--centered-buffer-fontify-region-max-pos' as BEGIN END."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when spacemacs--centered-buffer-origin-buffer
        (if spacemacs-centered-buffer-mode-only-grow
            (spacemacs//centered-buffer-update-fringes
             spacemacs--centered-buffer-fontify-region-min-pos
             spacemacs--centered-buffer-fontify-region-max-pos
             buffer)
          (spacemacs//centered-buffer-update-fringes
           (point-min)
           (point-max)
           buffer)))
      (setq spacemacs--centered-buffer-fontify-region-min-pos nil
            spacemacs--centered-buffer-fontify-region-max-pos nil
            spacemacs--centered-buffer-on-buffer-updated-timer nil))))
(byte-compile 'spacemacs//centered-buffer-on-buffer-update-internal)

(defun spacemacs//centered-buffer-on-buffer-updated (begin end _)
  "Schedules call to `spacemacs//centered-buffer-on-buffer-update-internal'
with `run-with-idle-timer'. BEGIN and END are pushed to
`spacemacs--centered-buffer-fontify-region-min-pos' and
`spacemacs--centered-buffer-fontify-region-max-pos'"
  (when spacemacs--centered-buffer-origin-buffer
    (setq
     spacemacs--centered-buffer-fontify-region-min-pos
     (min
      (or spacemacs--centered-buffer-fontify-region-min-pos
          most-positive-fixnum)
      begin)
     spacemacs--centered-buffer-fontify-region-max-pos
     (max
      (or spacemacs--centered-buffer-fontify-region-max-pos
          most-negative-fixnum)
      end))
    (unless spacemacs--centered-buffer-on-buffer-updated-timer
      (setq
       spacemacs--centered-buffer-on-buffer-updated-timer
       (run-with-idle-timer
        (if spacemacs-centered-buffer-mode-deferred-update 0.5 0)
        nil
        'spacemacs//centered-buffer-on-buffer-update-internal
        (current-buffer))))))
(byte-compile 'spacemacs//centered-buffer-on-buffer-updated)

(defun spacemacs//centered-buffer-toggle-hooks (on?)
  "Add `centered-buffer-mode' hooks and advices if ON? non-nil
otherwise remove them."
  (if on?
      (progn
        (unless (= spacemacs-centered-buffer-mode-min-content-width
                   spacemacs-centered-buffer-mode-max-content-width)
          (let ((fontify-func (or font-lock-fontify-region-function
                                  'font-lock-default-fontify-region)))
            (unless (memq fontify-func
                          spacemacs--centered-buffer-adviced-fontify-fs)
              (push fontify-func spacemacs--centered-buffer-adviced-fontify-fs)
              (advice-add fontify-func
                          :after
                          #'spacemacs//centered-buffer-on-buffer-updated)))
          (add-hook 'after-change-functions
                    #'spacemacs//centered-buffer-on-buffer-updated
                    nil
                    t))
        (add-hook 'kill-buffer-hook
                  #'spacemacs//centered-buffer-indirect-buffer-kill-hook)
        (advice-add 'toggle-frame-fullscreen
                    :after
                    #'spacemacs//centered-buffer-window-configuration-change-hk)
        (advice-add 'toggle-frame-maximized
                    :after
                    #'spacemacs//centered-buffer-window-configuration-change-hk)
        (add-hook 'window-configuration-change-hook
                  #'spacemacs//centered-buffer-window-configuration-change-hk))
    (dolist (func spacemacs--centered-buffer-adviced-fontify-fs)
      (when (fboundp func)
        (advice-remove func
                       #'spacemacs//centered-buffer-on-buffer-updated)))
    (setq spacemacs--centered-buffer-adviced-fontify-fs nil)
    (remove-hook 'after-change-functions
                 #'spacemacs//centered-buffer-on-buffer-updated)
    (advice-remove 'toggle-frame-maximized
                   #'spacemacs//centered-buffer-window-configuration-change-hk)
    (advice-remove 'toggle-frame-fullscreen
                   #'spacemacs//centered-buffer-window-configuration-change-hk)
    (remove-hook 'window-configuration-change-hook
                 #'spacemacs//centered-buffer-window-configuration-change-hk)))

(defun spacemacs//centered-buffer-indirect-buffer-kill-hook ()
  "`kill-buffer-hook for `centered-buffer-mode' buffers.'"
  (spacemacs//centered-buffer-cleanup (current-buffer))
  (spacemacs//centered-buffer-prune-indirect-buffer-list))
(byte-compile 'spacemacs//centered-buffer-indirect-buffer-kill-hook)

(defun spacemacs//centered-buffer-disable-branch ()
  "Used in `spacemacs-centered-buffer-mode'."
  ;; Don't run if the mode is disabled(we are not in the indirect buffer).
  (when spacemacs--centered-buffer-origin-buffer
    (let* ((window (selected-window))
           (origin-buffer spacemacs--centered-buffer-origin-buffer)
           (indirect-buffer (window-buffer window)))
      (setq spacemacs--centered-buffer-origin-buffer nil)
      (switch-to-buffer origin-buffer nil t)
      (spacemacs//centered-buffer-buffer-fringr-color-toggle origin-buffer
                                                             nil)
      (setq spacemacs--centered-buffer-indirect-buffer nil)
      (spacemacs//centered-buffer-mode-mark-buff-as-origin nil)
      (when (buffer-live-p indirect-buffer)
        (dolist (window (get-buffer-window-list indirect-buffer 2))
          (set-window-buffer window origin-buffer))
        (with-current-buffer indirect-buffer
          (set-buffer-modified-p nil))
        (kill-buffer indirect-buffer)))))

(defun spacemacs//centered-buffer-buffer-fringr-color-toggle (buffer flag)
  "Change fringe color of the BUFFER if FLAG has non-nil value.
Revert changes Otherwise."
  (with-current-buffer buffer
    (if flag
        (face-remap-add-relative 'fringe :background
                                 spacemacs-centered-buffer-mode-fringe-color)
      (face-remap-reset-base 'fringe)
      (face-remap-add-relative
       'fringe
       :background
       spacemacs-centered-buffer-mode-default-fringe-color))))

(defsubst spacemacs//centered-buffer-window-text-pixel-size (window
                                                             &optional from to)
  "Calculate WINDOW text pixel width with `window-text-pixel-size' in
FROM TO region
and clamp it in between `spacemacs-centered-buffer-mode-min-content-width'
and `spacemacs-centered-buffer-mode-max-content-width'.
If `spacemacs-centered-buffer-mode-min-content-width' and
`spacemacs-centered-buffer-mode-max-content-width' are equal return
their value instead."
  (if (= spacemacs-centered-buffer-mode-min-content-width
         spacemacs-centered-buffer-mode-max-content-width)
      spacemacs-centered-buffer-mode-max-content-width
    (max
     spacemacs-centered-buffer-mode-min-content-width
     (car (window-text-pixel-size
           window
           from
           to
           spacemacs-centered-buffer-mode-max-content-width)))))
(byte-compile 'spacemacs//centered-buffer-window-text-pixel-size)

(defsubst spacemacs//centered-buffer-calc-fringe
  (&optional window text-pixel-size)
  "Calculate fringe width for `spacemacs-centered-buffer-mode'.
The function Uses TEXT-PIXEL-SIZE if provided,
otherwise calculates it with `window-pixel-width'."
  (-(/ (- (window-pixel-width window)
          (or text-pixel-size
              (spacemacs//centered-buffer-window-text-pixel-size
               window)))
       2)
    (if (bound-and-true-p org-indent-mode) 40
      spacemacs-centered-buffer-mode-safety-gap-width)))
(byte-compile 'spacemacs//centered-buffer-calc-fringe)

(defsubst spacemacs//centered-buffer-buffer-update-window-fringes (window)
  "Update fringe width of WINDOW if it displays `centered-buffer-mode' buffer."
  (when (and (buffer-local-value 'spacemacs--centered-buffer-origin-buffer
                                 (window-buffer window))
             ;; Might be needed because
             ;; (spacemacs-centered-buffer-mode -1) kills buffers.
             (buffer-live-p (window-buffer window)))
    (let ((fringe-w (spacemacs//centered-buffer-calc-fringe
                     window
                     (buffer-local-value
                      'spacemacs--centered-buffer-text-pixel-size
                      (window-buffer window)))))
      (if (> fringe-w spacemacs-centered-buffer-mode-min-fringe-width)
          (set-window-fringes window fringe-w fringe-w t)
        (spacemacs-centered-buffer-mode -1)
        (spacemacs//centered-buffer-prune-indirect-buffer-list)))))

(defun spacemacs//centered-buffer-update-all ()
  "Update fringe width of all `centered-buffer-mode' windows."
  (setq spacemacs--centered-buffer-update-all-timer nil)
  (dolist (frame (frame-list))
    (when (frame-live-p frame)
      (dolist (window (window-list frame 2))
        (spacemacs//centered-buffer-buffer-update-window-fringes window)))))
(byte-compile 'spacemacs//centered-buffer-update-all)

(defun spacemacs//centered-buffer-window-configuration-change-hk ()
  "Schedules call to `spacemacs//centered-buffer-update-all'."
  (when (and spacemacs--centered-buffer-indirect-buffers
             (not spacemacs--centered-buffer-update-all-timer))
    (setq
     spacemacs--centered-buffer-update-all-timer
     (run-with-idle-timer
      (if spacemacs-centered-buffer-mode-deferred-update 0.5 0)
      nil
      'spacemacs//centered-buffer-update-all))))
(byte-compile 'spacemacs//centered-buffer-window-configuration-change-hk)

(defun spacemacs//centered-buffer-update-fringes (begin end buffer)
  "Check BEGIN END region pixel width and update fringes width
if BUFFER's content doesn't fit."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (dolist (window (get-buffer-window-list buffer 2 t))
        (save-excursion
          (let* ((min-pos (progn (goto-char begin)
                                 (point-at-bol)))
                 (max-pos (progn (goto-char end)
                                 (point-at-eol)))
                 (updated-segment-max-width
                  (spacemacs//centered-buffer-window-text-pixel-size
                   window
                   min-pos
                   max-pos)))
            (when (or (not spacemacs-centered-buffer-mode-only-grow)
                      (> updated-segment-max-width
                         spacemacs--centered-buffer-text-pixel-size))
              (setq spacemacs--centered-buffer-text-pixel-size
                    updated-segment-max-width))
            (spacemacs//centered-buffer-buffer-update-window-fringes
             window)))))))
(byte-compile 'spacemacs//centered-buffer-update-fringes)

(defsubst spacemacs//centered-buffer-cleanup (buffer)
  "Removes BUFFER from `spacemacs--centered-buffer-indirect-buffers'
and restores origin buffer configurations."
  (let ((origin-buffer (buffer-local-value
                        'spacemacs--centered-buffer-origin-buffer
                        buffer)))
    (setq spacemacs--centered-buffer-indirect-buffers
          (delete buffer
                  spacemacs--centered-buffer-indirect-buffers))
    (when (buffer-live-p origin-buffer)
      (spacemacs//centered-buffer-buffer-fringr-color-toggle
       origin-buffer nil)
      (with-current-buffer origin-buffer
        (setq spacemacs--centered-buffer-indirect-buffers nil)
        (spacemacs//centered-buffer-mode-mark-buff-as-origin nil)))))
(byte-compile 'spacemacs//centered-buffer-cleanup)

(defsubst spacemacs//centered-buffer-prune-indirect-buffer-list ()
  "Cleanups dead buffers with `spacemacs//centered-buffer-cleanup'
and disables `centered-buffer-mode' hooks and advices if
`spacemacs--centered-buffer-indirect-buffers' in empty (nil)."
  (dolist (buffer spacemacs--centered-buffer-indirect-buffers)
    (unless (buffer-live-p buffer)
      (spacemacs//centered-buffer-cleanup buffer)))
  ;; Remove hooks and advices when they are not needed anymore.
  (unless spacemacs--centered-buffer-indirect-buffers
    (spacemacs//centered-buffer-toggle-hooks nil)))

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
