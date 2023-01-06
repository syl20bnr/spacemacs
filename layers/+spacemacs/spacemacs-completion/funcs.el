;;; funcs.el --- Spacemacs Completion Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
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




;; Helm

(defun spacemacs/helm-faces ()
  "Describe face."
  (interactive)
  (require 'helm-elisp)
  (let ((default (or (face-at-point) (thing-at-point 'symbol))))
    (helm :sources (helm-def-source--emacs-faces
                    (format "%s" (or default "default")))
          :buffer "*helm faces*")))

(defun spacemacs//hide-cursor-in-helm-buffer ()
  "Hide the cursor in helm buffers."
  (with-helm-buffer
    (setq cursor-in-non-selected-windows nil)))

(defun spacemacs//set-dotted-directory ()
  "Set the face of diretories for `.' and `..'"
  (set-face-attribute 'helm-ff-dotted-directory
                      nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'helm-ff-directory))

(defun spacemacs//helm-find-files-enable-helm--in-fuzzy ()
  "Enabling `helm--in-fuzzy' with the hook:
`helm-find-files-after-init-hook'. Fixes the error:
Helm issued errors: helm-match-from-candidates in source `Actions': wrong-type-argument (stringp nil)
When searching in the helm-find-files (`SPC f f') actions (`C-z')."
  (setq helm--in-fuzzy t))

;; Helm Header line

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(defun helm-toggle-header-line ()
  "Hide the `helm' header if there is only one source."
  (when helm-no-header
    (if (> (length helm-sources) 1)
        (set-face-attribute
         'helm-source-header
         nil
         :foreground helm-source-header-default-foreground
         :background helm-source-header-default-background
         :box helm-source-header-default-box
         :height helm-source-header-default-height)
      (set-face-attribute
       'helm-source-header
       nil
       :foreground (face-attribute 'default :background)
       :background (face-attribute 'default :background)
       :box nil
       :height 0.1))))

;; helm navigation on hjkl
(defun spacemacs//helm-hjkl-navigation (style)
  "Set navigation on 'hjkl' for the given editing STYLE."
  (cond
   ((or (eq 'vim style)
        (and (eq 'hybrid style)
             hybrid-style-enable-hjkl-bindings))
    (define-key helm-map (kbd "C-j") 'helm-next-line)
    (define-key helm-map (kbd "C-k") 'helm-previous-line)
    (define-key helm-map (kbd "C-S-j") 'helm-follow-action-forward)
    (define-key helm-map (kbd "C-S-k") 'helm-follow-action-backward)
    (define-key helm-map (kbd "C-h") 'helm-next-source)
    (define-key helm-map (kbd "C-S-h") 'describe-key)
    (define-key helm-map (kbd "C-l") (kbd "RET"))
    (with-eval-after-load 'helm-files
      (dolist (keymap (list helm-find-files-map helm-read-file-map))
        (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
        (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
        ;; rebind `describe-key' for convenience
        (define-key keymap (kbd "C-S-h") 'describe-key))))
   (t
    (define-key helm-map (kbd "C-j") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-k") 'helm-delete-minibuffer-contents)
    (define-key helm-map (kbd "C-h") nil)
    (define-key helm-map
      (kbd "C-l") 'helm-recenter-top-bottom-other-window))))

;; Helm Window position

(defun spacemacs//display-helm-window (buffer &optional resume)
  "Display the Helm window respecting `helm-position'."
  (let ((display-buffer-alist
         (list spacemacs-helm-display-help-buffer-regexp
               ;; this or any specialized case of Helm buffer must be
               ;; added AFTER `spacemacs-helm-display-buffer-regexp'.
               ;; Otherwise, `spacemacs-helm-display-buffer-regexp' will
               ;; be used before
               ;; `spacemacs-helm-display-help-buffer-regexp' and display
               ;; configuration for normal Helm buffer is applied for helm
               ;; help buffer, making the help buffer unable to be
               ;; displayed.
               spacemacs-helm-display-buffer-regexp)))
    (helm-default-display-buffer buffer)))

(defun spacemacs//unprevent-minibuffer-escape ()
  "Workaround for a helm-evil incompatibility.
See https://github.com/syl20bnr/spacemacs/issues/3700"
  (when helm-prevent-escaping-from-minibuffer
    (define-key evil-motion-state-map
      [down-mouse-1] 'evil-mouse-drag-region)))

(defun spacemacs//prevent-minibuffer-escape ()
  "Workaround for a helm-evil incompatibility.
See https://github.com/syl20bnr/spacemacs/issues/3700"
  (when helm-prevent-escaping-from-minibuffer
    (define-key evil-motion-state-map [down-mouse-1] nil)))

;; Helm Transient state

(defun spacemacs//define-helm-action-functions ()
  "Define Spacemacs functions to pick actions."
  (dotimes (n 10)
    (let ((func (intern (format "spacemacs/helm-action-%d" n)))
          (doc (format "Select helm action #%d" n)))
      (eval `(defun ,func ()
               ,doc
               (interactive)
               (helm-select-nth-action ,(1- n)))))))

(defun spacemacs/helm-ts-edit ()
  "Switch in edit mode depending on the current helm buffer."
  (interactive)
  (cond
   ((string-equal "*helm-ag*" helm-buffer)
    (helm-ag-edit))
   ((string-equal "*helm find files*" helm-buffer)
    (spacemacs/helm-find-files-edit))
   ((string-equal "*Helm Swoop*" helm-buffer)
    (helm-swoop-edit))))

(defun spacemacs//helm-navigation-ts-on-enter ()
  "Initialization of helm transient-state."
  ;; faces
  (spacemacs//helm-navigation-ts-set-face)
  (setq spacemacs--helm-navigation-ts-face-cookie-minibuffer
        (face-remap-add-relative
         'minibuffer-prompt
         'spacemacs-helm-navigation-ts-face)))

(defun spacemacs//helm-navigation-ts-set-face ()
  "Set the face for helm header in helm navigation transient-state"
  (with-helm-window
    (setq spacemacs--helm-navigation-ts-face-cookie-header
          (face-remap-add-relative
           'helm-header
           'spacemacs-helm-navigation-ts-face))))

(defun spacemacs//helm-navigation-ts-on-exit ()
  "Action to perform when exiting helm transient-state."
  (with-helm-window
    (face-remap-remove-relative
     spacemacs--helm-navigation-ts-face-cookie-header))
  (face-remap-remove-relative
   spacemacs--helm-navigation-ts-face-cookie-minibuffer))

(defun spacemacs/helm-transient-state-select-action ()
  "Display the Helm actions page."
  (interactive)
  (call-interactively 'helm-select-action)
  (spacemacs//helm-navigation-ts-set-face))


;; Ivy

(defun spacemacs//ivy-hjkl-navigation (style)
  "Set navigation on 'hjkl' for the given editing STYLE."
  (cond
   ((or (eq 'vim style)
        (and (eq 'hybrid style)
             hybrid-style-enable-hjkl-bindings))
    (dolist (map (list ivy-minibuffer-map
                       ivy-switch-buffer-map
                       ivy-reverse-i-search-map))
      (define-key map (kbd "C-j") 'ivy-next-line)
      (define-key map (kbd "C-k") 'ivy-previous-line))
    (define-key ivy-minibuffer-map (kbd "C-h") (kbd "DEL"))
    ;; Move C-h to C-S-h
    (define-key ivy-minibuffer-map (kbd "C-S-h") help-map)
    (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done)
    (define-key ivy-minibuffer-map (kbd "<escape>")
      'minibuffer-keyboard-quit))
   (t
    (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-alt-done)
    (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-kill-line)
    (define-key ivy-minibuffer-map (kbd "C-h") nil)
    (define-key ivy-minibuffer-map (kbd "C-l") nil))))

(defun spacemacs//ivy-matcher-desc ()
  (replace-regexp-in-string "ivy--" "" (format "%s" ivy--regex-function)))


;; Ido

(defun spacemacs//ido-minibuffer-setup ()
  "Setup the minibuffer."
  ;; Since ido is implemented in a while loop where each
  ;; iteration setup a whole new minibuffer, we have to keep
  ;; track of any activated ido navigation transient-state and force
  ;; the reactivation at each iteration.
  (when spacemacs--ido-navigation-ts-enabled
    (spacemacs/ido-navigation-transient-state/body)))

(defun spacemacs//ido-setup ()
  (when spacemacs--ido-navigation-ts-face-cookie-minibuffer
    (face-remap-remove-relative
     spacemacs--ido-navigation-ts-face-cookie-minibuffer))
  ;; be sure to wipe any previous transient-state flag
  (setq spacemacs--ido-navigation-ts-enabled nil)
  ;; overwrite the key bindings for ido vertical mode only
  (define-key ido-completion-map (kbd "C-<return>") 'ido-select-text)
  ;; use M-RET in terminal
  (define-key ido-completion-map "\M-\r" 'ido-select-text)
  (define-key ido-completion-map (kbd "C-h") 'ido-delete-backward-updir)
  (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-k") 'ido-prev-match)
  (define-key ido-completion-map (kbd "C-l") 'ido-exit-minibuffer)
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  (define-key ido-completion-map (kbd "C-S-h") 'ido-prev-match-dir)
  (define-key ido-completion-map (kbd "C-S-j") 'next-history-element)
  (define-key ido-completion-map (kbd "C-S-k") 'previous-history-element)
  (define-key ido-completion-map (kbd "C-S-l") 'ido-next-match-dir)
  (define-key ido-completion-map (kbd "C-S-n") 'next-history-element)
  (define-key ido-completion-map (kbd "C-S-p") 'previous-history-element)
  ;; ido-other window maps
  (define-key ido-completion-map (kbd "C-o") 'spacemacs/ido-invoke-in-other-window)
  (define-key ido-completion-map (kbd "C-s") 'spacemacs/ido-invoke-in-vertical-split)
  (define-key ido-completion-map (kbd "C-t") 'spacemacs/ido-invoke-in-new-frame)
  (define-key ido-completion-map (kbd "C-v") 'spacemacs/ido-invoke-in-horizontal-split)
  ;; initiate transient-state
  (define-key ido-completion-map (kbd "M-SPC") 'spacemacs/ido-navigation-transient-state/body)
  (define-key ido-completion-map (kbd "S-M-SPC") 'spacemacs/ido-navigation-transient-state/body))

(defun spacemacs/ido-invoke-in-other-window ()
  "signals ido mode to switch to (or create) another window after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'other)
  (ido-exit-minibuffer))

(defun spacemacs/ido-invoke-in-horizontal-split ()
  "signals ido mode to split horizontally and switch after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'horizontal)
  (ido-exit-minibuffer))

(defun spacemacs/ido-invoke-in-vertical-split ()
  "signals ido mode to split vertically and switch after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'vertical)
  (ido-exit-minibuffer))

(defun spacemacs/ido-invoke-in-new-frame ()
  "signals ido mode to create a new frame after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'frame)
  (ido-exit-minibuffer))

(defun spacemacs//ido-navigation-ts-set-face ()
  "Set faces for ido navigation transient-state."
  (setq spacemacs--ido-navigation-ts-face-cookie-minibuffer
        (face-remap-add-relative
         'minibuffer-prompt
         'spacemacs-ido-navigation-ts-face)))

(defun spacemacs//ido-navigation-ts-on-enter ()
  "Initialization of ido transient-state."
  (setq spacemacs--ido-navigation-ts-enabled t)
  (spacemacs//ido-navigation-ts-set-face))

(defun spacemacs//ido-navigation-ts-on-exit ()
  "Action to perform when exiting ido transient-state."
  (face-remap-remove-relative
   spacemacs--ido-navigation-ts-face-cookie-minibuffer))

(defun spacemacs//ido-navigation-ts-full-doc ()
  "Full documentation for ido navigation transient-state."
  "
 [?]          display this help
 [e]          enter dired
 [j] [k]      next/previous match
 [J] [K]      sub/parent directory
 [h]          delete backward or parent directory
 [l]          select match
 [n] [p]      next/previous directory in history
 [o]          open in other window
 [s]          open in a new horizontal split
 [t]          open in other frame
 [v]          open in a new vertical split
 [q]          quit")
