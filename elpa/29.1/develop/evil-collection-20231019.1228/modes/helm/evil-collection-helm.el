;;; evil-collection-helm.el --- Evil bindings for Helm  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, helm, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for Helm.

;;; Code:
(require 'evil-collection)
(require 'helm-files nil t) ; TODO: Check if this is the ideal requirement and if we are not loading too much.

;; To navigate Helm entries with <hjkl> in insert state, we need a modifier.
;; Using the C- modifier would conflict with the help prefix "C-h".  So we use
;; M- prefixed bindings instead.

;; Helm-find-files: We cannot use "h" and "l" in normal state to navigate up and
;; down the file system hierarchy since we need them to use it to edit the
;; minibuffer content.

(defvar helm-map)
(defvar helm-find-files-map)
(defvar helm-generic-files-map)
(defvar helm-buffer-map)
(defvar helm-occur-map)
(defvar helm-grep-map)
(defvar helm-read-file-map)
(defvar helm-occur-mode-map)
(defvar helm-grep-mode-map)
(defvar helm-echo-input-in-header-line)
(defvar helm--prompt)
(defvar helm--action-prompt)
(defvar helm-header-line-space-before-prompt)
(defvar helm-default-prompt-display-function)

(declare-function helm-window "helm-lib")

(defconst evil-collection-helm-maps '(help-map
                                      help-find-files-map
                                      helm-read-file-map
                                      helm-generic-files-map
                                      helm-buffer-map
                                      helm-occur-map
                                      helm-grep-map))

;; From https://github.com/emacs-helm/helm/issues/362.
;; Also see https://emacs.stackexchange.com/questions/17058/change-cursor-type-in-helm-header-line#17097.
;; TODO: With Evil, the cursor type is not right in the header line and the evil
;; cursor remains in the minibuffer.  Visual selections also reveal overlayed
;; text.
(with-no-warnings
  (defun evil-collection-helm-hide-minibuffer-maybe ()
    "Hide text in minibuffer when `helm-echo-input-in-header-line' is non-nil."
    (when (with-current-buffer (helm-buffer-get) helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil)))))

(defun evil-collection-helm--set-prompt-display (pos)
  (let (beg state region-active m)
    (with-selected-window (minibuffer-window)
      (setq beg (save-excursion (vertical-motion 0 (helm-window)) (point))
            state evil-state
            region-active (region-active-p)
            m (mark t)))
    (when region-active
      (setq m (- m beg))
      ;; Increment pos to handle the space before prompt (i.e `pref').
      (put-text-property (1+ (min m pos)) (+ 2 (max m pos))
                         'face
                         (list :background (face-background 'region))
                         header-line-format))
    (put-text-property
     ;; Increment pos to handle the space before prompt (i.e `pref').
     (+ 1 pos) (+ 2 pos)
     'face
     (if (eq state 'insert)
         'underline
       ;; Don't just use 'cursor, this can hide the current character.
       (list :inverse-video t
             :foreground (face-background 'cursor)
             :background (face-background 'default)))
     header-line-format)))


;;;###autoload
(defun evil-collection-helm-setup ()
  "Set up `evil' bindings for `helm'."
  (add-hook 'helm-minibuffer-set-up-hook 'evil-collection-helm-hide-minibuffer-maybe)
  (setq helm-default-prompt-display-function 'evil-collection-helm--set-prompt-display)
  (custom-set-variables
   '(helm-minibuffer-history-key "M-p"))

  (evil-collection-define-key '(insert normal) 'helm-map
    (kbd "M-[") 'helm-previous-source
    (kbd "M-]") 'helm-next-source
    (kbd "M-l") 'helm-execute-persistent-action
    (kbd "M-j") 'helm-next-line
    (kbd "M-k") 'helm-previous-line
    (kbd "C-f") 'helm-next-page
    (kbd "C-b") 'helm-previous-page)

  (dolist (map '(helm-find-files-map helm-read-file-map))
    (evil-collection-define-key 'normal map
      "go" 'helm-ff-run-switch-other-window
      "/" 'helm-ff-run-find-sh-command)
    (evil-collection-define-key '(insert normal) map
      (kbd "S-<return>") 'helm-ff-run-switch-other-window
      (kbd "M-h") 'helm-find-files-up-one-level))

  ;; TODO: Change the Helm header to display "M-l" instead of "C-l".  We don't
  ;; want to modify the Emacs Helm map.

  (evil-collection-define-key '(insert normal) 'helm-generic-files-map
    (kbd "S-<return>") 'helm-ff-run-switch-other-window)

  (evil-collection-define-key '(insert normal) 'helm-buffer-map
    (kbd "S-<return>") 'helm-buffer-switch-other-window
    (kbd "M-<return>") 'display-buffer)

  (evil-collection-define-key '(insert normal) 'helm-occur-map
    (kbd "S-<return>") 'helm-occur-run-goto-line-ow)

  (evil-collection-define-key '(insert normal) 'helm-grep-map
    (kbd "S-<return>") 'helm-grep-run-other-window-action)

  (evil-collection-define-key 'normal 'helm-generic-files-map
    "go" 'helm-ff-run-switch-other-window)

  (evil-collection-define-key 'normal 'helm-buffer-map
    "go" 'helm-buffer-switch-other-window
    "gO" 'display-buffer
    "=" 'helm-buffer-run-ediff
    "%" 'helm-buffer-run-query-replace-regexp
    "D" 'helm-buffer-run-kill-persistent ; Ivy has "D".
    )

  (evil-collection-define-key 'normal 'helm-occur-map
    "go" 'helm-occur-run-goto-line-ow)

  (evil-collection-define-key 'normal 'helm-grep-map
    "go" 'helm-grep-run-other-window-action)

  (evil-collection-define-key 'normal 'helm-find-files-map
    "=" 'helm-ff-run-ediff-file
    "%" 'helm-ff-run-query-replace-regexp
    "D" 'helm-ff-run-delete-file)       ; Ivy has "D".

  ;; These helm bindings should always exist, the evil equivalents do
  ;; nothing useful in the minibuffer (error or pure failure).
  ;; RET can't do a second line in the minibuffer.
  ;; The C-n/C-p completions error with 'helm in helm' session.
  ;; C-o switches to evil state (again, not useful).
  (evil-collection-define-key '(insert normal) 'helm-map
    (kbd "RET") 'helm-maybe-exit-minibuffer
    (kbd "M-v") 'helm-previous-page
    (kbd "C-v") 'helm-next-page
    (kbd "C-p") 'helm-previous-line
    (kbd "C-n") 'helm-next-line
    (kbd "C-o") 'helm-next-source)

  (when evil-want-C-u-scroll
    (evil-collection-define-key 'normal 'helm-map
      (kbd "C-u") 'helm-previous-page))

  (when evil-want-C-d-scroll
    (evil-collection-define-key 'normal 'helm-map
      (kbd "C-d") 'helm-next-page))

  (evil-collection-define-key 'normal 'helm-map
    (kbd "<tab>") 'helm-select-action   ; TODO: Ivy has "ga".
    (kbd "[[") 'helm-previous-source
    (kbd "]]") 'helm-next-source
    "gk" 'helm-previous-source
    "gj" 'helm-next-source
    (kbd "(") 'helm-prev-visible-mark
    (kbd ")") 'helm-next-visible-mark
    "j" 'helm-next-line
    "k" 'helm-previous-line
    "gg" 'helm-beginning-of-buffer
    "G" 'helm-end-of-buffer

    "/" 'helm-quit-and-find-file

    ;; refresh
    "gr" 'helm-refresh

    "yp" 'helm-yank-selection
    "yP" 'helm-copy-to-buffer
    "yy" 'helm-kill-selection-and-quit
    (kbd "SPC") 'helm-toggle-visible-mark)

  (evil-collection-define-key 'normal 'helm-occur-mode-map
    (kbd "RET") 'helm-occur-mode-goto-line
    (kbd "C-o") 'helm-occur-mode-goto-line-ow
    (kbd "M-n") 'helm-occur-mode-goto-line-ow-forward
    (kbd "M-p") 'helm-occur-mode-goto-line-ow-backward
    "go"        'helm-occur-mode-goto-line-ow
    "gj"        'helm-occur-mode-goto-line-ow-forward
    "gk"        'helm-occur-mode-goto-line-ow-backward
    (kbd "M-N") 'helm-gm-next-file
    (kbd "M-P") 'helm-gm-precedent-file)

  (evil-collection-define-key 'normal 'helm-grep-mode-map
    (kbd "RET") 'helm-grep-mode-jump
    (kbd "C-o") 'helm-grep-mode-jump-other-window
    (kbd "M-n") 'helm-grep-mode-jump-other-window-forward
    (kbd "M-p") 'helm-grep-mode-jump-other-window-backward
    "go"        'helm-grep-mode-jump-other-window
    "gj"        'helm-grep-mode-jump-other-window-forward
    "gk"        'helm-grep-mode-jump-other-window-backward
    (kbd "M-N") 'helm-gm-next-file
    (kbd "M-P") 'helm-gm-precedent-file))

(provide 'evil-collection-helm)
;;; evil-collection-helm.el ends here
