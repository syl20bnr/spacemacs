;;; window-purpose-x.el --- Extensions for Purpose -*- lexical-binding: t -*-

;; Copyright (C) 2015-2021 Bar Magal & contributors

;; Author: Bar Magal
;; Package: purpose

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;; File containing extensions for Purpose.
;; Extensions included:
;; - code1: 4-window display: main edit window, `dired' side window,
;;   `ibuffer' side window and `imenu-list' side window.
;; - magit: purpose configurations for magit.
;; - golden-ratio: make `golden-ratio-mode' work correctly with Purpose.
;; - popup-switcher: command `purpose-x-psw-switch-buffer-with-purpose'
;;   uses `popup-switcher' to switch to another buffer with the same
;;   purpose as the current buffer
;; - popwin: extension that emulates popwin's behavior.
;; - persp: extension to attach purpose configurations to perspectives.
;;   also provides commands such as `purpose-x-persp-switch-buffer'.

;;; Code:

(require 'window-purpose)

;;; --- purpose-x-code1 ---
;;; purpose-x-code1 extension creates a 4-window display:
;;; 1 main window for code buffers (purpose 'edit)
;;; 3 sub windows:
;;; - dired window: show directory of current buffer
;;; - ibuffer window: show currently open files
;;; - imenu-list window: show imenu of current buffer

(require 'dired)
(require 'ibuffer)
(require 'ibuf-ext)
(require 'imenu-list)

(defvar purpose-x-code1--window-layout
  '(nil
    (0 0 152 35)
    (t
     (0 0 29 35)
     (:purpose dired :purpose-dedicated t :width 0.16 :height 0.5 :edges
               (0.0 0.0 0.19333333333333333 0.5))
     (:purpose buffers :purpose-dedicated t :width 0.16 :height 0.4722222222222222 :edges
               (0.0 0.5 0.19333333333333333 0.9722222222222222)))
    (:purpose edit :purpose-dedicated t :width 0.6 :height 0.9722222222222222 :edges
              (0.19333333333333333 0.0 0.8266666666666667 0.9722222222222222))
    (:purpose ilist :purpose-dedicated t :width 0.15333333333333332 :height 0.9722222222222222 :edges
              (0.8266666666666667 0.0 1.0133333333333334 0.9722222222222222)))
  "Window layout for purpose-x-code1-dired-ibuffer.
Has a main 'edit window, and two side windows - 'dired and 'buffers.
All windows are purpose-dedicated.")

;; the name arg ("purpose-x-code1") is necessary for Emacs 24.5 and older
;; (omitting it produces an "Invalid slot name" error)
(defvar purpose-x-code1-purpose-config
  (purpose-conf "purpose-x-code1"
                :mode-purposes
                '((ibuffer-mode . buffers)
                  (dired-mode . dired)
                  (imenu-list-major-mode . ilist))))

(defvar purpose-x-code1-buffers-changed nil
  "Internal variable for use with `frame-or-buffer-changed-p'.")

(define-ibuffer-filter purpose-x-code1-ibuffer-files-only
    "Display only buffers that are bound to files."
  ()
  (buffer-file-name buf))

(defun purpose-x-code1--setup-ibuffer ()
  "Set up ibuffer settings."
  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (ibuffer-filter-by-purpose-x-code1-ibuffer-files-only nil)))
  (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
  (setq ibuffer-formats '((mark " " name)))
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-header-line nil)
  ;; not sure if we want this...
  ;; (setq ibuffer-default-shrink-to-minimum-size t)
  (when (get-buffer "*Ibuffer*")
    (kill-buffer "*Ibuffer*"))
  (save-selected-window
    (ibuffer-list-buffers)))

(defun purpose-x-code1--unset-ibuffer ()
  "Unset ibuffer settings."
  (remove-hook 'ibuffer-mode-hook
               #'(lambda ()
                   (ibuffer-filter-by-purpose-x-code1-ibuffer-files-only nil)))
  (remove-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
  (setq ibuffer-formats '((mark modified read-only " "
                                (name 18 18 :left :elide)
                                " "
                                (size 9 -1 :right)
                                " "
                                (mode 16 16 :left :elide)
                                " " filename-and-process)
                          (mark " "
                                (name 16 -1)
                                " " filename)))
  (setq ibuffer-display-summary t)
  (setq ibuffer-use-header-line t))

(defun purpose-x-code1-update-dired ()
  "Update free dired window with current buffer's directory.
If a non-buffer-dedicated window with purpose 'dired exists, display
the directory of the current buffer in that window, using `dired'.
If there is no window available, do nothing.
If current buffer doesn't have a filename, do nothing."
  (when (and (buffer-file-name)
             (cl-delete-if #'window-dedicated-p
                           (purpose-windows-with-purpose 'dired)))
    (save-selected-window
      (let ((buffer (dired-noselect (file-name-directory (buffer-file-name)))))
        (with-current-buffer buffer
          (when (fboundp 'dired-hide-details-mode)
            (dired-hide-details-mode)))
        (display-buffer buffer))
      (bury-buffer (current-buffer)))))

(defun purpose-x-code1-update-changed ()
  "Update auxiliary buffers if frame/buffer had changed.
Uses `frame-or-buffer-changed-p' to determine whether the frame or
buffer had changed."
  (when (frame-or-buffer-changed-p 'purpose-x-code1-buffers-changed)
    (purpose-x-code1-update-dired)
    (imenu-list-update)))

;;;###autoload
(defun purpose-x-code1-setup ()
  "Setup purpose-x-code1.
This setup includes 4 windows:
1. dedicated 'edit window
2. dedicated 'dired window.  This window shows the current buffer's
directory in a special window, using `dired' and
`dired-hide-details-mode' (if available).
3. dedicated 'buffers window.  This window shows the currently open
files, using `ibuffer'.
4. dedicated 'ilist window.  This window shows the current buffer's
imenu."
  (interactive)
  (purpose-set-extension-configuration :purpose-x-code1 purpose-x-code1-purpose-config)
  (purpose-x-code1--setup-ibuffer)
  (purpose-x-code1-update-dired)
  (imenu-list-minor-mode)
  (frame-or-buffer-changed-p 'purpose-x-code1-buffers-changed)
  (add-hook 'post-command-hook #'purpose-x-code1-update-changed)
  (purpose-set-window-layout purpose-x-code1--window-layout))

(defun purpose-x-code1-unset ()
  "Unset purpose-x-code1."
  (interactive)
  (purpose-del-extension-configuration :purpose-x-code1)
  (purpose-x-code1--unset-ibuffer)
  (imenu-list-minor-mode -1)
  (remove-hook 'post-command-hook #'purpose-x-code1-update-changed))

;;; --- purpose-x-code1 ends here ---



;;; --- purpose-x-magit ---
;;; purpose-x-magit extension provides purpose configuration for magit.
;;; Two configurations available:
;;; - `purpose-x-magit-single-conf': all magit windows have the same purpose
;;;                                  ('Magit)
;;; - `purpose-x-magit-multi-conf': each magit major-mode has a seperate
;;;                                 purpose ('magit-status, 'magit-diff, ...)
;;; Use these commands to enable and disable magit's purpose configurations:
;;; - `purpose-x-magit-single-on'
;;; - `purpose-x-magit-multi-on'
;;; - `purpose-x-magit-off'

(defvar purpose-x-magit-single-conf
  (purpose-conf "magit-single"
                ;; using `magit' as a condition in
                ;; `purpose-special-action-sequences' is interpreted
                ;; as a predicate function (for buffer's without a
                ;; `magit' purpose). `Magit' doesn't have the same
                ;; problem (no function is named Magit), so that's why
                ;; we call the purpose `Magit' and not `magit'.
                :mode-purposes '((magit-mode . Magit)))
  "Configuration that gives each magit major mode the same purpose.")

(defvar purpose-x-magit-multi-conf
  (purpose-conf
   "magit-multi"
   :mode-purposes '((magit-diff-mode . magit-diff)
                    (magit-status-mode . magit-status)
                    (magit-log-mode . magit-log)
                    (magit-commit-mode . magit-commit)
                    (magit-cherry-mode . magit-cherry)
                    (magit-branch-manager-mode . magit-branch-manager)
                    (magit-process-mode . magit-process)
                    (magit-reflog-mode . magit-reflog)
                    (magit-wazzup-mode . magit-wazzup)))
  "Configuration that gives each magit major mode its own purpose.")

(defvar purpose-x-old-magit-display-buffer-function nil
  "Stores `magit-display-buffer-function'.

The value of `magit-display-buffer-function' at the time
`purpose-x-magit-single-on' or `purpose-x-magit-multi-on' is
invoked.")

(defun purpose-x-magit-display-buffer-function (buffer)
  "Integrate `magit' with `window-purpose'."
  (let ((display-buffer-overriding-action '(purpose--action-function . nil)))
    (funcall purpose-x-old-magit-display-buffer-function buffer)))

;;;###autoload
(defun purpose-x-magit-single-on ()
  "Turn on magit-single purpose configuration."
  (interactive)
  (with-eval-after-load 'magit
    ;; if `purpose-x-old-magit-display-buffer-function' is non-nil, then it
    ;; means magit-single-on was activated while magit-single-on or
    ;; magit-multi-on is already active. Magit's variable is already backed up,
    ;; so "backing it up" again will actually override it with a wrong value.
    (unless purpose-x-old-magit-display-buffer-function
      (setq purpose-x-old-magit-display-buffer-function magit-display-buffer-function))
    (setq magit-display-buffer-function 'purpose-x-magit-display-buffer-function))
  (purpose-set-extension-configuration :magit purpose-x-magit-single-conf))

;;;###autoload
(defun purpose-x-magit-multi-on ()
  "Turn on magit-multi purpose configuration."
  (interactive)
  (with-eval-after-load 'magit
    ;; if `purpose-x-old-magit-display-buffer-function' is non-nil, then it
    ;; means magit-multi-on was activated while magit-single-on or
    ;; magit-multi-on is already active. Magit's variable is already backed up,
    ;; so "backing it up" again will actually override it with a wrong value.
    (unless purpose-x-old-magit-display-buffer-function
      (setq purpose-x-old-magit-display-buffer-function magit-display-buffer-function))
    (setq magit-display-buffer-function 'purpose-x-magit-display-buffer-function))
  (purpose-set-extension-configuration :magit purpose-x-magit-multi-conf))

(defun purpose-x-magit-off ()
  "Turn off magit purpose configuration (single or multi)."
  (interactive)
  (purpose-del-extension-configuration :magit)
  (with-eval-after-load 'magit
    (when purpose-x-old-magit-display-buffer-function
      (setq magit-display-buffer-function purpose-x-old-magit-display-buffer-function))
    (setq purpose-x-old-magit-display-buffer-function nil)))

;;; --- purpose-x-magit ends here ---



;;; --- purpose-x-golden-ration ---
;;; Make `purpose-mode' and `golden-ratio-mode' work together properly.
;;; Basically, this adds a hook to `purpose-select-buffer-hook' so
;;; `golden-ratio' is called when a buffer is selected via Purpose.

(defun purpose-x-sync-golden-ratio ()
  "Add/remove `golden-ratio' to `purpose-select-buffer-hook'.
Add `golden-ratio' at the end of `purpose-select-buffer-hook' if
`golden-ratio-mode' is on, otherwise remove it."
  (if golden-ratio-mode
      (add-hook 'purpose-select-buffer-hook #'golden-ratio t)
    (remove-hook 'purpose-select-buffer-hook #'golden-ratio)))

;;;###autoload
(defun purpose-x-golden-ratio-setup ()
  "Make `golden-ratio-mode' aware of `purpose-mode'."
  (interactive)
  (add-hook 'golden-ratio-mode-hook #'purpose-x-sync-golden-ratio)
  (when (and (boundp 'golden-ratio-mode) golden-ratio-mode)
    (add-hook 'purpose-select-buffer-hook #'golden-ratio t)))

(defun purpose-x-golden-ratio-unset ()
  "Make `golden-ratio-mode' forget about `purpose-mode'."
  (interactive)
  (remove-hook 'golden-ratio-mode-hook #'purpose-x-sync-golden-ratio)
  (when (and (boundp 'golden-ratio-mode) golden-ratio-mode)
    (remove-hook 'purpose-select-buffer-hook #'golden-ratio)))

;;; --- purpose-x-golden-ration ends here ---



;;; --- purpose-x-popup-switcher ---
;;; A command for combinining `popup-switcher' with
;;; `purpose-switch-buffer-with-purpose'.
;;; This requires package `popup-switcher'

(when (require 'popup-switcher nil t)
  (defun purpose-x-psw-switch-buffer-with-purpose ()
    "Use `psw-switcher' to open another buffer with the current purpose."
    (interactive)
    (psw-switcher :items-list (purpose-buffers-with-purpose
                               (purpose-buffer-purpose (current-buffer)))
                  :item-name-getter #'buffer-name
                  :switcher #'purpose-switch-buffer)))

;;; --- purpose-x-popup-switcher ends here ---



;;; --- purpose-x-popwin ---
;;; An extension for displaying buffers in a temporary popup-window, similar
;;; to the `popwin' package.

(defcustom purpose-x-popwin-position 'bottom
  "Position for the popup window.
Legal values for this variable are 'top, 'bottom, 'left and 'right.  It
is also possible to set this variable to a function.  That function will
be used to create new popup windows and should be a display function
compatible with `display-buffer'."
  :group 'purpose
  :type '(choice (const top)
                 (const bottom)
                 (const left)
                 (const right)
                 function)
  :package-version '(window-purpose . "1.4"))

(defcustom purpose-x-popwin-width 0.4
  "Width of popup window when displayed at left or right.
Can have the same values as `purpose-display-at-left-width' and
`purpose-display-at-right-width'"
  :group 'purpose
  :type '(choice number
                 (const nil))
  :package-version '(window-purpose . "1.4"))

(defcustom purpose-x-popwin-height 0.35
  "Height of popup window when displayed at top or bottom.
Can have the same values as `purpose-display-at-top-height' and
`purpose-display-at-bottom-height'"
  :group 'purpose
  :type '(choice number
                 (const nil))
  :package-version '(window-purpose . "1.4"))

(defcustom purpose-x-popwin-major-modes '(help-mode
                                          compilation-mode
                                          occur-mode)
  "List of major modes that should be opened as popup windows.
When changing the value of this variable in elisp code, you should call
`purpose-x-popwin-update-conf' for the change to take effect."
  :group 'purpose
  :type '(repeat symbol)
  :set #'(lambda (symbol value)
           (prog1 (set-default symbol value)
             (purpose-x-popwin-update-conf)))
  :initialize 'custom-initialize-default
  :package-version '(window-purpose . "1.4"))

(defcustom purpose-x-popwin-buffer-names '("*Shell Command Output*")
  "List of buffer names that should be opened as popup windows.
Buffers whose name is contained in this list will be opened as popup
windows.
When changing the value of this variable in elisp code, you should call
`purpose-x-popwin-update-conf' for the change to take effect."
  :group 'purpose
  :type '(repeat string)
  :set #'(lambda (symbol value)
           (prog1 (set-default symbol value)
             (purpose-x-popwin-update-conf)))
  :initialize 'custom-initialize-default
  :package-version '(window-purpose . "1.4"))

(defcustom purpose-x-popwin-buffer-name-regexps nil
  "List of regexp that should be opened as popup windows.
Buffers whose name matches a regexp in this list will be opened as popup
windows.
When changing the value of this variable in elisp code, you should call
`purpose-x-popwin-update-conf' for the change to take effect."
  :group 'purpose
  :type '(repeat string)
  :set #'(lambda (symbol value)
           (prog1 (set-default symbol value)
             (purpose-x-popwin-update-conf)))
  :initialize 'custom-initialize-default
  :package-version '(window-purpose . "1.4"))

(defun purpose-x-popupify-purpose (purpose &optional display-fn)
  "Set up a popup-like behavior for buffers with purpose PURPOSE.
DISPLAY-FN is the display function to use for creating the popup window
for purpose PURPOSE, and defaults to `purpose-display-at-bottom'."
  (setq purpose-special-action-sequences
        (cl-delete purpose purpose-special-action-sequences :key #'car))
  (push (list purpose
              #'purpose-display-reuse-window-buffer
              #'purpose-display-reuse-window-purpose
              (or display-fn #'purpose-display-at-bottom))
        purpose-special-action-sequences))

(defun purpose-x-unpopupify-purpose (purpose)
  "Remove popup-like behavior for buffers purpose PURPOSE.
This actually removes any special treatment for PURPOSE in
`purpose-special-action-sequences', not only popup-like behavior."
  (setq purpose-special-action-sequences
        (cl-delete purpose purpose-special-action-sequences :key #'car)))

(defun purpose-x-popwin-update-conf ()
  "Update purpose-x-popwin's purpose configuration.
The configuration is updated according to
`purpose-x-popwin-major-modes', `purpose-x-popwin-buffer-names' and
`purpose-x-popwin-buffer-name-regexps'."
  (interactive)
  (cl-flet ((joiner (x) (cons x 'popup)))
    (let ((conf (purpose-conf
                 "popwin"
                 :mode-purposes (mapcar #'joiner purpose-x-popwin-major-modes)
                 :name-purposes (mapcar #'joiner purpose-x-popwin-buffer-names)
                 :regexp-purposes (mapcar #'joiner
                                          purpose-x-popwin-buffer-name-regexps))))
      (purpose-set-extension-configuration :popwin conf))))

(defun purpose-x-popwin-get-display-function ()
  "Return function for creating new popup windows.
The function is determined by the value of `purpose-x-popwin-position'."
  (or (cl-case purpose-x-popwin-position
        ('top 'purpose-display-at-top)
        ('bottom 'purpose-display-at-bottom)
        ('left 'purpose-display-at-left)
        ('right 'purpose-display-at-right))
      (and (functionp purpose-x-popwin-position)
           purpose-x-popwin-position)
      (user-error "purpose-x-popwin-position has an invalid value: %S"
                  purpose-x-popwin-position)))

(defun purpose-x-popwin-display-buffer (buffer alist)
  "Display BUFFER in a popup window.
See `display-buffer' for the meaning of ALIST."
  (let ((purpose-display-at-top-height purpose-x-popwin-height)
        (purpose-display-at-bottom-height purpose-x-popwin-height)
        (purpose-display-at-left-width purpose-x-popwin-width)
        (purpose-display-at-right-width purpose-x-popwin-width))
    (let ((window
           (funcall (purpose-x-popwin-get-display-function) buffer alist)))
      (purpose-set-window-purpose-dedicated-p window t)
      (purpose-x-popwin-add-hooks)
      window)))

(defun purpose-x-popwin-close-windows ()
  "Delete all popup windows.
Internally, this function works be deleting all windows that have the
'popup purpose.  It also buried all popup buffers so they don't bother
the user when switching buffers."
  (interactive)
  (mapc #'delete-window (purpose-windows-with-purpose 'popup))
  ;; we bury all popup buffers, in case the user poped several popup buffers
  ;; (e.g. help and then occur), so all used popup buffers are buried
  (mapc #'bury-buffer (purpose-buffers-with-purpose 'popup)))

;; additional hooks we might want to use: `post-command-hook',
;; `post-self-insert-hook', `window-configuration-change-hook',
;; `buffer-list-update-hook'
(defun purpose-x-popwin-add-hooks ()
  "Set hooks for closing popup window automatically."
  (global-set-key [remap keyboard-quit]
                  ;; using anonymous command so it's hidden from the user
                  (lambda ()
                    (interactive)
                    (purpose-x-popwin-closer-1 t)))
  (add-hook 'purpose-select-buffer-hook #'purpose-x-popwin-closer-1))

(defun purpose-x-popwin-remove-hooks ()
  "Remove hooks for closing popup window automatically.
This basically is an undo for `purpose-x-popwin-add-hooks'."
  (global-set-key [remap keyboard-quit] nil)
  (remove-hook 'purpose-select-buffer-hook #'purpose-x-popwin-closer-1))

(defun purpose-x-popwin-stick ()
  "Prevent current popup window from being automatically closed.
To cancel, use `purpose-x-popwin-unstick'."
  (interactive)
  (if (purpose-windows-with-purpose 'popup)
      (purpose-x-popwin-remove-hooks)
    (user-error "There is no popup window")))

(defun purpose-x-popwin-unstick ()
  "Allow current popup window to close automatically.
This is the opposite of `purpose-x-popwin-stick'."
  (interactive)
  (if (purpose-windows-with-purpose 'popup)
      (purpose-x-popwin-add-hooks)
    (user-error "There is no popup window")))

(defun purpose-x-popwin-closer-1 (&optional force)
  "Close popup window if appropriate, and remove hooks.
Closes the popup window if the selected window is not a popup window, a
helm window, or a minibuffer window.
If FORCE is non-nil, close popup window regardless to other conditions.
After closing the popup window, the relevant hooks are removed with
`purpose-x-popwin-remove-hooks'.  Note that the hooks are not removed if
the popup window doesn't need to close."
  (unless (and (not force)
               (or (member (purpose-window-purpose) '(helm popup))
                   (window-minibuffer-p)))
    (unwind-protect
        (purpose-x-popwin-close-windows)
      (purpose-x-popwin-remove-hooks))))

(defun purpose-x-popwin-quit-restore-window-advice (fn &optional window bury-or-kill)
  "Close pop up window when there aren't previous buffers can be shown in it."
  (when-let* ((window (ignore-errors (window-normalize-window window t))))
    (funcall fn window bury-or-kill)
    (when (and (window-live-p window)
               ;; quit-restore-window did not kill window
               (null (window-parameter window 'quit-restore))
               (not (window-prev-buffers window)))
      (ignore-errors (delete-window window)))))

;;;###autoload
(defun purpose-x-popwin-setup ()
  "Activate `popwin' emulation.
This extension treats certain buffers as \"popup\" buffers and displays
them in a special popup window.
The window is closed automatically when selecting another buffer (via
`switch-to-buffer' and the like), or by pressing \\[keyboard-quit].
You can control which buffers are treated as popup buffers by changing
the variables `purpose-x-popwin-major-modes',
`purpose-x-popwin-buffer-names' and
`purpose-x-popwin-buffer-name-regexps'.
Look at `purpose-x-popwin-*' variables and functions to learn more."
  (interactive)
  (purpose-x-popwin-update-conf)
  (setq purpose-special-action-sequences
        (cl-delete 'popup purpose-special-action-sequences :key #'car))
  (purpose-x-popupify-purpose 'popup #'purpose-x-popwin-display-buffer)
  (advice-add 'quit-restore-window :around 'purpose-x-popwin-quit-restore-window-advice))

(defun purpose-x-popwin-unset ()
  "Deactivate `popwin' emulation."
  (interactive)
  (purpose-del-extension-configuration :popwin)
  (purpose-x-unpopupify-purpose 'popup)
  (purpose-x-popwin-remove-hooks)
  (advice-remove 'quit-restore-window 'purpose-x-popwin-quit-restore-window-advice))

;;; --- purpose-x-popup ends here ---



;;; --- purpose-x-persp ---
;;; An extension for associating purpose configurations with perspectives.
;;; It activates and deactivates a :perspective purpose-conf extension
;;; automatically when switching perspectives or toggling `persp-mode'. It also
;;; provides switch-buffer commands for switching to a buffer with the same
;;; purpose and perspective as the current buffer
;;; (`purpose-x-persp-switch-buffer', `*-other-windw', `*-other-frame').

(defvar purpose-x-persp-confs (make-hash-table :test 'equal)
  "Hash table holding perspectives' purpose configurations.
The table maps a perspective's name to its purpose configuration.  A
perspective's name is a string (obviously), and its purpose
configuration is a `purpose-conf' object.
To add/remove entries, use:
  (puthash <name> <conf> purpose-x-persp-confs)
  (remhash <name> purpose-x-persp-confs)")

(defun purpose-x-persp-activate ()
  "Activate current perspective's purpose configuration."
  (let ((conf (gethash (persp-name (persp-curr)) purpose-x-persp-confs)))
    (if conf
        (purpose-set-extension-configuration :perspective conf)
      (purpose-x-persp-remove))))

(defun purpose-x-persp-remove ()
  "Remove current perspective's purpose configuration."
  (purpose-del-extension-configuration :perspective))

(defun purpose-x-persp-activate-or-remove ()
  "Activate/remove current perspective's purpose configuration.
Should be hooked to `persp-mode-hook'."
  (if persp-mode
      (purpose-x-persp-activate)
    (purpose-x-persp-remove)))

;;;###autoload
(defun purpose-x-persp-setup ()
  "Activate purpose-x-persp extension.
This extension automatically activates a purpose configuration for the
current perspective.  The configuration changes automatically when
switching perspectives or when toggling `persp-mode'.
The variable `purpose-x-persp-confs' matches between perspectives and
purpose configurations."
  (interactive)
  (unless (fboundp 'persp-mode)
    (user-error "Can't load purpose-x-persp: perspective not available"))
  (add-hook 'persp-switch-hook #'purpose-x-persp-activate)
  (add-hook 'persp-mode-hook #'purpose-x-persp-activate-or-remove)
  (when persp-mode
    (purpose-x-persp-activate)))

(defun purpose-x-persp-unset ()
  "Deactivate purpose-x-persp extension."
  (interactive)
  (remove-hook 'persp-switch-hook #'purpose-x-persp-activate)
  (remove-hook 'persp-mode-hook #'purpose-x-persp-activate-or-remove)
  (purpose-x-persp-remove))

(defun purpose-x-persp-get-buffer-names ()
  "Get names of all buffers with same purpose and perspective as current buffer.
The returned list doesn't contain the current buffer."
  (let ((persp-buffers (persp-buffers (persp-curr))))
    (mapcar #'buffer-name
            (cl-delete-if-not (lambda (buffer) (member buffer persp-buffers))
                              (delete (current-buffer)
                                      (purpose-buffers-with-purpose
                                       (purpose-buffer-purpose
                                        (current-buffer))))))))

;;;###autoload
(defun purpose-x-persp-switch-buffer (buffer &optional norecord force-same-window)
  "Switch to BUFFER, limited by purpose and perspective.
BUFFER is chosen from buffers with the same purpose as the current
buffer that are also part of the current perspective.
NORECORD and FORCE-SAME-WINDOW have the same meaning as in
`switch-to-buffer'."
  (interactive
   (list (completing-read "Switch to buffer: "
                          (purpose-x-persp-get-buffer-names)
                          nil
                          'confirm)))
  (switch-to-buffer buffer norecord force-same-window))

;;;###autoload
(defun purpose-x-persp-switch-buffer-other-window (buffer &optional norecord)
  "Switch to BUFFER in other window, limited by purpose and perspective.
NORECORD has the same meaning as in `switch-to-buffer-other-window'.
The relation between `purpose-x-persp-switch-buffer-other-window' and
`switch-to-buffer-other-window' is the same as the relation between
`purpose-x-persp-switch-buffer' and `switch-to-buffer'."
  (interactive
   (list (completing-read "Switch to buffer in other window: "
                          (purpose-x-persp-get-buffer-names)
                          nil
                          'confirm)))
  (switch-to-buffer-other-window buffer norecord))

;;;###autoload
(defun purpose-x-persp-switch-buffer-other-frame (buffer &optional norecord)
  "Switch to BUFFER in other frame, limited by purpose and perspective.
NORECORD has the same meaning as in `switch-to-buffer-other-frame'.
The relation between `purpose-x-persp-switch-buffer-other-frame' and
`switch-to-buffer-other-frame' is the same as the relation between
`purpose-x-persp-switch-buffer' and `switch-to-buffer'."
  (interactive
   (list (completing-read "Switch to buffer in other frame: "
                          (purpose-x-persp-get-buffer-names)
                          nil
                          'confirm)))
  (switch-to-buffer-other-frame buffer norecord))

;;; --- purpose-x-persp ends here ---



;;; --- purpose-x-kill ---
;;; an extensions that makes emacs respect purpose-dedicated window parameter
;;; when killing a buffer that is visible in a window.

;; copied from `replace-buffer-in-windows' and edited to respect the
;; purpose-dedicated window parameter
(defun purpose-x-replace-buffer-in-windows-1 (&optional buffer-or-name)
  "Replace BUFFER-OR-NAME with some other buffer in all windows showing it.
BUFFER-OR-NAME may be a buffer or the name of an existing buffer and
defaults to the current buffer.

When a window showing BUFFER-OR-NAME is buffer-dedicated, that window is
deleted.  If that window is the only window on its frame, the frame is
deleted too when there are other frames left.  If there are no other
frames left, some other buffer is displayed in that window.

When a window showing BUFFER-OR-NAME is purpose-dedicated, BUFFER-OR-NAME
is replaced with another buffer with the same purpose.  If there are no
other buffers with the same purpose, follow the same rules as if the
window was buffer-dedicated.

This function removes the buffer denoted by BUFFER-OR-NAME from all
window-local buffer lists."
  (interactive "bBuffer to replace: ")
  (let* ((buffer (window-normalize-buffer buffer-or-name))
         ;; Delay calculating other-buffers until we need it
         ;; This prevents unnecessary calculations on temporary
         ;; buffers created by `with-temp-buffer' and other likewise
         ;; non-displayed buffers
         (other-buffers-calculated nil)
         (other-buffers nil))
    (dolist (window (window-list-1 nil nil t))
      (if (eq (window-buffer window) buffer)
          (unless (window--delete window t t)
            (let* ((purpose (purpose-buffer-purpose buffer))
                   (dedicated (purpose-window-purpose-dedicated-p window))
                   (deletable (window-deletable-p window)))
              (unless other-buffers-calculated
                (setq other-buffers (delete buffer (purpose-buffers-with-purpose purpose))
                      other-buffers-calculated t))
              (cond
               ((and dedicated other-buffers)
                ;; dedicated, so replace with a buffer with the same purpose
                (set-window-buffer window (car other-buffers)))
               ((and dedicated deletable (not other-buffers))
                ;; dedicated, but no other buffers with the same purpose, so
                ;; delete the window/frame
                (if (eq deletable 'frame)
                    (delete-frame (window-frame window))
                  (delete-window window)))
               (t
                ;; 1) not dedicated, or 2) dedicated and no other buffers with
                ;; the same purpose, but the window isn't deletable.
                ;; remove dedicated status and switch to previous buffer
                (purpose-set-window-purpose-dedicated-p window nil)
                (set-window-dedicated-p window nil)
                (switch-to-prev-buffer window 'kill)))))
        ;; Unrecord BUFFER in WINDOW.
        (unrecord-window-buffer window buffer)))))

(defun purpose-x-replace-buffer-in-windows (&optional buffer-or-name)
  "Override `replace-buffer-in-windows' with a purpose-aware version."
  (purpose-x-replace-buffer-in-windows-1 buffer-or-name))

(defun purpose-x-kill-sync ()
  "Synchronize `replace-buffer-in-windows' with `purpose-mode'.
If `purpose-mode' is enabled, override `replace-buffer-in-windows' with
`purpose-x-replace-buffer-in-windows'.  If `purpose-mode' is disabled,
cancel the override of `replace-buffer-in-windows'."
  (if purpose-mode
      (advice-add 'replace-buffer-in-windows :override 'purpose-x-replace-buffer-in-windows)
    (advice-remove 'replace-buffer-in-windows 'purpose-x-replace-buffer-in-windows)))

;;;###autoload
(defun purpose-x-kill-setup ()
  "Activate purpose-x-kill extension.
This extension makes `kill-buffer' aware of the purpose-dedicated window
parameter, when killing a visible buffer.  If a buffer that is being
killed is displayed in a window,and that window is purpose-dedicated,
then try to replace the buffer with another buffer with the same purpose.
If that isn't possible, treat the window as if it was buffer-dedicated.

This is implemented by overriding `replace-buffer-in-windows' with
`purpose-x-replace-buffer-in-windows-1'.  See
`purpose-x-replace-buffer-in-windows-1' for more details."
  (interactive)
  (purpose-x-kill-sync)
  (add-hook 'purpose-mode-hook 'purpose-x-kill-sync))

(defun purpose-x-kill-unset ()
  "Deactivate purpose-x-kill extension."
  (interactive)
  (advice-remove 'replace-buffer-in-windows 'purpose-x-replace-buffer-in-windows)
  (remove-hook 'purpose-mode-hook 'purpose-x-kill-sync))

;;; --- purpose-x-kill ends here ---

(provide 'window-purpose-x)
;;; window-purpose-x.el ends here
