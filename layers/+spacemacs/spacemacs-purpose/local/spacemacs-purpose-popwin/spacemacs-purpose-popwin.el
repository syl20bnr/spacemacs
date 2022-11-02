;;; spacemacs-purpose-popwin.el --- Purpose extension to act like Popwin -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'window-purpose)

(defcustom pupo-split-active-window nil
  "Non-nil if Pupo splits the active window.
Nil if Pupo splits the entire frame."
  :type '(boolean)
  :group 'pupo)

(defconst pupo--direction-to-purpose '((left . popl)
                                       (right . popr)
                                       (top . popt)
                                       (bottom . popb))
  "Mapping of popwin positions to purposes.")

(defconst pupo--purposes
  (cl-loop for (direction . purpose) in pupo--direction-to-purpose collect purpose)
  "List of purposes used to present popwin positions.")

(defvar pupo--windows nil
  "List of popup windows.")
(defvar pupo--auto-windows nil
  "List of popup windows that should be closed automatically.")
(defvar pupo--saved-buffers nil
  "Temporary list of displayed popup buffers.")
(defvar pupo--saved-auto-buffers nil
  "Temporary list of non-sticky displayed popup buffers.")

(defun pupo//popup-function (position size)
  "Generate a display function to create a popup window.
POSITION should be one of bottom, top, left and right.
SIZE should be either a positive number of nil.  Size is interpreted as
width or height depending on POSITION."
  (let* ((size (cl-case position
                 (left (purpose--normalize-width (or size
                                                     popwin:popup-window-width)))
                 (right (purpose--normalize-width (or size
                                                      popwin:popup-window-width)))
                 (top (purpose--normalize-height (or size
                                                     popwin:popup-window-height)))
                 (bottom (purpose--normalize-height (or size
                                                        popwin:popup-window-height)))))
         (size (when size (- size)))
         (side (cl-case position
                 (left 'left)
                 (right 'right)
                 (top 'above)
                 (bottom 'below))))
    (lambda (buffer alist)
      (let* ((main-window (if pupo-split-active-window
                              (selected-window)
                            (frame-root-window)))
             (window (ignore-errors (split-window main-window size side))))
        (when window
          (purpose-change-buffer buffer window 'window alist))))))

(defun pupo//position-to-display-function (position width height)
  "Generate a display function for creating a popup window.
POSITION defaults to bottom.
WIDTH and HEIGHT should be either a positive number or nil."
  (cl-case (or position 'bottom)
    ((left right) (pupo//popup-function position width))
    ((top bottom) (pupo//popup-function position height))))

(defun pupo//position-to-purpose (position)
  "Translate POSITION to a purpose.
Direction -> purpose:
left -> popl
right -> popr
top -> popt
bottom -> popb
POSITION defaults to bottom."
  (cl-case (or position 'bottom)
    ;; names are short so they don't take much room in the mode-line
    (left 'popl)
    (right 'popr)
    (top 'popt)
    (bottom 'popb)))

(defun pupo//actions (settings)
  "Generate list of display functions for displaying a popup window.
SETTINGS is the settings for the popup buffer, and corresponds to what
popwin calls \"config keywords\"."
  (delq nil
        (list #'purpose-display-reuse-window-buffer
              (unless (plist-get settings :dedicated)
                #'purpose-display-reuse-window-purpose)
              (pupo//position-to-display-function (plist-get settings :position)
                                                  (plist-get settings :width)
                                                  (plist-get settings :height)))))

(defun pupo/display-condition (_purpose buffer _alist)
  "A condition to be used in `purpose-special-action-sequences'.
Return non-nil if BUFFER is a popup buffer, according to the settings in
`popwin:special-display-config'.

See `purpose-special-action-sequences' for a description of _PURPOSE,
BUFFER and _ALIST."
  (popwin:match-config buffer))

(defun pupo/display-function (buffer alist)
  "A display function to be used in `purpose-special-action-sequences'.
Display BUFFER as a popup buffer, according to the settings in
`popwin:special-display-config'.

See `purpose-special-action-sequences' for a description of BUFFER and
ALIST."
  (cl-do ((display-fns (pupo//actions (cdr (popwin:match-config buffer)))
                       (cdr display-fns))
          (window nil (and display-fns (funcall (car display-fns) buffer alist))))
      ((or window (null display-fns)) window)))

(defun pupo/after-display (window)
  "Additional initialization for popup windows.
Sets properties for WINDOW and updates some variables, if WINDOW is a
popup window.

This function should be hooked to `purpose-display-buffer-functions'."
  (let* ((buffer (window-buffer window))
         (config (popwin:match-config buffer))
         (settings (cdr (popwin:listify config))))
    (when config
      (setq pupo--windows (delete window pupo--windows))
      (push window pupo--windows)
      (when (plist-get settings :dedicated)
        (set-window-dedicated-p window t))
      (unless (plist-get settings :stick)
        (push window pupo--auto-windows))
      (unless (or (minibuffer-window-active-p (selected-window))
                  (plist-get settings :noselect))
        ;; popwin selects window unless :noselect is t
        ;; in contrast, popwin doesn't prevent selection when :noselect is nil
        (select-window window))
      ;; make \\[C-g] delete last popup window
      (global-set-key [remap keyboard-quit] #'pupo/close-window))))

(defun pupo//safe-delete-window (&optional window)
  "Delete WINDOW if possible.
Return t if successful, nil otherwise.
WINDOW defaults to the selected window."
  (ignore-errors (delete-window window) t))

(defun pupo/auto-delete-windows (window)
  "Delete all non-sticky popup windows, unless WINDOW is a popup window.
This function should be hooked to `purpose-display-buffer-functions'."
  (unless (member (purpose-window-purpose window) pupo--purposes)
    (mapc #'pupo//safe-delete-window pupo--auto-windows)
    (setq pupo--auto-windows nil)))

(defun pupo/close-window ()
  "Close most recent popup window.
This command can be used repeatedly to close all popup windows."
  (interactive)
  (let ((searching t))
    (while (and pupo--windows searching)
      (when (window-live-p (car pupo--windows))
        (pupo//safe-delete-window (car pupo--windows))
        (setq searching nil))
      (pop pupo--windows))
    (unless pupo--windows
      ;; no more popup windows, revert \\[C-g] to `keyboard-quit'
      (global-set-key [remap keyboard-quit] nil))))

(defun pupo/close-all-windows ()
  "Close all popup windows."
  (interactive)
  (dolist (purpose pupo--purposes)
    (mapc #'pupo//safe-delete-window (purpose-windows-with-purpose purpose))))

(defun pupo/popwin-config-to-purpose-config ()
  "Create a purpose configuration matching current popwin's settings.
Return a `purpose-conf' object.
Popwin's settings are taken from `popwin:special-display-config'."
  (let (mode-purposes name-purposes regexp-purposes)
    (cl-loop for config-entry in popwin:special-display-config
          for (pattern . settings) = (popwin:listify config-entry)
          do
          (push (cons pattern
                      (pupo//position-to-purpose (plist-get settings :position)))
                (cond ((symbolp pattern) mode-purposes)
                      ((plist-get settings :regexp) regexp-purposes)
                      (t name-purposes))))
    (purpose-conf :mode-purposes mode-purposes
                  :name-purposes name-purposes
                  :regexp-purposes regexp-purposes)))

(defun pupo/update-purpose-config ()
  "Update purpose configuration according to current popwin's settings.
Popwin's settings are taken from `popwin:special-display-config'."
  (purpose-set-extension-configuration :pupo (pupo/popwin-config-to-purpose-config)))

(define-minor-mode pupo-mode
  "Minor mode for combining `purpose-mode' and `popwin-mode'."
  :global t
  (if pupo-mode
      (progn
        (pupo/update-purpose-config)
        (push '(pupo/display-condition pupo/display-function)
              purpose-special-action-sequences)
        (add-hook 'purpose-display-buffer-functions #'pupo/after-display)
        (add-hook 'purpose-display-buffer-functions #'pupo/auto-delete-windows))
    (purpose-del-extension-configuration :pupo)
    (setq purpose-special-action-sequences
          (delete '(pupo/display-condition pupo/display-function)
                  purpose-special-action-sequences))
    (remove-hook 'purpose-display-buffer-functions #'pupo/after-display)
    (remove-hook 'purpose-display-buffer-functions #'pupo/auto-delete-windows)))

(defadvice popwin:create-popup-window (before pupo/before-popwin-create)
  "Save current popup windows for later restoration.
The windows are restored in `pupo/after-popwin-create'.
Note that the windows themselves aren't saved, but some internal
variables are updated instead."
  (setq pupo--saved-buffers (mapcar #'window-buffer pupo--windows))
  (setq pupo--saved-auto-buffers (mapcar #'window-buffer pupo--auto-windows)))

(defadvice popwin:create-popup-window (after pupo/after-popwin-create)
  "Restore popup windows.
The windows were saved in `pupo/before-popwin-create'.
Note that the windows themselves aren't restored, but some internal
variables are updated instead."
  (setq pupo--windows nil)
  (cl-loop for buffer in pupo--saved-buffers
        do (setq pupo--windows
              (append pupo--windows
                      (get-buffer-window-list buffer))))
  (setq pupo--auto-windows nil)
  (cl-loop for buffer in pupo--saved-auto-buffers
        do (setq pupo--auto-windows
                 (append pupo--auto-windows
                         (get-buffer-window-list buffer)))))

(defun pupo/sync-advices ()
  (if pupo-mode
      (progn
        (ad-enable-advice 'popwin:create-popup-window 'before 'pupo/before-popwin-create)
        (ad-enable-advice 'popwin:create-popup-window 'after 'pupo/after-popwin-create)
        (ad-update 'popwin:create-popup-window)
        (ad-activate 'popwin:create-popup-window))
    (ad-disable-advice 'popwin:create-popup-window 'before 'pupo/before-popwin-create)
    (ad-disable-advice 'popwin:create-popup-window 'after 'pupo/after-popwin-create)
    (ad-update 'popwin:create-popup-window)))
(add-hook 'pupo-mode-hook #'pupo/sync-advices)

(provide 'spacemacs-purpose-popwin)

;;; spacemacs-purpose-popwin.el ends here
