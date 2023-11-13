;;; eyebrowse.el --- Easy window config switching  -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Vasilij Schneidermann <mail@vasilij.de>

;; Author: Vasilij Schneidermann <mail@vasilij.de>
;; URL: https://depp.brause.cc/eyebrowse
;; Version: 0.7.8
;; Package-Requires: ((dash "2.7.0") (emacs "24.3.1"))
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This global minor mode provides a set of keybindings for switching
;; window configurations.  It tries mimicking the tab behaviour of
;; `ranger`, a file manager.

;; See the README for more info: https://depp.brause.cc/eyebrowse

;;; Code:

(require 'dash)
(require 'format-spec)


;;; variables

(defgroup eyebrowse nil
  "A window configuration switcher modeled after the ranger file
manager."
  :group 'convenience
  :prefix "eyebrowse-")

(defcustom eyebrowse-keymap-prefix (kbd "C-c C-w")
  "Prefix key for key-bindings."
  :type 'string
  :group 'eyebrowse)

(defface eyebrowse-mode-line-delimiters
  '((t (nil)))
  "Face for the mode line indicator delimiters."
  :group 'eyebrowse)

(defface eyebrowse-mode-line-separator
  '((t (nil)))
  "Face for the mode line indicator separator."
  :group 'eyebrowse)

(defface eyebrowse-mode-line-inactive
  '((t (nil)))
  "Face for the inactive items of the mode line indicator."
  :group 'eyebrowse)

(defface eyebrowse-mode-line-active
  '((t (:inherit mode-line-emphasis)))
  "Face for the active items of the mode line indicator."
  :group 'eyebrowse)

(defcustom eyebrowse-mode-line-separator ", "
  "Separator of the mode line indicator."
  :type 'string
  :group 'eyebrowse)

(defcustom eyebrowse-mode-line-left-delimiter "["
  "Left delimiter of the mode line indicator."
  :type 'string
  :group 'eyebrowse)

(defcustom eyebrowse-mode-line-right-delimiter "]"
  "Right delimiter of the mode line indicator."
  :type 'string
  :group 'eyebrowse)

(defcustom eyebrowse-mode-line-style 'smart
  "The mode line indicator style may be one of the following:

nil, 'hide: Don't show at all.

'smart: Hide when only one window config.

'current: Only show current config.

t, 'always: Always show."
  :type '(choice (const :tag "Hide" hide)
                 (const :tag "Smart" smart)
                 (const :tag "Always" always)
                 (const :tag "Current" current))
  :group 'eyebrowse)

(defcustom eyebrowse-wrap-around nil
  "Wrap around when switching to the next/previous window config?
If t, wrap around."
  :type 'boolean
  :group 'eyebrowse)

(defcustom eyebrowse-switch-back-and-forth nil
  "Switch to the last window automatically?
If t, switching to the same window config as
`eyebrowse-current-window-config', switches to
`eyebrowse-last-window-config'."
  :type 'boolean
  :group 'eyebrowse)

(defcustom eyebrowse-new-workspace nil
  "Type of the new workspace.
It may be one of the following:

nil: Clone last workspace.

string value: Clean up and display a buffer with that name.  If
  necessary, create the buffer beforehand.

symbol name: Clean up and call the specified function.

t: Clean up and display the scratch buffer."
  :type '(choice (const :tag "Clone last workspace." nil)
                 (string :tag "Switch to buffer name.")
                 (function :tag "Initialize with function.")
                 (const :tag "Switch to scratch buffer." t))
  :group 'eyebrowse)

(defcustom eyebrowse-pre-window-switch-hook nil
  "Hook run before switching to a window config."
  :type 'hook
  :group 'eyebrowse)
(add-hook 'eyebrowse-pre-window-switch-hook 'deactivate-mark)

(defcustom eyebrowse-post-window-switch-hook nil
  "Hook run after switching to a window config."
  :type 'hook
  :group 'eyebrowse)

(defcustom eyebrowse-pre-window-delete-hook nil
  "Hook run before deleting a window config."
  :type 'hook
  :group 'eyebrowse)

(defcustom eyebrowse-post-window-delete-hook nil
  "Hook run after deleting a window config."
  :type 'hook
  :group 'eyebrowse)

(defcustom eyebrowse-default-workspace-slot 1
  "Slot number assigned to the default workspace."
  :type 'integer
  :group 'eyebrowse)

(defcustom eyebrowse-slot-format "%s"
  "Format string for untagged slots.
The following format codes are supported:

%s: Current slot"
  :type 'string
  :group 'eyebrowse)

(defcustom eyebrowse-tagged-slot-format "%s:%t"
  "Format string for tagged slots.
The following format codes are supported:

%t: Tag

%s: Current slot"
  :type 'string
  :group 'eyebrowse)

(defcustom eyebrowse-close-window-config-prompt nil
  "Ask user for confirmation when closing a window config?
If t, ask for confirmation."
  :type 'boolean
  :group 'eyebrowse)

(defvar eyebrowse-mode-map
  (let ((map (make-sparse-keymap))
        (prefix-map (make-sparse-keymap)))
    (define-key prefix-map (kbd "<") 'eyebrowse-prev-window-config)
    (define-key prefix-map (kbd ">") 'eyebrowse-next-window-config)
    (define-key prefix-map (kbd "'") 'eyebrowse-last-window-config)
    (define-key prefix-map (kbd "\"") 'eyebrowse-close-window-config)
    (define-key prefix-map (kbd ",") 'eyebrowse-rename-window-config)
    (define-key prefix-map (kbd ".") 'eyebrowse-switch-to-window-config)
    (define-key prefix-map (kbd "0") 'eyebrowse-switch-to-window-config-0)
    (define-key prefix-map (kbd "1") 'eyebrowse-switch-to-window-config-1)
    (define-key prefix-map (kbd "2") 'eyebrowse-switch-to-window-config-2)
    (define-key prefix-map (kbd "3") 'eyebrowse-switch-to-window-config-3)
    (define-key prefix-map (kbd "4") 'eyebrowse-switch-to-window-config-4)
    (define-key prefix-map (kbd "5") 'eyebrowse-switch-to-window-config-5)
    (define-key prefix-map (kbd "6") 'eyebrowse-switch-to-window-config-6)
    (define-key prefix-map (kbd "7") 'eyebrowse-switch-to-window-config-7)
    (define-key prefix-map (kbd "8") 'eyebrowse-switch-to-window-config-8)
    (define-key prefix-map (kbd "9") 'eyebrowse-switch-to-window-config-9)
    (define-key prefix-map (kbd "c") 'eyebrowse-create-window-config)
    (define-key prefix-map (kbd "C-c") 'eyebrowse-create-window-config)
    (define-key map eyebrowse-keymap-prefix prefix-map)
    map)
  "Initial key map for `eyebrowse-mode'.")


;;; functions

(defun eyebrowse--get (type &optional frame)
  "Retrieve frame-specific value of TYPE.
If FRAME is nil, use current frame.  TYPE can be any of
'window-configs, 'current-slot, 'last-slot."
  (cond
   ((eq type 'window-configs)
    (frame-parameter frame 'eyebrowse-window-configs))
   ((eq type 'current-slot)
    (frame-parameter frame 'eyebrowse-current-slot))
   ((eq type 'last-slot)
    (frame-parameter frame 'eyebrowse-last-slot))))

(defun eyebrowse--set (type value &optional frame)
  "Set frame-specific value of TYPE to VALUE.
If FRAME is nil, use current frame.  TYPE can be any of
'window-configs, 'current-slot, 'last-slot."
  (cond
   ((eq type 'window-configs)
    (set-frame-parameter frame 'eyebrowse-window-configs value))
   ((eq type 'current-slot)
    (set-frame-parameter frame 'eyebrowse-current-slot value))
   ((eq type 'last-slot)
    (set-frame-parameter frame 'eyebrowse-last-slot value))))
(put 'eyebrowse--set 'lisp-indent-function 1)

(defun eyebrowse-init (&optional frame)
  "Initialize Eyebrowse for the current frame."
  (unless (eyebrowse--get 'window-configs frame)
    (eyebrowse--set 'last-slot eyebrowse-default-workspace-slot frame)
    (eyebrowse--set 'current-slot eyebrowse-default-workspace-slot frame)
    (eyebrowse--insert-in-window-config-list
     (eyebrowse--current-window-config eyebrowse-default-workspace-slot "")
     frame)))

(defun eyebrowse--update-window-config-element (new-element)
  "Replace the old element with NEW-ELEMENT in the window config list.
The old element is identified by the first element of NEW-ELEMENT."
  (eyebrowse--set 'window-configs
    (--replace-where (= (car it) (car new-element))
                     new-element (eyebrowse--get 'window-configs))))

(defun eyebrowse--insert-in-window-config-list (element &optional frame)
  "Insert ELEMENT in the list of window configs.
This function keeps the sortedness intact."
  (let* ((window-configs (eyebrowse--get 'window-configs frame))
         (index (--find-last-index (< (car it) (car element)) window-configs)))
    (eyebrowse--set 'window-configs
      (-insert-at (if index (1+ index) 0) element window-configs) frame)))

(defun eyebrowse--window-config-present-p (slot &optional frame)
  "Non-nil if there is a window config at SLOT."
  (assq slot (eyebrowse--get 'window-configs frame)))

(defun eyebrowse--current-window-config (slot tag)
  "Returns a window config list appliable for SLOT."
  (list slot (window-state-get nil t) tag))

(defun eyebrowse--dotted-list-p (list)
  "Non-nil if LIST is terminated by a non-nil value."
  (cdr (last list)))

(defun eyebrowse--walk-window-config (window-config function)
  "Walk through WINDOW-CONFIG and apply FUNCTION to each leaf."
  (dolist (item window-config)
    (when (consp item)
      (when (symbolp (car item))
        (funcall function item))
      (when (and (consp (cdr item))
                 (not (eyebrowse--dotted-list-p (cdr item))))
        (eyebrowse--walk-window-config (cdr item) function)))))

(defun eyebrowse--fixup-window-config (window-config)
  "Walk through WINDOW-CONFIG and fix it up destructively.
If a no longer existent buffer is encountered, it is replaced
with the scratch buffer."
  (eyebrowse--walk-window-config
   window-config
   (lambda (item)
     (when (eq (car item) 'buffer)
       (let* ((buffer-name (cadr item))
              (buffer (get-buffer buffer-name)))
         (when (not buffer)
           (message "Replaced deleted %s buffer with *scratch*" buffer-name)
           (setf (cadr item) "*scratch*")))))))

(defun eyebrowse--rename-window-config-buffers (window-config old new)
  "Walk through WINDOW-CONFIG and rename buffers when appropriate.
If a buffer name equal to OLD is found, it is replaced with NEW."
  (eyebrowse--walk-window-config
   window-config
   (lambda (item)
     (when (eq (car item) 'buffer)
       (let ((buffer-name (cadr item)))
         (when (equal buffer-name old)
           (setf (cadr item) new)))))))

(defadvice rename-buffer (around eyebrowse-fixup-window-configs activate)
  "Replace buffer names in all window configs."
  (let ((old (buffer-name)))
    ad-do-it
    (let ((new ad-return-value))
      (dolist (frame (frame-list))
        (dolist (window-config (eyebrowse--get 'window-configs frame))
          (eyebrowse--rename-window-config-buffers window-config old new))))
    ad-return-value))

(defun eyebrowse--load-window-config (slot)
  "Restore the window config from SLOT."
  (-when-let (match (assq slot (eyebrowse--get 'window-configs)))
    ;; KLUDGE: workaround for #36
    ;; see also http://debbugs.gnu.org/cgi/bugreport.cgi?bug=20848
    (when (version< emacs-version "25")
      (delete-other-windows)
      (set-window-dedicated-p nil nil))
    ;; KLUDGE: workaround for visual-fill-column foolishly
    ;; setting the split-window parameter
    (let ((ignore-window-parameters t)
          (window-config (cadr match)))
      (eyebrowse--fixup-window-config window-config)
      (window-state-put window-config (frame-root-window) 'safe))))

(defun eyebrowse--string-to-number (x)
  "Version of `string-to-number' that returns nil if not a number."
  (let ((result (string-to-number x)))
    (if (and (zerop result)
             (not (string-match-p (rx bos (* white) "0") x)))
        nil
      result)))

(defun eyebrowse--read-slot ()
  "Read in a window config SLOT to switch to.
A formatted list of window configs is presented as candidates.
If no match was found, the user input is interpreted as a new
slot to switch to."
  (let* ((candidates (--map (cons (eyebrowse-format-slot it)
                                  (car it))
                            (eyebrowse--get 'window-configs)))
         (candidate (completing-read "Enter slot: " candidates))
         (choice (cdr (assoc candidate candidates))))
    (or choice (eyebrowse--string-to-number candidate)
        (user-error "Invalid slot number"))))

(defun eyebrowse-switch-to-window-config (slot)
  "Switch to the window config SLOT.
This will save the current window config to
`eyebrowse-current-slot' first, then switch.  If
`eyebrowse-switch-back-and-forth' is t and
`eyebrowse-current-slot' equals SLOT, this will switch to the
last window config."
  (interactive (list (if (numberp current-prefix-arg)
                         current-prefix-arg
                       (eyebrowse--read-slot))))
  (when slot
    (let* ((current-slot (eyebrowse--get 'current-slot))
           (window-configs (eyebrowse--get 'window-configs))
           (current-tag (nth 2 (assoc current-slot window-configs)))
           (last-slot (eyebrowse--get 'last-slot)))
      (when (and eyebrowse-switch-back-and-forth (= current-slot slot))
        (setq slot last-slot))
      (let ((new-window-config (not (eyebrowse--window-config-present-p slot))))
        (when (/= current-slot slot)
          (run-hooks 'eyebrowse-pre-window-switch-hook)
          (eyebrowse--update-window-config-element
           (eyebrowse--current-window-config current-slot current-tag))
          (when new-window-config
            (eyebrowse--insert-in-window-config-list
             (eyebrowse--current-window-config slot "")))
          (eyebrowse--load-window-config slot)
          (eyebrowse--set 'last-slot current-slot)
          (eyebrowse--set 'current-slot slot)
          (when (and new-window-config eyebrowse-new-workspace)
            (delete-other-windows)
            (cond
             ((stringp eyebrowse-new-workspace)
              (switch-to-buffer (get-buffer-create eyebrowse-new-workspace)))
             ((functionp eyebrowse-new-workspace)
              (funcall eyebrowse-new-workspace))
             (t (switch-to-buffer "*scratch*"))))
          (run-hooks 'eyebrowse-post-window-switch-hook))))))

(defun eyebrowse-next-window-config (count)
  "Switch to the next available window config.
If `eyebrowse-wrap-around' is t, this will switch from the last
to the first one.  When used with a numerical argument, switch to
window config COUNT."
  (interactive "P")
  (let* ((window-configs (eyebrowse--get 'window-configs))
         (match (assq (eyebrowse--get 'current-slot) window-configs))
         (index (-elem-index match window-configs)))
    (if count
        (eyebrowse-switch-to-window-config count)
      (when (and index (> (length window-configs) 1))
        (if (< (1+ index) (length window-configs))
            (eyebrowse-switch-to-window-config
             (car (nth (1+ index) window-configs)))
          (when eyebrowse-wrap-around
            (eyebrowse-switch-to-window-config
             (caar window-configs))))))))

(defun eyebrowse-prev-window-config (count)
  "Switch to the previous available window config.
If `eyebrowse-wrap-around' is t, this will switch from the
first to the last one.  When used with a numerical argument,
switch COUNT window configs backwards and always wrap around."
  (interactive "P")
  (let* ((window-configs (eyebrowse--get 'window-configs))
         (match (assq (eyebrowse--get 'current-slot) window-configs))
         (index (-elem-index match window-configs)))
    (if count
        (let ((eyebrowse-wrap-around t))
          (eyebrowse-prev-window-config
           (when (> count 1)
             (eyebrowse-prev-window-config (1- count)))))
      (when (and index (> (length window-configs) 1))
        (if (> index 0)
            (eyebrowse-switch-to-window-config
             (car (nth (1- index) window-configs)))
          (when eyebrowse-wrap-around
            (eyebrowse-switch-to-window-config
             (caar (last window-configs)))))))))

(defun eyebrowse-last-window-config ()
  "Switch to the last window config."
  (interactive)
  (eyebrowse-switch-to-window-config (eyebrowse--get 'last-slot)))

(defun eyebrowse--delete-window-config (slot)
  "Remove the window config at SLOT."
  (let ((window-configs (eyebrowse--get 'window-configs)))
    (eyebrowse--set 'window-configs
      (remove (assq slot window-configs) window-configs))))

(defun eyebrowse-close-window-config ()
  "Close the current window config.
This removes it from `eyebrowse-window-configs' and switches to
another appropriate window config."
  (interactive)
  (let ((window-configs (eyebrowse--get 'window-configs)))
    (when (and (> (length window-configs) 1)
               (or (not eyebrowse-close-window-config-prompt)
                   (yes-or-no-p "Close current window config?")))
      (if (equal (assq (eyebrowse--get 'current-slot) window-configs)
                 (car (last window-configs)))
          (eyebrowse-prev-window-config nil)
        (eyebrowse-next-window-config nil))
      (run-hooks 'eyebrowse-pre-window-delete-hook)
      (eyebrowse--delete-window-config (eyebrowse--get 'last-slot))
      (run-hooks 'eyebrowse-post-window-delete-hook))))

(defun eyebrowse-rename-window-config (slot tag)
  "Rename the window config at SLOT to TAG.
When used interactively, default to the current window config,
use the prefix argument to prompt for a slot or a numerical
prefix argument to select a slot by its number."
  (interactive (list (cond
                      ((consp current-prefix-arg)
                       (eyebrowse--read-slot))
                      ((numberp current-prefix-arg)
                       current-prefix-arg)
                      (t (eyebrowse--get 'current-slot)))
                     nil))
  (let* ((window-configs (eyebrowse--get 'window-configs))
         (window-config (assoc slot window-configs))
         (current-tag (nth 2 window-config))
         (tag (or tag (read-string "Tag: " current-tag))))
    (setf (nth 2 window-config) tag)))

;; NOTE I've tried out generating the respective commands dynamically
;; with a macro, but this ended in unreadable code and Emacs not being
;; able to locate the generated commands, using lexical binding and a
;; loop resulted in very fun looking key bindings with closures in the
;; command description.  That's why I gave up and just wrote out the
;; first ten commands instead.

(defun eyebrowse-switch-to-window-config-0 ()
  "Switch to window configuration 0."
  (interactive)
  (eyebrowse-switch-to-window-config 0))

(defun eyebrowse-switch-to-window-config-1 ()
  "Switch to window configuration 1."
  (interactive)
  (eyebrowse-switch-to-window-config 1))

(defun eyebrowse-switch-to-window-config-2 ()
  "Switch to window configuration 2."
  (interactive)
  (eyebrowse-switch-to-window-config 2))

(defun eyebrowse-switch-to-window-config-3 ()
  "Switch to window configuration 3."
  (interactive)
  (eyebrowse-switch-to-window-config 3))

(defun eyebrowse-switch-to-window-config-4 ()
  "Switch to window configuration 4."
  (interactive)
  (eyebrowse-switch-to-window-config 4))

(defun eyebrowse-switch-to-window-config-5 ()
  "Switch to window configuration 5."
  (interactive)
  (eyebrowse-switch-to-window-config 5))

(defun eyebrowse-switch-to-window-config-6 ()
  "Switch to window configuration 6."
  (interactive)
  (eyebrowse-switch-to-window-config 6))

(defun eyebrowse-switch-to-window-config-7 ()
  "Switch to window configuration 7."
  (interactive)
  (eyebrowse-switch-to-window-config 7))

(defun eyebrowse-switch-to-window-config-8 ()
  "Switch to window configuration 8."
  (interactive)
  (eyebrowse-switch-to-window-config 8))

(defun eyebrowse-switch-to-window-config-9 ()
  "Switch to window configuration 9."
  (interactive)
  (eyebrowse-switch-to-window-config 9))

(defun eyebrowse-free-slot (slots)
  "Returns a yet unoccupied slot.
The specific behaviour is tmux-like."
  (let ((min (car slots)))
    (if (> min 1)
        1
      (let (last cur done)
        (while (and slots (not done))
          (setq last (car slots)
                cur (cadr slots))
          (when (and last cur
                     (> (- cur last) 1))
            (setq done t))
          (setq slots (cdr slots)))
        (1+ last)))))

(defun eyebrowse-create-window-config ()
  "Creates a window config at a yet unoccupied slot."
  (interactive)
  (let* ((window-configs (eyebrowse--get 'window-configs))
         (slots (mapcar 'car window-configs))
         (slot (eyebrowse-free-slot slots)))
    (eyebrowse-switch-to-window-config slot)))

(defun eyebrowse-create-named-window-config ()
  "Creates a window config at a yet unoccupied slot.
User is prompted to provide a tag name, so the window config is
created named."
  (interactive)
  (eyebrowse-create-window-config)
  (eyebrowse-rename-window-config
   (eyebrowse--get 'current-slot)
   (read-string "Tag: ")))

(defvar evil-motion-state-map)

;;;###autoload
(defun eyebrowse-setup-evil-keys ()
  "Set up key bindings specific to Evil.
Currently only gt, gT, gc and zx are supported."
  (define-key evil-motion-state-map (kbd "gt") 'eyebrowse-next-window-config)
  (define-key evil-motion-state-map (kbd "gT") 'eyebrowse-prev-window-config)
  (define-key evil-motion-state-map (kbd "gc") 'eyebrowse-close-window-config)
  (define-key evil-motion-state-map (kbd "zx") 'eyebrowse-last-window-config))

;;;###autoload
(defun eyebrowse-setup-opinionated-keys (&optional ignore-evil)
  "Set up more opinionated key bindings for using eyebrowse.

M-0..M-9, C-< / C->, C-'and C-\" are used for switching.  If
IGNORE-EVIL isn't set and Evil is detected, extra key bindings
will be set up with `eyebrowse-setup-evil-keys' as well."
  (let ((map eyebrowse-mode-map))
    (when (and (not ignore-evil)
               (bound-and-true-p evil-mode))
      (eyebrowse-setup-evil-keys))
    (define-key map (kbd "C-<") 'eyebrowse-prev-window-config)
    (define-key map (kbd "C->") 'eyebrowse-next-window-config)
    (define-key map (kbd "C-'") 'eyebrowse-last-window-config)
    (define-key map (kbd "C-\"") 'eyebrowse-close-window-config)
    (define-key map (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
    (define-key map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
    (define-key map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
    (define-key map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
    (define-key map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
    (define-key map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
    (define-key map (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
    (define-key map (kbd "M-7") 'eyebrowse-switch-to-window-config-7)
    (define-key map (kbd "M-8") 'eyebrowse-switch-to-window-config-8)
    (define-key map (kbd "M-9") 'eyebrowse-switch-to-window-config-9)))

(defun eyebrowse-format-slot (window-config)
  (let* ((slot (car window-config))
         (tag (nth 2 window-config))
         (format-string (if (and tag (> (length tag) 0))
                            eyebrowse-tagged-slot-format
                          eyebrowse-slot-format))
         ;; NOTE: `format-spec' sets `deactivate-mark' to t which
         ;; makes `eyebrowse-format-slot' usage in
         ;; `eyebrowse-mode-line-indicator' always deactivate the mark
         ;; after activating it as this triggers mode line updates...
         deactivate-mark)
    (format-spec format-string
                 (format-spec-make ?s slot ?t tag))))

(defun eyebrowse-mode-line-indicator ()
  "Return a string representation of the window configurations."
  (let* ((left-delimiter (propertize eyebrowse-mode-line-left-delimiter
                                     'face 'eyebrowse-mode-line-delimiters))
         (right-delimiter (propertize eyebrowse-mode-line-right-delimiter
                                      'face 'eyebrowse-mode-line-delimiters))
         (separator (propertize eyebrowse-mode-line-separator
                                'face 'eyebrowse-mode-line-separator))
         (current-slot (eyebrowse--get 'current-slot))
         (window-configs (if (eq eyebrowse-mode-line-style 'current)
                             (list (assoc current-slot (eyebrowse--get 'window-configs)))
                           (eyebrowse--get 'window-configs))))
    (if (and eyebrowse-mode-line-style
             (not (eq eyebrowse-mode-line-style 'hide))
             (or (and (not (eq eyebrowse-mode-line-style 'smart))
                      eyebrowse-mode-line-style)
                 (and (eq eyebrowse-mode-line-style 'smart)
                      (> (length window-configs) 1))))
        (concat
         left-delimiter
         (mapconcat
          (lambda (window-config)
            (let* ((slot (car window-config))
                   (face (if (= slot current-slot)
                             'eyebrowse-mode-line-active
                           'eyebrowse-mode-line-inactive))
                   (keymap
                    (let ((map (make-sparse-keymap)))
                      (define-key map (kbd "<mode-line><mouse-1>")
                        (lambda (_e)
                          (interactive "e")
                          (eyebrowse-switch-to-window-config slot)))
                      map))
                   (help-echo "mouse-1: Switch to indicated workspace")
                   (caption (eyebrowse-format-slot window-config)))
              (propertize caption 'face face 'slot slot
                          'mouse-face 'mode-line-highlight
                          'local-map keymap
                          'help-echo help-echo)))
          window-configs separator)
         right-delimiter)
      "")))

;;;###autoload
(define-minor-mode eyebrowse-mode
  "Toggle `eyebrowse-mode'.
This global minor mode provides a set of keybindings for
switching window configurations.  It tries mimicking the tab
behaviour of `ranger`, a file manager."
  :keymap eyebrowse-mode-map
  :global t
  (if eyebrowse-mode
      (progn
        ;; for some reason it's necessary to init both after emacs
        ;; started and after frame creation to make it work for both
        ;; emacs and emacsclient
        (eyebrowse-init)
        (add-hook 'after-make-frame-functions 'eyebrowse-init)
        (unless (assoc 'eyebrowse-mode mode-line-misc-info)
          (push '(eyebrowse-mode (:eval (eyebrowse-mode-line-indicator)))
                (cdr (last mode-line-misc-info)))))
    (remove-hook 'after-make-frame-functions 'eyebrowse-init)))

(provide 'eyebrowse)
;;; eyebrowse.el ends here
