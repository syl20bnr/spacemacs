;;; org-panel.el --- Simple routines for us with bad memory
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: Thu Nov 15 15:35:03 2007
;; Version: 0.21
;; Lxast-Updated: Wed Nov 21 03:06:03 2007 (3600 +0100)
;;
;; Features that might be required by this library:
;;
;;   `easymenu', `font-lock', `noutline', `org', `outline', `syntax',
;;   `time-date'.
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This defines a kind of control panel for `org-mode'. This control
;; panel should make it fast to move around and edit structure etc.
;;
;; To bring up the control panel type
;;
;;   M-x orgpan-panel
;;
;; Type ? there for help.
;;
;; I suggest you add the following to your .emacs for quick access of
;; the panel:
;;
;;   (eval-after-load 'org-mode
;;     (define-key org-mode-map [(control ?c) ?p] 'orgpan-panel))
;;
;;; Code:

(require 'org)
(require 'outline)

;; Fix-me: this is for testing. A minor mode version interferes badly
;; with emulation minor modes. On the other hand, the other version
;; interferes badly with (interactive ...).
(defvar orgpan-minor-mode-version t)

(defface orgpan-field
 '((t (:inherit 'widget-field)))
 "Face for fields."
 :group 'winsize)
(defvar orgpan-field-face 'orgpan-field)

(defface orgpan-active-field
 '((t (:inherit 'highlight)))
 "Face for fields."
 :group 'winsize)
(defvar orgpan-active-field-face 'orgpan-active-field)

(defface orgpan-spaceline
 '((t (:height 0.2)))
 "Face for spacing lines."
 :group 'winsize)

(defcustom orgpan-panel-buttons nil
 "Panel style, if non-nil use buttons.
If there are buttons in the panel they are used to change the way
the arrow keys work.  The panel looks something like this, with
the first button chosen:

 [Navigate] [Restructure] [TODO/Priority]
 ----------
 up/down, left: Go to, right: Visibility

The line below the buttons try to give a short hint about what
the arrow keys does.  \(Personally I prefer the version without
buttons since I then do not have to remember which button is
active.)"
 :type 'boolean
 :group 'winsize)

;; Fix-me: add org-mode-map
(defconst orgpan-org-mode-commands nil)
(defconst orgpan-org-commands
 '(
   orgpan-copy-subtree
   orgpan-cut-subtree
   orgpan-paste-subtree
   undo
   ;;
   ;orgpan-occur
   ;;
   org-cycle
   org-global-cycle
   outline-up-heading
   outline-next-visible-heading
   outline-previous-visible-heading
   outline-forward-same-level
   outline-backward-same-level
   org-todo
   org-show-todo-tree
   org-priority-up
   org-priority-down
   org-move-subtree-up
   org-move-subtree-down
   org-do-promote
   org-do-demote
   org-promote-subtree
   org-demote-subtree))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hook functions etc

(defun orgpan-delete-panel ()
 "Remove the panel."
 (interactive)
 (when (buffer-live-p orgpan-panel-buffer)
   (delete-windows-on orgpan-panel-buffer)
   (kill-buffer orgpan-panel-buffer))
 (setq orgpan-panel-buffer nil)
 (setq orgpan-panel-window nil)
 (orgpan-panel-minor-mode 0)
 (remove-hook 'post-command-hook 'orgpan-minor-post-command)
 (remove-hook 'post-command-hook 'orgpan-mode-post-command)
 ;;(remove-hook 'window-configuration-change-hook 'orgpan-window-config-change)
 )

(defvar orgpan-last-command-was-from-panel nil)
(defun orgpan-mode-pre-command ()
 (setq orgpan-last-command-was-from-panel nil)
 (condition-case err
     (if (not (and (windowp orgpan-org-window)
                   (window-live-p orgpan-org-window)))
         (progn
           (setq this-command 'ignore)
           (orgpan-delete-panel)
           (message "The window belonging to the panel had disappeared, removed panel."))
       (let ((buf (window-buffer orgpan-org-window)))
         (when (with-current-buffer buf
                 (derived-mode-p 'org-mode))
           (setq orgpan-last-org-buffer buf))
         ;; Fix me: add a list of those commands that are not
         ;; meaningful from the panel (for example org-time-stamp)
         (when (or (memq this-command orgpan-org-commands)
                   (memq this-command orgpan-org-mode-commands)
                   ;; For some reason not all org commands are found above:
                   (string= "org-" (substring (format "%s" this-command) 0 4)))
           (if (not (with-current-buffer buf
                      (derived-mode-p 'org-mode)))
               (progn
                 (if (buffer-live-p orgpan-org-buffer)
                     (set-window-buffer orgpan-org-window orgpan-org-buffer)
                   (message "Please use `l' or `b' to choose an org-mode buffer"))
                 (setq this-command 'ignore))
             (setq orgpan-org-buffer (window-buffer orgpan-org-window))
             (setq orgpan-last-command-was-from-panel t)
             (select-window orgpan-org-window)
             ;;(when (active-minibuffer-window
             ;;(set-buffer orgpan-org-buffer)
             ))))
   (error (lwarn 't :warning "orgpan-pre: %S" err))))

(defun orgpan-mode-post-command ()
 (condition-case err
     (progn
       (unless (and (windowp orgpan-panel-window)
                    (window-live-p orgpan-panel-window)
                    (bufferp orgpan-panel-buffer)
                    (buffer-live-p orgpan-panel-buffer))
         ;;(orgpan-delete-panel)
         )
       (when (and orgpan-last-command-was-from-panel
                  (windowp orgpan-panel-window)
                  (window-live-p orgpan-panel-window))
         (select-window orgpan-panel-window)
         (when (derived-mode-p 'orgpan-mode)
           (setq deactivate-mark t)
           (when orgpan-panel-buttons
             (unless (and orgpan-point
                          (= (point) orgpan-point))
               ;; Go backward so it is possible to click on a "button":
               (orgpan-backward-field))))))
   (error (lwarn 't :warning "orgpan-post: %S" err))))

;; (defun orgpan-window-config-change ()
;;   "Check if any frame is displaying an orgpan panel.
;; If not remove `orgpan-mode-post-command' and this function from
;; the hooks."
;;   (condition-case err
;;       (unless (and (
;;       (let ((found-pan nil))
;;         (dolist (f (frame-list))
;;           (dolist (w (window-list f 'nomini))
;;             (with-current-buffer (window-buffer w)
;;               (when (derived-mode-p 'orgpan-mode)
;;                 (setq found-pan t)))))
;;         (unless found-pan
;;           (remove-hook 'post-command-hook 'orgpan-mode-post-command)
;;           (remove-hook 'window-configuration-change-hook 'orgpan-window-config-change)))
;;     (error (lwarn 't :warning "Error in orgpan-config-change: %S" err))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commands

(defun orgpan-last-buffer ()
 "Open last org-mode buffer in panels org window."
 (interactive)
 (let ((buf (window-buffer orgpan-org-window))
       (last-buf orgpan-last-org-buffer))
   (when (with-current-buffer buf
           (derived-mode-p 'org-mode))
     (setq orgpan-last-org-buffer buf))
   (when (eq last-buf buf)
     (setq last-buf nil))
   (if (not last-buf)
       (orgpan-switch-buffer)
     (set-window-buffer orgpan-org-window last-buf))))

(defun orgpan-switch-buffer ()
 "Switch to next org-mode buffer in panels org window."
 (interactive)
 (let ((buf (window-buffer orgpan-org-window))
       (org-buffers nil))
   (with-current-buffer buf
     (when (derived-mode-p 'org-mode)
       (bury-buffer buf)
       (setq orgpan-last-org-buffer buf)))
   (setq org-buffers (delq nil (mapcar (lambda (buf)
                                         (when (with-current-buffer buf
                                                 (derived-mode-p 'org-mode))
                                           buf))
                                       (buffer-list))))
   (setq org-buffers (delq buf org-buffers))
   (set-window-buffer orgpan-org-window (car org-buffers))
   (setq orgpan-org-buffer (car org-buffers))))

(defun orgpan-paste-subtree ()
 (interactive)
 (if (y-or-n-p "Paste subtree here? ")
     (org-paste-subtree)
   (message "Nothing was pasted")))

(defun orgpan-cut-subtree ()
 (interactive)
 (let ((heading (progn
                  (org-back-to-heading)
                  (buffer-substring (point) (line-end-position))
                  )))
   (if (y-or-n-p (format "Do you want to cut the subtree\n%s\n? " heading))
       (org-cut-subtree)
     (message "Nothing was cut"))))

(defun orgpan-copy-subtree ()
 (interactive)
 (let ((heading (progn
                  (org-back-to-heading)
                  (buffer-substring (point) (line-end-position))
                  )))
   (if (y-or-n-p (format "Do you want to copy the subtree\n%s\n? " heading))
       (org-copy-subtree)
     (message "Nothing was copied"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buttons

(defvar orgpan-ovl-help nil)

(defun orgpan-check-panel-mode ()
 (unless (derived-mode-p 'orgpan-mode)
   (error "Not orgpan-mode in buffer: %s" major-mode)))

(defun orgpan-display-bindings-help ()
 (orgpan-check-panel-mode)
 (setq orgpan-point (point))
 (let* ((ovls (overlays-at (point)))
        (ovl (car ovls))
        (help (when ovl (overlay-get ovl 'orgpan-explain))))
   (dolist (o (overlays-in (point-min) (point-max)))
     (overlay-put o 'face orgpan-field-face))
   (overlay-put ovl 'face orgpan-active-field-face)
   (overlay-put orgpan-ovl-help 'before-string help)))

(defun orgpan-forward-field ()
 (interactive)
 (orgpan-check-panel-mode)
 (let ((pos (next-overlay-change (point))))
   (unless (overlays-at pos)
     (setq pos (next-overlay-change pos)))
   (when (= pos (point-max))
     (setq pos (point-min))
     (unless (overlays-at pos)
       (setq pos (next-overlay-change pos))))
   (goto-char pos))
 (orgpan-display-bindings-help))

(defun orgpan-backward-field ()
 (interactive)
 (orgpan-check-panel-mode)
 (when (= (point) (point-min))
   (goto-char (point-max)))
 (let ((pos (previous-overlay-change (point))))
   (unless (overlays-at pos)
     (setq pos (previous-overlay-change pos)))
   (goto-char pos))
 (orgpan-display-bindings-help))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode

(defconst orgpan-mode-map
 ;; Fix-me: clean up here!
 ;; Fix-me: viper support
 (let ((map (make-sparse-keymap)))
   (define-key map [?q] 'orgpan-delete-panel)
   (define-key map [??] 'orgpan-help)
   ;; Copying etc
   (define-key map [?c] 'orgpan-copy-subtree)
   (define-key map [?x] 'orgpan-cut-subtree)
   (define-key map [?p] 'orgpan-paste-subtree)
   (define-key map [?z] 'undo)
   ;; Buffers:
   (define-key map [?b] 'orgpan-switch-buffer)
   (define-key map [?l] 'orgpan-last-buffer)
   ;; Some keys for moving between headings. Emacs keys for next/prev
   ;; line seems ok:
   (define-key map [(control ?p)] 'outline-previous-visible-heading)
   (define-key map [(control ?n)] 'outline-next-visible-heading)
   (define-key map [(shift control ?p)] 'outline-backward-same-level)
   (define-key map [(shift control ?n)] 'outline-forward-same-level)
   ;; A mnemunic for up:
   (define-key map [(control ?u)] 'outline-up-heading)
   ;; Search sparse tree:
   ;;
   ;; Fix-me: Search does not work, some problem with
   ;; interactive. Probably have to turn the whole thing around and
   ;; always be in the org buffer, but with a minor mode running
   ;; there.
   ;;
   ;;(define-key map [?s] 'org-sparse-tree)
   (define-key map [?s] 'orgpan-occur)
   ;; Same as in org-mode:
   ;;(define-key map [(control ?c)(control ?v)] 'org-show-todo-tree)
   ;; Fix-me: This leads to strange problems:
   ;;(define-key map [t] 'ignore)
   map))

(defun orgpan-occur ()
 "Replacement for `org-occur'.
Technical reasons."
 (interactive)
 (let ((rgx (read-from-minibuffer "my mini Regexp: ")))
   (setq orgpan-last-command-was-from-panel t)
   (select-window orgpan-org-window)
   (org-occur rgx)))

(defvar orgpan-panel-window nil
 "The window showing `orgpan-panel-buffer'.")

(defvar orgpan-panel-buffer nil
 "The panel buffer.
There can be only one such buffer at any time.")

(defvar orgpan-org-window nil)
;;(make-variable-buffer-local 'orgpan-org-window)

;; Fix-me: used?
(defvar orgpan-org-buffer nil)
;;(make-variable-buffer-local 'orgpan-org-buffer)

(defvar orgpan-last-org-buffer nil)
;;(make-variable-buffer-local 'orgpan-last-org-buffer)

(defvar orgpan-point nil)
;;(make-variable-buffer-local 'orgpan-point)

(defvar viper-emacs-state-mode-list)
(defvar viper-new-major-mode-buffer-list)

(defun orgpan-avoid-viper-in-buffer ()
 ;; Fix-me: This is ugly. However see `this-major-mode-requires-vi-state':
 (set (make-local-variable 'viper-emacs-state-mode-list) '(orgpan-mode))
 (set (make-local-variable 'viper-new-major-mode-buffer-list) nil)
 (local-set-key [?\ ] 'ignore))

(define-derived-mode orgpan-mode nil "Org-Panel"
 "Mode for org-simple.el control panel."
 (setq buffer-read-only t)
 (unless orgpan-minor-mode-version
   (add-hook 'pre-command-hook 'orgpan-mode-pre-command nil t)
   (add-hook 'post-command-hook 'orgpan-mode-post-command t))
 (orgpan-avoid-viper-in-buffer)
 (setq cursor-type nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Panel layout

(defun orgpan-insert-field (text keymap explain)
 (insert text)
 (let* ((end (point))
        (len (length text))
        (beg (- end len))
        (ovl (make-overlay beg end)))
   (overlay-put ovl 'face orgpan-field-face)
   (overlay-put ovl 'keymap keymap)
   (overlay-put ovl 'orgpan-explain explain)))

(defconst orgpan-with-keymap
 (let ((map (make-sparse-keymap)))
   (set-keymap-parent map org-mode-map)
   ;; Users are used to tabbing between fields:
   (define-key map [(tab)] 'orgpan-forward-field)
   (define-key map [(shift tab)] 'orgpan-backward-field)
   ;; Now we must use something else for visibility (first does not work if Viper):
   (define-key map [(meta tab)] 'org-cycle)
   (define-key map [(control meta tab)] 'org-global-cycle)
   map))

(defconst orgpan-without-keymap
 (let ((map (make-sparse-keymap)))
   (set-keymap-parent map org-mode-map)
   ;; Visibility (those are in org-mode-map):
   ;;(define-key map [tab] 'org-cycle)
   ;;(define-key map [(shift tab)] 'org-global-cycle)
   ;; Navigate:
   (define-key map [left] 'outline-up-heading)
   (define-key map [right] 'org-cycle)
   (define-key map [up] 'outline-previous-visible-heading)
   (define-key map [down] 'outline-next-visible-heading)
   (define-key map [(shift down)] 'outline-forward-same-level)
   (define-key map [(shift up)] 'outline-backward-same-level)
   ;; Restructure:
   (define-key map [(control up)] 'org-move-subtree-up)
   (define-key map [(control down)] 'org-move-subtree-down)
   (define-key map [(control left)] 'org-do-promote)
   (define-key map [(control right)] 'org-do-demote)
   (define-key map [(control shift left)] 'org-promote-subtree)
   (define-key map [(control shift right)] 'org-demote-subtree)
   ;; Todo etc
   (define-key map [?+] 'org-priority-up)
   (define-key map [?-] 'org-priority-down)
   (define-key map [?t] 'org-todo)
   map))

(defun orgpan-make-panel-without-buttons (buf)
 (with-current-buffer buf
   (insert (propertize "Org Panel" 'face 'orgpan-active-field))
   (insert "  ? for help, q quit\n")
   (insert (propertize "arrows" 'face 'font-lock-keyword-face)
           ": Go to, "
           (propertize "C-arrows" 'face 'font-lock-keyword-face)
           ": Edit tree\n"
           (propertize "cxpz" 'face 'font-lock-keyword-face)
           ": copy cut paste undo, "
           (propertize "tT+-" 'face 'font-lock-keyword-face)
           ": todo priority, "
           (propertize "s" 'face 'font-lock-keyword-face)
           " search"
           )
   (set-keymap-parent orgpan-mode-map orgpan-without-keymap)
   ))

(defun orgpan-make-panel-with-buttons (buf)
 (with-current-buffer buf
   (let* ((base-map (make-sparse-keymap))
          (space-line (propertize "\n\n" 'face 'orgpan-spaceline))
          (arrow-face 'font-lock-keyword-face)
          (L (propertize "left" 'face arrow-face))
          (R (propertize "right" 'face arrow-face))
          (U (propertize "up" 'face arrow-face))
          (D (propertize "down" 'face arrow-face)))
     ;;(message D)(sit-for 2)
     (define-key base-map [left] 'ignore)
     (define-key base-map [right] 'ignore)
     (define-key base-map [up] 'ignore)
     (define-key base-map [down] 'ignore)
     (define-key base-map [?q] 'delete-window)
     (define-key base-map [??] 'orgpan-help)
     ;; Navigating
     (let ((map (copy-keymap base-map)))
       (define-key map [left] 'outline-up-heading)
       (define-key map [right] 'org-cycle)
       (define-key map [up] 'outline-previous-visible-heading)
       (define-key map [down] 'outline-next-visible-heading)
       (define-key map [(shift down)] 'outline-forward-same-level)
       (define-key map [(shift up)] 'outline-backward-same-level)
       (orgpan-insert-field "Navigate" map (concat U "/" D ", " L ": Go to, " R ": Visibility")))
     (insert "  ")
     (let ((map (copy-keymap base-map)))
       (define-key map [up] 'org-move-subtree-up)
       (define-key map [down] 'org-move-subtree-down)
       (define-key map [left] 'org-do-promote)
       (define-key map [right] 'org-do-demote)
       (define-key map [(shift left)] 'org-promote-subtree)
       (define-key map [(shift right)] 'org-demote-subtree)
       (orgpan-insert-field
        "Restructure" map
        (concat U "/" D ": "
                (propertize "Move" 'face 'font-lock-warning-face)
                ", " L "/" R ": "
                (propertize "Level (w S: Subtree Level)" 'face 'font-lock-warning-face))))
     (insert "  ")
     (let ((map (copy-keymap base-map)))
       (define-key map [up] 'org-priority-up)
       (define-key map [down] 'org-priority-down)
       (define-key map [right] 'org-todo)
       (orgpan-insert-field "TODO/priority" map
                            (concat R ": TODO, " U "/" D ": Priority")))
     )
   (insert "   ? for help, q quit\n")
   (orgpan-display-bindings-help)
   (setq orgpan-ovl-help (make-overlay (point) (point)))
   ))

(defun orgpan-make-panel-buffer ()
 "Make the panel buffer."
 (let* ((buf-name "*Org Panel*"))
   (when orgpan-panel-buffer (kill-buffer orgpan-panel-buffer))
   (setq orgpan-panel-buffer (get-buffer-create buf-name))
   (if orgpan-panel-buttons
       (orgpan-make-panel-with-buttons orgpan-panel-buffer)
     (orgpan-make-panel-without-buttons orgpan-panel-buffer))
   (with-current-buffer orgpan-panel-buffer
     (orgpan-mode)
     (goto-char (point-min)))
   orgpan-panel-buffer))

(defun orgpan-help ()
 (interactive)
 (set-keymap-parent orgpan-with-keymap nil)
 (set-keymap-parent orgpan-without-keymap nil)
 (describe-function 'orgpan-panel)
 (set-keymap-parent orgpan-with-keymap org-mode-map)
 (set-keymap-parent orgpan-without-keymap org-mode-map)
 (message "Use 'l' to remove help window")
 )

(defun orgpan-panel ()
 "Create a control panel for current `org-mode' buffer.
The control panel may be used to quickly move around and change
the headings. The idea is that when you want to to a lot of this
kind of editing you should be able to do that with few
keystrokes (and without having to remember the complicated
keystrokes). A typical situation when this perhaps can be useful
is when you are looking at your notes file \(usually ~/.notes,
see `remember-data-file') where you have saved quick notes with
`remember'.

The keys below are defined in the panel. Note that the commands
are carried out in the `org-mode' buffer that belongs to the
panel.

\\{orgpan-mode-map}

In addition to the keys above most of the keys in `org-mode' can
also be used from the panel.

Note: There are two forms of the control panel, one with buttons
and one without. The default is without, see
`orgpan-panel-buttons'.  If buttons are used choosing a different
button changes the binding of the arrow keys."
 (interactive)
 (unless (derived-mode-p 'org-mode)
   (error "Buffer is not in org-mode"))
 (orgpan-delete-panel)
 (unless orgpan-org-mode-commands
   (map-keymap (lambda (ev def)
                 (when (and def
                            (symbolp def)
                            (fboundp def))
                   (setq orgpan-org-mode-commands
                         (cons def orgpan-org-mode-commands))))
               org-mode-map))
 ;;(org-back-to-heading)
 ;;(remove-hook 'window-configuration-change-hook 'orgpan-window-config-change)
 (setq orgpan-org-window (selected-window))
 (setq orgpan-panel-window (split-window nil -4 'below))
 (select-window orgpan-panel-window)
 (set-window-buffer (selected-window) (orgpan-make-panel-buffer))
 ;;(set-window-dedicated-p (selected-window) t)
 ;; The minor mode version starts here:
 (when orgpan-minor-mode-version
   (select-window orgpan-org-window)
   (orgpan-panel-minor-mode 1)
   (add-hook 'post-command-hook 'orgpan-minor-post-command t)))

(defun orgpan-minor-post-command ()
 (unless (and
          ;; Check org window and buffer
          (windowp orgpan-org-window)
          (window-live-p orgpan-org-window)
          (eq orgpan-org-window (selected-window))
          (derived-mode-p 'org-mode)
          ;; Check panel window and buffer
          (windowp orgpan-panel-window)
          (window-live-p orgpan-panel-window)
          (bufferp orgpan-panel-buffer)
          (buffer-live-p orgpan-panel-buffer)
          (eq (window-buffer orgpan-panel-window) orgpan-panel-buffer)
          ;; Check minor mode
          orgpan-panel-minor-mode)
   (orgpan-delete-panel)))

(define-minor-mode orgpan-panel-minor-mode
 "Minor mode used in `org-mode' buffer when showing panel."
 :keymap orgpan-mode-map
 :lighter " PANEL"
 :group 'orgpan
 )


(provide 'org-panel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; org-panel.el ends here
