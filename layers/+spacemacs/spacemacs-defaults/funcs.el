;;; funcs.el --- Spacemacs Defaults Layer functions File
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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


(defun spacemacs//run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (format "%S-local-vars-hook" major-mode))))

(defun spacemacs/split-and-new-line ()
  "Split a quoted string or s-expression and insert a new line with
auto-indent."
  (interactive)
  (sp-split-sexp 1)
  (sp-newline))

(defun spacemacs/push-mark-and-goto-beginning-of-line ()
  "Push a mark at current location and go to the beginning of the line."
  (interactive)
  (push-mark (point))
  (evil-beginning-of-line))

(defun spacemacs/push-mark-and-goto-end-of-line ()
  "Push a mark at current location and go to the end of the line."
  (interactive)
  (push-mark (point))
  (evil-end-of-line))

(defun spacemacs/evil-insert-line-above (count)
  "Insert one or several lines above the current point's line without changing
the current state and point position."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun spacemacs/evil-insert-line-below (count)
  "Insert one or several lines below the current point's line without changing
the current state and point position."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

(defun spacemacs/evil-goto-next-line-and-indent (&optional count)
  "Match the current lines indentation to the next line.
A COUNT argument matches the indentation to the next COUNT lines."
  (interactive "p")
  (let ((counter (or count 1)))
    (while (> counter 0)
      (join-line 1)
      (newline-and-indent)
      (setq counter (1- counter)))))

;; from Prelude
;; TODO: dispatch these in the layers
(defcustom spacemacs-indent-sensitive-modes
  '(asm-mode
    coffee-mode
    elm-mode
    haml-mode
    haskell-mode
    slim-mode
    makefile-mode
    makefile-bsdmake-mode
    makefile-gmake-mode
    makefile-imake-mode
    python-mode
    yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type '(repeat symbol)
  :group 'spacemacs)

(defcustom spacemacs-yank-indent-modes '(latex-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here."
  :type '(repeat symbol)
  :group 'spacemacs)

(defcustom spacemacs-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'spacemacs)

(defcustom spacemacs-large-file-modes-list
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode
                 doc-view-mode doc-view-mode-maybe ebrowse-tree-mode
                 pdf-view-mode tags-table-mode fundamental-mode)
  "Major modes which `spacemacs/check-large-file' will not be
automatically applied to."
  :group 'spacemacs
  :type '(repeat symbol))

(defun spacemacs/custom-newline (pos)
  "Make `RET' in a Custom-mode search box trigger that field's action, rather
than enter an actual newline, which is useless and unexpected in a search box.
If not in such a search box, fall back on `Custom-newline'."
  (interactive "d")
  (let ((w (widget-at)))
    (if (and w
             (eq 'editable-field (widget-type w))
             (string-prefix-p "Search" (widget-get w :help-echo)))
        (funcall (widget-get w :action) w)
      (Custom-newline pos))))

;; ido-mode remaps some commands to ido counterparts.  We want default Emacs key
;; bindings (those under C-x) to use ido, but we want to use the original
;; commands in Spacemacs key bindings (those under M-m or SPC) so that they use
;; `read-file-name-function', `completing-read-function',
;; `completion-in-region-function', etc. configured by Helm or Ivy etc.  The
;; following aliases allow us to bind Spacemacs keys to the original commands.
(defalias 'spacemacs/find-file-other-frame 'find-file-other-frame)
(defalias 'spacemacs/dired 'dired)
(defalias 'spacemacs/dired-other-frame 'dired-other-frame)
(defalias 'spacemacs/switch-to-buffer-other-frame 'switch-to-buffer-other-frame)
(defalias 'spacemacs/insert-file 'insert-file)
(defalias 'spacemacs/display-buffer-other-frame 'display-buffer-other-frame)
(defalias 'spacemacs/find-file-and-replace-buffer 'find-alternate-file)

;; By default the emacs leader is M-m, turns out that Helm does this:
;;    (cl-dolist (k (where-is-internal 'describe-mode global-map))
;;         (define-key map k 'helm-help))
;; after doing this:
;;    (define-key map (kbd \"M-m\") 'helm-toggle-all-marks)
;; So when Helm is loaded we get the error:
;;    Key sequence M-m h d m starts with non-prefix key M-m
;; To prevent this error we just alias `describe-mode' to defeat the Helm hack.
(defalias 'spacemacs/describe-mode 'describe-mode
  "To avoid Helm key binding issue for `describe-mode'.
Refer Spacemacs #16397 for details.")

(defun spacemacs/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (evil-indent (point-min) (point-max))
      (message "Indented buffer."))
    (whitespace-cleanup)))

;; http://emacsblog.org/2007/01/17/indent-whole-buffer/
(defun spacemacs/iwb-region-or-buffer ()
  "IWBs a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (untabify (region-beginning) (region-end))
          (indent-region (region-beginning) (region-end)))
      (set-buffer-file-coding-system default-file-name-coding-system)
      ;; (set-buffer-file-coding-system 'utf-8-unix)
      (untabify (point-min) (point-max))
      (indent-region (point-min) (point-max))
      (whitespace-cleanup))))

(defun spacemacs//trailing-whitespace ()
  (setq show-trailing-whitespace dotspacemacs-show-trailing-whitespace))

(defun spacemacs//set-whitespace-style-for-diff ()
  "Whitespace configuration for `diff-mode'"
  (setq-local whitespace-style '(face
                                 tabs
                                 tab-mark
                                 spaces
                                 space-mark
                                 trailing
                                 indentation::space
                                 indentation::tab
                                 newline
                                 newline-mark)))

(defun spacemacs//delete-other-non-side-windows ()
  "Delete all windows except for the selected one and side windows.

Unlike `delete-other-windows', this function ignores the
window parameter no-delete-other-windows."
  (walk-windows
   (lambda (win)
     (unless (or (eq win (selected-window))
                 (window-parameter win 'window-side))
       (delete-window win)))
   'ignore-minibuffer))

;; adapted from https://gist.github.com/3402786
(defun spacemacs/toggle-maximize-window ()
  "Temporarily maximize window, restore other windows on the next
call.

The variable `dotspacemacs-maximize-window-keep-side-windows'
controls whether side windows (such as those created by treemacs, neotree or
persistent which-key) are kept or minimized too."
  (interactive)
  (let* ((max-target-window
          (if dotspacemacs-maximize-window-keep-side-windows
              (window-main-window)
            (frame-root-window)))
         (window-already-maximal
          (eq max-target-window (selected-window))))
    (cond ((and window-already-maximal
                (window-parameter nil 'spacemacs-max-state))
           ;; Restore the previously deleted windows, keeping the state of the
           ;; selected window.
           (let ((selected-win-state (window-state-get (selected-window))))
             (window-state-put
              (window-parameter nil 'spacemacs-max-state) (selected-window))
             (window-state-put selected-win-state (selected-window))
             (set-window-parameter nil 'spacemacs-max-state nil)))
          ((and (not window-already-maximal)
                (window-parameter nil 'window-side))
           ;; Raise the same error as `delete-other-windows'
           ;; (with `ignore-window-parameters' nil).
           (error "Cannot make side window the only window"))
          ((not window-already-maximal)
           ;; Store the current state as a window parameter of the selected window
           ;; and delete other windows.
           (walk-windows (lambda (win) (set-window-parameter win 'spacemacs-max-state nil)))
           (set-window-parameter nil 'spacemacs-max-state (window-state-get max-target-window t))
           (if dotspacemacs-maximize-window-keep-side-windows
               (spacemacs//delete-other-non-side-windows)
             (let ((ignore-window-parameters t))
               (delete-other-windows))))))
  (when (configuration-layer/layer-used-p 'spacemacs-layouts)
    ;; Make the existing advice for rename-buffer, `spacemacs//fixup-window-configs',
    ;; apply to buffers in "minimized" windows in the current workspace.
    (spacemacs/update-eyebrowse-for-perspective)))

(define-obsolete-function-alias 'spacemacs/toggle-maximize-buffer
  'spacemacs/toggle-maximize-window "2024-08")

;; https://tsdh.wordpress.com/2007/03/28/deleting-windows-vertically-or-horizontally/
(defun spacemacs/maximize-horizontally ()
  "Delete all windows to the left and right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-left) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-right) (error nil))
      (delete-window))))

(defun spacemacs/maximize-vertically ()
  "Delete all windows above and below the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-up) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-down) (error nil))
      (delete-window))))

(defun spacemacs/useful-buffer-p (buffer)
  "Determines if a buffer is useful."
  (let ((buf-name (buffer-name buffer)))
    (or (provided-mode-derived-p (buffer-local-value 'major-mode buffer) 'comint-mode)
        (cl-loop for useful-regexp in spacemacs-useful-buffers-regexp
                 thereis (string-match-p useful-regexp buf-name))
        (cl-loop for useless-regexp in spacemacs-useless-buffers-regexp
                 never (string-match-p useless-regexp buf-name)))))

(defun spacemacs/useless-buffer-p (buffer)
  "Determines if a buffer is useless."
  (not (spacemacs/useful-buffer-p buffer)))


(defun spacemacs/swap-windows (window1 window2)
  "Swap two windows.
WINDOW1 and WINDOW2 must be valid windows. They may contain child windows."
  (let ((state1 (window-state-get window1))
        (state2 (window-state-get window2)))
    ;; to put state into dedicated windows, we must undedicate them first (not
    ;; needed with Emacs 25.1)
    (dolist (win (list window1 window2))
      (if (window-live-p win)
          (set-window-dedicated-p win nil)
        ;; win has sub-windows, undedicate all of them
        (walk-window-subtree (lambda (leaf-window)
                               (set-window-dedicated-p leaf-window nil))
                             win)))
    (window-state-put state1 window2)
    (window-state-put state2 window1)))

;; from @bmag
(defun spacemacs/window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((window-tree (car (window-tree)))
             (current-split-vertical-p (car window-tree))
             (first-window (nth 2 window-tree))
             (second-window (nth 3 window-tree))
             (second-window-state (window-state-get second-window))
             (splitter (if current-split-vertical-p
                           #'split-window-horizontally
                         #'split-window-vertically)))
        (delete-other-windows first-window)
        ;; `window-state-put' also re-selects the window if needed, so we don't
        ;; need to call `select-window'
        (window-state-put second-window-state (funcall splitter)))
    (error "Can't toggle window layout when the number of windows isn't two.")))

;; originally from magnars and modified by ffevotte for dedicated windows
;; support, it has quite diverged by now
(defun spacemacs/rotate-windows-forward (count)
  "Rotate each window forwards.
A negative prefix argument rotates each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (let* ((non-dedicated-windows (cl-remove-if 'window-dedicated-p (window-list)))
         (states (mapcar #'window-state-get non-dedicated-windows))
         (num-windows (length non-dedicated-windows))
         (step (+ num-windows count)))
    (if (< num-windows 2)
        (error "You can't rotate a single window!")
      (dotimes (i num-windows)
        (window-state-put
         (elt states i)
         (elt non-dedicated-windows (% (+ step i) num-windows)))))))

(defun spacemacs/rotate-windows-backward (count)
  "Rotate each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (spacemacs/rotate-windows-forward (* -1 count)))

(if (configuration-layer/package-used-p 'winum)
    (progn
      (defun spacemacs/move-buffer-to-window (windownum follow-focus-p)
        "Moves a buffer to a window, using the spacemacs numbering. follow-focus-p
  controls whether focus moves to new window (with buffer), or stays on current"
        (interactive)
        (if (> windownum (length (window-list-1 nil nil t)))
            (message "No window numbered %s" windownum)
          (let ((b (current-buffer))
                (w1 (selected-window))
                (w2 (winum-get-window-by-number windownum)))
            (unless (eq w1 w2)
              (set-window-buffer w2 b)
              (switch-to-prev-buffer)
              (unrecord-window-buffer w1 b))
            (when follow-focus-p
              (select-window (winum-get-window-by-number windownum))))))

      (defun spacemacs/swap-buffers-to-window (windownum follow-focus-p)
        "Swaps visible buffers between active window and selected window.
  follow-focus-p controls whether focus moves to new window (with buffer), or
  stays on current"
        (interactive)
        (if (> windownum (length (window-list-1 nil nil t)))
            (message "No window numbered %s" windownum)
          (let* ((b1 (current-buffer))
                 (w1 (selected-window))
                 (w2 (winum-get-window-by-number windownum))
                 (b2 (window-buffer w2)))
            (unless (eq w1 w2)
              (set-window-buffer w1 b2)
              (set-window-buffer w2 b1)
              (unrecord-window-buffer w1 b1)
              (unrecord-window-buffer w2 b2)))
          (when follow-focus-p (winum-select-window-by-number windownum)))))
  ;; when the winum package isn't used
  (defun spacemacs//message-winum-package-required ()
    (interactive)
    (message (concat "This command requires the winum package," "\n"
                     "winum is part of the spacemacs-navigation layer."))))

;; define and evaluate numbered functions:
;; spacemacs/winum-select-window-0 to 9
(dotimes (i 10)
  (eval `(defun ,(intern (format "spacemacs/winum-select-window-%s" i)) (&optional arg)
           ,(concat (format "Select window %i\n" i)
                    "Or if the winum package isn't used:\n"
                    "For example in the spacemacs-base distribution."
                    "Show a message stating that the winum package,"
                    "is part of the spacemacs-navigation layer.\n")
           (interactive "P")
           (if (configuration-layer/package-used-p 'winum)
               (funcall ',(intern (format "winum-select-window-%s" i)) arg)
             (spacemacs//message-winum-package-required)))))

;; define and evaluate three numbered functions:
;; buffer-to-window-1 to 9
;; move-buffer-window-no-follow-1 to 9
;; swap-buffer-window-no-follow-1 to 9
(dotimes (i 9)
  (let ((n (+ i 1)))
    (eval `(defun ,(intern (format "buffer-to-window-%s" n)) (&optional arg)
             ,(format "Move buffer to the window with number %i." n)
             (interactive "P")
             (if (configuration-layer/package-used-p 'winum)
                 (if arg
                     (spacemacs/swap-buffers-to-window ,n t)
                   (spacemacs/move-buffer-to-window ,n t))
               (spacemacs//message-winum-package-required))))
    (eval `(defun ,(intern (format "move-buffer-window-no-follow-%s" n)) ()
             (interactive)
             (if (configuration-layer/package-used-p 'winum)
                 (spacemacs/move-buffer-to-window ,n nil)
               (spacemacs//message-winum-package-required))))
    (eval `(defun ,(intern (format "swap-buffer-window-no-follow-%s" n)) ()
             (interactive)
             (if (configuration-layer/package-used-p 'winum)
                 (spacemacs/swap-buffers-to-window ,n nil)
               (spacemacs//message-winum-package-required))))))

(defun spacemacs/rename-file (filename &optional new-filename)
  "Rename FILENAME to NEW-FILENAME.

When NEW-FILENAME is not specified, asks user for a new name.

Also renames associated buffers (if any exists), invalidates
projectile cache and updates recentf list."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let* ((is-dir (file-directory-p filename))
           (short-name
            (if is-dir
                (file-name-base (directory-file-name filename))
              (file-name-nondirectory filename)))
           (new-filename
            (if new-filename new-filename
              (read-file-name
               (format "Rename %s to: " short-name)))))

      ;; Rename filename to new-filename and error if new-filename already
      ;; exists. `dired-rename-file' handles renaming of directories and files.
      ;; It updates the name of all associated buffers.
      (dired-rename-file filename new-filename nil)

      ;; Update recentf list.
      (when (fboundp 'recentf-add-file)
        (seq-map
         (lambda (fp)
           (recentf-add-file
            (concat new-filename (string-remove-prefix filename fp)))
           (recentf-remove-if-non-kept fp))
         (seq-filter
          (lambda (fp)
            (string-prefix-p filename fp))
          recentf-list)))

      ;; Invalidate projectile cache.
      (when (and (configuration-layer/package-used-p 'projectile)
                 (projectile-project-p))
        (call-interactively #'projectile-invalidate-cache))

      ;; Inform user about tremendous success.
      (message "%s '%s' successfully renamed to '%s'"
               (if is-dir "Directory" "File")
               short-name
               (file-name-nondirectory new-filename)))))

;; originally from magnars
(defun spacemacs/rename-buffer-visiting-a-file (&optional arg)
  (let* ((old-filename (buffer-file-name))
         (old-short-name (file-name-nondirectory (buffer-file-name)))
         (old-dir (file-name-directory old-filename))
         (new-name (let ((path (read-file-name "New name: " (if arg old-dir old-filename))))
                     (if (string= (file-name-nondirectory path) "")
                         (concat path old-short-name)
                       path)))
         (new-dir (file-name-directory new-name))
         (new-short-name (file-name-nondirectory new-name))
         (file-moved-p (not (string-equal new-dir old-dir)))
         (file-renamed-p (not (string-equal new-short-name old-short-name))))
    (cond ((get-buffer new-name)
           (error "A buffer named '%s' already exists!" new-name))
          ((string-equal new-name old-filename)
           (spacemacs/show-hide-helm-or-ivy-prompt-msg
            "Rename failed! Same new and old name" 1.5)
           (spacemacs/rename-current-buffer-file))
          (t
           (let ((old-directory (file-name-directory new-name)))
             (when (and (not (file-exists-p old-directory))
                        (yes-or-no-p
                         (format "Create directory '%s'?" old-directory)))
               (make-directory old-directory t)))
           (rename-file old-filename new-name 1)
           (rename-buffer new-name)
           (set-visited-file-name new-name)
           (set-buffer-modified-p nil)
           (when (fboundp 'recentf-add-file)
             (recentf-add-file new-name)
             (recentf-remove-if-non-kept old-filename))
           (when (and (configuration-layer/package-used-p 'projectile)
                      (projectile-project-p))
             (funcall #'projectile-invalidate-cache nil))
           (message (cond ((and file-moved-p file-renamed-p)
                           (concat "File Moved & Renamed\n"
                                   "From: " old-filename "\n"
                                   "To:   " new-name))
                          (file-moved-p
                           (concat "File Moved\n"
                                   "From: " old-filename "\n"
                                   "To:   " new-name))
                          (file-renamed-p
                           (concat "File Renamed\n"
                                   "From: " old-short-name "\n"
                                   "To:   " new-short-name))))))))

(defun spacemacs/rename-buffer-or-save-new-file ()
  (let ((old-short-name (buffer-name))
        key)
    (while (not (memq key '(?s ?r)))
      (setq key (read-key (propertize
                           (format
                            (concat "Buffer '%s' is not visiting a file: "
                                    "[s]ave to file or [r]ename buffer?")
                            old-short-name)
                           'face 'minibuffer-prompt)))
      (cond ((eq key ?s)            ; save to file
             ;; this allows for saving a new empty (unmodified) buffer
             (unless (buffer-modified-p) (set-buffer-modified-p t))
             (save-buffer))
            ((eq key ?r)            ; rename buffer
             (let ((new-buffer-name (read-string "New buffer name: ")))
               (while (get-buffer new-buffer-name)
                 ;; ask to rename again, if the new buffer name exists
                 (if (yes-or-no-p
                      (format (concat "A buffer named '%s' already exists: "
                                      "Rename again?")
                              new-buffer-name))
                     (setq new-buffer-name (read-string "New buffer name: "))
                   (keyboard-quit)))
               (rename-buffer new-buffer-name)
               (message (concat "Buffer Renamed\n"
                                "From: " old-short-name "\n"
                                "To:   " new-buffer-name))))
            ;; ?\a = C-g, ?\e = Esc and C-[
            ((memq key '(?\a ?\e)) (keyboard-quit))))))

(defun spacemacs/rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting.
If the buffer isn't visiting a file, ask if it should
be saved to a file, or just renamed.

If called without a prefix argument, the prompt is
initialized with the current directory instead of filename."
  (interactive "P")
  (let ((file (buffer-file-name)))
    (if (and file (file-exists-p file))
        (spacemacs/rename-buffer-visiting-a-file arg)
      (spacemacs/rename-buffer-or-save-new-file))))

(defun spacemacs/show-hide-helm-or-ivy-prompt-msg (msg sec)
  "Show a MSG at the helm or ivy prompt for SEC.
With Helm, remember the path, then restore it after SEC.
With Ivy, the path isn't editable, just remove the MSG after SEC."
  (run-at-time
   0 nil
   (lambda (msg sec)
     (let* ((prev-prompt-contents
             (buffer-substring (line-beginning-position)
                               (line-end-position)))
            (prev-prompt-contents-p
             (not (string= prev-prompt-contents "")))
            (helmp (fboundp 'helm-mode)))
       (when prev-prompt-contents-p
         (delete-region (line-beginning-position)
                        (line-end-position)))
       (insert (propertize msg 'face 'warning))
       ;; stop checking for candidates
       ;; and update the helm prompt
       (when helmp (helm-suspend-update t))
       (sit-for sec)
       (delete-region (line-beginning-position)
                      (line-end-position))
       (when prev-prompt-contents-p
         (insert prev-prompt-contents)
         ;; start checking for candidates
         ;; and update the helm prompt
         (when helmp (helm-suspend-update nil)))))
   msg sec))

(defun spacemacs/delete-file (filename &optional ask-user)
  "Remove specified file or directory.

Also kills associated buffer (if any exists) and invalidates
projectile cache when it's possible.

When ASK-USER is non-nil, user will be asked to confirm file
removal."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (when (or (not ask-user)
              (yes-or-no-p "Are you sure you want to delete this file? "))
      (delete-file filename)
      (when (and (configuration-layer/package-used-p 'projectile)
                 (projectile-project-p))
        (call-interactively #'projectile-invalidate-cache)))))

(defun spacemacs/delete-file-confirm (filename)
  "Remove specified file or directory after users approval.

FILENAME is deleted using `spacemacs/delete-file' function.."
  (interactive "f")
  (funcall-interactively #'spacemacs/delete-file filename t))

;; from magnars
(defcustom spacemacs-keep-legacy-current-buffer-delete-bindings t
  "If nil, \\[spacemacs/delete-current-buffer-file-yes] deletes without confirmation.

This variable exists to preserve the previous behavior of the SPC
f D key binding, since users may be accustomed to seeing a prompt
before deleting."
  :type 'boolean
  :group 'spacemacs)

(defun spacemacs/delete-current-buffer-file (&optional arg)
  "Removes file connected to current buffer and kills buffer.

If prefix ARG is non-nil, delete without confirmation."
  (interactive "P")
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (if (or arg
              (yes-or-no-p
               (format "Are you sure you want to delete this file: '%s'?" name)))
          (progn
            (delete-file filename t)
            (kill-buffer buffer)
            (when (and (configuration-layer/package-used-p 'projectile)
                       (projectile-project-p))
              (call-interactively #'projectile-invalidate-cache))
            (message "File deleted: '%s'" filename))
        (message "Canceled: File deletion")))))

(defun spacemacs/delete-current-buffer-file-yes ()
  "Removes file connected to current buffer and kills buffer, without prompting.

For backwards compatibility, this command actually still prompts
the user if
`spacemacs-keep-legacy-current-buffer-delete-bindings' is
non-nil.  That customization is planned to be removed (and this
function will never prompt) in the future."
  (interactive)
  ;; Warn the user about the upcoming change to the defaults, which is
  ;; potentially dangerous.
  (when spacemacs-keep-legacy-current-buffer-delete-bindings
    (display-warning '(spacemacs delete-current-file-key-bindings-change)
                     (substitute-command-keys "\
\\[spacemacs/delete-current-buffer-file-yes] will stop prompting for confirmation in a future version of Spacemacs.

Use \\[spacemacs/delete-current-buffer-file] to delete files if you want to be prompted for confirmation.

Customize `spacemacs-keep-legacy-current-buffer-delete-bindings'
to nil to make \\[spacemacs/delete-current-buffer-file-yes] stop
prompting (opting into the future behavior).  Doing so will also
suppress this warning.")))

  (funcall #'spacemacs/delete-current-buffer-file
           (not spacemacs-keep-legacy-current-buffer-delete-bindings)))

;; from magnars
(defun spacemacs/sudo-edit (&optional arg)
  (interactive "P")
  (require 'tramp)
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (if (not (tramp-tramp-file-p fname))
         (concat "/sudo:root@localhost:" fname)
       (with-parsed-tramp-file-name fname parsed
         (when (equal parsed-user "root")
           (error "Already root!"))
         (let* ((new-hop (tramp-make-tramp-file-name
                          ;; Try to retrieve a tramp method suitable for
                          ;; multi-hopping
                          (cond ((tramp-get-method-parameter
                                  parsed 'tramp-login-program))
                                ((tramp-get-method-parameter
                                  parsed 'tramp-copy-program))
                                (t parsed-method))
                          parsed-user
                          parsed-domain
                          parsed-host
                          parsed-port
                          nil
                          parsed-hop))
                (new-hop (substring new-hop 1 -1))
                (new-hop (concat new-hop "|"))
                (new-fname (tramp-make-tramp-file-name
                            "sudo"
                            parsed-user
                            parsed-domain
                            parsed-host
                            parsed-port
                            parsed-localname
                            new-hop)))
           new-fname))))))

;; check when opening large files - literal file open
(defun spacemacs/check-large-file ()
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and
           (not (memq major-mode spacemacs-large-file-modes-list))
           size (> size (* 1024 1024 dotspacemacs-large-file-size))
           (y-or-n-p (format (concat "%s is a large file, open literally to "
                                     "avoid performance issues?")
                             filename)))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))

(defun spacemacs/delete-window (&optional arg)
  "Delete the current window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))

;; our own implementation of kill-this-buffer from menu-bar.el
(defun spacemacs/kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

;; found at http://emacswiki.org/emacs/KillingBuffers
(defun spacemacs/kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix argument is used then kill the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (let* ((buffers-to-kill (if (bound-and-true-p persp-mode)
                                (persp-buffer-list)
                              (buffer-list))))
      (mapc 'kill-buffer (delq (current-buffer) buffers-to-kill)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

(defun spacemacs//confirm-kill-buffer ()
  "Prompt the user to save a buffer to a file before killing it.
This skips the following buffers:
- A buffer with non-nil value of variable `buffer-file-name'.
  Or in other words, a buffer who has a file associated with.
  Emacs by default prompts the user to save it if it's modified.
- A buffer derived from `special-mode'."
  (when (and (not buffer-file-name)
             (buffer-modified-p)
             (not (derived-mode-p 'special-mode))
             (not (yes-or-no-p (format "Buffer %S modified; kill anyway? " (buffer-name)))))
    (save-buffer)))

;; from http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun spacemacs/toggle-current-window-dedication ()
  "Toggle dedication state of a window. Commands that change the buffer that a
window is displaying will not typically change the buffer displayed by
a dedicated window."
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(defun spacemacs//init-visual-line-keys ()
  (evil-define-minor-mode-key 'motion 'visual-line-mode "j" 'evil-next-visual-line)
  (evil-define-minor-mode-key 'motion 'visual-line-mode "k" 'evil-previous-visual-line)
  (evil-define-minor-mode-key 'motion 'visual-line-mode (kbd "<down>") 'evil-next-visual-line)
  (evil-define-minor-mode-key 'motion 'visual-line-mode (kbd "<up>") 'evil-previous-visual-line))


;; Copy file path

(defun spacemacs--directory-path ()
  "Retrieve the directory path of the current buffer.

If the buffer is not visiting a file, use the `list-buffers-directory' variable
as a fallback to display the directory, useful in buffers like the ones created
by `magit' and `dired'.

Returns:
  - A string containing the directory path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (directory-name (if-let (file-name (buffer-file-name))
                                (file-name-directory file-name)
                              list-buffers-directory))
    (file-truename directory-name)))

(defun spacemacs--file-path ()
  "Retrieve the file path of the current buffer.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (buffer-file-name))
    (file-truename file-path)))

(defun spacemacs--file-path-with-line ()
  "Retrieve the file path of the current buffer, including line number.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (spacemacs--file-path))
    (concat file-path ":" (number-to-string (line-number-at-pos)))))

(defun spacemacs--file-path-with-line-column ()
  "Retrieve the file path of the current buffer,
including line and column number.

This function respects the `column-number-indicator-zero-based' variable.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (spacemacs--file-path-with-line))
    (format "%s:%s" file-path
            (+ (current-column) (if column-number-indicator-zero-based 0 1)))))

(defun spacemacs/copy-directory-path ()
  "Copy and show the directory path of the current buffer.

If the buffer is not visiting a file, use the `list-buffers-directory'
variable as a fallback to display the directory, useful in buffers like the
ones created by `magit' and `dired'."
  (interactive)
  (if-let (directory-path (spacemacs--directory-path))
      (progn
        (kill-new directory-path)
        (message "%s" directory-path))
    (message "WARNING: Current buffer does not have a directory!")))

(defun spacemacs/copy-file-path ()
  "Copy and show the file path of the current buffer."
  (interactive)
  (if-let (file-path (spacemacs--file-path))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun spacemacs/copy-file-name ()
  "Copy and show the file name of the current buffer."
  (interactive)
  (if-let* ((file-path (spacemacs--file-path))
            (file-name (file-name-nondirectory file-path)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun spacemacs/copy-buffer-name ()
  "Copy and show the name of the current buffer."
  (interactive)
  (kill-new (buffer-name))
  (message "%s" (buffer-name)))

(defun spacemacs/copy-file-name-base ()
  "Copy and show the file name without its final extension of the current
buffer."
  (interactive)
  (if-let (file-name (file-name-base (spacemacs--file-path)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun spacemacs/copy-file-path-with-line ()
  "Copy and show the file path of the current buffer, including line number."
  (interactive)
  (if-let (file-path (spacemacs--file-path-with-line))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun spacemacs/copy-file-path-with-line-column ()
  "Copy and show the file path of the current buffer,
including line and column number.

This function respects the value of the `column-number-indicator-zero-based'
variable."
  (interactive)
  (if-let (file-path (spacemacs--file-path-with-line-column))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))



;; adapted from bozhidar
;; http://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun spacemacs/find-user-init-file ()
  "Edit the `user-init-file', in the current window."
  (interactive)
  (find-file-existing user-init-file))

(defun spacemacs/find-user-early-init-file ()
  "Edit the `early-init-file', in the current window."
  (interactive)
  (find-file-existing early-init-file))

(defun spacemacs/find-dotfile ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (find-file-existing (dotspacemacs/location)))

(defun spacemacs/ediff-dotfile-and-template ()
  "ediff the current `dotfile' with the template"
  (interactive)
  (ediff-files (dotspacemacs/location)
               (concat dotspacemacs-template-directory ".spacemacs.template")))

(defun spacemacs/new-empty-buffer (&optional split)
  "Create a new buffer called: \"untitled\".

SPLIT decides where the buffer opens:
- nil, open in current window.
- `left', `below', `above' or `right', split the window in the given direction.
- `frame', open in new frame.

If the variable `dotspacemacs-new-empty-buffer-major-mode' has been set,
then apply that major mode to the new buffer."
  (interactive)
  (let ((newbuf (generate-new-buffer "untitled")))
    (cl-case split
      (left  (split-window-horizontally))
      (below (spacemacs/split-window-vertically-and-switch))
      (above (split-window-vertically))
      (right (spacemacs/split-window-horizontally-and-switch))
      (frame (select-frame (make-frame))))
    ;; Prompt to save on `save-some-buffers' with positive PRED
    (with-current-buffer newbuf
      (setq-local buffer-offer-save t)
      (add-hook 'kill-buffer-hook
                #'spacemacs//confirm-kill-buffer
                nil t)
      (when dotspacemacs-new-empty-buffer-major-mode
        (funcall dotspacemacs-new-empty-buffer-major-mode)))
    ;; pass non-nil force-same-window to prevent `switch-to-buffer' from
    ;; displaying buffer in another window
    (switch-to-buffer newbuf nil 'force-same-window)))

(defun spacemacs/new-empty-buffer-left ()
  "Create a new buffer called untitled(<n>),
in a split window to the left."
  (interactive)
  (spacemacs/new-empty-buffer 'left))

(defun spacemacs/new-empty-buffer-below ()
  "Create a new buffer called untitled(<n>),
in a split window below."
  (interactive)
  (spacemacs/new-empty-buffer 'below))

(defun spacemacs/new-empty-buffer-above ()
  "Create a new buffer called untitled(<n>),
in a split window above."
  (interactive)
  (spacemacs/new-empty-buffer 'above))

(defun spacemacs/new-empty-buffer-right ()
  "Create a new buffer called untitled(<n>),
in a split window to the right."
  (interactive)
  (spacemacs/new-empty-buffer 'right))

(defun spacemacs/new-empty-buffer-new-frame ()
  "Create a new buffer called untitled(<n>),
in a new frame."
  (interactive)
  (spacemacs/new-empty-buffer 'frame))

;; from https://gist.github.com/timcharper/493269
(defun spacemacs/split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun spacemacs/split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))


;; Window Split

(defun spacemacs--window-split-splittable-windows ()
  (seq-remove
   (lambda (window)
     ;; TODO: find a way to identify unsplittable side windows reliably!
     nil)
   (spacemacs--window-split-non-ignored-windows)))

(defun spacemacs--window-split-non-ignored-windows ()
  "Determines the list of windows to be deleted."
  (seq-filter
   (lambda (window)
     (let* ((name (buffer-name (window-buffer window)))
            (prefixes-matching
             (seq-filter
              (lambda (prefix) (string-prefix-p prefix name))
              spacemacs-window-split-ignore-prefixes)))
       (not prefixes-matching)))
   (window-list (selected-frame))))

(defun spacemacs/window-split-default-delete ()
  "Deletes other windows, except a list of excluded ones."
  (if spacemacs-window-split-ignore-prefixes
      (let* ((deletable (spacemacs--window-split-non-ignored-windows))
             (splittable (spacemacs--window-split-splittable-windows)))
        (when splittable
          (let* ((selected (car splittable))
                 (to-delete (delq selected deletable)))
            (select-window selected)
            (dolist (window to-delete) (delete-window window)))))
    (delete-other-windows)))

(defvar spacemacs-window-split-ignore-prefixes nil
  "Prefixes for windows that are not deleted when changing split layout.

You can add an entry here by using the following:
(add-to-list 'spacemacs-window-split-ignore-prefixes \"Buffer prefix\")")

(defvar spacemacs-window-split-delete-function
  'spacemacs/window-split-default-delete
  "Function used to delete other windows when changing layout.

Used as a callback by the following functions:
  - spacemacs/window-split-grid
  - spacemacs/window-split-triple-columns
  - spacemacs/window-split-double-columns
  - spacemacs/window-split-single-column

Possible values:
  - 'spacemacs/window-split-default-delete (default)
  - 'delete-other-windows
  - 'treemacs-delete-other-windows (when using the treemacs package)
  - a lambda: (lambda () (delete-other-windows))
  - a custom function:
    (defun my-delete-other-windows () (delete-other-windows))
    (setq spacemacs-window-split-delete-function 'my-delete-other-windows)")

(defun spacemacs//window-split-eligible-buffers ()
  "Return a list of buffers to display automatically when splitting windows.

This excludes ephemeral buffers (those whose names begin with a
space), unless they are visitin a file, just as `list-buffers' does."
  (seq-remove
   (lambda (b)
     (and (string= (substring (buffer-name b) 0 1) " ")
          (not (buffer-file-name b))))
   (buffer-list)))

(defun spacemacs/window-split-grid (&optional purge)
  "Set the layout to a 2x2 grid.

Uses the funcion defined in `spacemacs-window-split-delete-function'
as a means to remove windows.

When called with a prefix argument, it uses `delete-other-windows'
as a means to remove windows, regardless of the value in
`spacemacs-window-split-delete-function'."
  (interactive "P")
  (if purge
      (let ((ignore-window-parameters t))
        (delete-other-windows))
    (funcall spacemacs-window-split-delete-function))
  (if (spacemacs--window-split-splittable-windows)
      (let* ((previous-files (spacemacs//window-split-eligible-buffers))
             (second (split-window-below))
             (third (split-window-right))
             (fourth (split-window second nil 'right)))
        (set-window-buffer third (or (nth 1 previous-files) "*scratch*"))
        (set-window-buffer second (or (nth 2 previous-files) "*scratch*"))
        (set-window-buffer fourth (or (nth 3 previous-files) "*scratch*"))
        (balance-windows))
    (message "There are no main windows available to split!")))

(defun spacemacs/window-split-triple-columns (&optional purge)
  "Set the layout to triple columns.

Uses the funcion defined in `spacemacs-window-split-delete-function'
as a means to remove windows.

When called with a prefix argument, it uses `delete-other-windows'
as a means to remove windows, regardless of the value in
`spacemacs-window-split-delete-function'."
  (interactive "P")
  (if purge
      (let ((ignore-window-parameters t))
        (delete-other-windows))
    (funcall spacemacs-window-split-delete-function))
  (if (spacemacs--window-split-splittable-windows)
      (let* ((previous-files (spacemacs//window-split-eligible-buffers))
             (second (split-window-right))
             (third (split-window second nil 'right)))
        (set-window-buffer second (or (nth 1 previous-files) "*scratch*"))
        (set-window-buffer third (or (nth 2 previous-files) "*scratch*"))
        (balance-windows))
    (message "There are no main windows available to split!")))

(defun spacemacs/window-split-double-columns (&optional purge)
  "Set the layout to double columns.

Uses the funcion defined in `spacemacs-window-split-delete-function'
as a means to remove windows.

Left side window is the current buffer. Right side one is the
most recently selected buffer other than current buffer.

When called with a prefix argument, it uses `delete-other-windows'
as a means to remove windows, regardless of the value in
`spacemacs-window-split-delete-function'."
  (interactive "P")
  (if purge
      (let ((ignore-window-parameters t))
        (delete-other-windows))
    (funcall spacemacs-window-split-delete-function))
  (if (spacemacs--window-split-splittable-windows)
      (let* ((right-side-buffer (other-buffer (current-buffer) t)))
        (set-window-buffer (split-window-right) right-side-buffer)
        (balance-windows))
    (message "There are no main windows available to split!")))

(defun spacemacs/window-split-single-column (&optional purge)
  "Set the layout to single column.

Uses the funcion defined in `spacemacs-window-split-delete-function'
as a means to remove windows.

When called with a prefix argument, it uses `delete-other-windows'
as a means to remove windows, regardless of the value in
`spacemacs-window-split-delete-function'."
  (interactive "P")
  (if purge
      (let ((ignore-window-parameters t))
        (delete-other-windows))
    (funcall spacemacs-window-split-delete-function))
  (balance-windows))



(defun spacemacs/insert-line-above-no-indent (count)
  "Insert a new line above with no indentation."
  (interactive "p")
  (let ((p (+ (point) count)))
    (save-excursion
      (if (eq (line-number-at-pos) 1)
          (evil-move-beginning-of-line)
        (progn
          (evil-previous-line)
          (evil-move-end-of-line)))
      (while (> count 0)
        (insert "\n")
        (setq count (1- count))))
    (goto-char p)))

(defun spacemacs/insert-line-below-no-indent (count)
  "Insert a new line below with no indentation."
  (interactive "p")
  (save-excursion
    (evil-move-end-of-line)
    (while (> count 0)
      (insert "\n")
      (setq count (1- count)))))

;; see https://github.com/gempesaw/dotemacs/blob/emacs/dg-elisp/dg-defun.el
(defun spacemacs/rudekill-matching-buffers (regexp &optional internal-too)
  "Kill - WITHOUT ASKING - buffers whose name matches the specified REGEXP. See
the `kill-matching-buffers` for grateful killing. The optional 2nd argument
indicates whether to kill internal buffers too.

Returns the count of killed buffers."
  (let* ((buffers (cl-remove-if-not
                   (lambda (buffer)
                     (let ((name (buffer-name buffer)))
                       (and name (not (string-equal name ""))
                            (or internal-too (/= (aref name 0) ?\s))
                            (string-match regexp name))))
                   (buffer-list))))
    (mapc 'kill-buffer buffers)
    (length buffers)))

(defun spacemacs/kill-matching-buffers-rudely (regexp &optional internal-too)
  "Kill - WITHOUT ASKING - buffers whose name matches the specified REGEXP. See
the `kill-matching-buffers` for grateful killing. The optional 2nd argument
indicates whether to kill internal buffers too.

Returns a message with the count of killed buffers."
  (interactive "sKill buffers matching this regular expression: \nP")
  (message
   (format "%d buffer(s) killed."
           (spacemacs/rudekill-matching-buffers regexp internal-too))))

;; advise to prevent server from closing when the window manager closes the last
;; Emacs frame, and `dotspacemacs-persistent-server' is non-nil.

(defvar spacemacs-really-kill-emacs nil
  "If nil, prevent window manager and \\[save-buffers-kill-emacs] from killing Emacs.

This only has an effect if `dotspacemacs-persistent-server' is non-nil.

This variable should be let-bound to t to actually kill Emacs,
such as is done by \\[spacemacs/prompt-kill-emacs].")

(defun spacemacs//persistent-server-running-p ()
  "Return t if `spacemacs-really-kill-emacs' should prevent killing Emacs."
  (and (fboundp 'server-running-p)
       (server-running-p)
       dotspacemacs-persistent-server))

(define-advice kill-emacs (:around (f &rest args) spacemacs-really-exit)
  "Do not actually kill Emacs if a persistent server is running.

If `dotspacemacs-persistent-server' is non-nil and the Emacs
server is running, just kill the current frame instead of the
Emacs server.

Setting `spacemacs-really-kill-emacs' non-nil overrides this advice."
  (if (and (not spacemacs-really-kill-emacs)
           (spacemacs//persistent-server-running-p))
      (spacemacs/frame-killer)
    (apply f args)))

(define-advice save-buffers-kill-emacs (:around (f &rest args) spacemacs-really-exit)
  "Do not actually kill Emacs if a persistent server is running.

If `dotspacemacs-persistent-server' is non-nil and the Emacs
server is running, just kill the current frame instead of the
Emacs server.

Setting `spacemacs-really-kill-emacs' non-nil overrides this advice."
  (if (and (not spacemacs-really-kill-emacs)
           (spacemacs//persistent-server-running-p))
      (spacemacs/frame-killer)
    (apply f args)))

(defun spacemacs/save-buffers-kill-emacs ()
  "Save all changed buffers and exit Spacemacs"
  (interactive)
  (let ((spacemacs-really-kill-emacs t))
    (save-buffers-kill-emacs)))

(defun spacemacs/kill-emacs ()
  "Lose all changes and exit Spacemacs"
  (interactive)
  (let ((spacemacs-really-kill-emacs t))
    (kill-emacs)))

(defun spacemacs/prompt-kill-emacs ()
  "Prompt to save changed buffers and exit Spacemacs"
  (interactive)
  (save-some-buffers nil t)
  (let ((spacemacs-really-kill-emacs t))
    (kill-emacs)))

(defun spacemacs/frame-killer ()
  "Kill server buffer and hide the main Emacs window"
  (interactive)
  (condition-case nil
      (delete-frame nil 1)
    (error
     (make-frame-invisible nil 1))))

(defun spacemacs/toggle-frame-fullscreen ()
  "Respect the `dotspacemacs-fullscreen-use-non-native' variable when
toggling fullscreen."
  (interactive)
  (if dotspacemacs-fullscreen-use-non-native
      (spacemacs/toggle-frame-fullscreen-non-native)
    (toggle-frame-fullscreen)))

(defun spacemacs/toggle-fullscreen ()
  "Toggle full screen on X11 and Carbon"
  (interactive)
  (cond
   ((eq window-system 'x)
    (set-frame-parameter nil 'fullscreen
                         (unless (frame-parameter nil 'fullscreen)
                           'fullboth)))
   ((eq window-system 'mac)
    (set-frame-parameter
     nil 'fullscreen
     (unless (frame-parameter nil 'fullscreen)) 'fullscreen))))

(defun spacemacs/toggle-frame-fullscreen-non-native ()
  "Toggle full screen using the `fullboth' frame parameter.
Using the `fullboth' frame parameter rather than `fullscreen' is
useful to use full screen on macOS without animations."
  (interactive)
  (modify-frame-parameters
   nil
   `((maximized
      . ,(unless (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
           (frame-parameter nil 'fullscreen)))
     (fullscreen
      . ,(if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
             (if (eq (frame-parameter nil 'maximized) 'maximized)
                 'maximized)
           'fullboth)))))

(defun spacemacs/safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
      (erase-buffer)))

(defun spacemacs//find-ert-test-buffer (ert-test)
  "Return the buffer where ERT-TEST is defined."
  (save-excursion
    (car (find-definition-noselect (ert-test-name ert-test) 'ert-deftest))))

(defun spacemacs/ert-run-tests-buffer ()
  "Run all the tests in the current buffer."
  (interactive)
  (save-buffer)
  (load-file (buffer-file-name))
  (let ((cbuf (current-buffer)))
    (ert '(satisfies (lambda (test)
                       (eq cbuf (spacemacs//find-ert-test-buffer test)))))))

(defun spacemacs//open-in-external-app (file-path)
  "Open `file-path' in external application."
  (cond
   ((spacemacs/system-is-mswindows)
    (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
   ((spacemacs/system-is-mac) (shell-command (format "open \"%s\"" file-path)))
   ((spacemacs/system-is-linux) (let ((process-connection-type nil))
                                  (start-process "" nil "xdg-open" file-path)))))

(defun spacemacs/open-file-or-directory-in-external-app (arg)
  "Open current file in external application.
If the universal prefix argument is used then open the folder
containing the current file by the default explorer."
  (interactive "P")
  (if arg
      (spacemacs//open-in-external-app (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (spacemacs//open-in-external-app file-path)
        (message "No file associated to this buffer.")))))

(defun spacemacs/switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

;; https://stackoverflow.com/a/10216338
(defun spacemacs/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun spacemacs/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;; BEGIN align functions

;; modified function from http://emacswiki.org/emacs/AlignCommands
(defun spacemacs/align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let* ((ws-regexp (if (string-empty-p regexp)
                        "\\(\\s-+\\)"
                      "\\(\\s-*\\)"))
         (complete-regexp (if after
                              (concat regexp ws-regexp)
                            (concat ws-regexp regexp)))
         (group (if justify-right -1 1)))

    (unless (use-region-p)
      (save-excursion
        (while (and
                (string-match-p complete-regexp (thing-at-point 'line))
                (= 0 (forward-line -1)))
          (setq start (point-at-bol))))
      (save-excursion
        (while (and
                (string-match-p complete-regexp (thing-at-point 'line))
                (= 0 (forward-line 1)))
          (setq end (point-at-eol)))))

    (align-regexp start end complete-regexp group 1 t)))

;; Modified answer from https://emacs.stackexchange.com/a/48
(defun spacemacs/align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)"
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defmacro spacemacs|create-align-repeat-x (name regexp &optional justify-right default-after)
  (let* ((new-func (intern (concat "spacemacs/align-repeat-" name)))
         (new-func-defn
          `(defun ,new-func (start end switch)
             (interactive "r\nP")
             (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
               (spacemacs/align-repeat start end ,regexp ,justify-right after)))))
    (put new-func 'function-documentation "Created by `spacemacs|create-align-repeat-x'.")
    new-func-defn))

(spacemacs|create-align-repeat-x "comma" "," nil t)
(spacemacs|create-align-repeat-x "semicolon" ";" nil t)
(spacemacs|create-align-repeat-x "colon" ":" nil t)
(spacemacs|create-align-repeat-x "equal" "=")
(spacemacs|create-align-repeat-x "math-oper" "[+\\-*/]")
(spacemacs|create-align-repeat-x "percent" "%")
(spacemacs|create-align-repeat-x "ampersand" "&")
(spacemacs|create-align-repeat-x "bar" "|")
(spacemacs|create-align-repeat-x "left-paren" "(")
(spacemacs|create-align-repeat-x "right-paren" ")" t)
(spacemacs|create-align-repeat-x "left-curly-brace" "{")
(spacemacs|create-align-repeat-x "right-curly-brace" "}" t)
(spacemacs|create-align-repeat-x "left-square-brace" "\\[")
(spacemacs|create-align-repeat-x "right-square-brace" "\\]" t)
(spacemacs|create-align-repeat-x "backslash" "\\\\")

;; END align functions

(defun spacemacs/dos2unix ()
  "Converts the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun spacemacs/unix2dos ()
  "Converts the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun spacemacs/save-as (filename &optional visit)
  "Save current buffer or active region as specified file.
When called interactively, it first prompts for FILENAME, and then asks
whether to VISIT it, and if so, whether to show it in current window or
another window. The variable `spacemacs-save-as-visit-action' can be
customized to supply a default VISIT action suppressing the latter prompt.
When prefixed with a universal-argument \\[universal-argument], include
filename in prompt.

FILENAME  a non-empty string as the name of the saved file.
VISIT     When it's `:current', open FILENAME in current window. When it's
          `:other', open FILENAME in another window. When it's nil, only
          save to FILENAME but does not visit it. (Default to `:current'
          when called from a LISP program.)

When FILENAME already exists, it also asks the user whether to
overwrite it."
  (interactive (let* ((filename (expand-file-name (read-file-name "Save buffer as: " nil nil nil
                                                                  (when current-prefix-arg (buffer-name)))))
                      (choices  '("Current window"
                                  "Other window"
                                  "Don't open"))
                      (actions  '(:current :other nil))
                      (visit
                       (if (eq spacemacs-save-as-visit-action 'ask)
                           (let ((completion-ignore-case t))
                             (nth (cl-position
                                   (completing-read "Do you want to open the file? "
                                                    choices nil t)
                                   choices
                                   :test #'equal)
                                  actions))
                         spacemacs-save-as-visit-action)))
                 (list filename visit)))
  (unless (called-interactively-p 'any)
    (cl-assert (and (stringp filename)
                    (not (string-empty-p filename))
                    (not (directory-name-p filename)))
               t "Expect a non-empty filepath, found: %s")
    (setq filename (expand-file-name filename)
          visit (or visit :other))
    (let ((choices '(:current :other nil)))
      (cl-assert (memq visit choices)
                 t "Found %s, expect one of %s")))
  (let ((dir (file-name-directory filename)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (if (use-region-p)
      (write-region (region-beginning) (region-end) filename nil nil nil t)
    (write-region nil nil filename nil nil nil t))
  (pcase visit
    (:current (find-file filename))
    (:other   (funcall-interactively 'find-file-other-window filename))))


;; from https://www.emacswiki.org/emacs/CopyingWholeLines
(defun spacemacs/duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ; Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      ;; Go to beginning of next line, or make a new one
                      (if (< 0 (forward-line 1))
                          (newline))))))
        (dotimes (i (abs (or n 1)))   ; Insert N times, or once if not specified
          (insert text))))
    (if use-region nil        ; Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ; Save column
        (if (> 0 n)                     ; Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

;; credits to Steve Purcell
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-editing-utils.el
;; https://emacsredux.com/blog/2013/04/08/kill-line-backward/
(defun spacemacs/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(defun spacemacs/uniquify-lines ()
  "Remove duplicate adjacent lines in a region or the current buffer"
  (interactive)
  (save-excursion
    (save-restriction
      (let* ((region-active (or (region-active-p) (evil-visual-state-p)))
             (beg (if region-active (region-beginning) (point-min)))
             (end (if region-active (region-end) (point-max))))
        (goto-char beg)
        (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
          (replace-match "\\1"))))))

(defun spacemacs/sort-lines (&optional reverse)
  "Sort lines in a region or the current buffer.
A non-nil argument sorts in reverse order."
  (interactive "P")
  (let* ((region-active (or (region-active-p) (evil-visual-state-p)))
         (beg (if region-active (region-beginning) (point-min)))
         (end (if region-active (region-end) (point-max))))
    (sort-lines reverse beg end)))

(defun spacemacs/sort-lines-reverse ()
  "Sort lines in reverse order, in a region or the current buffer."
  (interactive)
  (spacemacs/sort-lines -1))

(defun spacemacs/sort-lines-by-column (&optional reverse)
  "Sort lines by the selected column,
using a visual block/rectangle selection.
A non-nil argument sorts in REVERSE order."
  (interactive "P")
  (if (and
       ;; is there an active selection
       (or (region-active-p) (evil-visual-state-p))
       ;; is it a block or rectangle selection
       (or (eq evil-visual-selection 'block) (eq rectangle-mark-mode t))
       ;; is the selection height 2 or more lines
       (>= (1+ (- (line-number-at-pos (region-end))
                  (line-number-at-pos (region-beginning)))) 2))
      (sort-columns reverse (region-beginning) (region-end))
    (error
     "Sorting by column requires a block/rect selection on 2 or more lines.")))

(defun spacemacs/sort-lines-by-column-reverse ()
  "Sort lines by the selected column in reverse order,
using a visual block/rectangle selection."
  (interactive)
  (spacemacs/sort-lines-by-column -1))

;; Show scroll bar when using the mouse wheel
(defun spacemacs//scroll-bar-hide ()
  " Hide the scroll bar."
  (scroll-bar-mode -1))

(defun spacemacs//scroll-bar-show-delayed-hide (&rest _ignore)
  "Show the scroll bar for a couple of seconds, before hiding it.

This can be used to temporarily show the scroll bar when mouse wheel scrolling.
(advice-add 'mwheel-scroll :after #'spacemacs//scroll-bar-show-delayed-hide)

The advice can be removed with:
(advice-remove 'mwheel-scroll #'spacemacs//scroll-bar-show-delayed-hide)"
  (scroll-bar-mode 1)
  (run-with-idle-timer
   (if (numberp dotspacemacs-scroll-bar-while-scrolling)
       dotspacemacs-scroll-bar-while-scrolling
     3)
   nil
   #'spacemacs//scroll-bar-hide))
(when (and (fboundp 'scroll-bar-mode)
           dotspacemacs-scroll-bar-while-scrolling)
  (advice-add 'mwheel-scroll :after #'spacemacs//scroll-bar-show-delayed-hide))

;; from http://www.emacswiki.org/emacs/WordCount
(defun spacemacs/count-words-analysis (start end)
  "Count how many times each word is used in the region.
 Punctuation is ignored."
  (interactive "r")
  (let (words
        alist_words_compare
        (formatted "")
        (overview (call-interactively 'count-words)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\w+" end t)
        (let* ((word (intern (match-string 0)))
               (cell (assq word words)))
          (if cell
              (setcdr cell (1+ (cdr cell)))
            (setq words (cons (cons word 1) words))))))
    (defun alist_words_compare (a b)
      "Compare elements from an associative list of words count.
Compare them on count first,and in case of tie sort them alphabetically."
      (let ((a_key (car a))
            (a_val (cdr a))
            (b_key (car b))
            (b_val (cdr b)))
        (if (eq a_val b_val)
            (string-lessp a_key b_key)
          (> a_val b_val))))
    (setq words (cl-sort words 'alist_words_compare))
    (while words
      (let* ((word (pop words))
             (name (car word))
             (count (cdr word)))
        (setq formatted (concat formatted (format "[%s: %d], " name count)))))
    (when (interactive-p)
      (if (> (length formatted) 2)
          (message (format "%s\nWord count: %s"
                           overview
                           (substring formatted 0 -2)))
        (message "No words.")))
    words))

;; indent on paste
;; from Prelude: https://github.com/bbatsov/prelude
(defun spacemacs/yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) spacemacs-yank-indent-threshold)
      (indent-region beg end nil)))

(defun spacemacs//yank-indent-region (yank-func &rest args)
  "If current mode is not one of spacemacs-indent-sensitive-modes
indent yanked text (with universal arg don't indent)."
  (evil-start-undo-step)
  (prog1
      (let ((prefix (car args))
            (enable (and (not (member major-mode spacemacs-indent-sensitive-modes))
                         (or (derived-mode-p 'prog-mode)
                             (member major-mode spacemacs-yank-indent-modes)))))
        (when (and enable (equal '(4) prefix))
          (setq args (cdr args)))
        (prog1
            (apply yank-func args)
          (when (and enable (not (equal '(4) prefix)))
            (let ((transient-mark-mode nil)
                  (save-undo buffer-undo-list))
              (spacemacs/yank-advised-indent-function (region-beginning)
                                                      (region-end))))))
    (evil-end-undo-step)))

(dolist (func '(yank yank-pop evil-paste-before evil-paste-after))
  (advice-add func :around #'spacemacs//yank-indent-region))

;; find file functions in split
(defun spacemacs//display-in-split (buffer alist)
  "Split selected window and display BUFFER in the new window.
BUFFER and ALIST have the same form as in `display-buffer'. If ALIST contains
a split-side entry, its value must be usable as the SIDE argument for
`split-window'."
  (let ((window (split-window nil nil (cdr (assq 'split-side alist)))))
    (window--display-buffer buffer window 'window alist)
    window))

(defun spacemacs/find-file-vsplit (file)
  "find file in vertical split"
  (interactive "FFind file (vsplit): ")
  (let ((buffer (find-file-noselect file)))
    (pop-to-buffer buffer '(spacemacs//display-in-split (split-side . right)))))

(defun spacemacs/find-file-split (file)
  "find file in horizontal split"
  (interactive "FFind file (split): ")
  (let ((buffer (find-file-noselect file)))
    (pop-to-buffer buffer '(spacemacs//display-in-split (split-side . below)))))

(defun spacemacs/switch-to-help-buffer ()
  "Open or select the `*Help*' buffer, if it exists."
  (interactive)
  (if (get-buffer "*Help*")
      (switch-to-buffer (help-buffer))
    (message "No previous Help buffer found")))

(defun spacemacs/switch-to-scratch-buffer (&optional arg)
  "Switch to the `*scratch*' buffer, creating it first if needed.
if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (let ((exists (get-buffer "*scratch*")))
    (if arg
        (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
      (switch-to-buffer (get-buffer-create "*scratch*")))
    (when (not exists)
      (add-hook 'kill-buffer-hook
                #'spacemacs//confirm-kill-buffer
                nil t)
      (when (and (not (eq major-mode dotspacemacs-scratch-mode))
                 (fboundp dotspacemacs-scratch-mode))
        (funcall dotspacemacs-scratch-mode)
        (run-hooks 'spacemacs-scratch-mode-hook)))))

(defvar spacemacs--killed-buffer-list nil
  "List of recently killed buffers.")

(defun spacemacs//add-buffer-to-killed-list ()
  "If buffer is associated with a file name, add that file
to the `killed-buffer-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name spacemacs--killed-buffer-list)))

(defun spacemacs/reopen-killed-buffer ()
  "Reopen the most recently killed file buffer, if one exists."
  (interactive)
  (when spacemacs--killed-buffer-list
    (find-file (pop spacemacs--killed-buffer-list))))

(defun spacemacs/switch-to-messages-buffer (&optional arg)
  "Switch to the `*Messages*' buffer.
if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (with-current-buffer (messages-buffer)
    (goto-char (point-max))
    (if arg
        (switch-to-buffer-other-window (current-buffer))
      (switch-to-buffer (current-buffer)))
    (when (evil-evilified-state-p)
      (evil-normal-state))))

(defun spacemacs/show-hide-compilation-window ()
  "Show/Hide the window containing the compilation buffer."
  (interactive)
  (when-let ((buffer next-error-last-buffer))
    (if (get-buffer-window buffer 'visible)
        (delete-windows-on buffer)
      (spacemacs/switch-to-compilation-buffer))))

(defun spacemacs/switch-to-compilation-buffer ()
  "Go to last compilation buffer."
  (interactive)
  (if (buffer-live-p next-error-last-buffer)
      (pop-to-buffer next-error-last-buffer)
    (user-error "There is no compilation buffer")))


;; Line number

(defun spacemacs/enable-line-numbers-p ()
  "Return non-nil if line numbers should be enabled for current buffer.
Decision is based on `dotspacemacs-line-numbers'."
  (and dotspacemacs-line-numbers
       (spacemacs//enable-line-numbers-for-buffer-size-p)
       (if (listp dotspacemacs-line-numbers)
           (spacemacs//line-numbers-enabled-for-current-major-mode)
         (and (memq dotspacemacs-line-numbers '(t relative visual))
              (derived-mode-p 'prog-mode 'text-mode)))))

(defun spacemacs/relative-line-numbers-p ()
  "Return non-nil if line numbers should be relative.
Decision is based on `dotspacemacs-line-numbers'."
  (eq (spacemacs/line-numbers-type) 'relative))

(defun spacemacs/visual-line-numbers-p ()
  "Return non-nil if line numbers should be visual.
This is similar to relative line numbers, but wrapped lines are
treated as multiple lines.

Decision is based on `dotspacemacs-line-numbers'."
  (eq (spacemacs/line-numbers-type) 'visual))

(defun spacemacs/line-numbers-type ()
  "Returns a valid value for `display-line-numbers', activating
line numbers, with respect to `dotspacemacs-line-numbers'."
  (if (listp dotspacemacs-line-numbers)
      (cond ((car (spacemacs/mplist-get-values dotspacemacs-line-numbers :visual)) 'visual)
            ((car (spacemacs/mplist-get-values dotspacemacs-line-numbers :relative)) 'relative)
            (t t))
    dotspacemacs-line-numbers))

(defun spacemacs//enable-line-numbers-for-buffer-size-p ()
  "Return non-nil if the current buffer's size is not too big.

This is controlled by the `:size-limit-kb' property of
`dotspacemacs-line-numbers'."
  (if-let ((size-limit-kb
            (and (listp dotspacemacs-line-numbers)
                 (spacemacs/mplist-get-value dotspacemacs-line-numbers
                                             :size-limit-kb))))
      (<= (buffer-size) (* 1000 size-limit-kb))
    t))

;; see tests in tests/layers/+distribution/spacemacs-base/line-numbers-utest.el
;; for the different possible cases
(defun spacemacs//line-numbers-enabled-for-current-major-mode ()
  "Return non-nil if line number is enabled for current major-mode."
  (let* ((disabled-for-modes
          (spacemacs/mplist-get-values dotspacemacs-line-numbers
                                       :disabled-for-modes))
         (user-enabled-for-modes
          (spacemacs/mplist-get-values dotspacemacs-line-numbers
                                       :enabled-for-modes))
         ;; default `enabled-for-modes' to '(prog-mode text-mode), because it is
         ;; a more sensible default than enabling in all buffers - including
         ;; Magit buffers, terminal buffers, etc. But don't include prog-mode or
         ;; text-mode if they're explicitly disabled by user
         (enabled-for-modes (or user-enabled-for-modes
                                (seq-difference '(prog-mode text-mode)
                                                disabled-for-modes
                                                #'eq)))
         (enabled-for-parent (or (and (equal enabled-for-modes '(all)) 'all)
                                 (apply #'derived-mode-p enabled-for-modes)))
         (disabled-for-parent (apply #'derived-mode-p disabled-for-modes)))
    (or
     ;; special case 'all: enable for any mode that isn't specifically disabled
     (and (eq enabled-for-parent 'all) (not disabled-for-parent))
     ;; current mode or a parent is in :enabled-for-modes, and there isn't a
     ;; more specific parent (or the mode itself) in :disabled-for-modes
     (and enabled-for-parent
          (or (not disabled-for-parent)
              ;; handles the case where current major-mode has a parent both in
              ;; :enabled-for-modes and in :disabled-for-modes. Return non-nil
              ;; if enabled-for-parent is the more specific parent (IOW derives
              ;; from disabled-for-parent)
              (provided-mode-derived-p enabled-for-parent disabled-for-parent)))
     ;; current mode (or parent) not explicitly disabled
     (and (null user-enabled-for-modes)
          enabled-for-parent            ; mode is one of default allowed modes
          disabled-for-modes
          (not disabled-for-parent)))))


;; quick run
(defun spacemacs/quickrun ()
  "Call `quickrun' or `quickrun-region'"
  (interactive)
  (if (region-active-p)
      (call-interactively 'quickrun-region)
    (quickrun)))

;; randomize region

(defun spacemacs/randomize-words (beg end)
  "Randomize the order of words in region."
  (interactive "*r")
  (let ((all (mapcar
              (lambda (w) (if (string-match "\\w" w)
                              ;; Randomize words,
                              (cons (random) w)
                            ;; keep everything else in order.
                            (cons -1 w)))
              (split-string
               (delete-and-extract-region beg end) "\\b")))
        words sorted)
    (mapc (lambda (x)
            ;; Words are numbers >= 0.
            (unless (> 0 (car x))
              (setq words (cons x words))))
          all)
    ;; Random sort!
    (setq sorted (sort words
                       (lambda (a b) (< (car a) (car b)))))
    (mapc
     'insert
     ;; Insert using original list, `all',
     ;; but pull *words* from randomly-sorted list, `sorted'.
     (mapcar (lambda (x)
               (if (> 0 (car x))
                   (cdr x)
                 (prog1 (cdar sorted)
                   (setq sorted (cdr sorted)))))
             all))))

(defun spacemacs/randomize-lines (beg end)
  "Randomize lines in region from BEG to END."
  (interactive "*r")
  (let ((lines (split-string
                (delete-and-extract-region beg end) "\n")))
    (when (string-equal "" (car (last lines 1)))
      (setq lines (butlast lines 1)))
    (apply 'insert
           (mapcar 'cdr
                   (sort (mapcar (lambda (x) (cons (random) (concat x "\n"))) lines)
                         (lambda (a b) (< (car a) (car b))))))))


;; narrow region

(defun spacemacs/clone-indirect-buffer-de-activate-mark ()
  "This is a workaround for the evil visual state error message like:
Error in post-command-hook (evil-visual-post-command):
(error \"Marker points into wrong buffer\" #<marker at 27875 in .spacemacs<2>>)"
  (let ((region-was-active (region-active-p)))
    (when region-was-active (deactivate-mark))
    (call-interactively 'clone-indirect-buffer)
    (when region-was-active (activate-mark))))

(defun spacemacs/narrow-to-indirect-buffer (narrower target-name)
  "Use the function `narrower' to narrow within an indirect buffer, except where
the starting buffer is in a state (such as visual block mode) that would cause
this to work incorrectly. `target-name' is the string name of the entity being
narrowed to."
  ;; There may be a way to get visual block mode working similar to the
  ;; workaround we did for visual line mode; this usecase however seems like an
  ;; edgecase at best, so let's patch it if we find out it's needed; otherwise
  ;; let's not hold up the base functionality anymore.
  (if (and (eq evil-state 'visual) (eq evil-visual-selection 'block))
      (message "Cannot narrow to indirect buffer from visual block mode.")
    (when evil-ex-active-highlights-alist
      (spacemacs/evil-search-clear-highlight))
    (spacemacs/clone-indirect-buffer-de-activate-mark)
    (call-interactively narrower)
    (message (format "%s narrowed to an indirect buffer" target-name))))

(defun spacemacs/narrow-to-defun-indirect-buffer ()
  (interactive)
  (spacemacs/narrow-to-indirect-buffer 'narrow-to-defun "Function"))

(defun spacemacs/narrow-to-page-indirect-buffer ()
  (interactive)
  (spacemacs/narrow-to-indirect-buffer 'narrow-to-page "Page"))

(defun spacemacs/narrow-to-region-indirect-buffer ()
  (interactive)
  (spacemacs/narrow-to-indirect-buffer 'narrow-to-region "Region"))
