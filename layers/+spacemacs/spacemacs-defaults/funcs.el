;;; funcs.el --- Spacemacs Defaults Layer functions File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'cl-lib)

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
  :type 'list
  :group 'spacemacs)

(defcustom spacemacs-yank-indent-modes '(latex-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here."
  :type 'list
  :group 'spacemacs)

(defcustom spacemacs-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'spacemacs)

(defcustom spacemacs-large-file-modes-list
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode
                 doc-view-mode doc-view-mode-maybe ebrowse-tree-mode
                 pdf-view-mode fundamental-mode)
  "Major modes which `spacemacs/check-large-file' will not be
automatically applied to."
  :group 'spacemacs
  :type '(list symbol))


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

(defun spacemacs/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (evil-indent (point-min) (point-max))
        (message "Indented buffer.")))
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
      (progn
        (set-buffer-file-coding-system default-file-name-coding-system)
        ;; (set-buffer-file-coding-system 'utf-8-unix)
        (untabify (point-min) (point-max))
        (indent-region (point-min) (point-max))
        (whitespace-cleanup)))))

;; from https://gist.github.com/3402786
(defun spacemacs/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))

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
    (or (with-current-buffer buffer
          (derived-mode-p 'comint-mode))
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

(defun spacemacs/move-buffer-to-window (windownum follow-focus-p)
  "Moves a buffer to a window, using the spacemacs numbering. follow-focus-p
controls whether focus moves to new window (with buffer), or stays on current"
  (interactive)
  (let ((b (current-buffer))
        (w1 (selected-window))
        (w2 (winum-get-window-by-number windownum)))
    (unless (eq w1 w2)
      (set-window-buffer w2 b)
      (switch-to-prev-buffer)
      (unrecord-window-buffer w1 b)))
  (when follow-focus-p (select-window (winum-get-window-by-number windownum))))

(defun spacemacs/swap-buffers-to-window (windownum follow-focus-p)
  "Swaps visible buffers between active window and selected window.
follow-focus-p controls whether focus moves to new window (with buffer), or
stays on current"
  (interactive)
  (let* ((b1 (current-buffer))
         (w1 (selected-window))
         (w2 (winum-get-window-by-number windownum))
         (b2 (window-buffer w2)))
    (unless (eq w1 w2)
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (unrecord-window-buffer w1 b1)
      (unrecord-window-buffer w2 b2)))
  (when follow-focus-p (winum-select-window-by-number windownum)))

(dotimes (i 9)
  (let ((n (+ i 1)))
    (eval `(defun ,(intern (format "buffer-to-window-%s" n)) (&optional arg)
              ,(format "Move buffer to the window with number %i." n)
              (interactive "P")
              (if arg
                  (spacemacs/swap-buffers-to-window ,n t)
                (spacemacs/move-buffer-to-window ,n t))))
    (eval `(defun ,(intern (format "move-buffer-window-no-follow-%s" n)) ()
             (interactive)
             (spacemacs/move-buffer-to-window ,n nil)))
    (eval `(defun ,(intern (format "swap-buffer-window-no-follow-%s" n)) ()
             (interactive)
             (spacemacs/swap-buffers-to-window ,n nil)))
    ))

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

;; from magnars
(defun spacemacs/rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting.
If the buffer isn't visiting a file, ask if it should
be saved to a file, or just renamed.

If called without a prefix argument, the prompt is
initialized with the current directory instead of filename."
  (interactive "P")
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (and filename (file-exists-p filename))
        ;; the buffer is visiting a file
        (let* ((dir (file-name-directory filename))
               (new-name (read-file-name "New name: " (if arg dir filename))))
          (cond ((get-buffer new-name)
                 (error "A buffer named '%s' already exists!" new-name))
                (t
                 (let ((dir (file-name-directory new-name)))
                   (when (and (not (file-exists-p dir))
                              (yes-or-no-p
                               (format "Create directory '%s'?" dir)))
                     (make-directory dir t)))
                 (rename-file filename new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil)
                 (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept filename))
                 (when (and (configuration-layer/package-used-p 'projectile)
                            (projectile-project-p))
                   (call-interactively #'projectile-invalidate-cache))
                 (message "File '%s' successfully renamed to '%s'"
                          name (file-name-nondirectory new-name)))))
      ;; the buffer is not visiting a file
      (let ((key))
        (while (not (memq key '(?s ?r)))
          (setq key (read-key (propertize
                               (format
                                (concat "Buffer '%s' is not visiting a file: "
                                        "[s]ave to file or [r]ename buffer?")
                                name)
                               'face 'minibuffer-prompt)))
          (cond ((eq key ?s)            ; save to file
                 ;; this allows for saving a new empty (unmodified) buffer
                 (unless (buffer-modified-p) (set-buffer-modified-p t))
                 (save-buffer))
                ((eq key ?r)            ; rename buffer
                 (let ((new-name (read-string "New buffer name: ")))
                   (while (get-buffer new-name)
                     ;; ask to rename again, if the new buffer name exists
                     (if (yes-or-no-p
                          (format (concat "A buffer named '%s' already exists: "
                                          "Rename again?")
                                  new-name))
                         (setq new-name (read-string "New buffer name: "))
                       (keyboard-quit)))
                   (rename-buffer new-name)
                   (message "Buffer '%s' successfully renamed to '%s'"
                            name new-name)))
                ;; ?\a = C-g, ?\e = Esc and C-[
                ((memq key '(?\a ?\e)) (keyboard-quit))))))))

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
(defun spacemacs/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (if (yes-or-no-p
            (format "Are you sure you want to delete this file: '%s'?" name))
          (progn
            (delete-file filename t)
            (kill-buffer buffer)
            (when (and (configuration-layer/package-used-p 'projectile)
                       (projectile-project-p))
              (call-interactively #'projectile-invalidate-cache))
            (message "File deleted: '%s'" filename))
        (message "Canceled: File deletion")))))

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
         (let* ((new-hop (tramp-make-tramp-file-name parsed-method
                                                     parsed-user
                                                     parsed-host
                                                     nil
                                                     parsed-hop
                                                     ))
                (new-hop (substring new-hop 1 -1))
                (new-hop (concat new-hop "|"))
                (new-fname (tramp-make-tramp-file-name "sudo"
                                                       "root"
                                                       parsed-host
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
If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

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

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (spacemacs--file-path-with-line))
    (concat
      file-path
      ":"
      (number-to-string (if (and
                             ;; Emacs 26 introduced this variable. Remove this
                             ;; check once 26 becomes the minimum version.
                              (boundp column-number-indicator-zero-based)
                              (not column-number-indicator-zero-based))
                            (1+ (current-column))
                          (current-column))))))

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
  (if-let (file-name (file-name-nondirectory (spacemacs--file-path)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

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
  "Create a new buffer called untitled(<n>).
A SPLIT argument with the value: `left', `below', `above' or `right',
opens the new buffer in a split window.
If the variable `dotspacemacs-new-empty-buffer-major-mode' has been set,
then apply that major mode to the new buffer."
  (interactive)
  (let ((newbuf (generate-new-buffer "untitled")))
    (case split
      ('left  (split-window-horizontally))
      ('below (spacemacs/split-window-vertically-and-switch))
      ('above (split-window-vertically))
      ('right (spacemacs/split-window-horizontally-and-switch))
      ('frame (select-frame (make-frame))))
    ;; Prompt to save on `save-some-buffers' with positive PRED
    (with-current-buffer newbuf
      (setq-local buffer-offer-save t)
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
      (let* ((previous-files (seq-filter #'buffer-file-name
                               (delq (current-buffer) (buffer-list))))
              (second (split-window-below))
              (third (split-window-right))
              (fourth (split-window second nil 'right)))
        (set-window-buffer third (or (car previous-files) "*scratch*"))
        (set-window-buffer second (or (cadr previous-files) "*scratch*"))
        (set-window-buffer fourth (or (caddr previous-files) "*scratch*"))
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
      (let* ((previous-files (seq-filter #'buffer-file-name
                               (delq (current-buffer) (buffer-list))))
              (second (split-window-right))
              (third (split-window second nil 'right)))
        (set-window-buffer second (or (car previous-files) "*scratch*"))
        (set-window-buffer third (or (cadr previous-files) "*scratch*"))
        (balance-windows))
    (message "There are no main windows available to split!")))

(defun spacemacs/window-split-double-columns (&optional purge)
  "Set the layout to double columns.

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
      (let* ((previous-files (seq-filter #'buffer-file-name
                                         (delq (current-buffer) (buffer-list)))))
        (set-window-buffer (split-window-right)
                           (or (car previous-files) "*scratch*"))
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
  (let* ((buffers (remove-if-not
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

;; advise to prevent server from closing

(defvar spacemacs-really-kill-emacs nil
  "prevent window manager close from closing instance.")

(defun spacemacs//persistent-server-running-p ()
  "Requires spacemacs-really-kill-emacs to be toggled and
dotspacemacs-persistent-server to be t"
  (and (fboundp 'server-running-p)
       (server-running-p)
       dotspacemacs-persistent-server))

(defadvice kill-emacs (around spacemacs-really-exit activate)
  "Only kill emacs if a prefix is set"
  (if (and (not spacemacs-really-kill-emacs)
           (spacemacs//persistent-server-running-p))
      (spacemacs/frame-killer)
    ad-do-it))

(defadvice save-buffers-kill-emacs (around spacemacs-really-exit activate)
  "Only kill emacs if a prefix is set"
  (if (and (not spacemacs-really-kill-emacs)
           (spacemacs//persistent-server-running-p))
      (spacemacs/frame-killer)
    ad-do-it))

(defun spacemacs/save-buffers-kill-emacs ()
  "Save all changed buffers and exit Spacemacs"
  (interactive)
  (setq spacemacs-really-kill-emacs t)
  (save-buffers-kill-emacs))

(defun spacemacs/kill-emacs ()
  "Lose all changes and exit Spacemacs"
  (interactive)
  (setq spacemacs-really-kill-emacs t)
  (kill-emacs))

(defun spacemacs/prompt-kill-emacs ()
  "Prompt to save changed buffers and exit Spacemacs"
  (interactive)
  (setq spacemacs-really-kill-emacs t)
  (save-some-buffers nil t)
  (kill-emacs))

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
                         (when (not (frame-parameter nil 'fullscreen))
                           'fullboth)))
   ((eq window-system 'mac)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullscreen)))))

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

(defun spacemacs/safe-revert-buffer ()
  "Prompt before reverting the file."
  (interactive)
  (revert-buffer nil nil))

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

;; http://stackoverflow.com/a/10216338/4869
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

;; Modified answer from http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
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

(defun spacemacs/copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

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

;; BEGIN linum mouse helpers

(defvar spacemacs-linum-mdown-line nil
  "Define persistent variable for linum selection")

(defun spacemacs//line-at-click ()
  "Determine the visual line at click"
  (save-excursion
    (let ((click-y (cddr (mouse-position)))
          (debug-on-error t)
          (line-move-visual t))
      (goto-char (window-start))
      (next-line (1- click-y))
      (1+ (line-number-at-pos))
      )))

(defun spacemacs/md-select-linum (event)
  "Set point as spacemacs-linum-mdown-line"
  (interactive "e")
  (mouse-select-window event)
  (goto-line (spacemacs//line-at-click))
  (set-mark (point))
  (setq spacemacs-linum-mdown-line
        (line-number-at-pos)))

(defun spacemacs/mu-select-linum ()
  "Select code block between point and spacemacs-linum-mdown-line"
  (interactive)
  (when spacemacs-linum-mdown-line
    (let (mu-line)
      (setq mu-line (spacemacs//line-at-click))
      (goto-line (max spacemacs-linum-mdown-line mu-line))
      (set-mark (line-end-position))
      (goto-line (min spacemacs-linum-mdown-line mu-line))
      (setq spacemacs-linum-mdown-line nil))))

(defun spacemacs/select-current-block ()
  "Select the current block of text between blank lines."
  (interactive)
  (let (p1)
    (when (re-search-backward "\n[ \t]*\n" nil "move")
      (re-search-forward "\n[ \t]*\n"))
    (setq p1 (point))
    (if (re-search-forward "\n[ \t]*\n" nil "move")
        (re-search-backward "\n[ \t]*\n"))
    (set-mark p1)))

;; END linum mouse helpers

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
    (when (and (not exists)
               (not (eq major-mode dotspacemacs-scratch-mode))
               (fboundp dotspacemacs-scratch-mode))
      (funcall dotspacemacs-scratch-mode))))

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

(defun spacemacs/close-compilation-window ()
  "Close the window containing the '*compilation*' buffer."
  (interactive)
  (when compilation-last-buffer
    (delete-windows-on compilation-last-buffer)))


;; Line number

(defun spacemacs/no-linum (&rest ignore)
  "Disable linum if current buffer."
  (when (or 'linum-mode global-linum-mode)
    (linum-mode 0)))

(defun spacemacs/enable-line-numbers-p ()
  "Return non-nil if line numbers should be enabled for current buffer.
Decision is based on `dotspacemacs-line-numbers'."
  (and dotspacemacs-line-numbers
       (spacemacs//linum-curent-buffer-is-not-too-big)
       (or (spacemacs//linum-backward-compabitility)
           (and (listp dotspacemacs-line-numbers)
                (spacemacs//linum-enabled-for-current-major-mode)))))

(defun spacemacs/relative-line-numbers-p ()
  "Return non-nil if line numbers should be relative.
Decision is based on `dotspacemacs-line-numbers'."
  (or (eq dotspacemacs-line-numbers 'relative)
      (and (listp dotspacemacs-line-numbers)
           (car (spacemacs/mplist-get-values dotspacemacs-line-numbers
                                             :relative)))))

(defun spacemacs/visual-line-numbers-p ()
  "Return non-nil if line numbers should be visual.
This is similar to relative line numbers, but wrapped lines are
treated as multiple lines.

Decision is based on `dotspacemacs-line-numbers'."
  (or (eq dotspacemacs-line-numbers 'visual)
      (and (listp dotspacemacs-line-numbers)
           (car (spacemacs/mplist-get-values dotspacemacs-line-numbers :visual)))))

(defun spacemacs//linum-on (origfunc &rest args)
  "Advice function to improve `linum-on' function."
  (when (spacemacs/enable-line-numbers-p)
    (apply origfunc args)))

(defun spacemacs//linum-update-window-scale-fix (win)
  "Fix linum for scaled text in the window WIN."
  (when (display-multi-font-p)
    (unless (boundp 'text-scale-mode-step)
      (setq window-initial-margins (window-margins win)))
    (set-window-margins win
     (ceiling (* (if (boundp 'text-scale-mode-step)
                   (expt text-scale-mode-step
                     text-scale-mode-amount)
                   1)
                (or (car (if (boundp 'window-initial-margins)
                           window-initial-margins
                           (window-margins win)))
                  1))))))

(defun spacemacs//linum-backward-compabitility ()
  "Return non-nil if `dotspacemacs-line-numbers' has an old format and if
`linum' should be enabled."
  (and dotspacemacs-line-numbers
       (not (listp dotspacemacs-line-numbers))
       (or (eq dotspacemacs-line-numbers t)
           (eq dotspacemacs-line-numbers 'relative)
           (eq dotspacemacs-line-numbers 'visual))
       (derived-mode-p 'prog-mode 'text-mode)))

(defun spacemacs//linum-curent-buffer-is-not-too-big ()
  "Return non-nil if buffer size is not too big."
  (not (and (listp dotspacemacs-line-numbers)
            (spacemacs/mplist-get-values dotspacemacs-line-numbers
                                         :size-limit-kb)
            (> (buffer-size)
               (* 1000
                  (car (spacemacs/mplist-get-values dotspacemacs-line-numbers
                                                    :size-limit-kb)))))))

;; see tests in tests/layers/+distribution/spacemacs-base/line-numbers-utest.el
;; for the different possible cases
(defun spacemacs//linum-enabled-for-current-major-mode ()
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
              (spacemacs/derived-mode-p enabled-for-parent disabled-for-parent)))
     ;; current mode (or parent) not explicitly disabled
     (and (null user-enabled-for-modes)
          enabled-for-parent            ; mode is one of default allowed modes
          disabled-for-modes
          (not disabled-for-parent)))))
