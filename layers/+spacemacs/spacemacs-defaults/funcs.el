;;; funcs.el --- Space-macs Defaults Layer functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(require 'cl-lib)

(defun space-macs//run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (format "%S-local-vars-hook" major-mode))))

(defun space-macs/split-and-new-line ()
  "Split a quoted string or s-expression and insert a new line with
auto-indent."
  (interactive)
  (sp-split-sexp 1)
  (sp-newline))

(defun space-macs/push-mark-and-goto-beginning-of-line ()
  "Push a mark at current location and go to the beginning of the line."
  (interactive)
  (push-mark (point))
  (evil-beginning-of-line))

(defun space-macs/push-mark-and-goto-end-of-line ()
  "Push a mark at current location and go to the end of the line."
  (interactive)
  (push-mark (point))
  (evil-end-of-line))

(defun space-macs/evil-insert-line-above (count)
  "Insert one or several lines above the current point's line without changing
the current state and point position."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun space-macs/evil-insert-line-below (count)
  "Insert one or several lines below the current point's line without changing
the current state and point position."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

(defun space-macs/evil-goto-next-line-and-indent (&optional count)
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
(defcustom space-macs-indent-sensitive-modes
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
  :group 'space-macs)

(defcustom space-macs-yank-indent-modes '(latex-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here."
  :type 'list
  :group 'space-macs)

(defcustom space-macs-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'space-macs)

(defcustom space-macs-large-file-modes-list
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode
                 doc-view-mode doc-view-mode-maybe ebrowse-tree-mode
                 pdf-view-mode tags-table-mode fundamental-mode)
  "Major modes which `space-macs/check-large-file' will not be
automatically applied to."
  :group 'space-macs
  :type '(list symbol))

(defun space-macs/custom-newline (pos)
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

;; ido-mode remaps some commands to ido counterparts.  We want default e-macs key
;; bindings (those under C-x) to use ido, but we want to use the original
;; commands in Space-macs key bindings (those under M-m or SPC) so that they use
;; `read-file-name-function', `completing-read-function',
;; `completion-in-region-function', etc. configured by Helm or Ivy etc.  The
;; following aliases allow us to bind Space-macs keys to the original commands.
(defalias 'space-macs/find-file-other-frame 'find-file-other-frame)
(defalias 'space-macs/dired 'dired)
(defalias 'space-macs/dired-other-frame 'dired-other-frame)
(defalias 'space-macs/switch-to-buffer-other-frame 'switch-to-buffer-other-frame)
(defalias 'space-macs/insert-file 'insert-file)
(defalias 'space-macs/display-buffer-other-frame 'display-buffer-other-frame)
(defalias 'space-macs/find-file-and-replace-buffer 'find-alternate-file)

(defun space-macs/indent-region-or-buffer ()
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

;; http://e-macsblog.org/2007/01/17/indent-whole-buffer/
(defun space-macs/iwb-region-or-buffer ()
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
(defun space-macs/toggle-maximize-buffer ()
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
(defun space-macs/maximize-horizontally ()
  "Delete all windows to the left and right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-left) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-right) (error nil))
      (delete-window))))

(defun space-macs/maximize-vertically ()
  "Delete all windows above and below the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-up) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-down) (error nil))
      (delete-window))))

(defun space-macs/useful-buffer-p (buffer)
  "Determines if a buffer is useful."
  (let ((buf-name (buffer-name buffer)))
    (or (provided-mode-derived-p (buffer-local-value 'major-mode buffer) 'comint-mode)
        (cl-loop for useful-regexp in space-macs-useful-buffers-regexp
                 thereis (string-match-p useful-regexp buf-name))
        (cl-loop for useless-regexp in space-macs-useless-buffers-regexp
                 never (string-match-p useless-regexp buf-name)))))

(defun space-macs/useless-buffer-p (buffer)
  "Determines if a buffer is useless."
  (not (space-macs/useful-buffer-p buffer)))


(defun space-macs/swap-windows (window1 window2)
  "Swap two windows.
WINDOW1 and WINDOW2 must be valid windows. They may contain child windows."
  (let ((state1 (window-state-get window1))
        (state2 (window-state-get window2)))
    ;; to put state into dedicated windows, we must undedicate them first (not
    ;; needed with e-macs 25.1)
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
(defun space-macs/window-layout-toggle ()
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
(defun space-macs/rotate-windows-forward (count)
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

(defun space-macs/rotate-windows-backward (count)
  "Rotate each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (space-macs/rotate-windows-forward (* -1 count)))

(defun space-macs/move-buffer-to-window (windownum follow-focus-p)
  "Moves a buffer to a window, using the space-macs numbering. follow-focus-p
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

(defun space-macs/swap-buffers-to-window (windownum follow-focus-p)
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
    (when follow-focus-p (winum-select-window-by-number windownum))))

(dotimes (i 9)
  (let ((n (+ i 1)))
    (eval `(defun ,(intern (format "buffer-to-window-%s" n)) (&optional arg)
             ,(format "Move buffer to the window with number %i." n)
             (interactive "P")
             (if arg
                 (space-macs/swap-buffers-to-window ,n t)
               (space-macs/move-buffer-to-window ,n t))))
    (eval `(defun ,(intern (format "move-buffer-window-no-follow-%s" n)) ()
             (interactive)
             (space-macs/move-buffer-to-window ,n nil)))
    (eval `(defun ,(intern (format "swap-buffer-window-no-follow-%s" n)) ()
             (interactive)
             (space-macs/swap-buffers-to-window ,n nil)))
    ))

(defun space-macs/rename-file (filename &optional new-filename)
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
(defun space-macs/rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting.
If the buffer isn't visiting a file, ask if it should
be saved to a file, or just renamed.

If called without a prefix argument, the prompt is
initialized with the current directory instead of filename."
  (interactive "P")
  (let* ((old-short-name (buffer-name))
         (old-filename (buffer-file-name)))
    (if (and old-filename (file-exists-p old-filename))
        ;; the buffer is visiting a file
        (let* ((old-dir (file-name-directory old-filename))
               (new-name (read-file-name "New name: " (if arg old-dir old-filename)))
               (new-dir (file-name-directory new-name))
               (new-short-name (file-name-nondirectory new-name))
               (file-moved-p (not (string-equal new-dir old-dir)))
               (file-renamed-p (not (string-equal new-short-name old-short-name))))
          (cond ((get-buffer new-name)
                 (error "A buffer named '%s' already exists!" new-name))
                ((string-equal new-name old-filename)
                 (space-macs/show-hide-helm-or-ivy-prompt-msg
                  "Rename failed! Same new and old name" 1.5)
                 (space-macs/rename-current-buffer-file))
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
                   (call-interactively #'projectile-invalidate-cache))
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
                                         "To:   " new-short-name)))))))
      ;; the buffer is not visiting a file
      (let ((key))
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
                ((memq key '(?\a ?\e)) (keyboard-quit))))))))

(defun space-macs/show-hide-helm-or-ivy-prompt-msg (msg sec)
  "Show a MSG at the helm or ivy prompt for SEC.
With Helm, remember the path, then restore it after SEC.
With Ivy, the path isn't editable, just remove the MSG after SEC."
  (run-at-time
   0 nil
   #'(lambda (msg sec)
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

(defun space-macs/delete-file (filename &optional ask-user)
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

(defun space-macs/delete-file-confirm (filename)
  "Remove specified file or directory after users approval.

FILENAME is deleted using `space-macs/delete-file' function.."
  (interactive "f")
  (funcall-interactively #'space-macs/delete-file filename t))

;; from magnars
(defun space-macs/delete-current-buffer-file ()
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
(defun space-macs/sudo-edit (&optional arg)
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
(defun space-macs/check-large-file ()
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and
           (not (memq major-mode space-macs-large-file-modes-list))
           size (> size (* 1024 1024 dotspace-macs-large-file-size))
           (y-or-n-p (format (concat "%s is a large file, open literally to "
                                     "avoid performance issues?")
                             filename)))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))

(defun space-macs/delete-window (&optional arg)
  "Delete the current window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))

;; our own implementation of kill-this-buffer from menu-bar.el
(defun space-macs/kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

;; found at http://e-macswiki.org/e-macs/KillingBuffers
(defun space-macs/kill-other-buffers (&optional arg)
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

;; from http://dfan.org/blog/2009/02/19/e-macs-dedicated-windows/
(defun space-macs/toggle-current-window-dedication ()
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

(defun space-macs//init-visual-line-keys ()
  (evil-define-minor-mode-key 'motion 'visual-line-mode "j" 'evil-next-visual-line)
  (evil-define-minor-mode-key 'motion 'visual-line-mode "k" 'evil-previous-visual-line)
  (evil-define-minor-mode-key 'motion 'visual-line-mode (kbd "<down>") 'evil-next-visual-line)
  (evil-define-minor-mode-key 'motion 'visual-line-mode (kbd "<up>") 'evil-previous-visual-line))


;; Copy file path

(defun space-macs--directory-path ()
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

(defun space-macs--file-path ()
  "Retrieve the file path of the current buffer.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (buffer-file-name))
    (file-truename file-path)))

(defun space-macs--file-path-with-line ()
  "Retrieve the file path of the current buffer, including line number.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (space-macs--file-path))
    (concat file-path ":" (number-to-string (line-number-at-pos)))))

(defun space-macs--file-path-with-line-column ()
  "Retrieve the file path of the current buffer,
including line and column number.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (space-macs--file-path-with-line))
    (concat
     file-path
     ":"
     (number-to-string (if (and
                            ;; e-macs 26 introduced this variable. Remove this
                            ;; check once 26 becomes the minimum version.
                            (boundp column-number-indicator-zero-based)
                            (not column-number-indicator-zero-based))
                           (1+ (current-column))
                         (current-column))))))

(defun space-macs/copy-directory-path ()
  "Copy and show the directory path of the current buffer.

If the buffer is not visiting a file, use the `list-buffers-directory'
variable as a fallback to display the directory, useful in buffers like the
ones created by `magit' and `dired'."
  (interactive)
  (if-let (directory-path (space-macs--directory-path))
      (progn
        (kill-new directory-path)
        (message "%s" directory-path))
    (message "WARNING: Current buffer does not have a directory!")))

(defun space-macs/copy-file-path ()
  "Copy and show the file path of the current buffer."
  (interactive)
  (if-let (file-path (space-macs--file-path))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun space-macs/copy-file-name ()
  "Copy and show the file name of the current buffer."
  (interactive)
  (if-let (file-name (file-name-nondirectory (space-macs--file-path)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun space-macs/copy-buffer-name ()
  "Copy and show the name of the current buffer."
  (interactive)
  (kill-new (buffer-name))
  (message "%s" (buffer-name)))

(defun space-macs/copy-file-name-base ()
  "Copy and show the file name without its final extension of the current
buffer."
  (interactive)
  (if-let (file-name (file-name-base (space-macs--file-path)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun space-macs/copy-file-path-with-line ()
  "Copy and show the file path of the current buffer, including line number."
  (interactive)
  (if-let (file-path (space-macs--file-path-with-line))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun space-macs/copy-file-path-with-line-column ()
  "Copy and show the file path of the current buffer,
including line and column number.

This function respects the value of the `column-number-indicator-zero-based'
variable."
  (interactive)
  (if-let (file-path (space-macs--file-path-with-line-column))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))



;; adapted from bozhidar
;; http://e-macsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun space-macs/find-user-init-file ()
  "Edit the `user-init-file', in the current window."
  (interactive)
  (find-file-existing user-init-file))

(defun space-macs/find-dotfile ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (find-file-existing (dotspace-macs/location)))

(defun space-macs/ediff-dotfile-and-template ()
  "ediff the current `dotfile' with the template"
  (interactive)
  (ediff-files (dotspace-macs/location)
               (concat dotspace-macs-template-directory ".space-macs.template")))

(defun space-macs/new-empty-buffer (&optional split)
  "Create a new buffer called: untitled<n>

The SPLIT argument decides where the buffer opens:
Value                                Buffer
`nil'                                current window
`left', `below', `above' or `right'  split window
`frame'                              new frame

If the variable `dotspace-macs-new-empty-buffer-major-mode' has been set,
then apply that major mode to the new buffer."
  (interactive)
  (let ((newbuf (generate-new-buffer "untitled")))
    (cl-case split
      ('left  (split-window-horizontally))
      ('below (space-macs/split-window-vertically-and-switch))
      ('above (split-window-vertically))
      ('right (space-macs/split-window-horizontally-and-switch))
      ('frame (select-frame (make-frame))))
    ;; Prompt to save on `save-some-buffers' with positive PRED
    (with-current-buffer newbuf
      (setq-local buffer-offer-save t)
      (when dotspace-macs-new-empty-buffer-major-mode
        (funcall dotspace-macs-new-empty-buffer-major-mode)))
    ;; pass non-nil force-same-window to prevent `switch-to-buffer' from
    ;; displaying buffer in another window
    (switch-to-buffer newbuf nil 'force-same-window)))

(defun space-macs/new-empty-buffer-left ()
  "Create a new buffer called untitled(<n>),
in a split window to the left."
  (interactive)
  (space-macs/new-empty-buffer 'left))

(defun space-macs/new-empty-buffer-below ()
  "Create a new buffer called untitled(<n>),
in a split window below."
  (interactive)
  (space-macs/new-empty-buffer 'below))

(defun space-macs/new-empty-buffer-above ()
  "Create a new buffer called untitled(<n>),
in a split window above."
  (interactive)
  (space-macs/new-empty-buffer 'above))

(defun space-macs/new-empty-buffer-right ()
  "Create a new buffer called untitled(<n>),
in a split window to the right."
  (interactive)
  (space-macs/new-empty-buffer 'right))

(defun space-macs/new-empty-buffer-new-frame ()
  "Create a new buffer called untitled(<n>),
in a new frame."
  (interactive)
  (space-macs/new-empty-buffer 'frame))

;; from https://gist.github.com/timcharper/493269
(defun space-macs/split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun space-macs/split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))


;; Window Split

(defun space-macs--window-split-splittable-windows ()
  (seq-remove
   (lambda (window)
     ;; TODO: find a way to identify unsplittable side windows reliably!
     nil)
   (space-macs--window-split-non-ignored-windows)))

(defun space-macs--window-split-non-ignored-windows ()
  "Determines the list of windows to be deleted."
  (seq-filter
   (lambda (window)
     (let* ((name (buffer-name (window-buffer window)))
            (prefixes-matching
             (seq-filter
              (lambda (prefix) (string-prefix-p prefix name))
              space-macs-window-split-ignore-prefixes)))
       (not prefixes-matching)))
   (window-list (selected-frame))))

(defun space-macs/window-split-default-delete ()
  "Deletes other windows, except a list of excluded ones."
  (if space-macs-window-split-ignore-prefixes
      (let* ((deletable (space-macs--window-split-non-ignored-windows))
             (splittable (space-macs--window-split-splittable-windows)))
        (when splittable
          (let* ((selected (car splittable))
                 (to-delete (delq selected deletable)))
            (select-window selected)
            (dolist (window to-delete) (delete-window window)))))
    (delete-other-windows)))

(defvar space-macs-window-split-ignore-prefixes nil
  "Prefixes for windows that are not deleted when changing split layout.

You can add an entry here by using the following:
(add-to-list 'space-macs-window-split-ignore-prefixes \"Buffer prefix\")")

(defvar space-macs-window-split-delete-function
  'space-macs/window-split-default-delete
  "Function used to delete other windows when changing layout.

Used as a callback by the following functions:
  - space-macs/window-split-grid
  - space-macs/window-split-triple-columns
  - space-macs/window-split-double-columns
  - space-macs/window-split-single-column

Possible values:
  - 'space-macs/window-split-default-delete (default)
  - 'delete-other-windows
  - 'tree-macs-delete-other-windows (when using the tree-macs package)
  - a lambda: (lambda () (delete-other-windows))
  - a custom function:
    (defun my-delete-other-windows () (delete-other-windows))
    (setq space-macs-window-split-delete-function 'my-delete-other-windows)")

(defun space-macs/window-split-grid (&optional purge)
  "Set the layout to a 2x2 grid.

Uses the funcion defined in `space-macs-window-split-delete-function'
as a means to remove windows.

When called with a prefix argument, it uses `delete-other-windows'
as a means to remove windows, regardless of the value in
`space-macs-window-split-delete-function'."
  (interactive "P")
  (if purge
      (let ((ignore-window-parameters t))
        (delete-other-windows))
    (funcall space-macs-window-split-delete-function))
  (if (space-macs--window-split-splittable-windows)
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

(defun space-macs/window-split-triple-columns (&optional purge)
  "Set the layout to triple columns.

Uses the funcion defined in `space-macs-window-split-delete-function'
as a means to remove windows.

When called with a prefix argument, it uses `delete-other-windows'
as a means to remove windows, regardless of the value in
`space-macs-window-split-delete-function'."
  (interactive "P")
  (if purge
      (let ((ignore-window-parameters t))
        (delete-other-windows))
    (funcall space-macs-window-split-delete-function))
  (if (space-macs--window-split-splittable-windows)
      (let* ((previous-files (seq-filter #'buffer-file-name
                                         (delq (current-buffer) (buffer-list))))
             (second (split-window-right))
             (third (split-window second nil 'right)))
        (set-window-buffer second (or (car previous-files) "*scratch*"))
        (set-window-buffer third (or (cadr previous-files) "*scratch*"))
        (balance-windows))
    (message "There are no main windows available to split!")))

(defun space-macs/window-split-double-columns (&optional purge)
  "Set the layout to double columns.

Uses the funcion defined in `space-macs-window-split-delete-function'
as a means to remove windows.

When called with a prefix argument, it uses `delete-other-windows'
as a means to remove windows, regardless of the value in
`space-macs-window-split-delete-function'."
  (interactive "P")
  (if purge
      (let ((ignore-window-parameters t))
        (delete-other-windows))
    (funcall space-macs-window-split-delete-function))
  (if (space-macs--window-split-splittable-windows)
      (let* ((previous-files (seq-filter #'buffer-file-name
                                         (delq (current-buffer) (buffer-list)))))
        (set-window-buffer (split-window-right)
                           (or (car previous-files) "*scratch*"))
        (balance-windows))
    (message "There are no main windows available to split!")))

(defun space-macs/window-split-single-column (&optional purge)
  "Set the layout to single column.

Uses the funcion defined in `space-macs-window-split-delete-function'
as a means to remove windows.

When called with a prefix argument, it uses `delete-other-windows'
as a means to remove windows, regardless of the value in
`space-macs-window-split-delete-function'."
  (interactive "P")
  (if purge
      (let ((ignore-window-parameters t))
        (delete-other-windows))
    (funcall space-macs-window-split-delete-function))
  (balance-windows))



(defun space-macs/insert-line-above-no-indent (count)
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

(defun space-macs/insert-line-below-no-indent (count)
  "Insert a new line below with no indentation."
  (interactive "p")
  (save-excursion
    (evil-move-end-of-line)
    (while (> count 0)
      (insert "\n")
      (setq count (1- count)))))

;; see https://github.com/gempesaw/dote-macs/blob/e-macs/dg-elisp/dg-defun.el
(defun space-macs/rudekill-matching-buffers (regexp &optional internal-too)
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

(defun space-macs/kill-matching-buffers-rudely (regexp &optional internal-too)
  "Kill - WITHOUT ASKING - buffers whose name matches the specified REGEXP. See
the `kill-matching-buffers` for grateful killing. The optional 2nd argument
indicates whether to kill internal buffers too.

Returns a message with the count of killed buffers."
  (interactive "sKill buffers matching this regular expression: \nP")
  (message
   (format "%d buffer(s) killed."
           (space-macs/rudekill-matching-buffers regexp internal-too))))

;; advise to prevent server from closing

(defvar space-macs-really-kill-e-macs nil
  "prevent window manager close from closing instance.")

(defun space-macs//persistent-server-running-p ()
  "Requires space-macs-really-kill-e-macs to be toggled and
dotspace-macs-persistent-server to be t"
  (and (fboundp 'server-running-p)
       (server-running-p)
       dotspace-macs-persistent-server))

(defadvice kill-e-macs (around space-macs-really-exit activate)
  "Only kill e-macs if a prefix is set"
  (if (and (not space-macs-really-kill-e-macs)
           (space-macs//persistent-server-running-p))
      (space-macs/frame-killer)
    ad-do-it))

(defadvice save-buffers-kill-e-macs (around space-macs-really-exit activate)
  "Only kill e-macs if a prefix is set"
  (if (and (not space-macs-really-kill-e-macs)
           (space-macs//persistent-server-running-p))
      (space-macs/frame-killer)
    ad-do-it))

(defun space-macs/save-buffers-kill-e-macs ()
  "Save all changed buffers and exit Space-macs"
  (interactive)
  (setq space-macs-really-kill-e-macs t)
  (save-buffers-kill-e-macs))

(defun space-macs/kill-e-macs ()
  "Lose all changes and exit Space-macs"
  (interactive)
  (setq space-macs-really-kill-e-macs t)
  (kill-e-macs))

(defun space-macs/prompt-kill-e-macs ()
  "Prompt to save changed buffers and exit Space-macs"
  (interactive)
  (setq space-macs-really-kill-e-macs t)
  (save-some-buffers nil t)
  (kill-e-macs))

(defun space-macs/frame-killer ()
  "Kill server buffer and hide the main e-macs window"
  (interactive)
  (condition-case nil
      (delete-frame nil 1)
    (error
     (make-frame-invisible nil 1))))

(defun space-macs/toggle-frame-fullscreen ()
  "Respect the `dotspace-macs-fullscreen-use-non-native' variable when
toggling fullscreen."
  (interactive)
  (if dotspace-macs-fullscreen-use-non-native
      (space-macs/toggle-frame-fullscreen-non-native)
    (toggle-frame-fullscreen)))

(defun space-macs/toggle-fullscreen ()
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

(defun space-macs/toggle-frame-fullscreen-non-native ()
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

(defun space-macs/safe-revert-buffer ()
  "Prompt before reverting the file."
  (interactive)
  (revert-buffer nil nil))

(defun space-macs/safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
      (erase-buffer)))

(defun space-macs//find-ert-test-buffer (ert-test)
  "Return the buffer where ERT-TEST is defined."
  (save-excursion
    (car (find-definition-noselect (ert-test-name ert-test) 'ert-deftest))))

(defun space-macs/ert-run-tests-buffer ()
  "Run all the tests in the current buffer."
  (interactive)
  (save-buffer)
  (load-file (buffer-file-name))
  (let ((cbuf (current-buffer)))
    (ert '(satisfies (lambda (test)
                       (eq cbuf (space-macs//find-ert-test-buffer test)))))))

(defun space-macs//open-in-external-app (file-path)
  "Open `file-path' in external application."
  (cond
   ((space-macs/system-is-mswindows)
    (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
   ((space-macs/system-is-mac) (shell-command (format "open \"%s\"" file-path)))
   ((space-macs/system-is-linux) (let ((process-connection-type nil))
                                  (start-process "" nil "xdg-open" file-path)))))

(defun space-macs/open-file-or-directory-in-external-app (arg)
  "Open current file in external application.
If the universal prefix argument is used then open the folder
containing the current file by the default explorer."
  (interactive "P")
  (if arg
      (space-macs//open-in-external-app (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (space-macs//open-in-external-app file-path)
        (message "No file associated to this buffer.")))))

(defun space-macs/switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

;; http://stackoverflow.com/a/10216338/4869
(defun space-macs/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun space-macs/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;; BEGIN align functions

;; modified function from http://e-macswiki.org/e-macs/AlignCommands
(defun space-macs/align-repeat (start end regexp &optional justify-right after)
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

;; Modified answer from http://e-macs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun space-macs/align-repeat-decimal (start end)
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

(defmacro space-macs|create-align-repeat-x (name regexp &optional justify-right default-after)
  (let* ((new-func (intern (concat "space-macs/align-repeat-" name)))
         (new-func-defn
          `(defun ,new-func (start end switch)
             (interactive "r\nP")
             (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
               (space-macs/align-repeat start end ,regexp ,justify-right after)))))
    (put new-func 'function-documentation "Created by `space-macs|create-align-repeat-x'.")
    new-func-defn))

(space-macs|create-align-repeat-x "comma" "," nil t)
(space-macs|create-align-repeat-x "semicolon" ";" nil t)
(space-macs|create-align-repeat-x "colon" ":" nil t)
(space-macs|create-align-repeat-x "equal" "=")
(space-macs|create-align-repeat-x "math-oper" "[+\\-*/]")
(space-macs|create-align-repeat-x "percent" "%")
(space-macs|create-align-repeat-x "ampersand" "&")
(space-macs|create-align-repeat-x "bar" "|")
(space-macs|create-align-repeat-x "left-paren" "(")
(space-macs|create-align-repeat-x "right-paren" ")" t)
(space-macs|create-align-repeat-x "left-curly-brace" "{")
(space-macs|create-align-repeat-x "right-curly-brace" "}" t)
(space-macs|create-align-repeat-x "left-square-brace" "\\[")
(space-macs|create-align-repeat-x "right-square-brace" "\\]" t)
(space-macs|create-align-repeat-x "backslash" "\\\\")

;; END align functions

(defun space-macs/dos2unix ()
  "Converts the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun space-macs/unix2dos ()
  "Converts the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun space-macs/copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

;; from https://www.e-macswiki.org/e-macs/CopyingWholeLines
(defun space-macs/duplicate-line-or-region (&optional n)
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
;; https://github.com/purcell/e-macs.d/blob/master/lisp/init-editing-utils.el
;; https://e-macsredux.com/blog/2013/04/08/kill-line-backward/
(defun space-macs/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(defun space-macs/uniquify-lines ()
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

(defun space-macs/sort-lines (&optional reverse)
  "Sort lines in a region or the current buffer.
A non-nil argument sorts in reverse order."
  (interactive "P")
  (let* ((region-active (or (region-active-p) (evil-visual-state-p)))
         (beg (if region-active (region-beginning) (point-min)))
         (end (if region-active (region-end) (point-max))))
    (sort-lines reverse beg end)))

(defun space-macs/sort-lines-reverse ()
  "Sort lines in reverse order, in a region or the current buffer."
  (interactive)
  (space-macs/sort-lines -1))

(defun space-macs/sort-lines-by-column (&optional reverse)
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

(defun space-macs/sort-lines-by-column-reverse ()
  "Sort lines by the selected column in reverse order,
using a visual block/rectangle selection."
  (interactive)
  (space-macs/sort-lines-by-column -1))

;; BEGIN linum mouse helpers

(defvar space-macs-linum-mdown-line nil
  "Define persistent variable for linum selection")

(defun space-macs//line-at-click ()
  "Determine the visual line at click"
  (save-excursion
    (let ((click-y (cddr (mouse-position)))
          (debug-on-error t)
          (line-move-visual t))
      (goto-char (window-start))
      (next-line (1- click-y))
      (1+ (line-number-at-pos))
      )))

(defun space-macs/md-select-linum (event)
  "Set point as space-macs-linum-mdown-line"
  (interactive "e")
  (mouse-select-window event)
  (goto-line (space-macs//line-at-click))
  (set-mark (point))
  (setq space-macs-linum-mdown-line
        (line-number-at-pos)))

(defun space-macs/mu-select-linum ()
  "Select code block between point and space-macs-linum-mdown-line"
  (interactive)
  (when space-macs-linum-mdown-line
    (let (mu-line)
      (setq mu-line (space-macs//line-at-click))
      (goto-line (max space-macs-linum-mdown-line mu-line))
      (set-mark (line-end-position))
      (goto-line (min space-macs-linum-mdown-line mu-line))
      (setq space-macs-linum-mdown-line nil))))

(defun space-macs/select-current-block ()
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

;; from http://www.e-macswiki.org/e-macs/WordCount
(defun space-macs/count-words-analysis (start end)
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
(defun space-macs/yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) space-macs-yank-indent-threshold)
      (indent-region beg end nil)))

(defun space-macs//yank-indent-region (yank-func &rest args)
  "If current mode is not one of space-macs-indent-sensitive-modes
indent yanked text (with universal arg don't indent)."
  (evil-start-undo-step)
  (prog1
      (let ((prefix (car args))
            (enable (and (not (member major-mode space-macs-indent-sensitive-modes))
                         (or (derived-mode-p 'prog-mode)
                             (member major-mode space-macs-yank-indent-modes)))))
        (when (and enable (equal '(4) prefix))
          (setq args (cdr args)))
        (prog1
            (apply yank-func args)
          (when (and enable (not (equal '(4) prefix)))
            (let ((transient-mark-mode nil)
                  (save-undo buffer-undo-list))
              (space-macs/yank-advised-indent-function (region-beginning)
                                                      (region-end))))))
    (evil-end-undo-step)))

(dolist (func '(yank yank-pop evil-paste-before evil-paste-after))
  (advice-add func :around #'space-macs//yank-indent-region))

;; find file functions in split
(defun space-macs//display-in-split (buffer alist)
  "Split selected window and display BUFFER in the new window.
BUFFER and ALIST have the same form as in `display-buffer'. If ALIST contains
a split-side entry, its value must be usable as the SIDE argument for
`split-window'."
  (let ((window (split-window nil nil (cdr (assq 'split-side alist)))))
    (window--display-buffer buffer window 'window alist)
    window))

(defun space-macs/find-file-vsplit (file)
  "find file in vertical split"
  (interactive "FFind file (vsplit): ")
  (let ((buffer (find-file-noselect file)))
    (pop-to-buffer buffer '(space-macs//display-in-split (split-side . right)))))

(defun space-macs/find-file-split (file)
  "find file in horizontal split"
  (interactive "FFind file (split): ")
  (let ((buffer (find-file-noselect file)))
    (pop-to-buffer buffer '(space-macs//display-in-split (split-side . below)))))

(defun space-macs/switch-to-help-buffer ()
  "Open or select the `*Help*' buffer, if it exists."
  (interactive)
  (if (get-buffer "*Help*")
      (switch-to-buffer (help-buffer))
    (message "No previous Help buffer found")))

(defun space-macs/switch-to-scratch-buffer (&optional arg)
  "Switch to the `*scratch*' buffer, creating it first if needed.
if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (let ((exists (get-buffer "*scratch*")))
    (if arg
        (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
      (switch-to-buffer (get-buffer-create "*scratch*")))
    (when (and (not exists)
               (not (eq major-mode dotspace-macs-scratch-mode))
               (fboundp dotspace-macs-scratch-mode))
      (funcall dotspace-macs-scratch-mode)
      (run-hooks 'space-macs-scratch-mode-hook))))

(defvar space-macs--killed-buffer-list nil
  "List of recently killed buffers.")

(defun space-macs//add-buffer-to-killed-list ()
  "If buffer is associated with a file name, add that file
to the `killed-buffer-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name space-macs--killed-buffer-list)))

(defun space-macs/reopen-killed-buffer ()
  "Reopen the most recently killed file buffer, if one exists."
  (interactive)
  (when space-macs--killed-buffer-list
    (find-file (pop space-macs--killed-buffer-list))))

(defun space-macs/switch-to-messages-buffer (&optional arg)
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

(defun space-macs/show-hide-compilation-window ()
  "Show/Hide the window containing the compilation buffer."
  (interactive)
  (when-let ((buffer compilation-last-buffer))
    (if (get-buffer-window buffer 'visible)
        (delete-windows-on buffer)
      (space-macs/switch-to-compilation-buffer))))

(defun space-macs/switch-to-compilation-buffer ()
  "Go to last compilation buffer."
  (interactive)
  (if compilation-last-buffer
      (pop-to-buffer compilation-last-buffer)
    (user-error "There is no compilation buffer?")))


;; Line number

(defun space-macs/no-linum (&rest ignore)
  "Disable linum if current buffer."
  (when (or 'linum-mode global-linum-mode)
    (linum-mode 0)))

(defun space-macs/enable-line-numbers-p ()
  "Return non-nil if line numbers should be enabled for current buffer.
Decision is based on `dotspace-macs-line-numbers'."
  (and dotspace-macs-line-numbers
       (space-macs//linum-curent-buffer-is-not-too-big)
       (or (space-macs//linum-backward-compabitility)
           (and (listp dotspace-macs-line-numbers)
                (space-macs//linum-enabled-for-current-major-mode)))))

(defun space-macs/relative-line-numbers-p ()
  "Return non-nil if line numbers should be relative.
Decision is based on `dotspace-macs-line-numbers'."
  (or (eq dotspace-macs-line-numbers 'relative)
      (and (listp dotspace-macs-line-numbers)
           (car (space-macs/mplist-get-values dotspace-macs-line-numbers
                                             :relative)))))

(defun space-macs/visual-line-numbers-p ()
  "Return non-nil if line numbers should be visual.
This is similar to relative line numbers, but wrapped lines are
treated as multiple lines.

Decision is based on `dotspace-macs-line-numbers'."
  (or (eq dotspace-macs-line-numbers 'visual)
      (and (listp dotspace-macs-line-numbers)
           (car (space-macs/mplist-get-values dotspace-macs-line-numbers :visual)))))

(defun space-macs//linum-on (origfunc &rest args)
  "Advice function to improve `linum-on' function."
  (when (space-macs/enable-line-numbers-p)
    (apply origfunc args)))

(defun space-macs//linum-update-window-scale-fix (win)
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

(defun space-macs//linum-backward-compabitility ()
  "Return non-nil if `dotspace-macs-line-numbers' has an old format and if
`linum' should be enabled."
  (and dotspace-macs-line-numbers
       (not (listp dotspace-macs-line-numbers))
       (or (eq dotspace-macs-line-numbers t)
           (eq dotspace-macs-line-numbers 'relative)
           (eq dotspace-macs-line-numbers 'visual))
       (derived-mode-p 'prog-mode 'text-mode)))

(defun space-macs//linum-curent-buffer-is-not-too-big ()
  "Return non-nil if buffer size is not too big."
  (not (and (listp dotspace-macs-line-numbers)
            (space-macs/mplist-get-values dotspace-macs-line-numbers
                                         :size-limit-kb)
            (> (buffer-size)
               (* 1000
                  (car (space-macs/mplist-get-values dotspace-macs-line-numbers
                                                    :size-limit-kb)))))))

;; see tests in tests/layers/+distribution/space-macs-base/line-numbers-utest.el
;; for the different possible cases
(defun space-macs//linum-enabled-for-current-major-mode ()
  "Return non-nil if line number is enabled for current major-mode."
  (let* ((disabled-for-modes
          (space-macs/mplist-get-values dotspace-macs-line-numbers
                                       :disabled-for-modes))
         (user-enabled-for-modes
          (space-macs/mplist-get-values dotspace-macs-line-numbers
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
              (space-macs/derived-mode-p enabled-for-parent disabled-for-parent)))
     ;; current mode (or parent) not explicitly disabled
     (and (null user-enabled-for-modes)
          enabled-for-parent            ; mode is one of default allowed modes
          disabled-for-modes
          (not disabled-for-parent)))))


;; randomize region

(defun space-macs/randomize-words (beg end)
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

(defun space-macs/randomize-lines (beg end)
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

(defun space-macs/clone-indirect-buffer-de-activate-mark ()
  "This is a workaround for the evil visual state error message like:
Error in post-command-hook (evil-visual-post-command):
(error \"Marker points into wrong buffer\" #<marker at 27875 in .space-macs<2>>)"
  (let ((region-was-active (region-active-p)))
    (when region-was-active (deactivate-mark))
    (call-interactively 'clone-indirect-buffer)
    (when region-was-active (activate-mark))))

(defun space-macs/narrow-to-indirect-buffer (narrower target-name)
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
      (space-macs/evil-search-clear-highlight))
    (space-macs/clone-indirect-buffer-de-activate-mark)
    (call-interactively narrower)
    (message (format "%s narrowed to an indirect buffer" target-name))))

(defun space-macs/narrow-to-defun-indirect-buffer ()
  (interactive)
  (space-macs/narrow-to-indirect-buffer 'narrow-to-defun "Function"))

(defun space-macs/narrow-to-page-indirect-buffer ()
  (interactive)
  (space-macs/narrow-to-indirect-buffer 'narrow-to-page "Page"))

(defun space-macs/narrow-to-region-indirect-buffer ()
  (interactive)
  (space-macs/narrow-to-indirect-buffer 'narrow-to-region "Region"))


