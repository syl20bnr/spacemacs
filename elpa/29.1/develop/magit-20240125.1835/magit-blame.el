;;; magit-blame.el --- Blame support for Magit  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2024 The Magit Project Contributors

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Annotates each line in file-visiting buffer with information from
;; the revision which last modified the line.

;;; Code:

(require 'magit)

;;; Options

(defgroup magit-blame nil
  "Blame support for Magit."
  :link '(info-link "(magit)Blaming")
  :group 'magit-modes)

(defcustom magit-blame-styles
  '((headings
     (heading-format   . "%-20a %C %s\n"))
    (highlight
     (highlight-face   . magit-blame-highlight))
    (lines
     (show-lines       . t)
     (show-message     . t)))
  "List of styles used to visualize blame information.

The style used in the current buffer can be cycled from the blame
popup.  Blame commands (except `magit-blame-echo') use the first
style as the initial style when beginning to blame in a buffer.

Each entry has the form (IDENT (KEY . VALUE)...).  IDENT has
to be a symbol uniquely identifying the style.  The following
KEYs are recognized:

 `show-lines'
    Whether to prefix each chunk of lines with a thin line.
    This has no effect if `heading-format' is non-nil.
 `show-message'
    Whether to display a commit's summary line in the echo area
    when crossing chunks.
 `highlight-face'
    Face used to highlight the first line of each chunk.
    If this is nil, then those lines are not highlighted.
 `heading-format'
    String specifying the information to be shown above each
    chunk of lines.  It must end with a newline character.
 `margin-format'
    String specifying the information to be shown in the left
    buffer margin.  It must NOT end with a newline character.
    This can also be a list of formats used for the lines at
    the same positions within the chunk.  If the chunk has
    more lines than formats are specified, then the last is
    repeated.  WARNING: Adding this key affects performance;
    see the note at the end of this docstring.
 `margin-width'
    Width of the margin, provided `margin-format' is non-nil.
 `margin-face'
    Face used in the margin, provided `margin-format' is
    non-nil.  This face is used in combination with the faces
    that are specific to the used %-specs.  If this is nil,
    then `magit-blame-margin' is used.
 `margin-body-face'
    Face used in the margin for all but first line of a chunk.
    This face is used in combination with the faces that are
    specific to the used %-specs.  This can also be a list of
    faces (usually one face), in which case only these faces
    are used and the %-spec faces are ignored.  A good value
    might be `(magit-blame-dimmed)'.  If this is nil, then
    the same face as for the first line is used.

The following %-specs can be used in `heading-format' and
`margin-format':

  %H    hash              using face `magit-blame-hash'
  %s    summary           using face `magit-blame-summary'
  %a    author            using face `magit-blame-name'
  %A    author time       using face `magit-blame-date'
  %c    committer         using face `magit-blame-name'
  %C    committer time    using face `magit-blame-date'

Additionally if `margin-format' ends with %f, then the string
that is displayed in the margin is made at least `margin-width'
characters wide, which may be desirable if the used face sets
the background color.

Blame information is displayed using overlays.  Such extensive
use of overlays is known to slow down even basic operations, such
as moving the cursor. To reduce the number of overlays the margin
style had to be removed from the default value of this option.

Note that the margin overlays are created even if another style
is currently active.  This can only be prevented by not even
defining a style that uses the margin.  If you want to use this
style anyway, you can restore this definition, which used to be
part of the default value:

  (margin
   (margin-format    . (\" %s%f\" \" %C %a\" \" %H\"))
   (margin-width     . 42)
   (margin-face      . magit-blame-margin)
   (margin-body-face . (magit-blame-dimmed)))"
  :package-version '(magit . "2.13.0")
  :group 'magit-blame
  :type 'string)

(defcustom magit-blame-echo-style 'lines
  "The blame visualization style used by `magit-blame-echo'.
A symbol that has to be used as the identifier for one of the
styles defined in `magit-blame-styles'."
  :package-version '(magit . "2.13.0")
  :group 'magit-blame
  :type 'symbol)

(defcustom magit-blame-time-format "%F %H:%M"
  "Format for time strings in blame headings."
  :group 'magit-blame
  :type 'string)

(defcustom magit-blame-read-only t
  "Whether to initially make the blamed buffer read-only."
  :package-version '(magit . "2.13.0")
  :group 'magit-blame
  :type 'boolean)

(defcustom magit-blame-disable-modes '(fci-mode yascroll-bar-mode)
  "List of modes not compatible with Magit-Blame mode.
This modes are turned off when Magit-Blame mode is turned on,
and then turned on again when turning off the latter."
  :group 'magit-blame
  :type '(repeat (symbol :tag "Mode")))

(defcustom magit-blame-mode-lighter " Blame"
  "The mode-line lighter of the Magit-Blame mode."
  :group 'magit-blame
  :type '(choice (const :tag "No lighter" "") string))

(defcustom magit-blame-goto-chunk-hook
  '(magit-blame-maybe-update-revision-buffer
    magit-blame-maybe-show-message)
  "Hook run after point entered another chunk."
  :package-version '(magit . "2.13.0")
  :group 'magit-blame
  :type 'hook
  :get #'magit-hook-custom-get
  :options '(magit-blame-maybe-update-revision-buffer
             magit-blame-maybe-show-message))

;;; Faces

(defface magit-blame-highlight
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey80"
     :foreground "black")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey25"
     :foreground "white"))
  "Face used for highlighting when blaming.
Also see option `magit-blame-styles'."
  :group 'magit-faces)

(defface magit-blame-margin
  '((t :inherit magit-blame-highlight
       :weight normal
       :slant normal))
  "Face used for the blame margin by default when blaming.
Also see option `magit-blame-styles'."
  :group 'magit-faces)

(defface magit-blame-dimmed
  '((t :inherit magit-dimmed
       :weight normal
       :slant normal))
  "Face used for the blame margin in some cases when blaming.
Also see option `magit-blame-styles'."
  :group 'magit-faces)

(defface magit-blame-heading
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :inherit magit-blame-highlight
       :weight normal
       :slant normal))
  "Face used for blame headings by default when blaming.
Also see option `magit-blame-styles'."
  :group 'magit-faces)

(defface magit-blame-summary '((t nil))
  "Face used for commit summaries when blaming."
  :group 'magit-faces)

(defface magit-blame-hash '((t nil))
  "Face used for commit hashes when blaming."
  :group 'magit-faces)

(defface magit-blame-name '((t nil))
  "Face used for author and committer names when blaming."
  :group 'magit-faces)

(defface magit-blame-date '((t nil))
  "Face used for dates when blaming."
  :group 'magit-faces)

;;; Variables

(defvar-local magit-blame-buffer-read-only nil)
(defvar-local magit-blame-cache nil)
(defvar-local magit-blame-disabled-modes nil)
(defvar-local magit-blame-process nil)
(defvar-local magit-blame-recursive-p nil)
(defvar-local magit-blame-type nil)
(defvar-local magit-blame-separator nil)
(defvar-local magit-blame-previous-chunk nil)

(defvar-local magit-blame--make-margin-overlays nil)
(defvar-local magit-blame--style nil)

;;; Chunks

(defclass magit-blame-chunk ()
  (;; <orig-rev> <orig-line> <final-line> <num-lines>
   (orig-rev   :initarg :orig-rev)
   (orig-line  :initarg :orig-line)
   (final-line :initarg :final-line)
   (num-lines  :initarg :num-lines)
   ;; previous <prev-rev> <prev-file>
   (prev-rev   :initform nil)
   (prev-file  :initform nil)
   ;; filename <orig-file>
   (orig-file)))

(defun magit-current-blame-chunk (&optional type noerror)
  (or (and (not (and type (not (eq type magit-blame-type))))
           (magit-blame-chunk-at (point)))
      (and type
           (let ((rev  (or magit-buffer-refname magit-buffer-revision))
                 (file (and (not (derived-mode-p 'dired-mode))
                            (magit-file-relative-name
                             nil (not magit-buffer-file-name))))
                 (line (format "%d,+1" (line-number-at-pos))))
             (cond (file (with-temp-buffer
                           (magit-with-toplevel
                             (magit-git-insert
                              "blame" "--porcelain"
                              (if (memq magit-blame-type '(final removal))
                                  (cons "--reverse" (magit-blame-arguments))
                                (magit-blame-arguments))
                              "-L" line rev "--" file)
                             (goto-char (point-min))
                             (if (eobp)
                                 (unless noerror
                                   (error "Cannot get blame chunk at eob"))
                               (car (magit-blame--parse-chunk type))))))
                   (noerror nil)
                   (t (error "Buffer does not visit a tracked file")))))))

(defun magit-blame-chunk-at (pos)
  (--some (overlay-get it 'magit-blame-chunk)
          (overlays-at pos)))

(defun magit-blame--overlay-at (&optional pos key)
  (unless pos
    (setq pos (point)))
  (--first (overlay-get it (or key 'magit-blame-chunk))
           (nconc (overlays-at pos)
                  (overlays-in pos pos))))

;;; Keymaps

(defvar-keymap magit-blame-mode-map
  :doc "Keymap for `magit-blame-mode'.
Note that most blaming key bindings are defined
in `magit-blame-read-only-mode-map' instead."
  "C-c C-q" #'magit-blame-quit)

(defvar-keymap magit-blame-read-only-mode-map
  :doc "Keymap for `magit-blame-read-only-mode'."
  "C-m" #'magit-show-commit
  "p"   #'magit-blame-previous-chunk
  "P"   #'magit-blame-previous-chunk-same-commit
  "n"   #'magit-blame-next-chunk
  "N"   #'magit-blame-next-chunk-same-commit
  "b"   #'magit-blame-addition
  "r"   #'magit-blame-removal
  "f"   #'magit-blame-reverse
  "B"   #'magit-blame
  "c"   #'magit-blame-cycle-style
  "q"   #'magit-blame-quit
  "M-w" #'magit-blame-copy-hash
  "SPC"   #'magit-diff-show-or-scroll-up
  "S-SPC" #'magit-diff-show-or-scroll-down
  "DEL"   #'magit-diff-show-or-scroll-down)

;;; Modes
;;;; Base Mode

(define-minor-mode magit-blame-mode
  "Display blame information inline."
  :lighter magit-blame-mode-lighter
  :interactive nil
  (cond (magit-blame-mode
         (unless arg
           ;; Emacs < 28.1 doesn't support `:interactive'.
           (setq magit-blame-mode nil)
           (user-error
            (concat "Don't call `magit-blame-mode' directly; "
                    "instead use `magit-blame'")))
         (add-hook 'after-save-hook     #'magit-blame--refresh t t)
         (add-hook 'post-command-hook   #'magit-blame-goto-chunk-hook t t)
         (add-hook 'before-revert-hook  #'magit-blame--remove-overlays t t)
         (add-hook 'after-revert-hook   #'magit-blame--refresh t t)
         (add-hook 'read-only-mode-hook #'magit-blame-toggle-read-only t t)
         (setq magit-blame-buffer-read-only buffer-read-only)
         (when (or magit-blame-read-only magit-buffer-file-name)
           (read-only-mode 1))
         (dolist (mode magit-blame-disable-modes)
           (when (and (boundp mode) (symbol-value mode))
             (funcall mode -1)
             (push mode magit-blame-disabled-modes)))
         (setq magit-blame-separator (magit-blame--format-separator))
         (unless magit-blame--style
           (setq magit-blame--style (car magit-blame-styles)))
         (setq magit-blame--make-margin-overlays
               (and (cl-find-if (lambda (style)
                                  (assq 'margin-format (cdr style)))
                                magit-blame-styles)))
         (magit-blame--update-margin))
        (t
         (when (process-live-p magit-blame-process)
           (kill-process magit-blame-process)
           (while magit-blame-process
             (sit-for 0.01))) ; avoid racing the sentinel
         (remove-hook 'after-save-hook     #'magit-blame--refresh t)
         (remove-hook 'post-command-hook   #'magit-blame-goto-chunk-hook t)
         (remove-hook 'before-revert-hook  #'magit-blame--remove-overlays t)
         (remove-hook 'after-revert-hook   #'magit-blame--refresh t)
         (remove-hook 'read-only-mode-hook #'magit-blame-toggle-read-only t)
         (unless magit-blame-buffer-read-only
           (read-only-mode -1))
         (magit-blame-read-only-mode -1)
         (dolist (mode magit-blame-disabled-modes)
           (funcall mode 1))
         (kill-local-variable 'magit-blame-disabled-modes)
         (kill-local-variable 'magit-blame-type)
         (kill-local-variable 'magit-blame--style)
         (magit-blame--update-margin)
         (magit-blame--remove-overlays))))

(defun magit-blame--refresh ()
  (magit-blame--run (magit-blame-arguments)))

(defun magit-blame-goto-chunk-hook ()
  (let ((chunk (magit-blame-chunk-at (point))))
    (when (cl-typep chunk 'magit-blame-chunk)
      (unless (eq chunk magit-blame-previous-chunk)
        (run-hooks 'magit-blame-goto-chunk-hook))
      (setq magit-blame-previous-chunk chunk))))

(defun magit-blame-toggle-read-only ()
  (magit-blame-read-only-mode (if buffer-read-only 1 -1)))

;;;; Read-Only Mode

(define-minor-mode magit-blame-read-only-mode
  "Provide keybindings for Magit-Blame mode.

This minor-mode provides the key bindings for Magit-Blame mode,
but only when Read-Only mode is also enabled because these key
bindings would otherwise conflict badly with regular bindings.

When both Magit-Blame mode and Read-Only mode are enabled, then
this mode gets automatically enabled too and when one of these
modes is toggled, then this mode also gets toggled automatically.

\\{magit-blame-read-only-mode-map}")

;;;; Kludges

(defun magit-blame-put-keymap-before-view-mode ()
  "Put `magit-blame-read-only-mode' ahead of `view-mode' in `minor-mode-map-alist'."
  (when-let ((entry (assq 'magit-blame-read-only-mode
                          (cl-member 'view-mode minor-mode-map-alist
                                     :key #'car))))
    (setq minor-mode-map-alist
          (cons entry
                (delq entry minor-mode-map-alist))))
  (remove-hook 'view-mode-hook #'magit-blame-put-keymap-before-view-mode))

(add-hook 'view-mode-hook #'magit-blame-put-keymap-before-view-mode)

;;; Process

(defun magit-blame--run (args)
  (magit-with-toplevel
    (unless magit-blame-mode
      (magit-blame-mode 1))
    (message "Blaming...")
    (magit-blame-run-process
     (or magit-buffer-refname magit-buffer-revision)
     (magit-file-relative-name nil (not magit-buffer-file-name))
     (if (memq magit-blame-type '(final removal))
         (cons "--reverse" args)
       args)
     (list (line-number-at-pos (window-start))
           (line-number-at-pos (1- (window-end nil t)))))
    (set-process-sentinel magit-this-process
                          #'magit-blame-process-quickstart-sentinel)))

(defun magit-blame-run-process (revision file args &optional lines)
  (let ((process (magit-parse-git-async
                  "blame" "--incremental" args
                  (and lines (list "-L" (apply #'format "%s,%s" lines)))
                  revision "--" file)))
    (set-process-filter   process #'magit-blame-process-filter)
    (set-process-sentinel process #'magit-blame-process-sentinel)
    (process-put process 'arguments (list revision file args))
    (setq magit-blame-cache (make-hash-table :test #'equal))
    (setq magit-blame-process process)))

(defun magit-blame-process-quickstart-sentinel (process event)
  (when (memq (process-status process) '(exit signal))
    (magit-blame-process-sentinel process event t)
    (magit-blame-assert-buffer process)
    (with-current-buffer (process-get process 'command-buf)
      (when magit-blame-mode
        (let ((default-directory (magit-toplevel)))
          (apply #'magit-blame-run-process
                 (process-get process 'arguments)))))))

(defun magit-blame-process-sentinel (process _event &optional quiet)
  (let ((status (process-status process)))
    (when (memq status '(exit signal))
      (kill-buffer (process-buffer process))
      (if (and (eq status 'exit)
               (zerop (process-exit-status process)))
          (unless quiet
            (message "Blaming...done"))
        (magit-blame-assert-buffer process)
        (with-current-buffer (process-get process 'command-buf)
          (if magit-blame-mode
              (progn (magit-blame-mode -1)
                     (message "Blaming...failed"))
            (message "Blaming...aborted"))))
      (kill-local-variable 'magit-blame-process))))

(defun magit-blame-process-filter (process string)
  (internal-default-process-filter process string)
  (let ((buf  (process-get process 'command-buf))
        (pos  (process-get process 'parsed))
        (mark (process-mark process))
        type cache)
    (with-current-buffer buf
      (setq type  magit-blame-type)
      (setq cache magit-blame-cache))
    (with-current-buffer (process-buffer process)
      (goto-char pos)
      (while (and (< (point) mark)
                  (save-excursion (re-search-forward "^filename .+\n" nil t)))
        (pcase-let* ((`(,chunk ,revinfo)
                      (magit-blame--parse-chunk type))
                     (rev (oref chunk orig-rev)))
          (if revinfo
              (puthash rev revinfo cache)
            (setq revinfo
                  (or (gethash rev cache)
                      (puthash rev (magit-blame--commit-alist rev) cache))))
          (magit-blame--make-overlays buf chunk revinfo))
        (process-put process 'parsed (point))))))

(defun magit-blame--parse-chunk (type)
  (let (chunk revinfo)
    (unless (looking-at "^\\(.\\{40,\\}\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)")
      (error "Blaming failed due to unexpected output: %s"
             (buffer-substring-no-properties (point) (line-end-position))))
    (with-slots (orig-rev orig-file prev-rev prev-file)
        (setq chunk (magit-blame-chunk
                     :orig-rev                     (match-string 1)
                     :orig-line  (string-to-number (match-string 2))
                     :final-line (string-to-number (match-string 3))
                     :num-lines  (string-to-number (match-string 4))))
      (forward-line)
      (let (done)
        (while (not done)
          (cond ((looking-at "^filename \\(.+\\)")
                 (setq done t)
                 (setf orig-file (magit-decode-git-path (match-string 1))))
                ((looking-at "^previous \\(.\\{40,\\}\\) \\(.+\\)")
                 (setf prev-rev  (match-string 1))
                 (setf prev-file (magit-decode-git-path (match-string 2))))
                ((looking-at "^\\([^ ]+\\) \\(.+\\)")
                 (push (cons (match-string 1)
                             (match-string 2))
                       revinfo)))
          (forward-line)))
      (when (and (eq type 'removal) prev-rev)
        (cl-rotatef orig-rev  prev-rev)
        (cl-rotatef orig-file prev-file)
        (setq revinfo nil)))
    (list chunk revinfo)))

(defun magit-blame--commit-alist (rev)
  (cl-mapcar 'cons
             '("summary"
               "author" "author-time" "author-tz"
               "committer" "committer-time" "committer-tz")
             (split-string (magit-rev-format "%s\v%an\v%ad\v%cn\v%cd" rev
                                             "--date=format:%s\v%z")
                           "\v")))

(defun magit-blame-assert-buffer (process)
  (unless (buffer-live-p (process-get process 'command-buf))
    (kill-process process)
    (user-error "Buffer being blamed has been killed")))

;;; Display

(defsubst magit-blame--style-get (key)
  (cdr (assoc key (cdr magit-blame--style))))

(defun magit-blame--make-overlays (buf chunk revinfo)
  (with-current-buffer buf
    (save-excursion
      (save-restriction
        (widen)
        (let* ((line (oref chunk final-line))
               (beg (magit-blame--line-beginning-position line))
               (end (magit-blame--line-beginning-position
                     (+ line (oref chunk num-lines))))
               (before (magit-blame-chunk-at (1- beg))))
          (when (and before
                     (equal (oref before orig-rev)
                            (oref chunk orig-rev)))
            (setq beg (magit-blame--line-beginning-position
                       (oset chunk final-line (oref before final-line))))
            (cl-incf (oref chunk num-lines)
                     (oref before num-lines)))
          (magit-blame--remove-overlays beg end)
          (when magit-blame--make-margin-overlays
            (magit-blame--make-margin-overlays chunk revinfo beg end))
          (magit-blame--make-heading-overlay chunk revinfo beg end)
          (magit-blame--make-highlight-overlay chunk beg))))))

(defun magit-blame--line-beginning-position (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defun magit-blame--make-margin-overlays (chunk revinfo beg end)
  (save-excursion
    (let ((line 0))
      (goto-char beg)
      (while (< (point) end)
        (magit-blame--make-margin-overlay chunk revinfo line)
        (forward-line)
        (cl-incf line)))))

(defun magit-blame--make-margin-overlay (chunk revinfo line)
  (let* ((end (line-end-position))
         ;; If possible avoid putting this on the first character
         ;; of the line to avoid a conflict with the line overlay.
         (beg (min (1+ (line-beginning-position)) end))
         (ov  (make-overlay beg end)))
    (overlay-put ov 'magit-blame-chunk chunk)
    (overlay-put ov 'magit-blame-revinfo revinfo)
    (overlay-put ov 'magit-blame-margin line)
    (magit-blame--update-margin-overlay ov)))

(defun magit-blame--make-heading-overlay (chunk revinfo beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'magit-blame-chunk chunk)
    (overlay-put ov 'magit-blame-revinfo revinfo)
    (overlay-put ov 'magit-blame-heading t)
    (magit-blame--update-heading-overlay ov)))

(defun magit-blame--make-highlight-overlay (chunk beg)
  (let ((ov (make-overlay beg (save-excursion
                                (goto-char beg)
                                (1+ (line-end-position))))))
    (overlay-put ov 'magit-blame-chunk chunk)
    (overlay-put ov 'magit-blame-highlight t)
    (magit-blame--update-highlight-overlay ov)))

(defun magit-blame--update-margin ()
  (setq left-margin-width (or (magit-blame--style-get 'margin-width) 0))
  (set-window-buffer (selected-window) (current-buffer)))

(defun magit-blame--update-overlays ()
  (save-restriction
    (widen)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (cond ((overlay-get ov 'magit-blame-heading)
             (magit-blame--update-heading-overlay ov))
            ((overlay-get ov 'magit-blame-margin)
             (magit-blame--update-margin-overlay ov))
            ((overlay-get ov 'magit-blame-highlight)
             (magit-blame--update-highlight-overlay ov))))))

(defun magit-blame--update-margin-overlay (ov)
  (overlay-put
   ov 'before-string
   (and (magit-blame--style-get 'margin-width)
        (propertize
         "o" 'display
         (list (list 'margin 'left-margin)
               (let ((line   (overlay-get ov 'magit-blame-margin))
                     (format (magit-blame--style-get 'margin-format))
                     (face   (magit-blame--style-get 'margin-face)))
                 (magit-blame--format-string
                  ov
                  (or (and (atom format)
                           format)
                      (nth line format)
                      (car (last format)))
                  (or (and (not (zerop line))
                           (magit-blame--style-get 'margin-body-face))
                      face
                      'magit-blame-margin))))))))

(defun magit-blame--update-heading-overlay (ov)
  (overlay-put
   ov 'before-string
   (if-let ((format (magit-blame--style-get 'heading-format)))
       (magit-blame--format-string ov format 'magit-blame-heading)
     (and (magit-blame--style-get 'show-lines)
          (or (not (magit-blame--style-get 'margin-format))
              (save-excursion
                (goto-char (overlay-start ov))
                ;; Special case of the special case described in
                ;; `magit-blame--make-margin-overlay'.  For empty
                ;; lines it is not possible to show both overlays
                ;; without the line being to high.
                (not (= (point) (line-end-position)))))
          magit-blame-separator))))

(defun magit-blame--update-highlight-overlay (ov)
  (overlay-put ov 'font-lock-face (magit-blame--style-get 'highlight-face)))

(defun magit-blame--format-string (ov format face)
  (let* ((chunk   (overlay-get ov 'magit-blame-chunk))
         (revinfo (overlay-get ov 'magit-blame-revinfo))
         (key     (list format face))
         (string  (cdr (assoc key revinfo))))
    (unless string
      (setq string
            (and format
                 (magit-blame--format-string-1 (oref chunk orig-rev)
                                               revinfo format face)))
      (nconc revinfo (list (cons key string))))
    string))

(defun magit-blame--format-string-1 (rev revinfo format face)
  (let ((str
         (if (string-match-p "\\`0\\{40,\\}\\'" rev)
             (propertize (concat (if (string-prefix-p "\s" format) "\s" "")
                                 "Not Yet Committed"
                                 (if (string-suffix-p "\n" format) "\n" ""))
                         'font-lock-face face)
           (magit--format-spec
            (propertize format 'font-lock-face face)
            (cl-flet* ((p0 (s f)
                         (propertize s 'font-lock-face
                                     (if face
                                         (if (listp face)
                                             face
                                           (list f face))
                                       f)))
                       (p1 (k f)
                         (p0 (cdr (assoc k revinfo)) f))
                       (p2 (k1 k2 f)
                         (p0 (magit-blame--format-time-string
                              (cdr (assoc k1 revinfo))
                              (cdr (assoc k2 revinfo)))
                             f)))
              `((?H . ,(p0 rev         'magit-blame-hash))
                (?s . ,(p1 "summary"   'magit-blame-summary))
                (?a . ,(p1 "author"    'magit-blame-name))
                (?c . ,(p1 "committer" 'magit-blame-name))
                (?A . ,(p2 "author-time"    "author-tz"    'magit-blame-date))
                (?C . ,(p2 "committer-time" "committer-tz" 'magit-blame-date))
                (?f . "")))))))
    (if-let ((width (and (string-suffix-p "%f" format)
                         (magit-blame--style-get 'margin-width))))
        (concat str
                (propertize (make-string (max 0 (- width (length str))) ?\s)
                            'font-lock-face face))
      str)))

(defun magit-blame--format-separator ()
  (propertize
   (concat (propertize "\s" 'display '(space :height (2)))
           (propertize "\n" 'line-height t))
   'font-lock-face `(:background
                     ,(face-attribute 'magit-blame-heading
                                      :background nil t)
                     ,@(and (>= emacs-major-version 27) '(:extend t)))))

(defun magit-blame--format-time-string (time tz)
  (let* ((time-format (or (magit-blame--style-get 'time-format)
                          magit-blame-time-format))
         (tz-in-second (and (string-search "%z" time-format)
                            (car (last (parse-time-string tz))))))
    (format-time-string time-format
                        (seconds-to-time (string-to-number time))
                        tz-in-second)))

(defun magit-blame--remove-overlays (&optional beg end)
  (save-restriction
    (widen)
    (dolist (ov (overlays-in (or beg (point-min))
                             (or end (point-max))))
      (when (overlay-get ov 'magit-blame-chunk)
        (delete-overlay ov)))))

(defun magit-blame-maybe-show-message ()
  (when (magit-blame--style-get 'show-message)
    (let ((message-log-max 0))
      (if-let ((msg (cdr (assoc "summary"
                                (gethash (oref (magit-current-blame-chunk)
                                               orig-rev)
                                         magit-blame-cache)))))
          (progn (set-text-properties 0 (length msg) nil msg)
                 (message msg))
        (message "Commit data not available yet.  Still blaming.")))))

;;; Commands

;;;###autoload (autoload 'magit-blame-echo "magit-blame" nil t)
(transient-define-suffix magit-blame-echo (args)
  "For each line show the revision in which it was added.
Show the information about the chunk at point in the echo area
when moving between chunks.  Unlike other blaming commands, do
not turn on `read-only-mode'."
  :if (lambda ()
        (and buffer-file-name
             (or (not magit-blame-mode)
                 buffer-read-only)))
  (interactive (list (magit-blame-arguments)))
  (when magit-buffer-file-name
    (user-error "Blob buffers aren't supported"))
  (setq-local magit-blame--style
              (assq magit-blame-echo-style magit-blame-styles))
  (setq-local magit-blame-disable-modes
              (cons 'eldoc-mode magit-blame-disable-modes))
  (if (not magit-blame-mode)
      (let ((magit-blame-read-only nil))
        (magit-blame--pre-blame-assert 'addition)
        (magit-blame--pre-blame-setup  'addition)
        (magit-blame--run args))
    (read-only-mode -1)
    (magit-blame--update-overlays)))

;;;###autoload (autoload 'magit-blame-addition "magit-blame" nil t)
(transient-define-suffix magit-blame-addition (args)
  "For each line show the revision in which it was added."
  (interactive (list (magit-blame-arguments)))
  (magit-blame--pre-blame-assert 'addition)
  (magit-blame--pre-blame-setup  'addition)
  (magit-blame--run args))

;;;###autoload (autoload 'magit-blame-removal "magit-blame" nil t)
(transient-define-suffix magit-blame-removal (args)
  "For each line show the revision in which it was removed."
  :if-nil 'buffer-file-name
  (interactive (list (magit-blame-arguments)))
  (unless magit-buffer-file-name
    (user-error "Only blob buffers can be blamed in reverse"))
  (magit-blame--pre-blame-assert 'removal)
  (magit-blame--pre-blame-setup  'removal)
  (magit-blame--run args))

;;;###autoload (autoload 'magit-blame-reverse "magit-blame" nil t)
(transient-define-suffix magit-blame-reverse (args)
  "For each line show the last revision in which it still exists."
  :if-nil 'buffer-file-name
  (interactive (list (magit-blame-arguments)))
  (unless magit-buffer-file-name
    (user-error "Only blob buffers can be blamed in reverse"))
  (magit-blame--pre-blame-assert 'final)
  (magit-blame--pre-blame-setup  'final)
  (magit-blame--run args))

(defun magit-blame--pre-blame-assert (type)
  (unless (magit-toplevel)
    (magit--not-inside-repository-error))
  (if (and magit-blame-mode
           (eq type magit-blame-type))
      (if-let ((chunk (magit-current-blame-chunk)))
          (unless (oref chunk prev-rev)
            (user-error "Chunk has no further history"))
        (user-error "Commit data not available yet.  Still blaming."))
    (unless (magit-file-relative-name nil (not magit-buffer-file-name))
      (if buffer-file-name
          (user-error "Buffer isn't visiting a tracked file")
        (user-error "Buffer isn't visiting a file")))))

(defun magit-blame--pre-blame-setup (type)
  (when magit-blame-mode
    (if (eq type magit-blame-type)
        (let ((style magit-blame--style))
          (magit-blame-visit-other-file)
          (setq-local magit-blame--style style)
          (setq-local magit-blame-recursive-p t)
          ;; Set window-start for the benefit of quickstart.
          (redisplay))
      (magit-blame--remove-overlays)))
  (setq magit-blame-type type))

(defun magit-blame-visit-other-file ()
  "Visit another blob related to the current chunk."
  (interactive)
  (with-slots (prev-rev prev-file orig-line)
      (magit-current-blame-chunk)
    (unless prev-rev
      (user-error "Chunk has no further history"))
    (magit-with-toplevel
      (magit-find-file prev-rev prev-file))
    ;; TODO Adjust line like magit-diff-visit-file.
    (goto-char (point-min))
    (forward-line (1- orig-line))))

(defun magit-blame-visit-file ()
  "Visit the blob related to the current chunk."
  (interactive)
  (with-slots (orig-rev orig-file orig-line)
      (magit-current-blame-chunk)
    (magit-with-toplevel
      (magit-find-file orig-rev orig-file))
    (goto-char (point-min))
    (forward-line (1- orig-line))))

(transient-define-suffix magit-blame-quit ()
  "Turn off Magit-Blame mode.
If the buffer was created during a recursive blame,
then also kill the buffer."
  :if-non-nil 'magit-blame-mode
  (interactive)
  (magit-blame-mode -1)
  (when magit-blame-recursive-p
    (kill-buffer)))

(defun magit-blame-next-chunk ()
  "Move to the next chunk."
  (interactive)
  (if-let ((next (next-single-char-property-change
                  (point) 'magit-blame-chunk)))
      (goto-char next)
    (user-error "No more chunks")))

(defun magit-blame-previous-chunk ()
  "Move to the previous chunk."
  (interactive)
  (if-let ((prev (previous-single-char-property-change
                  (point) 'magit-blame-chunk)))
      (goto-char prev)
    (user-error "No more chunks")))

(defun magit-blame-next-chunk-same-commit (&optional previous)
  "Move to the next chunk from the same commit.\n\n(fn)"
  (interactive)
  (if-let ((rev (oref (magit-current-blame-chunk) orig-rev)))
      (let ((pos (point)) ov)
        (save-excursion
          (while (and (not ov)
                      (not (= pos (if previous (point-min) (point-max))))
                      (setq pos (funcall
                                 (if previous
                                     #'previous-single-char-property-change
                                   #'next-single-char-property-change)
                                 pos 'magit-blame-chunk)))
            (when-let ((o (magit-blame--overlay-at pos)))
              (when (equal (oref (magit-blame-chunk-at pos) orig-rev) rev)
                (setq ov o)))))
        (if ov
            (goto-char (overlay-start ov))
          (user-error "No more chunks from same commit")))
    (user-error "This chunk hasn't been blamed yet")))

(defun magit-blame-previous-chunk-same-commit ()
  "Move to the previous chunk from the same commit."
  (interactive)
  (magit-blame-next-chunk-same-commit #'previous-single-char-property-change))

(defun magit-blame-cycle-style ()
  "Change how blame information is visualized.
Cycle through the elements of option `magit-blame-styles'."
  (interactive)
  (setq magit-blame--style
        (or (cadr (cl-member (car magit-blame--style)
                             magit-blame-styles :key #'car))
            (car magit-blame-styles)))
  (magit-blame--update-margin)
  (magit-blame--update-overlays))

(defun magit-blame-copy-hash ()
  "Save hash of the current chunk's commit to the kill ring.

When the region is active, then save the region's content
instead of the hash, like `kill-ring-save' would."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (kill-new (message "%s" (oref (magit-current-blame-chunk) orig-rev)))))

;;; Popup

;;;###autoload (autoload 'magit-blame "magit-blame" nil t)
(transient-define-prefix magit-blame ()
  "Show the commits that added or removed lines in the visited file."
  :man-page "git-blame"
  :value '("-w")
  ["Arguments"
   ("-w" "Ignore whitespace" "-w")
   ("-r" "Do not treat root commits as boundaries" "--root")
   ("-P" "Follow only first parent" "--first-parent")
   (magit-blame:-M)
   (magit-blame:-C)]
  ["Actions"
   ("b" "Show commits adding lines" magit-blame-addition)
   ("r" "Show commits removing lines" magit-blame-removal)
   ("f" "Show last commits that still have lines" magit-blame-reverse)
   ("m" "Blame echo" magit-blame-echo)
   ("q" "Quit blaming" magit-blame-quit)]
  ["Refresh"
   :if-non-nil magit-blame-mode
   ("c" "Cycle style" magit-blame-cycle-style :transient t)])

(defun magit-blame-arguments ()
  (transient-args 'magit-blame))

(transient-define-argument magit-blame:-M ()
  :description "Detect lines moved or copied within a file"
  :class 'transient-option
  :argument "-M"
  :allow-empty t
  :reader #'transient-read-number-N+)

(transient-define-argument magit-blame:-C ()
  :description "Detect lines moved or copied between files"
  :class 'transient-option
  :argument "-C"
  :allow-empty t
  :reader #'transient-read-number-N+)

;;; Utilities

(defun magit-blame-maybe-update-revision-buffer ()
  (when-let* ((chunk  (magit-current-blame-chunk))
              (commit (oref chunk orig-rev))
              (buffer (magit-get-mode-buffer 'magit-revision-mode nil t)))
    (if magit--update-revision-buffer
        (setq magit--update-revision-buffer (list commit buffer))
      (setq magit--update-revision-buffer (list commit buffer))
      (run-with-idle-timer
       magit-update-other-window-delay nil
       (lambda ()
         (pcase-let ((`(,rev ,buf) magit--update-revision-buffer))
           (setq magit--update-revision-buffer nil)
           (when (buffer-live-p buf)
             (let ((magit-display-buffer-noselect t))
               (apply #'magit-show-commit rev
                      (magit-diff-arguments 'magit-revision-mode))))))))))

;;; _
(provide 'magit-blame)
;;; magit-blame.el ends here
