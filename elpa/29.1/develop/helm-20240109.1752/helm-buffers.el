;;; helm-buffers.el --- helm support for buffers. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2023 Thierry Volpiatto 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-types)
(require 'helm-utils)
(require 'helm-grep)
(require 'helm-regexp)
(require 'helm-help)
(require 'helm-occur)

(declare-function helm-comp-read "helm-mode")
(declare-function helm-browse-project "helm-files")
(declare-function helm-ff-switch-to-shell "helm-files")
(declare-function all-the-icons-icon-for-file "ext:all-the-icons.el")
(declare-function all-the-icons-octicon "ext:all-the-icons.el")

(defvar all-the-icons-mode-icon-alist)
(defvar dired-buffers)
(defvar org-directory)
(defvar helm-ff-default-directory)


(defgroup helm-buffers nil
  "Buffers related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-boring-buffer-regexp-list
  '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf")
  "The regexp list that match boring buffers.
Buffer candidates matching these regular expression will be
filtered from the list of candidates if the
`helm-skip-boring-buffers' candidate transformer is used."
  :type  '(repeat (choice regexp)))

(defcustom helm-white-buffer-regexp-list nil
  "The regexp list of not boring buffers.
These buffers will be displayed even if they match one of
`helm-boring-buffer-regexp-list'."
  :type '(repeat (choice regexp)))

(defcustom helm-buffers-favorite-modes '(lisp-interaction-mode
                                         emacs-lisp-mode
                                         text-mode
                                         org-mode)
  "List of preferred mode to open new buffers with."
  :type '(repeat (choice function)))

(defcustom helm-buffer-max-length 20
  "Max length of buffer names before truncate.
When disabled (nil) use the longest `buffer-name' length found."
  :type  '(choice (const :tag "Disabled" nil)
           (integer :tag "Length before truncate")))

(defcustom helm-buffer-details-flag t
  "Always show details in buffer list when non-nil."
  :type 'boolean)

(defcustom helm-buffers-fuzzy-matching nil
  "Fuzzy matching buffer names when non-nil.
Only buffer names are fuzzy matched when this is enabled,
`major-mode' matching is not affected by this."
  :type 'boolean)

(defcustom helm-buffer-skip-remote-checking nil
  "Ignore checking for `file-exists-p' on remote files."
  :type 'boolean)

(defcustom helm-buffers-truncate-lines t
  "Truncate lines in `helm-buffers-list' when non-nil."
  :type 'boolean)

(defcustom helm-buffers-left-margin-width helm-left-margin-width
  "`left-margin-width' value for `helm-mini' and `helm-buffers-list'."
  :type 'integer)

(defcustom helm-mini-default-sources '(helm-source-buffers-list
                                       helm-source-recentf
                                       helm-source-buffer-not-found)
  "Default sources list used in `helm-mini'.

When adding a source here it is up to you to ensure the library
of this source is accessible and properly loaded."
  :type '(repeat (choice symbol)))

(defcustom helm-buffers-end-truncated-string "..."
  "The string to display at end of truncated buffer names."
  :type 'string)

(defcustom helm-buffers-column-separator "  "
  "Separator for columns in buffer listing."
  :type 'string)

(defcustom helm-buffer--pretty-names '((dired-mode . "Dired")
                                       (lisp-interaction-mode . "Lisp Inter"))
  "An alist specifying pretty names for modes.
Most of the time buffer's `mode-name' is a string so no need to
add it here as there is no need to compute it, but sometimes it
may be a mode-line specification which may be costly to compute,
in this case add here the pretty name as a string to avoid this
costly computation.  Also if some pretty names are too long you
can add your own abbreviation here."
  :type '(alist :key-type symbol :value-type string))

(defcustom helm-buffers-maybe-switch-to-tab nil
  "Switch to buffer in its tab when non nil.
This has no effect when `tab-bar-mode' is not available."
  :type 'boolean)

(defcustom helm-buffer-list-reorder-fn #'helm-buffers-reorder-buffer-list
  "A function in charge of ordering the initial buffer list.
It takes two arguments VISIBLES buffers and OTHERS buffers.
Arg VISIBLES handles the buffers visibles in this frame.
Arg OTHERS handles all the other buffers.
You can write a function that reorder VISIBLES and OTHERS as you
want.
Default function returns OTHERS buffers on top and VISIBLES
buffer at the end.  See `helm-buffers-reorder-buffer-list'."
  :type 'function)

(defcustom helm-buffers-sort-fn helm-fuzzy-sort-fn
  "The sort function to use in `helm-buffers-list'.

Default to `helm-fuzzy-sort-fn' you can use
`helm-fuzzy-matching-sort-fn-preserve-ties-order' as alternative if
you want to keep the recentest order when narrowing candidates."
  :type 'function)

(defcustom helm-buffers-show-icons nil
  "Prefix buffer names with an icon when non nil.
Don't use `setq' to set this."
  :type 'boolean
  :set (lambda (var val)
         (if (require 'all-the-icons nil t)
             (set var val)
           (set var nil))))


;;; Faces
;;
;;
(defgroup helm-buffers-faces nil
  "Customize the appearance of helm-buffers."
  :prefix "helm-"
  :group 'helm-buffers
  :group 'helm-faces)

(defface helm-buffer-saved-out
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "red" :background "black"))
  "Face used for buffer files modified outside of emacs."
  :group 'helm-buffers-faces)

(defface helm-buffer-not-saved
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "Indianred2"))
  "Face used for buffer files not already saved on disk."
  :group 'helm-buffers-faces)

(defface helm-buffer-modified
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :inherit font-lock-comment-face))
  "Face used for modified buffers."
  :group 'helm-buffers-faces)

(defface helm-no-file-buffer-modified
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "orange" :background "black"))
  "Face used for modified buffers."
  :group 'helm-buffers-faces)

(defface helm-buffer-size
  `((((background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "RosyBrown")
    (((background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "SlateGray"))
  "Face used for buffer size."
  :group 'helm-buffers-faces)

(defface helm-buffer-process
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "Sienna3"))
  "Face used for process status in buffer."
  :group 'helm-buffers-faces)

(defface helm-buffer-directory
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "DarkRed" :background "LightGray"))
  "Face used for directories in `helm-buffers-list'."
  :group 'helm-buffers-faces)

(defface helm-buffer-file
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :inherit font-lock-builtin-face))
  "Face for buffer file names in `helm-buffers-list'."
  :group 'helm-buffers-faces)

(defface helm-buffer-archive
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "Gold"))
  "Face for archive file names in `helm-buffers-list'."
  :group 'helm-buffers-faces)

(defface helm-non-file-buffer
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :inherit italic))
  "Face used for non-file buffers in `helm-buffers-list'."
  :group 'helm-buffers-faces)

(defvar helm-buffers-tick-counter nil
  "Allows recording local changes to a non-file buffer.
Typical usage of this var is for modes that want to see if their
buffers have changed since last visit.
Such programs may want to record tick counter after visiting
their buffers like this:

    (setq helm-buffers-tick-counter (buffer-modified-tick))

See bug#1917.

Note that this variable is buffer-local.")
(make-variable-buffer-local 'helm-buffers-tick-counter)


;;; Buffers keymap
;;
(defvar helm-buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    ;; No need to have separate command for grep and zgrep
    ;; as we don't use recursivity for buffers.
    ;; So use zgrep for both as it is capable to handle non--compressed files.
    (define-key map (kbd "M-g s")     #'helm-buffer-run-zgrep)
    (define-key map (kbd "C-s")       #'helm-buffers-run-occur)
    (define-key map (kbd "C-x C-d")   #'helm-buffers-run-browse-project)
    (define-key map (kbd "C-c o")     #'helm-buffer-switch-other-window)
    (define-key map (kbd "C-c C-o")   #'helm-buffer-switch-other-frame)
    (define-key map (kbd "M-g M-g")   #'helm-buffer-run-goto-line)
    (define-key map (kbd "C-c =")     #'helm-buffer-run-ediff)
    (define-key map (kbd "M-=")       #'helm-buffer-run-ediff-merge)
    (define-key map (kbd "C-=")       #'helm-buffer-diff-persistent)
    (define-key map (kbd "M-G")       #'helm-buffer-revert-persistent)
    (define-key map (kbd "C-c d")     #'helm-buffer-run-kill-persistent)
    (define-key map (kbd "M-D")       #'helm-buffer-run-kill-buffers)
    (define-key map (kbd "C-x C-s")   #'helm-buffer-save-persistent)
    (define-key map (kbd "C-x s")     #'helm-buffer-run-save-some-buffers)
    (define-key map (kbd "C-M-%")     #'helm-buffer-run-query-replace-regexp)
    (define-key map (kbd "M-%")       #'helm-buffer-run-query-replace)
    (define-key map (kbd "M-R")       #'helm-buffer-run-rename-buffer)
    (define-key map (kbd "M-e")       #'helm-buffer-run-switch-to-shell)
    (define-key map (kbd "C-]")       #'helm-toggle-buffers-details)
    (define-key map (kbd "C-c a")     #'helm-buffers-toggle-show-hidden-buffers)
    (define-key map (kbd "C-M-SPC")   #'helm-buffers-mark-similar-buffers)
    (when (fboundp 'tab-bar-mode)
      (define-key map (kbd "C-c C-t") #'helm-buffers-switch-to-buffer-new-tab))
    map)
  "Keymap for buffer sources in helm.")


(defvar helm-buffer-max-len-mode nil)
(defvar helm-buffers-in-project-p nil)
(defvar helm-source-buffers-list nil)

(defun helm-buffers-list--init ()
  (require 'dired)
  ;; Bug#51 Create the list before `helm-buffer' creation.
  ;; We were using a global cache in the past and 'candidates was
  ;; bound to this cache, this was a problem when using more than one
  ;; source with a different 'buffer-list fn as the same cache was
  ;; reused in each source (Bug#1907), now 'candidates attr is set
  ;; directly so that each list of candidates is local to source.
  (helm-set-attr 'candidates (funcall (helm-get-attr 'buffer-list)))
  (let ((result (cl-loop with allbufs = (memq 'helm-shadow-boring-buffers
                                              (helm-get-attr
                                               'filtered-candidate-transformer
                                               helm-source-buffers-list))
                         for b in (if allbufs
                                      (helm-get-attr 'candidates)
                                    (helm-skip-boring-buffers
                                     (helm-get-attr 'candidates)
                                     helm-source-buffers-list))
                         maximize (length b) into len-buf
                         maximize (length (helm-buffer--format-mode-name b))
                         into len-mode
                         finally return (cons len-buf len-mode))))
    (unless (default-value 'helm-buffer-max-length)
      (helm-set-local-variable 'helm-buffer-max-length (car result)))
    (unless (default-value 'helm-buffer-max-len-mode)
      (helm-set-local-variable 'helm-buffer-max-len-mode (cdr result)))))

(defclass helm-source-buffers (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-buffer-list
    :custom function
    :documentation
    "  A function with no arguments to create buffer list.")
   (init :initform 'helm-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform 'helm-buffer-map)
   (find-file-target :initform #'helm-buffers-quit-and-find-file-fn)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(cl-defun helm-buffers-create-new-buffer-1 (candidate &optional (display-func 'switch-to-buffer))
  (let ((mjm (or (and helm-current-prefix-arg
                      (intern-soft (helm-comp-read
                                    "Major-mode: "
                                    helm-buffers-favorite-modes)))
                 (cl-loop for (r . m) in auto-mode-alist
                          when (string-match r candidate)
                          return m)))
        (buffer (get-buffer-create candidate)))
    (if mjm
        (with-current-buffer buffer (funcall mjm))
      (set-buffer-major-mode buffer))
    (funcall display-func buffer)))

(defun helm-buffers-create-new-buffer (candidate)
  (helm-buffers-create-new-buffer-1 candidate))

(defun helm-buffers-create-new-buffer-ow (candidate)
  (helm-buffers-create-new-buffer-1 candidate 'switch-to-buffer-other-window))

(helm-make-command-from-action helm-buffers-not-found-run-switch-ow
  "Run create new buffer other window action from keymap."
  'helm-buffers-create-new-buffer-ow)

(defun helm-buffers-create-new-buffer-of (candidate)
  (helm-buffers-create-new-buffer-1 candidate 'switch-to-buffer-other-frame))

(helm-make-command-from-action helm-buffers-not-found-run-switch-of
  "Run create new buffer other frame action from keymap."
  'helm-buffers-create-new-buffer-of)

(defvar helm-buffer-not-found-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o")   #'helm-buffers-not-found-run-switch-ow)
    (define-key map (kbd "C-c C-o") #'helm-buffers-not-found-run-switch-of)
    map)
  "Keymap for `helm-source-buffer-not-found' source.")

(defvar helm-source-buffer-not-found
  (helm-build-dummy-source
   "Create buffer"
   :action (helm-make-actions
            "Create buffer (C-u choose mode)"
            #'helm-buffers-create-new-buffer
            "Create buffer other window (C-u choose mode)"
            #'helm-buffers-create-new-buffer-ow
            "Create buffer other frame (C-u choose mode)"
            #'helm-buffers-create-new-buffer-of)
   :keymap helm-buffer-not-found-map))


(defun helm-buffers-get-visible-buffers ()
  "Returns buffers visibles on current frame."
  (let (result)
    (walk-windows
     (lambda (x)
       (push (buffer-name (window-buffer x)) result))
     nil 'visible)
    result))

(defun helm-buffer-list-1 (&optional visibles)
  (cl-loop for b in (buffer-list)
           for bn = (buffer-name b)
           unless (member bn visibles)
           collect bn))

(defun helm-buffers-reorder-buffer-list (visibles others)
  "Default function to reorder buffer-list.
Arg VISIBLES handles the buffers visibles in this frame.
Arg OTHERS handles all the other buffers.
This function returns OTHERS buffers on top and VISIBLES buffer
at the end."
  (nconc others visibles))

(defun helm-buffer-list ()
  "Return the current list of buffers.
The list is reordered with `helm-buffer-list-reorder-fn'."
  (let* ((visibles (helm-buffers-get-visible-buffers))
         (others   (helm-buffer-list-1 visibles)))
    (funcall helm-buffer-list-reorder-fn visibles others)))

(defun helm-buffer-size (buffer)
  "Return size of BUFFER."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (helm-file-human-size
       (- (position-bytes (point-max))
          (position-bytes (point-min)))))))

(defun helm-buffer--show-details (buf-name prefix help-echo
                                  size mode dir face1 face2
                                  proc details type)
  (append
   (list
    (let* ((buf-fname (buffer-file-name (get-buffer buf-name)))
           (ext (if buf-fname (helm-file-name-extension buf-fname) ""))
           (bmode (with-current-buffer buf-name major-mode))
           (icon (when helm-buffers-show-icons
                   (helm-aif (assq bmode all-the-icons-mode-icon-alist)
                       (apply (cadr it) (cddr it))
                     (cond ((eq type 'dired)
                            (all-the-icons-octicon "file-directory"))
                           (buf-fname
                            (all-the-icons-icon-for-file buf-name))
                           (t (all-the-icons-octicon "star" :v-adjust 0.0))))))
           (buf-name (propertize buf-name 'face face1
                                 'help-echo help-echo
                                 'type type)))
      (when (condition-case _err
                (string-match (format "\\.\\(%s\\)" ext) buf-name)
              (invalid-regexp nil))
        (add-face-text-property
         (match-beginning 1) (match-end 1)
         'helm-ff-file-extension nil buf-name))
      (if icon
          (concat icon " " prefix buf-name)
        (concat prefix buf-name))))
   (and details
        (list size mode
              (propertize
               (if proc
                   (format "(%s %s in `%s')"
                           (process-name proc)
                           (process-status proc) dir)
                 (format "(in `%s')" dir))
               'face face2)))))

(defun helm-buffer--format-mode-name (buf)
  "Prevent using `format-mode-line' as much as possible."
  (with-current-buffer buf
    (helm-acond ((assq major-mode helm-buffer--pretty-names)
                 (cdr it))
                ((stringp mode-name) mode-name)
                (t (format-mode-line mode-name nil nil (get-buffer buf))))))

(defun helm-buffer--details (buffer &optional details)
  (require 'dired)
  (let* ((mode (helm-buffer--format-mode-name buffer))
         (buf (get-buffer buffer))
         (size (propertize (helm-buffer-size buf)
                           'face 'helm-buffer-size))
         (proc (get-buffer-process buf))
         (dir (with-current-buffer buffer
                (helm-aif default-directory (abbreviate-file-name it))))
         (file-name (helm-aif (buffer-file-name buf) (abbreviate-file-name it)))
         (name (buffer-name buf))
         (name-prefix (when (and dir (file-remote-p dir))
                        (propertize "@ " 'face 'helm-ff-prefix)))
         (archive-p (and (fboundp 'tramp-archive-file-name-p)
                         (tramp-archive-file-name-p dir))))
    (when name-prefix
      ;; Remote tramp buffer names may be hexified, make them more readable.
      (setq dir  (helm-url-unhex-string dir)
            name (helm-url-unhex-string name)))
    ;; Handle tramp archive buffers specially.
    (if archive-p
        (helm-buffer--show-details
         name name-prefix file-name size mode dir
         'helm-buffer-archive 'helm-buffer-process nil details 'filebuf)
      ;; No fancy things on remote buffers.
      (if (and name-prefix helm-buffer-skip-remote-checking)
          (helm-buffer--show-details
           name name-prefix file-name size mode dir
           'helm-buffer-file 'helm-buffer-process nil details 'filebuf)
        (cond
          (;; A dired buffer.
           (rassoc buf dired-buffers)
           (helm-buffer--show-details
            name name-prefix dir size mode dir
            'helm-buffer-directory 'helm-buffer-process nil details 'dired))
          ;; A buffer file modified somewhere outside of emacs.=>red
          ((and file-name
                (file-exists-p file-name)
                (not (verify-visited-file-modtime buf)))
           (helm-buffer--show-details
            name name-prefix file-name size mode dir
            'helm-buffer-saved-out 'helm-buffer-process nil details 'modout))
          ;; A new buffer file not already saved on disk (or a deleted file) .=>indianred2
          ((and file-name (not (file-exists-p file-name)))
           (helm-buffer--show-details
            name name-prefix file-name size mode dir
            'helm-buffer-not-saved 'helm-buffer-process nil details 'notsaved))
          ;; A buffer file modified and not saved on disk.=>orange
          ((and file-name (buffer-modified-p buf))
           (helm-buffer--show-details
            name name-prefix file-name size mode dir
            'helm-buffer-modified 'helm-buffer-process nil details 'mod))
          ;; A buffer file not modified and saved on disk.=>green
          (file-name
           (helm-buffer--show-details
            name name-prefix file-name size mode dir
            'helm-buffer-file 'helm-buffer-process nil details 'filebuf))
          ;; A non-file, modified buffer See bug#1917
          ((with-current-buffer name
             (and helm-buffers-tick-counter
                  (/= helm-buffers-tick-counter (buffer-modified-tick))))
           (helm-buffer--show-details
            name (and proc name-prefix) dir size mode dir
            'helm-no-file-buffer-modified 'helm-buffer-process proc details 'nofile-mod))
          ;; Any non--file buffer.=>italic
          (t
           (helm-buffer--show-details
            name (and proc name-prefix) dir size mode dir
            'helm-non-file-buffer 'helm-buffer-process proc details 'nofile)))))))

(defun helm-highlight-buffers (buffers _source)
  "Transformer function to highlight BUFFERS list.
Should be called after others transformers i.e. (boring
buffers)."
  (cl-assert helm-fuzzy-matching-highlight-fn nil "Wrong type argument functionp: nil")
  (cl-loop for i in buffers
           for (name size mode meta) = (if helm-buffer-details-flag
                                           (helm-buffer--details i 'details)
                                         (helm-buffer--details i))
           for truncbuf = (if (> (string-width name) helm-buffer-max-length)
                              (helm-substring-by-width
                               name helm-buffer-max-length
                               helm-buffers-end-truncated-string)
                            (concat name
                                    (make-string
                                     (- (+ helm-buffer-max-length
                                           (length
                                            helm-buffers-end-truncated-string))
                                        (string-width name))
                                     ? )))
           for len = (length mode)
           when (> len helm-buffer-max-len-mode)
           do (setq helm-buffer-max-len-mode len)
           for fmode = (concat (make-string
                                (- (max helm-buffer-max-len-mode len) len) ? )
                               mode)
           ;; The max length of a number should be 1023.9X where X is the
           ;; units, this is 7 characters.
           for formatted-size = (and size (format "%7s" size))
           collect (let ((helm-pattern (helm-buffers--pattern-sans-filters
                                        (and helm-buffers-fuzzy-matching ""))))
                     (cons (if helm-buffer-details-flag
                               (concat
                                (funcall helm-fuzzy-matching-highlight-fn
                                         truncbuf)
                                helm-buffers-column-separator
                                formatted-size
                                helm-buffers-column-separator
                                fmode
                                helm-buffers-column-separator
                                meta)
                             (funcall helm-fuzzy-matching-highlight-fn name))
                           (get-buffer i)))))

(defun helm-buffer--get-preselection (buffer)
  (let* ((bufname     (buffer-name buffer))
         (dispbuf     (car (helm-buffer--details buffer)))
         (len-dispbuf (string-width dispbuf))
         (len-prefix  (- len-dispbuf (string-width bufname))))
    (when (and bufname
               (file-remote-p (with-current-buffer bufname
                                default-directory)))
      (setq bufname (concat "@ " (helm-url-unhex-string bufname))))
    (concat "^[[:multibyte:] ]*"
            (if (and (null helm-buffer-details-flag)
                     (numberp helm-buffer-max-length)
                     (> len-dispbuf helm-buffer-max-length))
                (regexp-quote
                 (helm-substring-by-width
                  bufname
                  (- helm-buffer-max-length len-prefix)
                  helm-buffers-end-truncated-string))
              (concat (regexp-quote bufname)
                      (if helm-buffer-details-flag
                          "$" "[[:blank:]]+"))))))

(defun helm-toggle-buffers-details ()
  (interactive)
  (with-helm-alive-p
    (let* ((buf (helm-get-selection))
           (preselect (helm-buffer--get-preselection buf)))
      (setq helm-buffer-details-flag (not helm-buffer-details-flag))
      (helm-force-update (lambda ()
                           (helm-awhile (re-search-forward preselect nil t)
                             (helm-mark-current-line)
                             (when (equal buf (helm-get-selection))
                               (cl-return t))))))))
(put 'helm-toggle-buffers-details 'helm-only t)

(defun helm-buffers--pattern-sans-filters (&optional separator)
  (cl-loop for p in (helm-mm-split-pattern helm-pattern)
           unless (member (substring p 0 1) '("*" "/" "@" "!"))
           collect p into lst
           finally return (mapconcat #'identity lst (or separator " "))))

(defun helm-buffers-sort-transformer (candidates source)
  (cl-assert helm-buffers-sort-fn nil "Wrong type argument functionp: nil")
  (if (string= helm-pattern "")
      candidates
    (let ((helm-pattern (helm-buffers--pattern-sans-filters)))
      (funcall helm-buffers-sort-fn candidates source))))

(defun helm-buffers-mark-similar-buffers-1 (&optional type)
  (with-helm-window
    (let* ((src (helm-get-current-source))
           (sel (helm-get-selection nil 'withprop src))
           (type (or type (get-text-property
                           (min 2 (length sel)) 'type sel))))
      (helm-map-candidates-in-source src
        (lambda (_cand) (helm-make-visible-mark))
        (lambda (cand)
          (and (not (helm-this-visible-mark))
               (eq (get-text-property 2 'type cand) type))))
      (helm-mark-current-line)
      (helm-display-mode-line src t)
      (when helm-marked-candidates
        (message "%s candidates marked" (length helm-marked-candidates))
        (set-window-margins (selected-window) 1)))))

(defun helm-buffers-mark-similar-buffers ()
    "Mark All buffers that have same property `type' than current.
I.e. same color."
  (interactive)
  (with-helm-alive-p
    (let ((marked (helm-marked-candidates)))
      (if (and (>= (length marked) 1)
               (with-helm-window helm-visible-mark-overlays))
          (helm-unmark-all)
          (helm-buffers-mark-similar-buffers-1)))))
(put 'helm-buffers-mark-similar-buffers 'helm-only t)


;;; match functions
;;
(defun helm-buffer--match-mjm (pattern mjm)
  (when (string-match "\\`\\*" pattern)
    (cl-loop with patterns = (split-string (substring pattern 1) ",")
             for pat in patterns
             if (string-match "\\`!" pat)
             collect (string-match (substring pat 1) mjm) into neg
             else collect (string-match pat mjm) into pos
             finally return
             (let ((neg-test (cl-loop for i in neg thereis (numberp i)))
                   (pos-test (cl-loop for i in pos thereis (numberp i))))
               (or
                (and neg (not pos) (not neg-test))
                (and pos pos-test)
                (and neg neg-test (not neg-test)))))))

(defvar helm-buffer--memo-hash (make-hash-table :test 'equal))
(defun helm-buffer--memo-pattern (pattern)
  (or (gethash pattern helm-buffer--memo-hash)
      (puthash pattern (helm--mapconcat-pattern pattern)
               helm-buffer--memo-hash)))

(defun helm-buffer--match-pattern (pattern candidate &optional nofuzzy)
  (let ((bfn (if (and helm-buffers-fuzzy-matching
                      (not nofuzzy)
                      (not helm-migemo-mode)
                      (not (string-match "\\`\\^" pattern)))
                 #'helm-buffer--memo-pattern
                 #'identity))
        (mfn (if helm-migemo-mode
                 #'helm-mm-migemo-string-match #'string-match)))
    (if (string-match "\\`!" pattern)
        (not (funcall mfn (funcall bfn (substring pattern 1))
                      candidate))
        (funcall mfn (funcall bfn pattern) candidate))))

(defun helm-buffers--match-from-mjm (candidate)
  (let* ((cand (replace-regexp-in-string "^\\s-\\{1\\}" "" candidate))
         (buf  (get-buffer cand))
         (regexp (cl-loop with pattern = helm-pattern
                          for p in (helm-mm-split-pattern pattern)
                          when (string-match "\\`\\*" p)
                          return p)))
    (if regexp
        (when buf
          (with-current-buffer buf
            (let ((mjm (symbol-name major-mode)))
              (helm-buffer--match-mjm regexp mjm))))
        t)))

(defun helm-buffers--match-from-pat (candidate)
  (let* ((regexp-list (cl-loop with pattern = helm-pattern
                               for p in (helm-mm-split-pattern pattern)
                               unless (string-match
                                       "\\`\\(\\*\\|/\\|@\\)" p)
                               collect p))
         (nofuzzy (cdr regexp-list)))
    (if regexp-list
        (cl-loop for re in regexp-list
                 always (helm-buffer--match-pattern re candidate nofuzzy))
        t)))

(defun helm-buffers--match-from-inside (candidate)
  (let* ((cand (replace-regexp-in-string "^\\s-\\{1\\}" "" candidate))
         (buf  (get-buffer cand))
         (pattern (cl-loop with pat = helm-pattern
                           for p in (helm-mm-split-pattern pat)
                           when (string-match "\\`@\\(.*\\)" p)
                           collect (match-string 1 p) into lst
                           finally return (mapconcat #'identity lst " ")))
         (patterns (helm-mm-3-get-patterns pattern)))
    (if (and buf patterns)
        (with-current-buffer buf
          (save-excursion
            (goto-char (point-min))
            (cl-loop for (pred . regexp) in patterns
                     always
                     (save-excursion
                       (funcall
                        pred
                        (if helm-migemo-mode
                            (helm-mm-migemo-forward regexp nil t)
                          (re-search-forward regexp nil t)))))))
      t)))

(defun helm-buffers--match-from-directory (candidate)
  (let* ((cand (replace-regexp-in-string "^\\s-\\{1\\}" "" candidate))
         (buf  (get-buffer cand))
         (buf-fname (or (buffer-file-name buf)
                        (car-safe (rassoc buf dired-buffers))))
         (regexps (cl-loop with pattern = helm-pattern
                          for p in (helm-mm-split-pattern pattern)
                          when (string-match "\\`/" p)
                          collect p)))
    (if regexps
        (cl-loop for re in regexps
                 thereis
                 (and buf-fname
                      (string-match
                       (substring re 1) (helm-basedir buf-fname))))
        t)))

(defun helm-buffers-match-function (candidate)
  "Default function to match buffers."
  (and (helm-buffers--match-from-pat candidate)
       (helm-buffers--match-from-mjm candidate)
       (helm-buffers--match-from-inside candidate)
       (helm-buffers--match-from-directory candidate)))


(defun helm-buffer-query-replace-1 (&optional regexp-flag buffers)
  "Query replace in marked buffers.
If REGEXP-FLAG is given use `query-replace-regexp'."
  (let ((prompt (if regexp-flag "Query replace regexp" "Query replace"))
        (bufs   (or buffers (helm-marked-candidates)))
        (helm--reading-passwd-or-string t))
    (cl-loop with args = (query-replace-read-args prompt regexp-flag t)
             for buf in bufs
             do
             (save-window-excursion
               (switch-to-buffer buf)
               (save-excursion
                 (let ((case-fold-search t))
                   (goto-char (point-min))
                   (apply #'perform-replace
                          (list (nth 0 args) (nth 1 args)
                                t regexp-flag (nth 2 args) nil
                                multi-query-replace-map))))))))

(defun helm-buffer-query-replace-regexp (_candidate)
  (helm-buffer-query-replace-1 'regexp))

(defun helm-buffer-query-replace (_candidate)
  (helm-buffer-query-replace-1))

(defun helm-buffer-toggle-diff (candidate)
  "Toggle diff buffer CANDIDATE with it's file."
  (helm-aif (get-buffer-window "*Diff*" 'visible)
      (progn (kill-buffer "*Diff*")
             (set-window-buffer it helm-current-buffer))
    (let ((buf (get-buffer candidate)))
      (if (buffer-file-name buf)
          (diff-buffer-with-file buf)
        (user-error "Buffer `%s' is not associated to a file"
                    (buffer-name buf))))))

(helm-make-persistent-command-from-action helm-buffer-diff-persistent
  "Toggle diff buffer without quitting helm."
  'diff-action 'helm-buffer-toggle-diff)

(defun helm-revert-buffer (candidate)
  (with-current-buffer candidate
    (helm-aif (buffer-file-name)
        (and (file-exists-p it) (revert-buffer t t)))))

(defun helm-revert-marked-buffers (_ignore)
  (mapc #'helm-revert-buffer (helm-marked-candidates)))

(defun helm-buffer-revert-and-update (_candidate)
  (with-helm-buffer
    (let ((marked (helm-marked-candidates))
          (preselect (helm-buffers--quote-truncated-buffer
                      (helm-get-selection))))
      (cl-loop for buf in marked do (helm-revert-buffer buf))
      (when helm-marked-candidates (helm-unmark-all))
      (helm-force-update preselect))))

(helm-make-persistent-command-from-action helm-buffer-revert-persistent
  "Revert buffer without quitting helm."
  'revert-action 'helm-buffer-revert-and-update)

(defun helm-buffer-save-and-update (_candidate)
  (with-helm-buffer
    (let ((marked (helm-marked-candidates))
          (preselect (helm-get-selection nil t))
          (enable-recursive-minibuffers t))
      (cl-assert marked nil "No buffers need to be saved")
      (cl-loop for buf in marked do
               (with-current-buffer (get-buffer buf)
                 (when (buffer-file-name) (save-buffer))))
      (when helm-marked-candidates (helm-unmark-all))
      (helm-force-update (regexp-quote preselect)))))

(defun helm-buffer-save-some-buffers (_candidate)
  (helm-buffers-mark-similar-buffers-1 'mod)
  (helm-buffer-save-and-update nil))

(helm-make-persistent-command-from-action helm-buffer-run-save-some-buffers
  "Save unsaved file buffers without quitting Helm."
  'save-some-action 'helm-buffer-save-some-buffers)

(helm-make-persistent-command-from-action helm-buffer-save-persistent
  "Save buffer without quitting Helm."
'save-action 'helm-buffer-save-and-update)

(defun helm-buffers-rename-buffer (candidate)
  (with-current-buffer candidate
    (rename-buffer (helm-read-string "New name: " (buffer-name)) t)))

(helm-make-command-from-action helm-buffer-run-rename-buffer
  "Run rename buffer action from `helm-source-buffers-list'."
  'helm-buffers-rename-buffer)

(defun helm-switch-to-buffer-at-linum (candidate)
  (let ((linum (read-number
                "Line number: "
                (with-current-buffer candidate
                  (line-number-at-pos)))))
    (switch-to-buffer candidate)
    (goto-char (point-min))
    (forward-line (1- linum))))

(helm-make-command-from-action helm-buffer-run-goto-line
  "Switch to buffer at line number."
  'helm-switch-to-buffer-at-linum)

(helm-make-persistent-command-from-action helm-buffer-run-kill-persistent
  "Kill buffer without quitting Helm."
  'kill-action 'helm-buffers-persistent-kill)

(defun helm-kill-marked-buffers (_ignore)
  (let* ((bufs (helm-marked-candidates))
         (killed-bufs (cl-count-if 'kill-buffer bufs)))
    (when (buffer-live-p helm-buffer)
      (with-helm-buffer
        (setq helm-marked-candidates nil
              helm-visible-mark-overlays nil)))
    (message "Killed %s buffer(s)" killed-bufs)))

(helm-make-command-from-action helm-buffer-run-kill-buffers
  "Run kill buffer action from `helm-source-buffers-list'."
  'helm-kill-marked-buffers)

(defun helm-buffer-switch-to-shell (candidate)
  (require 'helm-files)
  (let ((helm-ff-default-directory
         (with-current-buffer candidate
           default-directory)))
    (helm-ff-switch-to-shell nil)))

(helm-make-command-from-action helm-buffer-run-switch-to-shell
    "Run switch to shell action from helm-buffers-list."
  'helm-buffer-switch-to-shell)

(helm-make-command-from-action helm-buffer-run-grep
  "Run Grep action from `helm-source-buffers-list'."
  'helm-grep-buffers)

(helm-make-command-from-action helm-buffer-run-zgrep
  "Run Grep action from `helm-source-buffers-list'."
  'helm-zgrep-buffers)

(helm-make-command-from-action helm-buffer-run-query-replace-regexp
  "Run Query replace regexp action from `helm-source-buffers-list'."
'helm-buffer-query-replace-regexp)

(helm-make-command-from-action helm-buffer-run-query-replace
  "Run Query replace action from `helm-source-buffers-list'."
'helm-buffer-query-replace)

(helm-make-command-from-action helm-buffer-switch-other-window
  "Run switch to other window action from `helm-source-buffers-list'."
  'helm-buffer-switch-buffers-other-window)

(defun helm-buffer-switch-to-buffer-other-frame (_candidate)
  "Display marked buffers in other frame."
  (let ((bufs (helm-marked-candidates)))
    (select-frame (make-frame))
    (helm-window-show-buffers bufs)))

(defun helm-buffers-maybe-raise-buffer-frame (candidate)
  "Raise buffer frame handling buffer CANDIDATE and switch to it."
  (let ((oframe (window-frame (get-buffer-window candidate 0))))
    (unless (eql oframe (selected-frame))
      (raise-frame oframe))
    (with-selected-frame oframe
      (switch-to-buffer candidate))))

(helm-make-command-from-action helm-buffer-switch-other-frame
  "Run switch to other frame action from `helm-source-buffers-list'."
  'helm-buffer-switch-to-buffer-other-frame)

(defun helm-buffers-switch-to-buffer-other-tab (_candidate)
  (when (fboundp 'switch-to-buffer-other-tab)
    (let ((bufs (helm-marked-candidates)))
      (cl-loop for buf in bufs
               do (switch-to-buffer-other-tab buf)))))

(helm-make-command-from-action helm-buffers-switch-to-buffer-new-tab
  "Run switch to buffer in other tab action from `helm-source-buffers-list'."
  'helm-buffers-switch-to-buffer-other-tab
  (cl-assert (fboundp 'tab-bar-mode) nil "Tab-bar-mode not available"))

(defun helm-buffer-switch-buffers (_candidate)
  "Switch to buffer candidates and replace current buffer.

If more than one buffer marked switch to these buffers in
separate windows.  If a prefix arg is given split windows
vertically."
  (let ((buffers (helm-marked-candidates)))
    (helm-window-show-buffers buffers)))

(defun helm-buffer-switch-buffers-other-window (_candidate)
  "Switch to marked buffers in other windows."
  (let ((buffers (helm-marked-candidates)))
    (helm-window-show-buffers buffers t)))

(helm-make-command-from-action helm-buffer-run-ediff
  "Run ediff action from `helm-source-buffers-list'."
  'helm-ediff-marked-buffers)

(helm-make-command-from-action helm-buffer-run-ediff-merge
  "Run ediff action from `helm-source-buffers-list'."
  'helm-ediff-marked-buffers-merge)

(defun helm-buffers-persistent-kill-1 (buffer-or-name)
  "Persistent action to kill buffer."
  (let ((buf (get-buffer buffer-or-name)) helm-buf-or-cur)
    (if (or (and (eql buf (get-buffer helm-current-buffer))
                 (setq helm-buf-or-cur "helm-current-buffer"))
            (and (eql buf (get-buffer helm-buffer))
                 (setq helm-buf-or-cur "helm-buffer")))
        (progn
          (message "Can't kill `%s' without quitting session" helm-buf-or-cur)
          (sit-for 1))
      (kill-buffer buf)
      (helm-delete-current-selection))))

(defun helm-buffers--quote-truncated-buffer (buffer)
  (let ((bufname (and (bufferp buffer)
                      (buffer-name buffer))))
    (when (and bufname
               (file-remote-p (with-current-buffer bufname
                                default-directory)))
      (setq bufname (concat "@ " (helm-url-unhex-string bufname))))
    (when bufname
      (regexp-quote
       (if (and helm-buffer-max-length
                helm-buffer-details-flag)
           (helm-substring-by-width
            bufname helm-buffer-max-length
            "")
         bufname)))))

(defun helm-buffers-persistent-kill (_buffer)
  (let ((marked (helm-marked-candidates))
        (sel    (helm-get-selection)))
    (unwind-protect
         (cl-loop for b in marked
                  do (progn
                       ;; We need to preselect each marked because
                       ;; helm-buffers-persistent-kill is deleting
                       ;; current selection.
                       (helm-preselect
                        (format "^%s"
                                (helm-buffers--quote-truncated-buffer b)))
                       (helm-buffers-persistent-kill-1 b)
                       (message nil)
                       (helm--remove-marked-and-update-mode-line b)))
      (with-helm-buffer
        (setq helm-marked-candidates nil
              helm-visible-mark-overlays nil))
      (helm-force-update (helm-buffers--quote-truncated-buffer sel)))))

(defun helm-buffers-list-persistent-action (candidate)
  (let ((current (window-buffer helm-persistent-action-display-window)))
    (if (or (helm-follow-mode-p)
            (eql current (get-buffer helm-current-buffer))
            (not (eql current (get-buffer candidate))))
        (switch-to-buffer candidate)
      (if (and helm-persistent-action-display-window
               (window-dedicated-p
                (next-window helm-persistent-action-display-window 1)))
          (delete-window helm-persistent-action-display-window)
        (switch-to-buffer helm-current-buffer)))))

(defun helm-ediff-marked-buffers (_candidate &optional merge)
  "Ediff 2 marked buffers or CANDIDATE and `helm-current-buffer'.
With optional arg MERGE call `ediff-merge-buffers'."
  (let* ((mkd (helm-marked-candidates))
         (lg-lst (length mkd))
         buf1 buf2)
    (cl-case lg-lst
      (0
       (error "Error:You have to mark at least 1 buffer"))
      (1
       (setq buf1 helm-current-buffer
             buf2 (cl-first mkd)))
      (2
       (setq buf1 (cl-first mkd)
             buf2 (cl-second mkd)))
      (t
       (error "Error:Too many buffers marked!")))
    (if merge
        (ediff-merge-buffers buf1 buf2)
      (ediff-buffers buf1 buf2))))

(defun helm-ediff-marked-buffers-merge (candidate)
  "Ediff merge `helm-current-buffer' with CANDIDATE.
See `helm-ediff-marked-buffers'."
  (helm-ediff-marked-buffers candidate t))

(defun helm-multi-occur-as-action (_candidate)
  "Multi occur action for `helm-source-buffers-list'.
Can be used by any source that list buffers."
  (let ((helm-occur-always-search-in-current
         (if helm-current-prefix-arg
             (not helm-occur-always-search-in-current)
           helm-occur-always-search-in-current))
        (buffers (helm-marked-candidates))
        (input (cl-loop for i in (split-string (or (buffer-local-value
                                                    'helm-input-local
                                                    (get-buffer helm-buffer))
                                                   helm-pattern)
                                               " " t)
                        thereis (and (string-match "\\`@\\([^!]*\\)" i)
                                     (match-string 1 i)))))
    (helm-multi-occur-1 buffers input)))

(helm-make-command-from-action helm-buffers-run-occur
  "Run `helm-multi-occur-as-action' by key."
  'helm-multi-occur-as-action)

(defun helm-buffers-toggle-show-hidden-buffers ()
  (interactive)
  (with-helm-alive-p
    (let ((filter-attrs (helm-get-attr 'filtered-candidate-transformer
                                   helm-source-buffers-list))
          (sel          (helm-get-selection)))
      (if (memq 'helm-shadow-boring-buffers filter-attrs)
          (helm-set-attr 'filtered-candidate-transformer
                        (cons 'helm-skip-boring-buffers
                              (remove 'helm-shadow-boring-buffers
                                      filter-attrs))
                        helm-source-buffers-list)
        (helm-set-attr 'filtered-candidate-transformer
                      (cons 'helm-shadow-boring-buffers
                            (remove 'helm-skip-boring-buffers
                                    filter-attrs))
                      helm-source-buffers-list))
      (helm-force-update (helm-buffers--quote-truncated-buffer sel)))))
(put 'helm-buffers-toggle-show-hidden-buffers 'helm-only t)

(defun helm-buffers-browse-project (buf)
  "Browse project from buffer BUF."
  (with-current-buffer buf
    (helm-browse-project helm-current-prefix-arg)))

(helm-make-command-from-action helm-buffers-run-browse-project
    "Run `helm-buffers-browse-project' from key."
  'helm-buffers-browse-project
  (cl-assert (not helm-buffers-in-project-p)
             nil "You are already browsing this project"))

;;;###autoload
(defun helm-buffers-quit-and-find-file-fn (source)
  (let* ((sel   (get-buffer (helm-get-selection nil nil source)))
         (bname (and (bufferp sel) (buffer-name sel))))
    (when bname
      (or (buffer-file-name sel)
          (car (rassoc bname dired-buffers))
          (and (with-current-buffer bname
                 (eq major-mode 'org-agenda-mode))
               org-directory
               (expand-file-name org-directory))
          (with-current-buffer bname
            (expand-file-name default-directory))))))

;;; Candidate Transformers
;;
;;
(defun helm-skip-boring-buffers (buffers _source)
  "Remove buffers matching `helm-boring-buffer-regexp-list' in BUFFERS.
Where BUFFERS is a list of buffer names."
  (helm-skip-entries buffers
                     helm-boring-buffer-regexp-list
                     helm-white-buffer-regexp-list))

(defun helm-shadow-boring-buffers (buffers _source)
  "Buffers matching `helm-boring-buffer-regexp' will be
displayed with the `file-name-shadow' face if available."
  (helm-shadow-entries buffers helm-boring-buffer-regexp-list))


;;;###autoload
(defun helm-buffers-list ()
  "Preconfigured `helm' to list buffers."
  (interactive)
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))
  (helm :sources '(helm-source-buffers-list
                   helm-source-buffer-not-found)
        :buffer "*helm buffers*"
        :truncate-lines helm-buffers-truncate-lines
        :left-margin-width helm-buffers-left-margin-width))

;;;###autoload
(defun helm-mini ()
  "Preconfigured `helm' displaying `helm-mini-default-sources'."
  (interactive)
  (require 'helm-x-files)
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))
  (helm :sources helm-mini-default-sources
        :buffer "*helm mini*"
        :ff-transformer-show-only-basename nil
        :truncate-lines helm-buffers-truncate-lines
        :left-margin-width helm-buffers-left-margin-width))

(defun helm-quit-and-helm-mini ()
  "Drop into `helm-mini' from `helm'."
  (interactive)
  (with-helm-alive-p
    (helm-run-after-exit 'helm-mini)))

(provide 'helm-buffers)

;;; helm-buffers.el ends here
