;;; helm-occur.el --- Incremental Occur for Helm. -*- lexical-binding: t -*-

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
(require 'helm-help)
(require 'helm-utils)

(declare-function helm-buffers-get-visible-buffers "helm-buffers")
(declare-function helm-buffer-list "helm-buffers")
(declare-function helm-grep-split-line "helm-grep")
(declare-function helm-grep-highlight-match "helm-grep")
(declare-function helm-comp-read "helm-mode")

(defvar helm-current-error)

;;; Internals
;;
(defvar helm-source-occur nil
  "This will be the name of the source related to `current-buffer'.
Don't use it as it value changes always.")
(defvar helm-source-moccur nil
  "This is just a flag to add to `helm-sources-using-default-as-input'.
Don't set it to any value, it will have no effect.")
(defvar helm-occur--buffer-list nil)
(defvar helm-occur--buffer-tick nil)
(defvar helm-occur-history nil)
(defvar helm-occur--search-buffer-regexp "\\`\\([0-9]*\\)\\s-\\(.*\\)\\'"
  "The regexp matching candidates in helm-occur candidate buffer.")
(defvar helm-occur-mode--last-pattern nil)
(defvar helm-occur--initial-pos 0)

(defvar helm-occur-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o")    'helm-occur-run-goto-line-ow)
    (define-key map (kbd "C-c C-o")  'helm-occur-run-goto-line-of)
    (define-key map (kbd "C-x C-s")  'helm-occur-run-save-buffer)
    map)
  "Keymap used in occur source.")

(defgroup helm-occur nil
  "Regexp related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-occur-actions
  '(("Go to Line" . helm-occur-goto-line)
    ("Goto line other window (C-u vertically)" . helm-occur-goto-line-ow)
    ("Goto line new frame" . helm-occur-goto-line-of)
    ("Save buffer" . helm-occur-save-results)
    )
  "Actions for helm-occur."
  :type '(alist :key-type string :value-type function))

(defcustom helm-occur-use-ioccur-style-keys nil
  "Similar to `helm-grep-use-ioccur-style-keys' but for multi occur.

Note that if you define this variable with `setq' your change will
have no effect, use customize instead."
  :type 'boolean
  :set (lambda (var val)
         (set var val)
         (if val
             (progn
               (define-key helm-occur-map (kbd "<right>")  'helm-occur-right)
               (define-key helm-occur-map (kbd "<left>")   'helm-occur-run-default-action))
           (define-key helm-occur-map (kbd "<right>") nil)
           (define-key helm-occur-map (kbd "<left>")  nil))))

(defcustom helm-occur-always-search-in-current nil
  "Helm multi occur always search in current buffer when non--nil."
  :type 'boolean)

(defcustom helm-occur-truncate-lines t
  "Truncate lines in occur buffer when non nil."
  :type 'boolean)

(defcustom helm-occur-auto-update-on-resume nil
  "Allow auto updating helm-occur buffer when outdated.
noask => Always update without asking
nil   => Don't update but signal buffer needs update
never => Never update and do not signal buffer needs update
Any other non--nil value update after confirmation."
  :type '(radio :tag "Allow auto updating helm-occur buffer when outdated."
          (const :tag "Always update without asking" noask)
          (const :tag "Never update and do not signal buffer needs update" never)
          (const :tag "Don't update but signal buffer needs update" nil)
          (const :tag "Update after confirmation" t)))

(defcustom helm-occur-candidate-number-limit 99999
  "Value of `helm-candidate-number-limit' for helm-occur."
  :type 'integer)

(defcustom helm-occur-buffer-substring-fn-for-modes
  '((mu4e-headers-mode . buffer-substring))
  "Function used to display buffer contents per major-mode.

Use this to display lines with their text properties in helm-occur
buffer. Can be one of `buffer-substring' or `buffer-substring-no-properties'.
See `helm-occur-buffer-substring-default-mode' to setup this globally. 

Note that when using `buffer-substring' initialization will be slower."
  :type '(alist :key-type (symbol :tag "Mode")
                :value-type (radio (const :tag "With text properties"
                                    buffer-substring)
                                   (const :tag "Without text properties"
                                    buffer-substring-no-properties))))

(defcustom helm-occur-buffer-substring-default-mode
  'buffer-substring-no-properties
  "Function used to display buffer contents in helm-occur buffer.

Default mode for major modes not defined in
`helm-occur-buffer-substring-fn-for-modes'.
Can be one of `buffer-substring' or `buffer-substring-no-properties'.

Note that when using `buffer-substring' initialization will be
slower.  If buffer-substring, all buffers with the modes not
defined in helm-occur-buffer-substring-fn-for-modes will be
displayed with colors and properties in the helm-occur buffer"
  :type '(radio
          (const :tag "With text properties" buffer-substring)
          (const :tag "Without text properties" buffer-substring-no-properties)))

(defcustom helm-occur-keep-closest-position t
  "When non nil select closest candidate from point after update.
This happen only in `helm-source-occur' which is always related to
`current-buffer'."
  :type 'boolean)

(defcustom helm-occur-ignore-diacritics nil
  "When non nil helm-occur will ignore diacritics in patterns."
  :type 'boolean)

(defcustom helm-occur-match-shorthands nil
  "Transform pattern according to `read-symbol-shorthands' when non nil."
  :type 'boolean)

(defface helm-moccur-buffer
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "DarkTurquoise" :underline t))
  "Face used to highlight occur buffer names.")

(defface helm-resume-need-update
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :background "red"))
  "Face used to flash occur buffer when it needs update.")


(defun helm-occur--select-closest-candidate ()
  ;; Prevent error with `with-helm-window' when switching to help.
  (unless (or (not (get-buffer-window helm-buffer 'visible))
              (string-equal helm-pattern ""))
    (with-helm-window
      (let ((lst '())
            (name (helm-get-attr 'name helm-source-occur))
            closest beg end)
        (while-no-input
          (goto-char (point-min))
          (if (string= name "Helm occur")
              (setq beg (point)
                    end (point-max))
            (helm-awhile (helm-get-next-header-pos)
              (when (string= name (buffer-substring-no-properties
                                   (pos-bol) (pos-eol)))
                (forward-line 1)
                (setq beg (point)
                      end (or (helm-get-next-header-pos) (point-max)))
                (cl-return))))
          (save-excursion
            (when (and beg end)
              (goto-char beg)
              (while (re-search-forward "^[0-9]+" end t)
                (push (string-to-number (match-string 0)) lst))
              (setq closest (helm-closest-number-in-list
                             helm-occur--initial-pos lst))))
          (when (and closest (re-search-forward (format "^%s" closest) end t))
            (helm-mark-current-line)
            (goto-char (overlay-start
                        helm-selection-overlay))))))))

;;;###autoload
(defun helm-occur ()
    "Preconfigured helm for searching lines matching pattern in `current-buffer'.

When `helm-source-occur' is member of
`helm-sources-using-default-as-input' which is the default,
symbol at point is searched at startup.

When a region is marked search only in this region by narrowing.

To search in multiples buffers start from one of the commands listing
buffers (i.e. a helm command using `helm-source-buffers-list' like
`helm-mini') and use the multi occur buffers action.

This is the helm implementation that collect lines matching pattern
like vanilla Emacs `occur' but have nothing to do with it, the search
engine beeing completely different and also much faster."
  (interactive)
  (setq helm-source-occur
        (car (helm-occur-build-sources (list (current-buffer)) "Helm occur")))
  (helm-set-local-variable 'helm-occur--buffer-list (list (current-buffer))
                           'helm-occur--buffer-tick
                           (list (buffer-chars-modified-tick (current-buffer))))
  (helm-set-attr 'header-name (lambda (_name)
                                (format "HO [%s]"
                                        (buffer-name helm-current-buffer)))
                 helm-source-occur)
  (when helm-occur-keep-closest-position
    (setq helm-occur--initial-pos (line-number-at-pos))
    (add-hook 'helm-after-update-hook 'helm-occur--select-closest-candidate))
  (save-restriction
    (let ((helm-sources-using-default-as-input
           (unless (> (buffer-size) 2000000)
             helm-sources-using-default-as-input))
          def pos)
      (when (use-region-p)
        ;; When user mark defun with `mark-defun' with intention of
        ;; using helm-occur on this region, it is relevant to use the
        ;; thing-at-point located at previous position which have been
        ;; pushed to `mark-ring', if it's within the active region.
        (let ((beg (region-beginning))
              (end (region-end))
              (prev-pos (car mark-ring)))
          (when (and prev-pos (>= prev-pos beg) (< prev-pos end))
            (setq def (save-excursion
                        (goto-char (setq pos prev-pos))
                        (helm-aif (thing-at-point 'symbol) (regexp-quote it)))))
          (narrow-to-region beg end)))
      (unwind-protect
           (helm :sources 'helm-source-occur
                 :buffer "*helm occur*"
                 :history 'helm-occur-history
                 :default (or def (helm-aif (thing-at-point 'symbol)
                                      (regexp-quote it)))
                 :preselect (and (memq 'helm-source-occur
                                       helm-sources-using-default-as-input)
                                 (format "^%d:" (line-number-at-pos
                                                 (or pos (point)))))
                 :truncate-lines helm-occur-truncate-lines)
        (deactivate-mark t)
        (remove-hook 'helm-after-update-hook 'helm-occur--select-closest-candidate)))))

;;;###autoload
(defun helm-occur-visible-buffers ()
  "Run helm-occur on all visible buffers in frame."
  (interactive)
  (require 'helm-buffers)
  (if (or (one-window-p) (region-active-p))
      (call-interactively #'helm-occur)
    (let ((buffers (helm-buffers-get-visible-buffers)))
      (helm-multi-occur-1 (mapcar 'get-buffer buffers)))))

(defun helm-occur-transformer (candidates source)
  "Return CANDIDATES prefixed with line number."
  (cl-loop with buf = (helm-get-attr 'buffer-name source)
           for c in candidates
           for disp-linum = (when (string-match helm-occur--search-buffer-regexp c)
                              (let ((linum (match-string 1 c))
                                    (disp (match-string 2 c)))
                                (list
                                 linum
                                 (format "%s:%s"
                                         (propertize
                                          linum 'face 'helm-grep-lineno
                                          'help-echo (buffer-file-name
                                                      (get-buffer buf)))
                                         disp))))
           for linum = (car disp-linum)
           for disp = (cadr disp-linum)
           when (and disp (not (string= disp "")))
           collect (cons disp (string-to-number linum))))

(defvar helm-occur--gshorthands nil)
(defun helm-occur-symbol-shorthands-pattern-transformer (pattern buffer gshorthands)
  "Maybe transform PATTERN to its `read-symbol-shorthands' counterpart in BUFFER.

GSHORTHANDS is the concatenation of all `read-symbol-shorthands' value found in
all buffers i.e. `buffer-list'.
When GSHORTHANDS is nil use PATTERN unmodified."
  (if gshorthands
      (let* ((lshorthands (buffer-local-value 'read-symbol-shorthands buffer))
             (prefix (cl-loop for (k . v) in gshorthands
                              if (string-match (concat "\\`" k) pattern)
                              return k
                              else
                              if (string-match (concat "\\`" v) pattern)
                              return v))
             (lgstr (cdr (or (assoc prefix gshorthands)
                             (rassoc prefix gshorthands)))))
        (if (and lgstr lshorthands)
            (concat (car (rassoc lgstr lshorthands))
                    (replace-regexp-in-string prefix "" pattern))
          pattern))
    pattern))

(defclass helm-moccur-class (helm-source-in-buffer)
  ((buffer-name :initarg :buffer-name
                :initform nil)
   (moccur-buffers :initarg :moccur-buffers
                   :initform nil)
   (find-file-target :initform #'helm-occur-quit-an-find-file-fn)))

(defun helm-occur-build-sources (buffers &optional source-name)
  "Build sources for `helm-occur' for each buffer in BUFFERS list."
  (setq helm-occur--gshorthands nil)
  (and helm-occur-match-shorthands
       (setq helm-occur--gshorthands
             (cl-loop for b in (buffer-list)
                      for rss = (buffer-local-value
                                 'read-symbol-shorthands
                                 b)
                      when rss append rss)))
  (let (sources)
    (dolist (buf buffers)
      (let ((bname (buffer-name buf)))
        (push (helm-make-source (or source-name bname)
                  'helm-moccur-class
                :header-name (lambda (name)
                               (format "HO [%s]" (if (string= name "Helm occur")
                                                     bname name)))
                :buffer-name bname
                :match-part
                (lambda (candidate)
                  ;; The regexp should match what is in candidate buffer,
                  ;; not what is displayed in helm-buffer e.g. "12 foo"
                  ;; and not "12:foo".
                  (when (string-match helm-occur--search-buffer-regexp
                                      candidate)
                    (match-string 2 candidate)))
                :diacritics helm-occur-ignore-diacritics
                :search (lambda (pattern)
                          (when (string-match "\\`\\^\\([^ ]*\\)" pattern)
                            (setq pattern (concat "^[0-9]*\\s-" (match-string 1 pattern))))
                          (condition-case _err
                              (re-search-forward pattern nil t)
                            (invalid-regexp nil)))
                :pattern-transformer (lambda (pattern)
                                       (helm-occur-symbol-shorthands-pattern-transformer
                                        pattern buf helm-occur--gshorthands))
                :init (lambda ()
                        (with-current-buffer buf
                          (let* ((bsfn (or (cdr (assq
                                                 major-mode
                                                 helm-occur-buffer-substring-fn-for-modes))
                                           helm-occur-buffer-substring-default-mode))
                                 (contents (funcall bsfn (point-min) (point-max))))
                            (helm-set-attr 'get-line bsfn)
                            (with-current-buffer (helm-candidate-buffer 'global)
                              (insert contents)
                              (goto-char (point-min))
                              (let ((linum 1))
                                (insert (format "%s " linum))
                                (while (re-search-forward "\n" nil t)
                                  (cl-incf linum)
                                  (insert (format "%s " linum))))))))
                :filtered-candidate-transformer 'helm-occur-transformer
                :help-message 'helm-moccur-help-message
                :nomark t
                :migemo t
                ;; Needed for resume.
                :history 'helm-occur-history
                :candidate-number-limit helm-occur-candidate-number-limit
                :action 'helm-occur-actions
                :requires-pattern 2
                :follow 1
                :group 'helm-occur
                :keymap helm-occur-map
                :resume 'helm-occur-resume-fn
                :moccur-buffers buffers)
              sources)))
    (nreverse sources)))

(defun helm-multi-occur-1 (buffers &optional input default)
  "Run `helm-occur' on a list of buffers.
Each buffer's result is displayed in a separated source.
Arg INPUT if specified will be inserted as initial input in minibuffer.
Arg DEFAULT if specified will be inserted in minibuffer with M-n.
Arg INPUT takes precedence on DEFAULT if both are specified.
If `helm-source-moccur' is member of `helm-sources-using-default-as-input'
helm-occur will start immediately with DEFAULT as INPUT.
Always prefer using DEFAULT instead of INPUT, they have the same effect but
DEFAULT keep the minibuffer empty, allowing the user to write immediately
without having to delete its contents before."
  (let* ((curbuf (current-buffer))
         (bufs (if helm-occur-always-search-in-current
                   (cons curbuf (remove curbuf buffers))
                 buffers))
         (helm-sources-using-default-as-input
           (unless (cl-loop with total_size = 0
                            for b in bufs
                            do (setq total_size (buffer-size b))
                            finally return (> total_size 2000000))
             helm-sources-using-default-as-input))
         (sources (helm-occur-build-sources bufs (and (eql curbuf (car bufs))
                                                      (not (cdr bufs))
                                                      "Helm occur")))
         (helm-maybe-use-default-as-input
          (not (null (memq 'helm-source-moccur
                           helm-sources-using-default-as-input)))))
    (helm-set-local-variable 'helm-occur--buffer-list bufs
                             'helm-occur--buffer-tick
                             (cl-loop for b in bufs collect
                                      (buffer-chars-modified-tick
                                       (get-buffer b))))
    (when (and helm-occur-always-search-in-current
               helm-occur-keep-closest-position)
      (setq helm-source-occur
            (cl-loop for s in sources
                     when (eql helm-current-buffer
                               (get-buffer (helm-get-attr 'buffer-name s)))
                     return s))
      (setq helm-occur--initial-pos (line-number-at-pos))
      (add-hook 'helm-after-update-hook 'helm-occur--select-closest-candidate))
    (unwind-protect
        (helm :sources sources
              :buffer "*helm moccur*"
              :history 'helm-occur-history
              :default (or default
                           (helm-aif (thing-at-point 'symbol)
                               (regexp-quote it)))
              :input input
              :truncate-lines helm-occur-truncate-lines)
      (remove-hook 'helm-after-update-hook 'helm-occur--select-closest-candidate))))

;;; Actions
;;
(cl-defun helm-occur-action (lineno
                                  &optional (method (quote buffer)))
  "Jump to line number LINENO with METHOD.
METHOD can be one of buffer, buffer-other-window, buffer-other-frame."
  (require 'helm-grep)
  (let ((buf (if (eq major-mode 'helm-occur-mode)
                 (get-text-property (point) 'buffer-name)
               (helm-get-attr 'buffer-name)))
        (split-pat (helm-mm-split-pattern helm-input)))
    (cl-case method
      (buffer              (switch-to-buffer buf))
      (buffer-other-window (helm-window-show-buffers (list buf) t))
      (buffer-other-frame  (switch-to-buffer-other-frame buf)))
    (with-current-buffer buf
      (helm-goto-line lineno)
      ;; Move point to the nearest matching regexp from bol.
      (cl-loop for str in split-pat
               for reg = (helm-occur-symbol-shorthands-pattern-transformer
                          str (get-buffer buf) helm-occur--gshorthands)
               when (save-excursion
                      (condition-case _err
                          (if helm-migemo-mode
                              (helm-mm-migemo-forward reg (pos-eol) t)
                            (re-search-forward reg (pos-eol) t))
                        (invalid-regexp nil)))
               collect (match-beginning 0) into pos-ls
               finally (when pos-ls (goto-char (apply #'min pos-ls)))))))

(defun helm-occur-goto-line (candidate)
  "From multi occur, switch to buffer and CANDIDATE line."
  (helm-occur-action
   candidate 'buffer))

(defun helm-occur-goto-line-ow (candidate)
  "Go to CANDIDATE line in other window.
Same as `helm-occur-goto-line' but go in other window."
  (helm-occur-action
   candidate 'buffer-other-window))

(defun helm-occur-goto-line-of (candidate)
  "Go to CANDIDATE line in new frame.
Same as `helm-occur-goto-line' but go in new frame."
  (helm-occur-action
   candidate 'buffer-other-frame))

(helm-make-command-from-action helm-occur-run-goto-line-ow
  "Run goto line other window action from `helm-occur'."
  'helm-occur-goto-line-ow)

(helm-make-command-from-action helm-occur-run-goto-line-of
  "Run goto line new frame action from `helm-occur'."
  'helm-occur-goto-line-of)

(helm-make-command-from-action helm-occur-run-default-action
    "Goto matching line from helm-occur buffer."
    'helm-occur-goto-line)

(helm-make-command-from-action helm-occur-run-save-buffer
  "Run moccur save results action from `helm-moccur'."
  'helm-occur-save-results)

(defun helm-occur-right ()
  "`helm-occur' action for right arrow.
This is used when `helm-occur-use-ioccur-style-keys' is enabled.
If follow is enabled (default) go to next source, otherwise execute
persistent action."
  (interactive)
  (if (helm-aand (helm-get-attr 'follow) (> it 0))
      (helm-next-source)
    (helm-execute-persistent-action)))
(put 'helm-occur-right 'helm-only t)

(defun helm-occur-quit-an-find-file-fn (source)
  (let* ((sel (helm-get-selection nil nil source))
         (occur-fname (helm-aand (numberp sel)
                                 (helm-get-attr 'buffer-name)
                                 (buffer-file-name (get-buffer it)))))
    (when (and occur-fname (file-exists-p occur-fname))
      (expand-file-name occur-fname))))

;;; helm-occur-mode
;;
;;
(defvar helm-occur-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")      'helm-occur-mode-goto-line)
    (define-key map (kbd "C-o")      'helm-occur-mode-goto-line-ow)
    (define-key map (kbd "<C-down>") 'helm-occur-mode-goto-line-ow-forward)
    (define-key map (kbd "<C-up>")   'helm-occur-mode-goto-line-ow-backward)
    (define-key map (kbd "<M-down>") 'helm-gm-next-file)
    (define-key map (kbd "<M-up>")   'helm-gm-precedent-file)
    (define-key map (kbd "M-n")      'helm-occur-mode-goto-line-ow-forward)
    (define-key map (kbd "M-p")      'helm-occur-mode-goto-line-ow-backward)
    (define-key map (kbd "M-N")      'helm-gm-next-file)
    (define-key map (kbd "M-P")      'helm-gm-precedent-file)
    (define-key map (kbd "C-c b")    'helm-occur-mode-resume-session)
    map))

(defun helm-occur-mode-goto-line ()
  (interactive)
  (setq next-error-last-buffer (current-buffer))
  (setq-local helm-current-error (point-marker))
  (helm-aif (get-text-property (point) 'helm-realvalue)
    (progn (helm-occur-goto-line it) (helm-match-line-cleanup-pulse))))

(defun helm-occur-mode-goto-line-ow ()
  (interactive)
  (setq next-error-last-buffer (current-buffer))
  (setq-local helm-current-error (point-marker))
  (helm-aif (get-text-property (point) 'helm-realvalue)
    (progn (helm-occur-goto-line-ow it) (helm-match-line-cleanup-pulse))))

(defun helm-occur-mode-goto-line-ow-forward-1 (arg)
  (condition-case nil
      (progn
        (when (or (eq last-command 'helm-occur-mode-goto-line-ow-forward)
                  (eq last-command 'helm-occur-mode-goto-line-ow-backward))
          (forward-line arg))
        (save-selected-window
          (helm-occur-mode-goto-line-ow)
          (recenter)))
    (error nil)))

(defun helm-occur-mode-goto-line-ow-forward (arg)
  (interactive "p")
  (helm-occur-mode-goto-line-ow-forward-1 arg))

(defun helm-occur-mode-goto-line-ow-backward (arg)
  (interactive "p")
  (helm-occur-mode-goto-line-ow-forward-1 (- arg)))

(defun helm-occur-save-results (_candidate)
  "Save helm moccur results in a `helm-moccur-mode' buffer."
  (let ((buf "*hmoccur*")
        new-buf)
    (when (get-buffer buf)
      (setq new-buf (helm-read-string "OccurBufferName: " buf))
      (cl-loop for b in (helm-buffer-list)
               when (and (string= new-buf b)
                         (not (y-or-n-p
                               (format "Buffer `%s' already exists overwrite? "
                                       new-buf))))
               do (setq new-buf (helm-read-string
                                 "OccurBufferName: " "*hmoccur ")))
      (setq buf new-buf))
    (with-current-buffer (get-buffer-create buf)
      (kill-all-local-variables)
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (let ((inhibit-read-only t)
            (map (make-sparse-keymap))
            buf-name)
        (erase-buffer)
        (insert "-*- mode: helm-occur -*-\n\n"
                (format "Occur Results for `%s':\n\n" helm-input))
        (save-excursion
          (insert (with-current-buffer helm-buffer
                    (goto-char (point-min))
                    (forward-line 1)
                    (buffer-substring (point) (point-max)))))
        (save-excursion
          (forward-line -2)
          (while (not (eobp))
            (if (helm-pos-header-line-p)
                (let ((beg (pos-bol))
                      (end (pos-eol)))
                  (set-text-properties beg (1+ end) nil)
                  (delete-region (1- beg) end))
              (helm-aif (setq buf-name (assoc-default
                                        'buffer-name
                                        (get-text-property (point) 'helm-cur-source)))
                  (progn
                    (insert (propertize (concat it ":")
                                        'face 'helm-moccur-buffer
                                        'helm-realvalue (get-text-property (point) 'helm-realvalue)))
                    (add-text-properties
                     (pos-bol) (pos-eol)
                     `(buffer-name ,buf-name))
                    (add-text-properties
                     (pos-bol) (pos-eol)
                     `(keymap ,map
                              help-echo ,(concat
                                          (buffer-file-name
                                           (get-buffer buf-name))
                                          "\nmouse-1: set point\nmouse-2: jump to selection")
                              mouse-face highlight
                              invisible nil))
                    (define-key map [mouse-1] 'mouse-set-point)
                    (define-key map [mouse-2] 'helm-occur-mode-mouse-goto-line)
                    (define-key map [mouse-3] 'ignore))))
            (forward-line 1))))
      (buffer-enable-undo)
      (helm-occur-mode))
    (pop-to-buffer buf)
    (setq next-error-last-buffer (get-buffer buf))
    (message "Helm occur Results saved in `%s' buffer" buf)))

(defun helm-occur-mode-mouse-goto-line (event)
  (interactive "e")
  (let* ((window (posn-window (event-end event)))
         (pos    (posn-point (event-end event))))
    (with-selected-window window
      (when (eq major-mode 'helm-occur-mode)
        (goto-char pos)
        (helm-occur-mode-goto-line)))))
(put 'helm-moccur-mode-mouse-goto-line 'helm-only t)

(defun helm-occur-mode-resume-session ()
  (interactive)
  (cl-assert (eq major-mode 'helm-occur-mode) nil "Helm command called in wrong context")
  (helm-multi-occur-1 helm-occur--buffer-list helm-occur-mode--last-pattern))

(defun helm-occur-buffer-substring-with-linums ()
  "Return current-buffer contents as a string with all lines
numbered.  The property \\='buffer-name is added to the whole string."
  (let ((bufstr (buffer-substring-no-properties (point-min) (point-max)))
        (bufname (buffer-name)))
    (with-temp-buffer
      (save-excursion
        (insert bufstr))
      (let ((linum 1))
        (insert (format "%s " linum))
        (while (re-search-forward "\n" nil t)
          (cl-incf linum)
          (insert (format "%s " linum)))
        (add-text-properties (point-min) (point-max) `(buffer-name ,bufname)))
      (buffer-string))))

(defun helm-occur-mode--revert-buffer-function (&optional _ignore-auto _noconfirm)
  "The `revert-buffer-function' for `helm-occur-mode'."
  (goto-char (point-min))
  (let (pattern)
    (when (re-search-forward "^Occur Results for `\\(.*\\)'" nil t)
      (setq pattern (match-string 1))
      (forward-line 0)
      (when (re-search-forward "^$" nil t)
        (forward-line 1))
      (let ((inhibit-read-only t)
            (buffer (current-buffer))
            (buflst helm-occur--buffer-list))
        (delete-region (point) (point-max))
        (message "Reverting buffer...")
        (save-excursion
          (with-temp-buffer
            (insert
             "\n"
             (cl-loop for buf in buflst
                      for bufstr = (or (and (buffer-live-p (get-buffer buf))
                                            (with-current-buffer buf
                                              (helm-occur-buffer-substring-with-linums)))
                                       "")
                      concat bufstr)
             "\n")
            (goto-char (point-min))
            (cl-loop with linum
                     with mpart
                     ;; Bind helm-pattern used by `helm-grep-split-line'.
                     with helm-pattern = pattern
                     while (helm-mm-search pattern) ; point is at eol.
                     ;; Calculate line number (linum) and extract real
                     ;; part of line (mpart).
                     do (when (save-excursion
                                ;; `helm-mm-search' puts point at eol.
                                (forward-line 0)
                                (re-search-forward "^\\([0-9]*\\)\\s-\\{1\\}\\(.*\\)$"
                                                   (pos-eol) t))
                          (setq linum (string-to-number (match-string 1))
                                mpart (match-string 2)))
                     ;; Match part after line number.
                     when (and mpart (helm-mm-match mpart pattern))
                     for line = (format "%s:%d:%s"
                                        (get-text-property (point) 'buffer-name)
                                        linum
                                        mpart)
                     when line
                     do (with-current-buffer buffer
                          (insert
                           (propertize
                            (car (helm-occur-filter-one-by-one line))
                            'helm-realvalue linum)
                           "\n"))))
          (when (fboundp 'wgrep-cleanup-overlays)
            (wgrep-cleanup-overlays (point-min) (point-max)))
          (message "Reverting buffer done")
          (when executing-kbd-macro (sit-for 1)))))))

(defun helm-occur-filter-one-by-one (candidate)
  "`filter-one-by-one' function for `helm-source-moccur'."
  (require 'helm-grep)
  (let* ((split  (helm-grep-split-line candidate))
         (buf    (car split))
         (lineno (nth 1 split))
         (str    (nth 2 split)))
    (cons (concat (propertize
                   buf
                   'face 'helm-moccur-buffer
                   'help-echo (buffer-file-name
                               (get-buffer buf))
                   'buffer-name buf)
                  ":"
                  (propertize lineno 'face 'helm-grep-lineno)
                  ":"
                  (helm-grep-highlight-match str))
          candidate)))

(define-derived-mode helm-occur-mode
    special-mode "helm-moccur"
    "Major mode to provide actions in helm moccur saved buffer.

Special commands:
\\{helm-occur-mode-map}"
    (set (make-local-variable 'helm-occur--buffer-list)
         (with-helm-buffer helm-occur--buffer-list))
    (set (make-local-variable 'revert-buffer-function)
         #'helm-occur-mode--revert-buffer-function)
    (set (make-local-variable 'helm-occur-mode--last-pattern)
         helm-input)
    (set (make-local-variable 'next-error-function)
         #'helm-occur-next-error)
    (set (make-local-variable 'helm-current-error) nil))
(put 'helm-moccur-mode 'helm-only t)

(defun helm-occur-next-error (&optional argp reset)
  "Goto ARGP position from a `helm-occur-mode' buffer.
RESET non-nil means rewind to the first match.
This is the `next-error-function' for `helm-occur-mode'."
  (interactive "p")
  (goto-char (cond (reset (point-min))
		   ((and (< argp 0) helm-current-error)
                    (line-beginning-position))
		   ((and (> argp 0) helm-current-error)
                    (line-end-position))
		   ((point))))
  (let ((fun (if (> argp 0)
                 #'next-single-property-change
               #'previous-single-property-change)))
    (helm-aif (funcall fun (point) 'buffer-name)
        (progn
          (goto-char it)
          (forward-line 0)
          ;; `helm-current-error' is set in
          ;; `helm-occur-mode-goto-line'.
          (helm-occur-mode-goto-line))
      (user-error "No more matches"))))

;;; Resume
;;
(defun helm-occur-resume-fn ()
  (with-helm-buffer
    (let (new-tick-ls buffer-is-modified)
      (set (make-local-variable 'helm-occur--buffer-list)
           (cl-loop for b in helm-occur--buffer-list
                    when (buffer-live-p (get-buffer b))
                    collect b))
      (setq buffer-is-modified (/= (length helm-occur--buffer-list)
                                   (length (helm-get-attr 'moccur-buffers))))
      (helm-set-attr 'moccur-buffers helm-occur--buffer-list)
      (setq new-tick-ls (cl-loop for b in helm-occur--buffer-list
                                 collect (buffer-chars-modified-tick
                                          (get-buffer b))))
      (when buffer-is-modified
        (setq helm-occur--buffer-tick new-tick-ls))
      (cl-assert (> (length helm-occur--buffer-list) 0) nil
                 "helm-resume error: helm-(m)occur buffer list is empty")
      (unless (eq helm-occur-auto-update-on-resume 'never)
        (when (or buffer-is-modified
                  (cl-loop for b in helm-occur--buffer-list
                           for new-tick = (buffer-chars-modified-tick
                                           (get-buffer b))
                           for tick in helm-occur--buffer-tick
                           thereis (/= tick new-tick)))
          (helm-aif helm-occur-auto-update-on-resume
              (when (or (eq it 'noask)
                        (y-or-n-p "Helm (m)occur Buffer outdated, update? "))
                (run-with-idle-timer
                 0.1 nil (lambda ()
                           (with-helm-buffer
                             (helm-force-update)
                             (message "Helm (m)occur Buffer have been udated")
                             (sit-for 1) (message nil))))
                (unless buffer-is-modified (setq helm-occur--buffer-tick
                                                 new-tick-ls)))
            (run-with-idle-timer
             0.1 nil
             (lambda ()
               (with-helm-buffer
                 (let ((ov (make-overlay (save-excursion
                                           (goto-char (point-min))
                                           (forward-line 1)
                                           (point))
                                         (point-max))))
                   (overlay-put ov 'face 'helm-resume-need-update)
                   (sit-for 0)
                   (delete-overlay ov)
                   (message "[Helm occur Buffer outdated (C-c C-u to update)]")))))
            (unless buffer-is-modified
              (with-helm-after-update-hook
                (setq helm-occur--buffer-tick new-tick-ls)
                (message "Helm (m)occur Buffer have been udated")))))))))

;;; Helm occur from isearch
;;
;;;###autoload
(defun helm-occur-from-isearch ()
  "Invoke `helm-occur' from isearch.

To use this bind it to a key in `isearch-mode-map'."
  (interactive)
  (let ((input (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string)))
        (bufs (list (current-buffer)))
        ;; Use `helm-occur-always-search-in-current' as a flag for
        ;; `helm-occur--select-closest-candidate'.
        (helm-occur-always-search-in-current t))
    (isearch-exit)
    (helm-multi-occur-1 bufs input)))

;;;###autoload
(defun helm-multi-occur-from-isearch ()
  "Invoke `helm-multi-occur' from isearch.

With a prefix arg, reverse the behavior of
`helm-moccur-always-search-in-current'.
The prefix arg can be set before calling
`helm-multi-occur-from-isearch' or during the buffer selection.

To use this bind it to a key in `isearch-mode-map'."
  (interactive)
  (let (buf-list
        helm-moccur-always-search-in-current
        (input (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-exit)
    (setq buf-list (mapcar 'get-buffer
                           (helm-comp-read "Buffers: "
                                           (helm-buffer-list)
                                           :name "Occur in buffer(s)"
                                           :marked-candidates t)))
    (setq helm-moccur-always-search-in-current
          (if (or current-prefix-arg
                  helm-current-prefix-arg)
              (not helm-moccur-always-search-in-current)
            helm-moccur-always-search-in-current))
    (helm-multi-occur-1 buf-list input)))

(provide 'helm-occur)

;;; helm-occur.el ends here
