;;; helm-ag.el --- The silver searcher with helm interface -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-ag
;; Version: 0.64
;; Package-Requires: ((emacs "25.1") (helm "2.0"))

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

;;; Commentary:

;; helm-ag provides interfaces of the silver searcher(Other search programs can be used
;; such as the platinum searcher, ack). And helm-ag provides wgrep like features which
;; users can edit from searched result.

;;; Code:

(eval-when-compile
  (require 'grep)
  (defvar helm-help-message))

(require 'cl-lib)
(require 'helm)
(require 'helm-grep)
(require 'helm-occur)
(require 'helm-utils)
(require 'compile)
(require 'subr-x)

(declare-function helm-read-file-name "helm-mode")
(declare-function helm-grep-get-file-extensions "helm-grep")
(declare-function helm-help "helm-help")

(defgroup helm-ag nil
  "the silver searcher with helm interface"
  :group 'helm)

(defsubst helm-ag--windows-p ()
  "Check ag available window."
  (memq system-type '(ms-dos windows-nt)))

(defcustom helm-ag-base-command
  (if (helm-ag--windows-p)
      "ag --vimgrep"
    "ag --nocolor --nogroup")
  "Base command of `ag'."
  :type 'string
  :group 'helm-ag)

(defcustom helm-ag-command-option nil
  "Command line option of `ag'.  This is appended after `helm-ag-base-command'."
  :type 'string
  :group 'helm-ag)

(defcustom helm-ag-success-exit-status nil
  "Allows specifying the return code or codes of `helm-ag-base-command' that \
will be treated as successful."
  :type '(choice integer
                 (list integer)))

(defcustom helm-ag-insert-at-point nil
  "Insert thing at point as search pattern.
You can set value same as `thing-at-point'"
  :type 'symbol
  :group 'helm-ag)

(defcustom helm-ag-ignore-patterns nil
  "Ignore patterns for `ag'.  This parameters are specified as --ignore."
  :type '(repeat string))

(defcustom helm-ag-use-grep-ignore-list nil
  "Use `grep-find-ignored-files' and `grep-find-ignored-directories' as \
ignore pattern.
They are specified to `--ignore' options."
  :type 'boolean
  :group 'helm-ag)

(defcustom helm-ag-always-set-extra-option nil
  "Always set `ag' options of `helm-do-ag'."
  :type 'boolean
  :group 'helm-ag)

(defcustom helm-ag-fuzzy-match nil
  "Enable fuzzy match."
  :type 'boolean
  :group 'helm-ag)

(defcustom helm-ag-edit-save t
  "Save buffers you edit at completed."
  :type 'boolean
  :group 'helm-ag)

(defcustom helm-ag-use-emacs-lisp-regexp nil
  "[Experimental] Use Emacs Lisp regexp instead of PCRE."
  :type 'boolean
  :group 'helm-ag)

(defcustom helm-ag-use-agignore nil
  "Use .agignore where is at project root if it exists."
  :type 'boolean
  :group 'helm-ag)

(defcustom helm-ag-use-temp-buffer nil
  "Use temporary buffer for persistent action."
  :type 'boolean
  :group 'helm-ag)

(defcustom helm-ag-ignore-buffer-patterns nil
  "Use temporary buffer for persistent action."
  :type '(repeat regexp)
  :group 'helm-ag)

(defcustom helm-ag-show-status-function 'helm-ag-show-status-default-mode-line
  "Function called after that `ag' process is finished after `helm-do-ag'.
Default behaviour shows finish and result in mode-line."
  :type 'function
  :group 'helm-ag)

(defface helm-ag-edit-deleted-line
  '((t (:inherit font-lock-comment-face :strike-through t)))
  "Face of deleted line in edit mode.")

(defvar helm-ag--command-history '())
(defvar helm-ag--helm-history '())
(defvar helm-ag--context-stack nil)
(defvar helm-ag--default-directory nil)
(defvar helm-ag--last-default-directory nil)
(defvar helm-ag--last-query nil)
(defvar helm-ag--last-command nil)
(defvar helm-ag--elisp-regexp-query nil)
(defvar helm-ag--valid-regexp-for-emacs nil)
(defvar helm-ag--extra-options nil)
(defvar helm-ag--extra-options-history nil)
(defvar helm-ag--original-window nil)
(defvar helm-ag--search-this-file-p nil)
(defvar helm-ag--default-target nil)
(defvar helm-ag--buffer-search nil)
(defvar helm-ag--command-features '())
(defvar helm-ag--ignore-case nil)
(defvar helm-do-ag--extensions nil)
(defvar helm-do-ag--commands nil)

(defun helm-ag--ignore-case-p (cmds input)
  "Not documented, CMDS, INPUT."
  (cl-loop for cmd in cmds
           when (member cmd '("-i" "--ignore-case"))
           return t

           when (member cmd '("-s" "--case-sensitive"))
           return nil

           finally
           return (let ((case-fold-search nil))
                    (not (string-match-p "[A-Z]" input)))))

(defun helm-ag--save-current-context ()
  "Not documented."
  (let ((curpoint (with-helm-current-buffer
                   (point))))
    (helm-aif (buffer-file-name helm-current-buffer)
              (push (list :file it :point curpoint) helm-ag--context-stack)
              (push (list :buffer helm-current-buffer :point curpoint) helm-ag--context-stack))))

(defun helm-ag--insert-thing-at-point (thing)
  "Not documented, THING."
  (helm-aif (thing-at-point thing)
            (substring-no-properties it)
            ""))

(defun helm-ag--searched-word ()
  "Not documented."
  (if helm-ag-insert-at-point
      (helm-ag--insert-thing-at-point helm-ag-insert-at-point)
    ""))

(defun helm-ag--construct-ignore-option (pattern)
  "Not documented, PATTERN."
  (concat "--ignore=" pattern))

(defun helm-ag--grep-ignore-list-to-options ()
  "Not documented."
  (require 'grep)
  (cl-loop for ignore in (append grep-find-ignored-files
                                 grep-find-ignored-directories)
           collect (helm-ag--construct-ignore-option ignore)))

(defun helm-ag--parse-options-and-query (input)
  "Not documented, INPUT."
  (with-temp-buffer
    (insert input)
    (let (end options)
      (goto-char (point-min))
      (when (re-search-forward "\\s-*--\\s-+" nil t)
        (setq end (match-end 0)))
      (goto-char (point-min))
      (while (re-search-forward "\\(?:\\=\\|\\s-+\\)\\(-\\S-+\\)\\(?:\\s-+\\|$\\)" end t)
        (push (match-string-no-properties 1) options)
        (when end
          (cl-decf end (- (match-end 0) (match-beginning 0))))
        (replace-match ""))
      (cons options (buffer-string)))))

(defun helm-ag--parse-query (input)
  "Not documented, INPUT."
  (let* ((parsed (helm-ag--parse-options-and-query input))
         (options (car parsed))
         (query (cdr parsed)))
    (when helm-ag-use-emacs-lisp-regexp
      (setq query (helm-ag--elisp-regexp-to-pcre query)))
    (setq helm-ag--last-query query
          helm-ag--elisp-regexp-query (helm-ag--convert-to-elisp-regexp query))
    (setq helm-ag--valid-regexp-for-emacs
          (helm-ag--validate-regexp helm-ag--elisp-regexp-query))
    (if (not options)
        (list query)
      (nconc (nreverse options) (list query)))))

(defsubst helm-ag--search-buffer-p (bufname)
  "Not documented, BUFNAME."
  (cl-loop for regexp in helm-ag-ignore-buffer-patterns
           never (string-match-p regexp bufname)))

(defun helm-ag--file-visited-buffers ()
  "Not documented."
  (let ((bufs (cl-loop for buf in (buffer-list)
                       when (buffer-file-name buf)
                       collect it)))
    (if (not helm-ag-ignore-buffer-patterns)
        bufs
      (cl-loop for buf in bufs
               when (helm-ag--search-buffer-p buf)
               collect buf))))

(defun helm-ag--construct-targets (targets)
  "Not documented, TARGETS."
  (let ((default-directory helm-ag--default-directory))
    (cl-loop for target in targets
             collect (file-relative-name target))))

(defun helm-ag--root-agignore ()
  "Not documented."
  (let ((root (helm-ag--project-root)))
    (when root
      (let ((default-directory root))
        (when (file-exists-p ".agignore")
          (expand-file-name (concat default-directory ".agignore")))))))

(defun helm-ag--construct-command (this-file)
  "Not documented, THIS-FILE."
  (let* ((commands (split-string helm-ag-base-command nil t))
         (command (car commands))
         (args (cdr commands)))
    (when helm-ag-command-option
      (let ((ag-options (split-string helm-ag-command-option nil t)))
        (setq args (append args ag-options))))
    (when helm-ag-use-agignore
      (helm-aif (helm-ag--root-agignore)
                (setq args (append args (list "-p" it)))))
    (when helm-ag-ignore-patterns
      (setq args (append args (mapcar 'helm-ag--construct-ignore-option
                                      helm-ag-ignore-patterns))))
    (when helm-ag-use-grep-ignore-list
      (setq args (append args (helm-ag--grep-ignore-list-to-options))))
    (setq args (append args (helm-ag--parse-query helm-ag--last-query)))
    (when this-file
      (setq args (append args (list this-file))))
    (when helm-ag--buffer-search
      (setq args (append args (helm-ag--file-visited-buffers))))
    (when helm-ag--default-target
      (setq args (append args (helm-ag--construct-targets helm-ag--default-target))))
    (cons command args)))

(defun helm-ag--remove-carrige-returns ()
  "Not documented."
  (when (helm-ag--windows-p)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\xd" nil t)
        (replace-match "")))))

(defun helm-ag--abbreviate-file-name ()
  "Not documented."
  (unless (helm-ag--windows-p)
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (while (re-search-forward "^\\([^:]+\\)" nil t)
        (replace-match (abbreviate-file-name (match-string-no-properties 1)))))))

(defun helm-ag--command-succeeded-p (exit-status)
  "Not documented, EXIT-STATUS."
  (cond ((integerp helm-ag-success-exit-status) (= exit-status helm-ag-success-exit-status))
        ((consp helm-ag-success-exit-status) (member exit-status helm-ag-success-exit-status))
        (t (zerop exit-status))))

(defun helm-ag--init ()
  "Not documented."
  (let ((buf-coding buffer-file-coding-system))
    (helm-attrset 'recenter t)
    (with-current-buffer (helm-candidate-buffer 'global)
      (let* ((default-directory (or helm-ag--default-directory
                                    default-directory))
             (cmds (helm-ag--construct-command (helm-attr 'search-this-file)))
             (coding-system-for-read buf-coding)
             (coding-system-for-write buf-coding))
        (setq helm-ag--ignore-case (helm-ag--ignore-case-p cmds helm-ag--last-query)
              helm-ag--last-command cmds)
        (let ((ret (apply #'process-file (car cmds) nil t nil (cdr cmds))))
          (if (zerop (length (buffer-string)))
              (error "No ag output: '%s'" helm-ag--last-query)
            (unless (helm-ag--command-succeeded-p ret)
              (unless (executable-find (car cmds))
                (error "'%s' is not installed" (car cmds)))
              (error "Failed: '%s'" helm-ag--last-query))))
        (when helm-ag--buffer-search
          (helm-ag--abbreviate-file-name))
        (helm-ag--remove-carrige-returns)
        (helm-ag--save-current-context)))))

(add-to-list 'debug-ignored-errors "^No ag output: ")

(defun helm-ag--search-only-one-file-p ()
  "Not documented."
  (when (and helm-ag--default-target (= (length helm-ag--default-target) 1))
    (let ((target (car helm-ag--default-target)))
      (unless (file-directory-p target)
        target))))

(defun helm-ag--find-file-action (candidate find-func this-file &optional persistent)
  "Not documented, CANDIDATE, FIND-FUNC, THIS-FILE, PERSISTENT."
  (when (memq 'pt helm-ag--command-features)
    ;; 'pt' always show filename if matched file is only one.
    (setq this-file nil))
  (let* ((file-line (helm-grep-split-line candidate))
         (filename (or this-file (cl-first file-line) candidate))
         (line (if this-file
                   (cl-first (split-string candidate ":"))
                 (cl-second file-line)))
         (default-directory (or helm-ag--default-directory
                                helm-ag--last-default-directory
                                default-directory)))
    (unless persistent
      (setq helm-ag--last-default-directory default-directory))
    (funcall find-func filename)
    (goto-char (point-min))
    (when line
      (forward-line (1- (string-to-number line))))
    (ignore-errors
      (and (re-search-forward helm-ag--last-query (line-end-position) t)
           ;; `helm-goto-char' expands folded headings/outlines if needed
           (helm-goto-char (match-beginning 0))))))

(defun helm-ag--open-file-with-temp-buffer (filename)
  "Not documented, FILENAME."
  (let ((search-directory default-directory))
    (switch-to-buffer (get-buffer-create " *helm-ag persistent*"))
    (setq default-directory search-directory
          buffer-read-only nil)
    (fundamental-mode)
    (erase-buffer)
    (insert-file-contents filename)
    (let ((buffer-file-name filename))
      (set-auto-mode)
      (font-lock-fontify-region (point-min) (point-max)))))

(defsubst helm-ag--vimgrep-option ()
  "Not documented."
  (member "--vimgrep" helm-ag--last-command))

(defun helm-ag--search-this-file-p ()
  "Not documented."
  (unless (helm-ag--vimgrep-option)
    (if (eq (helm-get-current-source) 'helm-source-do-ag)
        (helm-ag--search-only-one-file-p)
      (helm-attr 'search-this-file))))

(defun helm-ag--persistent-action (candidate)
  "Not documented, CANDIDATE."
  (let ((find-func (if helm-ag-use-temp-buffer
                       #'helm-ag--open-file-with-temp-buffer
                     #'find-file))
        (helm-ag-p (assoc-default 'real-to-display (helm-get-current-source))))
    (helm-ag--find-file-action candidate find-func (helm-ag--search-this-file-p) t)
    (let ((helm-input (if helm-ag-p
                          (concat helm-ag--last-query " " helm-input)
                        helm-input)))
      (helm-highlight-current-line))))

(defun helm-ag--validate-regexp (regexp)
  "Not documented, REGEXP."
  (condition-case nil
      (progn
        (string-match-p regexp "")
        t)
    (invalid-regexp nil)))

(defun helm-ag--convert-to-elisp-regexp (regexp)
  "Not documented, REGEXP."
  ;; This is very simple conversion
  (with-temp-buffer
    (insert regexp)
    (goto-char (point-min))
    ;; convert (, ), {, }, |
    (while (re-search-forward "[(){}|]" nil t)
      (backward-char 1)
      (cond ((looking-back "\\\\\\\\" nil))
            ((looking-back "\\\\" nil)
             (delete-char -1))
            (t
             (insert "\\")))
      (forward-char 1))
    ;; convert \s and \S -> \s- \S-
    (goto-char (point-min))
    (while (re-search-forward "\\(\\\\s\\)" nil t)
      (unless (looking-back "\\\\\\\\s" nil)
        (insert "-")))
    (buffer-string)))

(defun helm-ag--elisp-regexp-to-pcre (regexp)
  "Not documented, REGEXP."
  (with-temp-buffer
    (insert regexp)
    (goto-char (point-min))
    (while (re-search-forward "[(){}|]" nil t)
      (backward-char 1)
      (cond ((looking-back "\\\\\\\\" nil))
            ((looking-back "\\\\" nil)
             (delete-char -1))
            (t
             (insert "\\")))
      (forward-char 1))
    (buffer-string)))

(defun helm-ag--highlight-candidate (candidate)
  "Not documented, CANDIDATE."
  (let ((limit (1- (length candidate)))
        (last-pos 0)
        (case-fold-search helm-ag--ignore-case))
    (when helm-ag--valid-regexp-for-emacs
      (while (and (< last-pos limit)
                  (string-match helm-ag--elisp-regexp-query candidate last-pos))
        (let ((start (match-beginning 0))
              (end (match-end 0)))
          (if (= start end)
              (cl-incf last-pos)
            (put-text-property start end 'face 'helm-match candidate)
            (setq last-pos (1+ (match-end 0)))))))
    candidate))

(defun helm-ag--candidate-transform-for-this-file (candidate)
  "Not documented, CANDIDATE."
  (when (string-match "\\`\\([^:]+\\):\\(.*\\)" candidate)
    (format "%s:%s"
            (propertize (match-string 1 candidate) 'face 'helm-grep-lineno)
            (helm-ag--highlight-candidate (match-string 2 candidate)))))

(defun helm-ag--candidate-transform-for-files (candidate)
  "Not documented, CANDIDATE."
  (helm-aif (helm-grep-split-line candidate)
            (format "%s:%s:%s"
                    (propertize (cl-first it) 'face 'helm-moccur-buffer)
                    (propertize (cl-second it) 'face 'helm-grep-lineno)
                    (helm-ag--highlight-candidate (cl-third it)))))

(defun helm-ag--candidate-transformer (candidate)
  "Not documented, CANDIDATE."
  (or (if (helm-attr 'search-this-file)
          (helm-ag--candidate-transform-for-this-file candidate)
        (helm-ag--candidate-transform-for-files candidate))
      candidate))

(defun helm-ag--action-find-file (candidate)
  "Not documented, CANDIDATE."
  (helm-ag--find-file-action candidate 'find-file (helm-ag--search-this-file-p)))

(defun helm-ag--action-find-file-other-window (candidate)
  "Not documented, CANDIDATE."
  (helm-ag--find-file-action candidate 'find-file-other-window (helm-ag--search-this-file-p)))

(defvar helm-ag--actions
  (helm-make-actions
   "Open file"              #'helm-ag--action-find-file
   "Open file other window" #'helm-ag--action-find-file-other-window
   "Save results in buffer" #'helm-ag--action-save-buffer
   "Edit search results"    #'helm-ag--edit)
  "Not documented.")

(defvar helm-ag-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") 'helm-ag--run-other-window-action)
    (define-key map (kbd "C-l") 'helm-ag--up-one-level)
    (define-key map (kbd "C-c C-e") 'helm-ag-edit)
    (define-key map (kbd "C-x C-s") 'helm-ag--run-save-buffer)
    (define-key map (kbd "C-c ?") 'helm-ag-help)
    (define-key map (kbd "C-c >") 'helm-ag--next-file)
    (define-key map (kbd "<right>") 'helm-ag--next-file)
    (define-key map (kbd "C-c <") 'helm-ag--previous-file)
    (define-key map (kbd "<left>") 'helm-ag--previous-file)
    map)
  "Keymap for `helm-ag'.")

(defvar helm-ag-source
  (helm-build-in-buffer-source "The Silver Searcher"
                               :init 'helm-ag--init
                               :real-to-display 'helm-ag--candidate-transformer
                               :persistent-action 'helm-ag--persistent-action
                               :fuzzy-match helm-ag-fuzzy-match
                               :action helm-ag--actions
                               :candidate-number-limit 9999
                               :keymap helm-ag-map
                               :follow (and helm-follow-mode-persistent 1))
  "Not documented.")

;;;###autoload
(defun helm-ag-pop-stack ()
  "Not documented."
  (interactive)
  (let ((context (pop helm-ag--context-stack)))
    (unless context
      (error "Context stack is empty !"))
    (helm-aif (plist-get context :file)
              (find-file it)
              (let ((buf (plist-get context :buffer)))
                (if (buffer-live-p buf)
                    (switch-to-buffer buf)
                  (error "The buffer is already killed"))))
    (goto-char (plist-get context :point))))

;;;###autoload
(defun helm-ag-clear-stack ()
  "Not documented."
  (interactive)
  (setq helm-ag--context-stack nil))

(defun helm-ag--marked-input (escape)
  "Not documented, ESCAPE."
  (when (use-region-p)
    (let ((input (buffer-substring-no-properties (region-beginning) (region-end))))
      (deactivate-mark)
      (if (not escape)
          input
        (replace-regexp-in-string " " "\\\\ " input)))))

(defun helm-ag--query (&optional query)
  "Not documented, QUERY."
  (let* ((searched-word (helm-ag--searched-word))
         (marked-word (helm-ag--marked-input nil))
         (query (or query
                    (read-from-minibuffer "Pattern: "
                                          (or marked-word searched-word)
                                          nil
                                          nil
                                          'helm-ag--command-history
                                          (helm-aif (symbol-at-point)
                                                    (symbol-name it))))))
    (when (string-empty-p query)
      (error "Input is empty!!"))
    (setq helm-ag--last-query query)))

(defsubst helm-ag--init-state ()
  "Not documented."
  (setq helm-ag--original-window (selected-window)
        helm-ag--last-default-directory nil))

(defun helm-ag--get-default-directory ()
  "Not documented."
  (let ((prefix-val (and current-prefix-arg (abs (prefix-numeric-value current-prefix-arg)))))
    (cond ((not prefix-val) default-directory)
          ((= prefix-val 4)
           (file-name-as-directory
            (read-directory-name "Search directory: " nil nil t)))
          ((= prefix-val 16)
           (let ((dirs (list (read-directory-name "Search directory: " nil nil t))))
             (while (y-or-n-p "More directories ? ")
               (push (read-directory-name "Search directory: " nil nil t) dirs))
             (reverse dirs))))))

(defsubst helm-ag--helm-header (dir)
  "Not documented, DIR."
  (if helm-ag--buffer-search
      "Search Buffers"
    (concat "Search at " (abbreviate-file-name dir))))

(defun helm-ag--run-other-window-action ()
  "Not documented."
  (interactive)
  (with-helm-alive-p
   (helm-exit-and-execute-action #'helm-ag--action-find-file-other-window)))

(defun helm-ag--exit-from-edit-mode ()
  "Not documented."
  (when (window-live-p helm-ag--original-window)
    (select-window helm-ag--original-window))
  (kill-buffer (get-buffer "*helm-ag-edit*")))

(defun helm-ag--match-line-regexp ()
  "Not documented."
  ;; $1: file name
  ;; $2: line
  ;; $3: match body
  ;; $4: file attributes part(filename, line, column)
  (cond ((helm-ag--vimgrep-option)
         "^\\(?4:\\(?1:[^:]+\\):\\(?2:[1-9][0-9]*\\):[^:]+:\\)\\(?3:.*\\)$")
        (helm-ag--search-this-file-p
         "^\\(?4:\\(?2:[1-9][0-9]*\\)[:-]\\)\\(?3:.*\\)$")
        (t
         "^\\(?4:\\(?1:[^:]+\\):\\(?2:[1-9][0-9]*\\)[:-]\\)\\(?3:.*\\)$")))

(defun helm-ag--edit-commit ()
  "Not documented."
  (interactive)
  (goto-char (point-min))
  (let ((read-only-files 0)
        (files-to-lines (make-hash-table :test #'equal))
        (regexp (helm-ag--match-line-regexp))
        (line-deletes (make-hash-table :test #'equal)))
    ;; Group changes by file
    (while (re-search-forward regexp nil t)
      (let* ((file (or (match-string-no-properties 1) helm-ag--search-this-file-p))
             (line (string-to-number (match-string-no-properties 2)))
             (body (match-string-no-properties 3))
             (ovs (overlays-at (line-beginning-position)))
             (lines-list (gethash file files-to-lines)))
        (if (not (file-writable-p file))
            (cl-incf read-only-files)
          (if lines-list
              (progn
                (push (list line body ovs) lines-list)
                (puthash file lines-list files-to-lines))
            (puthash file (list (list line body ovs)) files-to-lines)))))
    ;; Batch edits by file
    (maphash
     (lambda (curr-file lines-data)
       (with-temp-buffer
         (insert-file-contents curr-file)
         (dolist (curr-line-data (reverse lines-data))
           (cl-destructuring-bind
               (line body ovs) curr-line-data
             (goto-char (point-min))
             (let ((deleted-lines (gethash curr-file line-deletes 0))
                   (deleted (and ovs (overlay-get (car ovs) 'helm-ag-deleted))))
               (forward-line (- line 1 deleted-lines))
               (delete-region (line-beginning-position) (line-end-position))
               (if (not deleted)
                   (insert body)
                 (let ((beg (point)))
                   (forward-line 1)
                   (delete-region beg (point))
                   (puthash curr-file (1+ deleted-lines) line-deletes))))))
         (when helm-ag-edit-save
           (write-region (point-min) (point-max) curr-file))))
     files-to-lines)
    ;; Finish
    (helm-ag--exit-from-edit-mode)
    (if (not (zerop read-only-files))
        (message "%d files are read-only and not editable." read-only-files)
      (message "Success update"))))

(defun helm-ag--edit-abort ()
  "Not documented."
  (interactive)
  (when (y-or-n-p "Discard changes ? ")
    (helm-ag--exit-from-edit-mode)
    (message "Abort edit")))

(defun helm-ag--mark-line-deleted ()
  "Not documented."
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (ov (make-overlay beg end)))
    (overlay-put ov 'face 'helm-ag-edit-deleted-line)
    (overlay-put ov 'helm-ag-deleted t)))

(defun helm-ag--unmark ()
  "Not documented."
  (interactive)
  (dolist (ov (overlays-in (line-beginning-position) (line-end-position)))
    (when (overlay-get ov 'helm-ag-deleted)
      (delete-overlay ov))))

(defvar helm-ag-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'helm-ag--edit-commit)
    (define-key map (kbd "C-c C-k") 'helm-ag--edit-abort)
    (define-key map (kbd "C-c C-d") 'helm-ag--mark-line-deleted)
    (define-key map (kbd "C-c C-u") 'helm-ag--unmark)
    map)
  "Not documented.")

(defsubst helm-ag--edit-func-to-keys (func)
  "Not documented, FUNC."
  (key-description (car-safe (where-is-internal func helm-ag-edit-map))))

(defun helm-ag--edit (_candidate)
  "Not documented."
  (let* ((helm-buf-dir (or helm-ag--default-directory
                           helm-ag--last-default-directory
                           default-directory))
         (default-directory helm-buf-dir))
    (with-current-buffer (get-buffer-create "*helm-ag-edit*")
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq-local helm-ag--default-directory helm-buf-dir)
      (unless (helm-ag--vimgrep-option)
        (setq-local helm-ag--search-this-file-p
                    (assoc-default 'search-this-file (helm-get-current-source))))
      (let (buf-content)
        (with-current-buffer (get-buffer "*helm-ag*")
          (goto-char (point-min))
          (forward-line 1)
          (let* ((body-start (point))
                 (marked-lines (cl-loop for ov in (overlays-in body-start (point-max))
                                        when (eq 'helm-visible-mark (overlay-get ov 'face))
                                        return (helm-marked-candidates))))
            (if (not marked-lines)
                (setq buf-content (buffer-substring-no-properties
                                   body-start (point-max)))
              (setq buf-content (concat (string-join marked-lines "\n") "\n")))))
        (insert buf-content)
        (add-text-properties (point-min) (point-max)
                             '(read-only t rear-nonsticky t front-sticky t))
        (let ((inhibit-read-only t)
              (regexp (helm-ag--match-line-regexp)))
          (setq header-line-format
                (format "[%s] %s: Commit, %s: Abort"
                        (abbreviate-file-name helm-ag--default-directory)
                        (helm-ag--edit-func-to-keys #'helm-ag--edit-commit)
                        (helm-ag--edit-func-to-keys #'helm-ag--edit-abort)))
          (goto-char (point-min))
          (while (re-search-forward regexp nil t)
            (let ((file-line-begin (match-beginning 4))
                  (file-line-end (match-end 4))
                  (body-begin (match-beginning 3))
                  (body-end (match-end 3)))
              (add-text-properties file-line-begin file-line-end
                                   '(face font-lock-function-name-face
                                          intangible t))
              (remove-text-properties body-begin body-end '(read-only t))
              (set-text-properties body-end (1+ body-end)
                                   '(read-only t rear-nonsticky t))))))))
  (other-window 1)
  (switch-to-buffer (get-buffer "*helm-ag-edit*"))
  (goto-char (point-min))
  (setq next-error-function 'compilation-next-error-function)
  (setq-local compilation-locs (make-hash-table :test 'equal :weakness 'value))
  (use-local-map helm-ag-edit-map))

(defun helm-ag-edit ()
  "Not documented."
  (interactive)
  (helm-exit-and-execute-action 'helm-ag--edit))

(defconst helm-ag--help-message
  "\n* Helm Ag\n

\n** Specific commands for Helm Ag:\n
\\<helm-ag-map>
\\[helm-ag--run-other-window-action]\t\t-> Open result in other buffer
\\[helm-ag--up-one-level]\t\t-> Search in parent directory.
\\[helm-ag-edit]\t\t-> Edit search results.
\\[helm-ag-help]\t\t-> Show this help.
\n** Helm Ag Map\n
\\{helm-map}"
  "Not documented.")

(defun helm-ag-help ()
  "Not documented."
  (interactive)
  (let ((helm-help-message helm-ag--help-message))
    (helm-help)))

(defun helm-ag-mode-jump ()
  "Not documented."
  (interactive)
  (let ((line (helm-current-line-contents)))
    (helm-ag--find-file-action line 'find-file helm-ag--search-this-file-p)))

(defun helm-ag-mode-jump-other-window ()
  "Not documented."
  (interactive)
  (let ((line (helm-current-line-contents)))
    (helm-ag--find-file-action line 'find-file-other-window helm-ag--search-this-file-p)))

(defvar helm-ag-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'helm-ag-mode-jump)
    (define-key map (kbd "C-o") 'helm-ag-mode-jump-other-window)
    (define-key map (kbd "g") 'helm-ag--update-save-results)
    map)
  "Not documented.")

(define-derived-mode helm-ag-mode special-mode "helm-ag"
  "Major mode to provide actions in helm grep saved buffer.

Special commands:
\\{helm-ag-mode-map}")

(defun helm-ag--put-result-in-save-buffer (result search-this-file-p)
  "Not documented, RESULT, SEARCH-THIS-FILE-P."
  (setq buffer-read-only t)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "-*- mode: helm-ag -*-\n\n"
            (format "Ag Results for `%s':\n\n" helm-ag--last-query))
    (save-excursion
      (insert result)))
  (helm-ag-mode)
  (unless (helm-ag--vimgrep-option)
    (setq-local helm-ag--search-this-file-p search-this-file-p))
  (setq-local helm-ag--default-directory default-directory))

(defun helm-ag--save-results (use-other-buf)
  "Not documented, USE-OTHER-BUF."
  (let* ((search-this-file-p nil)
         (result (with-current-buffer helm-buffer
                   (goto-char (point-min))
                   (forward-line 1)
                   (buffer-substring (point) (point-max))))
         (default-directory helm-ag--default-directory)
         (buf (if use-other-buf
                  (read-string "Results buffer name: "
                               (format "*helm ag results for '%s'*" helm-ag--last-query))
                "*helm ag results*")))
    (when (buffer-live-p (get-buffer buf))
      (kill-buffer buf))
    (with-current-buffer (get-buffer-create buf)
      (helm-ag--put-result-in-save-buffer result search-this-file-p)
      (pop-to-buffer buf)
      (message "Helm Ag Results saved in `%s' buffer" buf))))

(defun helm-ag--update-save-results ()
  "Not documented."
  (interactive)
  (let* ((default-directory helm-ag--default-directory)
         (result (with-temp-buffer
                   (apply #'process-file (car helm-ag--last-command) nil t nil
                          (cdr helm-ag--last-command))
                   (helm-ag--remove-carrige-returns)
                   (when helm-ag--buffer-search
                     (helm-ag--abbreviate-file-name))
                   (helm-ag--propertize-candidates helm-ag--last-query)
                   (buffer-string))))
    (helm-ag--put-result-in-save-buffer result helm-ag--search-this-file-p)
    (message "Update Results")))

(defun helm-ag--action-save-buffer (_arg)
  "Not documented."
  (helm-ag--save-results nil))

(defun helm-ag--run-save-buffer ()
  "Not documented."
  (interactive)
  (let ((use-other-buf-p current-prefix-arg))
    (with-helm-alive-p
     (helm-exit-and-execute-action
      (lambda (_arg)
        (helm-ag--save-results use-other-buf-p))))))

(defun helm-ag--file-of-current-file ()
  "Not documented."
  (let ((line (helm-current-line-contents)))
    (when (string-match helm-grep-split-line-regexp line)
      (match-string-no-properties 1 line))))

(defun helm-ag--move-file-common (pred move-fn wrap-fn)
  "Not documented, PRED, MOVE-FN, WRAP-FN."
  (with-helm-window
   (let ((file (helm-ag--file-of-current-file)))
     (funcall move-fn)
     (while (and (not (funcall pred)) (string= file (helm-ag--file-of-current-file)))
       (funcall move-fn))
     (when (funcall pred)
       (funcall wrap-fn)))))

(defun helm-ag--previous-file ()
  "Not documented."
  (interactive)
  (helm-ag--move-file-common
   #'helm-beginning-of-source-p #'helm-previous-line #'helm-end-of-buffer))

(defun helm-ag--next-file ()
  "Not documented."
  (interactive)
  (helm-ag--move-file-common
   #'helm-end-of-source-p #'helm-next-line #'helm-beginning-of-buffer))

(defsubst helm-ag--root-directory-p ()
  "Not documented."
  (cl-loop for dir in '(".git/" ".hg/")
           thereis (file-directory-p dir)))

(defun helm-ag--up-one-level ()
  "Not documented."
  (interactive)
  (if (or (not (helm-ag--root-directory-p))
          (y-or-n-p "Current directory might be the project root.  \
Continue searching the parent directory? "))
      (let ((parent (file-name-directory (directory-file-name default-directory))))
        (helm-run-after-exit
         (lambda ()
           (let* ((default-directory parent)
                  (helm-ag--default-directory parent))
             (setq helm-ag--last-default-directory default-directory)
             (helm-attrset 'name (helm-ag--helm-header default-directory) helm-ag-source)
             (helm :sources '(helm-ag-source) :buffer "*helm-ag*" :keymap helm-ag-map
                   :history 'helm-ag--helm-history)))))
    (message nil)))

;;;###autoload
(defun helm-ag-this-file (&optional query)
  "Do ag with in this file with QUERY."
  (interactive)
  (helm-ag--init-state)
  (let ((filename (file-name-nondirectory (buffer-file-name)))
        (helm-ag--default-directory default-directory))
    (helm-ag--query query)
    (helm-ag--set-command-features)
    (helm-attrset 'search-this-file (file-relative-name (buffer-file-name))
                  helm-ag-source)
    (helm-attrset 'name (format "Search at %s" filename) helm-ag-source)
    (helm :sources '(helm-ag-source) :buffer "*helm-ag*" :keymap helm-ag-map
          :history 'helm-ag--helm-history)))

;;;###autoload
(defun helm-ag (&optional basedir query)
  "Do ag with in BASEDIR and with QUERY."
  (interactive)
  (helm-ag--init-state)
  (let ((dir (helm-ag--get-default-directory))
        targets)
    (when (listp dir)
      (setq basedir default-directory
            targets dir))
    (let ((helm-ag--default-directory (or basedir dir))
          (helm-ag--default-target targets))
      (helm-ag--query query)
      (helm-attrset 'search-this-file nil helm-ag-source)
      (helm-attrset 'name (helm-ag--helm-header helm-ag--default-directory) helm-ag-source)
      (helm :sources '(helm-ag-source) :buffer "*helm-ag*" :keymap helm-ag-map
            :history 'helm-ag--helm-history))))

(defun helm-ag--split-string (str)
  "Not documented, STR."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let ((prev (point))
          patterns)
      (while (search-forward " " nil 'move)
        (cond ((looking-back "\\\\\\\\ " nil)
               (push (buffer-substring-no-properties prev (1- (point))) patterns)
               (skip-chars-forward " ")
               (setq prev (point)))
              ((looking-back "\\\\ " nil)
               (replace-match " "))
              (t (push (buffer-substring-no-properties prev (1- (point))) patterns)
                 (skip-chars-forward " ")
                 (setq prev (point)))))
      (push (buffer-substring-no-properties prev (point)) patterns)
      (reverse (cl-loop for p in patterns unless (string= p "") collect p)))))

(defsubst helm-ag--convert-invert-pattern (pattern)
  "Not documented, PATTERN."
  (when (and (memq 'pcre helm-ag--command-features)
             (string-prefix-p "!" pattern) (> (length pattern) 1))
    (concat "^(?!.*" (substring pattern 1) ").+$")))

(defun helm-ag--join-patterns (input)
  "Not documented, INPUT."
  (let ((patterns (helm-ag--split-string input)))
    (if (= (length patterns) 1)
        (or (helm-ag--convert-invert-pattern (car patterns))
            (car patterns))
      (cond ((memq 'pcre helm-ag--command-features)
             (cl-loop for s in patterns
                      if (helm-ag--convert-invert-pattern s)
                      concat (concat "(?=" it ")")
                      else
                      concat (concat "(?=.*" s ".*)")))
            ((memq 're2 helm-ag--command-features)
             (string-join patterns ".*"))
            ;; we don't know anything about this pattern
            (t input)))))

(defun helm-ag--do-ag-highlight-patterns (input)
  "Not documented, INPUT."
  (if (or (memq 'pcre helm-ag--command-features)
          (memq 're2 helm-ag--command-features))
      (cl-loop with regexp = (helm-ag--convert-to-elisp-regexp input)
               for pattern in (helm-ag--split-string regexp)
               when (helm-ag--validate-regexp pattern)
               collect pattern)
    (list (helm-ag--join-patterns input))))

(defun helm-ag--propertize-candidates (input)
  "Not documented, INPUT."
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (let ((patterns (helm-ag--do-ag-highlight-patterns input)))
      (cl-loop with one-file-p = (and (not (helm-ag--vimgrep-option))
                                      (helm-ag--search-only-one-file-p))
               while (not (eobp))
               for num = 1 then (1+ num)
               do
               (progn
                 (let ((start (point))
                       (bound (line-end-position)))
                   (if (and one-file-p (search-forward ":" bound t))
                       (set-text-properties (line-beginning-position) (1- (point))
                                            '(face helm-grep-lineno))
                     (when (re-search-forward helm-grep-split-line-regexp bound t)
                       (set-text-properties (match-beginning 1) (match-end 1) '(face helm-moccur-buffer))
                       (set-text-properties (match-beginning 2) (match-end 2) '(face helm-grep-lineno))
                       (goto-char (match-beginning 3))))
                   (let ((curpoint (point))
                         (case-fold-search helm-ag--ignore-case))
                     (dolist (pattern patterns)
                       (let ((last-point (point)))
                         (while (and (< (point) bound)
                                     (re-search-forward pattern bound t))
                           (set-text-properties (match-beginning 0) (match-end 0)
                                                '(face helm-match))
                           (when (= last-point (point))
                             (forward-char 1))
                           (setq last-point (point)))
                         (goto-char curpoint))))
                   (put-text-property start bound 'helm-cand-num num))
                 (forward-line 1))))))

(defun helm-ag-show-status-default-mode-line ()
  "Not documented."
  (setq mode-line-format
        '(" " mode-line-buffer-identification " "
          (:eval (propertize
                  (format
                   "[AG process finished - (%s results)] "
                   (helm-get-candidate-number))
                  'face 'helm-grep-finish)))))

(defun helm-ag--do-ag-propertize (input)
  "Not documented, INPUT."
  (with-helm-window
   (helm-ag--remove-carrige-returns)
   (when helm-ag--buffer-search
     (helm-ag--abbreviate-file-name))
   (helm-ag--propertize-candidates input)
   (when helm-ag-show-status-function
     (funcall helm-ag-show-status-function)
     (force-mode-line-update))))

(defun helm-ag--construct-extension-options ()
  "Not documented."
  (cl-loop for ext in helm-do-ag--extensions
           unless (string= ext "*")
           collect
           (concat "-G" (replace-regexp-in-string
                         "\\*" ""
                         (replace-regexp-in-string "\\." "\\\\." ext)))))

(defun helm-ag--show-result-p (options has-query)
  "Not documented, OPTIONS, HAS-QUERY."
  (or has-query
      (cl-loop for opt in options
               thereis (string-prefix-p "-g" opt))))

(defun helm-ag--construct-do-ag-command (pattern)
  "Not documented, PATTERN."
  (let* ((opt-query (helm-ag--parse-options-and-query pattern))
         (options (car opt-query))
         (query (cdr opt-query))
         (has-query (not (string= query ""))))
    (when helm-ag-use-emacs-lisp-regexp
      (setq query (helm-ag--elisp-regexp-to-pcre query)))
    (when (helm-ag--show-result-p options has-query)
      (append (car helm-do-ag--commands)
              options
              (and has-query (list (helm-ag--join-patterns query)))
              (cdr helm-do-ag--commands)))))

(defun helm-ag--do-ag-set-command ()
  "Not documented."
  (let ((cmd-opts (split-string helm-ag-base-command nil t)))
    (when helm-ag-command-option
      (setq cmd-opts (append cmd-opts (split-string helm-ag-command-option nil t))))
    (when helm-ag--extra-options
      (setq cmd-opts (append cmd-opts (split-string helm-ag--extra-options))))
    (when helm-ag-ignore-patterns
      (setq cmd-opts
            (append cmd-opts
                    (mapcar #'helm-ag--construct-ignore-option
                            helm-ag-ignore-patterns))))
    (when helm-ag-use-agignore
      (helm-aif (helm-ag--root-agignore)
                (setq cmd-opts (append cmd-opts (list "-p" it)))))
    (when helm-do-ag--extensions
      (setq cmd-opts (append cmd-opts (helm-ag--construct-extension-options))))
    (when helm-ag-use-grep-ignore-list
      (setq cmd-opts (append cmd-opts (helm-ag--grep-ignore-list-to-options))))
    (let (targets)
      (when helm-ag--buffer-search
        (setq targets (helm-ag--file-visited-buffers)))
      (setq helm-do-ag--commands
            (cons cmd-opts
                  (if helm-ag--default-target
                      (append targets (helm-ag--construct-targets helm-ag--default-target))
                    targets))))))

(defun helm-ag--do-ag-candidate-process (dir)
  "Not documented, DIR."
  (let* ((non-essential nil)
         (default-directory dir)
         (cmd-args (helm-ag--construct-do-ag-command helm-pattern)))
    (when cmd-args
      (let ((proc (apply #'start-file-process "helm-do-ag" nil cmd-args)))
        (setq helm-ag--last-query helm-pattern
              helm-ag--last-command cmd-args
              helm-ag--ignore-case (helm-ag--ignore-case-p cmd-args helm-pattern)
              helm-ag--last-default-directory default-directory)
        (prog1 proc
          (set-process-sentinel
           proc
           (lambda (process event)
             (helm-process-deferred-sentinel-hook
              process event (helm-default-directory))
             (when (string= event "finished\n")
               (helm-ag--do-ag-propertize helm-input)))))))))

(defconst helm-do-ag--help-message
  "\n* Helm Do Ag\n

\n** Specific commands for Helm Ag:\n
\\<helm-do-ag-map>
\\[helm-ag--run-other-window-action]\t\t-> Open result in other buffer
\\[helm-ag--do-ag-up-one-level]\t\t-> Search in parent directory.
\\[helm-ag-edit]\t\t-> Edit search results.
\\[helm-ag--do-ag-help]\t\t-> Show this help.
\n** Helm Ag Map\n
\\{helm-map}"
  "Not documented.")

(defun helm-ag--do-ag-help ()
  "Not documented."
  (interactive)
  (let ((helm-help-message helm-do-ag--help-message))
    (helm-help)))

(defvar helm-do-ag-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-ag-map)
    (define-key map (kbd "C-l") 'helm-ag--do-ag-up-one-level)
    (define-key map (kbd "C-c ?") 'helm-ag--do-ag-help)
    map)
  "Keymap for `helm-do-ag'.")

(defun helm-ag--highlight-string-matched (str patterns)
  "Not documented, STR, PATTERNS."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (dolist (pattern patterns)
      (let ((last-point (point)))
        (while (and (not (eobp)) (re-search-forward pattern nil t))
          (set-text-properties (match-beginning 0) (match-end 0)
                               '(face helm-match))
          (when (= last-point (point))
            (forward-char 1))
          (setq last-point (point)))))
    (buffer-string)))

(defun helm-ag--filter-one (candidate input)
  "Not documented, CANDIDATE, INPUT."
  (let ((patterns (helm-ag--do-ag-highlight-patterns input))
        (one-file-p (and (not (helm-ag--vimgrep-option))
                         (helm-ag--search-only-one-file-p))))
    (if one-file-p
        (if (string-match "^\\([^:]+\\):\\(.*\\)$" candidate)
            (cons (concat (propertize (match-string-no-properties 1 candidate)
                                      'face 'helm-grep-lineno)
                          ":"
                          (helm-ag--highlight-string-matched
                           (match-string-no-properties 2 candidate) patterns))
                  candidate)
          candidate)
      (let* ((split (helm-grep-split-line candidate))
             (file (nth 0 split))
             (lineno (nth 1 split))
             (str (nth 2 split)))
        (if (and lineno str)
            (cons (concat (propertize file 'face 'helm-moccur-buffer)
                          ":"
                          (propertize lineno 'face 'helm-grep-lineno)
                          ":"
                          (helm-ag--highlight-string-matched str patterns))
                  candidate)
          candidate)))))

(defun helm-do-ag--filter-one-by-one (candidate)
  "Not documented, CANDIDATE."
  (save-excursion
    (if (consp candidate)
        candidate
      (when (stringp candidate)
        (helm-ag--filter-one candidate helm-input)))))

(defclass helm-do-ag-class (helm-source-async)
  ((nohighlight :initform t)
   (keymap :initform helm-do-ag-map)
   (history :initform 'helm-ag--helm-history)
   (filter-one-by-one :initform 'helm-do-ag--filter-one-by-one)
   (candidate-number-limit :initform 99999)
   (requires-pattern :initform 3)
   (persistent-action :initform 'helm-ag--persistent-action)
   (nomark :initform nil)
   (action :initform 'helm-ag--actions))
  "Not documented.")

(defvar helm-source-do-ag nil)

(defun helm-ag--do-ag-set-source (dir &optional search-dir)
  "Not documented, DIR, SEARCH-DIR."
  (let ((search-dir (or search-dir dir)))
    (setq helm-source-do-ag
          (helm-make-source "AG" 'helm-do-ag-class
                            :candidates-process
                            (lambda ()
                              (helm-ag--do-ag-set-command)
                              (helm-ag--do-ag-candidate-process dir))
                            :header-name
                            (lambda (_name) (helm-ag--helm-header search-dir))
                            :follow (and helm-follow-mode-persistent 1)))))

(defun helm-ag--do-ag-up-one-level ()
  "Not documented."
  (interactive)
  (if (or (not (helm-ag--root-directory-p))
          (y-or-n-p "Current directory might be the project root.  \
Continue searching the parent directory? "))
      (let ((parent (file-name-directory (directory-file-name default-directory)))
            (initial-input helm-input))
        (helm-run-after-exit
         (lambda ()
           (let ((default-directory parent)
                 (helm-ag--default-directory parent))
             (setq helm-ag--last-default-directory default-directory)
             (helm-ag--do-ag-set-source default-directory)
             (helm :sources 'helm-source-do-ag :buffer "*helm-ag*"
                   :keymap helm-do-ag-map :input initial-input
                   :history 'helm-ag--helm-history)))))
    (message nil)))

(defun helm-ag--set-do-ag-option ()
  "Not documented."
  (if (or (< (prefix-numeric-value current-prefix-arg) 0)
          helm-ag-always-set-extra-option)
      (let ((option (read-string "Extra options: " (or helm-ag--extra-options "")
                                 'helm-ag--extra-options-history)))
        (setq helm-ag--extra-options option))
    (setq helm-ag--extra-options nil)))

(defun helm-ag--set-command-features ()
  "Not documented."
  (let ((cmd (intern (car (split-string helm-ag-base-command)))))
    (setq helm-ag--command-features (list cmd))
    (cl-case cmd
      (ack (add-to-list 'helm-ag--command-features
                        (if (string-match-p "-\\(?:Q\\|-literal\\)\\>" helm-ag-base-command)
                            'fixed
                          'pcre)))
      (ag (add-to-list 'helm-ag--command-features
                       (if (string-match-p "-\\(?:[QF]\\|-literal\\|-fixed-strings\\)\\>" helm-ag-base-command)
                           'fixed
                         'pcre)))
      (pt (add-to-list 'helm-ag--command-features
                       (if (string-match-p "-e\\>" helm-ag-base-command)
                           're2
                         'fixed)))
      (rg (add-to-list 'helm-ag--command-features
                       (if (string-match-p "-\\(?:F\\|-fixed-strings\\)\\>" helm-ag-base-command)
                           'fixed
                         (if (string-match-p "--pcre2\\>" helm-ag-base-command)
                             'pcre
                           're2)))))))

(defun helm-ag--do-ag-searched-extensions ()
  "Not documented."
  (when (and current-prefix-arg (= (abs (prefix-numeric-value current-prefix-arg)) 4))
    (helm-grep-get-file-extensions helm-ag--default-target)))

(defsubst helm-do-ag--target-one-directory-p (targets)
  "Not documented, TARGETS."
  (and (listp targets) (= (length targets) 1) (file-directory-p (car targets))))

(defun helm-do-ag--helm (default-input search-this-file)
  "Not documented, DEFAULT-INPUT, SEARCH-THIS-FILE."
  (let ((search-dir (if (not (helm-ag--windows-p))
                        helm-ag--default-directory
                      (if (helm-do-ag--target-one-directory-p helm-ag--default-target)
                          (car helm-ag--default-target))))
        (dir (or helm-ag--default-directory
                 helm-ag--last-default-directory
                 default-directory)))
    (helm-ag--do-ag-set-source dir search-dir)
    (helm-attrset 'search-this-file search-this-file helm-source-do-ag)
    (helm :sources 'helm-source-do-ag :buffer "*helm-ag*" :keymap helm-do-ag-map
          :input (or default-input (helm-ag--marked-input t)
                     (helm-ag--insert-thing-at-point helm-ag-insert-at-point))
          :history 'helm-ag--helm-history)))

;;;###autoload
(defun helm-do-ag-this-file (&optional query)
  "Not documented, QUERY."
  (interactive)
  (helm-aif (buffer-file-name)
            (helm-do-ag default-directory (list it) query)
            (error "Error: This buffer is not visited file")))

;;;###autoload
(defun helm-do-ag (&optional basedir targets default-input)
  "Not documented, BASEDIR, TARGETS, DEFAULT-INPUT."
  (interactive)
  (require 'helm-mode)
  (helm-ag--init-state)
  (let* ((helm-ag--default-directory (or basedir default-directory))
         (helm-ag--default-target (cond (targets targets)
                                        ((and (helm-ag--windows-p) basedir) (list basedir))
                                        (t
                                         (when (and (not basedir) (not helm-ag--buffer-search))
                                           (helm-read-file-name
                                            "Search in file(s): "
                                            :default default-directory
                                            :marked-candidates t :must-match t)))))
         (helm-do-ag--extensions (when helm-ag--default-target
                                   (helm-ag--do-ag-searched-extensions)))
         (one-directory-p (helm-do-ag--target-one-directory-p
                           helm-ag--default-target))
         (search-this-file (and (= (length helm-ag--default-target) 1)
                                (not (file-directory-p (car helm-ag--default-target)))
                                (car helm-ag--default-target))))
    (helm-ag--set-do-ag-option)
    (helm-ag--set-command-features)
    (helm-ag--save-current-context)
    (if (or (helm-ag--windows-p) (not one-directory-p)) ;; Path argument must be specified on Windows
        (helm-do-ag--helm default-input search-this-file)
      (let* ((helm-ag--default-directory
              (file-name-as-directory (car helm-ag--default-target)))
             (helm-ag--default-target nil))
        (helm-do-ag--helm default-input search-this-file)))))

(defun helm-ag--project-root ()
  "Not documented."
  (cl-loop for dir in '(".git/" ".hg/" ".svn/" ".git")
           when (locate-dominating-file default-directory dir)
           return it))

;;;###autoload
(defun helm-ag-project-root (&optional query)
  "Not documented, QUERY."
  (interactive)
  (let ((rootdir (helm-ag--project-root)))
    (unless rootdir
      (error "Could not find the project root.  Create a git, hg, or svn repository there first"))
    (helm-ag rootdir query)))

;;;###autoload
(defun helm-do-ag-project-root (&optional query)
  "Not documented, QUERY."
  (interactive)
  (let ((rootdir (helm-ag--project-root)))
    (unless rootdir
      (error "Could not find the project root.  Create a git, hg, or svn repository there first"))
    (helm-do-ag rootdir nil query)))

;;;###autoload
(defun helm-ag-buffers (&optional query)
  "Not documented, QUERY."
  (interactive)
  (let ((helm-ag--buffer-search t))
    (helm-ag nil query)))

;;;###autoload
(defun helm-do-ag-buffers (&optional query)
  "Not documented, QUERY."
  (interactive)
  (let ((helm-ag--buffer-search t))
    (helm-do-ag nil nil query)))

(provide 'helm-ag)

;;; helm-ag.el ends here
