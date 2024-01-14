;;; company-clang.el --- company-mode completion backend for Clang  -*- lexical-binding: t -*-

;; Copyright (C) 2009-2011, 2013-2023  Free Software Foundation, Inc.

;; Author: Nikolaj Schumacher

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'company)
(require 'company-template)
(require 'cl-lib)

(defgroup company-clang nil
  "Completion backend for Clang."
  :group 'company)

(defcustom company-clang-executable
  (executable-find "clang")
  "Location of clang executable."
  :type 'file)

(defcustom company-clang-begin-after-member-access t
  "When non-nil, start automatic completion after member access operators.

Automatic completion starts whenever the current symbol is preceded by
\".\", \"->\" or \"::\", ignoring `company-minimum-prefix-length'.

If `company-begin-commands' is a list, it should include `c-electric-lt-gt'
and `c-electric-colon', for automatic completion right after \">\" and
\":\"."
  :type 'boolean)

(defcustom company-clang-use-compile-flags-txt nil
  "When non-nil, use flags from compile_flags.txt if present.

The lines from that files will be appended to `company-clang-arguments'.

And if such file is found, Clang is called from the directory containing
it.  That allows the flags use relative file names within the project."
  :type 'boolean
  :safe 'booleanp)

(defcustom company-clang-arguments nil
  "A list of additional arguments to pass to clang when completing.
Prefix files (-include ...) can be selected with `company-clang-set-prefix'
or automatically through a custom `company-clang-prefix-guesser'."
  :type '(repeat (string :tag "Argument")))

(defcustom company-clang-prefix-guesser 'company-clang-guess-prefix
  "A function to determine the prefix file for the current buffer."
  :type '(function :tag "Guesser function" nil))

(defvar company-clang-modes '(c-mode c++-mode objc-mode)
  "Major modes which clang may complete.")

(defcustom company-clang-insert-arguments t
  "When non-nil, insert function arguments as a template after completion."
  :type 'boolean
  :package-version '(company . "0.8.0"))

;; prefix ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-clang--prefix nil)

(defsubst company-clang--guess-pch-file (file)
  (let ((dir (directory-file-name (file-name-directory file))))
    (when (equal (file-name-nondirectory dir) "Classes")
      (setq dir (file-name-directory dir)))
    (car (directory-files dir t "\\([^.]h\\|[^h]\\).pch\\'" t))))

(defsubst company-clang--file-substring (file beg end)
  (with-temp-buffer
    (insert-file-contents-literally file nil beg end)
    (buffer-string)))

(defun company-clang-guess-prefix ()
  "Try to guess the prefix file for the current buffer."
  ;; Prefixes seem to be called .pch.  Pre-compiled headers do, too.
  ;; So we look at the magic number to rule them out.
  (let* ((file (company-clang--guess-pch-file buffer-file-name))
         (magic-number (and file (company-clang--file-substring file 0 4))))
    (unless (member magic-number '("CPCH" "gpch"))
      file)))

(defun company-clang-set-prefix (&optional prefix)
  "Use PREFIX as a prefix (-include ...) file for clang completion."
  (interactive (let ((def (funcall company-clang-prefix-guesser)))
     (unless (stringp def)
       (setq def default-directory))
     (list (read-file-name "Prefix file: "
                           (when def (file-name-directory def))
                           def t (when def (file-name-nondirectory def))))))
  ;; TODO: pre-compile?
  (setq company-clang--prefix (and (stringp prefix)
                                   (file-regular-p prefix)
                                   prefix)))

;; Clean-up on exit.
(add-hook 'kill-emacs-hook 'company-clang-set-prefix)

;; parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Do we ever see OVERLOAD (or OVERRIDE)?
(defconst company-clang--completion-pattern
  "^COMPLETION: \\_<\\(%s[a-zA-Z0-9_:]*\\|Pattern\\)\\(?:\\(?: (InBase)\\)? : \\(.*\\)$\\)?$")

(defconst company-clang--error-buffer-name "*clang-error*")

(defun company-clang--lang-option ()
     (if (eq major-mode 'objc-mode)
         (if (string= "m" (file-name-extension buffer-file-name))
             "objective-c" "objective-c++")
       (substring (symbol-name major-mode) 0 -5)))

(defun company-clang--parse-output (prefix _objc)
  (goto-char (point-min))
  (let ((pattern (format company-clang--completion-pattern
                         (regexp-quote prefix)))
        (case-fold-search nil)
        (results (make-hash-table :test 'equal :size (/ (point-max) 100)))
        lines)
    (while (re-search-forward pattern nil t)
      (let ((match (match-string-no-properties 1))
            (meta (match-string-no-properties 2)))
        (when (equal match "Pattern")
          (setq match (company-clang--pattern-to-match meta)))
          (when (string-match ":" match)
            (setq match (substring match 0 (match-beginning 0))))
          ;; Avoiding duplicates:
          ;; https://github.com/company-mode/company-mode/issues/841
          (cond
           ;; Either meta != completion (not a macro)
           ((not (equal match meta))
            (puthash match meta results))
           ;; Or it's the first time we see this completion
           ((eq (gethash match results 'none) 'none)
            (puthash match nil results)))))
    (maphash
     (lambda (match meta)
       (when meta
         (put-text-property 0 1 'meta (company-clang--strip-formatting meta) match))
       (push match lines))
     results)
    lines))

(defun company-clang--pattern-to-match (pat)
  (let ((start 0)
        (end nil))
    (when (string-match "#]" pat)
      (setq start (match-end 0)))
    (when (string-match "[ \(]<#" pat start)
      (setq end (match-beginning 0)))
    (substring pat start end)))

(defun company-clang--meta (candidate)
  (get-text-property 0 'meta candidate))

(defun company-clang--annotation (candidate)
  (let ((ann (company-clang--annotation-1 candidate)))
    (if (not (and ann (string-prefix-p "(*)" ann)))
        ann
      (with-temp-buffer
        (insert ann)
        (search-backward ")")
        (let ((pt (1+ (point))))
          (re-search-forward ".\\_>" nil t)
          (delete-region pt (point)))
        (buffer-string)))))

;; TODO: Parse the original formatting here, rather than guess.
;; Strip it every time in the `meta' handler instead.
(defun company-clang--annotation-1 (candidate)
  (let ((meta (company-clang--meta candidate)))
    (cond
     ((null meta) nil)
     ((string-match "[^:]:[^:]" meta)
      (substring meta (1+ (match-beginning 0))))
     ((string-match "(anonymous)" meta) nil)
     ((string-match "\\((.*)[ a-z]*\\'\\)" meta)
      (let ((paren (match-beginning 1)))
        (if (not (eq (aref meta (1- paren)) ?>))
            (match-string 1 meta)
          (with-temp-buffer
            (insert meta)
            (goto-char paren)
            (substring meta (1- (search-backward "<"))))))))))

(defun company-clang--strip-formatting (text)
  (replace-regexp-in-string
   "#]" " "
   (replace-regexp-in-string "[<{[]#\\|#[>}]" "" text t)
   t))

(defun company-clang--handle-error (res args)
  (goto-char (point-min))
  (let* ((buf (get-buffer-create company-clang--error-buffer-name))
         (cmd (concat company-clang-executable " " (mapconcat 'identity args " ")))
         (pattern (format company-clang--completion-pattern ""))
         (message-truncate-lines t)
         (err (if (and (re-search-forward pattern nil t)
                       ;; Something in the Windows build?
                       ;; Looks like Clang doesn't always include the error text
                       ;; before completions (even if exited with error).
                       (> (match-beginning 0) (point-min)))
                  (buffer-substring-no-properties (point-min)
                                                  (1- (match-beginning 0)))
                ;; Warn the user more aggressively if no match was found.
                (message "clang failed with error %d: %s" res cmd)
                (buffer-string))))

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (current-time-string)
                (format "\nclang failed with error %d:\n" res)
                cmd "\n\n")
        (insert err)
        (setq buffer-read-only t)
        (goto-char (point-min))))))

(defun company-clang--start-process (prefix callback &rest args)
  (let* ((objc (derived-mode-p 'objc-mode))
         (buf (get-buffer-create "*clang-output*"))
         ;; Looks unnecessary in Emacs 25.1 and later.
         ;; (Inconclusive, needs more testing):
         ;; https://github.com/company-mode/company-mode/pull/288#issuecomment-72491808
         (process-adaptive-read-buffering nil)
         (existing-process (get-buffer-process buf)))
    (when existing-process
      (kill-process existing-process))
    (with-current-buffer buf
      (erase-buffer)
      (setq buffer-undo-list t))
    (let* ((process-connection-type nil)
           (process (apply #'start-file-process "company-clang" buf
                           company-clang-executable args)))
      (set-process-sentinel
       process
       (lambda (proc status)
         (unless (string-match-p "hangup\\|killed" status)
           (funcall
            callback
            (let ((res (process-exit-status proc)))
              (with-current-buffer buf
                (unless (eq 0 res)
                  (company-clang--handle-error res args))
                ;; Still try to get any useful input.
                (company-clang--parse-output prefix objc)))))))
      (unless (company-clang--auto-save-p)
        (send-region process (point-min) (point-max))
        (send-string process "\n")
        (process-send-eof process)))))

(defsubst company-clang--build-location (pos)
  (save-excursion
    (goto-char pos)
    (format "%s:%d:%d"
            (if (company-clang--auto-save-p) buffer-file-name "-")
            (line-number-at-pos)
            (1+ (length
                 (encode-coding-region
                  (line-beginning-position)
                  (point)
                  'utf-8
                  t))))))

(defsubst company-clang--build-complete-args (pos)
  (append '("-fsyntax-only" "-Xclang" "-code-completion-macros")
          (unless (company-clang--auto-save-p)
            (list "-x" (company-clang--lang-option)))
          (company-clang--arguments)
          (when (stringp company-clang--prefix)
            (list "-include" (expand-file-name company-clang--prefix)))
          (list "-Xclang" (format "-code-completion-at=%s"
                                  (company-clang--build-location pos)))
          (list (if (company-clang--auto-save-p) buffer-file-name "-"))))

(defun company-clang--arguments ()
  (let ((fname "compile_flags.txt")
        (args company-clang-arguments)
        current-dir-rel)
    (when company-clang-use-compile-flags-txt
      (let ((dir (locate-dominating-file default-directory fname)))
        (when dir
          (setq current-dir-rel (file-relative-name default-directory dir))
          (setq default-directory dir)
          (with-temp-buffer
            (insert-file-contents fname)
            (setq args
                  (append
                   args
                   (split-string (buffer-substring-no-properties
                                  (point-min) (point-max))
                                 "[\n\r]+"
                                 t
                                 "[ \t]+"))))
          (unless (equal current-dir-rel "./")
            (push (format "-I%s" current-dir-rel) args)))))
    args))

(defun company-clang--candidates (prefix callback)
  (and (company-clang--auto-save-p)
       (buffer-modified-p)
       (basic-save-buffer))
  (when (null company-clang--prefix)
    (company-clang-set-prefix (or (funcall company-clang-prefix-guesser)
                                  'none)))
  (let ((default-directory default-directory))
    (apply 'company-clang--start-process
           prefix
           callback
           (company-clang--build-complete-args
            (if (company-clang--check-version 4.0 9.0)
                (point)
              (- (point) (length prefix)))))))

(defun company-clang--prefix ()
  (if company-clang-begin-after-member-access
      (company-grab-symbol-cons "\\.\\|->\\|::" 2)
    (company-grab-symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst company-clang-required-version 1.1)

(defvar company-clang--version nil)

(defun company-clang--auto-save-p ()
  (not
   (company-clang--check-version 2.9 3.1)))

(defun company-clang--check-version (min apple-min)
  (pcase-exhaustive company-clang--version
    (`(apple . ,ver) (>= ver apple-min))
    (`(normal . ,ver) (>= ver min))))

(defsubst company-clang-version ()
  "Return the version of `company-clang-executable'."
  (with-temp-buffer
    (call-process company-clang-executable nil t nil "--version")
    (goto-char (point-min))
    (if (re-search-forward
         "\\(clang\\|Apple LLVM\\|bcc32x\\|bcc64\\) version \\([0-9.]+\\)" nil t)
        (cons
         (if (equal (match-string-no-properties 1) "Apple LLVM")
             'apple
           'normal)
         (string-to-number (match-string-no-properties 2)))
      0)))

(defun company-clang (command &optional arg &rest _ignored)
  "`company-mode' completion backend for Clang.
Clang is a parser for C and ObjC.  Clang version 1.1 or newer is required.

Additional command line arguments can be specified in
`company-clang-arguments'.  Prefix files (-include ...) can be selected
with `company-clang-set-prefix' or automatically through a custom
`company-clang-prefix-guesser'.

With Clang versions before 2.9, we have to save the buffer before
performing completion.  With Clang 2.9 and later, buffer contents are
passed via standard input."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-clang))
    (init (when (memq major-mode company-clang-modes)
            (unless company-clang-executable
              (error "Company found no clang executable"))
            (setq company-clang--version (company-clang-version))
            (unless (company-clang--check-version
                     company-clang-required-version
                     company-clang-required-version)
              (error "Company requires clang version %s"
                     company-clang-required-version))))
    (prefix (and (memq major-mode company-clang-modes)
                 buffer-file-name
                 company-clang-executable
                 (not (company-in-string-or-comment))
                 (or (company-clang--prefix) 'stop)))
    (candidates (cons :async
                      (lambda (cb) (company-clang--candidates arg cb))))
    (meta       (company-clang--meta arg))
    (kind (company-clang--kind arg))
    (annotation (company-clang--annotation arg))
    (post-completion (let ((anno (company-clang--annotation arg)))
                       (when (and company-clang-insert-arguments anno)
                         (insert anno)
                         (if (string-match "\\`:[^:]" anno)
                             (company-template-objc-templatify anno)
                           (company-template-c-like-templatify
                            (concat arg anno))))))))

(defun company-clang--kind (arg)
  ;; XXX: Not very precise.
  ;; E.g. it will say that an arg-less ObjC method is a variable (perhaps we
  ;; could look around for brackets, etc, if there any actual users who's
  ;; bothered by it).
  ;; And we can't distinguish between local vars and struct fields.
  ;; Or between keywords and macros.
  (let ((meta (company-clang--meta arg)))
    (cond
     ((null meta) 'keyword)
     ((string-match "(" meta)
      (if (string-match-p (format "\\`%s *\\'" (regexp-quote arg))
                          (substring meta 0 (match-beginning 0)))
          'keyword ; Also macro, actually (no return type).
        'function))
     (t 'variable))))

(provide 'company-clang)
;;; company-clang.el ends here
