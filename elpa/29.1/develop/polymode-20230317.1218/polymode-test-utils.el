;;; polymode-test-utils.el --- Testing utilities for polymode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2018-2022  Free Software Foundation, Inc.
;; Author: Vitalie Spinu
;; URL: https://github.com/polymode/polymode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;; Commentary:
;;
;; This file should be loaded only in tests.
;;
;;; Code:

(require 'ert)
(require 'polymode)
(require 'poly-lock)
(eval-when-compile
  (require 'cl-lib))

;; (require 'font-lock)
;; (global-font-lock-mode t)
;; (add-hook 'after-change-major-mode-hook #'global-font-lock-mode-enable-in-buffers)
;; (message "ACMH: %s  GFL:%s" after-change-major-mode-hook global-font-lock-mode)

(defvar pm-verbose nil)
(defvar pm-test-current-change-set nil)
(defun pm-test-get-file (name)
  "Find the file with NAME from inside a poly-xyz repo.
Look into tests/input directory then in samples directory."
  (let ((files (list (expand-file-name (format "./tests/input/%s" name) default-directory)
                     (expand-file-name (format "./input/%s" name) default-directory)
                     (expand-file-name (format "./samples/%s" name) default-directory)
                     (expand-file-name (format "../samples/%s" name) default-directory))))
    (or (cl-loop for f in files
                 if (file-exists-p f) return f)
        (error "No file with name '%s' found in '%s'" name default-directory))))

(defun pm-test-matcher (string span-alist matcher &optional dry-run)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let (prev-span)
      (when dry-run
        (message "("))
      (while (not (eobp))
        (if dry-run
            (let ((span (funcall matcher)))
              (unless (equal prev-span span)
                (setq prev-span span)
                (message " (%d . %S)" (nth 1 span) span)))
          (let* ((span (funcall matcher))
                 (sbeg (nth 1 span))
                 (ref-span (alist-get sbeg span-alist)))
            (unless (equal span ref-span)
              (ert-fail (list :pos (point) :span span :ref-span ref-span)))
            (when (and prev-span
                       (not (or (eq (nth 1 prev-span) sbeg)
                                (eq (nth 2 prev-span) sbeg))))
              (ert-fail (list :pos (point) :span span :prev-span prev-span)))
            (setq prev-span span)))
        (forward-char 1))
      (when dry-run
        (message ")"))
      nil)))

(defmacro pm-test-run-on-string (mode string &rest body)
  "Run BODY in a temporary buffer containing STRING in MODE.
MODE is a quoted symbol."
  (declare (indent 1) (debug (form form body)))
  `(let ((*buf* "*pm-test-string-buffer*"))
     (when (get-buffer *buf*)
       (kill-buffer *buf*))
     (with-current-buffer (get-buffer-create *buf*)
       (insert (substring-no-properties ,string))
       (let ((inhibit-message (not pm-verbose)))
         (funcall ,mode))
       (setq-default indent-tabs-mode nil)
       ;; In emacs 27 this is called from run-mode-hooks
       (and (bound-and-true-p syntax-propertize-function)
            (not (local-variable-p 'parse-sexp-lookup-properties))
            (setq-local parse-sexp-lookup-properties t))
       (goto-char (point-min))
       (let ((poly-lock-allow-background-adjustment nil))
         (when polymode-mode
           ;; font-lock not activated in batch mode
           (setq-local poly-lock-allow-fontification t)
           (poly-lock-mode t))
         (font-lock-ensure)
         ,@body)
       (current-buffer))))

(defun pm-test-spans (mode string)
  (declare (indent 1))
  (pm-test-run-on-string mode
    string
    (pm-map-over-spans
     (lambda (span)
       (let ((range0 (pm-span-to-range span)))
         (goto-char (car range0))
         (while (< (point) (cdr range0))
           (let ((range-pos (pm-innermost-range (point) 'no-cache)))
             (unless (equal range0 range-pos)
               (switch-to-buffer (current-buffer))
               (ert-fail (list :pos (point)
                               :range0 range0
                               :range-pos range-pos))))
           (forward-char)))))))

(defun pm-test-spans-on-file (mode file-name)
  (let ((file (pm-test-get-file file-name)))
    (pm-test-spans mode
      (with-current-buffer (find-file-noselect file)
        (substring-no-properties (buffer-string))))))

(defmacro pm-test-run-on-file (mode file-name &rest body)
  "Run BODY in a buffer with the content of FILE-NAME in MODE."
  (declare (indent 2) (debug (sexp sexp body)))
  (let ((pre-form (when (eq (car body) :pre-form)
                    (prog1 (cadr body)
                      (setq body (cddr body))))))
    `(let ((poly-lock-allow-background-adjustment nil)
           ;; snapshot it during the expansion to be able to run polymode-organization tests
           (file ,(pm-test-get-file file-name))
           (pm-extra-span-info nil)
           (buf "*pm-test-file-buffer*"))
       (when (get-buffer buf)
         (kill-buffer buf))
       (with-current-buffer (get-buffer-create buf)
         (when pm-verbose
           (message "\n===================  testing %s =======================" file))
         (switch-to-buffer buf)
         (insert-file-contents file)
         (remove-hook 'text-mode-hook 'flyspell-mode) ;; triggers "too much reentrancy" error
         (let ((inhibit-message (not pm-verbose)))
           (funcall-interactively ',mode))
         (hack-local-variables 'ignore-mode)
         (goto-char (point-min))
         ,pre-form
         ;; FIXME: figure this mambo-jumbo
         ;; need this to activate all chunks
         (goto-char (point-min))
         (save-excursion
           (let ((font-lock-mode t))
             (pm-map-over-spans
              (lambda (_)
                (setq font-lock-mode t)
                ;; This is not picked up because font-lock is nil on innermode
                ;; initialization. Don't know how to fix this more elegantly.
                ;; For now our tests are all with font-lock, so we are fine for
                ;; now.
                ;; !! Font-lock is not activated in batch mode !!
                (setq-local poly-lock-allow-fontification t)
                (poly-lock-mode t)
                ;; redisplay is not triggered in batch and often it doesn't trigger
                ;; fontification in X either (waf?)
                (add-hook 'after-change-functions #'pm-test-invoke-fontification t t))
              (point-min) (point-max))))
         ;; (font-lock-flush)
         ;; (font-lock-ensure)
         ,@body
         (current-buffer)))))

(defun pm-test-span-faces (span &optional allow-failed-faces)
  ;; head/tail is usually highlighted incorrectly by host modes when only head
  ;; is in the buffer, so we just skip those head-tails which have
  ;; :head/tail-mode 'host
  (when (eq (car span) (pm-true-span-type *span*))
    (let* ((poly-lock-allow-background-adjustment nil)
           (sbeg (nth 1 span))
           (send (nth 2 span))
           (smode major-mode)
           (stext (buffer-substring-no-properties sbeg send))
           ;; other buffer
           (ref-buf (pm-test-run-on-string smode stext))
           (ref-pos 1))
      (when pm-verbose
        (message "---- testing %s ----" (pm-format-span span t)))
      ;; NB: String delimiters '' in pascal mode don't work in batch
      ;; (require 'polymode-debug)
      ;; (when (and (eq smode 'pascal-mode)
      ;;            (> (buffer-size ref-buf) 29)
      ;;            (> (buffer-size) 700))
      ;;   (message "%s"
      ;;            (list
      ;;             :parse-sexp-lookup-properties  parse-sexp-lookup-properties
      ;;             :font-lock-keywords-only font-lock-keywords-only
      ;;             :font-lock-syntactic-face-function font-lock-syntactic-face-function
      ;;             :font-lock-sk font-lock-syntactic-keywords
      ;;             :syntax-prop-fun syntax-propertize-function
      ;;             :ppss (syntax-ppss 675)
      ;;             :char (pm--syntax-after 675)))
      ;;   (with-current-buffer ref-buf
      ;;     (message "%s"
      ;;              (list
      ;;               :parse-sexp-lookup-properties  parse-sexp-lookup-properties
      ;;               :font-lock-keywords-only font-lock-keywords-only
      ;;               :font-lock-syntactic-face-function font-lock-syntactic-face-function
      ;;               :font-lock-sk font-lock-syntactic-keywords
      ;;               :syntax-prop-fun syntax-propertize-function
      ;;               :ppss-29 (syntax-ppss 29)
      ;;               :char-29 (pm--syntax-after 29)))))
      (while ref-pos
        (let* ((pos (1- (+ ref-pos sbeg)))
               (face (get-text-property pos 'face))
               (ref-face (get-text-property ref-pos 'face ref-buf)))
          (unless (or
                   ;; in markdown fence regexp matches end of line; it's likely
                   ;; to be a common mismatch between host mode and polymode,
                   ;; thus don't check first pos if it's a new line
                   (and (= ref-pos 1)
                        (with-current-buffer ref-buf
                          (eq (char-after 1) ?\n)))
                   (member face allow-failed-faces)
                   (equal face ref-face))
            (let ((data
                   (append
                    (when pm-test-current-change-set
                      (list :change pm-test-current-change-set))
                    (list
                     ;; :af poly-lock-allow-fontification
                     ;; :fl font-lock-mode
                     :face face
                     :ref-face ref-face
                     :pos pos
                     :ref-pos ref-pos
                     :line (progn (goto-char pos)
                                  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                     :ref-line (with-current-buffer ref-buf
                                 (goto-char ref-pos)
                                 (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                     :mode smode))))
              ;; for the interactive convenience
              (switch-to-buffer (current-buffer))
              (ert-fail data)))
          (setq ref-pos (next-single-property-change ref-pos 'face ref-buf)))))))

(defun pm-test-faces (&optional allow-failed-faces)
  "Execute `pm-test-span-faces' for every span in the buffer.
ALLOW-FAILED-FACES should be a list of faces on which failures
are OK."
  (save-excursion
    (font-lock-flush)
    (font-lock-ensure)
    (pm-map-over-spans
     (lambda (span) (pm-test-span-faces span allow-failed-faces)))))

(defun pm-test-goto-loc (loc)
  "Go to LOC and switch to polymode indirect buffer.
LOC can be either
  - a number giving position in the buffer
  - regexp to search for from ‘point-min’
  - a cons of the form (ROW . COL)
In the last case ROW can be either a number or a regexp to search
for and COL either a column number or symbols beg or end
indicating beginning or end of the line. When COL is nil, goto
indentation."
  (cond
   ((numberp loc)
    (goto-char loc))
   ((stringp loc)
    (goto-char (point-min))
    (re-search-forward loc))
   ((consp loc)
    (goto-char (point-min))
    (let ((row (car loc)))
      (goto-char (point-min))
      (cond
       ((stringp row)
        (re-search-forward row))
       ((numberp row)
        (forward-line (1- row)))
       (t (error "Invalid row spec %s" row))))
    (let* ((col (cdr loc))
           (col (if (listp col)
                    (car col)
                  col)))
      (cond
       ((numberp col)
        (forward-char col))
       ((eq col 'end)
        (end-of-line))
       ((eq col 'beg)
        (beginning-of-line))
       ((null col)
        (back-to-indentation))
       (t (error "Invalid col spec %s" col))))))
  (when polymode-mode
    ;; pm-set-buffer would do for programs but not for interactive debugging
    (pm-switch-to-buffer (point))))

(defun pm-test-goto-loc-other-window ()
  "Utility to navigate to loc at point in other buffer.
LOC is as in `pm-test-goto-loc'."
  (interactive)
  (let ((loc (or (sexp-at-point)
                 (read--expression "Loc: "))))
    (when (symbolp loc)
      (setq loc (string-to-number (thing-at-point 'word))))
    (other-window 1)
    (pm-test-goto-loc loc)))

(defun pm-test-invoke-fontification (&rest _ignore)
  "Mimic calls to fontification functions by redisplay.
Needed because redisplay is not triggered in batch mode."
  (when fontification-functions
    (save-match-data
      (save-restriction
        (widen)
        (save-excursion
          (let (pos)
            (while (setq pos (text-property-any (point-min) (point-max) 'fontified nil))
              (let ((inhibit-modification-hooks t)
                    (poly-lock-defer-after-change nil)
                    (inhibit-redisplay t))
                (when pm-verbose
                  (message "after change fontification-functions (%s)" pos))
                (run-hook-with-args 'fontification-functions pos)))))))))

(defmacro pm-test-poly-lock (mode file &rest change-sets)
  "Test font-lock for MODE and FILE.
CHANGE-SETS is a collection of forms of the form (NAME-LOC &rest
BODY). NAME-LOC is a list of the form (NAME LOCK) where NAME is a
symbol, LOC is the location as in `pm-test-goto-loc'. Before and
after execution of the BODY ‘undo-boundary’ is set and after the
execution undo is called once. After each change-set
`pm-test-faces' on the whole file is run."
  (declare (indent 2)
           (debug (sexp sexp &rest ((name sexp) &rest form))))
  `(kill-buffer
    (pm-test-run-on-file ,mode ,file
      (pm-test-faces)
      (set-buffer-modified-p nil)
      (dolist (cset ',change-sets)
        (let ((poly-lock-defer-after-change nil)
              (pm-test-current-change-set (caar cset)))
          (setq pm-extra-span-info (caar cset))
          (undo-boundary)
          (pm-test-goto-loc (nth 1 (car cset)))
          (eval (cons 'progn (cdr cset)))
          (undo-boundary)
          (pm-test-faces)
          (let ((inhibit-message (not pm-verbose)))
            (when (buffer-modified-p)
              (undo))))))))

(defun pm-test--run-indentation-tests ()
  "Run an automatic batch of indentation tests.
First run `indent-line' on every line and compare original and
indented version. Then compute stasrt,middle and end points of
each span and call `indent-region' on a shuffled set of these
points."
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (while (not (eobp))
    (let ((orig-line (buffer-substring-no-properties (point-at-eol) (point-at-bol))))
      (unless (string-match-p "no-indent-test" orig-line)
        (undo-boundary)
        ;; (pm-switch-to-buffer)
        ;; (message "line:%d pos:%s buf:%s ppss:%s spd:%s"
        ;;          (line-number-at-pos) (point) (current-buffer)
        ;;          (syntax-ppss) syntax-propertize--done)
        (pm-indent-line-dispatcher)
        (unless (equal orig-line (buffer-substring-no-properties (point-at-eol) (point-at-bol)))
          (undo-boundary)
          (pm-switch-to-buffer (point))
          (ert-fail (list :pos (point) :line (line-number-at-pos)
                          :mode major-mode
                          :indent-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))))
    (forward-line 1))
  (let (points1 points2)
    (pm-map-over-spans (lambda (span) (push (/ (+ (nth 1 span) (nth 2 span)) 2) points1)))
    (random "some-seed")
    (let ((len (length points1)))
      (dotimes (_ len)
        (push (elt points1 (random len)) points2)))
    (let ((points2 (reverse points1)))
      (cl-mapc
       (lambda (beg end)
         (unless (= beg end)
           (let ((orig-region (buffer-substring-no-properties beg end)))
             (unless (string-match-p "no-indent-test" orig-region)
               (undo-boundary)
               (indent-region beg end)
               (unless (equal orig-region (buffer-substring-no-properties beg end))
                 (undo-boundary)
                 (pm-switch-to-buffer beg)
                 (ert-fail `(indent-region ,beg ,end)))))))
       points1 points2))))

(defmacro pm-test-indentation (mode file)
  "Test indentation for MODE and FILE."
  `(pm-test-run-on-file ,mode ,file
     (undo-boundary)
     (let ((inhibit-message (not pm-verbose)))
       (unwind-protect
           (pm-test--run-indentation-tests)
         (undo-boundary)))))

(defmacro pm-test-file-indent (mode file-with-indent &optional file-no-indent)
  `(pm-test-run-on-file ,mode ,(or file-no-indent file-with-indent)
     (let ((indent-tabs-mode nil)
           (right (with-current-buffer (find-file-noselect
                                        ,(pm-test-get-file file-with-indent))
                    (substring-no-properties (buffer-string))))
           (inhibit-message t))
       (unless ,file-no-indent
         (goto-char 1)
         (while (re-search-forward "^[ \t]+"  nil t)
           (replace-match ""))
         (goto-char 1))
       (indent-region (point-min) (point-max))
       (let ((new (substring-no-properties (buffer-string))))
         (unless (string= right new)
           (require 'pascal)
           (let ((pos (1+ (pascal-string-diff right new))))
             (ert-fail (list "Wrong indent" :pos pos
                             :ref (with-temp-buffer
                                    (insert right)
                                    (goto-char pos)
                                    (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                             :new (progn
                                    (goto-char pos)
                                    (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))))))))

(defmacro pm-test-map-over-modes (mode file)
  `(pm-test-run-on-file ,mode ,file
     (let ((beg (point-min))
           (end (point-max))
           (pm-use-cache t))
       (with-buffer-prepared-for-poly-lock
        (remove-text-properties beg end '(:pm-span :pm-face)))
       (pm-map-over-modes (lambda (b e)) beg end)
       (while (< beg end)
         (let ((span (get-text-property beg :pm-span))
               (mid (next-single-property-change beg :pm-span nil end)))
           (dolist (pos (list beg
                              (/ (+ beg mid) 2)
                              (1- mid)))
             (let ((ispan (pm-innermost-span pos t)))
               (unless (equal span ispan)
                 (let ((span (copy-sequence span))
                       (ispan (copy-sequence ispan)))
                   (setf (nth 3 span) (eieio-object-name (nth 3 span)))
                   (setf (nth 3 ispan) (eieio-object-name (nth 3 ispan)))
                   (pm-switch-to-buffer pos)
                   (ert-fail (list :pos pos :mode-span span :innermost-span ispan))))))
           (setq beg (nth 2 span)))))))

(provide 'polymode-test-utils)
;;; polymode-test-utils.el ends here
