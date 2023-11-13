;;; polymode-debug.el --- Interactive debugging utilities for polymode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2016-2022  Free Software Foundation, Inc.
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

;;; Commentary:
;;

;;; Code:

(require 'polymode-core)
(require 'poly-lock)
(require 'trace)


;;; MINOR MODE

(defvar pm--underline-overlay
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face  '(:underline (:color "tomato" :style wave)))
    overlay)
  "Overlay used in function `pm-debug-mode'.")

(defvar pm--highlight-overlay
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face  '(:inverse-video t))
    overlay)
  "Overlay used by `pm-debug-map-over-spans-and-highlight'.")

(defvar pm-debug-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n M-i")     #'pm-debug-info-on-current-span)
    (define-key map (kbd "M-n i")       #'pm-debug-info-on-current-span)
    (define-key map (kbd "M-n M-p")     #'pm-debug-relevant-variables)
    (define-key map (kbd "M-n p")       #'pm-debug-relevant-variables)
    (define-key map (kbd "M-n M-h")     #'pm-debug-map-over-spans-and-highlight)
    (define-key map (kbd "M-n h")       #'pm-debug-map-over-spans-and-highlight)
    (define-key map (kbd "M-n M-t t")   #'pm-toggle-tracing)
    (define-key map (kbd "M-n M-t i")   #'pm-debug-toogle-info-message)
    (define-key map (kbd "M-n M-t f")   #'pm-debug-toggle-fontification)
    (define-key map (kbd "M-n M-t p")   #'pm-debug-toggle-post-command)
    (define-key map (kbd "M-n M-t c")   #'pm-debug-toggle-after-change)
    (define-key map (kbd "M-n M-t a")   #'pm-debug-toggle-all)
    (define-key map (kbd "M-n M-t M-t")   #'pm-toggle-tracing)
    (define-key map (kbd "M-n M-t M-i")   #'pm-debug-toogle-info-message)
    (define-key map (kbd "M-n M-t M-f")   #'pm-debug-toggle-fontification)
    (define-key map (kbd "M-n M-t M-p")   #'pm-debug-toggle-post-command)
    (define-key map (kbd "M-n M-t M-c")   #'pm-debug-toggle-after-change)
    (define-key map (kbd "M-n M-t M-a")   #'pm-debug-toggle-all)
    (define-key map (kbd "M-n M-f s")   #'pm-debug-fontify-current-span)
    (define-key map (kbd "M-n M-f b")   #'pm-debug-fontify-current-buffer)
    (define-key map (kbd "M-n M-f M-t")   #'pm-debug-toggle-fontification)
    (define-key map (kbd "M-n M-f M-s")   #'pm-debug-fontify-current-span)
    (define-key map (kbd "M-n M-f M-b")   #'pm-debug-fontify-current-buffer)
    map))

;;;###autoload
(define-minor-mode pm-debug-minor-mode
  "Turns on/off useful facilities for debugging polymode.

Key bindings:
\\{pm-debug-minor-mode-map}"
  :lighter " PMDBG"
  :group 'polymode
  (if pm-debug-minor-mode
      (progn
        ;; this is global hook. No need to complicate with local hooks
        (add-hook 'post-command-hook #'pm-debug-highlight-current-span)
        ;; (add-hook 'before-save-hook #'pm-debug-beore-change -99 t)
        ;; (add-hook 'after-save-hook #'pm-debug-after-change -99)
        )
    ;; (remove-hook 'before-save-hook #'pm-debug-beore-change)
    ;; (remove-hook 'after-save-hook #'pm-debug-after-change)
    (delete-overlay pm--underline-overlay)
    (delete-overlay pm--highlight-overlay)
    (remove-hook 'post-command-hook #'pm-debug-highlight-current-span)))

;; use to track point movements (#295)
(defun pm--debug-report-point (msg &optional r)
  (when polymode-mode
    (message "%s %s buffer[%s:%s %s:%s] window[%s:%s]"
             msg (if r r "")
             (pm-base-buffer) (with-current-buffer (pm-base-buffer) (point))
             (buffer-name) (point)
             (get-buffer-window (pm-base-buffer))
             (with-current-buffer (pm-base-buffer) (window-point))
             ;; FIXME: This arg is not used.
             (window-point))))

;; (defun pm-debug-beore-change (&rest r)
;;   (pm--debug-report-point "|before|" this-command))

;; (defun pm-debug-after-change (&rest r)
;;   (pm--debug-report-point "|after|" this-command))

;;;###autoload
(defun pm-debug-minor-mode-on ()
  ;; activating everywhere (in case font-lock infloops in a polymode buffer )
  ;; this doesn't activate in fundamental mode
  (unless (eq major-mode 'minibuffer-inactive-mode)
    (pm-debug-minor-mode t)))

;;;###autoload
(define-globalized-minor-mode pm-debug-mode pm-debug-minor-mode pm-debug-minor-mode-on
  :group 'polymode)


;;; INFO

(cl-defgeneric pm-debug-info (chunkmode))
(cl-defmethod pm-debug-info (chunkmode)
  (eieio-object-name chunkmode))
(cl-defmethod pm-debug-info ((chunkmode pm-inner-chunkmode))
  (format "%s head-matcher:\"%s\" tail-matcher:\"%s\""
          (cl-call-next-method)
          (eieio-oref chunkmode 'head-matcher)
          (eieio-oref chunkmode 'tail-matcher)))
(cl-defmethod pm-debug-info ((_chunkmode pm-inner-auto-chunkmode))
  (cl-call-next-method))

(defvar syntax-ppss-wide)
(defvar syntax-ppss-last)
(defun pm--debug-info (&optional span as-list)
  (let* ((span (or span (and polymode-mode (pm-innermost-span))))
         (message-log-max nil)
         (beg (nth 1 span))
         (end (nth 2 span))
         (obj (nth 3 span))
         (type (and span (or (car span) 'host))))
    (let ((out (list (current-buffer)
                     (point-min) (point) (point-max)
                     major-mode
                     type beg end
                     (and obj (pm-debug-info obj))
                     (format "lppss:%s"
                             (if pm--emacs>26
                                 (car syntax-ppss-wide)
                               syntax-ppss-last)))))
      (if as-list
          out
        (apply #'format
               "(%s) min:%d pos:%d max:%d || (%s) type:%s span:%s-%s %s %s"
               out)))))

(defun pm-debug-info-on-current-span (no-cache)
  "Show info on current span.
With NO-CACHE prefix, don't use cached values of the span."
  (interactive "P")
  (if (not polymode-mode)
      (message "not in a polymode buffer")
    (let ((span (pm-innermost-span nil no-cache)))
      (message (pm--debug-info span))
      ;; (move-overlay pm--highlight-overlay (nth 1 span) (nth 2 span) (current-buffer))
      (pm-debug-flick-region (nth 1 span) (nth 2 span)))))

(defun pm-debug-report-points (&optional where)
  (when polymode-mode
    (let* ((bufs (eieio-oref pm/polymode '-buffers))
           (poses (mapcar (lambda (b)
                            (format "%s:%d" b (with-current-buffer b (point))))
                          bufs)))
      (message "<%s> cb:%s %s" (or where "") (current-buffer) poses)))
  nil)


;;; TOGGLING

(defvar pm-debug-display-info-message nil)
(defun pm-debug-toogle-info-message ()
  "Toggle permanent info display."
  (interactive)
  (setq pm-debug-display-info-message (not pm-debug-display-info-message)))

(defvar poly-lock-allow-fontification)
(defun pm-debug-toggle-fontification ()
  "Enable or disable fontification in polymode buffers."
  (interactive)
  (if poly-lock-allow-fontification
      (progn
        (message "fontificaiton disabled")
        (dolist (b (buffer-list))
          (with-current-buffer b
            (when polymode-mode
              (setq poly-lock-allow-fontification nil
                    font-lock-mode nil
                    fontification-functions nil)))))
    (message "fontificaiton enabled")
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when polymode-mode
          (setq poly-lock-allow-fontification t
                font-lock-mode t
                fontification-functions '(poly-lock-function)))))))

(defun pm-debug-toggle-after-change ()
  "Allow or disallow polymode actions in `after-change-functions'."
  (interactive)
  (if pm-allow-after-change-hook
      (progn
        (message "after-change disabled")
        (setq pm-allow-after-change-hook nil))
    (message "after-change enabled")
    (setq pm-allow-after-change-hook t)))

(defun pm-debug-toggle-post-command ()
  "Allow or disallow polymode actions in `post-command-hook'."
  (interactive)
  (if pm-allow-post-command-hook
      (progn
        (message "post-command disabled")
        (setq pm-allow-post-command-hook nil))
    (message "post-command enabled")
    (setq pm-allow-post-command-hook t)))

(defun pm-debug-toggle-all ()
  "Toggle all polymode guards back and forth."
  (interactive)
  (if poly-lock-allow-fontification
      (progn
        (message "fontificaiton, after-chnage and command-hook disabled")
        (setq poly-lock-allow-fontification nil
              pm-allow-after-change-hook nil
              pm-allow-post-command-hook nil))
    (message "fontificaiton, after-change and command-hook enabled")
    (setq poly-lock-allow-fontification t
          pm-allow-after-change-hook t
          pm-allow-post-command-hook t)))


;;; FONT-LOCK

(defun pm-debug-fontify-current-span ()
  "Fontify current span."
  (interactive)
  (let ((span (pm-innermost-span))
        (poly-lock-allow-fontification t))
    (poly-lock-flush (nth 1 span) (nth 2 span))
    (poly-lock-fontify-now (nth 1 span) (nth 2 span))))

(defun pm-debug-fontify-current-buffer ()
  "Fontify current buffer."
  (interactive)
  (let ((poly-lock-allow-fontification t))
    (font-lock-unfontify-buffer)
    (poly-lock-flush (point-min) (point-max))
    (poly-lock-fontify-now (point-min) (point-max))))


;;; TRACING

(defvar pm-traced-functions
  '(
    ;; core initialization (traced even when polymode-mode is not yet installed)
    (0 (pm--common-setup
        pm--mode-setup
        pm--run-derived-mode-hooks
        pm--run-init-hooks
        pm-initialize
        hack-local-variables
        run-hooks
        run-mode-hooks))
    ;; core hooks
    (1 (polymode-pre-command
        polymode-post-command
        polymode-after-kill-fixes
        ;; this one indicates the start of a sequence
        poly-lock-after-change))
    ;; advises
    (2 (pm-override-output-cons
        pm-around-advice
        polymode-with-current-base-buffer
        polymode-inhibit-during-initialization
        pm-check-for-real-change-in-extend-multiline
        poly-lock-no-jit-lock-in-polymode-buffers
        pm-override-output-position))
    ;; (2.5 . "^markdown-fontify-.*")
    ;; init
    (3  (pm-map-over-spans
         pm-map-over-modes
         pm-innermost-span
         pm-next-chunk))
    ;; font-lock
    (4 . ".*\\(font\\|jit\\|poly\\)-lock.*")
    ;; syntax
    (5 (syntax-ppss
        pm--call-syntax-propertize-original
        polymode-syntax-propertize
        polymode-restrict-syntax-propertize-extension
        pm-flush-syntax-ppss-cache
        pm--reset-ppss-cache))
    ;; core functions
    (6 (pm-select-buffer
        pm-map-over-spans
        pm--get-intersected-span
        pm--cached-span))
    (6 . "^polymode-")
    (7 . "^pm-")
    (20 . "^syntax-")
    ))

(defvar pm--do-trace nil)
;;;###autoload
(defun pm-toggle-tracing (level)
  "Toggle polymode tracing.
With numeric prefix toggle tracing for that LEVEL. Currently
universal argument toggles maximum level of tracing (15). See
`pm-traced-functions'. Default level is 4."
  (interactive "P")
  (setq level (prefix-numeric-value (or level 4)))
  (with-current-buffer (get-buffer-create "*TMessages*")
    (read-only-mode -1))
  (when pm--do-trace
    (untrace-all))
  (setq pm--do-trace (not pm--do-trace))
  (if pm--do-trace
      (progn (dolist (kv pm-traced-functions)
               (when (<= (car kv) level)
                 (if (stringp (cdr kv))
                     (pm-trace-functions-by-regexp (cdr kv))
                   (dolist (fn (cadr kv))
                     (pm-trace fn)))))
             (message "Polymode tracing activated"))
    (message "Polymode tracing deactivated")))


;;;###autoload
(defun pm-trace (fn)
  "Trace function FN.
Use `untrace-function' to untrace or `untrace-all' to untrace all
currently traced functions."
  (interactive (trace--read-args "Trace:"))
  (let ((buff (get-buffer "*Messages*")))
    (unless (advice-member-p trace-advice-name fn)
      (advice-add
       fn :around
       (let ((advice (trace-make-advice
                      fn buff 'background
                      #'pm-trace--tracing-context)))
         (lambda (body &rest args)
           (when (eq fn 'polymode-flush-syntax-ppss-cache)
             ;; waf is this?
             (with-current-buffer buff
               (save-excursion
                 (goto-char (point-max))
                 (insert "\n"))))
           (if (or (memq fn (nth 1 (car pm-traced-functions)))
                   polymode-mode
                   ;; (derived-mode-p 'markdown-mode)
                   )
               (apply advice body args)
             (apply body args))))
       `((name . ,trace-advice-name)
         (depth . -100))))))

(defun pm-trace-functions-by-regexp (regexp)
  "Trace all functions whose name matched REGEXP."
  (interactive "sRegex: ")
  (cl-loop for sym being the symbols
           when (and (fboundp sym)
                     (not (memq sym '(pm-toggle-tracing
                                      pm-trace--tracing-context
                                      pm-format-span
                                      pm-fun-matcher
                                      pm--find-tail-from-head)))
                     (not (string-match "^pm-\\(trace\\|debug\\)" (symbol-name sym)))
                     (string-match regexp (symbol-name sym)))
           do (pm-trace sym)))

(defun pm-trace--tracing-context ()
  (let ((span (or *span*
                  (get-text-property (point) :pm-span))))
    (format " [%s pos:%d/%d(%d-%d) %s%s (%f)]"
            (current-buffer) (point) (window-point) (point-min) (point-max)
            (or (when span
                  (when (not (and (= (point-min) (nth 1 span))
                                  (= (point-max) (nth 2 span))))
                    "UNPR "))
                "")
            (when span
              (pm-format-span span))
            (float-time))))

;; fix object printing
(defun pm-trace--fix-1-arg-for-tracing (arg)
  (cond
   ((eieio-object-p arg) (eieio-object-name arg))
   ((and (listp arg) (eieio-object-p (nth 3 arg)))
    (list (nth 0 arg) (nth 1 arg) (nth 2 arg) (eieio-object-name (nth 3 arg))))
   (arg)))

(defun pm-trace--fix-args-for-tracing (orig-fn fn level args context)
  (let* ((args (or (and (listp args)
                        (listp (cdr args))
                        (ignore-errors (mapcar #'pm-trace--fix-1-arg-for-tracing args)))
                   args))
         (print-circle t)
         (sargs (format "%s" args)))
    (when (> (length sargs) 200)
      (setq args "[...]"))
    (funcall orig-fn fn level args context)))

(advice-add #'trace-entry-message :around #'pm-trace--fix-args-for-tracing)
(advice-add #'trace-exit-message :around #'pm-trace--fix-args-for-tracing)
;; (advice-remove #'trace-entry-message #'pm-trace--fix-args-for-tracing)
;; (advice-remove #'trace-exit-message #'pm-trace--fix-args-for-tracing)


;;; RELEVANT VARIABLES

(defvar pm-debug-relevant-variables
  `(:change
    (before-change-functions after-change-functions)
    :command (pre-command-hook
              post-command-hook)
    :font-lock (fontification-functions
                font-lock-function
                font-lock-flush-function
                font-lock-ensure-function
                font-lock-fontify-region-function
                font-lock-fontify-buffer-function
                font-lock-unfontify-region-function
                font-lock-unfontify-buffer-function
                jit-lock-after-change-extend-region-functions
                jit-lock-functions
                poly-lock-defer-after-change)
    ;; If any of these are reset by host mode it can create issues with
    ;; font-lock and syntax (e.g. scala-mode in #195)
    :search (parse-sexp-lookup-properties
             parse-sexp-ignore-comments
             ;; (syntax-table)
             ;; font-lock-syntax-table
             case-fold-search)
    :indent (indent-line-function
             indent-region-function
             pm--indent-line-function-original)
    :revert (revert-buffer-function
             before-revert-hook
             after-revert-hook)
    :save (after-save-hook
           before-save-hook
           write-contents-functions
           local-write-file-hooks
           write-file-functions)
    :syntax (syntax-propertize-function
             syntax-propertize-extend-region-functions
             pm--syntax-propertize-function-original)))

;;;###autoload
(defun pm-debug-relevant-variables (&optional out-type)
  "Get the relevant polymode variables.
If OUT-TYPE is `buffer', print the variables in the dedicated buffer,
if `message' issue a message, if nil just return a list of values."
  (interactive (list 'buffer))
  (let* ((cbuff (current-buffer))
         (vars (cl-loop for v on pm-debug-relevant-variables by #'cddr
                        collect (cons (car v)
                                      (mapcar (lambda (v)
                                                (cons v (buffer-local-value v cbuff)))
                                              (cadr v))))))
    (require 'pp)
    (cond
     ((eq out-type 'buffer)
      (let ((inhibit-read-only t)
            (buf (get-buffer-create "*polymode-vars*")))
        (with-current-buffer buf
          (erase-buffer)
          (goto-char (point-max))
          (insert (format "\n================== %s ===================\n" cbuff))
          (insert (pp-to-string vars))
          (toggle-truncate-lines -1)
          (goto-char (point-max))
          (view-mode)
          (display-buffer (current-buffer)))
        (pop-to-buffer buf)))
     ((eq out-type 'message)
      (message "%s" (pp-to-string vars)))
     (t vars))))

(defun pm-debug-diff-local-vars (&optional buffer1 buffer2)
  "Print differences between local variables in BUFFER1 and BUFFER2."
  (interactive)
  (let* ((buffer1 (or buffer1 (read-buffer "Buffer1: " (buffer-name (current-buffer)))))
         (buffer2 (or buffer2 (read-buffer "Buffer2: " (buffer-name (nth 2 (buffer-list))))))
         (vars1 (buffer-local-variables (get-buffer buffer1)))
         (vars2 (buffer-local-variables (get-buffer buffer2)))
         (all-keys (delete-dups (append (mapcar #'car vars1)
                                        (mapcar #'car vars2))))
         (out-buf (get-buffer-create "*pm-debug-output")))
    (with-current-buffer out-buf
      (erase-buffer)
      (pp (delq nil
                (mapcar (lambda (k)
                          (let ((val1 (cdr (assoc k vars1)))
                                (val2 (cdr (assoc k vars2))))
                            (unless (equal val1 val2)
                              (list k val1 val2))))
                        all-keys))
          out-buf))
    (pop-to-buffer out-buf)))


;;; HIGHLIGHT

(defun pm-debug-highlight-current-span ()
  (when polymode-mode
    (with-silent-modifications
      (unless (memq this-command '(pm-debug-info-on-current-span
                                   pm-debug-highlight-last-font-lock-error-region))
        (delete-overlay pm--highlight-overlay))
      (condition-case-unless-debug err
          (let ((span (pm-innermost-span)))
            (when pm-debug-display-info-message
              (message (pm--debug-info span)))
            (move-overlay pm--underline-overlay (nth 1 span) (nth 2 span) (current-buffer)))
        (error (message "%s" (error-message-string err)))))))

(defun pm-debug-flick-region (start end &optional delay)
  (move-overlay pm--highlight-overlay start end (current-buffer))
  (run-with-timer (or delay 0.4) nil (lambda () (delete-overlay pm--highlight-overlay))))

(defun pm-debug-map-over-spans-and-highlight ()
  "Map over all spans in the buffer and highlight briefly."
  (interactive)
  (pm-map-over-spans (lambda (span)
                       (let ((start (nth 1 span))
                             (end (nth 2 span)))
                         (pm-debug-flick-region start end)
                         (sit-for 1)))
                     (point-min) (point-max) nil nil t))

(defun pm-debug-map-over-modes-and-highlight (&optional beg end)
  "Map over all spans between BEG and END and highlight modes."
  (interactive)
  (let ((cbuf (current-buffer)))
    (pm-map-over-modes
     (lambda (beg end)
       (goto-char beg)
       ;; (dbg beg end (pm-format-span))
       (with-current-buffer cbuf
         (recenter-top-bottom)
         (pm-debug-flick-region (max beg (point-min))
                                (min end (point-max))))
       (sit-for 1))
     (or beg (point-min))
     (or end (point-max)))))

(defun pm-debug-run-over-check (no-cache)
  "Map over all spans and report the time taken.
Switch to buffer is performed on every position in the buffer.
On prefix NO-CACHE don't use cached spans."
  (interactive)
  (goto-char (point-min))
  (let ((start (current-time))
        (count 1)
        (pm-initialization-in-progress no-cache))
    (pm-switch-to-buffer)
    (while (< (point) (point-max))
      (setq count (1+ count))
      (forward-char)
      (pm-switch-to-buffer))
    (let ((elapsed  (float-time (time-subtract (current-time) start))))
      (message "Elapsed: %s  per-char: %s" elapsed (/ elapsed count)))))

(defun pm-dbg (msg &rest args)
  (let ((cbuf (current-buffer))
        (cpos (point)))
    (with-current-buffer (get-buffer-create "*pm-dbg*")
      (save-excursion
        (goto-char (point-max))
        (insert "\n")
        (insert (apply #'format (concat "%f [%s at %d]: " msg)
                       (float-time) cbuf cpos args))))))

(provide 'polymode-debug)
;;; polymode-debug.el ends here
