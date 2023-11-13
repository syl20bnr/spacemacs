;; -*- lexical-binding: t; -*-
(require 'sly)
(require 'sly-parse "lib/sly-parse")
(require 'font-lock)
(require 'cl-lib)

;;; Fontify WITH-FOO, DO-FOO, and DEFINE-FOO like standard macros.
;;; Fontify CHECK-FOO like CHECK-TYPE.
(defvar sly-additional-font-lock-keywords
 '(("(\\(\\(\\s_\\|\\w\\)*:\\(define-\\|do-\\|with-\\|without-\\)\\(\\s_\\|\\w\\)*\\)" 1 font-lock-keyword-face)
   ("(\\(\\(define-\\|do-\\|with-\\)\\(\\s_\\|\\w\\)*\\)" 1 font-lock-keyword-face)
   ("(\\(check-\\(\\s_\\|\\w\\)*\\)" 1 font-lock-warning-face)
   ("(\\(assert-\\(\\s_\\|\\w\\)*\\)" 1 font-lock-warning-face)))

;;;; Specially fontify forms suppressed by a reader conditional.
(defcustom sly-highlight-suppressed-forms t
  "Display forms disabled by reader conditionals as comments."
  :type '(choice (const :tag "Enable" t) (const :tag "Disable" nil))
  :group 'sly-mode)

(define-sly-contrib sly-fontifying-fu
  "Additional fontification tweaks:
Fontify WITH-FOO, DO-FOO, DEFINE-FOO like standard macros.
Fontify CHECK-FOO like CHECK-TYPE."
  (:authors "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:on-load
   (font-lock-add-keywords
    'lisp-mode sly-additional-font-lock-keywords)
   (when sly-highlight-suppressed-forms
     (sly-activate-font-lock-magic)))
  (:on-unload
   ;; FIXME: remove `sly-search-suppressed-forms', and remove the
   ;; extend-region hook.
   (font-lock-remove-keywords
    'lisp-mode sly-additional-font-lock-keywords)))

(defface sly-reader-conditional-face
  '((t (:inherit font-lock-comment-face)))
  "Face for compiler notes while selected."
  :group 'sly-mode-faces)

(defvar sly-search-suppressed-forms-match-data (list nil nil))

(defun sly-search-suppressed-forms-internal (limit)
  (when (search-forward-regexp sly-reader-conditionals-regexp limit t)
    (let ((start (match-beginning 0))   ; save match data
          (state (sly-current-parser-state)))
      (if (or (nth 3 state) (nth 4 state)) ; inside string or comment?
          (sly-search-suppressed-forms-internal limit)
        (let* ((char (char-before))
               (expr (read (current-buffer)))
               (val  (sly-eval-feature-expression expr)))
          (when (<= (point) limit)
            (if (or (and (eq char ?+) (not val))
                    (and (eq char ?-) val))
                ;; If `sly-extend-region-for-font-lock' did not
                ;; fully extend the region, the assertion below may
                ;; fail. This should only happen on XEmacs and older
                ;; versions of GNU Emacs.
                (ignore-errors
                  (forward-sexp) (backward-sexp)
                  ;; Try to suppress as far as possible.
                  (sly-forward-sexp)
                  (cl-assert (<= (point) limit))
                  (let ((md (match-data nil sly-search-suppressed-forms-match-data)))
                    (setf (cl-first md) start)
                    (setf (cl-second md) (point))
                    (set-match-data md)
                    t))
              (sly-search-suppressed-forms-internal limit))))))))

(defun sly-search-suppressed-forms (limit)
  "Find reader conditionalized forms where the test is false."
  (when (and sly-highlight-suppressed-forms
             (sly-connected-p))
    (let ((result 'retry))
      (while (and (eq result 'retry) (<= (point) limit))
        (condition-case condition
            (setq result (sly-search-suppressed-forms-internal limit))
          (end-of-file                        ; e.g. #+(
           (setq result nil))
          ;; We found a reader conditional we couldn't process for
          ;; some reason; however, there may still be other reader
          ;; conditionals before `limit'.
          (invalid-read-syntax                ; e.g. #+#.foo
           (setq result 'retry))
          (scan-error                         ; e.g. #+nil (foo ...
           (setq result 'retry))
          (sly-incorrect-feature-expression ; e.g. #+(not foo bar)
           (setq result 'retry))
          (sly-unknown-feature-expression   ; e.g. #+(foo)
           (setq result 'retry))
          (error
           (setq result nil)
           (sly-warning
            (concat "Caught error during fontification while searching for forms\n"
                    "that are suppressed by reader-conditionals. The error was: %S.")
            condition))))
      result)))


(defun sly-search-directly-preceding-reader-conditional ()
  "Search for a directly preceding reader conditional. Return its
position, or nil."
  ;;; We search for a preceding reader conditional. Then we check that
  ;;; between the reader conditional and the point where we started is
  ;;; no other intervening sexp, and we check that the reader
  ;;; conditional is at the same nesting level.
  (condition-case nil
      (let* ((orig-pt (point))
	     (reader-conditional-pt
	      (search-backward-regexp sly-reader-conditionals-regexp
				      ;; We restrict the search to the
				      ;; beginning of the /previous/ defun.
				      (save-excursion
					(beginning-of-defun)
					(point))
				      t)))
	(when reader-conditional-pt
          (let* ((parser-state
                  (parse-partial-sexp
		   (progn (goto-char (+ reader-conditional-pt 2))
			  (forward-sexp) ; skip feature expr.
			  (point))
		   orig-pt))
                 (paren-depth  (car  parser-state))
                 (last-sexp-pt (cl-caddr  parser-state)))
            (if (and paren-depth
		     (not (cl-plusp paren-depth)) ; no '(' in between?
                     (not last-sexp-pt)) ; no complete sexp in between?
                reader-conditional-pt
              nil))))
    (scan-error nil)))			; improper feature expression


;;; We'll push this onto `font-lock-extend-region-functions'. In past,
;;; we didn't do so which made our reader-conditional font-lock magic
;;; pretty unreliable (it wouldn't highlight all suppressed forms, and
;;; worked quite non-deterministic in general.)
;;;
;;; Cf. _Elisp Manual_, 23.6.10 Multiline Font Lock Constructs.
;;;
;;; We make sure that `font-lock-beg' and `font-lock-end' always point
;;; to the beginning or end of a toplevel form. So we never miss a
;;; reader-conditional, or point in mid of one.
(defvar font-lock-beg) ; shoosh compiler
(defvar font-lock-end)

(defun sly-extend-region-for-font-lock ()
  (when sly-highlight-suppressed-forms
    (condition-case c
        (let (changedp)
          (cl-multiple-value-setq (changedp font-lock-beg font-lock-end)
            (sly-compute-region-for-font-lock font-lock-beg font-lock-end))
          changedp)
      (error
       (sly-warning
        (concat "Caught error when trying to extend the region for fontification.\n"
                "The error was: %S\n"
                "Further: font-lock-beg=%d, font-lock-end=%d.")
        c font-lock-beg font-lock-end)))))

(defsubst sly-beginning-of-tlf ()
  (let ((pos (syntax-ppss-toplevel-pos (sly-current-parser-state))))
    (if pos (goto-char pos))))

(defun sly-compute-region-for-font-lock (orig-beg orig-end)
  (let ((beg orig-beg)
        (end orig-end))
    (goto-char beg)
    (sly-beginning-of-tlf)
    (cl-assert (not (cl-plusp (nth 0 (sly-current-parser-state)))))
    (setq beg (let ((pt (point)))
                (cond ((> (- beg pt) 20000) beg)
                      ((sly-search-directly-preceding-reader-conditional))
                      (t pt))))
    (goto-char end)
    (while (search-backward-regexp sly-reader-conditionals-regexp beg t)
      (setq end (max end (save-excursion
                           (ignore-errors (sly-forward-reader-conditional))
                           (point)))))
    (cl-values (or (/= beg orig-beg) (/= end orig-end)) beg end)))


(defun sly-activate-font-lock-magic ()
  (font-lock-add-keywords
   'lisp-mode
   `((sly-search-suppressed-forms 0 ,''sly-reader-conditional-face t)))

  (add-hook 'lisp-mode-hook
            #'(lambda ()
                (add-hook 'font-lock-extend-region-functions
                          'sly-extend-region-for-font-lock t t))))


;;; Compile hotspots
;;; 
(sly-byte-compile-hotspots
 '(sly-extend-region-for-font-lock
   sly-compute-region-for-font-lock
   sly-search-directly-preceding-reader-conditional
   sly-search-suppressed-forms
   sly-beginning-of-tlf))

(provide 'sly-fontifying-fu)
