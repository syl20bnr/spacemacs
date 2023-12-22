;;; evil-cleverparens.el --- Evil friendly minor-mode for editing lisp.
;;
;; Copyright (C) 2015 Olli Piepponen
;;
;; Author: Olli Piepponen <opieppo@gmail.com>
;; URL: https://github.com/emacs-evil/evil-cleverparens
;; Keywords: convenience, emulations
;; Version: 0.1.0
;; Package-Requires: ((evil "1.0") (paredit "1") (smartparens "1.6.1") (emacs "24.4") (dash "2.12.0"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; This file is free software (MIT License)

;;; Commentary:

;; Use Vim/evil like modal editing with Lisp without screwing up the structure
;; of your code.  Tries to offer useful alternatives for behavior which would
;; otherwise be destructive.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'evil)
(require 'paredit)
(require 'smartparens)
(require 'subr-x)
(require 'evil-cleverparens-text-objects)
(require 'evil-cleverparens-util)

;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup evil-cleverparens nil
  "`evil-mode' for handling your parentheses with a mix of `smartparens' and `paredit'"
  :group 'smartparens)

(defcustom evil-cleverparens-threshold 1500
  "If the region being operated on is larger than this we cop out.

   Quite a bit of work gets done to ensure the region being worked
   is in an safe state, so this lets us sarifice safety for a snappy
   editor on slower computers.

   Even on a large computer you shouldn't set this too high or your
   computer will freeze when copying large files out of Emacs.

   This is a feature copied from `evil-smartparens'."
  :group 'evil-cleverparens
  :type 'number)

(defcustom evil-cleverparens-complete-parens-in-yanked-region nil
  "Determines how to handle yanking a region containing
  unbalanced expressions. If this value is non-nil, a yanked
  region containing missing parentheses will include the missing
  parens appended to the end."
  :group 'evil-cleverparens
  :type 'boolean)

(defcustom evil-cleverparens-move-skip-delimiters t
  "Determines whether parentheses and other delimiters are
  considered symbols or not. The effect this has is that when
  enabled (default), the by-symbol navigation commands happily
  travel through these delimiters. This can be handy if you are
  already used to using evil-cleverparenses parentheses navigation
  commands."
  :group 'evil-cleverparens
  :type 'boolean)

(defcustom evil-cleverparens-swap-move-by-word-and-symbol nil
  "If true, the keys w, e, b, and ge will be bound to the
  evil-cleverparens movement by symbol commands, and the regular
  evil move by word commands will be bound to W, E, B and gE respectively."
  :group 'evil-cleverparens
  :type 'boolean)

(defcustom evil-cleverparens-drag-ignore-lines nil
  "Controls whether top-level sexps should be swapped with other
sexps or should lines be considered as well."
  :group 'evil-cleverparens
  :type 'boolean)

(defcustom evil-cleverparens-drag-comment-blocks t
  "Controls whether to drag by top-level comment block or a
  single line when point is inside a multi-line top-level command
  block."
  :group 'evil-cleverparens
  :type 'boolean)

(defcustom evil-cleverparens-indent-afterwards t
  "Controls whether to automatically indent when performing
  commands that alter the structure of the surrounding code.
  Enabled by default."
  :group 'evil-cleverparens
  :type 'boolean)

(defcustom evil-cleverparens-use-regular-insert nil
  "Determines whether to use `evil-insert' or `evil-cp-insert'."
  :group 'evil-cleverparens
  :type 'boolean)

(defvar evil-cp--override nil
  "Should the next command skip region checks?")

(defvar evil-cp--inserted-space-after-round-open nil
  "Helper var for `evil-cp-insert'.")

(defcustom evil-cleverparens-use-additional-bindings t
  "Should additional bindings be enabled."
  :type 'boolean
  :group 'evil-cleverparens)

(defcustom evil-cleverparens-use-additional-movement-keys t
  "Should additional movement keys, mostly those related to
  parentheses navigation, be enabled."
  :type 'boolean
  :group 'evil-cleverparens)

(defcustom evil-cleverparens-use-s-and-S t
  "Should we bind s and S, thus shadowing any existing bindings
to these keys."
  :type 'boolean
  :group 'evil-cleverparens)

(defcustom evil-cleverparens-enabled-hook nil
  "Called after `evil-cleverparens-mode' is turned on."
  :type 'hook
  :group 'evil-cleverparens)

(defcustom evil-cleverparens-disabled-hook nil
  "Called after `evil-cleverparens-mode' is turned off."
  :type 'hook
  :group 'evil-cleverparens)

;;; Overriding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-cp--override ()
  "Can be used as a predicate to determine if the next operation
should default to using regular evil functions. Resets the
`evil-cp--override' variable back to nil."
  (prog1 (or evil-cp--override
             (evil-cp--region-too-expensive-to-check))
    (setq evil-cp--override nil)))

(defun evil-cp-override ()
  "Calling this function will have evil-cleverparens default to
the regular evil equivalent of the next command that gets called."
  (interactive)
  (setq evil-cp--override t))

(defun evil-cp--region-too-expensive-to-check ()
  "When it takes prohobitively long to check region we cop out.

This is a feature copied from `evil-smartparens'."
  (when (region-active-p)
    (> (abs (- (region-beginning) (region-end)))
       evil-cleverparens-threshold)))

(defun evil-cp--fail ()
  "Error out with a friendly message."
  (error "Can't find a safe region to act on."))


;;; Evil Operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-cp--splice-form ()
  "Evil friendly version of splice that takes care of the
location of point, which also works for strings. Unlike
`sp-splice-sexp', this one doesn't perform clean-up for empty
forms."
  (evil-cp--guard-point-inc-string
   (-when-let (ok (sp-get-enclosing-sexp 1))
     (if (equal ";" (sp-get ok :prefix))
         (sp-get ok
           (goto-char :beg)
           (-when-let (enc (sp-get-enclosing-sexp 1))
             (sp--unwrap-sexp enc t)))
       (sp--unwrap-sexp ok t)))))

(defun evil-cp-delete-or-splice-region (beg end)
  "Deletes a region of text by splicing parens and deleting other
chars. Does not splice strings unless the whole expression is
contained within the region."
  (let ((chars-left (- end beg)))
    (while (> chars-left 0)
      (cond
       ((evil-cp--point-in-string-or-comment)
        (if (sp-get-comment-bounds)
            (let* ((ending (cdr (or (sp-get-comment-bounds)
                                    (sp-get-quoted-string-bounds))))
                   (diff (- (min end ending) (point))))
              (delete-char diff)
              (setq chars-left (- chars-left diff)))
          (progn
            (evil-cp--splice-form)
            (cl-decf chars-left))))

       ((evil-cp--looking-at-opening-p)
        (let ((other-paren (evil-cp--matching-paren-pos)))
          (if (< (point) other-paren end)
              (let ((char-count (sp-get (sp-get-sexp) (- :end :beg))))
                (delete-char char-count)
                (setq chars-left (- chars-left char-count)))
            (evil-cp--splice-form)
            (cl-decf chars-left))))

       ((evil-cp--looking-at-string-opening-p)
        (let ((other-quote (evil-cp--matching-paren-pos)))
          (if (< (point) other-quote end)
              (let ((char-count (sp-get (sp-get-string) (- :end :beg))))
                (delete-char char-count)
                (setq chars-left (- chars-left char-count)))
            (if (= 1 (- end (point)))
                (evil-cp--splice-form)
              (forward-char))
            (cl-decf chars-left))))

       ((evil-cp--looking-at-paren-p)
        (evil-cp--splice-form)
        (cl-decf chars-left))

       ((evil-cp--looking-at-string-delimiter-p)
        (delete-char 1)
        (cl-decf chars-left))

       (t
        (delete-char 1)
        (cl-decf chars-left))))))

(evil-define-operator evil-cp-delete-char-or-splice (beg end type register yank-handler)
  "Deletes the region by splicing parentheses / string delimiters
and deleting other characters. Can be overriden by
`evil-cp-override' in visual-mode."
  :motion evil-forward-char
  (interactive "<R><x>")
  (cond ((or (evil-cp--balanced-block-p beg end)
             (evil-cp--override))
         (evil-delete-char beg end type register))

        ((eq type 'block)
         (evil-cp--yank-rectangle beg end register yank-handler)
         (evil-apply-on-block #'evil-cp-delete-or-splice-region beg end nil))

        (t
         (if evil-cleverparens-complete-parens-in-yanked-region
             (evil-cp-yank beg end type register yank-handler)
           (evil-yank beg end type register yank-handler))
         (evil-cp-delete-or-splice-region beg end))))

(evil-define-operator evil-cp-delete-char-or-splice-backwards
  (beg end type register yank-handler)
  "Just like `evil-cp-delete-char-or-splice' but acts on the preceding character."
  :motion evil-backward-char
  (interactive "<R><x>")
  (evil-cp-delete-char-or-splice beg end type register yank-handler))

(evil-define-command evil-cp-delete-backward-word ()
  "Like `evil-delete-backward-word' with added behavior when
called in front of an opening delimiter, in which case the
enclosing form gets deleted."
  (if (evil-cp--looking-at-any-opening-p (1- (point)))
      (let ((enc (evil-cp--get-enclosing-bounds)))
        (delete-region (car enc) (cdr enc)))
    (evil-delete-backward-word)))

;; Originally from:
;; http://emacs.stackexchange.com/questions/777/closing-all-pending-parenthesis
(defun evil-cp--insert-missing-parentheses (backp)
  "Calling this function in a buffer with unbalanced parentheses
will have the missing parentheses be inserted at the end of the
buffer if BACKP is nil and at the beginning if it is true."
  (let ((closing nil))
    ;; fix for the degenerate case of nothing but closing parens
    (when backp (save-excursion (insert " ")))
    (save-excursion
      (while (condition-case nil
                 (progn
                   (if (not backp)
                       (backward-up-list)
                     (forward-char)
                     (up-list)
                     (backward-char))
                   (let* ((syntax (syntax-after (point)))
                          (head   (car syntax)))
                     (cond
                      ((eq head (if backp 5 4))
                       (setq closing (cons (cdr syntax) closing)))
                      ((member head '(7 8))
                       (setq closing (cons (char-after (point)) closing)))))
                   t)
               ((scan-error) nil))))
    (when backp (goto-char (point-min)))
    (apply #'insert (if backp closing (nreverse closing)))
    ;; see above
    (when backp (delete-char 1))))

(defun evil-cp--close-missing-parens (text)
  "Takes a text object and inserts missing parentheses first at
the end of the text, and then, if the expression is still
unbalanced, will insert the rest of the missing parens at the
beginning."
  (with-temp-buffer
    (insert text)
    (goto-char (point-max))
    (evil-cp--insert-missing-parentheses nil)
    (when (not (sp-region-ok-p (point-min) (point-max)))
      (goto-char (point-min))
      (evil-cp--insert-missing-parentheses t))
    (buffer-string)))

(defun evil-cp--region-has-unbalanced-string-p (beg end)
  "Predicate for checking if a region contains an unbalanced string."
  (not (cl-evenp (count-matches (sp--get-stringlike-regexp) beg end))))

(defun evil-cp--yank-characters
    (beg end &optional register yank-handler add-parens-p)
  "Yank characters while keeping parentheses balanced. The
optional ADD-PARENS-P arg determines how to handle the missing
parentheses: if nil, the non-balanced parens are
ignored. Otherwise they are added to the start/beginning of the
region."
  (unless (evil-cp--region-has-unbalanced-string-p beg end)
    (let ((text (string-trim (filter-buffer-substring beg end))))
      (when (not (evil-cp--text-balanced-p text))
        (setq text (evil-cp--close-missing-parens text)))
      (when yank-handler
        (setq text (propertize text 'yank-handler (list yank-handler))))
      (when register
        (evil-set-register register text))
      (when evil-was-yanked-without-register
        (evil-set-register ?0 text)) ; "0 register contains last yanked text
      (unless (eq register ?_)
        (kill-new text)))))

(defun evil-cp--yank-rectangle (beg end &optional register yank-handler)
  "Saves the rectangle defined by BEG and END into the kill-ring,
  while keeping the parentheses of the region balanced."
  (let ((lines (list nil)))
    (evil-apply-on-rectangle #'extract-rectangle-line beg end lines)
    (setq lines (nreverse (cdr lines)))
    (let* ((yank-handler (list (or yank-handler #'evil-yank-block-handler)
                               lines
                               nil
                               'evil-delete-yanked-rectangle))
           (joined (mapconcat #'identity lines "\n"))
           (text (if (evil-cp--text-balanced-p joined)
                     joined
                   (evil-cp--close-missing-parens joined)))
           (text (propertize text 'yank-handler yank-handler)))
      (when register (evil-set-register register text))
      (when evil-was-yanked-without-register (evil-set-register ?0 text))
      (unless (eq register ?_)
        (kill-new text)))))

(defun evil-cp--swap-regions (r1 r2)
  "Transposes the regions where R1 and R2 are cons-pairs where
the car is the start and the cdr is the end of each respective
region."
  (when (and r1 r2)
    (transpose-regions (cdr-safe r1)
                       (car-safe r1)
                       (cdr-safe r2)
                       (car-safe r2))))

(defun evil-cp--new-beginning (beg end &optional shrink)
  "Return a new value for BEG if POINT is inside an empty sexp.

If SHRINK is t we try to shrink the region until it is balanced
by decrementing BEG.

Copied from `evil-smartparens'."
  (if (not shrink)
      (min beg
           (if (sp-point-in-empty-sexp)
               (evil-cp--point-after
                (sp-backward-up-sexp))
             (point-max)))

    (let ((region (string-trim (buffer-substring-no-properties beg end))))
      (unless (string-blank-p region)
        (cond ((sp-point-in-empty-sexp)
               ;; expand region if we're in an empty sexp
               (setf end (save-excursion (sp-backward-up-sexp) (point))))

              ;; reduce region if it's unbalanced due to selecting too much
              (t (while (not (or (sp-region-ok-p beg end)
                                 (= beg end)))
                   (cl-incf beg)))))
      (when (= beg end)
        (evil-cp--fail)))
    beg))

(defun evil-cp--new-ending (beg end &optional no-error)
  "Find the largest safe region delimited by BEG END.

Copied from `evil-smartparens'."
  (let ((region (string-trim (buffer-substring-no-properties beg end))))
    (unless (string-blank-p region)
      (cond ((sp-point-in-empty-sexp)
             ;; expand region if we're in an empty sexp
             (setf end (save-excursion (sp-up-sexp) (point))))

            ;; reduce region if it's unbalanced due to selecting too much
            (t (while (not (or (sp-region-ok-p beg end)
                               (= beg end)))
                 (cl-decf end))))))
  (if (and (not no-error) (= beg end))
      (evil-cp--fail)
    end))

(defun evil-cp--ignoring-yank (beg end type register yank-handler)
  "This is a version of yank ignores unbalanced parentheses to
keep the region safe.

Copied from `evil-smartparens'."
  (save-excursion
    (condition-case nil
        (let ((new-beg (evil-cp--new-beginning beg end))
              (new-end (evil-cp--new-ending beg end)))
          (if (and (= new-end end)
                   (= new-beg beg))
              (evil-yank beg end type register yank-handler)
            (evil-yank new-beg new-end type register yank-handler)))
      (error (let* ((beg (evil-cp--new-beginning beg end :shrink))
                    (end (evil-cp--new-ending beg end)))
               (evil-yank beg end type register yank-handler))))))

(defun evil-cp-yank-form-handler (text)
  "Copy of `evil-yank-line-handler' that handles the lack of
newlines at the end of forms."
  (let* ((pcount (or evil-paste-count 1))
         (text (apply #'concat
                      (cdr (-interleave
                            (make-list pcount "\n")
                            (make-list pcount (string-trim text))))))
         (opoint (point)))
    (remove-list-of-text-properties
     0 (length text) yank-excluded-properties text)
    (cond
     ((eq this-command 'evil-paste-before)
      (evil-move-beginning-of-line)
      (evil-move-mark (point))
      (insert text "\n")
      (setq evil-last-paste
            (list 'evil-paste-before
                  evil-paste-count
                  opoint
                  (mark t)
                  (point)))
      (evil-set-marker ?\[ (mark))
      (evil-set-marker ?\] (1- (point)))
      (evil-exchange-point-and-mark)
      (back-to-indentation))
     ((eq this-command 'evil-paste-after)
      (evil-move-end-of-line)
      (evil-move-mark (point))
      (insert "\n")
      (insert text)
      (evil-set-marker ?\[ (1+ (mark)))
      (evil-set-marker ?\] (1- (point)))
      (setq evil-last-paste
            (list 'evil-paste-after
                  evil-paste-count
                  opoint
                  (mark t)
                  (point)))
      (evil-move-mark (1+ (mark t)))
      (evil-exchange-point-and-mark)
      (back-to-indentation))
     (t
      (insert text)))))

(evil-define-operator evil-cp-yank (beg end type register yank-handler)
  "Saves the characters in motion into the kill-ring while
respecting parentheses."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (let ((evil-was-yanked-without-register
         (and evil-was-yanked-without-register (not register)))
        (safep (sp-region-ok-p beg end)))
    (cond
     ;; block which is balanced
     ((and (eq type 'block) (evil-cp--balanced-block-p beg end))
      (evil-yank-rectangle beg end register yank-handler))

     ;; unbalanced block, add parens
     ((and (eq type 'block)
           evil-cleverparens-complete-parens-in-yanked-region)
      (evil-cp--yank-rectangle beg end register yank-handler))

     ;; unbalanced block, ignoring parens
     ((eq type 'block)
      (evil-cp--fail))

     ;; balanced region, or override
     ((or safep (evil-cp--override))
      (evil-yank beg end type register yank-handler))

     ;; unbalanced line, fill parens
     ((and (eq type 'line)
           evil-cleverparens-complete-parens-in-yanked-region)
      (evil-cp--yank-characters beg end register
                                'evil-cp-yank-form-handler))

     ((eq type 'line)
      (let ((beg (+ beg (save-excursion ; skip preceding whitespace
                          (beginning-of-line)
                          (sp-forward-whitespace t)))))
        (evil-cp--ignoring-yank beg end type register
                                'evil-yank-line-handler)))

     ;; unbalanced, fill missing
     (evil-cleverparens-complete-parens-in-yanked-region
      (evil-cp--yank-characters beg end register yank-handler))

     ;; unbalanced, ignore extra
     (t
      (evil-cp--ignoring-yank beg end type register yank-handler)))))

(evil-define-operator evil-cp-yank-line (beg end type register)
  "Acts like `paredit-copy-sexp-as-kill'."
  :motion evil-line
  :move-point nil
  (interactive "<R><x>")
  (cond
   ((evil-visual-state-p)
    (let ((beg (line-beginning-position))
          (end (if (eq type 'line)
                   (1- end)
                 (save-excursion
                   (goto-char end)
                   (line-end-position)))))
      (evil-exit-visual-state)
      (evil-cp--yank-characters beg end type register)))

   ;; Copied and modified from paredit.el
   ((paredit-in-string-p)
    (save-excursion
      (when (paredit-in-string-escape-p)
        (backward-char))
      (evil-yank-characters (point)
                            (min (line-end-position)
                                 (cdr (paredit-string-start+end-points)))
                            register)))

   ((paredit-in-comment-p)
    (evil-yank-characters (point) (line-end-position) register))

   ((save-excursion (paredit-skip-whitespace t (line-end-position))
                    (or (eolp) (eq (char-after) ?\; )))
    (save-excursion
      (if (paredit-in-char-p)
          (backward-char)))
    (evil-yank-characters (point) (line-end-position) register))

   (t
    (save-excursion
      (if (paredit-in-char-p)
          (backward-char 2))
      (let ((beginning (point))
            (eol (line-end-position)))
        (let ((end-of-list-p (paredit-forward-sexps-to-kill beginning eol)))
          (when end-of-list-p
            (up-list)
            (backward-char))
          (evil-yank-characters
           beginning
           (cond
            (kill-whole-line
             (or (save-excursion
                   (paredit-skip-whitespace t)
                   (and (not (eq (char-after) ?\; ))
                        (point)))
                 (line-end-position)))
            ((and (not end-of-list-p)
                  (eq (line-end-position) eol))
             eol)
            (t
             (point)))
           register)))))))

(defun evil-cp--delete-characters (beg end)
  "Deletes everything except unbalanced parentheses / string
delimiters in the region defined by BEG and END."
  (let ((chars-left (- end beg))
        (end (set-marker (make-marker) end)))
    (goto-char beg)
    (while (> chars-left 0)
      (cond
       ((evil-cp--point-in-comment)
        (let* ((ending (cdr (or (sp-get-comment-bounds)
                                (sp-get-quoted-string-bounds))))
               (diff (- (min end ending) (point))))
          (delete-char diff)
          (setq chars-left (- chars-left diff))))
       ((evil-cp--looking-at-any-opening-p)
        (let ((p (point))
              (other-end (evil-cp--matching-paren-pos)))
          ;; matching paren is in the range of the command
          (if (<= p other-end end)
              ;; 1+ makes the char-count inclusive
              (let ((char-count (1+ (- other-end p))))
                (delete-char char-count)
                (setq chars-left (- chars-left char-count)))
            (forward-char)
            (cl-decf chars-left))))
       ((and (evil-cp--looking-at-escape-p) (< 1 chars-left))
        (delete-char 2)
        (cl-decf chars-left 2))
       ((and (evil-cp--looking-at-any-closing-p) (= chars-left 1))
        (cl-decf chars-left))
       ((evil-cp--looking-at-any-closing-p)
        (forward-char 1)
        (cl-decf chars-left))
       (t
        (delete-char 1)
        (cl-decf chars-left))))
    (set-marker end nil)))

(defun evil-cp-first-non-blank-non-opening ()
  "Like `evil-first-non-blank' but also skips opening parentheses
and string delimiters."
  (interactive)
  (evil-first-non-blank)
  (while (and (evil-cp--looking-at-any-opening-p)
             (<= (point) (line-end-position)))
    (forward-char)))

(defun evil-cp--act-until-closing (beg end action)
  "Do ACTION on all balanced expressions, starting at BEG.
Stop ACTION when the first unbalanced closing delimeter or eol is reached."
  (goto-char beg)
  (let ((endp nil)
        (end (set-marker (make-marker) end)))
    (while (not endp)
      (cond
       ((<= end (point)) (setq endp t))
       ((evil-cp--looking-at-any-opening-p)
        (let ((other-end (evil-cp--matching-paren-pos)))
          ;; matching paren is in the range of the command
          (let ((char-count
                 (evil-cp--guard-point
                  (sp-get (sp-get-enclosing-sexp)
                    (- :end :beg)))))
            (funcall action char-count))))
       ((evil-cp--looking-at-any-closing-p)
        (setq endp t))
       (t (funcall action 1))))
    (set-marker end nil)))

(evil-define-operator evil-cp-delete-line (beg end type register yank-handler)
  "Kills the balanced expressions on the line until the eol."
  :motion nil
  :keep-visual t
  :move-point nil
  ;; TODO this could take a count, like `evil-delete-line'
  (interactive "<R><x>")
  (when (and (evil-visual-state-p) (eq type 'inclusive))
    (let ((range (evil-expand
                  beg end
                  (if (and evil-respect-visual-line-mode visual-line-mode)
                      'screen-line 'line))))
      (setq beg (car range)
            end (cadr range)
            type (evil-type range))))
  (unless beg (setq beg (point)))
  (unless end (setq end (line-end-position)))
  (cond ((evil-cp-region-ok-p beg end)
         (evil-delete-line beg end type register yank-handler))

        ((paredit-in-string-p)
         (save-excursion
           (when (paredit-in-string-escape-p)
             (backward-char))
           (let ((beg (point))
                 (end (min (line-end-position)
                           (cdr (paredit-string-start+end-points)))))
             (evil-yank-characters beg end register)
             (delete-region beg end))))

        ((paredit-in-comment-p)
         (let ((beg (point))
               (end (line-end-position)))
           (evil-yank-characters beg end register)
           (delete-region beg end)))

        ((evil-visual-state-p)
         (progn (evil-force-normal-state)
                (goto-char beg)
                (evil-cp-delete-line beg end type register yank-handler)))

        ((and (evil-cp--looking-at-any-closing-p)
              (evil-cp--looking-at-empty-form))
         (evil-cp-delete-enclosing 1))

        ((save-excursion (paredit-skip-whitespace t (line-end-position))
                         (or (eolp) (eq (char-after) ?\; )))
         (when (paredit-in-char-p) (backward-char))
         ;; `kill-line' inlined from from `simple.el'
         ;; this works but is not very evilidiomatic
         (kill-region
          (point)
          (progn
            (when (eobp) (evil-signal-at-eob))
            (let ((end (save-excursion (end-of-visible-line) (point))))
              (if (or (save-excursion
                        ;; If trailing whitespace is visible,
                        ;; don't treat it as nothing.
                        (unless show-trailing-whitespace
                          (skip-chars-forward " \t" end))
                        (= (point) end))
                      (and kill-whole-line (bolp)))
                  (forward-visible-line 1)
                (goto-char end)))
            (point))))

        (t
         (save-excursion
           (when (paredit-in-char-p) (backward-char 2))
           (let* ((beg (point))
                  (end (progn (evil-cp--act-until-closing beg end #'forward-char) (point))))
             (evil-yank-characters beg end register)
             (evil-cp--act-until-closing beg end #'delete-char))))))

(evil-define-operator evil-cp-delete (beg end type register yank-handler)
  "A version of `evil-delete' that attempts to leave the region
it's acting on with balanced parentheses. The behavior of
kill-ring is determined by the
`evil-cleverparens-complete-parens-in-yanked-region' variable."
  :move-point nil
  (interactive "<R><x><y>")
  (let ((safep (evil-cp-region-ok-p beg end)))
    (cond ((or (= beg end)
               (evil-cp--override)
               (and (eq type 'block) (evil-cp--balanced-block-p beg end))
               (and safep (not (eq type 'block))))
           (evil-delete beg end type register yank-handler))

          ((eq type 'line)
           (evil-cp-yank beg end type register yank-handler)
           (goto-char beg)
           (save-excursion
             (evil-cp--delete-characters
              (+ beg
                 (save-excursion  ; skip preceding whitespace
                   (beginning-of-line)
                   (sp-forward-whitespace t)))
              (if (save-excursion (goto-char end) (eobp))
                  end
                (1- end))))
           (when (and evil-cleverparens-indent-afterwards
                      (evil-cp--inside-any-form-p))
             (save-excursion
               (evil-cp--backward-up-list)
               (indent-sexp)))
           (if (save-excursion
                 (skip-chars-forward "\t ")
                 (evil-cp--looking-at-any-closing-p))
               (when (save-excursion
                       (forward-line -1)
                       (not (evil-cp--comment-block?)))
                 (forward-line -1)
                 (join-line 1)
                 (forward-line 1))
             (join-line 1)))

          (t (evil-cp-yank beg end type register yank-handler)
             (evil-cp--delete-characters beg end))))

  (when (and (eq type 'line)
             (called-interactively-p 'any))
    (evil-cp-first-non-blank-non-opening)
    (when (and (not evil-start-of-line)
               evil-operator-start-col
               ;; Special exceptions to ever saving column:
               (not (memq evil-this-motion '(evil-forward-word-begin
                                             evil-forward-WORD-begin
                                             evil-cp-forward-symbol-begin))))
      (move-to-column evil-operator-start-col))))

(evil-define-operator evil-cp-change (beg end type register yank-handler delete-func)
  "Call `evil-change' while keeping parentheses balanced."
  (interactive "<R><x><y>")
  (if (or (= beg end)
          (evil-cp--override)
          (and (eq type 'block) (evil-cp--balanced-block-p beg end))
          (and (sp-region-ok-p beg end) (not (eq type 'block))))
      (evil-change beg end type register yank-handler delete-func)
    (let ((delete-func (or delete-func #'evil-cp-delete))
          (nlines (1+ (- (line-number-at-pos end)
                         (line-number-at-pos beg))))
          (opoint (save-excursion
                    (goto-char beg)
                    (line-beginning-position))))
      (cond ((eq type 'line)
             (save-excursion
               (evil-cp--delete-characters
                (+ beg
                   (save-excursion
                     (beginning-of-line)
                     (sp-forward-whitespace t)))
                (1- end)))
             (evil-cp-first-non-blank-non-opening)
             (indent-according-to-mode)
             (evil-insert 1))

            ((eq type 'block)
             (evil-cp-delete beg end type register yank-handler)
             (evil-insert 1 nlines))

            (t
             (funcall delete-func beg end type register yank-handler)
             (evil-insert 1))))))

(evil-define-operator evil-cp-change-line (beg end type register yank-handler)
  "Change to end of line while respecting parentheses."
  :motion evil-end-of-line
  (interactive "<R><x><y>")
  (evil-cp-change beg end type register yank-handler #'evil-cp-delete-line))

(evil-define-operator evil-cp-change-whole-line (beg end type register yank-handler)
  "Change whole line while respecting parentheses."
  :motion evil-line
  (interactive "<R><x>")
  (if (sp-region-ok-p beg end)
      (evil-change beg end type register yank-handler)
    (evil-cp-first-non-blank-non-opening)
    (evil-cp-change beg end type register yank-handler #'evil-cp-delete-line)))


;;; Movement ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(evil-define-motion evil-cp-forward-sexp (count)
  "Motion for moving forward by a sexp via `sp-forward-sexp'."
  :type exclusive
  (let ((count (or count 1)))
    (when (evil-eolp) (forward-char))
    (sp-forward-sexp count)))

(evil-define-motion evil-cp-backward-sexp (count)
  "Motion for moving backwward by a sexp via `sp-backward-sexp'."
  :type exclusive
  (let ((count (or count 1)))
    (sp-backward-sexp count)))

(evil-define-motion evil-cp-beginning-of-defun (count)
  "Motion for moving to the beginning of a defun (i.e. a top
level sexp)."
  :type inclusive
  (let ((count (or count 1)))
    (beginning-of-defun count)))

(evil-define-motion evil-cp-end-of-defun (count)
  "Motion for moving to the end of a defun (i.e. a top level sexp)."
  :type inclusive
  (let ((count (or count 1)))
    (if (evil-cp--looking-at-closing-p)
        (forward-char 2))
    (end-of-defun count)
    (if (looking-back "[ \t\n]" 1) (skip-chars-backward " \t\n"))
    (backward-char 1)))

;; TODO: this looks ugly
(defun evil-cp--paren-navigation-helper (move-dir paren-side)
  (let ((move-fn (cl-case move-dir
                   (:next 'forward-char)
                   (:previous 'backward-char)))
        (the-end (cl-case move-dir
                   (:next 'point-max)
                   (:previous 'point-min)))
        (paren-p (cl-case paren-side
                   (:opening 'evil-cp--looking-at-any-opening-p)
                   (:closing 'evil-cp--looking-at-any-closing-p)))
        (pt-orig (point))
        done-p)
    (when (funcall paren-p) (funcall move-fn))
    (while (not done-p)
      (cond
       ((funcall paren-p)
        (setq done-p t))
       ((= (point) (funcall the-end))
        (goto-char pt-orig)
        (setq done-p t))
       (t
        (funcall move-fn))))))

(evil-define-motion evil-cp-next-opening (count)
  "Motion for moving to the next open parentheses."
  :move-point nil
  :type inclusive
  (let ((count (or count 1)))
    (dotimes (_ count)
      (evil-cp--paren-navigation-helper :next :opening))))

(evil-define-motion evil-cp-previous-opening (count)
  "Motion for moving to the previous open parentheses."
  :move-point nil
  :type inclusive
  (let ((count (or count 1)))
    (dotimes (_ count)
      (evil-cp--paren-navigation-helper :previous :opening))))

(evil-define-motion evil-cp-next-closing (count)
  "Motion for moving to the next closing parentheses."
  :move-point nil
  :type inclusive
  (let ((count (or count 1)))
    (dotimes (_ count)
      (evil-cp--paren-navigation-helper :next :closing))))

(evil-define-motion evil-cp-previous-closing (count)
  "Motion for moving to the previous closing parentheses."
  :move-point nil
  :type inclusive
  (let ((count (or count 1)))
    (dotimes (_ count)
      (evil-cp--paren-navigation-helper :previous :closing))))

(evil-define-motion evil-cp-backward-up-sexp (count)
  "Motion for moving backward up to the previous level of
parentheses. Basically just wraps `sp-backward-up-sexp' as an
evil-motion."
  :move-point nil
  :type inclusive
  (let ((count (or count 1)))
    ;; for some reason calling `sp-backward-up-sexp' with a large `count'
    ;; doesn't move the point at all
    (dotimes (_ count)
      (sp-backward-up-sexp))))

(evil-define-motion evil-cp-up-sexp (count)
  "Motion for moving up to the previous level of parentheses.
The same as `sp-up-sexp', but leaves the point on top of the
closing paren."
  :move-point nil
  :type inclusive
  (let ((count (or count 1))
        success)
    (when (evil-cp--looking-at-any-closing-p) (forward-char))
    (dotimes (_ count)
      (and (sp-up-sexp) (setq success t)))
    (and success (backward-char))))

(defun evil-cp--strict-forward-symbol (&optional count)
  "Move forward COUNT symbols.
Like `forward-symbol' but stricter.  Skips sexp prefixes."
  (let ((mmode-prefix (cdr (assoc major-mode sp-sexp-prefix))))
    (cond ((< 0 count)
           (forward-symbol 1)
           (if (and (eq (car mmode-prefix) 'regexp)
                    (looking-at-p (evil-cp--get-opening-regexp))
                    (looking-back (cadr mmode-prefix) 1))
               (evil-cp--strict-forward-symbol count)
             (evil-cp--strict-forward-symbol (1- count))))

          ((> 0 count)
           (forward-symbol -1)
           (if (and (eq (car mmode-prefix) 'regexp)
                    (looking-at-p (string-join (list (cadr mmode-prefix)
                                                     (evil-cp--get-opening-regexp)))))
               (evil-cp--strict-forward-symbol count)
             (evil-cp--strict-forward-symbol (1+ count)))))))

(defun forward-evil-cp-symbol (&optional count)
  "Move forward COUNT \"WORDS\".
Moves point COUNT WORDS forward or (- COUNT) WORDS backward if
COUNT is negative. Point is placed after the end of the WORD (if
forward) or at the first character of the WORD (if backward). A
WORD is a sequence of non-whitespace characters
'[^\\n\\r\\t\\f ]', or an empty line matching ^$."
  (evil-forward-nearest
   count
   #'(lambda (&optional cnt)
       (let ((pnt (point)))
         (evil-cp--strict-forward-symbol cnt)
         (if (= pnt (point)) cnt 0)))
   #'forward-evil-empty-line))

;; incomplete
(defun forward-evil-cp-word (&optional count)
  "TODO: This is exactly the same as `forward-evil-word' and not used
for anything right now. It would be nice to skip delimiters on
the small-word movements as well but I couldn't figure out how to
change this to make it work. Pull requests welcome."
  (evil-forward-nearest
   count
   #'(lambda (&optional cnt)
       (let ((word-separating-categories evil-cjk-word-separating-categories)
             (word-combining-categories evil-cjk-word-combining-categories)
             (pnt (point)))
         (forward-word cnt)
         (if (= pnt (point)) cnt 0)))
   #'(lambda (&optional cnt)
       (evil-forward-chars "^[:word:]\n\r\t\f " cnt))
   #'forward-evil-empty-line))

(defun evil-cp--forward-X-begin (thing count)
  "TODO: see `forward-evil-cp-word' which is currently not
working. Could be used to implement a future
`evil-cp-forward-word-begin' the same way that
`evil-cp-forward-symbol-begin' is defined."
  (let ((orig (point)))
    (evil-signal-at-bob-or-eob count)
    (cond ((not (evil-operator-state-p))
           (evil-forward-beginning thing count))

          ((and evil-want-change-word-to-end
                (memq evil-this-operator evil-change-commands)
                (< orig (or (cdr-safe (bounds-of-thing-at-point thing)) orig)))
           (forward-thing thing count))

          (t (prog1 (evil-forward-beginning thing count)
               (when (and (> (line-beginning-position) orig)
                          (looking-back "^[[:space:]]*" (line-beginning-position)))
                 (evil-move-end-of-line 0)
                 (while (and (looking-back "^[[:space:]]+$" (line-beginning-position))
                             (not (<= (line-beginning-position) orig)))
                   (evil-move-end-of-line 0))
                 (when (bolp) (forward-char))))))))

(evil-define-motion evil-cp-forward-symbol-begin (count)
  :type exclusive
  (evil-cp--forward-X-begin (if evil-cleverparens-move-skip-delimiters
                                'evil-cp-symbol
                              'evil-symbol)
                            (or count 1)))

(defun evil-cp--forward-X-end (thing count)
  (evil-signal-at-bob-or-eob count)
  (unless (and (evil-operator-state-p)
               (= 1 count)
               (let ((bnd (bounds-of-thing-at-point thing)))
                 (and bnd
                      (= (car bnd) (point))
                      (= (cdr bnd) (1+ (point)))))
               (looking-at "[[:word:]]"))
    (evil-forward-end thing count)))

(evil-define-motion evil-cp-forward-symbol-end (count)
  "Copy of `evil-forward-word-end' using 'evil-symbol for the
movement."
  :type inclusive
  (evil-cp--forward-X-end (if evil-cleverparens-move-skip-delimiters
                              'evil-cp-symbol
                            'evil-symbol)
                          (or count 1)))

(evil-define-motion evil-cp-backward-symbol-begin (count)
  "Copy of `evil-backward-word-begin' using 'evil-symbol for the
movement."
  :type exclusive
  (let ((thing (if evil-cleverparens-move-skip-delimiters
                   'evil-cp-symbol
                 'evil-symbol)))
    (evil-signal-at-bob-or-eob (- (or count 1)))
    (evil-backward-beginning thing count)))

(evil-define-motion evil-cp-backward-symbol-end (count)
  "Copy of `evil-backward-word-end' using 'evil-symbol for the
movement."
  :type inclusive
  (let ((thing (if evil-cleverparens-move-skip-delimiters
                   'evil-cp-symbol
                 'evil-symbol)))
    (evil-signal-at-bob-or-eob (- (or count 1)))
    (evil-backward-end thing count)))


;;; Additional Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is a lot slower than the approach below, but should behave correctly
;; with modes that use `sp-sexp-prefix'. Might be worth re-writing in the
;; future.
(defun evil-cp--slurp-backward-from-opening ()
  (catch 'abort
    (let ((depth 1))
      (save-excursion
        (while (evil-cp--first-form-of-form-p)
          (evil-cp--backward-up-list)
          (cl-incf depth))
        (when (and (= (evil-cp--point-after (backward-sexp)) 1)
                   (evil-cp--looking-at-whitespace-p))
          (throw 'abort nil)))
      (forward-char)
      (dotimes (_ depth)
        (sp-backward-slurp-sexp))
      (while (looking-back " " (1- (point)))
        (backward-delete-char 1))
      (insert " ")
      (backward-sexp)
      (backward-char)
      (indent-sexp))))

(defun evil-cp--slurp-forward-from-closing ()
  (let* ((pt (point))
         (next-thing
          (save-excursion
            (while (evil-cp--last-form-of-form-p)
              (evil-cp--up-list))
            (evil-cp--up-list)
            (when (not (eq :eobp (evil-cp--skip-whitespace-and-comments)))
              (evil-cp--movement-bounds (forward-sexp)))))
         delims)
    (when next-thing
      (save-excursion
        (goto-char (1- (car next-thing)))
        (while (<= pt (point))
          (cond
           ((and (evil-cp--looking-at-any-closing-p)
                 (not (evil-cp--point-in-string-or-comment)))
            (push (following-char) delims)
            (delete-char 1))
           ((and (looking-at-p " ")
                 (not (evil-cp--point-in-string-or-comment)))
            (delete-char 1)))
          (backward-char)))
      (if (looking-at-p evil-cp--ws-regexp)
          (evil-cp--skip-whitespace-and-comments)
        (insert " "))
      (forward-sexp)
      (insert (apply #'string delims))
      (backward-char (length delims))
      (save-excursion
        (goto-char pt)
        (evil-cp--backward-up-list)
        (indent-sexp)))))

(defun evil-cp--singleton-list-p ()
  "Checks that the list the point is in contains more than one
sexp inside it. Should not be called in an empty list."
  (when (evil-cp--inside-any-form-p)
    (save-excursion
      (goto-char (1+ (car (evil-cp--get-enclosing-bounds))))
      (forward-sexp)
      (evil-cp--cant-forward-sexp-p))))

(evil-define-command evil-cp-< (count)
  "Slurping/barfing operation that acts differently based on the
point's location in the form.

When point is on the opening delimiter of the form boundary, it
will slurp the next element backwards while maintaining the
location of the point in the original delimiter.

  foo [(]bar baz) -> [(]foo bar baz)

When point is on the closing delimiter, it will barf the
rightmost element of the form forward, again maintaining the
location of the point in the original delimiter. Only works if
the form has more than one element.

  (bar baz foo[)] -> (bar baz[)] foo

When point is in the middle of a form, it will act as like a
regular forward-barf."
  (interactive "<c>")
  (setq count (or count 1))
  (when (evil-cp--inside-any-form-p)
    (dotimes (_ count)
      (cond
       ((evil-cp--looking-at-any-opening-p)
        (evil-cp--slurp-backward-from-opening))
       ((evil-cp--looking-at-any-closing-p)
        (when (not (sp-point-in-empty-sexp))
          (when (not (evil-cp--singleton-list-p))
            (sp-forward-barf-sexp)
            (sp-backward-sexp)
            (evil-cp-previous-closing))))
       ((evil-cp--singleton-list-p)
        (save-excursion
          (while (and (evil-cp--singleton-list-p)
                      (not (evil-cp--outside-form-p)))
            (evil-cp--backward-up-list))
          (when (not (evil-cp--singleton-list-p))
            (forward-char)
            (sp-forward-barf-sexp))))
       (t
        (sp-forward-barf-sexp))))))

(evil-define-command evil-cp-> (count)
  "Slurping/barfing operation that acts differently based on the
points location in the form.

When cursor is on the opening delimiter of the form boundary, it
will barf the first element of the form while maintaining the
location of the cursor on the original delimiter.

  [(]foo bar baz) -> foo [(]bar baz)

When the cursor is on a closing delimiter, it will slurp the
first sexp after the last closing delimiter from point moving it
inside the form of which the cursor is currently on top of while
maintaining the location of the cursor on the current closing
delimiter.

  (((bar baz[)])) foo -> (((bar baz foo[)]))

When point is in the middle of a form, it will act as like a
regular forward-slurp."
  (interactive "<c>")
  (setq count (or count 1))
  (when (evil-cp--inside-any-form-p)
    (dotimes (_ count)
      (cond
       ((evil-cp--looking-at-any-closing-p)
        (evil-cp--slurp-forward-from-closing))
       ((evil-cp--looking-at-any-opening-p)
        (when (not (or (evil-cp--looking-at-empty-form)
                       (evil-cp--singleton-list-p)))
          (forward-char)
          (let (sp-barf-move-point-with-delimiter) ; It's not helpful here
            (sp-backward-barf-sexp))
          (sp-forward-sexp)
          (evil-cp-next-opening)))
       (t
        (sp-forward-slurp-sexp))))))


(defun evil-cp--symbol-bounds ()
  (save-excursion
    (if (looking-at-p "\\_<")
        (evil-cp--sp-obj-bounds (sp-get-symbol))
      (when (sp-point-in-symbol)
        (evil-cp--sp-obj-bounds (sp-get-symbol))))))

(defun evil-cp--cant-forward-sexp-p ()
  (save-excursion
    (condition-case nil
        (forward-sexp)
      (error t))))

(defun evil-cp--cant-backward-sexp-p ()
  (save-excursion
    (condition-case nil
        (backward-sexp)
      (error t))))

(defun evil-cp--last-symbol-of-form-p ()
  (save-excursion
    (-when-let (end (cdr (evil-cp--symbol-bounds)))
      (goto-char end)
      (evil-cp--cant-forward-sexp-p))))

(defun evil-cp--first-symbol-of-form-p ()
  (save-excursion
    (-when-let (beg (car (evil-cp--symbol-bounds)))
      (goto-char beg)
      (evil-cp--cant-backward-sexp-p))))

(defun evil-cp--first-form-of-form-p ()
  (when (evil-cp--inside-any-form-p)
    (save-excursion
      (evil-cp--guard-point
       (evil-cp-backward-up-sexp)
       (evil-cp--cant-backward-sexp-p)))))

(defun evil-cp--last-form-of-form-p ()
  (when (evil-cp--inside-any-form-p)
    (save-excursion
      (evil-cp--guard-point
       (evil-cp--up-list)
       (evil-cp--cant-forward-sexp-p)))))

(defun evil-cp--next-thing-bounds* (by-line-p)
  "Fetches the bounds for the next \"thing\" from the location of
  point. The thing may be a symbol, a form, a line or a comment
  block. When BY-LINE-P is true, defaults to swapping by line and
  only swaps by form if the next line is not safe. May move the
  point. Acts as a helper for `evil-cp--next-thing-bounds'."
  (let* ((next-form
          (save-excursion
            (evil-cp--skip-whitespace-and-comments)
            (evil-cp--movement-bounds (forward-sexp))))
         (next-line
          (or (when (not (= (line-end-position) (point-max)))
                (save-excursion
                  (forward-line)
                  (evil-cp--comment-block-bounds)))
              (evil-cp--safe-line-bounds (forward-line)))))
    (cond
     (by-line-p (or next-line next-form))
     (evil-cleverparens-drag-ignore-lines next-form)
     ((and next-line next-form
           (<= (cdr next-form) (cdr next-line)))
      next-form)
     (t (or next-line next-form)))))

(defun evil-cp--previous-thing-bounds* (by-line-p)
  "Fetches the bounds for the previous \"thing\" from the
  location of point. The thing may be a symbol, a form, a line or
  a comment block. When BY-LINE-P is true, defaults to swapping
  by line and only swaps by form if the next line is not safe.
  May move the point. Acts as a helper for
  `evil-cp--previous-thing-bounds'."
  (let ((prev-form
         (save-excursion
           (backward-sexp)
           (when (not (bobp))
             (evil-cp--next-sexp-bounds))))
        (prev-line
         (when (not (= (line-beginning-position) (point-min)))
           (or (save-excursion
                 (forward-line -1)
                 (evil-cp--comment-block-bounds))
               (evil-cp--safe-line-bounds (forward-line -1))))))
    (cond
     (by-line-p (or prev-line prev-form))
     (evil-cleverparens-drag-ignore-lines prev-form)
     ((and prev-line prev-form
           (>= (cdr prev-form) (cdr prev-line)))
      prev-form)
     (t (or prev-line prev-form)))))

(defun evil-cp--swap-with-next (this-bounds &optional by-line-p)
  "Swaps the region defined by THIS-BOUNDS with the next thing
from the end of THIS-BOUNDS."
  (when this-bounds
    (let ((that-bounds (evil-cp--next-thing-bounds
                        (goto-char (cdr this-bounds))
                        by-line-p)))
      (if (not (= (point) (line-end-position)))
          (evil-cp--swap-regions this-bounds that-bounds)
        (progn
          (backward-char)
          (evil-cp--swap-regions this-bounds that-bounds)
          (forward-char)))
      (when evil-cleverparens-indent-afterwards
        (save-excursion
          (goto-char (car this-bounds))
          (evil-cp--backward-up-list t)
          (indent-sexp))))))

(defun evil-cp--swap-with-previous (this-bounds &optional by-line-p)
  "Swaps the region defined by THIS-BOUNDS with the previous
thing from the beginning of of THIS-BOUNDS."
  (when this-bounds
    (let ((that-bounds (evil-cp--previous-thing-bounds
                        (goto-char (car this-bounds))
                        by-line-p)))
      (when that-bounds
        (if (not (= (point) (line-end-position)))
            (evil-cp--swap-regions this-bounds that-bounds)
          (backward-char)
          (evil-cp--swap-regions this-bounds that-bounds)
          (forward-char))
        (when evil-cleverparens-indent-afterwards
          (save-excursion
            (goto-char (car that-bounds))
            (evil-cp--backward-up-list t)
            (indent-sexp)))))))

(defun evil-cp--comment-block? ()
  "Checks whether point is on a line that starts with whitespace
or a comment."
  (save-excursion
    (evil-first-non-blank)
    (and sp-comment-char (looking-at-p sp-comment-char))))

(defun evil-cp--comment-block-bounds (&optional pos)
  "Gets the bounds for a comment block (i.e. a group of lines
that start with whitespace or a comment)."
  (save-excursion
    (when pos (goto-char pos))
    (when (evil-cp--comment-block?)
      (let (beg end)
        (save-excursion
          (beginning-of-line)
          (setq beg (point))
          (while (and (not (bobp)) (evil-cp--comment-block?))
            (forward-line -1)
            (when (evil-cp--comment-block?) (setq beg (point)))))
        (save-excursion
          (end-of-line)
          (setq end (point))
          (while (and (not (eobp)) (evil-cp--comment-block?))
            (forward-line 1)
            (end-of-line)
            (when (evil-cp--comment-block?) (setq end (point)))))
        (cons beg end)))))

(evil-define-command evil-cp-drag-forward (count)
  "Drags the thing under point forward. The thing can be either a
symbol, a form, a line or a comment block. Does not mess with the
depth of expressions by slurping or barfing. If the thing under
point is at the end of a form then tries to drag its \"parent\"
thing forward instead. Maintains the location of point relative
to the thing being dragged."
  (interactive "<c>")
  (if (evil-visual-state-p)
      (let* ((v-range (evil-visual-range))
             (linep   (eq 'line (evil-visual-type)))
             (beg     (car v-range))
             (end     (if linep (1- (cadr v-range)) (cadr v-range))))
        (when (sp-region-ok-p beg end)
          (evil-cp--swap-with-next (cons beg end) linep)))
    (let (drag-by-line-p)
      (evil-cp--swap-with-next
       (let ((sym-bounds (evil-cp--symbol-bounds))
             (in-form-p (evil-cp--inside-any-form-p)))
         (cond
          ((and sym-bounds
                (not (evil-cp--point-in-string-or-comment))
                (not (evil-cp--last-symbol-of-form-p)))
           sym-bounds)
          ((and in-form-p (evil-cp--point-in-comment))
           (save-excursion
             (evil-cp--backward-up-list)
             (evil-cp--next-sexp-bounds)))
          (in-form-p
           (save-excursion
             (while (evil-cp--last-form-of-form-p)
               (when (evil-cp--looking-at-any-opening-p)
                 (forward-char))
               (evil-cp--up-list))
             (evil-cp--get-enclosing-bounds t)))
          (t
           (setq drag-by-line-p t)
           (if (and evil-cleverparens-drag-comment-blocks
                    (evil-cp--comment-block?))
               (evil-cp--comment-block-bounds)
             (evil-cp--safe-line-bounds)))))
       drag-by-line-p))))

(evil-define-command evil-cp-drag-backward (count)
  "Drags the thing under point backward. The thing can be either
a symbol, a form, a line or a comment block. Does not mess with
the depth of expressions by slurping or barfing. If the thing
under point is at the beginning of a form then tries to drag its
\"parent\" thing backward instead. Maintains the location of
point relative to the thing being dragged."
  (interactive "<c>")
  (if (evil-visual-state-p)
      (let* ((v-range (evil-visual-range))
             (linep   (eq 'line (evil-visual-type)))
             (beg     (car v-range))
             (end     (if linep (1- (cadr v-range)) (cadr v-range))))
        (when (sp-region-ok-p beg end)
          (evil-cp--swap-with-previous (cons beg end) linep)))
    (let (drag-by-line-p)
      (evil-cp--swap-with-previous
       (let ((sym-bounds (evil-cp--symbol-bounds))
             (in-form-p (evil-cp--inside-any-form-p)))
         (cond
          ((and sym-bounds
                (not (evil-cp--point-in-string-or-comment))
                (not (evil-cp--first-symbol-of-form-p)))
           sym-bounds)
          ((and in-form-p (evil-cp--point-in-comment))
           (save-excursion
             (evil-cp--backward-up-list)
             (evil-cp--next-sexp-bounds)))
          (in-form-p
           (save-excursion
             (when (not (evil-cp--looking-at-any-opening-p))
               (evil-cp--backward-up-list))
             (while (evil-cp--first-form-of-form-p)
               (evil-cp--backward-up-list))
             (evil-cp--get-enclosing-bounds t)))
          (t
           (setq drag-by-line-p t)
           (if (and evil-cleverparens-drag-comment-blocks
                    (evil-cp--comment-block?))
               (evil-cp--comment-block-bounds)
             (evil-cp--safe-line-bounds)))))
       drag-by-line-p))))


(evil-define-operator evil-cp-substitute (beg end type register)
  "Parentheses safe version of `evil-substitute'."
  :motion evil-forward-char
  (interactive "<R><x>")
  (cond
   ((evil-cp--looking-at-any-opening-p)
    (evil-append 1))
   ((evil-cp--looking-at-any-closing-p)
    (evil-insert 1))
   (t (evil-substitute beg end type register))))

(evil-define-command evil-cp-insert-at-end-of-form (count)
  "Move point COUNT times with `sp-forward-sexp' and enter insert
mode at the end of form. Using an arbitrarily large COUNT is
guaranteened to take the point to the beginning of the top level
form."
  (interactive "<c>")
  (let ((line-end (line-end-position)))
    (when (or (when (sp-up-sexp count) (backward-char) t)
              (-when-let (enc-end (cdr (evil-cp--top-level-bounds)))
                (goto-char (1- enc-end))))
      (if (<= (point) line-end)
          (evil-insert 1)
        (insert "\n")
        (indent-according-to-mode)
        (evil-insert 1)))))

(evil-define-command evil-cp-insert-at-beginning-of-form (count)
  "Move point COUNT times with `sp-backward-up-sexp' and enter
insert mode at the beginning of the form. Using an arbitrarily
large COUNT is guaranteened to take the point to the beginning
of the top level form."
  (interactive "<c>")
  (when (or (when (sp-backward-up-sexp count) (forward-char) t)
            (-when-let (enc-beg (car (evil-cp--top-level-bounds)))
              (goto-char (1+ enc-beg))))
    (if evil-cleverparens-use-regular-insert
        (evil-insert 1)
      (evil-cp-insert 1))))

(evil-define-command evil-cp-copy-paste-form (count)
  "Copies the surrounding form and inserts it below itself. If
called with a single prefix-argument, will copy the top-level
sexp regardless of what level the point is currently at."
  (interactive "<c>")
  (setq count (or count 1))
  (if (evil-cp--inside-any-form-p)
      (let* ((prefixp (consp current-prefix-arg))
             (bounds
              (if prefixp
                  (evil-cp--top-level-bounds)
                (evil-cp--get-enclosing-bounds t)))
             (beg (car bounds))
             (end (cdr bounds))
             (offset (- end (point)))
             (text (buffer-substring-no-properties beg end)))
        (when (and beg end)
          (if prefixp
              (progn
                (end-of-defun)
                (insert "\n" text "\n"))
            (when (evil-cp--looking-at-any-opening-p)
              (forward-char))
            (sp-up-sexp)
            (dotimes (_ count)
              (insert "\n")
              (indent-according-to-mode)
              (insert text)
              (when (or (looking-at "\\b")
                        (evil-cp--looking-at-any-opening-p))
                (insert " ")
                (cl-incf offset))))
          (backward-char (if prefixp (1+ offset) offset))))
    (when (not (eobp))
      (let* ((col-pos (current-column))
             (end     (line-end-position))
             (line    (buffer-substring-no-properties (line-beginning-position) end)))
        (goto-char end)
        (insert "\n" line)
        (beginning-of-line)
        (forward-char col-pos)))))


(evil-define-command evil-cp-open-below-form (count)
  "Move COUNT levels up from the current form and enter
insert-state. If the last form is a top-level sexp then two
newlines are inserted instead. If point is not inside a form,
inserts two newlines and enters insert-state between them.

Note thath the COUNT parameter doesn't affect the amount of
times that the inserted text gets output into the buffer, unlike
in `evil-open-below'."
  (interactive "<c>")
  (setq count (or count 1))
  (if (not (evil-cp--inside-any-form-p))
      (progn
        (insert "\n\n")
        (forward-line -1)
        (evil-insert-state))
    (sp-up-sexp count)
    (if (save-excursion
          (backward-char)
          (evil-cp--top-level-form-p))
        (insert "\n\n")
      (insert "\n"))
    (indent-according-to-mode)
    (evil-insert-state)))

(evil-define-command evil-cp-open-above-form (count)
  "Move COUNT levels backwards up from the current form and
enter insert-state. If the form is a top-level sexp then two
newlines are inserted instead.

Note thath the COUNT parameter doesn't affect the amount of
times that the inserted text gets output into the buffer, unlike
in `evil-open-below'."
  (interactive "<c>")
  (setq count (or count 1))
  (sp-backward-up-sexp count)
  (save-excursion
    (if (evil-cp--top-level-form-p)
        (insert "\n\n")
      (insert "\n"))
    (indent-according-to-mode)
    (evil-insert 1)))

(defun evil-cp--kill-sexp-range (count)
  (save-excursion
    (when (not (evil-cp--inside-any-form-p))
      (sp-forward-whitespace))
    (let* ((beg       (point))
           (end       (point))
           (enc-range (evil-cp--get-enclosing-bounds))
           (e-beg     (or (car enc-range) (point)))
           (e-end     (or (cdr enc-range) (point)))
           (n         (or count 1))
           (ok        t))
      (while (and (> n 0) ok)
        (setq ok (sp-forward-sexp 1))
        (when ok
          (sp-get ok
            (when (> :end end) (setq end :end))
            (when (and (>= :end e-end) (> beg :beg))
              (setq beg :beg))))
        (setq n (1- n)))
      (cons beg end))))


(defun evil-cp--del-characters (beg end &optional register yank-handler)
  (evil-yank-characters beg end register yank-handler)
  (delete-region beg end))

(evil-define-command evil-cp-yank-sexp (count &optional register)
  "Copies COUNT many sexps from point to the kill-ring.
Essentially a less greedy version of `evil-cp-yank-line'. When
called with \\[universal-argument], copies everything from point
to the end of the the current form."
  (interactive "<c><x>")
  (if (and (consp current-prefix-arg)
           (evil-cp--inside-any-form-p))
      (sp-get (sp-get-enclosing-sexp)
        (evil-yank-characters (point) (1- :end)))
    (let* ((range (evil-cp--kill-sexp-range count))
           (beg (car range))
           (end (cdr range)))
      (evil-yank-characters (car range) (cdr range) register))))

(evil-define-command evil-cp-delete-sexp (count &optional register)
  "Kills COUNT many sexps from point. Essentially a less greedy
version of `evil-cp-delete-line'. When called with
\\[universal-argument], deletes everything from point to the end
of the the current form."
  (interactive "<c><x>")
  (if (and (consp current-prefix-arg)
           (evil-cp--inside-any-form-p))
      (sp-get (sp-get-enclosing-sexp)
        (evil-cp--del-characters (point) (1- :end)))
    (let* ((range (evil-cp--kill-sexp-range count))
           (beg (car range))
           (end (cdr range)))
      (evil-cp--del-characters beg end register))))

(evil-define-command evil-cp-change-sexp (count &optional register)
  "Like `evil-cp-delete-sexp' but enters insert mode after the
operation. When called with \\[universal-argument], deletes
everything from point to the end of the the current form before
entering insert-state."
  (interactive "<c><x>")
  (evil-cp-delete-sexp count register)
  (evil-insert-state))

(defun evil-cp-top-level-yank-handler (text)
  (insert (concat "\n" (string-trim text) "\n"))
  (backward-char 1))

(evil-define-command evil-cp-yank-enclosing (count &optional register)
  "Copies the enclosing form to kill-ring. With COUNT, yanks the
nth form upwards instead. When called with a raw prefix argument,
yanks the top-level form, while adding a yank-handler that inserts
two newlines at the end and beginning of the copied top-level form."
  (interactive "<c><x>")
  (when (evil-cp--inside-any-form-p)
    (save-excursion
      (if (consp current-prefix-arg)
          (progn
            (beginning-of-defun)
            (sp-get (sp-get-sexp)
              (evil-yank-characters
               :beg
               :end
               register
               #'evil-cp-top-level-yank-handler)))
        (evil-cp-backward-up-sexp count)
        (evil-cp--guard-point
         (sp-get (sp-get-enclosing-sexp)
           (evil-yank-characters :beg :end register)))))))

(defun evil-cp--should-join-line-p ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "\s")
    (or (evil-cp--looking-at-any-closing-p)
        (= (point) (line-end-position)))))

(evil-define-command evil-cp-delete-enclosing (count &optional register)
  "Kills the enclosing form. With COUNT, kills the nth form
upwards instead. When called with a raw prefix argument, kills
the top-level form and deletes the extra whitespace."
  (interactive "<c><x>")
  (when (evil-cp--inside-any-form-p)
    (if (consp current-prefix-arg)
        (progn
          (beginning-of-defun)
          (sp-get (sp-get-sexp)
            (evil-cp--del-characters
             :beg
             :end
             register
             #'evil-cp-top-level-yank-handler))
          (sp-backward-whitespace)
          (let ((del-me (save-excursion (sp-forward-whitespace t))))
            (delete-char (- del-me 2))
            (forward-char)))
      (evil-cp-backward-up-sexp count)
      (evil-cp--guard-point
       (sp-get (sp-get-enclosing-sexp)
         (evil-cp--del-characters :beg :end register)))
      (when (evil-cp--should-join-line-p)
        (forward-line -1)
        (join-line 1)
        (end-of-line))
      (when (looking-back "\s" (1- (point)))
        (delete-char -1)))))

(evil-define-command evil-cp-change-enclosing (count &optional register)
  "Calls `evil-cp-delete-enclosing' and enters insert-state."
  (interactive "<c><x>")
  (when (evil-cp--inside-any-form-p)
    (evil-cp-delete-enclosing count register)
    (when (consp current-prefix-arg)
      (insert "\n\n")
      (backward-char 1))
    (evil-insert-state)))


(defun evil-cp-raise-form (&optional count)
  "Raises the form under point COUNT many times."
  (interactive "P")
  (let ((count (or count 1)))
    (dotimes (_ count)
      (when (evil-cp--inside-any-form-p)
        (let ((orig (point)) offset)
          (evil-cp--guard-point
           (sp-beginning-of-sexp))
          (backward-char)
          (setq offset (- orig (point)))
          (sp-raise-sexp)
          (forward-char offset))))))


(defun evil-cp--wrap-region-with-pair (pair start end)
  "Helper that inserts a pair as indicated by PAIR at positions
  START and END. Performs no safety checks on the regions."
  (-when-let (this-pair (evil-cp-pair-for pair))
    (let ((open (car this-pair))
          (close (cdr this-pair)))
      (save-excursion
        (goto-char start)
        (insert open)
        (goto-char end)
        (insert close)))))

(defun evil-cp--wrap-next (pair count)
  (let ((pt-orig (point))
        (this-pair (evil-cp-pair-for pair))
        end)
    (when (sp-point-in-symbol)
      (sp-get (sp-get-symbol)
        (goto-char :beg)))
    (let ((start   (point))
          (open    (car this-pair))
          (close   (cdr this-pair))
          (enc-end (sp-get (sp-get-enclosing-sexp)
                     (when :end (1- :end)))))
      (sp-forward-sexp count)
      (setq end (if enc-end (min (point) enc-end) (point)))
      (if (= end pt-orig)
          (goto-char pt-orig)
        (goto-char start)
        (insert open)
        (goto-char (1+ end))
        (insert close)
        (goto-char start)
        (indent-region start end)
        (forward-char (1+ (- pt-orig start)))))))

(defun evil-cp--wrap-previous (pair count)
  (let ((pt-orig (point))
        (this-pair (evil-cp-pair-for pair))
        beg)
    (when (and (sp-point-in-symbol) (not (eobp))) ; bug in `sp-point-in-symbol'?
      (sp-get (sp-get-symbol)
        (goto-char :end)))
    (let ((start   (point))
          (open    (car this-pair))
          (close   (cdr this-pair))
          (enc-beg (sp-get (sp-get-enclosing-sexp)
                     (when :beg (1+ :beg)))))
      (sp-backward-sexp count)
      (setq beg (if enc-beg (max (point) enc-beg) (point)))
      (when (not (= pt-orig beg))
        (goto-char beg)
        (insert open)
        (goto-char (1+ start))
        (insert close)
        (goto-char start)
        (indent-region beg start)
        (backward-char (- (point) pt-orig 1))))))

(defun evil-cp-universal-invoke-arg-count ()
  "Gets the count for how many times the universal prefix
argument was invoked, i.e. for \\[universal-argument]
\\[universal-argument] it would return 2."
  (when (consp current-prefix-arg)
    (truncate (log (car current-prefix-arg) 4))))

(evil-define-command evil-cp-wrap-next-round (count)
  "Wraps the next COUNT sexps inside parentheses. If the point is
inside a symbol, that symbol is treated as the first sexp to
wrap.

When called with \\[universal-argument], wraps the current
enclosing form and the next N forms, where N is the count for how
many times the \\[universal-argument] was invoked."
  (interactive "<c>")
  (setq count (or count 1))
  (if (consp current-prefix-arg)
      (let ((count (evil-cp-universal-invoke-arg-count)))
        (save-excursion
          (sp-backward-up-sexp)
          (evil-cp--wrap-next "(" count))
        (evil-cp--backward-up-list))
    (evil-cp--wrap-next "(" count)))

(evil-define-command evil-cp-wrap-previous-round (count)
  "Wraps the previous COUNT sexps inside parentheses. If the point is
inside a symbol, that symbol is treated as the first sexp to
wrap.

When called with \\[universal-argument], wraps the current
enclosing form and the previous N forms, where N is the count for how
many times the \\[universal-argument] was invoked."
  (interactive "<c>")
  (setq count (or count 1))
  (if (consp current-prefix-arg)
      (let ((count (evil-cp-universal-invoke-arg-count)))
        (save-excursion
          (sp-up-sexp)
          (evil-cp--wrap-previous "(" count))
        (evil-cp--up-list))
    (evil-cp--wrap-previous "(" count)))

(evil-define-command evil-cp-wrap-next-square (count)
  "Wraps the next COUNT sexps inside square braces. If the point
is inside a symbol, that symbol is treated as the first sexp to
wrap.

When called with \\[universal-argument], wraps the current
enclosing form and the next N forms, where N is the count for how
many times the \\[universal-argument] was invoked."
  (interactive "<c>")
  (setq count (or count 1))
  (if (consp current-prefix-arg)
      (let ((count (evil-cp-universal-invoke-arg-count)))
        (save-excursion
          (sp-backward-up-sexp)
          (evil-cp--wrap-next "[" count))
        (evil-cp--backward-up-list))
    (evil-cp--wrap-next "[" count)))

(evil-define-command evil-cp-wrap-previous-square (count)
  "Wraps the previous COUNT sexps inside square braces. If the
point is inside a symbol, that symbol is treated as the first
sexp to wrap.

When called with \\[universal-argument], wraps the current
enclosing form and the previous N forms, where N is the count for how
many times the \\[universal-argument] was invoked."
  (interactive "<c>")
  (setq count (or count 1))
  (if (consp current-prefix-arg)
      (let ((count (evil-cp-universal-invoke-arg-count)))
        (save-excursion
          (sp-up-sexp)
          (evil-cp--wrap-previous "[" count))
        (evil-cp--up-list))
    (evil-cp--wrap-previous "[" count)))

(evil-define-command evil-cp-wrap-next-curly (count)
  "Wraps the next COUNT sexps inside curly braces. If the point
is inside a symbol, that symbol is treated as the first sexp to
wrap.

When called with \\[universal-argument], wraps the current
enclosing form and the next N forms, where N is the count for how
many times the \\[universal-argument] was invoked."
  (interactive "<c>")
  (setq count (or count 1))
  (if (consp current-prefix-arg)
      (let ((count (evil-cp-universal-invoke-arg-count)))
        (save-excursion
          (sp-backward-up-sexp)
          (evil-cp--wrap-next "{" count))
        (evil-cp--backward-up-list))
    (evil-cp--wrap-next "{" count)))

(evil-define-command evil-cp-wrap-previous-curly (count)
  "Wraps the previous COUNT sexps inside curly braces. If the point is
inside a symbol, that symbol is treated as the first sexp to
wrap.

When called with \\[universal-argument], wraps the current
enclosing form and the previous N forms, where N is the count for
how many times the \\[universal-argument] was invoked."
  (interactive "<c>")
  (setq count (or count 1))
  (if (consp current-prefix-arg)
      (let ((count (evil-cp-universal-invoke-arg-count)))
        (save-excursion
          (sp-up-sexp)
          (evil-cp--wrap-previous "{" count))
        (evil-cp--up-list))
    (evil-cp--wrap-previous "{" count)))


(defun evil-cp-insert (count &optional vcount skip-empty-lines)
  "Like `evil-insert', but tries to be helpful by automatically
inserting a space in situations where the need for it is highly
likely, and cleaning after itself in the case where the space
wasn't actually needed:

- When point is at an open parentheses (but not in an empty
  list), a space gets inserted but the point still remains where
  it used to.

Can be disabled by setting `evil-cleverparens-use-regular-insert'
to true."
  (interactive "p")
  (cond
   ((or (evil-visual-state-p)
        (looking-at-p "[\n\t ]+")
        (bobp)
        (eobp)
        (evil-cp--point-in-string-or-comment)
        (evil-cp--looking-at-empty-form))
    (call-interactively 'evil-insert))
   ((and (looking-back "(" (1- (point)))
         ;; should I use `sp-sexp-prefix' here?
         (not (looking-back "'(" (- (point) 2)))
         (not (looking-back "#(" (- (point) 2))))
    (setq evil-cp--inserted-space-after-round-open t)
    (insert " ")
    (backward-char)
    (evil-insert count))
   (t
    (call-interactively 'evil-insert))))

(defun evil-cp-append (count &optional vcount skip-empty-lines)
  "Like `evil-append', but tries to be helpful by automatically
inserting a space in situations where the need for it is highly
likely, and cleaning after itself in the case where the space
wasn't actually needed:

- When point is before an open parentheses (but not in an empty
  list), a space gets inserted but the point still remains where
  it used to.

Can be disabled by setting `evil-cleverparens-use-regular-insert'
to true."
  (interactive "p")
  (cond
   ((or (evil-visual-state-p)
        (looking-at-p ".[\n\t ]+")
        (bobp)
        (eobp)
        (evil-cp--point-in-string-or-comment)
        (evil-cp--looking-at-empty-form))
    (call-interactively 'evil-append))
   ((and (or (looking-at-p "(\\b")
             (evil-cp--looking-at-any-opening-p))
         (not (looking-back "'" (1- (point))))
         (not (looking-back "#" (1- (point)))))
    (setq evil-cp--inserted-space-after-round-open t)
    (forward-char)
    (insert " ")
    (backward-char)
    (evil-insert count))
   (t
    (call-interactively 'evil-append))))

(defun evil-cp-insert-exit-hook ()
  "Deletes the extra space left by `evil-cp-insert' if nothing was inserted."
  (if (and evil-cp--inserted-space-after-round-open
           (looking-at-p "[[:space:]]"))
      (when (or (not evil-current-insertion)
                (= (car evil-current-insertion)
                   (cdr evil-current-insertion)))
        (delete-char 1)))
  (setq evil-cp--inserted-space-after-round-open nil))


(defun evil-cp-toggle-balanced-yank (&optional forcep)
  "Toggles the setting `evil-cleverparens-complete-parens-in-yanked-region',
which determines whether or not an incomplete yanked region
should be supplemented with the missing parentheses at the end
and/or beginning."
  (interactive)
  (cond
   (forcep
    (message "Turned yank auto-balancing ON.")
    (setq evil-cleverparens-complete-parens-in-yanked-region t))
   ((not evil-cleverparens-complete-parens-in-yanked-region)
    (message "Turned yank auto-balancing ON.")
    (setq evil-cleverparens-complete-parens-in-yanked-region t))
   (t
    (message "Turned yank auto-balancing OFF.")
    (setq evil-cleverparens-complete-parens-in-yanked-region nil))))

;;; Keys ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evil-cp-regular-movement-keys
  '(("w"  . evil-forward-word-begin)
    ("e"  . evil-forward-word-end)
    ("b"  . evil-backward-word-begin)
    ("ge" . evil-backward-word-end)
    ("W"  . evil-cp-forward-symbol-begin)
    ("E"  . evil-cp-forward-symbol-end)
    ("B"  . evil-cp-backward-symbol-begin)
    ("gE" . evil-cp-backward-symbol-end)))

(defvar evil-cp-swapped-movement-keys
  '(("w"  . evil-cp-forward-symbol-begin)
    ("e"  . evil-cp-forward-symbol-end)
    ("b"  . evil-cp-backward-symbol-begin)
    ("ge" . evil-cp-backward-symbol-end)
    ("W"  . evil-forward-word-begin)
    ("E"  . evil-forward-word-end)
    ("B"  . evil-backward-word-begin)
    ("gE" . evil-backward-word-end)))

(defvar evil-cp-additional-movement-keys
  '(("L"   . evil-cp-forward-sexp)
    ("H"   . evil-cp-backward-sexp)
    ("M-l" . evil-cp-end-of-defun)
    ("M-h" . evil-cp-beginning-of-defun)
    ("["   . evil-cp-previous-opening)
    ("]"   . evil-cp-next-closing)
    ("{"   . evil-cp-next-opening)
    ("}"   . evil-cp-previous-closing)
    ("("   . evil-cp-backward-up-sexp)
    (")"   . evil-cp-up-sexp)))

(defvar evil-cp-regular-bindings
  '(("d"   . evil-cp-delete)
    ("c"   . evil-cp-change)
    ("y"   . evil-cp-yank)
    ("D"   . evil-cp-delete-line)
    ("C"   . evil-cp-change-line)
    ("Y"   . evil-cp-yank-line)
    ("x"   . evil-cp-delete-char-or-splice)
    ("X"   . evil-cp-delete-char-or-splice-backwards)
    (">"   . evil-cp->)
    ("<"   . evil-cp-<)
    ("_"   . evil-cp-first-non-blank-non-opening)
    ("M-T" . evil-cp-toggle-balanced-yank)
    ("M-z" . evil-cp-override))
  "Alist containing the regular evil-cleverparens bindings that
  override evil's bindings in normal mode.")

(defvar evil-cp-additional-bindings
  '(("M-t" . sp-transpose-sexp)
    ("M-k" . evil-cp-drag-backward)
    ("M-j" . evil-cp-drag-forward)
    ("M-J" . sp-join-sexp)
    ("M-s" . sp-splice-sexp)
    ("M-S" . sp-split-sexp)
    ("M-R" . evil-cp-raise-form)
    ("M-r" . sp-raise-sexp)
    ("M-a" . evil-cp-insert-at-end-of-form)
    ("M-i" . evil-cp-insert-at-beginning-of-form)
    ("M-w" . evil-cp-copy-paste-form)
    ("M-y" . evil-cp-yank-sexp)
    ("M-d" . evil-cp-delete-sexp)
    ("M-c" . evil-cp-change-sexp)
    ("M-Y" . evil-cp-yank-enclosing)
    ("M-D" . evil-cp-delete-enclosing)
    ("M-C" . evil-cp-change-enclosing)
    ("M-q" . sp-indent-defun)
    ("M-o" . evil-cp-open-below-form)
    ("M-O" . evil-cp-open-above-form)
    ("M-v" . sp-convolute-sexp)
    ("M-(" . evil-cp-wrap-next-round)
    ("M-)" . evil-cp-wrap-previous-round)
    ("M-[" . evil-cp-wrap-next-square)
    ("M-]" . evil-cp-wrap-previous-square)
    ("M-{" . evil-cp-wrap-next-curly)
    ("M-}" . evil-cp-wrap-previous-curly))
  "Alist containing additional functionality for
  evil-cleverparens via a modifier key (using the meta-key by
  default). Only enabled in evil's normal mode.")

(defvar evil-cp-insert-key "i"
  "Key to use to switch to insert mode")

(defvar evil-cp-append-key "a"
  "Key to use to switch to append mode")

(defun evil-cp--populate-mode-bindings-for-state (bindings state addp)
  "Helper function that defines BINDINGS for the evil-state
STATE when ADDP is true. If ADDP is false, then the keys in
BINDINGS are set to nil instead, effectively disabling the keys
in question."
  (--each bindings
    (evil-define-key state evil-cleverparens-mode-map
      (read-kbd-macro (car it))
      (if addp (cdr it) nil))))

;;;###autoload
(defun evil-cp-set-movement-keys ()
  "Sets the movement keys in
`evil-cleverparens-regular-movement-keys' or
`evil-cp-swapped-movement-keys' based on the value of
`evil-cleverparens-swap-move-by-word-and-symbol'."
  (interactive)
  (let ((keys (if evil-cleverparens-swap-move-by-word-and-symbol
                  evil-cp-swapped-movement-keys
                evil-cp-regular-movement-keys)))
    (evil-cp--populate-mode-bindings-for-state keys 'motion t)))

(defun evil-cp--enable-regular-bindings ()
  "Enables the regular evil-cleverparens bindings based on
`evil-cp-regular-bindings'."
  (dolist (state '(normal visual))
    (evil-cp--populate-mode-bindings-for-state
     evil-cp-regular-bindings
     state
     t))
  ;; Enable `evil-cp-change' as an `evil-change command'.
  (add-to-list 'evil-change-commands #'evil-cp-change)

  (if evil-cleverparens-use-regular-insert
      ;; in case we change our mind
      (progn
        (evil-define-key 'normal evil-cleverparens-mode-map
          evil-cp-insert-key 'evil-insert)
        (remove-hook 'evil-insert-state-exit-hook
                     'evil-cp-insert-exit-hook))
    (evil-define-key 'normal evil-cleverparens-mode-map
      evil-cp-insert-key 'evil-cp-insert
      evil-cp-append-key 'evil-cp-append)
    (add-hook 'evil-insert-state-exit-hook
              'evil-cp-insert-exit-hook))
  (when evil-cleverparens-use-s-and-S
    (evil-define-key 'normal evil-cleverparens-mode-map
      "s" 'evil-cp-substitute
      "S" 'evil-cp-change-whole-line)))

;;;###autoload
(defun evil-cp-set-additional-movement-keys ()
  "Sets the movement keys is `evil-cp-additional-movement-keys'
for normal, visual and operator states if
`evil-cleverparens-use-additional-movement-keys' is true."
  (interactive)
  (dolist (state '(normal visual operator))
    (evil-cp--populate-mode-bindings-for-state
     evil-cp-additional-movement-keys
     state
     evil-cleverparens-use-additional-movement-keys)))

;;;###autoload
(defun evil-cp-set-additional-bindings ()
  "Sets the movement keys is `evil-cp-additional-bindings' for
normal-state if `evil-cleverparens-use-additional-bindings' is
true."
  (interactive)
  (evil-cp--populate-mode-bindings-for-state
   evil-cp-additional-bindings
   'normal
   evil-cleverparens-use-additional-bindings))

(defun evil-cp--enable-C-w-delete ()
  (when evil-want-C-w-delete
    (evil-define-key 'insert evil-cleverparens-mode-map
      "\C-w" 'evil-cp-delete-backward-word)))

(defun evil-cp--enable-text-objects ()
  "Enables text-objects defined in evil-cleverparens."
  (define-key evil-outer-text-objects-map "f" #'evil-cp-a-form)
  (define-key evil-inner-text-objects-map "f" #'evil-cp-inner-form)
  (define-key evil-outer-text-objects-map "c" #'evil-cp-a-comment)
  (define-key evil-inner-text-objects-map "c" #'evil-cp-inner-comment)
  (define-key evil-outer-text-objects-map "d" #'evil-cp-a-defun)
  (define-key evil-inner-text-objects-map "d" #'evil-cp-inner-defun)
  (define-key evil-outer-text-objects-map "W" #'evil-cp-a-WORD)
  (define-key evil-inner-text-objects-map "W" #'evil-cp-inner-WORD))

(defun evil-cp--enable-surround-operators ()
  "Enables the use of `evil-cp-delete' and `evil-cp-change' with
`evil-surround-mode'"
  (when (boundp 'evil-surround-operator-alist)
    (add-to-list 'evil-surround-operator-alist '(evil-cp-delete . delete))
    (add-to-list 'evil-surround-operator-alist '(evil-cp-change . change))))

;; Setup keymap
(defvar evil-cleverparens-mode-map (make-sparse-keymap))
(evil-cp-set-movement-keys)
(evil-cp--enable-regular-bindings)
(evil-cp-set-additional-bindings)
(evil-cp-set-additional-movement-keys)
(evil-cp--enable-C-w-delete)
(evil-cp--enable-text-objects)

;;;###autoload
(define-minor-mode evil-cleverparens-mode
  "Minor mode for setting up evil with smartparens and paredit
for an advanced modal structural editing experience."
  :group 'evil-cleverparens
  :keymap evil-cleverparens-mode-map
  :lighter (" ecp"
            (:eval (if evil-cleverparens-complete-parens-in-yanked-region
                       "/b" "/i")))
  :init-value nil
  (if evil-cleverparens-mode
      (progn
        (if (bound-and-true-p evil-surround-mode)
            (evil-cp--enable-surround-operators)
          (add-hook 'evil-surround-mode-hook
                    'evil-cp--enable-surround-operators))
        (run-hooks 'evil-cleverparens-enabled-hook)
        (sp-update-local-pairs nil)
        (evil-normalize-keymaps))
    (run-hooks 'evil-cleverparens-disabled-hook)))

(provide 'evil-cleverparens)
;;; evil-cleverparens.el ends here
