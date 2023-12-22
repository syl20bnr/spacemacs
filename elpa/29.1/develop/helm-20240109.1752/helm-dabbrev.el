;;; helm-dabbrev.el --- Helm implementation of dabbrev. -*- lexical-binding: t -*-

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

(require 'helm)
(require 'helm-lib)
(require 'helm-help)
(require 'helm-elisp) ; For show-completion.

(defgroup helm-dabbrev nil
  "Dabbrev related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-dabbrev-always-search-all t
  "Always search in all buffers when non--nil.
Note that even if nil, a search in all buffers will occur if the
length of candidates is <= than
`helm-dabbrev-max-length-result'."
  :type 'boolean)

(defcustom helm-dabbrev-candidates-number-limit 1000
  "Maximum number of candidates to collect.

The higher this number is, the slower the computation of
candidates will be.  You can use safely a higher value with
emacs-26+.
Note that this have nothing to do with
`helm-candidate-number-limit', this means that computation of
candidates stop when this value is reached but only
`helm-candidate-number-limit' candidates are displayed in the
Helm buffer."
  :type 'integer)

(defcustom helm-dabbrev-ignored-buffers-regexps
  '("\\*helm" "\\*Messages" "\\*Echo Area" "\\*Buffer List")
  "List of regexps matching names of buffers that `helm-dabbrev' should not check."
  :type '(repeat regexp))

(defcustom helm-dabbrev-related-buffer-fn #'helm-dabbrev--same-major-mode-p
  "A function that decide if a buffer to search in its related to `current-buffer'.

This is actually determined by comparing `major-mode' of the
buffer to search and the `current-buffer'.

The function take one arg, the buffer which is current, look at
`helm-dabbrev--same-major-mode-p' for an example.

When nil all buffers are considered related to `current-buffer'."
  :type 'function)

(defcustom helm-dabbrev-major-mode-assoc nil
  "Major mode association alist.

This allow helm-dabbrev searching in buffers with the associated
`major-mode'.
E.g. (emacs-lisp-mode . lisp-interaction-mode)

will allow searching in the lisp-interaction-mode buffer when
`current-buffer' is an `emacs-lisp-mode' buffer and vice versa
i.e. no need to provide (lisp-interaction-mode .
emacs-lisp-mode) association.

When nil check is the searched buffer has same `major-mode' than
the `current-buffer'.

This has no effect when `helm-dabbrev-related-buffer-fn' is nil
or of course bound to a function that doesn't handle this var."
  :type '(alist :key-type symbol :value-type symbol))

(defcustom helm-dabbrev-lineno-around 30
  "Search first in this number of lines before and after point."
  :type 'integer)

(defcustom helm-dabbrev-cycle-threshold 5
  "Number of time helm-dabbrev cycle before displaying helm completion.
When nil or 0 disable cycling."
  :type '(choice (const :tag "Cycling disabled" nil) integer))

(defcustom helm-dabbrev-case-fold-search 'smart
  "Set `case-fold-search' in `helm-dabbrev'.
Same as `helm-case-fold-search' but for `helm-dabbrev'.
Note that this is not affecting searching in Helm buffer, but the
initial search for all candidates in buffer(s)."
  :type '(choice (const :tag "Ignore case" t)
          (const :tag "Respect case" nil)
          (other :tag "Smart" smart)))

(defvaralias 'helm-dabbrev--regexp 'helm-dabbrev-separator-regexp)
(make-obsolete-variable 'helm-dabbrev--regexp
                        'helm-dabbrev-separator-regexp "2.8.3")
;; Check for beginning of line should happen last (^\n\\|^).
(defvar helm-dabbrev-separator-regexp
  "\\s-\\|\t\\|[(\\[\\{\"'`=<>$;,@.#+]\\|\\s\\\\|^\n\\|^"
  "Regexp matching the start of a dabbrev candidate.")


(defvar helm-dabbrev-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-/") #'helm-next-line)
    (define-key map (kbd "M-:") #'helm-previous-line)
    map))

;; Internal
(defvar helm-dabbrev--cache nil)
(defvar helm-dabbrev--data nil)
(cl-defstruct helm-dabbrev-info dabbrev limits iterator)
(defvar helm-dabbrev--already-tried nil)
(defvar helm-dabbrev--computing-cache nil
  "[INTERNAL] Flag to notify helm-dabbrev is blocked.
Do nothing when non nil.")

(defun helm-dabbrev--buffer-list ()
  (cl-loop for buf in (buffer-list)
           unless (cl-loop for r in helm-dabbrev-ignored-buffers-regexps
                           thereis (string-match r (buffer-name buf)))
           collect buf))

(defun helm-dabbrev--same-major-mode-p (start-buffer)
  "Decide if current-buffer is related to START-BUFFER."
  (helm-same-major-mode-p start-buffer helm-dabbrev-major-mode-assoc))

(defun helm-dabbrev--collect (str limit ignore-case all)
  (let* ((case-fold-search ignore-case)
         (buffer1 (current-buffer))     ; start buffer.
         (minibuf (minibufferp buffer1))
         results pos-before pos-after)
    (catch 'break
      (dolist (buf (if all (helm-dabbrev--buffer-list)
                     (list (current-buffer))))
        (with-current-buffer buf
          (when (or minibuf ; check against all buffers when in minibuffer.
                    (if helm-dabbrev-related-buffer-fn
                        (funcall helm-dabbrev-related-buffer-fn buffer1)
                      t))
            (save-excursion
              ;; Start searching before thing before point.
              (goto-char (- (point) (length str)))
              ;; Search the last 30 lines BEFORE point and set POS-BEFORE.
              (cl-multiple-value-bind (res _pa pb)
                  (helm-dabbrev--search-and-store str -2 limit results)
                (setq results    res
                      ;; No need to set POS-AFTER here.
                      pos-before pb)))
            (save-excursion
              ;; Search the next 30 lines AFTER point and set POS-AFTER.
              (cl-multiple-value-bind (res pa _pb)
                  (helm-dabbrev--search-and-store str 2 limit results)
                (setq results    res
                      ;; No need to set POS-BEFORE, we keep the last
                      ;; value found.
                      pos-after  pa)))
            (save-excursion
              ;; Search all BEFORE point maybe starting from
              ;; POS-BEFORE to not search again what previously found.
              ;; If limit is reached in previous call of
              ;; `helm-dabbrev--search-and-store' POS-BEFORE is nil and
              ;; goto-char will fail, so check it.
              (when pos-before (goto-char pos-before))
              (cl-multiple-value-bind (res _pa _pb)
                  (helm-dabbrev--search-and-store str -1 limit results)
                ;; No need to set POS-BEFORE and POS-AFTER here.
                (setq results res)))
            (save-excursion
              ;; Search all AFTER point maybe starting from POS-AFTER.
              ;; Same comment as above for POS-AFTER.
              (when pos-after (goto-char pos-after))
              (cl-multiple-value-bind (res _pa _pb)
                  (helm-dabbrev--search-and-store str 1 limit results)
                ;; No need to set POS-BEFORE and POS-AFTER here.
                (setq results res)))))
        (when (>= (length results) limit) (throw 'break nil))))
    (nreverse results)))

(defun helm-dabbrev--search-and-store (pattern direction limit results)
  "Search words or symbols matching PATTERN in DIRECTION up to LIMIT.
Finally returns all matched candidates appended to RESULTS.
Argument DIRECTION can be:
    -  (1):  Search forward from point.
    - (-1):  Search backward from point.
    -  (2):  Search forward from the
             `helm-dabbrev-lineno-around'
             lines after point.
    - (-2):  Search backward from the
             `helm-dabbrev-lineno-around'
             lines before point."
  (let ((res results)
         after before)
    (while (and (<= (length res) limit)
                (cl-case direction
                  (1   (search-forward pattern nil t))
                  (-1  (search-backward pattern nil t))
                  (2   (let ((pos
                              (save-excursion
                                (forward-line
                                 helm-dabbrev-lineno-around)
                                (point))))
                         (setq after pos)
                         (search-forward pattern pos t)))
                  (-2  (let ((pos
                              (save-excursion
                                (forward-line
                                 (- helm-dabbrev-lineno-around))
                                (point))))
                         (setq before pos)
                         (search-backward pattern pos t)))))
      (let* ((mb (match-beginning 0))
             (replace-regexp (concat "\\(" helm-dabbrev-separator-regexp
                                     "\\)\\'"))
             (match-word (helm-dabbrev--search
                          pattern mb replace-regexp)))
        (when (and match-word (not (member match-word res)))
          (push match-word res))))
    (list res after before)))

(defun helm-dabbrev--search (pattern beg sep-regexp)
  "Search word or symbol at point matching PATTERN.
Argument BEG is corresponding to the previous `match-beginning'
search.
The search starts at (1- BEG) with a regexp starting with
`helm-dabbrev-separator-regexp' followed by PATTERN followed by a
regexp matching syntactically any word or symbol.
The possible false positives matching SEP-REGEXP at end are
finally removed."
  (let ((eol (pos-eol)))
    (save-excursion
      (goto-char (1- beg))
      (when (re-search-forward
             (concat "\\("
                     helm-dabbrev-separator-regexp
                     "\\)"
                     "\\(?99:\\("
                     (regexp-quote pattern)
                     "\\(\\sw\\|\\s_\\)+\\)\\)")
             eol t)
        (replace-regexp-in-string
         sep-regexp ""
         (match-string-no-properties 99))))))

(defun helm-dabbrev--get-candidates (dabbrev &optional limit)
  (cl-assert dabbrev nil "[No Match]")
  (helm-dabbrev--collect
   dabbrev (or limit helm-dabbrev-candidates-number-limit)
   (cl-case helm-dabbrev-case-fold-search
     (smart (helm-set-case-fold-search-1 dabbrev))
     (t helm-dabbrev-case-fold-search))
   helm-dabbrev-always-search-all))

(defun helm-dabbrev-default-action (candidate)
  (with-helm-current-buffer
    (let* ((limits (helm-bounds-of-thing-before-point
                    helm-dabbrev-separator-regexp))
           (beg (car limits))
           (end (point)))
      (run-with-timer
       0.01 nil
       #'helm-insert-completion-at-point
       beg end candidate))))

;;;###autoload
(cl-defun helm-dabbrev ()
  "Preconfigured helm for dynamic abbreviations."
  (interactive)
  (unless helm-dabbrev--computing-cache
    (let ((dabbrev (helm-thing-before-point
                    nil helm-dabbrev-separator-regexp))
          (limits (helm-bounds-of-thing-before-point
                   helm-dabbrev-separator-regexp))
          (enable-recursive-minibuffers t)
          (cycling-disabled-p (or (null helm-dabbrev-cycle-threshold)
                                  (zerop helm-dabbrev-cycle-threshold)))
          (helm-execute-action-at-once-if-one t)
          (helm-quit-if-no-candidate
           (lambda ()
             (message "[Helm-dabbrev: No expansion found]"))))
      (cl-assert (and (stringp dabbrev) (not (string= dabbrev "")))
                 nil "[Helm-dabbrev: Nothing found before point]")
      (when (and
             ;; have been called at least once.
             (helm-dabbrev-info-p helm-dabbrev--data)
             ;; But user have moved with some other command
             ;; in the meaning time.
             (not (eq last-command 'helm-dabbrev)))
        (setq helm-dabbrev--data nil))
      ;; When candidates are requested in helm directly without cycling,
      ;; we need them right now before running helm.
      (when cycling-disabled-p
        (message "Waiting for helm-dabbrev candidates...")
        (setq helm-dabbrev--cache (helm-dabbrev--get-candidates dabbrev)))
      (unless (or cycling-disabled-p
                  (helm-dabbrev-info-p helm-dabbrev--data))
        (setq helm-dabbrev--data
              (make-helm-dabbrev-info
               :dabbrev dabbrev
               :limits limits
               :iterator
               (helm-iter-list
                (cl-loop for i in (helm-dabbrev--get-candidates
                                   dabbrev helm-dabbrev-cycle-threshold)
                         when (string-match-p
                               (concat "^" (regexp-quote dabbrev)) i)
                         collect i)))))
      (let ((iter (and (helm-dabbrev-info-p helm-dabbrev--data)
                       (helm-dabbrev-info-iterator helm-dabbrev--data)))
            deactivate-mark)
        ;; Cycle until iterator is consumed.
        (helm-aif (and iter (helm-iter-next iter))
            (progn
              (helm-insert-completion-at-point
               (car (helm-dabbrev-info-limits helm-dabbrev--data))
               ;; END is the end of the previous inserted string, not
               ;; the end (apart for first insertion) of the initial string.
               (cdr limits) it)
              ;; Move already tried candidates to end of list.
              (push it helm-dabbrev--already-tried))
          ;; Iterator is now empty, or cycling was disabled, maybe
          ;; reset dabbrev to initial value and start helm completion.
          (let* ((old-dabbrev (if (helm-dabbrev-info-p helm-dabbrev--data)
                                  (helm-dabbrev-info-dabbrev helm-dabbrev--data)
                                dabbrev))
                 (only-one (eq (length helm-dabbrev--already-tried) 1)))
            (unless helm-dabbrev--cache ; Already computed when
                                        ; cycling is disabled.
              (message "Waiting for helm-dabbrev candidates...")
              (setq helm-dabbrev--computing-cache t)
              (setq helm-dabbrev--cache
                    (helm-dabbrev--get-candidates old-dabbrev))
              ;; If user continues typing M-/ while display is blocked by
              ;; helm-dabbrev--get-candidates delete these events.
              (setq unread-command-events nil))
            ;; If the length of candidates is only one when computed
            ;; that's mean the unique matched item have already been
            ;; inserted by the iterator, so no need to reinsert the old dabbrev,
            ;; just let helm exiting with "No expansion found".
            (unless (or only-one cycling-disabled-p)
              (setq dabbrev old-dabbrev
                    limits  (helm-dabbrev-info-limits helm-dabbrev--data))
              (setq helm-dabbrev--data nil)
              (delete-region (car limits) (point))
              (insert dabbrev))
            (when (and (null cycling-disabled-p) only-one)
              (setq helm-dabbrev--cache nil
                    helm-dabbrev--already-tried nil
                    helm-dabbrev--computing-cache nil)
              (cl-return-from helm-dabbrev
                (message "[Helm-dabbrev: No expansion found]")))
            (with-helm-show-completion (car limits) (cdr limits)
              (unwind-protect
                  (helm :sources
                        (helm-build-in-buffer-source "Dabbrev Expand"
                          :data
                          (append
                           (cl-loop with lst = helm-dabbrev--cache
                                    for cand in helm-dabbrev--already-tried
                                    do (setq lst (delete cand lst))
                                    finally return lst)
                           helm-dabbrev--already-tried)
                          :persistent-action 'ignore
                          :persistent-help "DoNothing"
                          :keymap helm-dabbrev-map
                          :action 'helm-dabbrev-default-action
                          :group 'helm-dabbrev)
                        :buffer "*helm dabbrev*"
                        :input (concat "^" dabbrev " ")
                        :resume 'noresume
                        :allow-nest t)
                (setq helm-dabbrev--computing-cache nil
                      helm-dabbrev--already-tried   nil
                      helm-dabbrev--cache           nil)))))))))

(provide 'helm-dabbrev)

;;; helm-dabbrev.el ends here
