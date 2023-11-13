;;; helm-ring.el --- kill-ring, mark-ring, and register browsers for helm. -*- lexical-binding: t -*-

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
(require 'helm-utils)
(require 'helm-help)
(require 'helm-elisp)

(declare-function undo-tree-restore-state-from-register "ext:undo-tree.el" (register))
(declare-function kmacro--keys "kmacro.el")
(declare-function frameset-register-p "frameset")

(defgroup helm-ring nil
  "Ring related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-kill-ring-threshold 3
  "Minimum length of a candidate to be listed by `helm-source-kill-ring'."
  :type 'integer
  :group 'helm-ring)

(defcustom helm-kill-ring-max-offset 400
  "Max number of chars displayed per candidate in kill-ring browser.
When `t', don't truncate candidate, show all.
By default it is approximatively the number of bits contained in five lines
of 80 chars each, i.e. 80*5.
Note that if you set this to nil multiline will be disabled, i.e. you
will not have separators between candidates any more."
  :type '(choice (const :tag "Disabled" t)
          (integer :tag "Max candidate offset"))
  :group 'helm-ring)

(defcustom helm-kill-ring-actions
  '(("Yank marked" . helm-kill-ring-action-yank)
    ("Delete marked" . helm-kill-ring-action-delete)
    ("Search from candidate" . helm-kill-ring-search-from-string))
  "List of actions for kill ring source."
  :group 'helm-ring
  :type '(alist :key-type string :value-type function))

(defcustom helm-kill-ring-separator "\n"
  "The separator used to separate marked candidates when yanking."
  :group 'helm-ring
  :type 'string)

(defcustom helm-register-max-offset 160
  "Max size of string register entries before truncating."
  :group 'helm-ring
  :type  'integer)

;;; Kill ring
;;
;;
(defvar helm-kill-ring-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-y")     'helm-next-line)
    (define-key map (kbd "M-u")     'helm-previous-line)
    (define-key map (kbd "M-D")     'helm-kill-ring-delete)
    (define-key map (kbd "C-s")     'helm-kill-ring-run-search-from-string)
    (define-key map (kbd "C-]")     'helm-kill-ring-toggle-truncated)
    (define-key map (kbd "C-c C-k") 'helm-kill-ring-kill-selection)
    (define-key map (kbd "C-c d")   'helm-kill-ring-run-persistent-delete)
    map)
  "Keymap for `helm-show-kill-ring'.")

(defvar helm-source-kill-ring
  (helm-build-sync-source "Kill Ring"
    :init (lambda ()
            (helm-set-attr 'last-command last-command)
            (helm-set-attr 'multiline helm-kill-ring-max-offset))
    :candidates #'helm-kill-ring-candidates
    :filtered-candidate-transformer #'helm-kill-ring-transformer
    :action 'helm-kill-ring-actions
    :persistent-action 'ignore
    :help-message 'helm-kill-ring-help-message
    :persistent-help "DoNothing"
    :keymap helm-kill-ring-map
    :migemo t
    :multiline 'helm-kill-ring-max-offset
    :group 'helm-ring)
  "Source for browse and insert contents of kill-ring.")

(defun helm-kill-ring-candidates ()
  (cl-loop with cands = (helm-fast-remove-dups kill-ring :test 'equal)
           for kill in (if (eq (helm-get-attr 'last-command) 'yank)
                            (cdr cands)
                          cands)
           unless (or (< (length kill) helm-kill-ring-threshold)
                      (string-match "\\`[\n[:blank:]]+\\'" kill))
           collect kill))

(defun helm-kill-ring-transformer (candidates _source)
  "Ensure CANDIDATES are not read-only."
  (cl-loop for i in candidates
           when (get-text-property 0 'read-only i)
           do (set-text-properties 0 (length i) '(read-only nil) i)
           collect i))

(defvar helm-kill-ring--truncated-flag nil)
(defun helm-kill-ring-toggle-truncated ()
  "Toggle truncated view of candidates in helm kill-ring browser."
  (interactive)
  (with-helm-alive-p
    (setq helm-kill-ring--truncated-flag (not helm-kill-ring--truncated-flag))
    (let* ((cur-cand (helm-get-selection))
           (presel-fn (lambda ()
                        (helm-kill-ring--preselect-fn cur-cand)))
           helm-display-source-at-screen-top)
      (helm-set-attr 'multiline
                     (if helm-kill-ring--truncated-flag
                         15000000
                       helm-kill-ring-max-offset))
      (helm-update presel-fn))))
(put 'helm-kill-ring-toggle-truncated 'helm-only t)

(defun helm-kill-ring-kill-selection ()
  "Store the real value of candidate in kill-ring.
Same as `helm-kill-selection-and-quit' called with a prefix arg."
  (interactive)
  (helm-kill-selection-and-quit t))
(put 'helm-kill-ring-kill-selection 'helm-only t)

(defun helm-kill-ring--preselect-fn (candidate)
  "Internal, used to preselect CANDIDATE when toggling truncated view."
  ;; Preselection by regexp may not work if candidate is huge, so walk
  ;; the helm buffer until selection is on CANDIDATE.
  (helm-awhile (condition-case-unless-debug nil
                   (and (not (helm-pos-header-line-p))
                        (helm-get-selection))
                 (error nil))
    (if (string= it candidate)
        (cl-return)
        (helm-next-line))))

(defun helm-kill-ring-action-yank (_str)
  "Insert concatenated marked candidates in current-buffer.

When two prefix args are given prompt to choose separator, otherwise
use `helm-kill-ring-separator' as default."
  (let ((marked (helm-marked-candidates))
        (sep (if (equal helm-current-prefix-arg '(16))
                 (read-string "Separator: ")
               helm-kill-ring-separator)))
    (helm-kill-ring-action-yank-1
     (cl-loop for c in (butlast marked)
              concat (concat c sep) into str
              finally return (concat str (car (last marked)))))))

(defun helm-kill-ring-action-yank-1 (str)
  "Insert STR in `kill-ring' and set STR to the head.

When called with a prefix arg, point and mark are exchanged
without activating region.
If this action is executed just after `yank', replace with STR as
yanked string."
  (let ((yank-fn (lambda (&optional before yank-pop)
                   (insert-for-yank str)
                   ;; Set the window start back where it was in
                   ;; the yank command, if possible.
                   (when yank-pop
                     (set-window-start (selected-window) yank-window-start t))
                   (when (or (equal helm-current-prefix-arg '(4)) before)
                     ;; Same as exchange-point-and-mark but without
                     ;; activating region.
                     (goto-char (prog1 (mark t)
                                  (set-marker (mark-marker)
                                              (point)
                                              helm-current-buffer)))))))
    ;; Prevent inserting and saving highlighted items.
    (set-text-properties 0 (length str) nil str)
    (with-helm-current-buffer
      (unwind-protect
           (progn
             (setq kill-ring (delete str kill-ring))
             ;; Adding a `delete-selection' property
             ;; to `helm-kill-ring-action' is not working
             ;; because `this-command' will be `helm-maybe-exit-minibuffer',
             ;; so use this workaround (Bug#1520).
             (when (and (region-active-p) delete-selection-mode)
               (delete-region (region-beginning) (region-end)))
             (if (not (eq (helm-get-attr 'last-command helm-source-kill-ring) 'yank))
                 (progn
                   ;; Ensure mark is at beginning of inserted text.
                   (push-mark)
                   ;; When yanking in a helm minibuffer we need a small
                   ;; delay to detect the mark in previous minibuffer. [1]
                   (run-with-timer 0.01 nil yank-fn))
               ;; from `yank-pop'
               (let ((inhibit-read-only t)
                     (before (< (point) (mark t))))
                 (if before
                     (funcall (or yank-undo-function 'delete-region) (point) (mark t))
                   (funcall (or yank-undo-function 'delete-region) (mark t) (point)))
                 (setq yank-undo-function nil)
                 (set-marker (mark-marker) (point) helm-current-buffer)
                 ;; Same as [1] but use the same mark and point as in
                 ;; the initial yank according to BEFORE even if no
                 ;; prefix arg is given.
                 (run-with-timer 0.01 nil yank-fn before 'pop))))
        (kill-new str)))))
(define-obsolete-function-alias 'helm-kill-ring-action 'helm-kill-ring-action-yank "2.4.0")

(defun helm-kill-ring-search-from-string (candidate)
  (let ((str (car (split-string candidate "\n"))))
    (helm-multi-occur-1
     (list (current-buffer))
     (regexp-quote (substring-no-properties str)))))

(helm-make-command-from-action helm-kill-ring-run-search-from-string
    "Run helm-occur from kill ring."
  'helm-kill-ring-search-from-string)

(defun helm-kill-ring-action-delete (_candidate)
  "Delete marked candidates from `kill-ring'."
  (cl-loop for c in (helm-marked-candidates)
           do (setq kill-ring
                    (delete c kill-ring))))

(defun helm-kill-ring-persistent-delete (_candidate)
  (unwind-protect
       (cl-loop for c in (helm-marked-candidates)
                do (progn
                     (helm-preselect (format "^%s" (regexp-quote c)))
                     (setq kill-ring (delete c kill-ring))
                     (helm-delete-current-selection)
                     (helm--remove-marked-and-update-mode-line c)))
    (with-helm-buffer
      (setq helm-marked-candidates nil
            helm-visible-mark-overlays nil))
    (helm-force-update (helm-aif (helm-get-selection nil t) (regexp-quote it)))))

(helm-make-persistent-command-from-action helm-kill-ring-run-persistent-delete
  "Delete current candidate without quitting."
  'quick-delete 'helm-kill-ring-persistent-delete)

(helm-make-command-from-action helm-kill-ring-delete
  "Delete marked candidates from `kill-ring'."
  'helm-kill-ring-action-delete)


;;;; <Mark ring>
;; DO NOT use these sources with other sources use
;; the commands `helm-mark-ring', `helm-global-mark-ring' or
;; `helm-all-mark-rings' instead.

(defun helm-mark-ring-line-string-at-pos (pos)
  "Return line string at position POS."
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (let ((line (car (split-string (thing-at-point 'line) "[\n\r]"))))
      (remove-text-properties 0 (length line) '(read-only) line)
      (if (string= "" line)
          "<EMPTY LINE>"
        line))))

(defun helm-mark-ring-get-candidates ()
  (with-helm-current-buffer
    (cl-loop with marks = (if (mark t)
                              (cons (mark-marker) mark-ring)
                            mark-ring)
             for marker in marks
             with max-line-number = (line-number-at-pos (point-max))
             with width = (length (number-to-string max-line-number))
             for m = (format (concat "%" (number-to-string width) "d: %s")
                             (line-number-at-pos marker)
                             (helm-mark-ring-line-string-at-pos marker))
             unless (and recip (assoc m recip))
             collect (cons m marker) into recip
             finally return recip)))

(defun helm-mark-ring-default-action (candidate)
  (let ((target (copy-marker candidate)))
    (helm-aif (marker-buffer candidate)
        (progn
          (switch-to-buffer it)
          (helm-log-run-hook "helm-mark-ring-default-action" 'helm-goto-line-before-hook)
          (helm-match-line-cleanup)
          (with-helm-current-buffer
            (unless helm-yank-point (setq helm-yank-point (point))))
          (helm-goto-char target)
          (helm-highlight-current-line))
      ;; marker points to no buffer, no need to dereference it, just
      ;; delete it.
      (setq mark-ring (delete target mark-ring))
      (error "Marker points to no buffer"))))

(defvar helm-source-mark-ring
  (helm-build-sync-source "mark-ring"
    :candidates #'helm-mark-ring-get-candidates
    :action '(("Goto line" . helm-mark-ring-default-action))
    :persistent-help "Show this line"
    :group 'helm-ring))

;;; Global-mark-ring
(defvar helm-source-global-mark-ring
  (helm-build-sync-source "global-mark-ring"
    :candidates #'helm-global-mark-ring-get-candidates
    :action '(("Goto line" . helm-mark-ring-default-action))
    :persistent-help "Show this line"
    :group 'helm-ring))

(defun helm-global-mark-ring-format-buffer (marker)
  (with-current-buffer (marker-buffer marker)
    (goto-char marker)
    (forward-line 0)
    (let ((line (pcase (thing-at-point 'line)
                  ((and line (pred stringp)
                        (guard (not (string-match-p "\\`\n?\\'" line))))
                   (car (split-string line "[\n\r]")))
                  (_ "<EMPTY LINE>"))))
      (remove-text-properties 0 (length line) '(read-only) line)
      (format "%7d:%s:    %s"
              (line-number-at-pos) (marker-buffer marker) line))))

(defun helm-global-mark-ring-get-candidates ()
  (let ((marks global-mark-ring))
    (when marks
      (cl-loop for marker in marks
               for mb = (marker-buffer marker)
               for gm = (unless (or (string-match "^ " (format "%s" mb))
                                    (null mb))
                          (helm-global-mark-ring-format-buffer marker))
               when (and gm (not (assoc gm recip)))
               collect (cons gm marker) into recip
               finally return recip))))

;;;; <Register>
;;; Insert from register
(defvar helm-source-register
  (helm-build-sync-source "Registers"
    :candidates #'helm-register-candidates
    :action-transformer #'helm-register-action-transformer
    :persistent-help ""
    :multiline t
    :action '(("Delete Register(s)" .
               (lambda (_candidate)
                 (cl-loop for candidate in (helm-marked-candidates)
                          for register = (car candidate)
                          do (setq register-alist
                                (delq (assoc register register-alist)
                                      register-alist))))))
    :group 'helm-ring)
  "See (info \"(emacs)Registers\")")

(defun helm-register-candidates ()
  "Collecting register contents and appropriate commands."
  (require 'frameset)
  (cl-loop for (char . rval) in register-alist
        for key    = (single-key-description char)
        for e27 = (registerv-p rval)
        for val = (if e27 ; emacs-27
                      (registerv-data rval)
                    rval)
        for string-actions =
        (cond
          ((numberp val)
           (list (int-to-string val)
                 'insert-register
                 'increment-register))
          ((markerp val)
           (let ((buf (marker-buffer val)))
             (if (null buf)
                 (list "a marker in no buffer")
               (list (concat
                      "a buffer position:"
                      (buffer-name buf)
                      ", position "
                      (int-to-string (marker-position val)))
                     'jump-to-register
                     'insert-register))))
          ((and (consp val) (window-configuration-p (car val)))
           (list (if (fboundp 'describe-register-1)
                     (describe-register-1 char) "window configuration.")
                 'jump-to-register))
          ((and (vectorp val)
                (fboundp 'undo-tree-register-data-p)
                (undo-tree-register-data-p (if e27 val (elt val 1))))
           (list
            "Undo-tree entry."
            'undo-tree-restore-state-from-register))
          ((or (and (vectorp val) (eq 'registerv (aref val 0)))
               (and (consp val) (frame-configuration-p (car val)))
               (or (frame-configuration-p val)
                   (frameset-register-p val)))
           (list (if (fboundp 'describe-register-1)
                     (describe-register-1 char) "Frame configuration")
                 'jump-to-register))
          ((and (consp val) (eq (car val) 'file))
           (list (concat "file:"
                         (prin1-to-string (cdr val))
                         ".")
                 'jump-to-register))
          ((and (consp val) (eq (car val) 'buffer))
           (list (concat "buffer:"
                         (prin1-to-string (cdr val))
                         ".")
                 'jump-to-register))
          ((and (consp val) (eq (car val) 'file-query))
           (list (concat "file:a file-query reference: file "
                         (car (cdr val))
                         ", position "
                         (int-to-string (car (cdr (cdr val))))
                         ".")
                 'jump-to-register))
          ((consp val)
           (let ((lines (format "%4d" (length val))))
             (list (format "%s: %s\n" lines
                           (truncate-string-to-width
                            (mapconcat 'identity (list (car val))
                                       "^J")
                            (- (window-width) 15)))
                   'insert-register)))
          ((stringp val)
           (list
            (concat (substring-no-properties
                     val 0 (min (length val) helm-register-max-offset))
                    (if (> (length val) helm-register-max-offset)
                        "[...]" ""))
            'insert-register
            'kill-new
            'append-to-register
            'prepend-to-register)))
        unless (null string-actions) ; Fix Bug#1107.
        collect (cons (format "Register %3s:\n %s" key (car string-actions))
                      (cons char (cdr string-actions)))))

(defun helm-register-action-transformer (actions register-and-functions)
  "Decide actions by the contents of register."
  (cl-loop with func-actions =
           '((insert-register
              "Insert Register" .
              (lambda (c) (insert-register (car c))))
             (kill-new
              "Kill Register" .
              (lambda (c) (with-temp-buffer
                            (insert-register (car c))
                            (kill-new (buffer-string)))))
             (jump-to-register
              "Jump to Register" .
              (lambda (c) (jump-to-register (car c))))
             (append-to-register
              "Append Region to Register" .
              (lambda (c) (append-to-register
                           (car c) (region-beginning) (region-end))))
             (prepend-to-register
              "Prepend Region to Register" .
              (lambda (c) (prepend-to-register
                           (car c) (region-beginning) (region-end))))
             (increment-register
              "Increment Prefix Arg to Register" .
              (lambda (c) (increment-register
                           helm-current-prefix-arg (car c))))
             (undo-tree-restore-state-from-register
              "Restore Undo-tree register" .
              (lambda (c) (and (fboundp 'undo-tree-restore-state-from-register)
                               (undo-tree-restore-state-from-register (car c))))))
           for func in (cdr register-and-functions)
           when (assq func func-actions)
           collect (cdr it) into transformer-actions
           finally return (append transformer-actions actions)))

;;;###autoload
(defun helm-mark-ring ()
  "Preconfigured `helm' for `helm-source-mark-ring'."
  (interactive)
  (helm :sources 'helm-source-mark-ring
        :resume 'noresume
        :buffer "*helm mark*"))

;;;###autoload
(defun helm-global-mark-ring ()
  "Preconfigured `helm' for `helm-source-global-mark-ring'."
  (interactive)
  (helm :sources 'helm-source-global-mark-ring
        :resume 'noresume
        :buffer "*helm global mark*"))

;;;###autoload
(defun helm-all-mark-rings ()
  "Preconfigured `helm' for mark rings.
Source used are `helm-source-global-mark-ring' and
`helm-source-mark-ring'."
  (interactive)
  (helm :sources '(helm-source-mark-ring
                   helm-source-global-mark-ring)
        :resume 'noresume
        :buffer "*helm mark ring*"))

;;;###autoload
(defun helm-register ()
  "Preconfigured `helm' for Emacs registers."
  (interactive)
  (helm :sources 'helm-source-register
        :resume 'noresume
        :buffer "*helm register*"))

;;;###autoload
(defun helm-show-kill-ring ()
  "Preconfigured `helm' for `kill-ring'.
It is drop-in replacement of `yank-pop'.

First call open the kill-ring browser, next calls move to next line."
  (interactive)
  (setq helm-kill-ring--truncated-flag nil)
  (let ((enable-recursive-minibuffers t))
    (helm :sources helm-source-kill-ring
          :buffer "*helm kill ring*"
          ;; :display-source-at-screen-top nil
          :resume 'noresume
          :allow-nest t)))

;;;###autoload
(defun helm-execute-kmacro ()
  "Preconfigured helm for keyboard macros.
Define your macros with `f3' and `f4'.
See (info \"(emacs) Keyboard Macros\") for detailed infos."
  (interactive)
  (let ((helm-quit-if-no-candidate
         (lambda () (message "No kbd macro has been defined"))))
    (helm :sources
          (helm-build-sync-source "Kmacro"
            :candidates (lambda ()
                          (delq nil
                                (helm-fast-remove-dups
                                 (cons (kmacro-ring-head)
                                       kmacro-ring)
                                 :test 'equal)))
            
            :multiline t
            :candidate-transformer
            (lambda (candidates)
              (cl-loop for c in candidates
                       for keys = (if (functionp c)
                                      ;; Emacs-29+ (Oclosure).
                                      (kmacro--keys c)
                                    ;; Emacs-28 and below (list).
                                    (car c))
                       collect (propertize (help-key-description keys nil)
                                           'helm-realvalue c)))
            :persistent-action 'ignore
            :persistent-help "Do nothing"
            :help-message 'helm-kmacro-help-message
            :action
            (helm-make-actions
             "Execute kmacro (`C-u <n>' to execute <n> times)"
             'helm-kbd-macro-execute
             "Concat marked macros"
             'helm-kbd-macro-concat-macros
             "Delete marked macros"
             'helm-kbd-macro-delete-macro
             "Edit marked macro"
             'helm-kbd-macro-edit-macro
             "Insert kbd macro"
             'helm-kbd-macro-insert-macro)
            :group 'helm-ring)
          :buffer "*helm kmacro*")))

(defun helm-kbd-macro-make-current (candidate)
  "Make CANDIDATE macro the current one."
  (setq kmacro-ring (delete candidate kmacro-ring))
  (kmacro-push-ring)
  (kmacro-split-ring-element candidate))

(defun helm-kbd-macro-insert-macro (candidate)
  "Insert macro at point in `helm-current-buffer'."
  (let ((desc (read-string "Describe macro briefly: "))
        name key)
    (while (fboundp (setq name (intern (read-string "New name for macro: "))))
      (message "Symbol `%s' already exists, choose another name" name)
      (sit-for 1.5))
    (helm-kbd-macro-make-current candidate)
    (kmacro-name-last-macro name)
    (when (y-or-n-p "Bind macro to a new key?")
      (helm-awhile (key-binding
                    (setq key (read-key-sequence-vector "Bind macro to key: ")))
        (message "`%s' already run command `%s', choose another one"
                 (help-key-description key nil) it)
        (sit-for 1.5))
      (global-set-key key name))
    (with-helm-current-buffer
      (insert (format ";; %s%s\n"
                      desc
                      (and key (format " (bound to `%s')"
                                       (help-key-description key nil)))))
      (insert-kbd-macro name (not (null key))))))

(defun helm-kbd-macro-execute (candidate)
  ;; Move candidate on top of list for next use.
  (helm-kbd-macro-make-current candidate)
  (kmacro-exec-ring-item
   candidate helm-current-prefix-arg))

(defun helm-kbd-macro-concat-macros (_candidate)
  (let ((mkd (helm-marked-candidates)))
    (when (cdr mkd)
      (kmacro-push-ring)
      (setq last-kbd-macro
            (cl-loop for km in mkd
                     for keys = (if (functionp km)
                                    (kmacro--keys km)
                                  (pcase (car km)
                                    ((and vec (pred vectorp)) vec)
                                    ((and str (pred stringp))
                                     (kmacro--to-vector str))))
                     vconcat keys)))))

(defun helm-kbd-macro-delete-macro (_candidate)
  (let ((mkd  (helm-marked-candidates))
        (head (kmacro-ring-head)))
    (cl-loop for km in mkd
             do (setq kmacro-ring (delete km kmacro-ring)))
    (when (member head mkd)
      (kmacro-delete-ring-head))))

(defun helm-kbd-macro-edit-macro (candidate)
  (kmacro-push-ring)
  (setq kmacro-ring (delete candidate kmacro-ring))
  (kmacro-split-ring-element candidate)
  (kmacro-edit-macro))

(provide 'helm-ring)

;;; helm-ring.el ends here
