;;; evil-surround.el --- emulate surround.vim from Vim  -*- lexical-binding: t; -*-

;; Copyright (C) 2010 - 2017 Tim Harper
;; Copyright (C) 2018 - 2020 The evil-surround.el Contributors

;; Author: Tim Harper <timcharper at gmail dot com>
;;         Vegard Øye <vegard_oye at hotmail dot com>
;;
;; The following line is included for NonGNU ELPA's build script:
;; Maintainer: Tom Dalziel <tom.dalziel@gmail.com>
;;
;; Created: July 23 2011
;; Version: 1.0.4
;; Package-Requires: ((evil "1.2.12"))
;; Mailing list: <implementations-list at lists.ourproject.org>
;;      Subscribe: http://tinyurl.com/implementations-list
;;      Newsgroup: nntp://news.gmane.org/gmane.emacs.vim-emulation
;;      Archives: http://dir.gmane.org/gmane.emacs.vim-emulation
;; Keywords: emulation, vi, evil

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package emulates surround.vim by Tim Pope.
;; The functionality is wrapped into a minor mode. To enable
;; it globally, add the following lines to ~/.emacs:
;;
;;     (require 'evil-surround)
;;     (global-evil-surround-mode 1)
;;
;; Alternatively, you can enable evil-surround-mode along a major mode
;; by adding `turn-on-evil-surround-mode' to the mode hook.
;;
;; This package uses Evil as its vi layer. It is available from:
;;
;;     https://github.com/emacs-evil/evil

;;; Code:

(require 'evil)

(defgroup evil-surround nil
  "surround.vim for Emacs"
  :prefix "evil-surround-"
  :group 'evil)

;; make surround's `ysw' work like `cw', not `ce'
(when (boundp 'evil-change-commands)
  (add-to-list 'evil-change-commands 'evil-surround-region))

(defcustom evil-surround-pairs-alist
  '((?\( . ("( " . " )"))
    (?\[ . ("[ " . " ]"))
    (?\{ . ("{ " . " }"))

    (?\) . ("(" . ")"))
    (?\] . ("[" . "]"))
    (?\} . ("{" . "}"))

    (?# . ("#{" . "}"))
    (?b . ("(" . ")"))
    (?B . ("{" . "}"))
    (?> . ("<" . ">"))
    (?t . evil-surround-read-tag)
    (?< . evil-surround-read-tag)
    (?\C-f . evil-surround-prefix-function)
    (?f . evil-surround-function))
  "Association list of surround items.
Each item is of the form (TRIGGER . (LEFT . RIGHT)), all strings.
Alternatively, a function can be put in place of (LEFT . RIGHT).
This only affects inserting pairs, not deleting or changing them."
  :group 'evil-surround
  :type '(alist
          :key-type (character :tag "Key")
          :value-type (choice
                       (cons (string :tag "Opening") (string :tag "Closing"))
                       (function :tag "Function"))))
(make-variable-buffer-local 'evil-surround-pairs-alist)

(defcustom evil-surround-operator-alist
  '((evil-change . change)
    (evil-delete . delete))
  "Association list of operators to their fundamental operation.
Each item is of the form (OPERATOR . OPERATION)."
  :group 'evil-surround
  :type '(repeat (cons (symbol :tag "Operator")
                       (symbol :tag "Operation"))))

(defvar evil-surround-read-tag-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map ">" (lambda ()
                          (interactive)
                          (call-interactively #'self-insert-command)
                          (exit-minibuffer)))
    map)
  "Keymap used by `evil-surround-read-tag'.")

(defvar evil-surround-record-repeat nil
  "Flag to indicate we're manually recording repeat info.")

(defvar evil-surround-last-deleted-left ""
  "The previously deleted LEFT region.")

(defvar evil-surround-current-pair nil
  "The current pair.

When non-nil, it can be either a cons or a function returning a cons.")

(defun evil-surround-read-from-minibuffer (&rest args)
  (when (or evil-surround-record-repeat
            (evil-repeat-recording-p))
    (evil-repeat-keystrokes 'post))
  (let ((res (apply #'read-from-minibuffer args)))
    (when (or evil-surround-record-repeat
              (evil-repeat-recording-p))
      (evil-repeat-record res))
    res))

;; The operator state narrows the buffer to the current field. This
;; function widens temporarily before reading a character so the
;; narrowing is not visible to the user.
(defun evil-surround-read-char ()
  (if (evil-operator-state-p)
      (save-restriction (widen) (read-char))
    (read-char)))

(defun evil-surround-input-char ()
  (list (evil-surround-read-char)))

(defun evil-surround-input-region-char ()
  (append (evil-operator-range t)
          (evil-surround-input-char)))

(defun evil-surround-function ()
  "Read a functionname from the minibuffer and wrap selection in function call"
  (let ((fname (evil-surround-read-from-minibuffer "" "")))
    (cons (format "%s(" (or fname ""))
          ")")))

(defun evil-surround-prefix-function ()
  "Read a function name from the minibuffer and wrap selection in a
function call in prefixed form."
  (let ((fname (evil-surround-read-from-minibuffer "prefix function: " "")))
    (cons (format "(%s " (or fname ""))
          ")")))

(defconst evil-surround-tag-name-re "\\([0-9a-zA-Z\.-]+\\)"
  "Regexp matching an XML tag name.")

(defun evil-surround-tag-p (string)
  "Return t if `STRING' looks like a tag."
  (string-match-p evil-surround-tag-name-re string))

(defun evil-surround-read-tag ()
  "Read a XML tag from the minibuffer."
  (let* ((input (evil-surround-read-from-minibuffer "<" "" evil-surround-read-tag-map))
         (_ (string-match (concat evil-surround-tag-name-re "\\(.*?\\)\\([>]*\\)$") input))
         (tag  (match-string 1 input))
         (rest (match-string 2 input))
         (keep-attributes (not (string-match-p ">" input)))
         (original-tag (when (evil-surround-tag-p evil-surround-last-deleted-left)
                         (substring evil-surround-last-deleted-left
                                    (string-match (concat "<" evil-surround-tag-name-re) evil-surround-last-deleted-left)
                                    (match-end 0))))
         (original-attributes (when (and keep-attributes original-tag)
                                (substring evil-surround-last-deleted-left (length original-tag)))))
    (cons (format "<%s%s%s" (or tag "") (or rest "") (or original-attributes ">"))
          (format "</%s>" (or tag "")))))

(defun evil-surround-valid-char-p (char)
  "Returns whether CHAR is a valid surround char or not."
  (not (memq char '(?\C-\[ ?\C-?))))

(defun evil-surround-delete-char-noop-p (char)
  "Returns whether CHAR is a noop when used with surround delete."
  (memq char (list (string-to-char "w")
                   (string-to-char "W")
                   (string-to-char "s")
                   (string-to-char "p"))))

(defun evil-surround-pair (char)
  "Return the evil-surround pair of char.
This is a cons cell (LEFT . RIGHT), both strings."
  (let ((evil-surround-current-pair (assoc-default char evil-surround-pairs-alist)))
    (cond
     ((functionp evil-surround-current-pair)
      (funcall evil-surround-current-pair))

     ((consp evil-surround-current-pair)
      evil-surround-current-pair)

     (t
      (cons (format "%c" char) (format "%c" char))))))

(defvar-local evil-surround-local-outer-text-object-map-list nil
  "Buffer-local list of outer text object keymaps that are added to
  evil-surround")

(defvar-local evil-surround-local-inner-text-object-map-list nil
  "Buffer-local list of inner text object keymaps that are added to
  evil-surround")

(defun evil-surround-trim-whitespace-from-range (range &optional regexp)
  "Given an evil-range, trim whitespace around range by shrinking
the range such that it neither begins nor ends with whitespace.
Does not modify the buffer."
  (let ((regexp (or regexp "[ \f\t\n\r\v]")))
    (save-excursion
      (save-match-data
        (goto-char (evil-range-beginning range))
        (while (looking-at regexp) (forward-char))
        (let ((new-beg (point)))
          (evil-set-range-beginning range new-beg)
          (goto-char (evil-range-end range))
          (while (looking-back regexp new-beg) (backward-char))
          (evil-set-range-end range (point)))))))

(defun evil-surround-outer-overlay (delims char)
  "Return overlay from provided delimiters or character.
Preferably, use DELIMS to select the correct range.  Otherwise, use CHAR.
This overlay includes the delimeters."
  (let ((open (car-safe delims))
        (close (cdr-safe delims))
        outer)
    (if (and (stringp open) (stringp close))
        (let* ((o (regexp-quote open))
               (c (regexp-quote close))
               (p (point)))
          (setq outer (evil-select-paren o c p p nil 1 t)))
      (let ((outer-obj (lookup-key
                        (make-composed-keymap
                         evil-surround-local-outer-text-object-map-list
                         evil-outer-text-objects-map)
                        (string char))))
        (when (functionp outer-obj) (setq outer (funcall outer-obj)))))
    (when (evil-range-p outer)
      (evil-surround-trim-whitespace-from-range outer "[[:space:]]")
      (make-overlay (evil-range-beginning outer)
                    (evil-range-end outer)
                    nil nil t))))

(defun evil-surround-inner-overlay (delims char)
  "Return overlay from provided delimiters or character.
Preferably, use DELIMS to select the correct range.  Otherwise, use CHAR.
This overlay excludes the delimeters."
  (let ((open (car-safe delims))
        (close (cdr-safe delims))
        inner)
    (if (and (stringp open) (stringp close))
        (let* ((o (regexp-quote open))
               (c (regexp-quote close))
               (p (point)))
          (setq inner (evil-select-paren o c p p nil 1 nil)))
      (let ((inner-obj (lookup-key
                        (make-composed-keymap
                         evil-surround-local-inner-text-object-map-list
                         evil-inner-text-objects-map)
                        (string char))))
        (when (functionp inner-obj)
          (setq inner (funcall inner-obj)))))
    (when (evil-range-p inner)
      (when (eq (char-syntax char) ?\()
        (evil-surround-trim-whitespace-from-range inner "[[:space:]]"))
      (make-overlay (evil-range-beginning inner)
                    (evil-range-end inner)
                    nil nil t))))

(evil-define-motion evil-surround-line (count)
  "Move COUNT - 1 lines down but return exclusive character motion."
  :type exclusive
  (let ((beg (line-beginning-position)))
    (evil-line count)
    (end-of-line)
    (let ((range (evil-range beg (point) 'exclusive)))
      (evil-expand-range range)
      range)))

(defun evil-surround--get-delims (char)
  "Given a CHAR, return delims from the pairs alist. Trim whitespace."
  (let ((kv-pair (assoc char evil-surround-pairs-alist)))
    (when kv-pair
      (let* ((delims (cdr kv-pair))
             (o (car-safe delims))
             (c (cdr-safe delims)))
        (if (and (stringp o) (stringp c))
            (cons (string-trim o) (string-trim c))
          delims)))))

;;;###autoload
(defun evil-surround-delete (char &optional outer inner)
  "Delete the surrounding delimiters represented by CHAR.
Alternatively, the text to delete can be represented with
the overlays OUTER and INNER, where OUTER includes the delimiters
and INNER excludes them. The intersection (i.e., difference)
between these overlays is what is deleted."
  (interactive (evil-surround-input-char))
  (cond
   ((and outer inner)
    (setq evil-surround-last-deleted-left
          (delete-and-extract-region (overlay-start outer) (overlay-start inner)))
    (delete-region (overlay-end inner) (overlay-end outer))
    (goto-char (overlay-start outer)))
   (t
    ;; no overlays specified: create them on the basis of CHAR
    ;; and delete after use
    (let* ((delims (evil-surround--get-delims char))
           (outer (evil-surround-outer-overlay delims char))
           (inner (evil-surround-inner-overlay delims char)))
      (unwind-protect
          (when (and outer inner)
            (evil-surround-delete char outer inner))
        (when outer (delete-overlay outer))
        (when inner (delete-overlay inner)))))))

;;;###autoload
(defun evil-surround-change (char &optional outer inner)
  "Change the surrounding delimiters represented by CHAR.
Alternatively, the text to delete can be represented with the
overlays OUTER and INNER, which are passed to `evil-surround-delete'."
  (interactive (evil-surround-input-char))
  (cond
   ((and outer inner)
    (unless (evil-surround-delete-char-noop-p char)
      (evil-surround-delete char outer inner))
    (let ((key (evil-surround-read-char)))
      (evil-surround-region (overlay-start outer)
                            (overlay-end outer)
                            nil (if (evil-surround-valid-char-p key) key char))))
   (t
    (let* ((delims (evil-surround--get-delims char))
           (outer (evil-surround-outer-overlay delims char))
           (inner (evil-surround-inner-overlay delims char)))
      (unwind-protect
          (when (and outer inner)
            (evil-surround-change char outer inner))
        (when outer (delete-overlay outer))
        (when inner (delete-overlay inner)))))))

(defun evil-surround-interactive-setup ()
  (setq evil-inhibit-operator t)
  (list (assoc-default evil-this-operator
                       evil-surround-operator-alist)))

(defun evil-surround-setup-surround-line-operators ()
  (define-key evil-operator-shortcut-map "s" 'evil-surround-line)
  (define-key evil-operator-shortcut-map "S" 'evil-surround-line))

(defun evil-surround-column-at (pos)
  (save-excursion (goto-char pos) (current-column)))

(defun evil-surround-block (beg end char)
  "Surrounds a block selection with a character, as if `evil-surround-region'
were called on each segment in each line. This skips lines where EOL < BEG's
column."
  (let ((beg-col (evil-surround-column-at beg))
        (end-col (evil-surround-column-at end)))
    (evil-apply-on-block
     (lambda (ibeg iend)
       (unless (< (evil-surround-column-at ibeg) (min beg-col end-col))
         (evil-surround-region ibeg iend t char)))
     beg end nil)))

(defun evil-surround-call-with-repeat (callback)
  "Record keystrokes to repeat surround-region operator and it's motion.
This is necessary because `evil-yank' operator is not repeatable (:repeat nil)"
  (evil-repeat-start)
  (evil-repeat-record "y")
  (evil-repeat-record (this-command-keys))

  ;; set `this-command-keys' to the command that will be executed
  ;; interactively; as a result, `evil-this-operator' will be
  ;; correctly set to, for example, `evil-surround-region' instead of
  ;; `evil-yank' when surround has been invoked by `ys'
  (setq this-command callback)
  (let ((evil-surround-record-repeat t))
    (call-interactively callback))
  (evil-repeat-keystrokes 'post)
  (evil-repeat-stop))

;; Dispatcher function in Operator-Pending state.
;; "cs" calls `evil-surround-change', "ds" calls `evil-surround-delete',
;; and "ys" calls `evil-surround-region'.
(evil-define-command evil-surround-edit (operation)
  "Edit the surrounding delimiters represented by CHAR.
If OPERATION is `change', call `evil-surround-change'.
if OPERATION is `delete', call `evil-surround-delete'.
Otherwise call `evil-surround-region'."
  (interactive (evil-surround-interactive-setup))
  (cond
   ((eq operation 'change)
    (call-interactively 'evil-surround-change))
   ((eq operation 'delete)
    (call-interactively 'evil-surround-delete))
   (t
    (evil-surround-setup-surround-line-operators)
    (evil-surround-call-with-repeat 'evil-surround-region)))
  ;; Return an empty range so evil-motion-range doesn't try to guess
  (let ((p (point))) (list p p 'exclusive)))

(evil-define-command evil-Surround-edit (operation)
  "Like evil-surround-edit, but for surrounding with additional new-lines.

It does nothing for change / delete."
  (interactive (evil-surround-interactive-setup))
  (cond
   ((eq operation 'change) nil)
   ((eq operation 'delete) nil)
   (t
    (evil-surround-setup-surround-line-operators)
    (evil-surround-call-with-repeat 'evil-Surround-region))))

(evil-define-operator evil-surround-region (beg end type char &optional force-new-line)
  "Surround BEG and END with CHAR.

When force-new-line is true, and region type is not line, the
following: (vertical bars indicate region start/end points)

   do |:thing|

Becomes this:

   do {
     :thing
   }"

  (interactive (evil-surround-input-region-char))
  (if evil-this-motion-count
    (evil-repeat-record (int-to-string evil-this-motion-count)))

  (when (evil-surround-valid-char-p char)
    (let* ((overlay (make-overlay beg end nil nil t))
           (evil-surround-current-pair (or evil-surround-current-pair (evil-surround-pair char)))
           (open (car evil-surround-current-pair))
           (close (cdr evil-surround-current-pair))
           (beg-pos (overlay-start overlay)))
      (unwind-protect
          (progn
            (goto-char beg-pos)
            (cond ((eq type 'block)
                   (evil-surround-block beg end char))

				  ((and (eq type 'screen-line) evil-respect-visual-line-mode)
                   (setq force-new-line
                         (or force-new-line
                             ;; Force newline if not invoked from an operator, e.g. visual line mode with VS)
                             (evil-visual-state-p)
                             ;; Or on multi-line operator surrounds (like 'ysj]')
                             (/= (line-number-at-pos) (line-number-at-pos (1- end)))))

                   (beginning-of-visual-line)
				   (skip-syntax-forward " " (save-excursion (evil-end-of-visual-line) (point)))
				   (backward-prefix-chars)
                   (setq beg-pos (point))
                   (insert open)
                   (when force-new-line (newline-and-indent))
                   (goto-char (overlay-end overlay))
                   (if force-new-line
                       (when (eobp)
                         (newline-and-indent))
                     (backward-char)
                     (evil-end-of-visual-line)
				     (skip-syntax-backward " " (save-excursion (evil-beginning-of-visual-line) (point))))
                   (insert close)
                   (when (or force-new-line
                             (/= (line-number-at-pos) (line-number-at-pos beg-pos)))
                     (indent-region beg-pos (point))
                     (newline-and-indent)))

                  ((eq type 'line)
                   (setq force-new-line
                         (or force-new-line
                             ;; Force newline if not invoked from an operator, e.g. visual line mode with VS)
                             (evil-visual-state-p)
                             ;; Or on multi-line operator surrounds (like 'ysj]')
                             (/= (line-number-at-pos) (line-number-at-pos (1- end)))))

                   (back-to-indentation)
                   (setq beg-pos (point))
                   (insert open)
                   (when force-new-line (newline-and-indent))
                   (goto-char (overlay-end overlay))
                   (if force-new-line
                       (when (eobp)
                         (newline-and-indent))
                     (backward-char)
                     (evil-last-non-blank)
                     (forward-char))
                   (insert close)
                   (when (or force-new-line
                             (/= (line-number-at-pos) (line-number-at-pos beg-pos)))
                     (indent-region beg-pos (point))
                     (newline-and-indent)))


                  (force-new-line
                   (insert open)
                   (newline-and-indent)
                   (let ((pt (point)))
                     (goto-char (overlay-end overlay))
                     (newline-and-indent)
                     (insert close)
                     (indent-region pt (point))))

                  (t
                   (insert open)
                   (goto-char (overlay-end overlay))
                   (insert close)))
            (goto-char beg-pos))
        (delete-overlay overlay)))))

(evil-define-operator evil-Surround-region (beg end type char)
  "Call surround-region, toggling force-new-line"
  (interactive (evil-surround-input-region-char))
  (evil-surround-region beg end type char t))

;;;###autoload
(define-minor-mode evil-surround-mode
  "Buffer-local minor mode to emulate surround.vim."
  :keymap (make-sparse-keymap)
  (evil-normalize-keymaps))

;;;###autoload
(defun turn-on-evil-surround-mode ()
  "Enable evil-surround-mode in the current buffer."
  (evil-surround-mode 1))

;;;###autoload
(defun turn-off-evil-surround-mode ()
  "Disable evil-surround-mode in the current buffer."
  (evil-surround-mode -1))

;;;###autoload
(define-globalized-minor-mode global-evil-surround-mode
  evil-surround-mode turn-on-evil-surround-mode
  "Global minor mode to emulate surround.vim.")

(evil-define-key 'operator evil-surround-mode-map "s" 'evil-surround-edit)
(evil-define-key 'operator evil-surround-mode-map "S" 'evil-Surround-edit)

(evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)
(evil-define-key 'visual evil-surround-mode-map "gS" 'evil-Surround-region)

(provide 'evil-surround)

;;; evil-surround.el ends here
