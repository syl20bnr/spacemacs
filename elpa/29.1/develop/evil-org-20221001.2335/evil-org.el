;;; evil-org.el --- evil keybindings for org-mode

;; Copyright (C) 2012-2017 by Somelauw
;; Maintainer: Somelauw
;; Original-author: Edward Tjörnhammar
;; URL: https://github.com/Somelauw/evil-org-mode.git
;; Git-Repository: git://github.com/Somelauw/evil-org-mode.git
;; Created: 2012-06-14
;; Forked-since: 2017-02-12
;; Version: 1.0.3
;; Package-Requires: ((emacs "24.4") (evil "1.0"))
;; Keywords: evil vim-emulation org-mode key-bindings presets

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Minimal config
;; (add-hook 'org-mode-hook 'evil-org-mode)

;;; Commentary:
;;
;; Known Bugs:
;; See, https://github.com/Somelauw/evil-org-mode/issues
;;
;;; Code:
(eval-when-compile
  (require 'let-alist))
(require 'cl-lib)
(require 'evil)
(require 'org)
(require 'org-element)
(require 'org-table)

(defgroup evil-org nil
  "Provides integration of org-mode and evil."
  :group 'org
  :prefix "evil-org-")

;;; Customizations
(defcustom evil-org-key-theme
  (if (bound-and-true-p evil-disable-insert-state-bindings)
      '(navigation textobjects additional calendar)
    '(navigation insert textobjects additional calendar))
  "Which key themes to enable.
If you use this variable, you should call `evil-org-set-key-theme' with zero
arguments."
  :group 'evil-org
  :type '(set (const navigation)
              (const insert)
              (const return)
              (const textobjects)
              (const additional)
              (const shift)
              (const todo)
              (const heading)
              (const calendar)))

(defcustom evil-org-movement-bindings
  '((up . "k")
    (down . "j")
    (left . "h")
    (right . "l"))
  "AList of normal keys to use for arrows.
This can be used by non-qwerty users who don't use hjkl."
  :group 'evil-org
  :type '(alist :key-type symbol :value-type string)
  :options '(up down left right))

(defcustom evil-org-use-additional-insert nil
  "Whether additional keybindings should also be available in insert mode."
  :group 'evil-org
  :type 'boolean)

(defcustom evil-org-special-o/O '(table-row item)
  "When o and O should be special.
This makes them continue item lists and table rows.
By default, o and O are bound to ‘evil-org-open-above’ and ‘evil-org-open-below’."
  :group 'evil-org
  :type '(set (const table-row) (const item)))

(defcustom evil-org-retain-visual-state-on-shift nil
  "Whether < and > should retain selection when used in visual state."
  :group 'evil-org
  :type 'boolean)

(defcustom evil-org-want-hybrid-shift t
  "Whether HJKL should fall back on default bindings if not on heading/item.
This variable only takes effect when shift keytheme is enabled and should be set
before calling `evil-org-set-keytheme'."
  :group 'evil-org
  :type 'boolean)

;;; Variable declarations
(defvar browse-url-generic-program)
(defvar browse-url-generic-args)
(defvar evil-disable-insert-state-bindings)
(defvar org-capture-mode-map)

;;;###autoload
(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvilOrg"
  :group 'evil-org
  )

(with-eval-after-load 'evil-surround
  (add-to-list 'evil-surround-operator-alist '(evil-org-delete . delete)))

(defun evil-org-eol-call (fun &rest arguments)
  "Go to end of line and call provided function.
FUN function callback
Optional argument ARGUMENTS arguments to pass to FUN."
  (declare (obsolete 'evil-org-define-eol-command "0.9.4"))
  (end-of-visible-line)
  (apply fun arguments)
  (evil-insert nil))

(defun evil-org-bol-call (fun &rest arguments)
  "Go to beginning of line and call provided function.
FUN function callback
Optional argument ARGUMENTS arguments to pass to FUN."
  (declare (obsolete 'evil-org-define-bol-command "0.9.4"))
  (beginning-of-line)
  (apply fun arguments)
  (evil-insert nil))

;;; motion declarations
(evil-declare-motion 'org-beginning-of-line)
(evil-declare-motion 'org-end-of-line)
(evil-declare-motion 'org-backward-sentence)
(evil-declare-motion 'org-forward-sentence)
(evil-declare-motion 'org-backward-paragraph)
(evil-declare-motion 'org-forward-paragraph)

;; heading
(evil-declare-motion 'org-backward-heading-same-level)
(evil-declare-motion 'org-forward-heading-same-level)
(evil-declare-motion 'org-previous-visible-heading)
(evil-declare-motion 'org-next-visible-heading)

;; elements
(evil-declare-motion 'org-backward-element)
(evil-declare-motion 'org-forward-element)
(evil-declare-motion 'org-up-element)
(evil-declare-motion 'org-down-element)

;; items
(evil-declare-motion 'org-previous-item)
(evil-declare-motion 'org-next-item)
(evil-declare-motion 'org-beginning-of-item)
(evil-declare-motion 'org-end-of-item)
(evil-declare-motion 'org-beginning-of-item-list)
(evil-declare-motion 'org-end-of-item-list)

;; blocks
(evil-declare-motion 'org-previous-block)
(evil-declare-motion 'org-next-block)

;; table
(evil-declare-motion 'org-table-previous-row)
(evil-declare-motion 'org-table-next-row)
(evil-declare-motion 'org-table-previous-field)
(evil-declare-motion 'org-table-next-field)
(evil-declare-motion 'org-table-beginning-of-field)
(evil-declare-motion 'org-table-end-of-field)

;;; non-repeatible
(evil-declare-change-repeat 'org-cycle)
(evil-declare-change-repeat 'org-shifttab)
(evil-declare-change-repeat 'org-table-end-of-field)

;;; new motions
(evil-define-motion evil-org-forward-sentence (count)
  "In a table go to next cell, otherwise go to next sentence."
  :type exclusive
  :jump t
  (interactive "p")
  (if (org-at-table-p)
      (org-table-end-of-field count)
    (evil-forward-sentence-begin count)))

(evil-define-motion evil-org-backward-sentence (count)
  "In a table go to previous cell, otherwise go to previous sentence."
  :type exclusive
  :jump t
  (interactive "p")
  (if (org-at-table-p)
      (org-table-beginning-of-field count)
    (evil-backward-sentence-begin count)))

(evil-define-motion evil-org-top ()
  "Find the nearest one-star heading."
  :type exclusive
  :jump t
  (while (org-up-heading-safe)))

(evil-define-motion evil-org-end-of-line (&optional n)
  "Like evil-org-end-of-line but makes org-special-ctrl-a work in evil."
  (when (and org-special-ctrl-a/e
             evil-move-cursor-back
             (not evil-move-beyond-eol)
             (memq evil-state '(normal visual operator))
             (not (invisible-p (line-end-position)))
             (= (point) (1- (line-end-position))))
    (forward-char))
  (org-end-of-line n))

(defalias 'evil-org-beginning-of-line 'org-beginning-of-line)

;;; insertion commands
(defun evil-org-insert-line (count)
  "Insert at beginning of line.
If ‘org-special-ctrl-a/e’ insertion will be done after heading and item markers.
The insertion will be repeated COUNT times."
  (interactive "p")
  (if (org-at-heading-or-item-p)
      ;; Manipulate org-beginning-of-line to become special
      (progn (beginning-of-line)
             (org-beginning-of-line nil)
             (evil-insert count))
    (evil-insert-line count)))

(defun evil-org-append-line (count)
  "Append at end of line before ellipses if present.
If ‘org-special-ctrl-a/e’ insert before tags on headlines.
The insertion will be repeated COUNT times."
  (interactive "p")
  (if (org-at-heading-p)
      ;; Manipulate org-end-of-line to become special
      (progn (end-of-line)
             (org-end-of-line nil)
             (evil-insert count))
    (evil-append-line count)))

(defun evil-org-open-below (count)
  "Clever insertion of org item.
Argument COUNT number of lines to insert.
The behavior in items and tables can be controlled using ‘evil-org-special-o/O’.
Passing in any prefix argument, executes the command without special behavior."
  (interactive "P")
  (cond ((and (memq 'table-row evil-org-special-o/O) (org-at-table-p))
         (org-table-insert-row '(4))
         (evil-insert nil))
        ((and (memq 'item evil-org-special-o/O) (org-at-item-p)
              (progn (end-of-visible-line)
                     (org-insert-item (org-at-item-checkbox-p))))
         (evil-insert nil))
        ((evil-open-below count))))

(defun evil-org-open-above (count)
  "Clever insertion of org item.
Argument COUNT number of lines to insert.
The behavior in items and tables can be controlled using ‘evil-org-special-o/O’.
Passing in any prefix argument, executes the command without special behavior."
  (interactive "P")
  (cond ((and (memq 'table-row evil-org-special-o/O) (org-at-table-p))
         (org-table-insert-row)
         (evil-insert nil))
        ((and (memq 'item evil-org-special-o/O) (org-at-item-p)
              (progn (beginning-of-line)
                     (org-insert-item (org-at-item-checkbox-p))))
         (evil-insert nil))
        ((evil-open-above count))))

(defun evil-org-return (arg)
  "Like `org-return', but continues items and tables like `evil-open-below'.
Pressing return twice cancels the continuation of the itemlist or table.
If ARG is set it will not cancel the continuation.
The behavior of this function can be controlled using `evil-org-special-o/O’."
  (interactive "P")
  (cond ((and (not arg) (evil-org--empty-element-p))
         (delete-region (line-beginning-position) (line-end-position)))
        ((eolp)
         (if (bolp)
             (call-interactively #'org-return)
           (call-interactively #'evil-org-open-below)))
        ('otherwise
         (call-interactively #'org-return-indent))))

(defun evil-org--empty-element-p ()
  "Return if pointer is on an empty element."
  (cond ((org-at-table-p)
         (let* ((rows (cl-remove 'hline (org-table-to-lisp)))
                (row (nth (1- (org-table-current-line)) rows)))
           (cl-every 'string-empty-p row)))
        ((org-at-item-p)
         (string-match-p "^[[:space:]]*\\([+-]\\|[[:digit:]]+[.)]\\)[[:space:]]*\\(::[[:space:]]*\\)?$"
                         (thing-at-point 'line)))))

;; other
(defun evil-org-shift-fallback-command ()
  "Call the default evil command that is bound the currently pressed key."
  (when (and evil-mode evil-org-mode evil-org-want-hybrid-shift)
    (let ((key (this-command-keys)))
      (when-let ((command (or (lookup-key evil-normal-state-map key)
                              (lookup-key evil-motion-state-map key))))
        (when (commandp command)
          (call-interactively command)
          t)))))

(defmacro evil-org-define-eol-command (cmd)
  "Return a function that executes CMD at eol and then enters insert state.
eol stands for end of line.
For many org functions such as `org-insert-heading', this creates a heading below the current line."
  (let ((newcmd (intern (concat "evil-org-" (symbol-name cmd) "-below"))))
    `(progn
       (defun ,newcmd ()
         ,(concat "Call `" (symbol-name cmd) "' at end of line and go to insert mode.")
         (interactive)
         (end-of-visible-line)
         (call-interactively #',cmd)
         (evil-insert nil))
       #',newcmd)))

(defmacro evil-org-define-bol-command (cmd)
  "Return a function that executes CMD at bol and then enters insert state.
bol stands for beginning of line.
For many org functions such as `org-insert-heading', this creates a heading above the current line."
  (let ((newcmd (intern (concat "evil-org-" (symbol-name cmd) "-above"))))
    `(progn
       (defun ,newcmd ()
         ,(concat "Call `" (symbol-name cmd) "' at beginning of line and go to insert mode.")
         (interactive)
         (beginning-of-line)
         (call-interactively #',cmd)
         (evil-insert nil))
       #',newcmd)))

;;; operators
(evil-define-operator evil-org-> (beg end count)
  "Demote, indent, move column right.
In items or headings, demote heading/item.
In code blocks, indent lines
In tables, move column to the right."
  :move-point nil
  (interactive "<r><vc>")
  (when (null count) (setq count 1))
  (cond
   ;; Work with subtrees and headings
   ((org-with-limited-levels
     (or (org-at-heading-p)
         (save-excursion (goto-char beg) (org-at-heading-p))))
    (if (> count 0)
        (org-map-region 'org-do-demote beg end)
      (org-map-region 'org-do-promote beg end)))
   ;; Shifting table columns
   ((and (org-at-table-p)
         (save-excursion
           (goto-char beg)
           (<= (line-beginning-position) end (line-end-position))))
    (evil-org-table-move-column beg end count))
   ;; Work with items
   ((and (org-at-item-p)
         (<= end (save-excursion (org-end-of-item-list))))
    (evil-org-indent-items beg end count))
   ;; Default indentation
   (t
    ;; special casing tables
    (when (and (not (region-active-p)) (org-at-table-p))
      (setq beg (min beg (org-table-begin)))
      (setq end (max end (org-table-end))))
    (evil-shift-right beg end count)))
  (when (and evil-org-retain-visual-state-on-shift (evil-visual-state-p))
    (evil-normal-state)
    (evil-visual-restore)))

(evil-define-operator evil-org-< (beg end count)
  "Promote, dedent, move column left.
In items or headings, promote heading/item.
In code blocks, indent lines
In tables, move column to the left."
  (interactive "<r><vc>")
  (evil-org-> beg end (- (or count 1))))

(defun evil-org-indent-items (beg end count)
  "Indent all selected items in itemlist.
Argument BEG Begin of subtree items to indent.
Argument END End of subtree items to indent.
Argument COUNT if negative, items are dedented instead."
  (when (null count) (setq count 1))
  (let* ((struct (save-excursion (goto-char beg) (org-list-struct)))
         (region-p (region-active-p)))
    ;; special case: indenting all items
    (if (and struct org-list-automatic-rules (not region-p)
             (= (point-at-bol) (org-list-get-top-point struct)))
        (org-list-indent-item-generic count nil struct)
      ;; indenting selected items
      (save-excursion
        (when region-p (deactivate-mark))
        (set-mark beg)
        (goto-char end)
        (org-list-indent-item-generic count t struct)))))

(defun evil-org-table-move-column (beg end arg)
  "Move org table column.
Argument BEG, first column
Argument END, second column
If ARG > 0, move column BEG to END.
If ARG < 0, move column END to BEG"
  (let* ((text (buffer-substring beg end))
         (n-cells-selected (max 1 (cl-count ?| text)))
         (n-columns-to-move (* n-cells-selected (abs arg)))
         (move-left-p (< arg 0)))
    (goto-char (if move-left-p end beg))
    (dotimes (_ n-columns-to-move) (org-table-move-column move-left-p))))

(evil-define-operator evil-org-delete-char (count beg end type register)
  "Combine evil-delete-char with org-delete-char"
  :motion evil-forward-char
  (interactive "p<R><x>")
  (if (evil-visual-state-p)             ; No special support for visual state
      (evil-delete-char beg end type register)
    (evil-set-register ?- (filter-buffer-substring beg end))
    (evil-yank beg end type register)
    (org-delete-char count)))

(evil-define-operator evil-org-delete-backward-char (count beg end type register)
  "Combine evil-delete-backward-char with org-delete-backward-char"
  :motion evil-backward-char
  (interactive "p<R><x>")
  (if (evil-visual-state-p)             ; No special support for visual state
      (evil-delete-backward-char beg end type register)
    (evil-set-register ?- (filter-buffer-substring beg end))
    (evil-yank beg end type register)
    (org-delete-char count)))

(evil-define-operator evil-org-delete (beg end type register yank-handler)
  "Like evil-delete, but realigns tags and numbered lists."
  (interactive "<R><x><y>")
  (let ((renumber-lists-p (or (< beg (line-beginning-position))
                              (> end (line-end-position)))))
    (evil-delete beg end type register yank-handler)
    (cond ((and renumber-lists-p (org-at-item-p))
           (org-list-repair))
          ((org-at-heading-p)
           (org-fix-tags-on-the-fly)))))

(defun evil-org-generic-open-links (beg end incog)
  "Open org mode links in visual selection.
Argument BEG beginning of region.
Argument END end of region.
Argument INCOG whether to open in incognito mode."
  (progn
    (save-excursion
      (goto-char beg)
      (catch 'break
        (while t
          (org-next-link)
          ;; break from outer loop when there are no more
          ;; org links
          (when (or (not (< (point) end))
                    (not (null org-link-search-failed)))
            (throw 'break 0))
          (if (not (null incog))
              (evil-org-open-incognito)
            (let ((browse-url-generic-args '("")))
              (org-open-at-point '(16)))))))))

(defun evil-org-open-incognito ()
  "Open link in new incognito window."
  (interactive)
  (let* ((new-arg
          ;; decide which incognito settings to use depending on the browser
          (cond ((not (null (string-match "^.*\\(iceweasel\\|firefox\\).*$" browse-url-generic-program))) "--private-window")
                ((not (null (string-match "^.*\\(chrome\\|chromium\\).*$" browse-url-generic-program))) "--incognito")
                (t "")))
         (old-b (list browse-url-generic-args " "))
         (browse-url-generic-args (add-to-ordered-list 'old-b new-arg 0)))
    (org-open-at-point '(16))))

(evil-define-operator evil-org-open-links (beg end &optional count)
  "Open links in visual selection.
If a prefix argument is given, links are opened in incognito mode."
  :keep-visual t
  :move-point nil
  (interactive "<r><vc>")
  (evil-org-generic-open-links beg end (not (null count))))

(evil-define-operator evil-org-open-links-incognito (beg end)
  "Open links in visual selection in incognito mode."
  :keep-visual t
  :move-point nil
  (interactive "<r>")
  (evil-org-generic-open-links beg end t))

;;; text-objects
(defun evil-org-select-an-element (element)
  "Select an org ELEMENT."
  (list (min (region-beginning) (org-element-property :begin element))
        (org-element-property :end element)))

(defun evil-org-select-inner-element (element)
  "Select inner org ELEMENT."
  (let ((type (org-element-type element))
        (begin (org-element-property :begin element))
        (end (org-element-property :end element))
        (contents-begin (org-element-property :contents-begin element))
        (contents-end (org-element-property :contents-end element))
        (post-affiliated (org-element-property :post-affiliated element))
        (post-blank (org-element-property :post-blank element)))
    (cond ((or (string-suffix-p "-block" (symbol-name type))
               (memq type '(latex-environment)))
           ;; Special case on block types (thanks Nicolas Goaziou)
           (list (org-with-point-at post-affiliated (line-beginning-position 2))
                 (org-with-point-at end (line-beginning-position (- post-blank)))))
          ((memq type '(verbatim code))
           (list (1+ begin) (- end post-blank 1)))
          ('otherwise
           (list (or contents-begin post-affiliated begin)
                 (or contents-end
                     ;; Prune post-blank lines from :end element
                     (org-with-point-at end
                       ;; post-blank is charwise for objects and linewise for elements
                       (if (memq type org-element-all-objects)
                           (- end post-blank)
                         (line-end-position (- post-blank))))))))))


(defun evil-org-parent (element)
  "Find a parent or nearest heading of ELEMENT."
  (or (org-element-property :parent element)
      (save-excursion
        (goto-char (org-element-property :begin element))
        (if (org-with-limited-levels (org-at-heading-p))
            (org-up-heading-safe)
          (org-with-limited-levels (org-back-to-heading)))
        (org-element-at-point))))

(evil-define-text-object evil-org-an-object (count beg end type)
  "An org object.
Matches urls and table cells."
  (when (null end) (setq end (point)))
  (when (null beg) (setq beg (point)))
  (let* ((first (org-element-context))
         (element first))
    ;; select next object on repetitive presses
    (goto-char end)
    (when (<= (org-element-property :end element) end)
      (setq element (org-element-context)))
    (dotimes (_ (1- count))
      (goto-char (org-element-property :end element))
      (setq element (org-element-context)))
    (evil-org-select-an-element element)))

(evil-define-text-object evil-org-inner-object (count &optional beg end type)
  "Select an org object.
Matches urls and table cells."
  (evil-org-select-inner-element (org-element-context)))

(evil-define-text-object evil-org-an-element (count &optional beg end type)
  "An org element.
Includes paragraphs, table rows and code blocks.
"
  (let* ((first (org-element-at-point))
         (element first))
    (when (and end (>= end (org-element-property :end element)))
      (org-forward-element)
      (setq element (org-element-at-point)))
    (dotimes (_ (1- count))
      (org-forward-element)
      (setq element (org-element-at-point)))
    (evil-org-select-an-element element)))

(evil-define-text-object evil-org-inner-element (count &optional beg end type)
  "Inner org element.
Includes paragraphs, table rows and code blocks.
"
  (evil-org-select-inner-element (org-element-at-point)))

(evil-define-text-object evil-org-a-greater-element (count &optional beg end type)
  "A greater (recursive) org element.
Includes tables, list items and subtrees."
  :type line
  (when (null count) (setq count 1))
  (save-excursion
    (when beg (goto-char beg))
    (let ((element (org-element-at-point)))
      (when (or (not (memq (cl-first element) org-element-greater-elements))
                (and end (>= end (org-element-property :end element))))
        (setq element (evil-org-parent element)))
      (dotimes (_ (1- count))
        (setq element (evil-org-parent element)))
      (evil-org-select-an-element element))))

(evil-define-text-object evil-org-inner-greater-element (count &optional beg end type)
  "Inner greater (recursive) org element.
Includes tables, list items and subtrees."
  (when (null count) (setq count 1))
  (save-excursion
    (when beg (goto-char beg))
    (let ((element (org-element-at-point)))
      (unless (memq (cl-first element) org-element-greater-elements)
        (setq element (evil-org-parent element)))
      (dotimes (_ (1- count))
        (setq element (evil-org-parent element)))
      (evil-org-select-inner-element element))))

(evil-define-text-object evil-org-a-subtree (count &optional beg end type)
  "An org subtree."
  :type line
  (when (null count) (setq count 1))
  (org-with-limited-levels
   (cond ((org-at-heading-p) (beginning-of-line))
         ((org-before-first-heading-p) (user-error "Not in a subtree"))
         (t (outline-previous-visible-heading 1))))
  (when count (while (and (> count 1) (org-up-heading-safe)) (cl-decf count)))
  (evil-org-select-an-element (org-element-at-point)))

(evil-define-text-object evil-org-inner-subtree (count &optional beg end type)
  "Inner org subtree."
  :type line
  (when (null count) (setq count 1))
  (org-with-limited-levels
   (cond ((org-at-heading-p) (beginning-of-line))
         ((org-before-first-heading-p) (user-error "Not in a subtree"))
         (t (outline-previous-visible-heading 1))))
  (when count (while (and (> count 1) (org-up-heading-safe)) (cl-decf count)))
  (evil-org-select-inner-element (org-element-at-point)))
;;; Keythemes
(defun evil-org--populate-base-bindings ()
  "Bindings that are always available."
  (evil-define-key 'motion 'evil-org-mode
    (kbd "0") 'evil-org-beginning-of-line
    (kbd "$") 'evil-org-end-of-line
    (kbd ")") 'evil-org-forward-sentence
    (kbd "(") 'evil-org-backward-sentence
    (kbd "}") 'org-forward-paragraph
    (kbd "{") 'org-backward-paragraph)
  (evil-define-key 'normal 'evil-org-mode
    (kbd "I") 'evil-org-insert-line
    (kbd "A") 'evil-org-append-line
    (kbd "o") 'evil-org-open-below
    (kbd "O") 'evil-org-open-above
    (kbd "d") 'evil-org-delete
    (kbd "x") 'evil-org-delete-char
    (kbd "X") 'evil-org-delete-backward-char
    (kbd "<C-return>") (evil-org-define-eol-command
                        org-insert-heading-respect-content)
    (kbd "<C-S-return>") (evil-org-define-eol-command
                          org-insert-todo-heading-respect-content))
  (evil-define-key '(normal visual) 'evil-org-mode
    (kbd "<tab>") 'org-cycle
    (kbd "g TAB") 'org-cycle
    (kbd "<backtab>") 'org-shifttab
    (kbd "<") 'evil-org-<
    (kbd ">") 'evil-org->))

(defun evil-org--populate-textobjects-bindings ()
  "Text objects."
  (evil-define-key '(visual operator) 'evil-org-mode
    "ae" 'evil-org-an-object
    "ie" 'evil-org-inner-object
    "aE" 'evil-org-an-element
    "iE" 'evil-org-inner-element
    "ir" 'evil-org-inner-greater-element
    "ar" 'evil-org-a-greater-element
    "aR" 'evil-org-a-subtree
    "iR" 'evil-org-inner-subtree))

(defun evil-org--populate-insert-bindings ()
  "Define insert mode bindings."
  (evil-define-key 'insert 'evil-org-mode
    (kbd "C-t") 'org-metaright
    (kbd "C-d") 'org-metaleft))

(defun evil-org--populate-navigation-bindings ()
  "Configures gj/gk/gh/gl for navigation."
  (let-alist evil-org-movement-bindings
    ;; Other evil packages can override normal state and visual state bindings.
    ;; Thus, it is necessary to bind keys on motion, normal, and visual states.
    (evil-define-key '(motion normal visual) 'evil-org-mode
      (kbd (concat "g" .left)) 'org-up-element
      (kbd (concat "g" .right)) 'org-down-element
      (kbd (concat "g" .up)) 'org-backward-element
      (kbd (concat "g" .down)) 'org-forward-element
      (kbd (concat "g" (capitalize .left))) 'evil-org-top)))

(defun evil-org--populate-additional-bindings ()
  "Bindings with meta and control."
  (let-alist evil-org-movement-bindings
    (let ((state (if evil-org-use-additional-insert
                     '(normal visual insert)
                   '(normal visual))))
      (evil-define-key state 'evil-org-mode
        (kbd (concat "M-" .left)) 'org-metaleft
        (kbd (concat "M-" .right)) 'org-metaright
        (kbd (concat "M-" .up)) 'org-metaup
        (kbd (concat "M-" .down)) 'org-metadown
        (kbd (concat "M-" (capitalize .left))) 'org-shiftmetaleft
        (kbd (concat "M-" (capitalize .right))) 'org-shiftmetaright
        (kbd (concat "M-" (capitalize .up))) 'org-shiftmetaup
        (kbd (concat "M-" (capitalize .down))) 'org-shiftmetadown
        (kbd (concat "C-S-" .left)) 'org-shiftcontrolleft
        (kbd (concat "C-S-" .right)) 'org-shiftcontrolright
        (kbd (concat "C-S-" .up)) 'org-shiftcontrolup
        (kbd (concat "C-S-" .down)) 'org-shiftcontroldown))))

(defun evil-org--populate-shift-bindings ()
  "Shift bindings that conflict with evil bindings."
  (let-alist evil-org-movement-bindings
    (evil-define-key 'normal 'evil-org-mode
      (capitalize .left) 'org-shiftleft
      (capitalize .right) 'org-shiftright
      (capitalize .down) 'org-shiftdown
      (capitalize .up) 'org-shiftup)

    ;; Make shift keys fall back on the keys they have replaced
    (when evil-org-want-hybrid-shift
      (dolist (hook '(org-shiftleft-final-hook
                      org-shiftright-final-hook
                      org-shiftdown-final-hook
                      org-shiftup-final-hook))
        (add-hook hook #'evil-org-shift-fallback-command 'append)))))

(defun evil-org--populate-todo-bindings ()
  "Bindings for easy todo insertion."
  (evil-define-key 'normal 'evil-org-mode
    (kbd "t") 'org-todo
    (kbd "T") (evil-org-define-eol-command org-insert-todo-heading)
    (kbd "M-t") (evil-org-define-eol-command org-insert-todo-subheading)))

(defun evil-org--populate-heading-bindings ()
  "Bindings for easy heading insertion."
  (evil-define-key 'normal 'evil-org-mode
    (kbd "O") (evil-org-define-eol-command org-insert-heading)
    (kbd "M-o") (evil-org-define-eol-command org-insert-subheading)))

(defun evil-org--populate-calendar-bindings ()
  "Bindings for easy date selection."
  (define-key org-read-date-minibuffer-local-map
    (kbd "C-f") (lambda () (interactive)
                  (org-eval-in-calendar
                   '(calendar-scroll-left-three-months 1))))
  (define-key org-read-date-minibuffer-local-map
    (kbd "C-b") (lambda () (interactive)
                  (org-eval-in-calendar
                   '(calendar-scroll-right-three-months 1))))
  (let-alist evil-org-movement-bindings
    (define-key org-read-date-minibuffer-local-map
      (kbd (concat "M-" .left)) (lambda () (interactive)
                    (org-eval-in-calendar '(calendar-backward-day 1))))
    (define-key org-read-date-minibuffer-local-map
      (kbd (concat "M-" .right)) (lambda () (interactive)
                    (org-eval-in-calendar '(calendar-forward-day 1))))
    (define-key org-read-date-minibuffer-local-map
      (kbd (concat "M-" .up)) (lambda () (interactive)
                    (org-eval-in-calendar '(calendar-backward-week 1))))
    (define-key org-read-date-minibuffer-local-map
      (kbd (concat "M-" .down)) (lambda () (interactive)
                    (org-eval-in-calendar '(calendar-forward-week 1))))
    (define-key org-read-date-minibuffer-local-map
      (kbd (concat "M-" (capitalize .left))) (lambda () (interactive)
                      (org-eval-in-calendar '(calendar-backward-month 1))))
    (define-key org-read-date-minibuffer-local-map
      (kbd (concat "M-" (capitalize .right))) (lambda () (interactive)
                      (org-eval-in-calendar '(calendar-forward-month 1))))
    (define-key org-read-date-minibuffer-local-map
      (kbd (concat "M-" (capitalize .up))) (lambda () (interactive)
                      (org-eval-in-calendar '(calendar-backward-year 1))))
    (define-key org-read-date-minibuffer-local-map
      (kbd (concat "M-" (capitalize .down))) (lambda () (interactive)
                      (org-eval-in-calendar '(calendar-forward-year 1))))))

(defun evil-org-set-key-theme (&optional theme)
  "Select what keythemes to enable.
Optional argument THEME list of themes. See evil-org-key-theme for a list of values."
  (dolist (state evil-minor-mode-keymaps-alist)
    ;; Remove evil-org-mode keymaps in evil minor-mode keymaps
    (setcdr state (assq-delete-all 'evil-org-mode (cdr state))))
  (evil-org--populate-base-bindings)
  (let ((theme (or theme evil-org-key-theme)))
    (when (memq 'navigation theme) (evil-org--populate-navigation-bindings))
    (when (memq 'insert theme) (evil-org--populate-insert-bindings))
    (when (memq 'return theme)
      (evil-define-key '(insert emacs) 'evil-org-mode
        (kbd "RET") 'evil-org-return))
    (when (memq 'textobjects theme) (evil-org--populate-textobjects-bindings))
    (when (memq 'additional theme) (evil-org--populate-additional-bindings))
    (when (memq 'shift theme) (evil-org--populate-shift-bindings))
    (when (memq 'todo theme) (evil-org--populate-todo-bindings))
    (when (memq 'heading theme) (evil-org--populate-heading-bindings))
    (when (memq 'calendar theme) (evil-org--populate-calendar-bindings))))

;;; vim-like confirm/abort for capture and src
;;; Taken from mwillsey (Max Willsey) on https://github.com/syl20bnr/spacemacs/pull/7400
(with-eval-after-load 'org-capture
  (define-key org-capture-mode-map [remap evil-save-and-close]          'org-capture-finalize)
  (define-key org-capture-mode-map [remap evil-save-modified-and-close] 'org-capture-finalize)
  (define-key org-capture-mode-map [remap evil-quit]                    'org-capture-kill))

(with-eval-after-load 'org-src
  (define-key org-src-mode-map [remap evil-save-and-close]          'org-edit-src-exit)
  (define-key org-src-mode-map [remap evil-save-modified-and-close] 'org-edit-src-exit)
  (define-key org-src-mode-map [remap evil-quit]                    'org-edit-src-abort))

(with-eval-after-load 'org-table
  (define-key org-table-fedit-map [remap evil-save-and-close]          'org-table-fedit-finish)
  (define-key org-table-fedit-map [remap evil-save-modified-and-close] 'org-table-fedit-finish)
  (define-key org-table-fedit-map [remap evil-quit]                    'org-table-fedit-abort))

;; Set customizable theme
(evil-org-set-key-theme evil-org-key-theme)

(provide 'evil-org)
;;; evil-org.el ends here
