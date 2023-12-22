;;; evil-integration.el --- Integrate Evil with other modules -*- lexical-binding: t -*-

;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.15.0

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides evil integration for various emacs modes.
;; Additional keybindings (or default state) should go into evil-keybindings.el.

;;; Code:

(require 'evil-maps)
(require 'evil-core)
(require 'evil-macros)
(require 'evil-types)
(require 'evil-repeat)

;;; Evilize some commands

;; unbound keys should be ignored
(evil-declare-ignore-repeat 'undefined)

(mapc #'(lambda (cmd)
          (evil-set-command-property cmd :keep-visual t)
          (evil-declare-not-repeat cmd))
      '(digit-argument
        negative-argument
        universal-argument
        universal-argument-minus
        universal-argument-more
        universal-argument-other-key))
(mapc #'evil-declare-not-repeat
      '(what-cursor-position))
(mapc #'evil-declare-change-repeat
      '(dabbrev-expand
        hippie-expand
        quoted-insert))
(mapc #'evil-declare-abort-repeat
      '(balance-windows
        eval-expression
        execute-extended-command
        exit-minibuffer
        compile
        delete-window
        delete-other-windows
        find-file-at-point
        ffap-other-window
        recompile
        redo
        save-buffer
        split-window
        split-window-horizontally
        split-window-vertically
        undo
        undo-tree-redo
        undo-tree-undo))

(evil-set-type #'previous-line 'line)
(evil-set-type #'next-line 'line)

(dolist (cmd '(keyboard-quit keyboard-escape-quit))
  (evil-set-command-property cmd :suppress-operator t))

;;; Mouse
(evil-declare-insert-at-point-repeat 'mouse-yank-primary)
(evil-declare-insert-at-point-repeat 'mouse-yank-secondary)

;;; key-binding

;; Calling `keyboard-quit' should cancel repeat
(defadvice keyboard-quit (before evil activate)
  (when (fboundp 'evil-repeat-abort)
    (evil-repeat-abort)))

(eval-after-load 'wdired
  '(progn
     (add-hook 'wdired-mode-hook #'evil-change-to-initial-state)
     (defadvice wdired-change-to-dired-mode (after evil activate)
       (evil-change-to-initial-state nil t))))

;;; Parentheses

(defadvice show-paren-function (around evil disable)
  "Match parentheses in Normal state."
  (if (eq (not (memq 'not evil-highlight-closing-paren-at-point-states))
          (not (memq evil-state evil-highlight-closing-paren-at-point-states)))
      ad-do-it
    (let* ((orig-spdf show-paren-data-function)
           (show-paren-data-function
            (lambda ()
              (let ((pos (point)) narrow)
                (setq
                 pos (cl-dotimes (i (1+ (* 2 evil-show-paren-range)))
                       (setq pos (+ pos (if (cl-evenp i) i (- i))))
                       (pcase (syntax-class (syntax-after pos))
                         (4 (setq narrow pos) (cl-return pos))
                         (5 (cl-return (1+ pos))))))
                (when pos
                  (save-excursion
                    (goto-char pos)
                    (save-restriction
                      (when narrow (narrow-to-region narrow (point-max)))
                      (funcall orig-spdf))))))))
      ad-do-it)))

;;; Undo tree
(eval-after-load 'undo-tree
  '(with-no-warnings
     (evil-ex-define-cmd "undol[ist]" 'undo-tree-visualize)
     (evil-ex-define-cmd "ul" 'undo-tree-visualize)

     (define-key undo-tree-visualizer-mode-map
       [remap evil-backward-char] 'undo-tree-visualize-switch-branch-left)
     (define-key undo-tree-visualizer-mode-map
       [remap evil-forward-char] 'undo-tree-visualize-switch-branch-right)
     (define-key undo-tree-visualizer-mode-map
       [remap evil-next-line] 'undo-tree-visualize-redo)
     (define-key undo-tree-visualizer-mode-map
       [remap evil-previous-line] 'undo-tree-visualize-undo)
     (define-key undo-tree-visualizer-mode-map
       [remap evil-ret] 'undo-tree-visualizer-set)

     (define-key undo-tree-visualizer-selection-mode-map
       [remap evil-backward-char] 'undo-tree-visualizer-select-left)
     (define-key undo-tree-visualizer-selection-mode-map
       [remap evil-forward-char] 'undo-tree-visualizer-select-right)
     (define-key undo-tree-visualizer-selection-mode-map
       [remap evil-next-line] 'undo-tree-visualizer-select-next)
     (define-key undo-tree-visualizer-selection-mode-map
       [remap evil-previous-line] 'undo-tree-visualizer-select-previous)
     (define-key undo-tree-visualizer-selection-mode-map
       [remap evil-ret] 'undo-tree-visualizer-set)))

;;; Auto-complete
(eval-after-load 'auto-complete
  '(progn
     (evil-add-command-properties 'auto-complete :repeat 'evil-ac-repeat)
     (evil-add-command-properties 'ac-complete :repeat 'evil-ac-repeat)
     (evil-add-command-properties 'ac-expand :repeat 'evil-ac-repeat)
     (evil-add-command-properties 'ac-next :repeat 'ignore)
     (evil-add-command-properties 'ac-previous :repeat 'ignore)

     (defvar evil-ac-prefix-len nil
       "The length of the prefix of the current item to be completed.")

     (defvar ac-prefix)
     (defun evil-ac-repeat (flag)
       "Record the changes for auto-completion."
       (cond
        ((eq flag 'pre)
         (setq evil-ac-prefix-len (length ac-prefix))
         (evil-repeat-start-record-changes))
        ((eq flag 'post)
         ;; Add change to remove the prefix
         (evil-repeat-record-change (- evil-ac-prefix-len)
                                    ""
                                    evil-ac-prefix-len)
         ;; Add change to insert the full completed text
         (evil-repeat-record-change
          (- evil-ac-prefix-len)
          (buffer-substring-no-properties (- evil-repeat-pos
                                             evil-ac-prefix-len)
                                          (point))
          0)
         ;; Finish repeation
         (evil-repeat-finish-record-changes))))))

;;; Company
(eval-after-load 'company
  '(progn
     (mapc #'evil-declare-change-repeat
           '(company-complete-mouse
             company-complete-number
             company-complete-selection
             company-complete-common))

     (mapc #'evil-declare-ignore-repeat
           '(company-abort
             company-select-next
             company-select-previous
             company-select-next-or-abort
             company-select-previous-or-abort
             company-select-mouse
             company-show-doc-buffer
             company-show-location
             company-search-candidates
             company-filter-candidates))))

;; Eval last sexp
(cond
 ((version< emacs-version "25")
  (defadvice preceding-sexp (around evil activate)
    "In normal-state or motion-state, last sexp ends at point."
    (if (and (not evil-move-beyond-eol)
             (or (evil-normal-state-p) (evil-motion-state-p)))
        (save-excursion
          (unless (or (eobp) (eolp)) (forward-char))
          ad-do-it)
      ad-do-it))

  (defadvice pp-last-sexp (around evil activate)
    "In normal-state or motion-state, last sexp ends at point."
    (if (and (not evil-move-beyond-eol)
             (or (evil-normal-state-p) (evil-motion-state-p)))
        (save-excursion
          (unless (or (eobp) (eolp)) (forward-char))
          ad-do-it)
      ad-do-it)))
 (t
  (defun evil--preceding-sexp (command &rest args)
    "In normal-state or motion-state, last sexp ends at point."
    (if (and (not evil-move-beyond-eol)
             (or (evil-normal-state-p) (evil-motion-state-p)))
        (save-excursion
          (unless (or (eobp) (eolp)) (forward-char))
          (apply command args))
      (apply command args)))

  (advice-add 'elisp--preceding-sexp :around 'evil--preceding-sexp '((name . evil)))
  (advice-add 'pp-last-sexp          :around 'evil--preceding-sexp '((name . evil)))
  (advice-add 'lisp-eval-last-sexp   :around 'evil--preceding-sexp '((name . evil)))))

;; Show key
(defadvice quail-show-key (around evil activate)
  "Temporarily go to Emacs state"
  (evil-with-state emacs ad-do-it))

(defadvice describe-char (around evil activate)
  "Temporarily go to Emacs state"
  (evil-with-state emacs ad-do-it))

;; ace-jump-mode
(declare-function ace-jump-char-mode "ext:ace-jump-mode")
(declare-function ace-jump-word-mode "ext:ace-jump-mode")
(declare-function ace-jump-line-mode "ext:ace-jump-mode")
(defvar ace-jump-mode-scope)

(defvar evil-ace-jump-active nil)

(defmacro evil-enclose-ace-jump-for-motion (&rest body)
  "Enclose ace-jump to make it suitable for motions.
This includes restricting `ace-jump-mode' to the current window
in visual and operator state, deactivating visual updates, saving
the mark and entering `recursive-edit'."
  (declare (indent defun)
           (debug t))
  `(let ((old-mark (mark))
         (ace-jump-mode-scope
          (if (and (not (memq evil-state '(visual operator)))
                   (boundp 'ace-jump-mode-scope))
              ace-jump-mode-scope
            'window)))
     (remove-hook 'pre-command-hook #'evil-visual-pre-command t)
     (remove-hook 'post-command-hook #'evil-visual-post-command t)
     (unwind-protect
         (let ((evil-ace-jump-active 'prepare))
           (add-hook 'ace-jump-mode-end-hook
                     #'evil-ace-jump-exit-recursive-edit)
           ,@body
           (when evil-ace-jump-active
             (setq evil-ace-jump-active t)
             (recursive-edit)))
       (remove-hook 'post-command-hook
                    #'evil-ace-jump-exit-recursive-edit)
       (remove-hook 'ace-jump-mode-end-hook
                    #'evil-ace-jump-exit-recursive-edit)
       (if (evil-visual-state-p)
           (progn
             (add-hook 'pre-command-hook #'evil-visual-pre-command nil t)
             (add-hook 'post-command-hook #'evil-visual-post-command nil t)
             (set-mark old-mark))
         (push-mark old-mark)))))

(eval-after-load 'ace-jump-mode
  `(defadvice ace-jump-done (after evil activate)
     (when evil-ace-jump-active
       (add-hook 'post-command-hook #'evil-ace-jump-exit-recursive-edit))))

(defun evil-ace-jump-exit-recursive-edit ()
  "Exit a recursive edit caused by an evil jump."
  (cond
   ((eq evil-ace-jump-active 'prepare)
    (setq evil-ace-jump-active nil))
   (evil-ace-jump-active
    (remove-hook 'post-command-hook #'evil-ace-jump-exit-recursive-edit)
    (exit-recursive-edit))))

(evil-define-motion evil-ace-jump-char-mode (_count)
  "Jump visually directly to a char using ace-jump."
  :type inclusive
  (evil-without-repeat
    (let ((pnt (point))
          (buf (current-buffer)))
      (evil-enclose-ace-jump-for-motion
        (call-interactively 'ace-jump-char-mode))
      ;; if we jump backwards, motion type is exclusive, analogously
      ;; to `evil-find-char-backward'
      (when (and (equal buf (current-buffer))
                 (< (point) pnt))
        (setq evil-this-type
              (cond
               ((eq evil-this-type 'exclusive) 'inclusive)
               ((eq evil-this-type 'inclusive) 'exclusive)))))))

(evil-define-motion evil-ace-jump-char-to-mode (_count)
  "Jump visually to the char in front of a char using ace-jump."
  :type inclusive
  (evil-without-repeat
    (let ((pnt (point))
          (buf (current-buffer)))
      (evil-enclose-ace-jump-for-motion
        (call-interactively 'ace-jump-char-mode))
      (if (and (equal buf (current-buffer))
               (< (point) pnt))
          (progn
            (or (eobp) (forward-char))
            (setq evil-this-type
                  (cond
                   ((eq evil-this-type 'exclusive) 'inclusive)
                   ((eq evil-this-type 'inclusive) 'exclusive))))
        (backward-char)))))

(evil-define-motion evil-ace-jump-line-mode (_count)
  "Jump visually to the beginning of a line using ace-jump."
  :type line
  :repeat abort
  (evil-without-repeat
    (evil-enclose-ace-jump-for-motion
      (call-interactively 'ace-jump-line-mode))))

(evil-define-motion evil-ace-jump-word-mode (_count)
  "Jump visually to the beginning of a word using ace-jump."
  :type exclusive
  :repeat abort
  (evil-without-repeat
    (evil-enclose-ace-jump-for-motion
      (call-interactively 'ace-jump-word-mode))))

(define-key evil-motion-state-map [remap ace-jump-char-mode] #'evil-ace-jump-char-mode)
(define-key evil-motion-state-map [remap ace-jump-line-mode] #'evil-ace-jump-line-mode)
(define-key evil-motion-state-map [remap ace-jump-word-mode] #'evil-ace-jump-word-mode)

;;; avy
(declare-function avy-goto-word-or-subword-1 "ext:avy")
(declare-function avy-goto-line "ext:avy")
(declare-function avy-goto-char "ext:avy")
(declare-function avy-goto-char-2 "ext:avy")
(declare-function avy-goto-char-2-above "ext:avy")
(declare-function avy-goto-char-2-below "ext:avy")
(declare-function avy-goto-char-in-line "ext:avy")
(declare-function avy-goto-word-0 "ext:avy")
(declare-function avy-goto-word-1 "ext:avy")
(declare-function avy-goto-word-1-above "ext:avy")
(declare-function avy-goto-word-1-below "ext:avy")
(declare-function avy-goto-subword-0 "ext:avy")
(declare-function avy-goto-subword-1 "ext:avy")
(declare-function avy-goto-char-timer "ext:avy")
(defvar avy-all-windows)

(defmacro evil-enclose-avy-for-motion (&rest body)
  "Enclose avy to make it suitable for motions.
Based on `evil-enclose-ace-jump-for-motion'."
  (declare (indent defun)
           (debug t))
  `(let ((avy-all-windows
          (if (and (not (memq evil-state '(visual operator)))
                   (boundp 'avy-all-windows))
              avy-all-windows
            nil)))
     ,@body))

(defmacro evil-define-avy-motion (command type)
  (declare (indent defun)
           (debug t))
  (let ((name (intern (format "evil-%s" command))))
    `(evil-define-motion ,name (_count)
       ,(format "Evil motion for `%s'." command)
       :type ,type
       :jump t
       :repeat abort
       (evil-without-repeat
         (evil-enclose-avy-for-motion
           (call-interactively ',command))))))

;; define evil-avy-* motion commands for avy-* commands
(evil-define-avy-motion avy-goto-char inclusive)
(evil-define-avy-motion avy-goto-char-2 inclusive)
(evil-define-avy-motion avy-goto-char-2-above inclusive)
(evil-define-avy-motion avy-goto-char-2-below inclusive)
(evil-define-avy-motion avy-goto-char-in-line inclusive)
(evil-define-avy-motion avy-goto-char-timer inclusive)
(evil-define-avy-motion avy-goto-line line)
(evil-define-avy-motion avy-goto-line-above line)
(evil-define-avy-motion avy-goto-line-below line)
(evil-define-avy-motion avy-goto-subword-0 exclusive)
(evil-define-avy-motion avy-goto-subword-1 exclusive)
(evil-define-avy-motion avy-goto-symbol-1 exclusive)
(evil-define-avy-motion avy-goto-symbol-1-above exclusive)
(evil-define-avy-motion avy-goto-symbol-1-below exclusive)
(evil-define-avy-motion avy-goto-word-0 exclusive)
(evil-define-avy-motion avy-goto-word-1 exclusive)
(evil-define-avy-motion avy-goto-word-1-above exclusive)
(evil-define-avy-motion avy-goto-word-1-below exclusive)
(evil-define-avy-motion avy-goto-word-or-subword-1 exclusive)

;; remap avy-* commands to evil-avy-* commands
(dolist (command '(avy-goto-char
                   avy-goto-char-2
                   avy-goto-char-2-above
                   avy-goto-char-2-below
                   avy-goto-char-in-line
                   avy-goto-char-timer
                   avy-goto-line
                   avy-goto-line-above
                   avy-goto-line-below
                   avy-goto-subword-0
                   avy-goto-subword-1
                   avy-goto-symbol-1
                   avy-goto-symbol-1-above
                   avy-goto-symbol-1-below
                   avy-goto-word-0
                   avy-goto-word-1
                   avy-goto-word-1-above
                   avy-goto-word-1-below
                   avy-goto-word-or-subword-1))
  (define-key evil-motion-state-map
    (vector 'remap command) (intern-soft (format "evil-%s" command))))

;;; nXhtml/mumamo
;; ensure that mumamo does not toggle evil through its globalized mode
(eval-after-load 'mumamo
  '(with-no-warnings
     (push 'evil-mode-cmhh mumamo-change-major-mode-no-nos)))

;; visual-line-mode integration
(when evil-respect-visual-line-mode
  (evil-define-minor-mode-key 'motion 'visual-line-mode
    "j" 'evil-next-visual-line
    "gj" 'evil-next-line
    "k" 'evil-previous-visual-line
    "gk" 'evil-previous-line
    "0" 'evil-beginning-of-visual-line
    "g0" 'evil-beginning-of-line
    "$" 'evil-end-of-visual-line
    "g$" 'evil-end-of-line
    "V" 'evil-visual-screen-line))

;;; abbrev.el
(defun evil-maybe-expand-abbrev ()
  (when (and abbrev-mode evil-want-abbrev-expand-on-insert-exit)
    (expand-abbrev)))

(eval-after-load 'abbrev
  '(add-hook 'evil-insert-state-exit-hook #'evil-maybe-expand-abbrev))

;;; ElDoc
(eval-after-load 'eldoc
  '(when (fboundp 'eldoc-add-command-completions)
     (eldoc-add-command-completions "evil-window-")))

;;; XRef
(eval-after-load 'xref
  '(progn
     (evil-set-command-property 'xref-find-definitions :jump t)
     (evil-set-command-property 'xref-find-references :jump t)))

(provide 'evil-integration)

;;; evil-integration.el ends here
