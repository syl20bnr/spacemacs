;;; ido-vertical-mode.el --- Makes ido-mode display vertically.

;; Copyright (C) 2013, 2014  Steven Degutis

;; Author: Steven Degutis
;; Maintainer: Christopher Reichert <creichert07@gmail.com>
;; Version: 1.0.0
;; Package-Version: 20180618.2101
;; Keywords: convenience
;; URL: https://github.com/creichert/ido-vertical-mode.el

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

;;; Commentary:

;; Makes ido-mode display prospects vertically

;;; Code:

(require 'ido)

;;; The following three variables and their comments are lifted
;;; directly from `ido.el'; they are defined here to avoid compile-log
;;; warnings. See `ido.el' for more information.

;; Non-nil if we should add [confirm] to prompt
(defvar ido-show-confirm-message)

;; Remember if current directory is non-readable (so we cannot do completion).
(defvar ido-directory-nonreadable)

;; Remember if current directory is 'huge' (so we don't want to do completion).
(defvar ido-directory-too-big)

(defcustom ido-vertical-indicator "->"
  "Indicator displayed next to the candidate that will be selected."
  :type 'string
  :group 'ido-vertical-mode)

(defvar ido-vertical-decorations
  `(,(format "\n%s " ido-vertical-indicator)  ; left bracket around prospect list
    ""                                        ; right bracket around prospect list
    "\n   "                                   ; separator between prospects, depends on `ido-separator`
    "\n   ..."                                ; inserted at the end of a truncated list of prospects
    "["                                       ; left bracket around common match string
    "]"                                       ; right bracket around common match string
    " [No match]"
    " [Matched]"
    " [Not readable]"
    " [Too big]"
    " [Confirm]"
    ,(format "\n%s " ido-vertical-indicator)  ; left bracket around the sole remaining completion
    ""                                        ; right bracket around the sole remaining completion
    )

  "Changing the decorations does most of the work for ido-vertical

This sets up newlines and arrows before, between, and after the
prospects. For additional information, see `ido-decorations'.")

(defvar ido-vertical-old-decorations nil
  "The original `ido-decorations' variable

We need to keep track of the original value so we can restore it
when turning `ido-vertical-mode' off")

(defvar ido-vertical-old-completions nil
  "The original `ido-completions' function

We need to keep track of the original value of `ido-completions'
so we can restore it when turning `ido-vertical-mode' off")

(defgroup ido-vertical-mode nil
  "Make ido behave vertically."
  :group 'ido)

(defcustom ido-vertical-show-count nil
  "Non nil means show the count of candidates while completing."
  :type 'boolean
  :group 'ido-vertical-mode)

(defvar ido-vertical-count-active nil
  "Used internally to track whether we're already showing the count")

(defcustom ido-vertical-define-keys nil
  "Defines which keys that `ido-vertical-mode' redefines."
  :type '(choice
          (const :tag "Keep default ido keys." nil)
          (const :tag "C-p and C-n are up & down in match" C-n-and-C-p-only)
          (const :tag "C-p/up and C-n/down are up and down in match." C-n-C-p-up-and-down)
          (const :tag "C-p/up, C-n/down are up/down in match. left or right cycle history or directory." C-n-C-p-up-down-left-right))
  :group 'ido-vertical-mode)

(defcustom ido-vertical-pad-list t
  "Non nil means to pad the list of candidates to ensure the minibuffer area is always tall"
  :type 'boolean
  :group 'ido-vertical-mode)

(defcustom ido-vertical-disable-if-short nil
  "Non nil means that ido will go back to horizontal mode if the candidates all fit in the minibuffer area"
  :type 'boolean
  :group 'ido-vertical-mode)

(defface ido-vertical-first-match-face
  '((t (:inherit ido-first-match)))
  "Face used by Ido Vertical for highlighting first match."
  :group 'ido-vertical-mode)

(defface ido-vertical-only-match-face
  '((t (:inherit ido-only-match)))
  "Face used by Ido Vertical for highlighting only match."
  :group 'ido-vertical-mode)

(defface ido-vertical-match-face
  '((t (:inherit font-lock-variable-name-face :bold t :underline t)))
  "Face used by Ido Vertical for the matched part."
  :group 'ido-vertical-mode)

(defun ido-vertical-or-horizontal-completions (name)
  (if (and ido-vertical-disable-if-short
           (<= (length ido-matches) ido-max-prospects))

      (let ((short-result
             (let ((ido-decorations ido-vertical-old-decorations))
               (funcall ido-vertical-old-completions name))))
        (if (>= (window-body-width (minibuffer-window))
                (+ (minibuffer-prompt-width)
                   (length short-result)))
            short-result
          (ido-vertical-completions name)))

    (ido-vertical-completions name)))

;; borrowed from ido.el and modified to work better when vertical
(defun ido-vertical-completions (name)
  ;; Return the string that is displayed after the user's text.
  ;; Modified from `icomplete-completions'.

  (let* ((comps ido-matches)
         (ind (and (consp (car comps)) (> (length (cdr (car comps))) 1)
                   ido-merged-indicator))
         (lencomps (length comps))
         (additional-items-indicator (nth 3 ido-decorations))
         (comps-empty (null comps))
         (ncomps lencomps)
         first)

    ;; Keep the height of the suggestions list constant by padding
    ;; when lencomps is too small. Also, if lencomps is too short, we
    ;; should not indicate that there are additional prospects.
    (when (< lencomps (1+ ido-max-prospects))
      (setq additional-items-indicator "\n")
      (when ido-vertical-pad-list
        (setq comps (append comps (make-list (- (1+ ido-max-prospects) lencomps) "")))
        (setq ncomps (length comps))))

    (if (not ido-incomplete-regexp)
        (when ido-use-faces
          ;; Make a copy of [ido-matches], otherwise the selected string
          ;; could contain text properties which could lead to weird
          ;; artifacts, e.g. buffer-file-name having text properties.
          (setq comps (cl-loop for comps-i being the elements of (if (eq comps ido-matches)
                                                                     ido-matches
                                                                   comps)
                               do
                               (setf comps-i (substring-no-properties
                                              (if (listp comps-i)
                                                  (car comps-i)
                                                comps-i)
                                              0))
                               (when (string-match (if ido-enable-regexp name (regexp-quote name)) comps-i)
                                 (ignore-errors
                                   (add-face-text-property (match-beginning 0)
                                                           (match-end 0)
                                                           'ido-vertical-match-face
                                                           nil comps-i)))
                               collect comps-i))))

    (if (and ind ido-use-faces)
        (put-text-property 0 1 'face 'ido-indicator ind))

    (when ido-vertical-show-count
      (setcar ido-vertical-decorations (format " [%d]\n%s " lencomps ido-vertical-indicator))
      (setq ido-vertical-count-active t))
    (when (and (not ido-vertical-show-count)
               ido-vertical-count-active)
      (setcar ido-vertical-decorations (format "\n%s "ido-vertical-indicator))
      (setq ido-vertical-count-active nil))

    (if (and ido-use-faces comps)
        (let* ((fn (ido-name (car comps)))
               (ln (length fn)))
          (setq first (format "%s" fn))
          (if (fboundp 'add-face-text-property)
              (add-face-text-property 0 (length first)
                                      (cond ((> lencomps 1)
                                             'ido-vertical-first-match-face)

                                            (ido-incomplete-regexp
                                             'ido-incomplete-regexp)

                                            (t
                                             'ido-vertical-only-match-face))
                                      nil first)
            (put-text-property 0 ln 'face
                               (if (= lencomps 1)
                                   (if ido-incomplete-regexp
                                       'ido-incomplete-regexp
                                     'ido-vertical-only-match-face)
                                 'ido-vertical-first-match-face)
                               first))
          (if ind (setq first (concat first ind)))
          (setq comps (cons first (cdr comps)))))

    ;; Previously we'd check null comps to see if the list was
    ;; empty. We pad the list with empty items to keep the list at a
    ;; constant height, so we have to check if the entire list is
    ;; empty, instead of (null comps)
    (cond (comps-empty
           (cond
            (ido-show-confirm-message
             (or (nth 10 ido-decorations) " [Confirm]"))
            (ido-directory-nonreadable
             (or (nth 8 ido-decorations) " [Not readable]"))
            (ido-directory-too-big
             (or (nth 9 ido-decorations) " [Too big]"))
            (ido-report-no-match
             (nth 6 ido-decorations)) ;; [No match]
            (t "")))
          (ido-incomplete-regexp
           (concat " " (car comps)))
          ((null (cdr comps))                       ;one match
           (concat (concat (nth 11 ido-decorations) ;; [ ... ]
                           (ido-name (car comps))
                           (nth 12 ido-decorations))
                   (if (not ido-use-faces) (nth 7 ido-decorations)))) ;; [Matched]
          (t                            ;multiple matches
           (let* ((items (if (> ido-max-prospects 0) (1+ ido-max-prospects) 999))
                  (alternatives
                   (apply
                    #'concat
                    (cdr (apply
                          #'nconc
                          (mapcar
                           (lambda (com)
                             (setq com (ido-name com))
                             (setq items (1- items))
                             (cond
                              ((< items 0) ())
                              ((= items 0) (list additional-items-indicator)) ; " | ..."
                              (t
                               (list (nth 2 ido-decorations) ; " | "
                                     (let ((str (substring com 0)))
                                       (if (and ido-use-faces
                                                (not (string= str first))
                                                (ido-final-slash str))
                                           (put-text-property 0 (length str) 'face 'ido-subdir str))
                                       str)))))
                           comps))))))

             (concat
              ;; put in common completion item -- what you get by pressing tab
              (if (and (stringp ido-common-match-string)
                       (> (length ido-common-match-string) (length name)))
                  (concat (nth 4 ido-decorations) ;; [ ... ]
                          (substring ido-common-match-string (length name))
                          (nth 5 ido-decorations)))
              ;; list all alternatives
              (nth 0 ido-decorations) ;; { ... }
              alternatives
              (nth 1 ido-decorations)))))))

(defun ido-vertical-disable-line-truncation ()
  "Prevent the newlines in the minibuffer from being truncated"
  (set (make-local-variable 'truncate-lines) nil))

(defun turn-on-ido-vertical ()
  (if (and (eq nil ido-vertical-old-decorations)
           (eq nil ido-vertical-old-completions))
      (progn
        (setq ido-vertical-old-decorations ido-decorations)
        (setq ido-vertical-old-completions (symbol-function 'ido-completions))))

  (setq ido-decorations ido-vertical-decorations)
  (fset 'ido-completions 'ido-vertical-or-horizontal-completions)

  (add-hook 'ido-minibuffer-setup-hook 'ido-vertical-disable-line-truncation)
  (add-hook 'ido-setup-hook 'ido-vertical-define-keys))

(defun turn-off-ido-vertical ()
  (setq ido-decorations ido-vertical-old-decorations)
  (fset 'ido-completions ido-vertical-old-completions)

  (remove-hook 'ido-minibuffer-setup-hook 'ido-vertical-disable-line-truncation)
  (remove-hook 'ido-setup-hook 'ido-vertical-define-keys))

(defun ido-vertical-next-match ()
  "Call the correct next-match function for right key.

This is based on:
- Different functions for completing directories and prior history.
"
  (interactive)
  (cond
   ((and (boundp 'item) item (eq item 'file))
    (ido-next-match-dir))
   (t
    (next-history-element 1))))

(defun ido-vertical-prev-match ()
  "Call the correct prev-match function for left key.

This is based on:
- Different functions for completing directories and prior history.
"
  (interactive)
  (cond
   ((and (boundp 'item) item (eq item 'file))
    (ido-prev-match-dir))
   (t
    (previous-history-element 1))))

(defun ido-vertical-define-keys () ;; C-n/p is more intuitive in vertical layout
  (when ido-vertical-define-keys
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
    (define-key ido-completion-map (kbd "C-c C-t") 'ido-toggle-prefix))
  (when (memq ido-vertical-define-keys '(C-n-C-p-up-and-down C-n-C-p-up-down-left-right))
    (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
    (define-key ido-completion-map (kbd "<down>") 'ido-next-match))
  (when (eq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
    (define-key ido-completion-map (kbd "<left>") 'ido-vertical-prev-match)
    (define-key ido-completion-map (kbd "<right>") 'ido-vertical-next-match)))

;;;###autoload
(define-minor-mode ido-vertical-mode
  "Makes ido-mode display vertically."
  :global t
  :group 'ido-vertical-mode
  (if ido-vertical-mode
      (turn-on-ido-vertical)
    (turn-off-ido-vertical)))

(provide 'ido-vertical-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ido-vertical-mode.el ends here
