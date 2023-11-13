;;; org-fragtog.el --- Auto-toggle Org LaTeX fragments -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Benjamin Levy - MIT/X11 License
;; Author: Benjamin Levy <blevy@protonmail.com>
;; Version: 0.4.2
;; Description: Automatically toggle Org mode LaTeX fragment previews as the cursor enters and exits them
;; Homepage: https://github.com/io12/org-fragtog
;; Package-Requires: ((emacs "27.1"))

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This package automates toggling Org mode LaTeX fragment
;; previews.  Fragment previews are disabled for editing when
;; your cursor steps onto them, and re-enabled when the cursor
;; leaves.

;;; Code:

(require 'org)
(require 'org-element)

(defgroup org-fragtog nil
  "Auto-toggle Org LaTeX fragments."
  :group 'org-latex)

(defcustom org-fragtog-ignore-predicates nil
  "List of predicates to determine whether to ignore a fragment.
For example, adding `org-at-table-p' will ignore fragments inside tables."
  :group 'org-fragtog
  :type 'hook
  :options '(org-at-table-p
             org-at-table.el-p
             org-at-block-p
             org-at-heading-p))

(defcustom org-fragtog-preview-delay 0.0
  "Seconds of delay before LaTeX preview."
  :group 'org-fragtog
  :type 'number)

;;;###autoload
(define-minor-mode org-fragtog-mode
  "A minor mode that automatically toggles Org mode LaTeX fragment previews.
Fragment previews are disabled for editing when your cursor steps onto them,
and re-enabled when the cursor leaves."
  :init-value nil

  ;; Fix nil error in `org-element-context'
  ;; when using `org-fragtog' without Org mode.
  (setq org-complex-heading-regexp (or org-complex-heading-regexp ""))

  (if org-fragtog-mode
      (add-hook 'post-command-hook #'org-fragtog--post-cmd nil t)
    (remove-hook 'post-command-hook #'org-fragtog--post-cmd t)))

(defvar-local org-fragtog--prev-frag nil
  "Previous fragment that surrounded the cursor.
If the cursor was not on a fragment, this variable is nil.  This is used to
track when the cursor leaves a fragment.")

(defvar-local org-fragtog--prev-point nil
  "Value of point from before the most recent command.")

(defvar-local org-fragtog--timer nil
  "Current active timer.")

(defun org-fragtog--post-cmd ()
  "This function is executed by `post-command-hook' in `org-fragtog-mode'.
It handles toggling fragments depending on whether the cursor entered or exited
them."
  (let*
      ;; Previous fragment
      ((prev-frag (or org-fragtog--prev-frag
                      ;; If there is no previous fragment,
                      ;; try to use a fragment at the previous cursor position.
                      ;; This case matters when the cursor constructs a fragment
                      ;; without ever being inside of it while it's constructed.
                      ;; For example, if the user types "$foo$" in sequential order,
                      ;; entering the final "$" creates a fragment without
                      ;; the cursor ever being inside of it.
                      (if org-fragtog--prev-point
                          (save-excursion
                            (goto-char org-fragtog--prev-point)
                            (org-fragtog--cursor-frag)))))
       ;; Previous fragment's start position
       (prev-frag-start-pos (org-fragtog--frag-start prev-frag))
       ;; Current fragment
       (cursor-frag (org-fragtog--cursor-frag))
       ;; The current fragment didn't change
       (frag-same (equal
                   ;; Fragments are considered the same if they have the same
                   ;; start position
                   (org-fragtog--frag-start cursor-frag)
                   prev-frag-start-pos))
       ;; The current fragment changed
       (frag-changed (not frag-same))
       ;; The fragment at position of previous fragment.
       ;; This can be nil when for example $foo$ is edited to become $foo $.
       (frag-at-prev-pos (and prev-frag-start-pos
                              (save-excursion
                                (goto-char prev-frag-start-pos)
                                (org-fragtog--cursor-frag)))))

    ;; Only do anything if the current fragment changed
    (when frag-changed
      ;; Current fragment is the new previous
      (setq org-fragtog--prev-frag cursor-frag)
      ;; Enable fragment if cursor left it after a timed disable
      ;; and the fragment still exists
      (when (and frag-at-prev-pos
                 (not (org-fragtog--frag-enabled frag-at-prev-pos)))
        (org-fragtog--enable-frag frag-at-prev-pos))
      ;; Cancel and expire timer
      (when org-fragtog--timer
        (cancel-timer org-fragtog--timer)
        (setq org-fragtog--timer nil))
      ;; Disable fragment if cursor entered it
      (when cursor-frag
        (if (> org-fragtog-preview-delay 0)
            (setq org-fragtog--timer (run-with-idle-timer org-fragtog-preview-delay
                                                          nil
                                                          #'org-fragtog--disable-frag
                                                          cursor-frag
                                                          t))
          (org-fragtog--disable-frag cursor-frag))))

    (setq org-fragtog--prev-point (point))))

(defun org-fragtog--frag-enabled (frag)
  "Return whether FRAG is enabled.
A fragment is enabled when it has a preview image overlay in the buffer."
  (org-fragtog--overlay-in-p (org-fragtog--frag-start frag)
                             (org-fragtog--frag-end frag)))

(defun org-fragtog--overlay-in-p (start-pos end-pos)
  "Return whether there is a fragment overlay between START-POS and END-POS."
  (seq-find (lambda (overlay)
              (equal (overlay-get overlay 'org-overlay-type)
                     'org-latex-overlay))
            (overlays-in start-pos end-pos)))

(defun org-fragtog--frag-start (frag)
  "Return the position of the beginning of FRAG."
  (org-element-property :begin frag))

(defun org-fragtog--frag-end (frag)
  "Return the position of the end of FRAG."
  ;; Normally org-mode considers whitespace after an element as part of the
  ;; element.  Avoid this behavior and consider trailing whitespace as outside
  ;; the fragment.
  (- (org-element-property :end frag)
     (org-element-property :post-blank frag)))

(defun org-fragtog--cursor-frag ()
  "Return the fragment currently surrounding the cursor.
If there is none, return nil.
If the fragment is ignored from rules in `org-fragtog-ignore-predicates',
return nil."
  (let*
      ;; Element surrounding the cursor
      ((elem (org-element-context))
       ;; A LaTeX fragment or environment is surrounding the cursor
       (elem-is-latex (and (member (org-element-type elem)
                                   '(latex-fragment latex-environment))
                           (< (point) (org-fragtog--frag-end elem))))
       ;; Whether the fragment should be ignored
       (should-ignore (run-hook-with-args-until-success
                       'org-fragtog-ignore-predicates)))

    (if (and elem-is-latex (not should-ignore))
        elem
      nil)))

(defun org-fragtog--enable-frag (frag)
  "Enable the Org LaTeX fragment preview for the fragment FRAG."

  ;; The fragment must be disabled before `org-latex-preview', since
  ;; `org-latex-preview' only toggles, leaving no guarantee that it's enabled
  ;; afterwards.
  (save-excursion
    (org-fragtog--disable-frag frag))

  ;; Move to fragment and enable
  (save-excursion
    (goto-char (org-fragtog--frag-start frag))
    ;; Org's "\begin ... \end" style LaTeX fragments consider whitespace
    ;; before the fragment as part of the fragment.
    ;; Some users overload `org-latex-preview' to functions with similar functionality,
    ;; such as `math-preview-at-point' from the `math-preview' package on MELPA.
    ;; These alternatives might get confused if they're asked to enable a fragment at
    ;; point when point is on whitespace before the fragment.
    ;; So, advance to the nearest non-whitespace character before enabling.
    (re-search-forward "[^ \t]")
    (ignore-errors (org-latex-preview))))

(defun org-fragtog--set-point-after-disable-frag (frag)
  "Set point to where it should be after FRAG was disabled.
If point decreases and enters a fragment from the end, disabling it, then point
should move to the end of the fragment.  If point moved up one line, its column
should be maintained."
  (when (and ;; There has to be a prev-point.
         org-fragtog--prev-point
         ;; Only move to the end of the fragment if it's closer to the
         ;; prev-point location than the start of the fragment is.
         (< (abs (- org-fragtog--prev-point (org-fragtog--frag-end frag)))
            (abs (- org-fragtog--prev-point (org-fragtog--frag-start frag)))))
    (let ((prev-point-column (save-excursion
                               (goto-char org-fragtog--prev-point)
                               (current-column))))
      ;; Move to the line where the fragment ends while preserving the point
      ;; column.
      (goto-char (1- (org-fragtog--frag-end frag)))
      (when (/= (line-number-at-pos org-fragtog--prev-point)
                (line-number-at-pos))
        (move-to-column prev-point-column)))))

(defun org-fragtog--disable-frag (frag &optional renew)
  "Disable the Org LaTeX fragment preview for the fragment FRAG.
If RENEW is non-nil, renew the fragment at point."

  ;; Renew frag at point in case point was adjusted
  ;; See Emacs Lisp manual, 21.6 Adjusting Point After Commands
  (when renew
    (setq frag (org-fragtog--cursor-frag))
    (setq org-fragtog--prev-frag frag)
    (setq org-fragtog--timer nil))

  ;; There may be nothing at the adjusted point
  (when frag
    (org-clear-latex-preview (org-fragtog--frag-start frag)
                             (org-fragtog--frag-end frag))
    (org-fragtog--set-point-after-disable-frag frag)))

(provide 'org-fragtog)

;;; org-fragtog.el ends here
