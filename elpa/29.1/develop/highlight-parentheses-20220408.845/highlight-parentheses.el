;;; highlight-parentheses.el --- Highlight surrounding parentheses  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2007, 2009, 2013 Nikolaj Schumacher
;; Copyright (C) 2018 Tim Perkins
;; Copyright (C) 2018-2022 Tassilo Horn
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Maintainer: Tassilo Horn <tsdh@gnu.org>
;; Version: 2.1.1
;; Keywords: faces, matching
;; URL: https://sr.ht/~tsdh/highlight-parentheses.el/
;; Package-Requires: ((emacs "24.3"))
;; Compatibility: GNU Emacs 24.3, 25.x, 26.x, 27.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Add the following to your .emacs file:
;;
;;     (require 'highlight-parentheses)
;;
;; Enable the mode using `M-x highlight-parentheses-mode' or by adding it to a
;; hook such as `prog-mode-hook'.  You can also enable
;; `global-highlight-parentheses-mode' which enables it in all buffers
;; automatically.  Additionally, you can
;;
;;     (add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup)
;;
;; in order to enable it also in the minibuffer, e.g., when input is read from
;; the minibuffer.
;;
;; The look of the highlighted parens can be customized using these options:
;;
;; - `highlight-parentheses-colors'
;; - `highlight-parentheses-background-colors'
;; - `highlight-parentheses-attributes'
;;
;; If you set them from Lisp rather than through the customize interface, you
;; may have to call `highlight-parentheses--color-update' in order to have the
;; changes affect already opened buffers.
;;
;; You can also customize the `highlight-parentheses-highlight' face which acts
;; as a blanket face on top of which the colors and attributes defined by the
;; three customize options listed above are applied.

;;; Code:

(require 'cl-lib)

(defgroup highlight-parentheses nil
  "Highlight surrounding parentheses."
  :group 'faces
  :group 'matching)


;;; Custom Variables

(define-obsolete-function-alias
  'hl-paren-set 'highlight-parentheses--set "2.0.0")
(defun highlight-parentheses--set (variable value)
  "Set VARIABLE to a new VALUE and update highlighted parens in all buffers.

This function is used so that appropriate custom variables apply
immediately once set (through the custom interface)."
  (set variable value)
  ;; REVIEW: I assume this check is here for cases that are too early into the
  ;; load process? Exactly! ;-)
  (when (fboundp 'highlight-parentheses--color-update)
    (highlight-parentheses--color-update)))

(define-obsolete-variable-alias 'hl-paren-colors
  'highlight-parentheses-colors "2.0.0")
(defcustom highlight-parentheses-colors
  '("firebrick1" "IndianRed1" "IndianRed3" "IndianRed4")
  "List of colors for the highlighted parentheses.
The list starts with the inside parentheses and moves outwards.
May also be a function returning a list of colors."
  :type '(choice (repeat color) function)
  :set #'highlight-parentheses--set
  :group 'highlight-parentheses)

(define-obsolete-variable-alias 'hl-paren-background-colors
  'highlight-parentheses-background-colors "2.0.0")
(defcustom highlight-parentheses-background-colors nil
  "List of colors for the background highlighted parentheses.
The list starts with the inside parentheses and moves outwards.
May also be a function returning a list of colors."
  :type '(choice (repeat color) function)
  :set #'highlight-parentheses--set
  :group 'highlight-parentheses)

(define-obsolete-variable-alias 'hl-paren-attributes
  'highlight-parentheses-attributes "2.0.0")
(defcustom highlight-parentheses-attributes nil
  "List of face attributes for the highlighted parentheses.
Each element is a plist of face attributes.
The list starts with the inside parentheses and moves outwards.
May also be a function returning a list of plists."
  :type '(choice (repeat plist) function)
  :set #'highlight-parentheses--set
  :group 'highlight-parentheses)

(define-obsolete-variable-alias 'hl-paren-highlight-adjacent
  'highlight-parentheses-highlight-adjacent "2.0.0")
(defcustom highlight-parentheses-highlight-adjacent nil
  "Highlight adjacent parentheses, just like Show Paren mode."
  :type '(boolean)
  :set #'highlight-parentheses--set
  :group 'highlight-parentheses)

(define-obsolete-variable-alias 'hl-paren-delay
  'highlight-parentheses-delay "2.0.0")
(defcustom highlight-parentheses-delay 0.137
  "Fraction of seconds after which the overlays are adjusted.
In general, this should at least be larger than your keyboard
repeat rate in order to prevent excessive movements of the
overlays when scrolling or moving point by pressing and holding
\\[next-line], \\[scroll-up-command] and friends."
  :type 'number
  :group 'highlight-parentheses)


;; Custom Faces

(define-obsolete-face-alias 'hl-paren-face
  'highlight-parentheses-highlight "2.0.0")
(defface highlight-parentheses-highlight nil
  "Face used for highlighting parentheses.
Color attributes might be overriden by `highlight-parentheses-colors' and
`highlight-parentheses-background-colors'."
  :group 'highlight-parentheses)


;;; Internal Variables

(defvar-local highlight-parentheses--overlays nil
  "This buffers currently active overlays.")

(defvar-local highlight-parentheses--last-point 0
  "The last point for which parentheses were highlighted.
This is used to prevent analyzing the same context over and over.")

(defvar-local highlight-parentheses--timer nil
  "A timer initiating the movement of the `highlight-parentheses--overlays'.")


;;; Internal Functions
(define-obsolete-function-alias 'hl-paren-delete-overlays
  'highlight-parentheses--delete-overlays "2.0.0")

(cl-defun highlight-parentheses--delete-overlays
    (&optional (overlays highlight-parentheses--overlays))
  "Delete all overlays set by Highlight Parentheses in the current buffer.

If the optional argument OVERLAYS (a list) is non-nil, delete all
overlays in it instead."
  (mapc #'delete-overlay overlays))

(define-obsolete-function-alias 'hl-paren-highlight
  'highlight-parentheses--highlight "2.0.0")
(defun highlight-parentheses--highlight ()
  "Highlight the parentheses around point."
  (unless (= (point) highlight-parentheses--last-point)
    (setq highlight-parentheses--last-point (point))
    (let ((overlays highlight-parentheses--overlays)
          pos1 pos2)
      (save-excursion
        (ignore-errors
          (when highlight-parentheses-highlight-adjacent
            (cond ((memq (preceding-char) '(?\) ?\} ?\] ?\>))
                   (backward-char 1))
                  ((memq (following-char) '(?\( ?\{ ?\[ ?\<))
                   (forward-char 1))))
          (while (and (setq pos1 (cadr (syntax-ppss pos1)))
                      (cdr overlays))
            (move-overlay (pop overlays) pos1 (1+ pos1))
            (when (setq pos2 (scan-sexps pos1 1))
              (move-overlay (pop overlays) (1- pos2) pos2)))))
      (highlight-parentheses--delete-overlays overlays))))

(define-obsolete-function-alias 'hl-paren-initiate-highlight
  'highlight-parentheses--initiate-highlight "2.0.0")
(defun highlight-parentheses--initiate-highlight ()
  "Move the overlays after a short delay.
The overlays are specified by `highlight-parentheses--overlays',
the delay by `highlight-parentheses-delay' seconds."
  (when highlight-parentheses--timer
    (cancel-timer highlight-parentheses--timer))
  (setq highlight-parentheses--timer
        (run-at-time highlight-parentheses-delay nil
                     #'highlight-parentheses--highlight)))


;;; Overlays

(defvar highlight-parentheses--face-property
  'font-lock-face
  "Face property to be used for fontification.
This is `font-lock-face' in all normal buffers and `face' for
minibuffer.")

(defun highlight-parentheses--create-overlays ()
  "Initialize `highlight-parentheses--overlays' buffer-locally."
  (let ((fg (if (functionp highlight-parentheses-colors)
                (funcall highlight-parentheses-colors)
              highlight-parentheses-colors))
        (bg (if (functionp highlight-parentheses-background-colors)
                (funcall highlight-parentheses-background-colors)
              highlight-parentheses-background-colors))
        (attr (if (functionp highlight-parentheses-attributes)
                  (funcall highlight-parentheses-attributes)
                highlight-parentheses-attributes))
        attributes)
    ;; If no colors, bg-colors, or attrs are set, then only the effect of
    ;; highlight-parentheses-highlight should be applied.  In that case, fontify
    ;; up to 8 nesting levels with the specification of that face.
    (when (and (null fg) (null bg) (null attr))
      (setq fg (make-list 8 nil)))
    (while (or fg bg attr)
      (setq attributes (face-attr-construct 'highlight-parentheses-highlight))
      (let ((car-fg (car fg))
            (car-bg (car bg))
            (car-attr (car attr)))
        (cl-loop for (key . (val . _rest)) on car-attr by #'cddr
                 do (setq attributes
                          (plist-put attributes key val)))
        (when car-fg
          (setq attributes (plist-put attributes :foreground car-fg)))
        (when car-bg
          (setq attributes (plist-put attributes :background car-bg))))
      (pop fg)
      (pop bg)
      (pop attr)
      (dotimes (_i 2) ;; front and back
        (push (make-overlay 0 0 nil t) highlight-parentheses--overlays)
        ;; Add a 'highlight-parentheses property just that we can easily
        ;; identify "our" overlay with `C-u C-x =' and friends.
        (overlay-put (car highlight-parentheses--overlays)
                     'highlight-parentheses t)
        (overlay-put (car highlight-parentheses--overlays)
                     highlight-parentheses--face-property attributes)))
    (setq highlight-parentheses--overlays
          (nreverse highlight-parentheses--overlays))))

(defun highlight-parentheses--color-update ()
  "Force-update highlighted parentheses in all buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when highlight-parentheses--overlays
        (highlight-parentheses--delete-overlays)
        (setq highlight-parentheses--overlays nil)
        (highlight-parentheses--create-overlays)
        (let ((highlight-parentheses--last-point -1)) ;; force update
          (highlight-parentheses--highlight))))))


;;; Mode Functions
;;;###autoload
(define-minor-mode highlight-parentheses-mode
  "Minor mode to highlight the surrounding parentheses."
  :lighter " hl-p"
  (highlight-parentheses--delete-overlays)
  (kill-local-variable 'highlight-parentheses--overlays)
  (kill-local-variable 'highlight-parentheses--last-point)
  (remove-hook 'post-command-hook
               #'highlight-parentheses--initiate-highlight t)
  (remove-hook 'before-revert-hook
               #'highlight-parentheses--delete-overlays)
  (remove-hook 'change-major-mode-hook
               #'highlight-parentheses--delete-overlays)
  (when (and highlight-parentheses-mode
             ;; Don't enable in *Messages* buffer.
             ;; https://github.com/tsdh/highlight-parentheses.el/issues/14
             (not (eq major-mode 'messages-buffer-mode))
             (not (string= (buffer-name) "*Messages*")))
    (highlight-parentheses--create-overlays)
    (add-hook 'post-command-hook
              #'highlight-parentheses--initiate-highlight nil t)
    (add-hook 'before-revert-hook
              #'highlight-parentheses--delete-overlays)
    (add-hook 'change-major-mode-hook
              #'highlight-parentheses--delete-overlays)))

(defun highlight-parentheses-minibuffer-setup ()
  "Setup `highlight-parentheses-mode' in the minibuffer.
This function is meant to be added to `minibuffer-setup-hook' in
order to highlight parentheses also in the minibuffer, e.g., in
the input given at the `eval-expression' prompt (`M-:')."
  (setq-local highlight-parentheses--face-property 'face)
  (font-lock-mode)
  (highlight-parentheses-mode))

;;;###autoload
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda () (highlight-parentheses-mode 1)))

(provide 'highlight-parentheses)

;;; highlight-parentheses.el ends here
