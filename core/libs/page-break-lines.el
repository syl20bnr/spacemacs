;;; page-break-lines.el --- Display ugly ^L page breaks as tidy horizontal lines

;; Copyright (C) 2012-2015 Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/page-break-lines
;; Package-Version: 20160109.1813
;; Package-X-Original-Version: DEV
;; Keywords: convenience, faces

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

;; This library provides a global mode which displays form feed
;; characters as horizontal rules.

;; Install from Melpa or Marmalade, or add to `load-path' and use
;; (require 'page-break-lines).

;; Use `page-break-lines-mode' to enable the mode in specific buffers,
;; or customize `page-break-lines-modes' and enable the mode globally with
;; `global-page-break-lines-mode'.

;; Issues and limitations:

;; If `page-break-lines-char' is displayed at a different width to
;; regular characters, the rule may be either too short or too long:
;; rules may then wrap if `truncate-lines' is nil. On some systems,
;; Emacs may erroneously choose a different font for the page break
;; symbol, which choice can be overridden using code such as:

;; (set-fontset-font "fontset-default"
;;                   (cons page-break-lines-char page-break-lines-char)
;;                   (face-attribute 'default :family))

;; Use `describe-char' on a page break char to determine whether this
;; is the case.

;; Additionally, the use of `text-scale-increase' or
;; `text-scale-decrease' will cause the rule width to be incorrect,
;; because the reported window width (in characters) will continue to
;; be the width in the frame's default font, not the scaled font used to
;; display the rule.

;; Adapted from code http://www.emacswiki.org/emacs/PageBreaks

;;; Code:

(defgroup page-break-lines nil
  "Display ugly ^L page breaks as tidy horizontal lines."
  :prefix "page-break-lines-"
  :group 'faces)

(defcustom page-break-lines-char ?─
  "Character used to render page break lines."
  :type 'character
  :group 'page-break-lines)

(defcustom page-break-lines-lighter " PgLn"
  "Mode-line indicator for `page-break-lines-mode'."
  :type '(choice (const :tag "No lighter" "") string)
  :group 'page-break-lines)

(defcustom page-break-lines-modes
  '(emacs-lisp-mode lisp-mode scheme-mode compilation-mode outline-mode help-mode)
  "Modes in which to enable `page-break-lines-mode'."
  :type '(repeat symbol)
  :group 'page-break-lines)

(defface page-break-lines
  '((t :inherit font-lock-comment-face :bold nil :italic nil))
  "Face used to colorize page break lines.
If using :bold or :italic, please ensure `page-break-lines-char'
is available in that variant of your font, otherwise it may be
displayed as a junk character."
  :group 'page-break-lines)



;;;###autoload
(define-minor-mode page-break-lines-mode
  "Toggle Page Break Lines mode.

In Page Break mode, page breaks (^L characters) are displayed as a
horizontal line of `page-break-string-char' characters."
  :lighter page-break-lines-lighter
  :group 'page-break-lines
  (page-break-lines--update-display-tables))

;;;###autoload
(defun turn-on-page-break-lines-mode ()
  "Enable `page-break-lines-mode' in this buffer."
  (page-break-lines-mode 1))

;;;###autoload
(defun turn-off-page-break-lines-mode ()
  "Disable `page-break-lines-mode' in this buffer."
  (page-break-lines-mode -1))


(dolist (hook '(window-configuration-change-hook
                after-setting-font-hook))
  (add-hook hook 'page-break-lines--update-display-tables))



(defun page-break-lines--update-display-table (window)
  "Modify a display-table that displays page-breaks prettily.
If the buffer inside WINDOW has `page-break-lines-mode' enabled,
its display table will be modified as necessary."
  (with-current-buffer (window-buffer window)
    (if page-break-lines-mode
        (progn
          (unless buffer-display-table
            (setq buffer-display-table (make-display-table)))
          (let* ((width (- (window-width window) 1))
                 (glyph (make-glyph-code page-break-lines-char 'page-break-lines))
                 (new-display-entry (vconcat (make-list width glyph))))
            (unless (equal new-display-entry (elt buffer-display-table ?\^L))
              (aset buffer-display-table ?\^L new-display-entry))))
      (when buffer-display-table
        (aset buffer-display-table ?\^L nil)))))

(defun page-break-lines--update-display-tables  ()
  "Function called for updating display table."
  (mapc 'page-break-lines--update-display-table (window-list nil 'no-minibuffer)))



;;;###autoload
(defun page-break-lines-mode-maybe ()
  "Enable `page-break-lines-mode' in the current buffer if desired.
When `major-mode' is listed in `page-break-lines-modes', then
`page-break-lines-mode' will be enabled."
  (if (and (not (minibufferp))
           (apply 'derived-mode-p page-break-lines-modes))
      (page-break-lines-mode 1)))

;;;###autoload
(define-global-minor-mode global-page-break-lines-mode
  page-break-lines-mode page-break-lines-mode-maybe
  :group 'page-break-lines)


(provide 'page-break-lines)

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; checkdoc-minor-mode: t
;; End:

;;; page-break-lines.el ends here
