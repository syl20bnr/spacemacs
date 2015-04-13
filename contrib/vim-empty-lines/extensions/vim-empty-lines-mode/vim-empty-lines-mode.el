;;; vim-empty-lines-mode.el --- Vim-like empty line indicator at end of files.

;; Copyright (C) 2015 Jonne Mickelin

;; Author: Jonne Mickelin <jonne@ljhms.com>
;; Created: 06 Jan 2015
;; Version: 0.1
;; Keywords: emulations
;; URL: https://github.com/jmickelin/vim-empty-lines-mode
;; Package-Requires: ((emacs "23"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This mode emulates the way that vim indicates the end of a file,
;; that is by putting a "tilde" character (~) on any empty line that
;; follows the end of file.

;; Emacs provides a similar functionality that inserts a bitmap in the
;; fringe on the extraneous lines. By customizing
;; `indicate-empty-lines' and `fringe-indicator-alist' it is possible
;; to nearly emulate the vim behaviour.

;; However, there is a slight difference in what the two editors
;; consider to be "empty lines".

;; A line in vim can be considered to contain the newline character
;; that ended the previous line, as well as the other visible
;; characters.  Equivalently, a line is empty only if it contains no
;; text and the previous line has no trailing whitespace.

;; Example:

;;    foo     <- not empty
;;    \nbar   <- not empty
;;    \n      <- not empty
;;    ~       <- empty
;;    ~       <- empty
;;
;;    foo     <- not empty
;;    \nbar   <- not empty
;;    ~       <- empty
;;    ~       <- empty

;; A line in emacs, on the other hand, will contain the newline
;; character that breaks it. Thus a line is empty even if the previous
;; line has a trailing linebreak.

;; Example:

;;    foo\n    <- not empty
;;    bar\n    <- not empty
;;    ~        <- empty
;;    ~        <- empty
;;                                        ;
;;    foo\n    <- not empty
;;    bar      <- not empty
;;    ~        <- empty
;;    ~        <- empty

;; Note that Emacs displays the two cases identically!

;; There is currently (as of Emacs 24.4) no way to implement the
;; vim-like behaviour for `indicate-empty-lines' short of modifying
;; the Emacs core.

;; This module emulates the vim-like behaviour using a different
;; approach, namely by inserting at the end of the buffer a read-only
;; overlay containing the indicators for the empty lines. This has the
;; added advantage that it's trivial to customize the indicator to an
;; arbitrary string, and customize its text properties.

;; To enable `vim-empty-lines-mode' in a buffer, run
;;    (vim-empty-lines-mode)

;; To enable it globally, run
;;    (global-vim-empty-lines-mode)

;; The string that indicates an empty line can be customized, e.g.
;;    (setq vim-empty-lines-indicator "**********************")

;; The face that is used to display the indicators is `vim-empty-lines-face'.

;;; Code:

(defgroup vim-empty-lines-mode
  nil
  "Vim-like empty line indicators."
  :group 'emulations
  :prefix "vim-empty-lines")

(defface vim-empty-lines-face
  '((t (:inherit font-lock-comment-face)))
  "Face for empty lines in `vim-empty-lines-mode'."
  :group 'vim-empty-lines-mode)

(defcustom vim-empty-lines-indicator "~"
  "String to display on lines following end-of-buffer in `vim-empty-lines-mode'.

Must not contain '\\n'."
  :group 'vim-empty-lines-mode)

(defvar vim-empty-lines-overlay nil
  "Overlay that displays the empty line indicators.")
(put 'vim-empty-lines-overlay 'permanent-local t)

(defun vim-empty-lines-create-overlay ()
  (setq-local vim-empty-lines-overlay (make-overlay (point-max)
                                                    (point-max)
                                                    nil
                                                    t t))
  (overlay-put vim-empty-lines-overlay 'window t))

(defun vim-empty-lines-count-screen-lines (beg end &optional max)
  "Return the number of screen lines in the region.

Taken from `count-screen-lines' and quite stripped down.
Unlike `count-screen-lines', calls `vertical-motion' with MAX as the
argument for efficiencies. It is too expensive calling `vertical-motion'
with `buffer-size' if the buffer is large."
  (let (count-final-newline
        window)
    (if (= beg end)
        0
      (save-excursion
        (save-restriction
          (widen)
          (narrow-to-region (min beg end)
                            (if (and (not count-final-newline)
                                     (= ?\n (char-before (max beg end))))
                                (1- (max beg end))
                              (max beg end)))
          (goto-char (point-min))
          (1+ (vertical-motion (or max (buffer-size)) ; XXX: changed
                               window)))))))

(defun vim-empty-lines-nlines-after-buffer-end (window &optional window-start)
  (with-current-buffer (window-buffer window)
    (if (and window-start ;; chance to optimize for some cases.
             (or (and (window-end) (= (point-max) (window-end)))
                 (not (pos-visible-in-window-p (point-max) window))))
        0
      (vim-empty-lines-nlines-after-buffer-end-aux window))))

(defun vim-empty-lines-nlines-after-buffer-end-aux (window)
  (save-selected-window
    (with-selected-window window
      (with-current-buffer (window-buffer)
        (let ((screen-height (- (window-height)
                                1 ;; mode-line
                                (if header-line-format 1 0))))
          (- screen-height
             (1- (vim-empty-lines-count-screen-lines
                  (window-start) (point-max) screen-height))))))))

(defun vim-empty-lines-update-overlay (&optional window window-start)
  (let ((w (or window
               (let ((w (selected-window)))
                 (and (window-valid-p w) w)))))
    ;; `w' could be nil but it's ok for `window-height', `window-start' etc.
    (when (overlayp vim-empty-lines-overlay)
      (vim-empty-lines-update-overlay-aux
       (apply 'max
              (vim-empty-lines-nlines-after-buffer-end w window-start)
              (mapcar 'vim-empty-lines-nlines-after-buffer-end
                      (remq w (get-buffer-window-list nil nil t)))))
      (move-overlay vim-empty-lines-overlay (point-max) (point-max)))))

(defun vim-empty-lines-update-overlay-aux (nlines-after-buffer-end)
  (when (> nlines-after-buffer-end 1)
    (save-excursion
      (let ((indicators
             (apply 'concat
                    (make-list nlines-after-buffer-end
                               (concat "\n" vim-empty-lines-indicator)))))
        (overlay-put vim-empty-lines-overlay
                     'after-string
                     (concat (propertize " "
                                         ;; Forbid movement past
                                         ;; the beginning of the
                                         ;; after-string.
                                         'cursor nlines-after-buffer-end)
                             (propertize indicators
                                         'face 'vim-empty-lines-face)))))))

(defun vim-empty-lines-update-overlay-windows ()
  (with-selected-frame (selected-frame)
    (save-selected-window
      (mapc (lambda (w)
              (with-selected-window w
                (vim-empty-lines-update-overlay w)))
            (window-list (selected-frame) -1)))))

(defun vim-empty-lines-hide-overlay ()
  (when (overlayp vim-empty-lines-overlay)
    (let ((ov vim-empty-lines-overlay))
      (overlay-put ov 'invisible nil)
      (overlay-put ov 'display nil)
      (overlay-put ov 'after-string nil))))

(defvar vim-empty-lines-initialize-p nil)

(defun vim-empty-lines-initialize-maybe ()
  "Setup some advices to emacs primitives for workarounds"
  (unless vim-empty-lines-initialize-p
    (setq vim-empty-lines-initialize-p t)

    ;; A kludge to bug#19553
    ;; fixed in b544ab561fcb575790c963a2eda51524fa366409
    ;; XXX: The fix is in the emacs-24 branch only at this time.
    (unless (and (version< emacs-version "25")
                 (version< "24.4.51" emacs-version))
      (eval-when-compile
        (defmacro vim-empty-lines-advice-add-overlay-handling (&rest functions)
          `(progn
             ,@(mapcar
                (lambda (function)
                  `(defadvice ,function (around vim-empty-lines activate)
                     (if (not (overlayp vim-empty-lines-overlay))
                         ad-do-it
                       (let ((inhibit-redisplay t) ;;Hope not to break anything
                             (p (overlay-start vim-empty-lines-overlay)))
                         (delete-overlay vim-empty-lines-overlay)
                         (unwind-protect ad-do-it
                           (move-overlay vim-empty-lines-overlay p p))))))
                functions))))

      (vim-empty-lines-advice-add-overlay-handling vertical-motion
                                                   move-to-window-line))))

;;;###autoload
(define-minor-mode vim-empty-lines-mode
  "Display `vim-empty-lines-indicator' on visible lines after the end of the buffer.

This differs from `indicate-empty-lines' in the way that it deals
with trailing newlines."
  :lighter " ~"
  :global nil
  (if vim-empty-lines-mode
      (progn
        (unless (overlayp vim-empty-lines-overlay)
          (vim-empty-lines-create-overlay))
        (vim-empty-lines-initialize-maybe)
        (make-local-variable 'vim-empty-lines-overlay)
        (vim-empty-lines-create-overlay)
        (vim-empty-lines-update-overlay)
        (add-hook 'post-command-hook 'vim-empty-lines-update-overlay t)
        (add-hook 'window-scroll-functions 'vim-empty-lines-update-overlay t)
        (add-hook 'window-configuration-change-hook
                  'vim-empty-lines-update-overlay-windows t))
    (remove-hook 'post-command-hook 'vim-empty-lines-update-overlay t)
    (remove-hook 'window-scroll-functions 'vim-empty-lines-update-overlay t)
    (remove-hook 'window-configuration-change-hook
                 'vim-empty-lines-update-overlay-windows t)
    (when (overlayp vim-empty-lines-overlay)
      (delete-overlay vim-empty-lines-overlay)
      (setq vim-empty-lines-overlay nil))))

;;;###autoload
(define-global-minor-mode global-vim-empty-lines-mode
  vim-empty-lines-mode
  (lambda ()
    (unless (or (minibufferp)
                ;; Is there really no built-in function for detecting
                ;; the echo area?
                (string-match-p "\\*Echo Area [0-9]+\\*" (buffer-name)))
      (vim-empty-lines-mode +1)))
  :group 'vim-empty-lines-mode
  :require 'vim-empty-lines-mode)

(provide 'vim-empty-lines-mode)

;;; vim-empty-lines-mode.el ends here
