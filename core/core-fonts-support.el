;;; core-fonts-support.el --- Spacemacs Core File -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'core-funcs)
(require 'core-load-paths)

(require 'core-spacemacs-buffer)

(defvar spacemacs--diminished-minor-modes nil
  "List of diminished modes to unicode or ascii values.")

(defun spacemacs/set-default-font (plists)
  "Set the font given the passed PLISTS.

PLISTS has either the form (\"fontname\" :prop1 val1 :prop2 val2 ...)
or is a list of such. The first font that can be found will be used.

The return value is nil if no font was found, non-nil otherwise."
  (unless (listp (car plists))
    (setq plists (list plists)))
  (catch 'success
    (dolist (plist plists)
      (when (find-font (font-spec :name (car plist)))
        (let* ((font (car plist))
               (props (cdr plist))
               (font-props (spacemacs/mplist-remove
                            ;; although this keyword does not exist anymore
                            ;; we keep it for backward compatibility
                            (spacemacs/mplist-remove props :powerline-scale)
                            :powerline-offset))
               (fontspec (apply 'font-spec :name font font-props)))
          (spacemacs-buffer/message "Setting font \"%s\"..." font)
          (set-face-attribute 'default nil :font fontspec)

          ;; Make sure that our font is used for fixed-pitch face as well
          (set-face-attribute 'fixed-pitch nil :family 'unspecified)

          ;; fallback font for unicode characters used in spacemacs
          (cl-destructuring-bind (fallback-font-name fallback-font-name2)
              (cl-case system-type
                ((gnu/linux android) '("NanumGothic"      "NanumGothic"))
                ((darwin)            '("Arial Unicode MS" "Arial Unicode MS"))
                ((windows-nt cygwin) '("MS Gothic"        "Lucida Sans Unicode"))
                (t nil))
            (when (and fallback-font-name fallback-font-name2)
              ;; remove any size or height properties in order to be able to
              ;; scale the fallback fonts with the default one (for zoom-in/out
              ;; for instance)
              (let* ((fallback-props (spacemacs/mplist-remove
                                      (spacemacs/mplist-remove font-props :size)
                                      :height))
                     (fallback-spec (apply 'font-spec
                                           :name fallback-font-name
                                           fallback-props))
                     (fallback-spec2 (apply 'font-spec
                                            :name fallback-font-name2
                                            fallback-props)))
                ;; Active fontset might be `fontset-default', `fontset-startup',
                ;; or something else.
                (let ((active-fontset (face-attribute 'default :fontset)))
                  ;; window numbers (ding bang circled digits)
                  (set-fontset-font active-fontset
                                    '(#x2776 . #x2793) fallback-spec nil 'prepend)
                  ;; mode-line circled letters (circled latin capital/small letters)
                  (set-fontset-font active-fontset
                                    '(#x24b6 . #x24e9) fallback-spec nil 'prepend)
                  ;; mode-line additional characters (circled/squared mathematical operators)
                  (set-fontset-font active-fontset
                                    '(#x2295 . #x22a1) fallback-spec nil 'prepend)
                  ;; new version lighter (arrow block)
                  (set-fontset-font active-fontset
                                    '(#x2190 . #x21ff) fallback-spec2 nil 'prepend))))))
        (throw 'success t)))
    nil))

(defun spacemacs//set-default-font-from-dotfile ()
  "Set the `default' face based on `dotspacemacs-default-font'."
  (spacemacs|do-after-display-system-init
    (unless (spacemacs/set-default-font dotspacemacs-default-font)
      (spacemacs-buffer/warning
       "Cannot find any of the specified fonts (%s)! Font settings may not be correct."
       (if (listp (car dotspacemacs-default-font))
           (mapconcat 'car dotspacemacs-default-font ", ")
         (car dotspacemacs-default-font))))))

(defun spacemacs/compute-mode-line-height ()
  "Return an adjusted mode-line height."
  (let ((scale (if (and (boundp 'powerline-scale) powerline-scale)
                   powerline-scale 1)))
    (truncate (* scale (frame-char-height)))))

(defmacro spacemacs|diminish (mode &optional unicode ascii)
  "Diminish MODE name in mode line to UNICODE or ASCII depending on the value
`dotspacemacs-mode-line-unicode-symbols'.
If ASCII is not provided then UNICODE is used instead. If neither are provided,
the mode will not show in the mode line."
  (when (and unicode
             (not (display-graphic-p)) ; terminal
             ;; the new indicator is 3 chars (including the space), ex: " Ⓔh"
             (= (length unicode) 3))
    (setq unicode (spacemacs/terminal-fix-mode-line-indicator-overlap unicode)))
  `(let ((cell (assq ',mode spacemacs--diminished-minor-modes)))
     (if cell
         (setcdr cell '(,unicode ,ascii))
       (push '(,mode ,unicode ,ascii) spacemacs--diminished-minor-modes))))

(defun spacemacs/diminish-undo (mode)
  "Restore the diminished lighter."
  (interactive
   (list (read (completing-read
                "Restore what diminished mode: "
                (cons (list "diminished-modes")
                      (mapcar (lambda (x) (list (symbol-name (car x))))
                              diminished-mode-alist))
                nil t nil 'diminish-history-symbols))))
  ;; remove the `mode' entry from spacemacs own list
  (setq spacemacs--diminished-minor-modes
        (delq nil (mapcar (lambda (x) (unless (eq (car x) mode) x))
                          spacemacs--diminished-minor-modes)))
  (diminish-undo mode))

(defmacro spacemacs|hide-lighter (mode)
  "Diminish MODE name in mode line to LIGHTER."
  `(eval-after-load 'diminish '(diminish ',mode)))

(provide 'core-fonts-support)
