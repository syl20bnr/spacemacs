;;; core-fonts-support.el --- Spacemacs Core File -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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

The return value is nil if no font was found, truthy otherwise."
  (unless (listp (car plists))
    (setq plists (list plists)))
  (catch 'break
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
          ;; We set the INHIBIT-CUSTOMIZE parameter to t to tell set-frame-font
          ;; not to fiddle with the default face in the user's Customization
          ;; settings. We don't need Customization because our way of ensuring
          ;; that the font is applied to future frames is to modify
          ;; default-frame-alist, and Customization causes issues, see
          ;; https://github.com/syl20bnr/spacemacs/issues/5353.
          ;; INHIBIT-CUSTOMIZE is only present in recent emacs versions.
          (if (version< emacs-version "28.0.90")
              (set-frame-font fontspec nil t)
            (set-frame-font fontspec nil t t))
          (push `(font . ,(frame-parameter nil 'font)) default-frame-alist)

          ;; Make sure that our font is used for fixed-pitch face as well
          (set-face-attribute 'fixed-pitch nil :family 'unspecified)

          ;; fallback font for unicode characters used in spacemacs
          (pcase system-type
            (`gnu/linux
             (setq fallback-font-name "NanumGothic")
             (setq fallback-font-name2 "NanumGothic"))
            (`android
             (setq fallback-font-name "NanumGothic")
             (setq fallback-font-name2 "NanumGothic"))
            (`darwin
             (setq fallback-font-name "Arial Unicode MS")
             (setq fallback-font-name2 "Arial Unicode MS"))
            (`windows-nt
             (setq fallback-font-name "MS Gothic")
             (setq fallback-font-name2 "Lucida Sans Unicode"))
            (`cygwin
             (setq fallback-font-name "MS Gothic")
             (setq fallback-font-name2 "Lucida Sans Unicode"))
            (other
             (setq fallback-font-name nil)
             (setq fallback-font-name2 nil)))
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
              ;; window numbers (ding bang circled digits)
              (set-fontset-font "fontset-default"
                                '(#x2776 . #x2793) fallback-spec nil 'prepend)
              ;; mode-line circled letters (circled latin capital/small letters)
              (set-fontset-font "fontset-default"
                                '(#x24b6 . #x24e9) fallback-spec nil 'prepend)
              ;; mode-line additional characters (circled/squared mathematical operators)
              (set-fontset-font "fontset-default"
                                '(#x2295 . #x22a1) fallback-spec nil 'prepend)
              ;; new version lighter (arrow block)
              (set-fontset-font "fontset-default"
                                '(#x2190 . #x21ff) fallback-spec2 nil 'prepend))))
        (throw 'break t)))
    nil))

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
