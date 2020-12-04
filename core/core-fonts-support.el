;;; core-fonts-support.el --- Space-macs Core File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3
(require 'core-funcs)
(require 'core-space-macs-buffer)

(defvar space-macs--diminished-minor-modes nil
  "List of diminished modes to unicode or ascii values.")

(defun space-macs/set-default-font (plists)
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
               (font-props (space-macs/mplist-remove
                            ;; although this keyword does not exist anymore
                            ;; we keep it for backward compatibility
                            (space-macs/mplist-remove props :powerline-scale)
                            :powerline-offset))
               (fontspec (apply 'font-spec :name font font-props)))
          (space-macs-buffer/message "Setting font \"%s\"..." font)
          (set-frame-font fontspec nil t)
          (push `(font . ,(frame-parameter nil 'font)) default-frame-alist)
          ;; fallback font for unicode characters used in space-macs
          (pcase system-type
            (`gnu/linux
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
            (let* ((fallback-props (space-macs/mplist-remove
                                    (space-macs/mplist-remove font-props :size)
                                    :height))
                   (fallback-spec (apply 'font-spec
                                         :name fallback-font-name
                                         fallback-props))
                   (fallback-spec2 (apply 'font-spec
                                          :name fallback-font-name2
                                          fallback-props)))
              ;; window numbers
              (set-fontset-font "fontset-default"
                                '(#x2776 . #x2793) fallback-spec nil 'prepend)
              ;; mode-line circled letters
              (set-fontset-font "fontset-default"
                                '(#x24b6 . #x24fe) fallback-spec nil 'prepend)
              ;; mode-line additional characters
              (set-fontset-font "fontset-default"
                                '(#x2295 . #x22a1) fallback-spec nil 'prepend)
              ;; new version lighter
              (set-fontset-font "fontset-default"
                                '(#x2190 . #x2200) fallback-spec2 nil 'prepend))))
        (throw 'break t)))
    nil))

(defun space-macs/compute-mode-line-height ()
  "Return an adjusted mode-line height."
  (let ((scale (if (and (boundp 'powerline-scale) powerline-scale)
                   powerline-scale 1)))
    (truncate (* scale (frame-char-height)))))

(defun space-macs/set-font (&rest args)
  "Deprecated function, display a warning message."
  (space-macs-buffer/warning (concat "space-macs/set-font is deprecated. "
                             "Use the variable `dotspace-macs-default-font' "
                             "instead (see Font section in "
                             "~/.e-macs.d/doc/DOCUMENTATION.org for more "
                             "info).")))

(defmacro space-macs|diminish (mode &optional unicode ascii)
  "Diminish MODE name in mode line to UNICODE or ASCII depending on the value
`dotspace-macs-mode-line-unicode-symbols'.
If ASCII is not provided then UNICODE is used instead. If neither are provided,
the mode will not show in the mode line."
  (when (and unicode
             (not (display-graphic-p)) ; terminal
             ;; the new indicator is 3 chars (including the space), ex: " â’ºh"
             (= (length unicode) 3))
    (setq unicode (space-macs/terminal-fix-mode-line-indicator-overlap unicode)))
  `(let ((cell (assq ',mode space-macs--diminished-minor-modes)))
     (if cell
         (setcdr cell '(,unicode ,ascii))
       (push '(,mode ,unicode ,ascii) space-macs--diminished-minor-modes))))

(defun space-macs/diminish-undo (mode)
  "Restore the diminished lighter."
  (interactive
   (list (read (completing-read
                "Restore what diminished mode: "
                (cons (list "diminished-modes")
                      (mapcar (lambda (x) (list (symbol-name (car x))))
                              diminished-mode-alist))
                nil t nil 'diminish-history-symbols))))
  ;; remove the `mode' entry from space-macs own list
  (setq space-macs--diminished-minor-modes
        (delq nil (mapcar (lambda (x) (unless (eq (car x) mode) x))
                          space-macs--diminished-minor-modes)))
  (diminish-undo mode))

(defmacro space-macs|hide-lighter (mode)
  "Diminish MODE name in mode line to LIGHTER."
  `(eval-after-load 'diminish '(diminish ',mode)))

(provide 'core-fonts-support)


