;;; core-fonts-support.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'core-funcs)
(require 'core-spacemacs-buffer)

(defvar spacemacs--diminished-minor-modes nil
  "List of diminished modes to unicode or ascii values.")

(defun spacemacs/set-default-font (plist)
  "Set the font given the passed PLIST.

PLIST has the form (\"fontname\" :prop1 val1 :prop2 val2 ...)"
  (let* ((font (car plist))
         (props (cdr plist))
         (scale (plist-get props :powerline-scale))
         (font-props (spacemacs/mplist-remove
                      (spacemacs/mplist-remove props :powerline-scale)
                      :powerline-offset))
         (fontspec (apply 'font-spec :name font font-props)))
    (spacemacs-buffer/message "Setting font \"%s\"..." font)
    (set-frame-font fontspec nil t)
    (setq-default powerline-scale scale)
    (setq-default powerline-height (spacemacs/compute-powerline-height))
    ;; fallback font for unicode characters used in spacemacs
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
      (let* ((fallback-props (spacemacs/mplist-remove
                              (spacemacs/mplist-remove font-props :size)
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
                          '(#x2190 . #x2200) fallback-spec2 nil 'prepend)))
    ))

(defun spacemacs/compute-powerline-height ()
  "Return an adjusted powerline height."
  (let ((scale (if (and (boundp 'powerline-scale) powerline-scale)
                   powerline-scale 1)))
    (truncate (* scale (frame-char-height)))))

(defun spacemacs/set-font (&rest args)
  "Deprecated function, display a warning message."
  (spacemacs-buffer/warning (concat "spacemacs/set-font is deprecated. "
                             "Use the variable `dotspacemacs-default-font' "
                             "instead (see Font section in "
                             "~/.emacs.d/doc/DOCUMENTATION.org for more "
                             "info).")))

(defmacro spacemacs|diminish (mode unicode &optional ascii)
  "Diminish MODE name in mode line to UNICODE or ASCII depending on the value
`dotspacemacs-mode-line-unicode-symbols'.
If ASCII si not provided then UNICODE is used instead."
  `(add-to-list 'spacemacs--diminished-minor-modes '(,mode ,unicode ,ascii)))

(defmacro spacemacs|hide-lighter (mode)
  "Diminish MODE name in mode line to LIGHTER."
  `(eval-after-load 'diminish '(diminish ',mode)))

(provide 'core-fonts-support)
