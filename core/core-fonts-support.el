;;; core-fonts-support.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'core-funcs)
(require 'core-spacemacs-buffer)

(defun spacemacs/set-default-font (plist)
  "Set the font given the passed PLIST.

PLIST has the form (\"fontname\" :prop1 val1 :prop2 val2 ...)"
  (let* ((font (car plist))
         (props (cdr plist))
         (powerline-offset (plist-get props :powerline-offset))
         (font-props (spacemacs/mplist-remove props :powerline-offset))
         (fontspec (apply 'font-spec :family font font-props)))
    (set-default-font fontspec nil t)
    (setq-default powerline-height (+ powerline-offset (frame-char-height)))
    ;; fallback font for unicode characters used in spacemacs
    (pcase system-type
      ("gnu/linux"
       (setq fallback-font-name "NanumGothic")
       (setq fallback-font-name2 "NanumGothic"))
      ("darwin"
       (setq fallback-font-name "Arial Unicode MS")
       (setq fallback-font-name2 "Arial Unicode MS"))
      ("windows-nt"
       (setq fallback-font-name "MS Gothic")
       (setq fallback-font-name2 "Lucida Sans Unicode"))
      (other
       (setq fallback-font-name nil)
       (setq fallback-font-name2 nil)))
    (when (and fallback-font-name fallback-font-name2)
      (let ((fallback-spec (apply 'font-spec
                                   :family fallback-font-name font-props))
            (fallback-spec2 (apply 'font-spec
                                   :family fallback-font-name2 font-props)))
        ;; window numbers
        (set-fontset-font "fontset-default"
                          '(#x2776 . #x2793) fallback-spec nil 'prepend)
        ;; mode-line circled letters
        (set-fontset-font "fontset-default"
                          '(#x24b6 . #x24fe) fallback-spec nil 'prepend)
        ;; mode-line additional characters (i.e. golden ratio)
        (set-fontset-font "fontset-default"
                          '(#x2295 . #x22a1) fallback-spec nil 'prepend)
        ;; new version lighter
        (set-fontset-font "fontset-default"
                          '(#x2190 . #x2200) fallback-spec2 nil 'prepend)))
    ))

(defun spacemacs/set-font (&rest args)
  "Deprecated function, display a warning message."
  (spacemacs/message (concat "Warning: spacemacs/set-font is deprecated. "
                             "Use the variable `dotspacemacs-default-font' "
                             "instead (see Font section in "
                             "~/.emacs.d/doc/DOCUMENTATION.md for more "
                             "info).")))

(provide 'core-fonts-support)
