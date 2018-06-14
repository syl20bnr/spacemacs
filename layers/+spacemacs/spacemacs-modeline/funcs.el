;;; funcs.el --- Spacemacs Mode-line Layer functions File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/get-mode-line-theme-name ()
  "Return the mode-line theme name."
  (if (listp dotspacemacs-mode-line-theme)
      (car dotspacemacs-mode-line-theme)
    dotspacemacs-mode-line-theme))

(defun spacemacs/mode-line-separator ()
  "Return the separator type for the mode-line.
Return nil if no separator is defined."
  (when (listp dotspacemacs-mode-line-theme)
    (plist-get (cdr dotspacemacs-mode-line-theme) :separator)))

(defun spacemacs/mode-line-separator-scale ()
  "Return the separator scale for the mode-line.
Return nil if no scale is defined."
  (if (eq 'utf-8 (spacemacs/mode-line-separator))
      1
    (when (listp dotspacemacs-mode-line-theme)
      (plist-get (cdr dotspacemacs-mode-line-theme) :separator-scale))))


;; spaceline

(defun spacemacs/customize-powerline-faces ()
  "Alter powerline face to make them work with more themes."
  (when (boundp 'powerline-inactive2)
    (set-face-attribute 'powerline-inactive2 nil
                        :inherit 'font-lock-comment-face)))

(defun spacemacs//evil-state-face ()
  (let ((state (if (eq 'operator evil-state) evil-previous-state evil-state)))
    (intern (format "spacemacs-%S-face" state))))

(defun spacemacs//restore-powerline (buffer)
  "Restore the powerline in buffer"
  (with-current-buffer buffer
    (setq-local mode-line-format (default-value 'mode-line-format))
    (powerline-set-selected-window)
    (powerline-reset)))

(defun spacemacs//restore-buffers-powerline ()
  "Restore the powerline in all buffers."
  (dolist (buffer (buffer-list))
    (spacemacs//restore-powerline buffer)))

(defun spacemacs//prepare-diminish ()
  (when spaceline-minor-modes-p
    (let ((unicodep (dotspacemacs|symbol-value
                     dotspacemacs-mode-line-unicode-symbols)))
      (setq spaceline-minor-modes-separator
            (if unicodep (if (display-graphic-p) "" " ") "|"))
      (dolist (mm spacemacs--diminished-minor-modes)
        (let ((mode (car mm)))
          (when (and (boundp mode) (symbol-value mode))
            (let* ((unicode (cadr mm))
                   (ascii (caddr mm))
                   (dim (if unicodep
                            unicode
                          (if ascii ascii unicode))))
              (diminish mode dim))))))))


;; Vim powerline

(defun spacemacs//set-vimish-powerline-for-startup-buffers ()
  "Set the powerline for buffers created when Emacs starts."
  (dolist (buffer '("*Messages*" "*spacemacs*" "*Compile-Log*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (setq-local mode-line-format (default-value 'mode-line-format))
        (powerline-set-selected-window)
        (powerline-reset)))))
