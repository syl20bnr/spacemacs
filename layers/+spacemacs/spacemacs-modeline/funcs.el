;;; funcs.el --- Spacemacs Mode-line Layer functions File
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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


(defun spacemacs/get-mode-line-theme-name ()
  "Return the mode-line theme name."
  (if (listp dotspacemacs-mode-line-theme)
      (car dotspacemacs-mode-line-theme)
    dotspacemacs-mode-line-theme))

(defun spacemacs/mode-line-separator ()
  "Return the separator type for the mode-line.
Return nil if no separator is defined."
  (let ((separator (when (listp dotspacemacs-mode-line-theme)
                     (plist-get (cdr dotspacemacs-mode-line-theme) :separator))))
    ;; `utf-8' separator is not supported by all-the-icons font
    ;; we force `utf-8' to be `arrow'
    (if (and (eq 'utf-8 separator)
             (eq 'all-the-icons (spacemacs/get-mode-line-theme-name)))
        'arrow
      separator)))

(defun spacemacs/mode-line-separator-scale ()
  "Return the separator scale for the mode-line.
Return nil if no scale is defined."
  (if (eq 'utf-8 (spacemacs/mode-line-separator))
      1
    (when (listp dotspacemacs-mode-line-theme)
      (plist-get (cdr dotspacemacs-mode-line-theme) :separator-scale))))


;; spaceline

(defun spacemacs/spaceline-config-startup-hook ()
  "Install a transient hook to delay spaceline config after Emacs starts."
  (spacemacs|add-transient-hook window-configuration-change-hook
    (lambda () (spacemacs/spaceline-config-startup)) lazy-load-spaceline))

(defun spacemacs/spaceline-config-startup ()
  "Compile the spaceline config."
  (setq spaceline-byte-compile t)
  ;; this must also be set in this hook because
  ;; (spacemacs/compute-mode-line-height) returns incorrect
  ;; results if it is called before the display system is
  ;; initialized. see issue for details:
  ;; https://github.com/syl20bnr/spacemacs/issues/10181
  (setq powerline-height
        (spacemacs/compute-mode-line-height))
  (spaceline-compile))


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
  "Restore the powerline in the buffers.
Excluding which-key."
  (dolist (buffer (buffer-list))
    (unless (string-match-p "\\*which-key\\*" (buffer-name buffer))
      (spacemacs//restore-powerline buffer))))

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
