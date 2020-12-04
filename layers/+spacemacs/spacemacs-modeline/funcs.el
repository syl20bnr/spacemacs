;;; funcs.el --- Space-macs Mode-line Layer functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs/get-mode-line-theme-name ()
  "Return the mode-line theme name."
  (if (listp dotspace-macs-mode-line-theme)
      (car dotspace-macs-mode-line-theme)
    dotspace-macs-mode-line-theme))

(defun space-macs/mode-line-separator ()
  "Return the separator type for the mode-line.
Return nil if no separator is defined."
  (let ((separator (when (listp dotspace-macs-mode-line-theme)
                     (plist-get (cdr dotspace-macs-mode-line-theme) :separator))))
    ;; `utf-8' separator is not supported by all-the-icons font
    ;; we force `utf-8' to be `arrow'
    (if (and (eq 'utf-8 separator)
             (eq 'all-the-icons (space-macs/get-mode-line-theme-name)))
        'arrow
      separator)))

(defun space-macs/mode-line-separator-scale ()
  "Return the separator scale for the mode-line.
Return nil if no scale is defined."
  (if (eq 'utf-8 (space-macs/mode-line-separator))
      1
    (when (listp dotspace-macs-mode-line-theme)
      (plist-get (cdr dotspace-macs-mode-line-theme) :separator-scale))))


;; spaceline

(defun space-macs/spaceline-config-startup-hook ()
  "Install a transient hook to delay spaceline config after e-macs starts."
  (space-macs|add-transient-hook window-configuration-change-hook
    (lambda () (space-macs/spaceline-config-startup)) lazy-load-spaceline))

(defun space-macs/spaceline-config-startup ()
  "Compile the spaceline config."
  (setq spaceline-byte-compile t)
  ;; this must also be set in this hook because
  ;; (space-macs/compute-mode-line-height) returns incorrect
  ;; results if it is called before the display system is
  ;; initialized. see issue for details:
  ;; https://github.com/syl20bnr/space-macs/issues/10181
  (setq powerline-height
        (space-macs/compute-mode-line-height))
  (spaceline-compile))


(defun space-macs/customize-powerline-faces ()
  "Alter powerline face to make them work with more themes."
  (when (boundp 'powerline-inactive2)
    (set-face-attribute 'powerline-inactive2 nil
                        :inherit 'font-lock-comment-face)))

(defun space-macs//evil-state-face ()
  (let ((state (if (eq 'operator evil-state) evil-previous-state evil-state)))
    (intern (format "space-macs-%S-face" state))))

(defun space-macs//restore-powerline (buffer)
  "Restore the powerline in buffer"
  (with-current-buffer buffer
    (setq-local mode-line-format (default-value 'mode-line-format))
    (powerline-set-selected-window)
    (powerline-reset)))

(defun space-macs//restore-buffers-powerline ()
  "Restore the powerline in the buffers.
Excluding which-key."
  (dolist (buffer (buffer-list))
    (unless (string-match-p "\\*which-key\\*" (buffer-name buffer))
      (space-macs//restore-powerline buffer))))

(defun space-macs//prepare-diminish ()
  (when spaceline-minor-modes-p
    (let ((unicodep (dotspace-macs|symbol-value
                     dotspace-macs-mode-line-unicode-symbols)))
      (setq spaceline-minor-modes-separator
            (if unicodep (if (display-graphic-p) "" " ") "|"))
      (dolist (mm space-macs--diminished-minor-modes)
        (let ((mode (car mm)))
          (when (and (boundp mode) (symbol-value mode))
            (let* ((unicode (cadr mm))
                   (ascii (caddr mm))
                   (dim (if unicodep
                            unicode
                          (if ascii ascii unicode))))
              (diminish mode dim))))))))


;; Vim powerline

(defun space-macs//set-vimish-powerline-for-startup-buffers ()
  "Set the powerline for buffers created when e-macs starts."
  (dolist (buffer '("*Messages*" "*space-macs*" "*Compile-Log*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (setq-local mode-line-format (default-value 'mode-line-format))
        (powerline-set-selected-window)
        (powerline-reset)))))


