;;; linum+.el --- Extension to linum.el displaying line numbers

;; Author: INA Lintaro <ina@kuis.kyoto-u.ac.jp>

;;; License:

;; NYSL Version 0.9982 (en)
;; ----------------------------------------
;; A. This software is "Everyone'sWare". It means:
;;   Anybody who has this software can use it as if you're
;;   the author.
;;
;;   A-1. Freeware. No fee is required.
;;   A-2. You can freely redistribute this software.
;;   A-3. You can freely modify this software. And the source
;;       may be used in any software with no limitation.
;;   A-4. When you release a modified version to public, you
;;       must publish it with your name.
;;
;; B. The author is not responsible for any kind of damages or loss
;;   while using or misusing this software, which is distributed
;;   "AS IS". No warranty of any kind is expressed or implied.
;;   You use AT YOUR OWN RISK.
;;
;; C. Copyrighted to INA Lintaro
;;
;; D. Above three clauses are applied both to source and binary
;;   form of this software.

;;; Commentary:

;; An extension to linum.el, which provides display of line numbers on the left
;; side of buffers.

;; Currently, this file provides only relative-linum-mode, which is to display
;; relative line numbers for the current buffer. It is convenient to show line
;; numbers counted from the line of the cursor position when you are using
;; vi emulation mode such as viper.el and about to operate multi-line commands
;; e.g. 'y15j'.

;; To use this mode, copy linum.el (written by Markus Triska) and
;; linum+.el (this file) to your load path and add to your .emacs:

;;   (require 'linum+)

;; Then toggle display of line numbers with M-x relative-linum-mode.

;;; Change Log:

;; Code:

(require 'linum)

(defvar relative-linum-mode nil)

(defcustom relative-disable-if-changed t
  "Whether relative line number mode should be disabled if buffer is changed."
  :group 'linum
  :type 'boolean)

(defadvice linum-mode
  (before ensure-relative-disabled first (&optional arg) activate)
  (if relative-linum-mode (relative-linum-mode 0)))
(defvar rel-linum-last 0 "Last state of linum-mode")
(defvar rel-linum-format 'dynamic "Last value of linum-format")
(defun rel-linum-restore ()
  (setq linum-format rel-linum-format)
  (internal-linum-mode rel-linum-last))
(defun internal-linum-mode (&optional arg)
  (ad-disable-advice 'linum-mode 'before 'ensure-relative-disabled)
  (ad-activate 'linum-mode)
  (linum-mode arg)
  (ad-enable-advice 'linum-mode 'before 'ensure-relative-disabled)
  (ad-activate 'linum-mode))

(defvar rel-linum-point nil)
(defun rel-linum-save-point () (setq rel-linum-point (point)))
(defun rel-linum-update (buf)
  (linum-delete-overlays)
  (rel-linum-save-point)
  (linum-update buf))
(defun rel-linum-schedule ()
  (run-with-idle-timer 0 nil #'rel-linum-update-current))
(defun rel-linum-update-current ()
  (rel-linum-update (current-buffer)))
(defun rel-linum-after-scroll (win start)
  (rel-linum-update (window-buffer win)))
(defun rel-linum-after-size (frame)
  (rel-linum-after-config))
(defun rel-linum-after-config ()
  (walk-windows (lambda (w) (rel-linum-update (window-buffer))) nil 'visible))
(defun rel-linum-after-change (beg end len)
  (if relative-disable-if-changed (relative-linum-mode 0)))

(define-minor-mode relative-linum-mode
  "Toggle display of relative line numbers in the left marginal area."
  :lighter ""
  (if relative-linum-mode
      (progn
        (if linum-mode (setq rel-linum-last 1) (setq rel-linum-last 0))
        (setq rel-linum-format
              (if linum-format linum-format 'dynamic))
        (internal-linum-mode 0)
        (setq linum-format
              (lambda (line)
                (let ((w (length (number-to-string
                                  (count-lines (point-min) (point-max))))))
                  (let ((fmt
                         (cond ((stringp rel-linum-format) rel-linum-format)
                               ((eq rel-linum-format 'dynamic)
                                (concat "%" (number-to-string w) "d"))))
                        (rline (abs (- (line-number-at-pos rel-linum-point)
                                       line))))
                    (if fmt
                        (propertize (format fmt rline) 'face 'linum)
                      (funcall rel-linum-format rline))))))
        (add-hook 'after-change-functions 'rel-linum-after-change nil t)
        (add-hook 'post-command-hook (if linum-delay
                                         'rel-linum-schedule
                                       'rel-linum-update-current) nil t)
        (add-hook 'window-scroll-functions 'rel-linum-after-scroll nil t)
        (add-hook 'window-size-change-functions 'rel-linum-after-size)
        (add-hook 'window-configuration-change-hook
                  'rel-linum-after-config nil t)
        (rel-linum-save-point)
        (internal-linum-mode 1))
    (remove-hook 'after-change-functions 'rel-linum-after-change t)
    (remove-hook 'post-command-hook 'rel-linum-update t)
    (remove-hook 'window-scroll-functions 'rel-linum-update t)
    (remove-hook 'window-size-change-functions 'rel-linum-update)
    (remove-hook 'window-configuration-change-hook 'rel-linum-update t)
    (rel-linum-restore)))

(provide 'linum+)
;;; linum+.el ends here
