;;; -*- coding: utf-8 -*-
;;; eim-extra.el --- provide extra function for chinese input method 

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Version: $Id: eim-extra.el,v 1.2 2007/01/14 01:51:51 ywb Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'eim-extra)

;;;_* Code:

(provide 'eim-extra)
(eval-when-compile
  (require 'cl))
(require 'eim)

(defvar eim-punc-escape-list
  (number-sequence ?0 ?9)
  "Punctuation will not insert after this characters.
If you don't like this funciton, set the variable to nil")
(defvar eim-insert-ascii-char (cons ?\; "；")
  "*Key used for `eim-insert-ascii'.")

(defvar eim-punc-translate-p t
  "*Non-nil means will translate punctuation.")

;;;_. handle punctuation
(defun eim-read-punctuation (package)
  (let ((eim-current-package package)
	buf punc-list punc)
    (setq buf (cdr (assoc "buffer" (car (eim-buffer-list)))))
    (save-excursion
      (set-buffer buf)
      (save-restriction
        (widen)
        (let ((region (eim-section-region "Punctuation")))
          (goto-char (car region))
          (while (< (point) (cdr region))
            (setq punc (eim-line-content))
            (if (> (length punc) 3)
                (error "标点不支持多个转换！"))
            (add-to-list 'punc-list punc)
            (forward-line 1)))))
    punc-list))

(defun eim-punc-translate (punc-list char)
  (if eim-punc-translate-p
      (cond ((< char ? ) "")
            ((and eim-insert-ascii-char
                  (= char (car eim-insert-ascii-char)))
             (char-to-string char))
            (t (let ((str (char-to-string char))
                     punc)
                 (if (and (not (member (char-before) eim-punc-escape-list))
                          (setq punc (cdr (assoc str punc-list))))
                     (progn
                       (if (= char (char-before))
                           (delete-char -1))
                       (if (= (safe-length punc) 1)
                           (car punc)
                         (setcdr (cdr punc) (not (cddr punc)))
                         (if (cddr punc)
                             (car punc)
                           (nth 1 punc))))
                   str))))
    (char-to-string char)))

(defun eim-punc-translate-toggle (arg)
  (interactive "P")
  (setq eim-punc-translate-p
        (if (null arg)
            (not eim-punc-translate-p)
          (> (prefix-numeric-value arg) 0))))

;;;_. 一个快速插入英文的命令。按自己的需要绑定到 ";"
(defun eim-insert-ascii () 
  (interactive) 
  (if current-input-method
      (let (c)
        (message (format "自定义输入(直接空格%s, 回车%c): "
                         (cdr eim-insert-ascii-char)
                         (car eim-insert-ascii-char)))
        (setq c (read-event)) 
        (cond ((= c ? ) (insert (cdr eim-insert-ascii-char)))
              ((= c ?\r) (insert-char (car eim-insert-ascii-char) 1))
              (t 
               (setq unread-command-events (list last-input-event)) 
               (insert (read-from-minibuffer "自定义输入: ")))))
    (call-interactively 'self-insert-command)))

;;;_. load and save history
(defun eim-load-history (history-file package)
  (let* ((eim-current-package package)
         (history (eim-history))
         item)
    (when (file-exists-p history-file)
      (with-current-buffer (find-file-noselect history-file)
        (goto-char (point-min))
        (while (not (eobp))
          (if (and (setq item (eim-line-content))
                   (= (length item) 2))
              (puthash (car item)
                       `(nil ("pos" . ,(string-to-number (cadr item))))
                       history))
          (forward-line 1))
        (kill-buffer (current-buffer))))))

(defun eim-save-history (history-file package)
  (interactive)
  (let* ((eim-current-package package)
         (history (eim-history)))
    (with-temp-buffer 
      (erase-buffer)
      (let (pos)
        (maphash (lambda (key val)
                   (unless (or (eim-string-emptyp key)
                               (= (setq pos (cdr (assoc "pos" (cdr val)))) 1))
                     (insert key " " (number-to-string pos) "\n")))
                 history))
      (write-file history-file))))

;;;_. 增加两个快速选择的按键
(defun eim-quick-select-1 ()
  "如果没有可选项，插入数字，否则选择对应的词条"
  (interactive)
  (if (car eim-current-choices)
      (let ((index (eim-page-start))
            (end (eim-page-end)))
        (if (>= index end)
            (eim-append-string (eim-translate last-command-event))
          (eim-remember-select (1+ index))
          (setq eim-current-str (eim-choice (nth index (car eim-current-choices))))))
    (eim-append-string (eim-translate last-command-event)))
  (eim-terminate-translation))

(defun eim-quick-select-2 ()
  "如果没有可选项，插入数字，否则选择对应的词条"
  (interactive)
  (if (car eim-current-choices)
      (let ((index (1+ (eim-page-start)))
            (end (eim-page-end)))
        (if (>= index end)
            (eim-append-string (eim-translate last-command-event))
          (eim-remember-select (1+ index))
          (setq eim-current-str (eim-choice (nth index (car eim-current-choices))))))
    (eim-append-string (eim-translate last-command-event)))
  (eim-terminate-translation))

(defun eim-describe-char (pos package)
  (interactive
   (list (point)
         (if (eq input-method-function 'eim-input-method)
             (eim-package-name)
           (let (eim-current-package)
             (setq eim-current-package
                   (if (= (length eim-package-list) 1)
                       (cdar eim-package-list)
                     (assoc 
                      (completing-read "In package: "
                                       eim-package-list nil t
                                       (caar eim-package-list))
                      eim-package-list)))
             (eim-package-name)))))
  (if (>= pos (point-max))
      (error "No character follows specified position"))
  (let ((char (char-after pos))
        (func (intern-soft (format "%s-get-char-code" package)))
        code)
    (when func
      (setq code (funcall func char))
      (if code
          (message "Type %S to input %c for input method %s"
                   code char package)
        (message "Can't find char code for %c" char)))))

;;;_. char table
(defun eim-make-char-table (chars table)
  "Set `eim-char-database'"
  (dolist (char chars)
    (let ((code (car char)))
      (dolist (c (cdr char))
        (set (intern c table) code)))))

(defsubst eim-get-char-code (char table)
  "Get the code of the character CHAR"
  (symbol-value (intern-soft (char-to-string char) table)))

;;; eim-extra.el ends here
