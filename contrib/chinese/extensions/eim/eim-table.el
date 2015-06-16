;;; eim-table.el --- Common function for table like chinese input method

;; Copyright (C) 2008 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 27 Apr 2008
;; Version: 0.01
;; Keywords: convenience

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

;; - punctuation-list: A symbol to translate punctuation
;; - translate-chars: The first letter which will invoke reverse
;;                   search the code for char
;; - max-length: max input string length
;; - char-table: a obarray to search code for char
;; - all-completion-limit: A minimal length to add all completions
;; - table-create-word-function
;; 
;; - table-user-file
;; - table-history-file

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'eim-table)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'eim)
(require 'eim-extra)

(defun eim-table-translate (char)
  (eim-punc-translate (symbol-value (eim-get-option 'punctuation-list))
                      char))

(defun eim-table-get-char-code (char)
  (eim-get-char-code char (eim-get-option 'char-table)))

(defun eim-table-format (key cp tp choice)
  (if (memq (aref key 0) (eim-get-option 'translate-chars))
      (setq choice
            (mapcar (lambda (c)
                      (if (consp c)
                          (setq c (car c)))
                      (cons c
                            (eim-table-get-char-code (aref c 0))))
                    choice)))
  (let ((i 0))
    (format "%s[%d/%d]: %s"
            key  cp tp
            (mapconcat 'identity
                       (mapcar
                        (lambda (c)
                          (format "%d.%s " (setq i (1+ i))
                                  (if (consp c)
                                      (concat (car c) (cdr c))
                                    c)))
                        choice) " "))))

;;;_. 增加补全
(defun eim-table-add-completion ()
  (if (= (length eim-current-key) 1)
      t
    (let ((reg (concat "^" (regexp-quote eim-current-key)))
          (len (length eim-current-key))
          (package eim-current-package)
          (key eim-current-key)
          line completion)
      (save-excursion
        (dolist (buf (mapcar 'cdar (eim-buffer-list)))
          (set-buffer buf)
          (setq eim-current-package package)
          (beginning-of-line)
          (if (or (string= (eim-code-at-point) key)
                  (not (looking-at reg)))
              (forward-line 1))
          (while (looking-at reg)
            (setq line (eim-line-content))
            (mapc (lambda (c)
                    (when (or (>= len (eim-get-option 'all-completion-limit))
                              (= (length c) 1))
                      (push (cons c (substring
                                     (car line)
                                     len))
                            completion)))
                  (cdr line))
            (forward-line 1))))
      (setq completion (sort (delete-dups (nreverse completion))
                             (lambda (a b)
                               (< (length (cdr a)) (length (cdr b))))))
      ;;      (message "%s, %s" eim-current-choices completion)
      (setcar eim-current-choices (append (car eim-current-choices)
                                          completion))
      ;;      (message "%s, %s" eim-current-choices completion))
      t)))

(defun eim-table-stop-function ()
  (if (memq (aref eim-current-key 0) (eim-get-option 'translate-chars))
      nil
    (> (length eim-current-key) (eim-get-option 'max-length))))

(defun eim-table-active-function ()
  (setq eim-add-completion-function 'eim-table-add-completion
        eim-translate-function 'eim-table-translate
        eim-format-function 'eim-table-format
        eim-stop-function 'eim-table-stop-function))

;; user file and history file
;;;_. eim-wb-add-user-file
(defun eim-table-add-user-file (file)
  (when file
    (let* ((buflist (eim-buffer-list))
           (ufile (expand-file-name file))
           user-buffer)
      (or (file-exists-p ufile)
          (setq ufile (locate-file file load-path)))
      (when (and ufile (file-exists-p ufile))
        ;; make sure the file not load again
        (mapc (lambda (buf)
                (if (string= (expand-file-name (cdr (assoc "file" buf)))
                             ufile)
                    (setq user-buffer (cdr (assoc "buffer" buf)))))
              buflist)
        (unless user-buffer
          (save-excursion
            (setq file (eim-read-file ufile (format eim-buffer-name-format
                                                    (eim-package-name)))))
          (eim-table-add-user-char)
          (nconc buflist (list file))
          (eim-set-option 'table-user-file (cons ufile (cdar file))))))))

(defun eim-table-add-user-char ()
  "Add user characters. Currently eim-wb may not contain all
chinese characters, so if you want more characters to input, you
can add here."
  (let (line chars)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq line (eim-line-content))
        (forward-line 1)
        (if (and (= (length (cadr line)) 1)
                 (> (length (car line)) 2))
            (push  line chars)))
      (eim-make-char-table chars (eim-get-option 'char-table)))))

(defun eim-table-load-history (his-file)
  (when (and his-file (file-exists-p his-file))
    (ignore-errors
      (eim-load-history his-file eim-current-package)
      (eim-set-option 'record-position t)
      (eim-set-option 'table-history-file his-file))))

(defun eim-table-save-history ()
  "Save history and user files."
  (dolist (package eim-package-list)
    (let* ((eim-current-package (cdr package))
           (his-file (eim-get-option 'table-history-file))
           (user-file (eim-get-option 'table-user-file)))
      (when (and his-file
                 (file-exists-p his-file)
                 (file-writable-p his-file))
        (eim-save-history his-file eim-current-package))
      (when (and user-file
                 (file-exists-p (car user-file))
                 (file-writable-p (car user-file)))
        (with-current-buffer (cdr user-file)
          (save-restriction
            (widen)
            (write-region (point-min) (point-max) (car user-file))))))))
;; 按 TAB 显示补全
(defun eim-table-show-completion ()
  (interactive)
  (if (eq last-command 'eim-table-show-completion)
      (ignore-errors
        (with-selected-window (get-buffer-window "*Completions*")
          (scroll-up)))
    (if (or (= (length eim-current-key) 1) (= (aref eim-current-key 0) ?z))
        nil
      (while (not (eim-add-completion)))
      (let ((choices (car eim-current-choices))
            completion)
        (dolist (c choices)
          (if (listp c)
              (push (list (format "%-4s %s"
                                  (concat eim-current-key (cdr c))
                                  (car c)))
                    completion)))
        (with-output-to-temp-buffer "*Completions*"
          (display-completion-list
           (all-completions eim-current-key (nreverse completion))
           eim-current-key)))))
  (funcall eim-handle-function))

;; 增加新词
(defvar eim-table-minibuffer-map nil)
(defvar eim-table-save-always nil)
(when (null eim-table-minibuffer-map)
  (setq eim-table-minibuffer-map
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map minibuffer-local-map)
          (define-key map "\C-e" 'eim-table-minibuffer-forward-char)
          (define-key map "\C-a" 'eim-table-minibuffer-backward-char)
          map)))
;;;_. 增加新词
(defun eim-table-minibuffer-forward-char ()
  (interactive)
  (end-of-line)
  (let ((char (save-excursion
                (set-buffer buffer)
                (char-after end))))
    (when char
      (insert char)
      (incf end))))

(defun eim-table-minibuffer-backward-char ()
  (interactive)
  (beginning-of-line)
  (let ((char (save-excursion
                (set-buffer buffer)
                (when (>= start (point-min))
                  (decf start)
                  (char-after start)))))
    (when char
      (insert char))))

(defun eim-table-add-word ()
  "Create a map for word. The default word is the two characters
before cursor. You can use C-a and C-e to add character at the
begining or end of the word.

默认新词为光标前的两个字，通过两个按键延长这个词：
 C-e 在头部加入一个字
 C-a 在尾部加入一个字
"
  (interactive)
  (let* ((buffer (current-buffer))
         (end (point))
         (start (- (point) 2))
         (word (buffer-substring-no-properties
                start end))
         (user-file (eim-get-option 'table-user-file))
         (func (eim-get-option 'table-create-word-function))
         choice code words)
    (when func
      (setq word (read-from-minibuffer "加入新词: " word
                                       eim-table-minibuffer-map)
            code (funcall func word))
      (setq choice (eim-get code))
      (unless (member word (car choice))
        (if (buffer-live-p (cdr user-file))
            (save-excursion
              (set-buffer (cdr user-file))
              (if (string-match "^\\s-$" (buffer-string))
                  (insert "\n" code " " word)
                (eim-bisearch-word code (point-min) (point-max))
                (let ((words (eim-line-content)))
                  (goto-char (line-end-position))
                  (if (string= (car words) code)
                      (insert " " word)
                    (insert "\n" code " " word))))
              (setcar choice (append (car choice) (list word)))
              (if eim-table-save-always
                  (save-restriction
                    (widen)
                    (write-region (point-min) (point-max) (car user-file)))))
          (error "the user buffer is closed!")))))
  (message nil))

(add-hook 'kill-emacs-hook 'eim-table-save-history)

(provide 'eim-table)
;;; eim-table.el ends here
