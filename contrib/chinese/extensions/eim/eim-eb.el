;;; eim-eb.el --- 

;; Copyright (C) 2008 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 25 Apr 2008
;; Version: 0.01
;; Keywords: tools

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
;;   (require 'eim-eb)

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'eim-table)

(defvar eim-eb-user-file nil)
(defvar eim-eb-history-file nil)
(defvar eim-eb-package nil)
(defvar eim-eb-punctuation-list nil)
(defvar eim-eb-load-hook nil)
(defvar eim-eb-initialized nil)
(defvar eim-eb-char-table (make-vector 1511 0))

(defun eim-eb-create-word (word)
  "Insert word to database and write into user file"
  (let ((len (length word))
        code)
    (setq code
     (cond
      ((= len 2)
       (concat (substring (eim-table-get-char-code (aref word 0)) 0 2)
               (substring (eim-table-get-char-code (aref word 1)) 0 2)))
      ((= len 3)
       (concat (substring (eim-table-get-char-code (aref word 0)) 0 2)
               (substring (eim-table-get-char-code (aref word 1)) 0 1)
               (substring (eim-table-get-char-code (aref word 2)) 0 1)))
      (t
       (concat (substring (eim-table-get-char-code (aref word 0)) 0 1)
               (substring (eim-table-get-char-code (aref word 1)) 0 1)
               (substring (eim-table-get-char-code (aref word 2)) 0 1)
               (substring (eim-table-get-char-code (aref word (1- (length word)))) 0 1)))))))

(unless eim-eb-initialized
  (setq eim-eb-package eim-current-package)
  (setq eim-eb-punctuation-list
        (eim-read-punctuation eim-eb-package))
  (run-hooks 'eim-eb-load-hook)
  (let ((path (file-name-directory load-file-name)))
    (load (concat path "eim-eb-map")))
  (let ((map (eim-mode-map)))
    (define-key map "\t" 'eim-table-show-completion))
  
  (eim-table-add-user-file eim-eb-user-file)
  (eim-table-load-history eim-eb-history-file)
  (eim-set-option 'table-create-word-function 'eim-eb-create-word)
  (eim-set-option 'char-table eim-eb-char-table)
  (eim-set-option 'punctuation-list 'eim-eb-punctuation-list)
  (eim-set-option 'max-length 4)
  (eim-set-option 'translate-chars '(?\[))
  (eim-set-option 'all-completion-limit 3)
  (eim-set-active-function 'eim-table-active-function)
  (setq eim-eb-initialized t))

(provide 'eim-eb)
;;; eim-eb.el ends here
