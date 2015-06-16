;; -*- coding: utf-8 -*-
;;; eim-wb.el --- emacs chinese wubi input method for eim

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Version: $Id: eim-wb.el,v 1.3 2007/01/14 02:01:48 ywb Exp $
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

;;; Features:
;; 1. 能导入输入历史
;; 2. 提供造词的命令
;; 3. 提供候选的单字
;; 4. 拼音输入，提示五笔字根
;; 5. 处理标点
;; 6. 使用 ; ' 快速选择

;;; Commentary:

;;;_* Code:

(eval-when-compile
  (require 'cl))

(require 'eim-table)

(defgroup eim-wb nil
  "eim wubi input method"
  :group 'eim)
  
(defcustom eim-wb-history-file "~/.emacs.d/wbx-history"
  "保存选择的历史记录"
  :type 'file
  :group 'eim-wb)

(defcustom eim-wb-user-file "mywb.txt"
  "保存用户自造词"
  :type 'file
  :group 'eim-wb)

(defcustom eim-wb-save-always nil
  "是否每次加入新词都要保存。
当然设置为 nil，也会在退出 emacs 里保存一下的。"
  :type 'boolean
  :group 'eim-wb)

(defcustom eim-wb-add-all-completion-limit 3
  "在超过输入字符串超过这个长度时会添加所有补全。"
  :type 'integer
  :group 'eim-wb)

(defvar eim-wb-load-hook nil)
(defvar eim-wb-package nil)
(defvar eim-wb-char-table (make-vector 1511 0))
(defvar eim-wb-punctuation-list nil)
(defvar eim-wb-initialized nil)

(defun eim-wb-create-word (word)
  "Insert word to database and write into user file"
  (let ((len (length word))
        code)
    (setq code
     (cond
      ((= len 2)
       (concat (substring (eim-table-get-char-code (aref word 0)) 0 2)
               (substring (eim-table-get-char-code (aref word 1)) 0 2)))
      ((= len 3)
       (concat (substring (eim-table-get-char-code (aref word 0)) 0 1)
               (substring (eim-table-get-char-code (aref word 1)) 0 1)
               (substring (eim-table-get-char-code (aref word 2)) 0 2)))
      (t
       (concat (substring (eim-table-get-char-code (aref word 0)) 0 1)
               (substring (eim-table-get-char-code (aref word 1)) 0 1)
               (substring (eim-table-get-char-code (aref word 2)) 0 1)
               (substring (eim-table-get-char-code (aref word (1- (length word)))) 0 1)))))))

;;;_. load it
(unless eim-wb-initialized
  (setq eim-wb-package eim-current-package)
  (setq eim-wb-punctuation-list
        (eim-read-punctuation eim-wb-package))
  (let ((map (eim-mode-map)))
    (define-key map "\t" 'eim-table-show-completion)
    (define-key map ";" 'eim-quick-select-1)
    (define-key map "'" 'eim-quick-select-2))
  (defvar eim-wb-use-gbk nil)
  (let ((path (file-name-directory load-file-name)))
    (load (concat path 
                  (if (and (boundp 'eim-wb-use-gbk)
                           eim-wb-use-gbk)
                      "eim-wb-gbk" "eim-wb-gb2312"))))

  (eim-table-add-user-file eim-wb-user-file)
  (eim-table-load-history eim-wb-history-file)
  (run-hooks 'eim-wb-load-hook)
  (eim-set-option 'table-create-word-function 'eim-wb-create-word)
  (eim-set-option 'punctuation-list 'eim-wb-punctuation-list)
  (eim-set-option 'max-length 4)
  (eim-set-option 'translate-chars '(?z))
  (eim-set-option 'all-completion-limit eim-wb-add-all-completion-limit)
  (eim-set-option 'char-table eim-wb-char-table)
  (eim-set-active-function 'eim-table-active-function)
  (setq eim-wb-initialized t))

(provide 'eim-wb)
;;; eim-wb.el ends here
