;;; eim-cj.el --- 

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
;;   (require 'eim-cj)

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'eim-table)

(defvar eim-cj-package nil)
(defvar eim-cj-punctuation-list nil)
(defvar eim-cj-initialized nil)
(defvar eim-cj-load-hook nil)
(defvar eim-cj-char-table (make-vector 1511 0))

(unless eim-cj-initialized
  (setq eim-cj-package eim-current-package)
  (setq eim-cj-punctuation-list
        (eim-read-punctuation eim-cj-package))
  (run-hooks 'eim-cj-load-hook)
  (let ((path (file-name-directory load-file-name)))
    (load (concat path "eim-cj-chars")))
  (eim-set-option 'char-table eim-cj-char-table)
  (eim-set-option 'punctuation-list 'eim-cj-punctuation-list)
  (eim-set-option 'max-length 5)
  (eim-set-option 'translate-chars '(?x ?z))
  (eim-set-option 'all-completion-limit 3)
  (eim-set-active-function 'eim-table-active-function)
  (setq eim-cj-initialized t))

(provide 'eim-cj)
;;; eim-cj.el ends here
