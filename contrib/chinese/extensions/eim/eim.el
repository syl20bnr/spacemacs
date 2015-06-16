;;; -*- coding: utf-8 -*-
;;; eim.el --- Emacs Input method

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Version: $Id: eim.el,v 1.5 2007/01/14 01:50:15 ywb Exp $
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
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/eim")
;; (autoload 'eim-use-package "eim" "Another emacs input method")

;; (register-input-method
;;  "eim-wb" "euc-cn" 'eim-use-package
;;  "五笔" "汉字五笔输入法" "wb.txt")
;; (register-input-method
;;  "eim-py" "euc-cn" 'eim-use-package
;;  "拼音" "汉字拼音输入法" "py.txt")

;;; Code:

(provide 'eim)
(eval-when-compile
  (require 'cl))
(require 'help-mode)

(defvar eim-version "2.4")

;;;_. emacs21 compatible
(when (not (fboundp 'number-sequence))
  (defun number-sequence (from &optional to inc)
    (if (and to (<= from to))
        (cons from
              (number-sequence (+ from (or inc 1)) to inc)))))

(when (not (fboundp 'delete-dups))
  (defun delete-dups (list)
    "Destructively remove `equal' duplicates from LIST.
Store the result in LIST and return it.  LIST must be a proper list.
Of several `equal' occurrences of an element in LIST, the first
one is kept."
    (let ((tail list))
      (while tail
        (setcdr tail (delete (car tail) (cdr tail)))
        (setq tail (cdr tail))))
    list))

;;;_. customize varable
(defgroup eim nil
  "eim: emacs input method"
  :group 'leim)
(defvar eim-page-length 7 "每页显示的词条数目")

(defface eim-string-face '((t (:underline t)))
  "Face to show current string"
  :group 'eim)

;;;_. variable declare
(defvar eim-package-list nil "所有正在使用的输入法")
(defvar eim-current-package (make-vector 5 nil)
  "当前使用的输入法，一个 vector，有五个部分: package-name,
buffer-list,history, keymap, active-function.

buffer-list 中的每个 buffer 是这样的一个 Association List：
----------------------------------------
buffer         对应的 buffer
param          Parameter 部分的参数
file           对应的文件名
")
(defvar eim-first-char (number-sequence ?a ?z) "Table 中所有首字母列表")
(defvar eim-total-char (number-sequence ?a ?z) "所有可能的字符")
(defvar eim-do-completion t "是否读入可能的补全")

(defvar eim-current-key "" "已经输入的代码")
(defvar eim-current-str "" "当前选择的词条")
(defvar eim-current-choices nil "所有可选的词条。

这个 list 的 CAR 是可选的词条，一般是一个字符串列表，但是也可以含有
list。但是这个 list 的第一个元素必须是将要插入的字符串。

CDR 部分是一个 Association list。通常含有这样的内容：
---------------------------
pos         上次选择的位置
completion  下一个可能的字母（如果 eim-do-completion 为 t）
")
(defvar eim-current-pos nil "当前选择的词条在 eim-current-choices 中的位置")
(defvar eim-guidance-str "" "显示可选词条的字符串")
(defvar eim-translating nil "记录是否在转换状态")
(defvar eim-overlay nil "显示当前选择词条的 overlay")
(defvar eim-guidance-frame nil)
(defvar eim-guidance-buf nil)

(defvar eim-load-hook nil)
(defvar eim-active-hook nil)

(defvar eim-stop-function nil)
(defvar eim-translate-function nil)
(defvar eim-add-completion-function nil)
(defvar eim-format-function 'eim-format)
(defvar eim-handle-function 'eim-handle-string)

(defvar eim-use-tooltip (not (or noninteractive
                                 emacs-basic-display
                                 (not (display-graphic-p))
                                 (not (fboundp 'x-show-tip)))))
(defvar eim-tooltip-timeout 15)

(defvar eim-buffer-name-format " *%s*"
  "buffer 的名字格式，%s 对应 package name")

(defvar eim-mode-map
  (let ((map (make-sparse-keymap))
        (i ?\ ))
    (while (< i 127)
      (define-key map (char-to-string i) 'eim-self-insert-command)
      (setq i (1+ i)))
    (setq i 128)
    (while (< i 256)
      (define-key map (vector i) 'eim-self-insert-command)
      (setq i (1+ i)))
    (dolist (i (number-sequence ?1 ?9))
      (define-key map (char-to-string i) 'eim-number-select))
    (define-key map " " 'eim-select-current)
    (define-key map [backspace] 'eim-delete-last-char)
    (define-key map [delete] 'eim-delete-last-char)
    (define-key map "\177" 'eim-delete-last-char)
    (define-key map "\C-n" 'eim-next-page)
    (define-key map "\C-p" 'eim-previous-page)
    (define-key map "\C-m" 'eim-quit-no-clear)
    (define-key map "\C-c" 'eim-quit-clear)
    (define-key map "\C-g" 'eim-quit-clear)
    map)
  "Keymap")

(defvar eim-local-variable-list
  '(eim-current-package

    eim-page-length
    eim-first-char
    eim-total-char
    eim-do-completion

    eim-current-key
    eim-current-str
    eim-current-choices
    eim-current-pos
    eim-guidance-str
    eim-translating
    eim-overlay
    eim-guidance-frame
    eim-guidance-buf

    eim-load-hook
    eim-active-hook

    eim-translate-function
    eim-format-function
    eim-handle-function
    eim-add-completion-function
    eim-stop-function

    input-method-function
    inactivate-current-input-method-function
    describe-current-input-method-function)
  "A list of buffer local variable")

(dolist (var eim-local-variable-list)
  (make-variable-buffer-local var)
  (put var 'permanent-local t))

;;;_ , package contents
(defsubst eim-package-name ()
  (aref eim-current-package 0))

(defsubst eim-buffer-list ()
  (aref eim-current-package 1))

(defsubst eim-history ()
  "保存输入过的词的选择，另一方面加快搜索。另外在这里来处理标点。
这个散列中的每个元素都有这样的格式：
  ((list WORDS) other-properties)
OTHER-PROPERTIES 是一些其它的属性，比如，上次的位置，用来输入标点等。"
  (aref eim-current-package 2))

(defsubst eim-mode-map ()
  (aref eim-current-package 3))

(defsubst eim-options ()
  (aref eim-current-package 4))

(defsubst eim-active-function ()
  (aref eim-current-package 5))

(defsubst eim-set-package-name (name)
  (aset eim-current-package 0 name))

(defsubst eim-set-buffer-list (list)
  (aset eim-current-package 1 list))

(defsubst eim-set-history (history)
  (aset eim-current-package 2 history))

(defsubst eim-set-mode-map (map)
  (aset eim-current-package 3 map))

(defsubst eim-set-options (options)
  (aset eim-current-package 4 options))

(defsubst eim-set-active-function (func)
  (aset eim-current-package 5 func))

(defun eim-get-option (option)
  (cdr (assoc option (eim-options))))
(defun eim-set-option (option flag)
  (let ((options (eim-options))
        opt)
    (if (setq opt (assoc option options))
        (setcdr opt flag)
      (push (cons option flag) options)
      (eim-set-options options))))

;;;_. read file functions
(defun eim-load-file (file)
  (let ((bufname (format eim-buffer-name-format (eim-package-name)))
        buflist buf param files)
    (save-excursion
      (setq buf (eim-read-file file bufname t))
      (setq param (cdr (assoc "param" buf)))
      (setq buflist (append buflist (list buf)))
      (when (setq files (assoc "other-files" param))
        (setq files (split-string (cadr files) ";"))
        (dolist (f files)
          (if (file-exists-p (expand-file-name f))
              (setq f (expand-file-name f))
            (setq f (locate-file f load-path)))
          (setq buflist (append buflist (list (eim-read-file f bufname))))))
      buflist)))

(defun eim-read-file (file name &optional read-param)
  (let* (param region)
    (set-buffer (generate-new-buffer name))
    (insert-file-contents file)
    (if read-param
        (setq param (eim-read-parameters)))
    (setq region (eim-section-region "Table"))
    (narrow-to-region (car region) (cdr region))
    `(("buffer" . ,(current-buffer))
      ("param" . ,param)
      ("file" . ,file))))

(defun eim-section-region (sec)
  "得到一个部分的起点和终点位置，忽略最后的空行"
  (let ((reg (concat "^\\[" sec "\\]\n")))
    (save-excursion
      (if (not (re-search-forward reg nil t))
          (if (re-search-backward reg nil t)
              (forward-line 1)
            (error "文件类型错误！没有 %s 部分！" sec)))
      (cons (point) (progn
                      (if (re-search-forward "^\\[\\sw+\\]\n" nil t)
                          (forward-line -1)
                        (goto-char (point-max)))
                      (re-search-backward "[^  \t\n]" nil t)
                      (1+ (point)))))))

(defun eim-read-parameters ()
  "得到 [Parameter] 部分的参数，以 assoc list 的形式返回"
  (let* ((r (eim-section-region "Parameter"))
         param pair)
    (goto-char (car r))
    (while (< (point) (cdr r))
      (when (setq pair (eim-line-content "=" t))
        (add-to-list 'param pair))
      (forward-line 1))
    param))

;;;_. common functions

(defsubst eim-delete-region ()
  "Delete the text in the current translation region of E+."
  (if (overlay-start eim-overlay)
      (delete-region (overlay-start eim-overlay)
                     (overlay-end eim-overlay))))

;;; steal from emms-compat.el. Is this a good idea?
(when (not (fboundp 'emms-delete-if))
  (defun emms-delete-if (predicate seq)
    "Remove all items satisfying PREDICATE in SEQ.
This is a destructive function: it reuses the storage of SEQ
whenever possible."
    ;; remove from car
    (while (when (funcall predicate (car seq))
             (setq seq (cdr seq))))
    ;; remove from cdr
    (let ((ptr seq)
          (next (cdr seq)))
      (while next
        (when (funcall predicate (car next))
          (setcdr ptr (if (consp next)
                          (cdr next)
                        nil)))
        (setq ptr (cdr ptr))
        (setq next (cdr ptr))))
    seq))

(defun eim-subseq (list from &optional to)
  (if (null to) (nthcdr from list)
    (butlast (nthcdr from list) (- (length list) to))))

(defun eim-mod (x y)
  "like `mod', but when result is 0, return Y"
  (let ((base (mod x y)))
    (if (= base 0)
        y
      base)))

(defun eim-string-emptyp (str)
  (not (string< "" str)))

(defun eim-line-content (&optional seperaters omit-nulls)
  "用 SEPERATERS 分解当前行，所有参数传递给 split-string 函数"
  (let ((items   (split-string
                  (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position)) seperaters)))
    (if omit-nulls
        (emms-delete-if 'eim-string-emptyp items)
      items)))

(defsubst eim-delete-line ()
  (delete-region (line-beginning-position) (min (+ (line-end-position) 1)
                                                (point-max))))

(defsubst eim-append-string (str)
  "append STR to eim-current-str"
  (setq eim-current-str (concat eim-current-str str)))

;;;_. code search
(defun eim-get (code)
  (when (and (stringp code) (not (eim-string-emptyp code)))
    (let ((history (gethash code (eim-history)))
          pos words completions)
      (if (and (car history) (assoc "completions" (cdr history)))
          history
        (dolist (buf (eim-buffer-list))
          (with-current-buffer (cdr (assoc "buffer" buf))
            (setq words (append words
                                (cdr
                                 (eim-bisearch-word code
                                                    (point-min)
                                                    (point-max)))))
            (if eim-do-completion
                (setq completions (eim-completions code completions)))))
        (setq words (delete-dups words))
        (puthash code (list words
                            (cons "pos" (or (cdr (assoc "pos" (cdr history))) 1))
                            (cons "completions" completions))
                 (eim-history))))))

(defun eim-completions (code completions)
  (let ((maxln 200)
        (cnt 0)
        (len (length code))
        (reg (concat "^" (regexp-quote code))))
    (save-excursion
      (forward-line 1)
      (while (and (looking-at reg)
                  (< cnt maxln))
        (add-to-list 'completions (buffer-substring-no-properties
                                   (+ (point) len)
                                   (+ (point) len 1)))
        (forward-line 1)
        (setq cnt (1+ cnt)))
      completions)))

(defun eim-bisearch-word (code start end)
  (let ((mid (/ (+ start end) 2))
        ccode)
    (goto-char mid)
    (beginning-of-line)
    (setq ccode (eim-code-at-point))
    ;;    (message "%d, %d, %d: %s" start mid end ccode)
    (if (string= ccode code)
        (eim-line-content)
      (if (> mid start)
          (if (string< ccode code)
              (eim-bisearch-word code mid end)
            (eim-bisearch-word code start mid))))))

(defun eim-code-at-point ()
  "Before calling this function, be sure that the point is at the
beginning of line"
  (save-excursion
    (if (re-search-forward "[ \t]" (line-end-position) t)
        (buffer-substring-no-properties (line-beginning-position) (1- (point)))
      (error "文件类型错误！%s 的第 %d 行没有词条！" (buffer-name) (line-number-at-pos)))))

;;;_. interface
(defun eim-check-buffers ()
  "检查所有的 buffer 是否还存在，如果不存在，重新打开文件，如果文件不
存在，从 buffer-list 中删除这个 buffer"
  (let ((buflist (eim-buffer-list))
        (bufname (eim-package-name))
        buffer file)
    (dolist (buf buflist)
      (unless (buffer-live-p (cdr (setq buffer (assoc "buffer" buf))))
        (if (file-exists-p (setq file (cdr (assoc "file" buf))))
            (with-current-buffer (format "*%s*" (generate-new-buffer bufname))
              (insert-file-contents file)
              (setcdr buffer (current-buffer)))
          (message "%s for %s is not exists!" file bufname)
          (setq buflist (remove buf buflist)))))
    t))

(defun eim-install-variable ()
  (let ((param (cdr (assoc "param" (car (eim-buffer-list))))))
    (mapc (lambda (p)
            (let ((sym (intern-soft (concat "eim-" (car p)))))
              (if sym
                  (set sym (mapconcat 'identity (cdr p) "=")))))
          param)
    (if (stringp eim-page-length)
        (setq eim-page-length (string-to-number eim-page-length)))
    (setq eim-first-char (append eim-first-char nil)
          eim-total-char (append eim-total-char nil))))

;;;_ , eim-use-package
(defun eim-use-package (package-name &optional word-file active-func)
  (interactive)
  (mapc 'kill-local-variable eim-local-variable-list)
  (mapc 'make-local-variable eim-local-variable-list)
  (if (assoc package-name eim-package-list)
      (setq eim-current-package (cdr (assoc package-name
                                            eim-package-list)))
    ;; make more room for extension
    (setq eim-current-package (make-vector 9 nil)))
  (if (functionp active-func)
      (funcall active-func))
  (unless (and (eim-package-name)
               (eim-check-buffers))
    (if (and word-file
             (if (file-exists-p (expand-file-name word-file))
                 (setq word-file (expand-file-name word-file))
               (setq word-file (locate-file word-file load-path))))
        (progn
          (eim-set-package-name package-name)
          (eim-set-buffer-list (eim-load-file word-file))
          (eim-set-history (make-hash-table :test 'equal))
          (eim-set-mode-map (let ((map (make-sparse-keymap)))
                              (set-keymap-parent map eim-mode-map)
                              map))
          (add-to-list 'eim-package-list (cons package-name eim-current-package))
          (let ((param (cdr (assoc "param" (car (eim-buffer-list))))))
            (if (assoc "lib" param)
                (load (cadr (assoc "lib" param)))))
          (run-hooks 'eim-load-hook)
          (message nil))
      (error "没有这个文件: %s" word-file)))
  (eim-install-variable)
  (setq input-method-function 'eim-input-method)
  (setq inactivate-current-input-method-function 'eim-inactivate)
  (setq describe-current-input-method-function 'eim-help)
  ;; If we are in minibuffer, turn off the current input method
  ;; before exiting.
  (when (eq (selected-window) (minibuffer-window))
    (add-hook 'minibuffer-exit-hook 'eim-exit-from-minibuffer))
  (run-hooks 'eim-active-hook)
  (if (functionp (eim-active-function))
      (funcall (eim-active-function))))

(defun eim-inactivate ()
  (interactive)
  (mapc 'kill-local-variable eim-local-variable-list))

(defun eim-help (&optional package)
  "Show input method docstring"
  (save-excursion
    (let ((map (eim-mode-map))
          (elt (assoc (eim-package-name) input-method-alist))
          reg desc docstr buf)
      (setq buf (cdr (assoc "buffer" (car (eim-buffer-list)))))
      (set-buffer buf)
      (save-restriction
        (widen)
        (setq reg (condition-case err
                      (eim-section-region "Description")
                    (error nil))
              desc (if reg
                       (buffer-substring-no-properties (car reg) (cdr reg))
                     "")
              docstr (format "Input method: %s (`%s' in mode line) for %s\n  %s\n%s\n\n%s\n"
                             (nth 0 elt) (nth 3 elt) (nth 1 elt) (nth 4 elt)
                             desc
                             (substitute-command-keys "\\{map}")))
        (help-setup-xref (list #'describe-input-method (nth 0 elt))
                         (interactive-p))
        (with-output-to-temp-buffer (help-buffer)
          (princ docstr))))))

;;;_ , page format
(defsubst eim-choice (choice)
  (if (consp choice)
      (car choice)
    choice))

(defun eim-add-completion ()
  "注意, eim-add-completion-function 在没有完补全之前返回 nil, 在加完所
有补全之后一定要返回一个 t"
  (if (functionp eim-add-completion-function)
      (funcall eim-add-completion-function)
    t))

(defun eim-format (key cp tp choice)
  (let ((i 0))
    (format "%s[%d/%d]: %s"
            key  cp tp
            (mapconcat 'identity
                       (mapcar
                        (lambda (c)
                          (format "%d.%s " (setq i (1+ i)) c))
                        choice) " "))))

(defun eim-format-page ()
  "按当前位置，生成候选词条"
  (let ((end (eim-page-end)))
    (if (car eim-current-choices)
        (let* ((start (1- (eim-page-start)))
               (choices (car eim-current-choices))
               (choice (eim-subseq choices start end))
               (pos (1- (min eim-current-pos (length choices))))
               (i 0))
          (setq eim-current-str (eim-choice (nth pos choices)))
          (setq eim-guidance-str
                (funcall eim-format-function eim-current-key (eim-current-page)
                         (eim-total-page) choice))
          ;; (message "%d, %s, %s" pos eim-current-str eim-guidance-str)
          (eim-show))
      (setq eim-current-str eim-current-key)
      (setq eim-guidance-str
            (concat eim-current-key
                    (if (cdr (assoc "completions" (cdr eim-current-choices)))
                        (format "[%s]: "
                                (mapconcat 'identity
                                           (cdr (assoc
                                                 "completions"
                                                 (cdr eim-current-choices)))
                                           "")))))
      (eim-show))))

(defun eim-current-page ()
  (1+ (/ (1- eim-current-pos) eim-page-length)))

(defun eim-total-page ()
  (1+ (/ (1- (length (car eim-current-choices))) eim-page-length)))

(defun eim-page-start ()
  "计算当前所在页的第一个词条的位置"
  (let ((pos (min (length (car eim-current-choices)) eim-current-pos)))
    (1+ (- pos (eim-mod pos eim-page-length)))))

(defun eim-page-end (&optional finish)
  "计算当前所在页的最后一个词条的位置，如果 eim-current-choices 用
完，则检查是否有补全。如果 FINISH 为 non-nil，说明，补全已经用完了"
  (let* ((whole (length (car eim-current-choices)))
         (len eim-page-length)
         (pos eim-current-pos)
         (last (+ (- pos (eim-mod pos len)) len)))
    (if (< last whole)
        last
      (if finish
          whole
        (eim-page-end (eim-add-completion))))))

;;;_ , commands
(defun eim-next-page (arg)
  (interactive "p")
  (if (> (length eim-current-key) 0)
      (let ((new (+ eim-current-pos (* eim-page-length arg) 1)))
        (setq eim-current-pos (if (> new 0) new 1)
              eim-current-pos (eim-page-start))
        (eim-format-page))
    (message "%c" last-command-event)
    (eim-append-string (eim-translate last-command-event))
    (eim-terminate-translation)))

(defun eim-previous-page (arg)
  (interactive "p")
  (eim-next-page (- arg)))

(defun eim-delete-last-char ()
  (interactive)
  (if (> (length eim-current-key) 1)
      (progn
        (setq eim-current-key (substring eim-current-key 0 -1))
        (funcall eim-handle-function))
    (setq eim-current-str "")
    (eim-terminate-translation)))

(defun eim-self-insert-command ()
  "如果在 eim-first-char 列表中，则查找相应的词条，否则停止转换，插入对应的字符"
  (interactive "*")
  ;; (message "%s" (current-buffer))
  (if (if (eim-string-emptyp eim-current-key)
          (member last-command-event eim-first-char)
        (member last-command-event eim-total-char))
      (progn
        (setq eim-current-key (concat eim-current-key (char-to-string last-command-event)))
        (funcall eim-handle-function))
    (eim-append-string (eim-translate last-command-event))
    (eim-terminate-translation)))

(defun eim-select-current ()
  "如果没有可选项，而且是用空格来绑定这个键，就插入空格，否则选择第一
个词条"
  (interactive)
  (if (null (car eim-current-choices))
      (setq eim-current-str
            (if (> (length eim-current-str) 0)
                ""
              (eim-translate last-command-event)))
    (eim-remember-select))
  (eim-terminate-translation))

(defun eim-remember-select (&optional pos)
  (let ((rest (emms-delete-if (lambda (p) (string= (car p) "pos"))
                              (cdr eim-current-choices))))
    (setq rest (append rest (list (cons "pos" (or pos
                                                  eim-current-pos)))))
    (puthash eim-current-key (cons (car eim-current-choices)
                                   rest) (eim-history))))

(defun eim-number-select ()
  "如果没有可选项，插入数字，否则选择对应的词条"
  (interactive)
  (if (car eim-current-choices)
      (let ((index (+ (eim-page-start) (- last-command-event ?2)))
            (end (eim-page-end)))
        (if (>= index end)
            (eim-show)
          (eim-remember-select (1+ index))
          (setq eim-current-str (eim-choice (nth index (car eim-current-choices))))
          (eim-terminate-translation)))
    (eim-append-string (char-to-string last-command-event))
    (eim-terminate-translation)))

(defun eim-quit-clear ()
  (interactive)
  (setq eim-current-str "")
  (eim-terminate-translation))

(defun eim-quit-no-clear ()
  (interactive)
  (setq eim-current-str eim-current-key)
  (eim-terminate-translation))

(defun eim-terminate-translation ()
  "Terminate the translation of the current key."
  (setq eim-translating nil)
  (eim-delete-region)
  (setq eim-current-choices nil)
  (setq eim-guidance-str "")
  (when eim-use-tooltip
    (x-hide-tip)))

;;;_ , eim-handle-string
(defun eim-handle-string ()
  (if (and (functionp eim-stop-function)
           (funcall eim-stop-function))
      (progn
        (setq unread-command-events
              (list (aref eim-current-key (1- (length eim-current-key)))))
        (eim-terminate-translation))
    (setq eim-current-choices (eim-get eim-current-key)
          eim-current-pos
          (if (eim-get-option 'record-position)
              (cdr (assoc "pos" (cdr eim-current-choices)))
            1))
    (eim-format-page)))

(defun eim-translate (char)
  (if (functionp eim-translate-function)
      (funcall eim-translate-function char)
    (char-to-string char)))

;;;_ , Core function of input method (stole from quail)
(defun eim-exit-from-minibuffer ()
  (inactivate-input-method)
  (if (<= (minibuffer-depth) 1)
      (remove-hook 'minibuffer-exit-hook 'quail-exit-from-minibuffer)))

(defun eim-setup-overlays ()
  (let ((pos (point)))
    (if (overlayp eim-overlay)
        (move-overlay eim-overlay pos pos)
      (setq eim-overlay (make-overlay pos pos))
      (if input-method-highlight-flag
          (overlay-put eim-overlay 'face 'eim-string-face)))))

(defun eim-delete-overlays ()
  (if (and (overlayp eim-overlay) (overlay-start eim-overlay))
      (delete-overlay eim-overlay)))

(defun eim-show ()
  (unless enable-multibyte-characters
    (setq eim-current-key nil
          eim-current-str nil)
    (error "Can't input characters in current unibyte buffer"))
  (eim-delete-region)
  (insert eim-current-str)
  (move-overlay eim-overlay (overlay-start eim-overlay) (point))
  ;; Then, show the guidance.
  (when (and (not input-method-use-echo-area)
             (null unread-command-events)
             (null unread-post-input-method-events))
    (if (eq (selected-window) (minibuffer-window))
        ;; Show the guidance in the next line of the currrent
        ;; minibuffer.
        (eim-minibuffer-message
         (format "  [%s]\n%s"
                 current-input-method-title eim-guidance-str))
      ;; Show the guidance in echo area without logging.
      (let ((message-log-max nil))
        (if eim-use-tooltip
            (let ((pos (string-match ": " eim-guidance-str)))
              (if pos
                  (setq eim-guidance-str
                        (concat (substring eim-guidance-str 0 pos)
                                "\n"
                                (make-string (/ (- (string-width eim-guidance-str) pos) 2) (decode-char 'ucs #x2501))
                                "\n"
                                (substring eim-guidance-str (+ pos 2)))))
              (eim-show-tooltip eim-guidance-str))
          (message "%s" eim-guidance-str))))))

(defun eim-make-guidance-frame ()
  "Make a new one-line frame for Quail guidance."
  (let* ((fparam (frame-parameters))
         (top (cdr (assq 'top fparam)))
         (border (cdr (assq 'border-width fparam)))
         (internal-border (cdr (assq 'internal-border-width fparam)))
         (newtop (- top
                    (frame-char-height) (* internal-border 2) (* border 2))))
    (if (< newtop 0)
        (setq newtop (+ top (frame-pixel-height) internal-border border)))
    (make-frame (append '((user-position . t) (height . 1)
                          (minibuffer)
                          (menu-bar-lines . 0) (tool-bar-lines . 0))
                        (cons (cons 'top newtop) fparam)))))

(defun eim-minibuffer-message (string)
  (message nil)
  (let ((point-max (point-max))
        (inhibit-quit t))
    (save-excursion
      (goto-char point-max)
      (insert string))
    (sit-for 1000000)
    (delete-region point-max (point-max))
    (when quit-flag
      (setq quit-flag nil
            unread-command-events '(7)))))

(defun eim-input-method (key)
  (if (or buffer-read-only
          overriding-terminal-local-map
          overriding-local-map)
      (list key)
    ;; (message "call with key: %c" key)
    (eim-setup-overlays)
    (let ((modified-p (buffer-modified-p))
          (buffer-undo-list t)
          (inhibit-modification-hooks t))
      (unwind-protect
          (let ((input-string (eim-start-translation key)))
            ;;   (message "input-string: %s" input-string)
            (setq eim-guidance-str "")
            (when (and (stringp input-string)
                       (> (length input-string) 0))
              (if input-method-exit-on-first-char
                  (list (aref input-string 0))
                (eim-input-string-to-events input-string))))
        (eim-delete-overlays)
        (set-buffer-modified-p modified-p)
        ;; Run this hook only when the current input method doesn't
        ;; require conversion. When conversion is required, the
        ;; conversion function should run this hook at a proper
        ;; timing.
        (run-hooks 'input-method-after-insert-chunk-hook)))))

(defun eim-start-translation (key)
  "Start translation of the typed character KEY by the current Quail package.
Return the input string."
  ;; Check the possibility of translating KEY.
  ;; If KEY is nil, we can anyway start translation.
  (if (or (integerp key) (null key))
      ;; OK, we can start translation.
      (let* ((echo-keystrokes 0)
             (help-char nil)
             (overriding-terminal-local-map (eim-mode-map))
             (generated-events nil)
             (input-method-function nil)
             (modified-p (buffer-modified-p))
             last-command-event last-command this-command)
        (setq eim-current-str ""
              eim-current-key ""
              eim-translating t)
        (if key
            (setq unread-command-events
                  (cons key unread-command-events)))
        (while eim-translating
          (set-buffer-modified-p modified-p)
          (let* ((prompt (if input-method-use-echo-area
                             (format "%s%s %s"
                                     (or input-method-previous-message "")
                                     eim-current-key
                                     eim-guidance-str)))
                 (keyseq (read-key-sequence prompt nil nil t))
                 (cmd (lookup-key (eim-mode-map) keyseq)))
            ;;             (message "key: %s, cmd:%s\nlcmd: %s, lcmdv: %s, tcmd: %s"
            ;;                      key cmd last-command last-command-event this-command)
            (if (if key
                    (commandp cmd)
                  (eq cmd 'eim-self-insert-command))
                (progn
                  ;; (message "keyseq: %s" keyseq)
                  (setq last-command-event (aref keyseq (1- (length keyseq)))
                        last-command this-command
                        this-command cmd)
                  (setq key t)
                  (condition-case err
                      (call-interactively cmd)
                    (error (message "%s" (cdr err)) (beep))))
              ;; KEYSEQ is not defined in the translation keymap.
              ;; Let's return the event(s) to the caller.
              (setq unread-command-events
                    (string-to-list (this-single-command-raw-keys)))
              ;; (message "unread-command-events: %s" unread-command-events)
              (eim-terminate-translation))))
        ;;    (1message "return: %s" eim-current-str)
        eim-current-str)
    ;; Since KEY doesn't start any translation, just return it.
    ;; But translate KEY if necessary.
    (char-to-string key)))

(defun eim-input-string-to-events (str)
  (let ((events (mapcar
                 (lambda (c)
                   ;; This gives us the chance to unify on input
                   ;; (e.g. using ucs-tables.el).
                   (or (and translation-table-for-input
                            (aref translation-table-for-input c))
                       c))
                 str)))
    (if (or (get-text-property 0 'advice str)
            (next-single-property-change 0 'advice str))
        (setq events
              (nconc events (list (list 'eim-advice str)))))
    events))

(defun eim-advice (args)
  (interactive "e")
  (let* ((string (nth 1 args))
         (func (get-text-property 0 'advice string)))
    (if (functionp func)
        (funcall func string))))

(global-set-key [eim-advice] 'eim-advice)

;;; borrow from completion-ui
(defun eim-frame-posn-at-point (&optional position window)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of frame containing WINDOW. Defaults
to the position of point in the selected window."
  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))
  (let ((x-y (posn-x-y (posn-at-point position window)))
        (edges (window-inside-pixel-edges window)))
    (cons (+ (car x-y) (car  edges))
          (+ (cdr x-y) (cadr edges)))))

(defface eim-tooltip-face '((((class color)) :inherit tooltip))
  "face to display items"
  :group 'eim)

(defun eim-show-tooltip (text)
  "Show tooltip text near cursor."
  (let ((pos (eim-frame-posn-at-point))
        (fg (face-attribute 'eim-tooltip-face :foreground nil 'tooltip))
        (bg (face-attribute 'eim-tooltip-face :background nil 'tooltip))
        (params tooltip-frame-parameters)
        ;; seem the top position should add 65 pixel to make
        ;; the text display under the baseline of cursor
        (top-adjust 65)
        (frame-height (frame-pixel-height))
        (frame-width (frame-pixel-width))
        (lines (split-string text "\n"))
        width height left top)
    (setq width (* (frame-char-width) (apply 'max (mapcar 'string-width lines)))
          height (* (frame-char-height) (length lines)))
    (setq left (frame-parameter nil 'left)
          top (frame-parameter nil 'top))
    ;; if the cursor is at near the right frame fringe or at bottom
    ;; of the bottom fringe, move the frame to
    ;; -frame-width or -frame-height from right or bottom
    (if (< (- frame-width (car pos)) width)
        (setq left (+ left (max 0 (- frame-width width))))
      (setq left (+ left (car pos))))
    (if (< (- frame-height (cdr pos)) (+ height top-adjust))
        (setq top (+ top (max 0 (- frame-height height))))
      (setq top (+ top (cdr pos))))
    (setq top (+ top top-adjust))
    (when (stringp fg)
      (setq params (append params `((foreground-color . ,fg)
                                    (border-color . ,fg)))))
    (when (stringp bg)
      (setq params (append params `((background-color . ,bg)))))
    (setq params (append params `((left . ,left) (top . ,top))))
    (x-show-tip (propertize text 'face 'eim-tooltip-face)
                nil params eim-tooltip-timeout)))

;;;_. utils
;;;###autoload
(defun eim-create-word-file ()
  "创建一个能用于 eim 的新文件，按说明填入相应的内容就能生成对应的输入法"
  (interactive)
  (let ((buffer (generate-new-buffer "eim-word")))
    (switch-to-buffer buffer)
    (insert
     "[Comment]\n"
     "要创建一个新的 eim 输入法文件，最简单的方法是只要在 Table 部分填入码表\n"
     "就行了。更多的设置如下：\n"
     "# 控制是否进入转换。一般设置成所有词库中的首字母\n"
     "first-char=\n"
     "# 控制是否退出转换，一般设置成所有词库中的字母\n"
     "total-char=\n"
     "# 在启动时 load 的 elisp 文件\n"
     "lib=\n"
     "# 其它词库文件，用 ; 隔开\n"
     "other-files=\n"
     "# 每页显示的词条数目\n"
     "page-length=\n\n"
     "如果需要加入标点，加入一个 Punctuation 部分。然后设置 eim-translate-fuction。\n"
     "如果需要排序，或者合并相同编码的词条，使用 C-c C-c 或者 M-x eim-build-table。\n"
     "如果有需要，可能还要修改 first-char 和 total-char\n\n"
     "[Parameter]\n"
     "first-char=abcdefghijklmnopqrstuvwxyz\n"
     "total-char=abcdefghijklmnopqrstuvwxyz\n\n"
     "[Description]\n"
     "\n\n"
     "[Table]\n"
     )
    (local-set-key "\C-c\C-c" 'eim-build-table)))

;;;###autoload
(defun eim-build-table ()
  (interactive)
  (save-restriction
    (let ((table (eim-section-region "Table"))
          (param (eim-section-region "Parameter"))
          (lastw "")
          first-char total-char currw)
      (narrow-to-region (car table) (cdr table))
      (perform-replace "[ \t]+$" "" nil t nil nil nil (point-min) (point-max))
      (sort-lines nil (point-min) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at "^[ \t]*$")     ; 如果有空行，删除
            (eim-delete-line)
          (setq currw (eim-code-at-point))
          (add-to-list 'first-char (aref currw 0))
          (mapc (lambda (c) (add-to-list 'total-char c)) (append currw nil))
          (if (string= currw lastw)
              (delete-region (1- (point)) (+ (point) (length currw))))
          (setq lastw currw)
          (forward-line 1)))
      (narrow-to-region (car param) (cdr param))
      (goto-char (point-min))
      (insert "first-char=" (concat first-char) "\n"
              "total-char=" (concat total-char) "\n")
      (while (not (eobp))
        (if (or (looking-at "^first-char=")
                (looking-at "^total-char="))
            (eim-delete-line)
          (forward-line 1)))
      (if (looking-at "^$")
          (delete-backward-char 1)))))

;;;_* eim.el ends here
