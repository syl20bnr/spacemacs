;;; helm-c-yasnippet.el --- helm source for yasnippet.el -*- lexical-binding: t -*-

;; Copyright (C) 2008, 2009, 2010, 2011 Kenji.I (Kenji Imakado) <ken.imakaado@gmail.com>
;; Copyright (C) 2012,2013 Yuhei Maeda <yuhei.maeda_at_gmail.com>
;; Copyright (C) 2014 Yuhei Maeda <yuhei.maeda_at_gmail.com>, Yasuyuki Oka <yasuyk_at_gmail.com>

;; Author: Kenji.I (Kenji Imakado) <ken.imakaado@gmail.com>
;; Version: 0.6.7
;; Package-Requires: ((emacs "25.1") (helm "1.7.7") (yasnippet "0.8.0"))
;; Keywords: convenience, emulation

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Actions: Insert snippet, Open snippet file, Open snippet file other window
;; C-z: execute-persistent-action

;;; Changelog:
;;  2012/08/11   Port to helm
;;               Fixed bug on yasnippet 0.7.0
;;  2012/08/23   Support yasnippet 0.8.0
;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `helm-yas-complete'
;;    List of yasnippet snippets using `helm' interface.
;;  `helm-yas-create-snippet-on-region'
;;    Create a snippet from region.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `helm-yas-not-display-dups'
;;    if non-nil not display duplicate snippet otherwise display all snippet
;;    default = t
;;  `helm-yas-display-msg-after-complete'
;;    if non-nil display snippet key message in minibuffer after Complete
;;    default = t
;;  `helm-yas-space-match-any-greedy'
;;    if non-nil helm pattern space match anyword greedy.
;;    default = nil
;;  `helm-yas-display-key-on-candidate'
;;    if non-nil helm display candidate(snippet name) include key
;;    default = nil

;; here's my yasnippet's configuration
;; (require 'yasnippet)
;; (require 'helm-c-yasnippet)
;; (setq helm-yas-space-match-any-greedy t) ;[default: nil]
;; (global-set-key (kbd "C-c y") 'helm-yas-complete)
;; (yas-global-mode 1)
;; (yas-load-directory "<path>/<to>/snippets/")

;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'helm-files)
(require 'yasnippet)

(defgroup helm-yasnippet nil
  "helm config yasnippet"
  :group 'helm)

(defcustom helm-yas-not-display-dups t
  "if non-nil not display duplicate snippet otherwise display all snippet"
  :type 'boolean
  :group 'helm-yasnippet)

(defcustom helm-yas-display-msg-after-complete t
  "if non-nil display snippet key message in minibuffer after Complete"
  :type 'boolean
  :group 'helm-yasnippet)

(defcustom helm-yas-space-match-any-greedy nil
  "if non-nil helm pattern space match anyword greedy.
pattern regexp: \"if else\" replace to \"if.*else\"
match \"if (...) { ... } else { ... }\" and \"if, elsif, else ...\"
quite convenience
Default: nil"
  :type 'boolean
  :group 'helm-yasnippet)

(defcustom helm-yas-display-key-on-candidate nil
  "if non-nil helm display candidate(snippet name) include key
ex. [for] for (...) { ... }
otherwise display just name
ex. for (...) { ... }"
  :type 'boolean
  :group 'helm-yasnippet)

(defcustom helm-yas-create-new-snippet-insert-function
  'helm-yas-create-new-snippet-insert
  "Function to be called when create new insert file."
  :type 'function
  :group 'helm-yasnippet)

(defface helm-yas-key '((t (:foreground "orange" :underline t)))
  "Face used in helm-yas-complete to show key triggers."
  :group 'helm-yasnippet)

(defvar helm-yas-cur-snippets-alist nil)

(defun helm-yas-create-new-snippet-insert (selected-text snippet-file)
  "Insert SELECTED-TEXT into SNIPPET-FILE."
  (let* ((name (file-name-sans-extension
                (file-name-nondirectory
                 (directory-file-name snippet-file))))
         (string-format "# -*- mode: snippet -*-\n#name : %s\n#key : %s\n#contributor : %s\n# --\n"))
    (insert (format string-format name name user-full-name) selected-text)))

(defun helm-yas-create-new-snippet-file (selected-text snippet-file)
  "Create snippet file with inserted SELECTED-TEXT into SNIPPET-FILE."
  (with-current-buffer (find-file snippet-file)
    (snippet-mode)
    (funcall helm-yas-create-new-snippet-insert-function selected-text snippet-file)))

(defun helm-yas-create-new-snippet (selected-text &optional snippet-file)
  "Create snippet from SELECTED-TEXT into SNIPPET-FILE.
If SNIPPET-FILE is nil, asks file name.
If SNIPPET-FILE does not contain directory, it is placed in default snippet directory."
  (let* ((major-mode-dir (symbol-name major-mode))
         (yas-dir (file-name-as-directory (expand-file-name (or (car-safe yas-snippet-dirs) yas-snippet-dirs))))
         (snippet-dir
          (or (helm-yas-find-recursively major-mode-dir yas-dir 'dir)
              (let ((target-dir (file-name-as-directory (concat yas-dir major-mode-dir))))
                (if (yes-or-no-p (format "%s doesn't exist. Would you like to create this directory?" target-dir))
                    (progn
                      (make-directory target-dir)
                      target-dir)
                  (deactivate-mark)
                  (error "Snippet creation failed"))))))
    (setq snippet-file
          (helm-aif snippet-file
              (expand-file-name snippet-file snippet-dir)
            (read-file-name "create snippet : " snippet-dir snippet-dir)))
    (when (file-exists-p snippet-file)
      (error "can't create file [%s] already exists" (file-name-nondirectory snippet-file)))
    (helm-yas-create-new-snippet-file selected-text snippet-file)))

(defun helm-yas-find-recursively (name &optional directory predicate)
  (let* ((directory (or directory default-directory))
         (predfunc (cl-case predicate
                     (dir 'file-directory-p)
                     (file 'file-regular-p)
                     (otherwise 'identity)))
         (files (cl-remove-if (lambda (s) (string-match "^\\." (file-name-nondirectory  s))) (directory-files directory t)))
         (found nil)
         (result nil))
    (cl-loop for file in files
             unless found
             do (if (and (funcall predfunc file)
                         (string= name (file-name-nondirectory file)))
                    (progn (setq found t)
                           (cl-return (file-name-as-directory file)))
                  (when (file-directory-p file)
                    (setq result (helm-yas-find-recursively name file predicate))))
             finally (cl-return result))))


(defun helm-yas-build-cur-snippets-alist (&optional table)
  (let ((yas-choose-keys-first nil)
        (yas-choose-tables-first nil)
        (yas-buffer-local-condition 'always))
    (let* ((result-alist '((candidates) (transformed) (template-key-alist)
                           (template-file-alist) (template-expand-env-alist)))
           (cur-tables
            (if table
                (list table)
              (yas--get-snippet-tables)))
           (hash-value-alist nil))
      (let ((hashes (cl-loop for table in cur-tables
                             collect (yas--table-hash table))))
        (cl-loop for hash in hashes
                 do (maphash (lambda (k v)
                               (let (a)
                                 (maphash (lambda (_n te)
                                            (setq a (append (list (cons k te)) a)))
                                          v)
                                 (setq hash-value-alist (append a hash-value-alist))))
                             hash))
        (cl-loop with transformed
                 with templates
                 with template-key-alist
                 with template-file-alist
                 with template-expand-env-alist
                 for lst in hash-value-alist
                 for key = (car lst)
                 for template-struct = (cdr lst)
                 for name = (yas--template-name template-struct) ;`yas--template-name'
                 for template = (yas--template-content template-struct) ;`yas--template-content'
                 for file = (yas--template-load-file template-struct) ;`yas--template-content'
                 for condition = (yas--template-condition template-struct)
                 for expand-env = (yas--template-expand-env template-struct)
                 when (or (not condition) (ignore-errors (eval condition)))
                 do (progn (push template templates)
                           (push `(,name . ,template) transformed)
                           (push `(,template . ,key) template-key-alist)
                           (push `(,template . ,file) template-file-alist)
                           (push `(,template . ,expand-env) template-expand-env-alist)
                           )
                 finally (progn (push `(candidates . ,templates) result-alist)
                                (push `(transformed . ,transformed) result-alist)
                                (push `(template-file-alist . ,template-file-alist) result-alist)
                                (push `(template-key-alist . ,template-key-alist) result-alist)
                                (push `(template-expand-env-alist . ,template-expand-env-alist) result-alist)
                                ))
        result-alist)
      )))

(defun helm-yas-get-cmp-context ()
  "Return list (initial-input point-start point-end)
like `yas--current-key'"
  (let ((start (point))
        (end (point))
        (syntax "w_"))
    (condition-case nil
        (save-excursion
          (when mark-active
            (error ""))
          (skip-syntax-backward syntax)
          (setq start (point))
          (cl-values (buffer-substring-no-properties start end) start end))
      (error (cl-values "" (point) (point))))))

(defun helm-yas-get-key-by-template (template alist) ;str template
  "Return key"
  (assoc-default template (assoc-default 'template-key-alist alist)))

(defun helm-yas-get-candidates (alist)
  "Return list of template"
  (assoc-default 'candidates alist 'eq))

(defun helm-yas-get-transformed-list (alist initial-input)
  "Return list of dotlist, (DISPLAY . REAL) DISPLAY is name of snippet, REAL is template of snippet"
  (let ((transformed-list (assoc-default 'transformed alist 'eq)))
    (cond
     ;; display key on candidate ex: [for] for (...) { ... }
     (helm-yas-display-key-on-candidate
      (setq transformed-list (cl-remove-if-not (lambda (lst)
                                                 (string-match (concat "^" (regexp-quote initial-input)) (car lst)))
                                               transformed-list))
      (setq transformed-list (cl-loop for dotlst in transformed-list
                                      for name = (car dotlst)
                                      for template = (cdr dotlst)
                                      for key = (helm-yas-get-key-by-template template alist)
                                      for key-str = (cond ((stringp key) key)
                                                          ((vectorp key) (key-description key))
                                                          (t (format "%s" key)))
                                      for name-inc-key = (concat "[" (propertize key-str 'face 'helm-yas-key) "] " name)
                                      collect `(,name-inc-key . ,template))))

     ;; default ex: for (...) { ... }
     (t
      (setq transformed-list (cl-remove-if-not (lambda (lst)
                                                 (string-match (concat "^" (regexp-quote initial-input)) (car lst)))
                                               transformed-list))))
    (when helm-yas-not-display-dups
      (setq transformed-list (delete-dups transformed-list)))
    ;; sort
    (setq transformed-list (cl-sort transformed-list 'string< :key 'car))
    transformed-list))

(defun helm-yas-find-file-snippet-by-template (template &optional other-window)
  (let* ((path (helm-yas-get-path-by-template template))
         ;;  (let* ((path (assoc-default template (assoc-default 'template-file-alist helm-yas-cur-snippets-alist)))
         (ff-func (if other-window 'find-file-other-window 'find-file)))
    (if path
        (funcall ff-func path)
      (message "not found snippet file"))))

(defun helm-yas-get-path-by-template (template)
  (assoc-default template (assoc-default 'template-file-alist helm-yas-cur-snippets-alist)))

(defun helm-yas-get-expand-env-by-template (template)
  (assoc-default template (assoc-default 'template-expand-env-alist helm-yas-cur-snippets-alist)))

(defun helm-yas-match (candidate)
  "if customize variable `helm-yas-space-match-any-greedy' is non-nil
space match anyword greedy"
  (cond
   (helm-yas-space-match-any-greedy
    (let ((re (replace-regexp-in-string "[ \t]+" ".*" helm-pattern)))
      (string-match re candidate)))
   (t
    (string-match helm-pattern candidate))))

(defvar helm-yas-initial-input "")
(defvar helm-yas-point-start nil)
(defvar helm-yas-point-end nil)
(defvar helm-yas-selected-text "" "region text if mark-active otherwise \"\"")
(defvar helm-source-yasnippet
  `((name . "Yasnippet")
    (init . (lambda ()
              (setq helm-yas-selected-text (if mark-active (buffer-substring-no-properties (region-beginning) (region-end)) ""))
              (cl-multiple-value-setq
               (helm-yas-initial-input helm-yas-point-start helm-yas-point-end) (helm-yas-get-cmp-context)) ;return values(str point point)
              (setq helm-yas-cur-snippets-alist (helm-yas-build-cur-snippets-alist))))
    (candidates . (helm-yas-get-candidates helm-yas-cur-snippets-alist))
    (candidate-transformer . (lambda (candidates)
                               (helm-yas-get-transformed-list helm-yas-cur-snippets-alist helm-yas-initial-input)))
    (action . (("Insert snippet" . (lambda (template)
                                     (yas-expand-snippet template helm-yas-point-start helm-yas-point-end
                                                         (helm-yas-get-expand-env-by-template template))
                                     (when helm-yas-display-msg-after-complete
                                       (message "this snippet is bound to [ %s ]"
                                                (helm-yas-get-key-by-template template helm-yas-cur-snippets-alist)))))
               ("Open snippet file" . (lambda (template)
                                        (helm-yas-find-file-snippet-by-template template)))
               ("Open snippet file other window" . (lambda (template)
                                                     (helm-yas-find-file-snippet-by-template template t)))
               ("Create new snippet on region" . (lambda (template)
                                                   (helm-yas-create-new-snippet helm-yas-selected-text)))
               ("Reload All Snippts" . (lambda (template)
                                         (yas-reload-all)
                                         (message "Reload All Snippts done")))
               ("Rename snippet file" . (lambda (template)
                                          (let* ((path (or (helm-yas-get-path-by-template template) ""))
                                                 (dir (file-name-directory path))
                                                 (filename (file-name-nondirectory path))
                                                 (rename-to (read-string (concat "rename [" filename "] to: "))))
                                            (rename-file path (concat dir rename-to))
                                            (yas-reload-all))))
               ("Delete snippet file" . (lambda (template)
                                          (let ((path (or (helm-yas-get-path-by-template template) "")))
                                            (when (y-or-n-p "really delete?")
                                              (delete-file path)
                                              (yas-reload-all)))))))
    (persistent-action . (lambda (template)
                           (helm-yas-find-file-snippet-by-template template)))
    (match . (helm-yas-match))))


;;; visit template
(defun helm-yas-all-templates ()
  (let ((tables (yas--get-snippet-tables)))
    (cl-loop for table in tables
             append (yas--table-templates table))))

(defun helm-yas-snippet-files-candidates ()
  "called in `helm-source-yasnippet-snippet-files' candidates"
  (let ((yas-choose-keys-first nil)
        (yas-choose-tables-first nil)
        (yas-buffer-local-condition 'always))
    (with-current-buffer helm-current-buffer
      (cl-mapcar 'yas--template-load-file
                 (mapcar 'cdr
                         (helm-yas-all-templates))))))

(defun helm-yas--visit-files-transformer (candidate)
  (file-name-nondirectory candidate))

;; (helm 'helm-source-yasnippet-snippet-files)
(defvar helm-source-yasnippet-snippet-files
  (helm-build-sync-source "yasnippet snippet files"
    :candidates #'helm-yas-snippet-files-candidates
    :real-to-display #'helm-yas--visit-files-transformer
    :action (helm-actions-from-type-file)))


;;; Commands

;;;###autoload
(defun helm-yas-complete ()
  "List of yasnippet snippets using `helm' interface."
  (interactive)
  (helm 'helm-source-yasnippet))

;;;###autoload
(defun helm-yas-visit-snippet-file ()
  "List of yasnippet snippet files"
  (interactive)
  (helm 'helm-source-yasnippet-snippet-files))

;;;###autoload
(defun helm-yas-create-snippet-on-region (&optional start end file-name)
  "Create a snippet from region."
  (interactive "r")
  (let ((str (buffer-substring-no-properties start end)))
    (helm-yas-create-new-snippet str file-name)))
;; (helm-yas-create-snippet-on-region (region-beginning) (region-end) "aaaa")


(define-obsolete-variable-alias 'helm-c-yas-not-display-dups 'helm-yas-not-display-dups "0.6.4")
(define-obsolete-variable-alias 'helm-c-yas-display-msg-after-complete 'helm-yas-display-msg-after-complete "0.6.4")
(define-obsolete-variable-alias 'helm-c-yas-space-match-any-greedy 'helm-yas-space-match-any-greedy "0.6.4")
(define-obsolete-variable-alias 'helm-c-yas-display-key-on-candidate 'helm-yas-display-key-on-candidate "0.6.4")
(define-obsolete-variable-alias 'helm-c-yas-cur-snippets-alist 'helm-yas-cur-snippets-alist "0.6.4")
(define-obsolete-variable-alias 'helm-c-yas-initial-input 'helm-yas-initial-input "0.6.4")
(define-obsolete-variable-alias 'helm-c-yas-point-start 'helm-yas-point-start "0.6.4")
(define-obsolete-variable-alias 'helm-c-yas-point-end 'helm-yas-point-end "0.6.4")
(define-obsolete-variable-alias 'helm-c-yas-selected-text 'helm-yas-selected-text "0.6.4")
(define-obsolete-variable-alias 'helm-c-source-yasnippet 'helm-source-yasnippet "0.6.4")
(define-obsolete-variable-alias 'helm-c-source-yasnippet-snippet-files 'helm-source-yasnippet-snippet-files "0.6.4")

(define-obsolete-function-alias 'helm-c-yas-create-new-snippet 'helm-yas-create-new-snippet "0.6.4")
(define-obsolete-function-alias 'helm-c-yas-find-recursively 'helm-yas-find-recursively "0.6.4")
(define-obsolete-function-alias 'helm-c-yas-build-cur-snippets-alist 'helm-yas-build-cur-snippets-alist "0.6.4")
(define-obsolete-function-alias 'helm-c-yas-get-cmp-context 'helm-yas-get-cmp-context "0.6.4")
(define-obsolete-function-alias 'helm-c-yas-get-key-by-template 'helm-yas-get-key-by-template "0.6.4")
(define-obsolete-function-alias 'helm-c-yas-get-candidates 'helm-yas-get-candidates "0.6.4")
(define-obsolete-function-alias 'helm-c-yas-get-transformed-list 'helm-yas-get-transformed-list "0.6.4")
(define-obsolete-function-alias 'helm-c-yas-find-file-snippet-by-template 'helm-yas-find-file-snippet-by-template "0.6.4")
(define-obsolete-function-alias 'helm-c-yas-get-path-by-template 'helm-yas-get-path-by-template "0.6.4")
(define-obsolete-function-alias 'helm-c-yas-match 'helm-yas-match "0.6.4")
(define-obsolete-function-alias 'helm-c-yas-all-templates 'helm-yas-all-templates "0.6.4")
(define-obsolete-function-alias 'helm-c-yas-snippet-files-candidates 'helm-yas-snippet-files-candidates "0.6.4")
(define-obsolete-function-alias 'helm-c-yas-complete 'helm-yas-complete "0.6.4")
(define-obsolete-function-alias 'helm-c-yas-visit-snippet-file 'helm-yas-visit-snippet-file "0.6.4")
(define-obsolete-function-alias 'helm-c-yas-create-snippet-on-region 'helm-yas-create-snippet-on-region "0.6.4")

(provide 'helm-c-yasnippet)
;;; helm-c-yasnippet.el ends here
