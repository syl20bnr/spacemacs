;;; core-funcs.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defun spacemacs/mplist-get (plist prop)
  "Get the values associated to PROP in PLIST, a modified plist.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the first property
and its values is returned.

Currently this function infloops when the list is circular."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(defun spacemacs/mplist-remove (plist prop)
  "Return a copy of a modified PLIST without PROP and its values.

If there are multiple properties with the same keyword, only the first property
and its values are removed."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (push (pop tail) result))
    (when (eq prop (car tail))
      (pop tail)
      (while (and (consp tail) (not (keywordp (car tail))))
        (pop tail)))
    (while (consp tail)
      (push (pop tail) result))
    (nreverse result)))

;; From http://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun spacemacs/dump-vars-to-file (varlist filename)
  "simplistic dumping of variables in VARLIST to a file FILENAME"
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (spacemacs/dump varlist buf)
      (make-directory (file-name-directory filename) t)
      (save-buffer)
      (kill-buffer))))

;; From http://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun spacemacs/dump (varlist buffer)
  "insert into buffer the setq statement to recreate the variables in VARLIST"
  (loop for var in varlist do
        (print (list 'setq var (list 'quote (symbol-value var)))
               buffer)))

(defvar spacemacs--init-redisplay-count 0
  "The number of calls to `redisplay'")
(defun spacemacs//redisplay ()
  "`redisplay' wrapper."
  (setq spacemacs--init-redisplay-count (1+ spacemacs--init-redisplay-count))
  (redisplay))

(defun spacemacs//create-key-binding-form (props func)
  "Helper which returns a from to bind FUNC to a key according to PROPS.

Supported properties:

`:evil-leader STRING'
    One or several key sequence strings to be set with `evil-leader/set-key'.

`:evil-leader-for-mode CONS CELL'
    One or several cons cells (MODE . KEY) where MODE is a major-mode symbol
    and KEY is a key sequence string to be set with
    `evil-leader/set-key-for-mode'.

`:global-key STRING'
    One or several key sequence strings to be set with `global-set-key'.

`:define-key CONS CELL'
    One or several cons cells (MAP . KEY) where MAP is a mode map and KEY is a
    key sequence string to be set with `define-key'. "
  (let ((evil-leader (spacemacs/mplist-get props :evil-leader))
        (evil-leader-for-mode (spacemacs/mplist-get props :evil-leader-for-mode))
        (global-key (spacemacs/mplist-get props :global-key))
        (def-key (spacemacs/mplist-get props :define-key)))
    `((unless (null ',evil-leader)
        (dolist (key ',evil-leader)
          (evil-leader/set-key key ',func)))
      (unless (null ',evil-leader-for-mode)
        (dolist (val ',evil-leader-for-mode)
          (evil-leader/set-key-for-mode
            (car val) (cdr val) ',func)))
      (unless (null ',global-key)
        (dolist (key ',global-key)
          (global-set-key (kbd key) ',func)))
      (unless (null ',def-key)
        (dolist (val ',def-key)
          (define-key (eval (car val)) (kbd (cdr val)) ',func))))))

(defun spacemacs/view-org-file (file &optional anchor-text expand-scope)
  "Open the change log for the current version."
  (interactive)
  (find-file file)
  (org-indent-mode)
  (view-mode)
  (goto-char (point-min))

  (when anchor-text
    (re-search-forward anchor-text))
  (beginning-of-line)

  (cond
   ((eq expand-scope 'subtree)
    (show-subtree))
   ((eq expand-scope 'all)
    (show-all))
   (t nil))

  ;; Make ~SPC ,~ work, reference:
  ;; http://stackoverflow.com/questions/24169333/how-can-i-emphasize-or-verbatim-quote-a-comma-in-org-mode
  (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\n")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  (setq-local org-emphasis-alist '(("*" bold)
                                   ("/" italic)
                                   ("_" underline)
                                   ("=" org-verbatim verbatim)
                                   ("~" org-kbd)
                                   ("+"
                                    (:strike-through t))))

  (setq-local org-hide-emphasis-markers t))

(provide 'core-funcs)
