;;; core-funcs.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar configuration-layer--protected-packages)
(defvar dotspacemacs-filepath)

(defun spacemacs/load-or-install-protected-package (pkg &optional log file-to-load)
  "Load PKG package, and protect it against being deleted as an orphan.
See `spacemacs/load-or-install-package' for more information."
  (push pkg configuration-layer--protected-packages)
  (spacemacs/load-or-install-package pkg log file-to-load))

(defun spacemacs/load-or-install-package (pkg &optional log file-to-load)
  "Load PKG package. PKG will be installed if it is not already installed.
Whenever the initial require fails the absolute path to the package
directory is returned.
If LOG is non-nil a message is displayed in spacemacs-buffer-mode buffer.
FILE-TO-LOAD is an explicit file to load after the installation."
  (let ((warning-minimum-level :error))
    (condition-case nil
        (require pkg)
      (error
       ;; not installed, we try to initialize package.el only if required to
       ;; precious seconds during boot time
       (require 'cl)
       (let ((pkg-elpa-dir (spacemacs//get-package-directory pkg)))
         (if pkg-elpa-dir
             (add-to-list 'load-path pkg-elpa-dir)
           ;; install the package
           (when log
             (spacemacs-buffer/append
              (format "(Bootstrap) Installing %s...\n" pkg))
             (spacemacs//redisplay))
           (configuration-layer/retrieve-package-archives 'quiet)
           (package-install pkg)
           (setq pkg-elpa-dir (spacemacs//get-package-directory pkg)))
         (require pkg nil 'noerror)
         (when file-to-load
           (load-file (concat pkg-elpa-dir file-to-load)))
         pkg-elpa-dir)))))

(defun spacemacs//get-package-directory (pkg)
  "Return the directory of PKG. Return nil if not found."
  (let ((elpa-dir (concat user-emacs-directory "elpa/")))
    (when (file-exists-p elpa-dir)
      (let ((dir (cl-reduce (lambda (x y) (if x x y))
                         (mapcar (lambda (x)
                                   (when (string-match
                                          (concat "/"
                                                  (symbol-name pkg)
                                                  "-[0-9]+") x) x))
                                 (directory-files elpa-dir 'full))
                         :initial-value nil)))
        (when dir (file-name-as-directory dir))))))

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
  (cl-loop for var in varlist do
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
    One or several key sequence strings to be set with `spacemacs/set-leader-keys .

`:evil-leader-for-mode CONS CELL'
    One or several cons cells (MODE . KEY) where MODE is a major-mode symbol
    and KEY is a key sequence string to be set with
    `spacemacs/set-leader-keys-for-major-mode'.

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
          (spacemacs/set-leader-keys key ',func)))
      (unless (null ',evil-leader-for-mode)
        (dolist (val ',evil-leader-for-mode)
          (spacemacs/set-leader-keys-for-major-mode
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
    (outline-show-subtree))
   ((eq expand-scope 'all)
    (outline-show-all))
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

(defun spacemacs//test-var (pred var test-desc)
  "Test PRED against VAR and print test result, incrementing
passed-tests and total-tests."
  (let ((var-name (symbol-name var))
        (var-val (symbol-value var)))
    (when (boundp 'total-tests) (setq total-tests (1+ total-tests)))
    (insert (format "** TEST: [[file:%s::%s][%s]] %s\n"
                    dotspacemacs-filepath var-name var-name test-desc))
    (if (funcall pred var-val)
        (progn
          (when (boundp 'passed-tests) (setq passed-tests (1+ passed-tests)))
          (insert (format "*** PASS: %s\n" var-val)))
      (insert (propertize (format "*** FAIL: %s\n" var-val)
                                  'font-lock-face 'font-lock-warning-face)))))

(defun spacemacs//test-list (pred varlist test-desc &optional element-desc)
  "Test PRED against each element of VARLIST and print test
result, incrementing passed-tests and total-tests."
  (let ((varlist-name (symbol-name varlist))
        (varlist-val (symbol-value varlist)))
    (if element-desc
        (insert (format "** TEST: Each %s in [[file:%s::%s][%s]] %s\n"
                        element-desc dotspacemacs-filepath varlist-name
                        varlist-name test-desc))
      (insert (format "** TEST: Each element of [[file:%s::%s][%s]] %s\n"
                      dotspacemacs-filepath varlist-name varlist-name
                      test-desc)))
    (dolist (var varlist-val)
      (when (boundp 'total-tests) (setq total-tests (1+ total-tests)))
      (if (funcall pred var)
          (progn
            (when (boundp 'passed-tests) (setq passed-tests (1+ passed-tests)))
            (insert (format "*** PASS: %s\n" var)))
        (insert (propertize (format "*** FAIL: %s\n" var) 'font-lock-face 'font-lock-warning-face))))))

;; hide mode line
;; from http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)
(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(defun spacemacs/recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch
Emacs versions."
  (interactive)
  (byte-recompile-directory package-user-dir nil t))

(provide 'core-funcs)

