;;; core-funcs.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar configuration-layer--protected-packages)
(defvar dotspacemacs-filepath)
(defvar spacemacs-repl-list '()
  "List of all registered REPLs.")

(defmacro spacemacs|dotspacemacs-backward-compatibility (variable default)
  "Return `if' sexp for backward compatibility with old dotspacemacs
values."
  `(if (boundp ',variable) ,variable ',default))

(defun spacemacs/system-is-mac ()
  (eq system-type 'darwin))
(defun spacemacs/system-is-linux ()
  (eq system-type 'gnu/linux))
(defun spacemacs/system-is-mswindows ()
  (eq system-type 'windows-nt))

(defun spacemacs/window-system-is-mac ()
  ;; ns is returned instead of mac on Emacs 25+
  (memq (window-system) '(mac ns)))

(defun spacemacs/mplist-get-values (plist prop)
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

(defun spacemacs/mplist-get-value (plist prop)
  "Get a single value associated to PROP in PLIST, a modified plist.

You should always use this function instead of builtin `plist-get'
in Spacemacs.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the first property
and its values is returned.

Currently this function infloops when the list is circular."
  (car (spacemacs/mplist-get-values plist prop)))

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

;; Originally based on http://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun spacemacs/dump-vars-to-file (varlist filename)
  "simplistic dumping of variables in VARLIST to a file FILENAME"
  (with-temp-file filename
    (spacemacs/dump-vars varlist (current-buffer))
    (make-directory (file-name-directory filename) t)))

;; From http://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun spacemacs/dump-vars (varlist buffer)
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
  (let ((evil-leader (spacemacs/mplist-get-values props :evil-leader))
        (evil-leader-for-mode (spacemacs/mplist-get-values props :evil-leader-for-mode))
        (global-key (spacemacs/mplist-get-values props :global-key))
        (def-key (spacemacs/mplist-get-values props :define-key)))
    (append
     (when evil-leader
       `((dolist (key ',evil-leader)
            (spacemacs/set-leader-keys key ',func))))
     (when evil-leader-for-mode
       `((dolist (val ',evil-leader-for-mode)
          (spacemacs/set-leader-keys-for-major-mode
            (car val) (cdr val) ',func))))
     (when global-key
       `((dolist (key ',global-key)
          (global-set-key (kbd key) ',func))))
     (when def-key
       `((dolist (val ',def-key)
          (define-key (eval (car val)) (kbd (cdr val)) ',func)))))))

(defun spacemacs/prettify-org-buffer ()
  "Apply visual enchantments to the current buffer.
The buffer's major mode should be `org-mode'."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "org-mode should be enabled in the current buffer."))
  (if (require 'space-doc nil t)
      (space-doc-mode)
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
                                      (:strike-through t))))))

(defun spacemacs/view-org-file (file &optional anchor-text expand-scope)
  "Open org file and apply visual enchantments.
FILE is the org file to be opened.
If ANCHOR-TEXT  is `nil' then run `re-search-forward' with ^ (beginning-of-line).
If ANCHOR-TEXT is a GitHub style anchor then find a corresponding header.
If ANCHOR-TEXT isn't a GitHub style anchor then run `re-search-forward' with
ANCHOR-TEXT.
If EXPAND-SCOPE is `subtree' then run `outline-show-subtree' at the matched line.
If EXPAND-SCOPE is `all' then run `outline-show-all' at the matched line."
  (interactive)
  (find-file file)
  (spacemacs/prettify-org-buffer)
  (goto-char (point-min))
  (when anchor-text
    ;; If `anchor-text' is GitHub style link.
    (if (string-prefix-p "#" anchor-text)
        ;; If the toc-org package is loaded.
        (if (configuration-layer/package-used-p 'toc-org)
            ;; For each heading. Search the heading that corresponds
            ;; to `anchor-text'.
            (while (and (re-search-forward "^[\\*]+\s\\(.*\\).*$" nil t)
                        (not (string= (toc-org-hrefify-gh (match-string 1))
                                      anchor-text))))
          ;; This is not a problem because without the space-doc package
          ;; those links will be opened in the browser.
          (message (format (concat "Can't follow the GitHub style anchor: '%s' "
                                   "without the org layer.") anchor-text)))
      (re-search-forward anchor-text)))
  (beginning-of-line)
  (cond
   ((eq expand-scope 'subtree)
    (outline-show-subtree))
   ((eq expand-scope 'all)
    (outline-show-all))
   (t nil)))

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

;; https://github.com/syl20bnr/spacemacs/issues/8414
(defun spacemacs/recompile-elpa (arg)
  "Compile or recompile packages in elpa directory, if needed, that is
    if the corresponding .elc file is either missing or outdated.

      If ARG is non-nil, also recompile every `.el' file, regardless of date.

      Useful if you switch Emacs versions."
  (interactive "P")
  ;; First argument must be 0 (not nil) to get missing .elc files rebuilt.
  ;; Bonus: Optionally force recompilation with universal ARG
  (when arg
    (seq-do
     (lambda (fname)
       (when (file-exists-p fname)
         (delete-file fname)))
     (directory-files-recursively user-emacs-directory "\\.elc$" t)))
  (byte-recompile-directory package-user-dir 0 arg))

(defun spacemacs/register-repl (feature repl-func &optional tag)
  "Register REPL-FUNC to the global list of REPLs SPACEMACS-REPL-LIST.
FEATURE will be loaded before running the REPL, in case it is not already
loaded. If TAG is non-nil, it will be used as the string to show in the helm
buffer."
  (push `(,(or tag (symbol-name repl-func))
          . (,feature . ,repl-func))
        spacemacs-repl-list))

;; http://stackoverflow.com/questions/11847547/emacs-regexp-count-occurrences
(defun spacemacs/how-many-str (regexp str)
  (cl-loop with start = 0
        for count from 0
        while (string-match regexp str start)
        do (setq start (match-end 0))
        finally return count))

(defun spacemacs/echo (msg &rest args)
  "Display MSG in echo-area without logging it in *Messages* buffer."
  (interactive)
  (let ((message-log-max nil))
    (apply 'message msg args)))

(defun spacemacs/derived-mode-p (mode &rest modes)
  "Non-nil if MODE is derived from one of MODES."
  ;; We could have copied the built-in `derived-mode-p' and modified it a bit so
  ;; it works on arbitrary modes instead of only the current major-mode. We
  ;; don't do that because then we will need to modify the function if
  ;; `derived-mode-p' changes.
  (let ((major-mode mode))
    (apply #'derived-mode-p modes)))

(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window)))
    ;; if no window is found in the windows history, `switch-to-buffer' will
    ;; default to calling `other-buffer'.
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window)))
     nil t)))

(defun spacemacs/alternate-window ()
  "Switch back and forth between current and last window in the
current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found."))
    (select-window prev-window)))

(defun spacemacs/comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))


;; Generalized next-error system ("gne")

(defun spacemacs/error-delegate ()
  "Decide which error API to delegate to.

Delegates to flycheck if it is enabled and the next-error buffer
is not visible. Otherwise delegates to regular Emacs next-error."
  (if (and (bound-and-true-p flycheck-mode)
           (not next-error-function))
      'flycheck
    'emacs))

(defun spacemacs/next-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (let ((sys (spacemacs/error-delegate)))
    (cond
     ((eq 'flycheck sys) (call-interactively 'flycheck-next-error))
     ((eq 'emacs sys) (call-interactively 'next-error)))))

(defun spacemacs/previous-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (let ((sys (spacemacs/error-delegate)))
    (cond
     ((eq 'flycheck sys) (call-interactively 'flycheck-previous-error))
     ((eq 'emacs sys) (call-interactively 'previous-error)))))

(defvar-local spacemacs--gne-min-line nil
  "The first line in the buffer that is a valid result.")
(defvar-local spacemacs--gne-max-line nil
  "The last line in the buffer that is a valid result.")
(defvar-local spacemacs--gne-cur-line 0
  "The current line in the buffer. (It is problematic to use
point for this.)")
(defvar-local spacemacs--gne-line-func nil
  "The function to call to visit the result on a line.")

(defun spacemacs/gne-next (num reset)
  "A generalized next-error function. This function can be used
as `next-error-function' in any buffer that conforms to the
Spacemacs generalized next-error API.

The variables `spacemacs--gne-min-line',
`spacemacs--gne-max-line', and `spacemacs--line-func' must be
set."
  (when reset (setq spacemacs--gne-cur-line
                    spacemacs--gne-min-line))
  (setq spacemacs--gne-cur-line
        (min spacemacs--gne-max-line
             (max spacemacs--gne-min-line
                  (+ num spacemacs--gne-cur-line))))
  (goto-line spacemacs--gne-cur-line)
  (funcall spacemacs--gne-line-func
           (buffer-substring (point-at-bol) (point-at-eol))))

(provide 'core-funcs)
