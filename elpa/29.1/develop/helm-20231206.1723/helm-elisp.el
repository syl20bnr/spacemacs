;;; helm-elisp.el --- Elisp symbols completion for helm. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2023 Thierry Volpiatto

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'helm-lib)
(require 'helm-help)
(require 'helm-types)
(require 'helm-utils)
(require 'helm-info)
(require 'helm-eval)
(require 'helm-files)

(declare-function helm-describe-function "helm-lib")
(declare-function helm-describe-variable "helm-lib")
(declare-function helm-describe-face "helm-lib")
(declare-function helm-read-file-name "helm-mode")
(declare-function helm-comp-read "helm-mode")
(declare-function helm-M-x-transformer-no-sort-no-props "helm-command")
(defvar helm-M-x-show-short-doc)
(defvar helm-completions-detailed)


;;; Customizable values

(defgroup helm-elisp nil
  "Elisp related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-turn-on-show-completion t
  "Display candidate in `current-buffer' while moving selection when non--nil."
  :group 'helm-elisp
  :type  'boolean)

(defcustom helm-show-completion-min-window-height 7
  "Minimum completion window height used in show completion.
This is used in macro `with-helm-show-completion'."
  :group 'helm-elisp
  :type  'integer)

(defcustom helm-lisp-quoted-function-list
  '(funcall apply mapc cl-mapc mapcar cl-mapcar
    callf callf2 cl-callf cl-callf2 fset
    fboundp fmakunbound symbol-function)
  "List of function where quoted function completion happen.
E.g. give only function names after (funcall \\='."
  :group 'helm-elisp
  :type '(repeat (choice symbol)))

(defcustom helm-lisp-unquoted-function-list
  '(function defadvice)
  "List of function where unquoted function completion happen.
E.g. give only function names after (function ."
  :group 'helm-elisp
  :type '(repeat (choice symbol)))

(defcustom helm-apropos-fuzzy-match nil
  "Enable fuzzy matching for `helm-apropos' when non-nil."
  :group 'helm-elisp
  :type 'boolean)

(defcustom helm-lisp-fuzzy-completion nil
  "Enable fuzzy matching in emacs-lisp completion when non-nil.
NOTE: This enables fuzzy matching in Helm native implementation of
elisp completion, but not on helmized elisp completion, i.e. fuzzy
completion is not available in `completion-at-point'."
  :group 'helm-elisp
  :type 'boolean)

(defcustom helm-apropos-function-list '(helm-def-source--emacs-commands
                                        helm-def-source--emacs-functions
                                        helm-def-source--eieio-classes
                                        helm-def-source--eieio-generic
                                        helm-def-source--emacs-variables
                                        helm-def-source--emacs-faces)
  "A list of functions that build helm sources to use in `helm-apropos'."
  :group 'helm-elisp
  :type '(repeat (choice symbol)))

(defcustom helm-apropos-defaut-info-lookup-sources '(helm-source-info-elisp
                                                     helm-source-info-cl
                                                     helm-source-info-eieio)
  "A list of sources to look into when searching info page of a symbol."
  :group 'helm-elisp
  :type '(repeat (choice symbol)))

(defcustom helm-show-completion-display-function
  (if (display-graphic-p)
      #'helm-display-buffer-in-own-frame
    #'helm-show-completion-default-display-function)
  "The function used to display helm completion buffer.

This function is used by `with-helm-show-completion', when nil
fallback to `helm-default-display-buffer'.

Default is to use a separate frame on graphic display and
`helm-show-completion-default-display-function' on non graphic
display."
  :group 'helm-elisp
  :type 'function)

;;; Faces
;;
;;
(defgroup helm-elisp-faces nil
  "Customize the appearance of helm-elisp."
  :prefix "helm-"
  :group 'helm-elisp
  :group 'helm-faces)

(defface helm-lisp-show-completion
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :background "DarkSlateGray"))
  "Face used for showing candidates in `helm-lisp-completion'."
  :group 'helm-elisp-faces)

(defface helm-lisp-completion-info
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "red"))
  "Face used for showing info in `helm-lisp-completion'."
  :group 'helm-elisp-faces)

(defcustom helm-elisp-help-function
  'helm-elisp-show-help
  "Function for displaying help for Lisp symbols."
  :group 'helm-elisp
  :type '(choice (function :tag "Open help for the symbol."
                  helm-elisp-show-help)
                 (function :tag "Show one liner in modeline."
                  helm-elisp-show-doc-modeline)))

(defcustom helm-locate-library-fuzzy-match t
  "Enable fuzzy-matching in `helm-locate-library' when non--nil."
  :type 'boolean
  :group 'helm-elisp)


;;; Show completion.
;;
;; Provide show completion with macro `with-helm-show-completion'.

(defvar helm-show-completion-overlay nil)

;; Called each time cursor move in helm-buffer.
(defun helm-show-completion ()
  (with-helm-current-buffer
    (helm-aif (helm-get-selection)
        (overlay-put helm-show-completion-overlay
                     'display (substring-no-properties it)))))

(defun helm-show-completion-init-overlay (beg end)
  (setq helm-show-completion-overlay (make-overlay beg end))
  (overlay-put helm-show-completion-overlay
               'face 'helm-lisp-show-completion))

(defun helm-show-completion-default-display-function (buffer &rest _args)
  "A special resized Helm window is used depending on position in BUFFER."
  (with-selected-window (selected-window)
    (if (window-dedicated-p)
        (helm-default-display-buffer buffer)
      (let* ((screen-size  (+ (count-screen-lines (window-start) (point) t)
                              1                         ; mode-line
                              (if header-line-format 1 0))) ; header-line
             (def-size     (- (window-height)
                              helm-show-completion-min-window-height))
             (upper-height (max window-min-height (min screen-size def-size)))
             split-window-keep-point)
        (recenter -1)
        (set-window-buffer (if (active-minibuffer-window)
                               (minibuffer-selected-window)
                               (split-window nil upper-height
                                             helm-split-window-default-side))
                           buffer)))))

(defmacro with-helm-show-completion (beg end &rest body)
  "Show Helm candidate in an overlay at point.
BEG and END are the beginning and end position of the current
completion in `helm-current-buffer'.
BODY is an Helm call where we want to enable show completion.
If `helm-turn-on-show-completion' is nil do nothing."
  (declare (indent 2) (debug t))
  `(unwind-protect
        (if helm-turn-on-show-completion
            (let ((helm-move-selection-after-hook
                   (append (list 'helm-show-completion)
                           helm-move-selection-after-hook))
                  (helm-split-window-default-side
                   (if (eq helm-split-window-default-side 'same)
                       'below helm-split-window-default-side))
                  helm-split-window-inside-p
                  helm-reuse-last-window-split-state)
              (helm-set-local-variable
               'helm-display-function
               (or helm-show-completion-display-function
                   'helm-default-display-buffer))
              (with-helm-after-update-hook
                ;; Show immediately first candidate as soon as helm popup.
                (helm-show-completion))
              (helm-show-completion-init-overlay ,beg ,end)
              ,@body)
          ,@body)
     (when (and helm-show-completion-overlay
                (overlayp helm-show-completion-overlay))
       (delete-overlay helm-show-completion-overlay))))


;;; Lisp symbol completion.
;;
;;
(defun helm-lisp-completion--predicate-at-point (beg)
  ;; Return a predicate for `all-completions'.
  (let ((fn-sym-p (lambda ()
                    (or
                     (and (eq (char-before) ?\ )
                          (save-excursion
                            (skip-syntax-backward " " (pos-bol))
                            (memq (symbol-at-point)
                                  helm-lisp-unquoted-function-list)))
                     (and (eq (char-before) ?\')
                          (save-excursion
                            (forward-char -1)
                            (eq (char-before) ?\#)))))))
    (save-excursion
      (goto-char beg)
      (if (or
           ;; Complete on all symbols in non--lisp modes (logs mail etc..)
           (not (memq major-mode '(emacs-lisp-mode
                                   lisp-interaction-mode
                                   inferior-emacs-lisp-mode)))
           (not (or (funcall fn-sym-p)
                    (and (eq (char-before) ?\')
                         (save-excursion
                           (forward-char (if (funcall fn-sym-p) -2 -1))
                           (skip-syntax-backward " " (pos-bol))
                           (memq (symbol-at-point)
                                 helm-lisp-quoted-function-list)))
                    (eq (char-before) ?\())) ; no paren before str.
           ;; Looks like we are in a let statement.
           (condition-case nil
               (progn (up-list -2) (forward-char 1)
                      (eq (char-after) ?\())
             (error nil)))
          (lambda (sym)
            (or (boundp sym) (fboundp sym) (symbol-plist sym)))
        #'fboundp))))

(defun helm-thing-before-point (&optional limits regexp)
  "Return symbol name before point.
If REGEXP is specified return what REGEXP find before point.
By default match the beginning of symbol before point.
With LIMITS arg specified return the beginning and end position
of symbol before point."
  (save-excursion
    (let (beg
          (end (point))
          (boundary (field-beginning nil nil (pos-bol))))
      (if (re-search-backward (or regexp "\\_<") boundary t)
          (setq beg (match-end 0))
        (setq beg boundary))
      (unless (= beg end)
        (if limits
            (cons beg end)
          (buffer-substring-no-properties beg end))))))

(defun helm-bounds-of-thing-before-point (&optional regexp)
  "Get the beginning and end position of `helm-thing-before-point'.
Return a cons (beg . end)."
  (helm-thing-before-point 'limits regexp))

(defun helm-insert-completion-at-point (beg end str)
  ;; When there is no space after point
  ;; we are completing inside a symbol or
  ;; after a partial symbol with the next arg aside
  ;; without space, in this case mark the region.
  ;; deleting it would remove the
  ;; next arg which is unwanted.
  (delete-region beg end)
  (insert str)
  (let ((pos (cdr (or (bounds-of-thing-at-point 'symbol)
                      ;; needed for helm-dabbrev.
                      (bounds-of-thing-at-point 'filename)))))
    (when (and pos (< (point) pos))
      (push-mark pos t t))))

(defvar helm-lisp-completion--cache nil)
(defvar helm-lgst-len nil)
;;;###autoload
(defun helm-lisp-completion-at-point ()
  "Preconfigured Helm for Lisp symbol completion at point."
  (interactive)
  (setq helm-lgst-len 0)
  (let* ((target     (helm-thing-before-point))
         (beg        (car (helm-bounds-of-thing-before-point)))
         (end        (point))
         (pred       (and beg (helm-lisp-completion--predicate-at-point beg)))
         (loc-vars   (and (fboundp 'elisp--local-variables)
                          (ignore-errors
                            (mapcar #'symbol-name (elisp--local-variables)))))
         (glob-syms  (and target pred (all-completions target obarray pred)))
         (candidates (append loc-vars glob-syms))
         (helm-quit-if-no-candidate t)
         (helm-execute-action-at-once-if-one t)
         (enable-recursive-minibuffers t))
    (setq helm-lisp-completion--cache (cl-loop for sym in candidates
                                            for len = (length sym)
                                            when (> len helm-lgst-len)
                                            do (setq helm-lgst-len len)
                                            collect sym))
    (if candidates
        (with-helm-show-completion beg end
          ;; Overlay is initialized now in helm-current-buffer.
          (helm
           :sources (helm-build-in-buffer-source "Lisp completion"
                      :data helm-lisp-completion--cache
                      :persistent-action `(helm-lisp-completion-persistent-action .
                                           ,(and (eq helm-elisp-help-function
                                                     'helm-elisp-show-doc-modeline)
                                                 'never-split))
                      :nomark t
                      :match-part (lambda (c) (car (split-string c)))
                      :fuzzy-match helm-lisp-fuzzy-completion
                      :persistent-help (helm-lisp-completion-persistent-help)
                      :filtered-candidate-transformer
                      #'helm-lisp-completion-transformer
                      :action (lambda (candidate)
                                (with-helm-current-buffer
                                  (run-with-timer
                                   0.01 nil
                                   #'helm-insert-completion-at-point
                                   beg end candidate))))
           :input (if helm-lisp-fuzzy-completion
                      target (concat target " "))
           :resume 'noresume
           :truncate-lines t
           :buffer "*helm lisp completion*"
           :allow-nest t))
      (message "[No Match]"))))

(defun helm-lisp-completion-persistent-action (candidate &optional name)
  "Show documentation for the function.
Documentation is shown briefly in mode-line or completely in
other window according to the value of
`helm-elisp-help-function'."
  (funcall helm-elisp-help-function candidate name))

(defun helm-lisp-completion-persistent-help ()
  "Return persistent-help according to the value of `helm-elisp-help-function'"
    (cl-ecase helm-elisp-help-function
      (helm-elisp-show-doc-modeline "Show brief doc in mode-line")
      (helm-elisp-show-help "Toggle show help for the symbol")))

(defun helm-elisp--show-help-1 (candidate &optional name)
  (let ((sym (intern-soft candidate)))
    (pcase sym
      ((and (pred fboundp) (pred boundp))
       (if (member name `(,helm-describe-function-function ,helm-describe-variable-function))
           (funcall (intern (format "helm-%s" name)) sym)
           ;; When there is no way to know what to describe
           ;; prefer describe-function.
           (helm-describe-function sym)))
      ((pred fboundp)  (helm-describe-function sym))
      ((pred boundp)    (helm-describe-variable sym))
      ((pred facep)     (helm-describe-face sym)))))

(defun helm-elisp-show-help (candidate &optional name)
  "Show full help for the function CANDIDATE.
Arg NAME specifies the name of the top level function calling
Helm generic completion (e.g., \"describe-function\") which
allows calling the right function when CANDIDATE symbol refers at
the same time to variable and a function."
  (helm-elisp--persistent-help
   candidate 'helm-elisp--show-help-1 name))

(defun helm-elisp-show-doc-modeline (candidate &optional name)
  "Show brief documentation for the function in the mode-line."
  (let ((cursor-in-echo-area t)
        mode-line-in-non-selected-windows)
    (helm-show-info-in-mode-line
     (propertize
      (helm-get-first-line-documentation
       (intern candidate) name)
      'face 'helm-lisp-completion-info))))

(defun helm-lisp-completion-transformer (candidates _source)
  "Helm candidates transformer for Lisp completion."
  (cl-loop for c in candidates
        for sym = (intern c)
        for annot = (pcase sym
                      ((pred commandp) " (Com)")
                      ((pred class-p)   " (Class)")
                      ((pred cl-generic-p) " (Gen)")
                      ((pred fboundp)  " (Fun)")
                      ((pred boundp)   " (Var)")
                      ((pred facep)    " (Face)"))
        for spaces = (make-string (- helm-lgst-len (length c)) ? )
        collect (cons (concat c spaces annot) c) into lst
        finally return (sort lst #'helm-generic-sort-fn)))

;;;###autoload
(cl-defun helm-get-first-line-documentation (sym &optional
                                                   (name "describe-function")
                                                   (end-column 72))
  "Return first line documentation of symbol SYM truncated at END-COLUMN.
If SYM is not documented, return \"Not documented\".
Argument NAME allows specifiying what function to use to display
documentation when SYM name is the same for function and variable."
  (let ((doc (condition-case _err
                 (pcase sym
                   ((pred class-p) (cl--class-docstring (cl--find-class sym)))
                   ((and (pred fboundp) (pred boundp))
                    (pcase name
                      ("describe-function"
                       (documentation sym t))
                      ("describe-variable"
                       (documentation-property sym 'variable-documentation t))
                      (_ (documentation sym t))))
                   ((pred custom-theme-p)
                    (documentation-property sym 'theme-documentation t))
                   ((pred helm-group-p) (documentation-property
                                         sym 'group-documentation t))
                   ((pred fboundp)  (documentation sym t))
                   ((pred boundp)   (documentation-property
                                     sym 'variable-documentation t))
                   ((pred facep)   (face-documentation sym)))
               (void-function "Void function -- Not documented"))))
    (if (and doc (not (string= doc ""))
             ;; `documentation' return "\n\n(args...)"
             ;; for CL-style functions.
             (not (string-match-p "\\`\n\n" doc)))
        ;; Some commands specify key bindings in their first line.
        (truncate-string-to-width
         (substitute-command-keys (car (split-string doc "\n")))
         end-column nil nil t)
      (if (or (symbol-function sym) (boundp sym) (facep sym) (helm-group-p sym))
          "Not documented"
        ;; Symbol exist but has no definition yet e.g.
        ;; (advice-add 'foo-test :override (lambda () (message "invalid
        ;; function"))) and foo-test is not already defined.
        "Not already defined or loaded"))))

;;; File completion.
;;
;; Complete file name at point.

;;;###autoload
(defun helm-complete-file-name-at-point (&optional force)
  "Preconfigured Helm to complete file name at point."
  (interactive)
  (require 'helm-mode)
  (let* ((tap (or (thing-at-point 'filename t) ""))
         beg
         (init (and tap
                    (or force
                        (save-excursion
                          (end-of-line)
                          (search-backward tap (pos-bol) t)
                          (setq beg (point))
                          (looking-back "[^'`( ]" (1- (point)))))
                    (expand-file-name tap)))
         (end  (point))
         (helm-quit-if-no-candidate t)
         (helm-execute-action-at-once-if-one t)
         completion)
    (with-helm-show-completion beg end
      (setq completion (helm-read-file-name "FileName: "
                                            :initial-input init)))
    (when (and completion (not (string= completion "")))
      (delete-region beg end) (insert (if (string-match "^~" tap)
                                          (abbreviate-file-name completion)
                                        completion)))))
(make-obsolete 'helm-complete-file-name-at-point 'helm-find-files "3.9.6")

;;;###autoload
(defun helm-lisp-indent ()
  ;; It is meant to use with `helm-define-multi-key' which
  ;; does not support args for functions yet, so use `current-prefix-arg'
  ;; for now instead of (interactive "P").
  (interactive)
  (let ((tab-always-indent (or (eq tab-always-indent 'complete)
                               tab-always-indent)))
    (indent-for-tab-command current-prefix-arg)))


;;; Apropos
;;
;;
(defvar helm-apropos-history nil)

(defcustom helm-apropos-show-short-doc nil
  "Show short docstring of symbols when non nil.

NOTE: When displaying helm-apropos in a frame, i.e. when
`helm-apropos' is member of `helm-commands-using-frame' setting this
to non nil have no effect, you have first to remove `helm-apropos'
from `helm-commands-using-frame'."
  :group 'helm-elisp
  :type 'boolean)

(defvar helm-apropos-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-]") #'helm-apropos-toggle-details)
    map))

(defun helm-apropos-init (test default &optional fn)
  "Setup `helm-candidate-buffer' for `helm-apropos' sources.
A list of symbols fetched with FN is inserted in
`helm-candidate-buffer', if FN is not provided symbols are fetched
against obarray with predicate TEST. When FN is provided predicate TEST
is only used to test DEFAULT."
  (require 'helm-help)
  (helm-init-candidates-in-buffer 'global
    (let ((default-symbol (and (stringp default)
                               (intern-soft default)))
          (symbols (if fn (funcall fn) (all-completions "" obarray test))))
      (if (and default-symbol (funcall test default-symbol))
          (cons default-symbol symbols)
        symbols))))

(defun helm-apropos-short-doc-transformer (candidates _source)
  (if helm-apropos-show-short-doc
      (cl-loop with max-len = (helm-in-buffer-get-longest-candidate)
               for cand in candidates
               for doc = (helm-get-first-line-documentation (intern-soft cand))
               collect (cons (format "%s%s%s"
                                     cand
                                     (if doc
                                         (make-string (+ 1 (if (zerop max-len)
                                                               max-len
                                                             (- max-len (string-width cand))))
                                                      ? )
                                       "")
                                     (if doc (propertize doc 'face 'helm-M-x-short-doc) ""))
                             cand))
    candidates))

(defun helm-apropos-default-sort-fn (candidates _source)
  (if (string= helm-pattern "")
      candidates
      (sort candidates #'helm-generic-sort-fn)))

(defun helm-apropos-clean-history-variable (candidate)
  (with-helm-current-buffer ; var is maybe local
    (let* ((sym   (intern-soft candidate))
           (cands (symbol-value sym))
           (mkds  (and (listp cands)
                       (helm-comp-read "Delete entry: "
                                       cands :marked-candidates t))))
      (cl-assert (listp mkds) nil "Variable value is not a list")
      (cl-loop for elm in mkds do
               (if (local-variable-p sym)
                   (set (make-local-variable sym)
                        (setq cands (delete elm cands)))
                 (set sym (setq cands (delete elm cands))))))))

(defun helm-apropos-clean-ring (candidate)
  (with-helm-current-buffer ; var is maybe local
    (let* ((sym   (intern-soft candidate))
           (val   (symbol-value sym))
           (cands (and (ring-p val) (ring-elements val)))
           (mkds  (and cands (helm-comp-read
                                "Delete entry: "
                                cands :marked-candidates t))))
      (when mkds
        (cl-loop for elm in mkds do
                 (ring-remove
                  val (helm-position
                       elm
                       (ring-elements val)
                       :test 'equal))
                 and do (if (local-variable-p sym)
                            (set (make-local-variable sym) val)
                          (set sym val)))))))

(defun helm-apropos-action-transformer (actions candidate)
  (let* ((sym (helm-symbolify candidate))
         (val (with-helm-current-buffer (symbol-value sym))))
    (cond ((custom-variable-p sym)
           (append
            actions
            (let ((standard-value (eval (car (get sym 'standard-value)) t)))
              (unless (equal standard-value (symbol-value sym))
                `(("Reset Variable to default value"
                   . ,(lambda (candidate)
                        (let ((sym (helm-symbolify candidate)))
                          (set sym standard-value)))))))
            '(("Customize variable" .
               (lambda (candidate)
                 (customize-option (helm-symbolify candidate)))))))
          ((and val (with-helm-current-buffer (ring-p (symbol-value sym))))
           (append actions
                   '(("Clean ring" . helm-apropos-clean-ring))))
          ((and (string-match-p "history" candidate) (listp val))
           (append actions
                   '(("Clean variable" .
                      helm-apropos-clean-history-variable))))
          (t actions))))

(defun helm-def-source--emacs-variables (&optional default)
  (helm-build-in-buffer-source "Variables"
    :init (lambda ()
            (helm-apropos-init
             (lambda (x)
               (and (boundp x) (not (keywordp x)) (not (class-p x))))
             default))
    :fuzzy-match helm-apropos-fuzzy-match
    :filtered-candidate-transformer
    (delq nil (list (and (null helm-apropos-fuzzy-match)
                         'helm-apropos-default-sort-fn)
                    (and (null (memq 'helm-apropos helm-commands-using-frame))
                         #'helm-apropos-short-doc-transformer)))
    :nomark t
    :persistent-action (lambda (candidate)
                         (helm-elisp--persistent-help
                          candidate 'helm-describe-variable))
    :persistent-help "Toggle describe variable"
    :keymap helm-apropos-map
    :action '(("Describe variable" . helm-describe-variable)
              ("Find variable" . helm-find-variable)
              ("Info lookup" . helm-info-lookup-symbol)
              ("Set variable" . helm-set-variable))
    :action-transformer 'helm-apropos-action-transformer))

(defun helm-def-source--emacs-faces (&optional default)
  "Create `helm' source for faces to be displayed with
`helm-apropos'."
  (helm-build-in-buffer-source "Faces"
    :init (lambda () (helm-apropos-init 'facep default #'face-list))
    :fuzzy-match helm-apropos-fuzzy-match
    :filtered-candidate-transformer
    (delq nil (list
               (and (null helm-apropos-fuzzy-match)
                    #'helm-apropos-default-sort-fn)
               (lambda (candidates _source)
                 (cl-loop for c in candidates
                          collect (propertize c 'face (intern c))))
               (and (null (memq 'helm-apropos helm-commands-using-frame))
                    #'helm-apropos-short-doc-transformer)))
    :persistent-action (lambda (candidate)
                         (helm-elisp--persistent-help
                          candidate 'helm-describe-face))
    :persistent-help "Toggle describe face"
    :keymap helm-apropos-map
    :action '(("Describe face" . helm-describe-face)
              ("Find face" . helm-find-face-definition)
              ("Customize face" . (lambda (candidate)
                                    (customize-face (helm-symbolify candidate)))))))

(defun helm-def-source--emacs-commands (&optional default)
  (require 'helm-command)
  (helm-build-in-buffer-source "Commands"
    :init (lambda ()
            (helm-apropos-init 'commandp default))
    :fuzzy-match helm-apropos-fuzzy-match
    :filtered-candidate-transformer
    (append (list #'helm-M-x-transformer-no-sort-no-props)
            (and (null helm-apropos-fuzzy-match)
                 '(helm-apropos-default-sort-fn)))
    :display-to-real 'helm-symbolify
    :nomark t
    :persistent-action (lambda (candidate)
                         (helm-elisp--persistent-help
                          candidate 'helm-describe-function))
    :persistent-help "Toggle describe command"
    :keymap helm-apropos-map
    :action 'helm-type-function-actions))

(defun helm-def-source--emacs-functions (&optional default)
  (helm-build-in-buffer-source "Functions"
    :init (lambda ()
            (helm-apropos-init (lambda (x)
                                 (and (fboundp x)
                                      (not (commandp x))
                                      (not (cl-generic-p x))
                                      (not (class-p x))))
                               default))
    :fuzzy-match helm-apropos-fuzzy-match
    :filtered-candidate-transformer
    (delq nil (list (and (null helm-apropos-fuzzy-match)
                         'helm-apropos-default-sort-fn)
                    (and (null (memq 'helm-apropos helm-commands-using-frame))
                         #'helm-apropos-short-doc-transformer)))
    :display-to-real 'helm-symbolify
    :persistent-action (lambda (candidate)
                         (helm-elisp--persistent-help
                          candidate 'helm-describe-function))
    :persistent-help "Toggle describe function"
    :keymap helm-apropos-map
    :nomark t
    :action 'helm-type-function-actions))

(defun helm-def-source--eieio-classes (&optional default)
  (helm-build-in-buffer-source "Classes"
    :init (lambda ()
            (helm-apropos-init (lambda (x)
                                 (class-p x))
                               default))
    :fuzzy-match helm-apropos-fuzzy-match
    :filtered-candidate-transformer
    (delq nil (list (and (null helm-apropos-fuzzy-match)
                         'helm-apropos-default-sort-fn)
                    (and (null (memq 'helm-apropos helm-commands-using-frame))
                         #'helm-apropos-short-doc-transformer)))
    :nomark t
    :persistent-action (lambda (candidate)
                         (helm-elisp--persistent-help
                          candidate 'helm-describe-class))
    :persistent-help "Toggle describe class"
    :keymap helm-apropos-map
    :action '(("Describe Class" . helm-describe-class)
              ("Find Class (C-u for source)" . helm-find-function)
              ("Info lookup" . helm-info-lookup-symbol))))

(defun helm-def-source--eieio-generic (&optional default)
  (helm-build-in-buffer-source "Generic functions"
    :init (lambda ()
            (helm-apropos-init (lambda (x)
                                 (cl-generic-p x))
                               default))
    :fuzzy-match helm-apropos-fuzzy-match
    :filtered-candidate-transformer
    (delq nil (list (and (null helm-apropos-fuzzy-match)
                         'helm-apropos-default-sort-fn)
                    (and (null (memq 'helm-apropos helm-commands-using-frame))
                         #'helm-apropos-short-doc-transformer)))
    :nomark t
    :persistent-action (lambda (candidate)
                         (helm-elisp--persistent-help
                          candidate 'helm-describe-function))
    :persistent-help "Toggle describe generic function"
    :keymap helm-apropos-map
    :action '(("Describe function" . helm-describe-function)
              ("Find function (C-u for source)" . helm-find-function)
              ("Info lookup" . helm-info-lookup-symbol))))

(defun helm-info-lookup-fallback-source (candidate)
  (let ((sym (helm-symbolify candidate))
        src-name fn)
    (cond ((class-p sym)
           (setq fn #'helm-describe-function
                 src-name "Describe class"))
          ((cl-generic-p sym)
           (setq fn #'helm-describe-function
                 src-name "Describe generic function"))
          ((fboundp sym)
           (setq fn #'helm-describe-function
                 src-name "Describe function"))
          ((facep sym)
           (setq fn #'helm-describe-face
                 src-name "Describe face"))
          (t
           (setq fn #'helm-describe-variable
                 src-name "Describe variable")))
    (helm-build-sync-source src-name
      :candidates (list candidate)
      :persistent-action (lambda (candidate)
                           (helm-elisp--persistent-help
                            candidate fn))
      :persistent-help src-name
      :nomark t
      :action fn)))

(defun helm-info-lookup-symbol-1 (c)
  (let ((helm-execute-action-at-once-if-one 'current-source))
    (helm :sources (append helm-apropos-defaut-info-lookup-sources
                           (list (helm-info-lookup-fallback-source c)))
          :resume 'noresume
          :buffer "*helm lookup*"
          :input (helm-stringify c))))

(defun helm-info-lookup-symbol (candidate)
  ;; ???:Running an idle-timer allows not catching RET when exiting
  ;; with the fallback source.
  ;; (run-with-idle-timer 0.01 nil #'helm-info-lookup-symbol-1 candidate)
  (helm-info-lookup-symbol-1 candidate))

(defun helm-apropos-toggle-details ()
  "Toggle details in `helm-apropos'."
  (interactive)
  (with-helm-buffer
    (unless (memq 'helm-apropos helm-commands-using-frame)
      (setq helm-M-x-show-short-doc (not helm-M-x-show-short-doc)
            helm-apropos-show-short-doc (not helm-apropos-show-short-doc))
      (helm-force-update (concat "^" (helm-stringify (helm-get-selection)))
                         (helm-get-current-source)))))

(defun helm-apropos-get-default ()
  (with-syntax-table emacs-lisp-mode-syntax-table
    (symbol-name (intern-soft (thing-at-point 'symbol)))))

;;;###autoload
(defun helm-apropos (default)
  "Preconfigured Helm to describe commands, functions, variables and faces.
In non interactives calls DEFAULT argument should be provided as
a string, i.e. the `symbol-name' of any existing symbol."
  (interactive (list (helm-apropos-get-default)))
  (let ((helm-M-x-show-short-doc
         (and helm-apropos-show-short-doc
              (null (memq 'helm-apropos helm-commands-using-frame)))))
    (helm :sources
          (mapcar (lambda (func)
                    (funcall func default))
                  helm-apropos-function-list)
          :history 'helm-apropos-history
          :buffer "*helm apropos*"
          :preselect (and default (concat "^\\_<" (regexp-quote default) "\\_>"))
          :truncate-lines t)))


;;; Advices
;;
;;
(defvar ad-advised-functions)
(defvar ad-advice-classes)
(declare-function ad-make-single-advice-docstring "advice")
(declare-function ad-get-advice-info-field "advice")
(declare-function ad-advice-set-enabled "advice")
(declare-function ad-advice-set-enabled "advice")
(declare-function ad-advice-enabled "advice")

(defvar helm-source-advice
  (helm-build-sync-source "Function Advice"
    :init (lambda () (require 'advice))
    :candidates 'helm-advice-candidates
    :action (helm-make-actions "Toggle Enable/Disable" 'helm-advice-toggle)
    :persistent-action 'helm-advice-persistent-action
    :nomark t
    :multiline t
    :persistent-help "Toggle describe function / C-u C-j: Toggle advice"))

(defun helm-advice-candidates ()
  (cl-loop for fname in ad-advised-functions
           for function = (intern fname)
           append
           (cl-loop for class in ad-advice-classes append
                    (cl-loop for advice in (ad-get-advice-info-field function class)
                             for enabled = (ad-advice-enabled advice)
                             collect
                             (cons (format
                                    "%s %s %s"
                                    (if enabled "Enabled " "Disabled")
                                    (propertize fname 'face 'font-lock-function-name-face)
                                    (ad-make-single-advice-docstring advice class nil))
                                   (list function class advice))))))

(defun helm-advice-persistent-action (func-class-advice)
  (if current-prefix-arg
      (helm-advice-toggle func-class-advice)
    (describe-function (car func-class-advice))))

(defun helm-advice-toggle (func-class-advice)
  (cl-destructuring-bind (function _class advice) func-class-advice
    (cond ((ad-advice-enabled advice)
           (ad-advice-set-enabled advice nil)
           (message "Disabled"))
          (t
           (ad-advice-set-enabled advice t)
           (message "Enabled")))
    (ad-activate function)
    (and helm-in-persistent-action
         (helm-advice-update-current-display-string))))

(defun helm-advice-update-current-display-string ()
  (helm-edit-current-selection
    (let ((newword (cond ((looking-at "Disabled") "Enabled")
                         ((looking-at "Enabled")  "Disabled"))))
      (when newword
        (delete-region (point) (progn (forward-word 1) (point)))
        (insert newword)))))

;;;###autoload
(defun helm-manage-advice ()
  "Preconfigured `helm' to disable/enable function advices."
  (interactive)
  (helm-other-buffer 'helm-source-advice "*helm advice*"))


;;; Locate elisp library
;;
;;
(defvar helm--locate-library-cache nil)
(defvar helm--locate-library-doc-cache (make-hash-table :test 'equal))
(defun helm-locate-library-scan-list ()
  (cl-loop for dir in load-path
           when (file-directory-p dir)
           nconc (directory-files
                  dir nil (concat (regexp-opt (find-library-suffixes)) "\\'"))))

;;;###autoload
(defun helm-locate-library (&optional arg)
  "Preconfigured helm to locate elisp libraries.

When `completions-detailed' or `helm-completions-detailed' is non
nil, a description of libraries is provided. The libraries are
partially cached in the variables
`helm--locate-library-doc-cache' and
`helm--locate-library-cache'.  TIP: You can make these vars
persistent for faster start with the psession package, using M-x
psession-make-persistent-variable.  NOTE: The caches affect as
well `find-libray' and `locate-library' when `helm-mode' is
enabled and `completions-detailed' is non nil.  There is no need
to refresh the caches, they will be updated automatically if some
new libraries are found, however when a library update its
headers and the description change you can reset the caches with
a prefix arg."
  (interactive "P")
  (let (done)
    (when arg
      (setq helm--locate-library-cache nil)
      (clrhash helm--locate-library-doc-cache))
    (helm :sources
          (helm-build-in-buffer-source  "Elisp libraries (Scan)"
            :data #'helm-locate-library-scan-list
            :fuzzy-match helm-locate-library-fuzzy-match
            :keymap helm-generic-files-map
            :candidate-transformer
            (lambda (candidates)
              (cl-loop with reporter = (unless done
                                         (make-progress-reporter
                                          "Scanning libraries..." 0 (length candidates)))
                       with lgst = (helm-in-buffer-get-longest-candidate)
                       for c in candidates
                       for count from 0
                       for bn = (helm-basename c 2)
                       for sep = (helm-make-separator bn lgst)
                       for path = (or (assoc-default bn helm--locate-library-cache)
                                      ;; A lock file in LOAD-PATH (bug#2626).
                                      (unless (string-match "\\`\\.#" bn)
                                        (let ((p (find-library-name bn)))
                                          (push (cons bn p) helm--locate-library-cache)
                                          p)))
                       for doc = (and path
                                      (or completions-detailed helm-completions-detailed)
                                      (or (gethash bn helm--locate-library-doc-cache)
                                          (puthash bn (helm-locate-lib-get-summary path)
                                                   helm--locate-library-doc-cache)))
                       for disp = (and path
                                       (if (and doc
                                                (or completions-detailed helm-completions-detailed))
                                           (helm-aand (propertize doc 'face 'font-lock-warning-face)
                                                      (propertize " " 'display (concat sep it))
                                                      (concat bn it))
                                         bn))
                       when (and disp path)
                       collect (cons disp path)
                       when reporter do (progress-reporter-update reporter count)
                       finally do (setq done t)))
            :action (helm-actions-from-type-file))
          :buffer "*helm locate library*")))


;;; Modify variables from Helm
;;
;;
(defun helm-set-variable (var)
  "Set VAR value interactively."
  (let* ((sym  (helm-symbolify var))
         (val  (default-value sym))
         (strv (prin1-to-string val)))
    (if (> (length strv) 25)
        (helm-edit-variable var)
      (set-default sym (eval-minibuffer
                        (format "Set `%s': " var)
                        (if (or (arrayp val)
                                (memq val '(nil t))
                                (numberp val))
                            strv
                          (format "'%s" strv)))))))

(defvar helm-edit-variable-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'helm-set-variable-from-pp-buffer)
    (define-key map (kbd "C-c C-k") 'helm-edit-variable-quit)
    (define-key map (kbd "C-c =")   'helm-edit-variable-toggle-diff)
    map))

(define-derived-mode helm-edit-variable-mode
    emacs-lisp-mode "helm-edit-variable"
    "A mode to edit variables values.

Special commands:
\\{helm-edit-variable-mode-map}")

(defvar helm-pretty-print-temp-file-name
  (expand-file-name "helm-edit-variable.el" temporary-file-directory))
(defvar helm-pretty-print-buffer-name "*pretty print output*")
(defvar helm-pretty-print-current-symbol nil)
(defun helm-edit-variable (var)
  (let* ((sym (intern-soft var))
         (val (symbol-value sym))
         (pp  (pp-to-string val))
         start)
    (with-current-buffer (get-buffer-create helm-pretty-print-buffer-name)
      (erase-buffer)
      (helm-edit-variable-mode)
      (setq start (point))
      ;; Any number of lines starting with ";;;" + one empty line.
      (insert (format ";;; Edit variable `%s' and hit C-c C-c when done\n" sym)
              ";;; Abort with C-c C-k\n\n")
      (add-text-properties start (1- (point)) '(read-only t))
      (add-text-properties (1- (point)) (point) '(read-only t rear-nonsticky t))
      (set (make-local-variable 'helm-pretty-print-current-symbol) sym)
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when (file-exists-p helm-pretty-print-temp-file-name)
                    (delete-file helm-pretty-print-temp-file-name)))
                nil t)
      (save-excursion (insert pp))
      (write-region
       (point-min) (point-max) helm-pretty-print-temp-file-name nil 1)
      (setq buffer-file-name helm-pretty-print-temp-file-name))
    (display-buffer helm-pretty-print-buffer-name)))

(defun helm-edit-variable-toggle-diff ()
  "Show diff in edit variable buffer."
  (interactive)
  (if (get-buffer-window "*Diff*" 'visible)
      (kill-buffer "*Diff*")
    (diff-buffer-with-file)))

(defun helm-set-variable-from-pp-buffer ()
  "Set variable associated with buffer to buffer contents.

The associated variable is the local variable
`helm-pretty-print-current-symbol'."
  (interactive)
  (with-current-buffer helm-pretty-print-buffer-name
    (goto-char (point-min))
    (when (re-search-forward "^$" nil t)
      (forward-line 1))
    (let ((val (symbol-value helm-pretty-print-current-symbol)))
      (save-excursion
        (if (or (arrayp val)
                (memq val '(nil t))
                (numberp val)
                (looking-at "[`']"))
            (set-default helm-pretty-print-current-symbol
                         (read (current-buffer)))
          (set-default helm-pretty-print-current-symbol
                       `(,@(read (current-buffer))))))
      (if (equal val (symbol-value helm-pretty-print-current-symbol))
          (message "No changes done")
        (message "`%s' value modified" helm-pretty-print-current-symbol))
      (helm-edit-variable-quit))))

(defun helm-edit-variable-quit ()
  "Quit edit variable buffer."
  (interactive)
  (set-buffer-modified-p nil)
  (quit-window t)
  (helm-aif (get-buffer-window "*Diff*" 'visible)
      (quit-window t it)))


;;; Elisp Timers.
;;
;;
(defclass helm-absolute-time-timers-class (helm-source-sync helm-type-timers)
  ((candidates :initform 'timer-list)
   (allow-dups :initform t)
   (candidate-transformer
    :initform
    (lambda (candidates)
      (cl-loop for timer in candidates
               collect (cons (helm-elisp--format-timer timer) timer))))))

(defvar helm-source-absolute-time-timers
  (helm-make-source "Absolute Time Timers" 'helm-absolute-time-timers-class))

(defclass helm-idle-time-timers-class (helm-source-sync helm-type-timers)
  ((candidates :initform 'timer-idle-list)
   (allow-dups :initform t)
   (candidate-transformer
    :initform
    (lambda (candidates)
      (cl-loop for timer in candidates
               collect (cons (helm-elisp--format-timer timer) timer))))))

(defvar helm-source-idle-time-timers
  (helm-make-source "Idle Time Timers" 'helm-idle-time-timers-class))

(defun helm-elisp--format-timer (timer)
  (format "%s repeat=%s %s(%s)"
          (let ((time (timer--time timer)))
            (if (timer--idle-delay timer)
                (format "idle-for=[%s]"
                        (format-seconds "%dd %hh %mmin %z%,3ss"
                                        (time-to-seconds time)))
              (format-time-string "%m/%d %T" time)))
          (or (timer--repeat-delay timer) "nil")
          (mapconcat #'identity (split-string
                                (prin1-to-string (timer--function timer))
                                "\n")
                     " ")
          (mapconcat #'prin1-to-string (timer--args timer) " ")))

;;;###autoload
(defun helm-timers ()
  "Preconfigured `helm' for timers."
  (interactive)
  (helm :sources '(helm-source-absolute-time-timers
                   helm-source-idle-time-timers)
        :buffer "*helm timers*"))


;;; Complex command history
;;
;;

(defvar helm-sexp--last-sexp nil)
;; This wont work compiled.
(defun helm-sexp-eval-1 ()
  (interactive)
  (unwind-protect
      (progn
        ;; Trick called-interactively-p into thinking that `cand' is
        ;; an interactive call, See `repeat-complex-command'.
        (add-hook 'called-interactively-p-functions
                  #'helm-complex-command-history--called-interactively-skip)
        (eval (read helm-sexp--last-sexp) t))
    (remove-hook 'called-interactively-p-functions
                 #'helm-complex-command-history--called-interactively-skip)))

(defun helm-complex-command-history--called-interactively-skip (i _frame1 frame2)
  (and (eq 'eval (cadr frame2))
       (eq 'helm-sexp-eval-1
           (cadr (backtrace-frame (+ i 2) #'called-interactively-p)))
       1))

(defun helm-sexp-eval (_candidate)
  (call-interactively #'helm-sexp-eval-1))

(defvar helm-source-complex-command-history
  (helm-build-sync-source "Complex Command History"
    :candidates (lambda ()
                  ;; Use cdr to avoid adding
                  ;; `helm-complex-command-history' here.
                  (cl-loop for i in command-history
                           unless (equal i '(helm-complex-command-history))
                           collect (prin1-to-string i)))
    :action (helm-make-actions
             "Eval" (lambda (candidate)
                      (and (boundp 'helm-sexp--last-sexp)
                           (setq helm-sexp--last-sexp candidate))
                      (let ((command (read candidate)))
                        (unless (equal command (car command-history))
                          (setq command-history (cons command command-history))))
                      (run-with-timer 0.1 nil #'helm-sexp-eval candidate))
             "Edit and eval" (lambda (candidate)
                               (edit-and-eval-command "Eval: " (read candidate))))
    :persistent-action #'helm-sexp-eval
    :multiline t))

;;;###autoload
(defun helm-complex-command-history ()
  "Preconfigured `helm' for complex command history."
  (interactive)
  (helm :sources 'helm-source-complex-command-history
        :buffer "*helm complex commands*"))

(provide 'helm-elisp)

;;; helm-elisp.el ends here
