;;; helm-lib.el --- Helm routines. -*- lexical-binding: t -*-

;; Copyright (C) 2015 ~ 2020  Thierry Volpiatto 

;; Author: Thierry Volpiatto 
;; URL: http://github.com/emacs-helm/helm

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

;;; Commentary:
;; All helm functions that don't require specific helm code should go here.

;;; Code:

(require 'cl-lib)

(declare-function ansi-color--find-face "ansi-color.el")
(declare-function ansi-color-apply-sequence "ansi-color.el")
(declare-function dired-current-directory "dired.el")
(declare-function ffap-file-remote-p "ffap.el")
(declare-function ffap-url-p "ffap.el")
(declare-function helm-get-attr "helm-core.el")
(declare-function helm-set-attr "helm-core.el")
(declare-function helm-follow-mode-p "helm-core.el")
(declare-function helm-get-current-source "helm-core.el")
(declare-function helm-get-selection "helm-core.el")
(declare-function helm-get-sources "helm-core.el")
(declare-function helm-interpret-value "helm-core.el")
(declare-function helm-log-run-hook "helm-core.el")
(declare-function helm-next-line "helm-core.el")
(declare-function helm-get-next-header-pos "helm-core.el")
(declare-function helm-mark-current-line "helm-core.el")
(declare-function helm-marked-candidates "helm-core.el")
(declare-function helm-set-case-fold-search "helm-core.el")
(declare-function helm-get-previous-header-pos "helm-core.el")
(declare-function helm-source--cl--print-table "helm-source.el")
(declare-function helm-update "helm-core.el")
(declare-function org-content "org.el")
(declare-function org-mark-ring-goto "org.el")
(declare-function org-mark-ring-push "org.el")
(declare-function org-table-p "org-compat.el")
(declare-function org-table-align "org-table.el")
(declare-function org-table-end "org-table.el")
(declare-function org-open-at-point "org.el")
(declare-function helm-read-file-name "helm-mode.el")
(declare-function find-function-library "find-func.el")
(declare-function find-library-name "find-func.el")

(defvar helm-sources)
(defvar helm-initial-frame)
(defvar helm-current-position)
(defvar helm-persistent-action-display-window)
(defvar helm--buffer-in-new-frame-p)
(defvar helm-completion-style)
(defvar helm-completion-styles-alist)
(defvar helm-persistent-action-window-buffer)
(defvar helm-help-buffer-name)
(defvar completion-flex-nospace)
(defvar find-function-source-path)
(defvar ffap-machine-p-unknown)
(defvar ffap-machine-p-local)
(defvar ffap-machine-p-known)
(defvar helm-debug-output-buffer)

;;; User vars.
;;
(defcustom helm-file-globstar t
  "Same as globstar bash shopt option.
When non-nil a pattern beginning with two stars will expand
recursively.
Directories expansion is not supported yet."
  :group 'helm
  :type 'boolean)

(defcustom helm-yank-text-at-point-function nil
  "The function used to forward point with `helm-yank-text-at-point'.
With a nil value, fallback to default `forward-word'.
The function should take one arg, an integer like `forward-word'.
NOTE: Using `forward-symbol' here is not very useful as it is
already provided by \\<helm-map>\\[next-history-element]."
  :type  'function
  :group 'helm)

(defcustom helm-scroll-amount nil
  "Scroll amount when scrolling helm window or other window in a helm session.
It is used by `helm-scroll-other-window', `helm-scroll-up', `helm-scroll-down'
and `helm-scroll-other-window-down'.

If you prefer scrolling line by line, set this value to 1."
  :group 'helm
  :type 'integer)

(defcustom helm-help-full-frame t
  "Display help window in full frame when non nil.

Even when nil probably the same result (full frame) can be
reached by tweaking `display-buffer-alist', but it is much more
convenient to use a simple boolean value here."
  :type 'boolean
  :group 'helm-help)

(defvar helm-ff--boring-regexp nil)
(defun helm-ff--setup-boring-regex (var val)
  (set var val)
  (setq helm-ff--boring-regexp
          (cl-loop with last = (car (last val))
                   for r in (butlast val)
                   if (string-match "\\$\\'" r)
                   concat (concat r "\\|") into result
                   else concat (concat r "$\\|") into result
                   finally return
                   (concat result last
                           (if (string-match "\\$\\'" last) "" "$")))))

(defcustom helm-boring-file-regexp-list
  (mapcar (lambda (f)
            (let ((rgx (regexp-quote f)))
              (if (string-match-p "[^/]$" f)
                  ;; files: e.g .o => \\.o$
                  (concat rgx "$")
                ;; directories: e.g .git/ => \.git\\(/\\|$\\)
                (concat (substring rgx 0 -1) "\\(/\\|$\\)"))))
          completion-ignored-extensions)
  "A list of regexps matching boring files.

This list is build by default on `completion-ignored-extensions'.
The directory names should end with \"/?\" e.g. \"\\.git/?\" and
the file names should end with \"$\" e.g. \"\\.o$\".

These regexps may be used to match the entire path, not just the
file name, so for example to ignore files with a prefix
\".bak.\", use \"\\.bak\\..*$\" as the regexp.

NOTE: When modifying this, be sure to use customize interface or
the customize functions e.g. `customize-set-variable' and NOT
`setq'."
  :group 'helm-files
  :type  '(repeat (choice regexp))
  :set 'helm-ff--setup-boring-regex)

(defcustom helm-describe-function-function 'describe-function
  "Function used to describe functions in Helm."
  :group 'helm-elisp
  :type 'function)

(defcustom helm-describe-variable-function 'describe-variable
  "Function used to describe variables in Helm."
  :group 'helm-elisp
  :type 'function)


;;; Internal vars
;;
(defvar helm-yank-point nil)
(defvar helm-pattern ""
  "The input pattern used to update the helm buffer.")
(defvar helm-buffer "*helm*"
  "Buffer showing completions.")
(defvar helm-current-buffer nil
  "Current buffer when `helm' is invoked.")
(defvar helm-suspend-update-flag nil)
(defvar helm-action-buffer "*helm action*"
  "Buffer showing actions.")
(defvar helm-current-prefix-arg nil
  "Record `current-prefix-arg' when exiting minibuffer.")
(defvar helm-current-error nil
  "Same as `compilation-current-error' but for helm-occur and helm-grep.")

;;; Compatibility
;;
(defun helm-add-face-text-properties (beg end face &optional append object)
  "Add the face property to the text from START to END.
It is a compatibility function which behaves exactly like
`add-face-text-property' if available, otherwise like
`add-text-properties'.  When only `add-text-properties' is
available APPEND is ignored."
  (if (fboundp 'add-face-text-property)
      (add-face-text-property beg end face append object)
      (add-text-properties beg end `(face ,face) object)))

;;; Override `push-mark'
;;
;; Fix duplicates in `mark-ring' and `global-mark-ring' and update
;; buffers in `global-mark-ring' to recentest mark.
(defun helm--advice-push-mark (&optional location nomsg activate)
  (unless (null (mark t))
    (let ((marker (copy-marker (mark-marker))))
      (setq mark-ring (cons marker (delete marker mark-ring))))
    (when (> (length mark-ring) mark-ring-max)
      ;; Move marker to nowhere.
      (set-marker (car (nthcdr mark-ring-max mark-ring)) nil)
      (setcdr (nthcdr (1- mark-ring-max) mark-ring) nil)))
  (set-marker (mark-marker) (or location (point)) (current-buffer))
  ;; Now push the mark on the global mark ring.
  (setq global-mark-ring (cons (copy-marker (mark-marker))
                               ;; Avoid having multiple entries
                               ;; for same buffer in `global-mark-ring'.
                               (cl-loop with mb = (current-buffer)
                                        for m in global-mark-ring
                                        for nmb = (marker-buffer m)
                                        unless (eq mb nmb)
                                        collect m)))
  (when (> (length global-mark-ring) global-mark-ring-max)
    (set-marker (car (nthcdr global-mark-ring-max global-mark-ring)) nil)
    (setcdr (nthcdr (1- global-mark-ring-max) global-mark-ring) nil))
  (or nomsg executing-kbd-macro (> (minibuffer-depth) 0)
      (message "Mark set"))
  (when (or activate (not transient-mark-mode))
    (set-mark (mark t)))
  nil)

(defcustom helm-advice-push-mark t
  "Override `push-mark' with a version avoiding duplicates when non-nil."
  :group 'helm
  :type 'boolean
  :set (lambda (var val)
         (set var val)
         (if val
             (advice-add 'push-mark :override #'helm--advice-push-mark '((depth . 100)))
           (advice-remove 'push-mark #'helm--advice-push-mark))))

;; This the version of Emacs-27 written by Stefan
(defun helm-advice--ffap-read-file-or-url (prompt guess)
  (or guess (setq guess default-directory))
  (if (ffap-url-p guess)
      (read-string prompt guess nil nil t)
    (unless (ffap-file-remote-p guess)
      (setq guess (abbreviate-file-name (expand-file-name guess))))
    (read-file-name prompt (file-name-directory guess) nil nil
                    (file-name-nondirectory guess))))

;; The native-comp branch of emacs "is a modified Emacs capable of compiling
;; and running Emacs Lisp as native code in form of re-loadable elf files."
;; (https://akrl.sdf.org/gccemacs.html). The function subr-native-elisp-p is a
;; native function available only in this branch and evaluates to true if the
;; argument supplied is a natively compiled lisp function. Use this function
;; if it's available, otherwise return nil. Helm needs to distinguish compiled
;; functions from other symbols in a various places.
(defun helm-subr-native-elisp-p (object)
  (when (fboundp 'subr-native-elisp-p)
      (subr-native-elisp-p object)))

;; Available only in Emacs-28+
(unless (fboundp 'file-modes-number-to-symbolic)
  (defun file-modes-number-to-symbolic (mode &optional filetype)
    "Return a string describing a file's MODE.
For instance, if MODE is #o700, then it produces `-rwx------'.
FILETYPE if provided should be a character denoting the type of file,
such as `?d' for a directory, or `?l' for a symbolic link and will override
the leading `-' char."
    (string
     (or filetype
         (pcase (ash mode -12)
           ;; POSIX specifies that the file type is included in st_mode
           ;; and provides names for the file types but values only for
           ;; the permissions (e.g., S_IWOTH=2).

           ;; (#o017 ??) ;; #define S_IFMT  00170000
           (#o014 ?s) ;; #define S_IFSOCK 0140000
           (#o012 ?l) ;; #define S_IFLNK  0120000
           ;; (8  ??)    ;; #define S_IFREG  0100000
           (#o006  ?b) ;; #define S_IFBLK  0060000
           (#o004  ?d) ;; #define S_IFDIR  0040000
           (#o002  ?c) ;; #define S_IFCHR  0020000
           (#o001  ?p) ;; #define S_IFIFO  0010000
           (_ ?-)))
     (if (zerop (logand   256 mode)) ?- ?r)
     (if (zerop (logand   128 mode)) ?- ?w)
     (if (zerop (logand    64 mode))
         (if (zerop (logand  2048 mode)) ?- ?S)
       (if (zerop (logand  2048 mode)) ?x ?s))
     (if (zerop (logand    32 mode)) ?- ?r)
     (if (zerop (logand    16 mode)) ?- ?w)
     (if (zerop (logand     8 mode))
         (if (zerop (logand  1024 mode)) ?- ?S)
       (if (zerop (logand  1024 mode)) ?x ?s))
     (if (zerop (logand     4 mode)) ?- ?r)
     (if (zerop (logand     2 mode)) ?- ?w)
     (if (zerop (logand 512 mode))
         (if (zerop (logand   1 mode)) ?- ?x)
       (if (zerop (logand   1 mode)) ?T ?t)))))

(unless (and (fboundp 'pos-bol) (fboundp 'pos-eol))
  (defalias 'pos-bol 'line-beginning-position)
  (defalias 'pos-eol 'line-end-position))

;;; Compatibility with < Emacs-29
;;  Needed by helm-packages.el and affixations functions for helm-mode (27)
;;  waiting package.el moves on Elpa.  Slightly modified to fit with
;;  Emacs-27/28.
(when (eval-when-compile (< emacs-major-version 29)) ; Avoid warnings.
  (progn
    (require 'package)
    (eval-and-compile
      (defun package--archives-initialize ()
        "Make sure the list of installed and remote packages are initialized."
        (unless package--initialized
          (package-initialize t))
        (unless package-archive-contents
          (package-refresh-contents)))

      (defun package-get-descriptor (pkg-name)
        "Return the `package-desc' of PKG-NAME."
        (unless package--initialized (package-initialize 'no-activate))
        (or (cadr (assq pkg-name package-alist))
            (cadr (assq pkg-name package-archive-contents))))
  
      (defun package--upgradeable-packages ()
        ;; Initialize the package system to get the list of package
        ;; symbols for completion.
        (package--archives-initialize)
        (mapcar
         #'car
         (seq-filter
          (lambda (elt)
            (or (let ((available
                       (assq (car elt) package-archive-contents)))
                  (and available
                       (version-list-<
                        (package-desc-version (cadr elt))
                        (package-desc-version (cadr available)))))))
          package-alist)))

      (defun package-upgrade (name)
        "Upgrade package NAME if a newer version exists."
        (let* ((package (if (symbolp name)
                            name
                          (intern name)))
               (pkg-desc (cadr (assq package package-alist))))
          ;; `pkg-desc' will be nil when the package is an "active built-in".
          (when pkg-desc
            (package-delete pkg-desc 'force 'dont-unselect))
          (package-install package
                           ;; An active built-in has never been "selected"
                           ;; before.  Mark it as installed explicitly.
                           (and pkg-desc 'dont-select))))

      (defun package-recompile (pkg)
        "Byte-compile package PKG again.
PKG should be either a symbol, the package name, or a `package-desc'
object."
        (let ((pkg-desc (if (package-desc-p pkg)
                            pkg
                          (cadr (assq pkg package-alist)))))
          ;; Delete the old .elc files to ensure that we don't inadvertently
          ;; load them (in case they contain byte code/macros that are now
          ;; invalid).
          (dolist (elc (directory-files-recursively
                        (package-desc-dir pkg-desc) "\\.elc\\'"))
            (delete-file elc))
          (package--compile pkg-desc)))

      (defun package--dependencies (pkg)
        "Return a list of all dependencies PKG has.
This is done recursively."
        ;; Can we have circular dependencies?  Assume "nope".
        (when-let* ((desc (cadr (assq pkg package-archive-contents)))
                    (deps (mapcar #'car (package-desc-reqs desc))))
          (delete-dups (apply #'nconc deps (mapcar #'package--dependencies deps))))))))

;;; Provide `help--symbol-class' not available in emacs-27
;;
(unless (fboundp 'help--symbol-class)
  (defun help--symbol-class (s)
    "Return symbol class characters for symbol S."
    (when (stringp s)
      (setq s (intern-soft s)))
    (concat
     (when (fboundp s)
       (concat
        (cond
          ((commandp s) "c")
          ((eq (car-safe (symbol-function s)) 'macro) "m")
          (t "f"))
        (and (let ((flist (indirect-function s)))
               (advice--p (if (eq 'macro (car-safe flist)) (cdr flist) flist)))
             "!")
        (and (get s 'byte-obsolete-info) "-")))
     (when (boundp s)
       (concat
        (if (custom-variable-p s) "u" "v")
        (and (local-variable-if-set-p s) "'")
        (and (ignore-errors (not (equal (symbol-value s) (default-value s)))) "*")
        (and (get s 'byte-obsolete-variable) "-")))
     (and (facep s) "a")
     (and (fboundp 'cl-find-class) (cl-find-class s) "t"))))

;; Inline `kmacro--to-vector' from E29 to fix compatibility of
;; `helm-kbd-macro-concat-macros' with E29 and E28.
(unless (fboundp #'kmacro--to-vector)
  (defun kmacro--to-vector (object)
  "Normalize an old-style key sequence to the vector form."
  (if (not (stringp object))
      object
    (let ((vec (string-to-vector object)))
      (unless (multibyte-string-p object)
	(dotimes (i (length vec))
	  (let ((k (aref vec i)))
	    (when (> k 127)
	      (setf (aref vec i) (+ k ?\M-\C-@ -128))))))
      vec))))

;;; Macros helper.
;;
(defmacro helm-with-gensyms (symbols &rest body)
  "Bind the SYMBOLS to fresh uninterned symbols and eval BODY."
  (declare (indent 1))
  `(let ,(mapcar (lambda (s)
                   ;; Use cl-gensym here instead of make-symbol
                   ;; to ensure a symbol that have a live that go
                   ;; beyond the live of its macro have different name.
                   ;; i.e symbols created with `with-helm-temp-hook'
                   ;; should have random names.
                   `(,s (cl-gensym (symbol-name ',s))))
                 symbols)
     ,@body))

;;; Command loop helper
;;
(defconst helm-this-command-black-list
  '(helm-maybe-exit-minibuffer
    helm-confirm-and-exit-minibuffer
    helm-exit-minibuffer
    exit-minibuffer
    helm-M-x))

(defun helm-this-command ()
  "Return the actual command in action.
Like `this-command' but return the real command, and not
`exit-minibuffer' or other unwanted functions."
  (cl-loop for count from 1 to 50
           for btf = (backtrace-frame count)
           for fn = (cl-second btf)
           if (and
               ;; In some case we may have in the way an
               ;; advice compiled resulting in byte-code,
               ;; ignore it (Bug#691).
               (symbolp fn)
               (commandp fn)
               (not (memq fn helm-this-command-black-list)))
           return fn
           else
           if (and (eq fn 'call-interactively)
                   (> (length btf) 2))
           return (cadr (cdr btf))))


;;; Iterators
;;
(cl-defmacro helm-position (item seq &key test all)
  "A simple and faster replacement of CL `position'.

Returns ITEM first occurence position found in SEQ.
When SEQ is a string, ITEM have to be specified as a char.
Argument TEST when unspecified default to `eq'.
When argument ALL is non-nil return a list of all ITEM positions
found in SEQ."
  (let ((key (if (stringp seq) 'across 'in)))
    `(cl-loop with deftest = 'eq
              for c ,key ,seq
              for index from 0
              when (funcall (or ,test deftest) c ,item)
              if ,all collect index into ls
              else return index
              finally return ls)))

(defun helm-iter-list (seq &optional cycle)
  "Return an iterator object from SEQ.
The iterator die and return nil when it reach end of SEQ.
When CYCLE is specified the iterator never ends."
  (let ((lis seq))
    (lambda ()
      (let ((elm (car lis)))
        (setq lis (if cycle
                      (or (cdr lis) seq)
                    (cdr lis)))
        elm))))

(defun helm-iter-circular (seq)
  "Infinite iteration on SEQ."
  (helm-iter-list seq 'cycle))

(cl-defun helm-iter-sub-next-circular (seq elm &key (test 'eq))
  "Infinite iteration of SEQ starting at ELM."
  (let* ((pos      (1+ (helm-position elm seq :test test)))
         (sub      (append (nthcdr pos seq) (helm-take seq pos)))
         (iterator (helm-iter-circular sub)))
    (lambda ()
      (helm-iter-next iterator))))

(defun helm-iter-next (iterator)
  "Return next elm of ITERATOR."
  (and iterator (funcall iterator)))


;;; Anaphoric macros.
;;
(defmacro helm-aif (test-form then-form &rest else-forms)
  "Anaphoric version of `if'.
Like `if' but set the result of TEST-FORM in a temporary variable
called `it'.  THEN-FORM and ELSE-FORMS are then executed just like
in `if'."
  (declare (indent 2) (debug t))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defmacro helm-awhile (sexp &rest body)
  "Anaphoric version of `while'.
Same usage as `while' except that SEXP is bound to a temporary
variable called `it' at each turn.
An implicit nil block is bound to the loop so usage of
`cl-return' is possible to exit the loop."
  (declare (indent 1) (debug t))
  (helm-with-gensyms (flag)
    `(let ((,flag t))
       (cl-block nil
         (while ,flag
           (helm-aif ,sexp
               (progn ,@body)
             (setq ,flag nil)))))))

(defmacro helm-acond (&rest clauses)
  "Anaphoric version of `cond'.
In each clause of CLAUSES, the result of the car of clause is
stored in a temporary variable called `it' and usable in the cdr
of this same clause.  Each `it' variable is independent of its
clause.  The usage is the same as `cond'."
  (declare (debug cond))
  (unless (null clauses)
    (helm-with-gensyms (sym)
      (let ((clause1 (car clauses)))
        `(let ((,sym ,(car clause1)))
           (helm-aif ,sym
               (if (cdr ',clause1)
                   (progn ,@(cdr clause1))
                 it)
             (helm-acond ,@(cdr clauses))))))))

(defmacro helm-aand (&rest conditions)
  "Anaphoric version of `and'.
Each condition is bound to a temporary variable called `it' which
is usable in next condition."
  (declare (debug (&rest form)))
  (cond ((null conditions) t)
        ((null (cdr conditions)) (car conditions))
        (t `(helm-aif ,(car conditions)
                (helm-aand ,@(cdr conditions))))))

(defmacro helm-acase (expr &rest clauses)
  "A simple anaphoric `cl-case' implementation handling strings.
EXPR is bound to a temporary variable called `it' which is usable
in CLAUSES to refer to EXPR.
NOTE: Duplicate keys in CLAUSES are deliberately not handled.

\(fn EXPR (KEYLIST BODY...)...)"
  (declare (indent 1) (debug (form &rest (sexp body))))
  (unless (null clauses)
    (let ((clause1 (car clauses)))
      `(let ((key ',(car clause1))
             (it ,expr))
         (if (or (equal it key)
                 (and (listp key) (member it key))
                 (eq key t))
             (progn ,@(cdr clause1))
           (helm-acase it ,@(cdr clauses)))))))

;;; Fuzzy matching routines
;;
(defsubst helm--mapconcat-pattern (pattern)
  "Transform string PATTERN in regexp for further fuzzy matching.
E.g.: helm.el$
     => \"[^h]*?h[^e]*?e[^l]*?l[^m]*?m[^.]*?[.][^e]*?e[^l]*?l$\"
     ^helm.el$
     => \"helm[.]el$\"."
  (let ((ls (split-string-and-unquote pattern "")))
    (if (string= "^" (car ls))
        ;; Exact match.
        (mapconcat (lambda (c)
                     (if (and (string= c "$")
                              (string-match "$\\'" pattern))
                         c (regexp-quote c)))
                   (cdr ls) "")
        ;; Fuzzy match.
        (mapconcat (lambda (c)
                     (if (and (string= c "$")
                              (string-match "$\\'" pattern))
                         c (format "[^%s]*?%s" c (regexp-quote c))))
                   ls ""))))

(defsubst helm--collect-pairs-in-string (string)
  (cl-loop for str on (split-string string "" t) by 'cdr
           when (cdr str)
           collect (list (car str) (cadr str))))

;;; Help routines.
;;
(defvar helm-help--iter-org-state nil)

(defvar helm-help-mode-before-hook nil
  "A hook that runs before helm-help starts.")

(defvar helm-help-mode-after-hook nil
  "A hook that runs when helm-help exits.")

(defcustom helm-help-default-prompt
  "[SPC,C-v,next:ScrollUp  b,M-v,prior:ScrollDown TAB:Cycle M-TAB:All C-s/r:Isearch q:Quit]"
  "The prompt used in `helm-help'."
  :type 'string
  :group 'helm)

(defcustom helm-help-hkmap
  '(("C-v" . helm-help-scroll-up)
    ("SPC" . helm-help-scroll-up)
    ("<next>" . helm-help-scroll-up)
    ("M-v" . helm-help-scroll-down)
    ("b" . helm-help-scroll-down)
    ("<prior>" . helm-help-scroll-down)
    ("C-s" . isearch-forward)
    ("C-r" . isearch-backward)
    ("C-a" . move-beginning-of-line)
    ("C-e" . move-end-of-line)
    ("C-f" . forward-char)
    ("<right>" . forward-char)
    ("C-b" . backward-char)
    ("<left>" . backward-char)
    ("C-n" . helm-help-next-line)
    ("C-p" . helm-help-previous-line)
    ("<down>" . helm-help-next-line)
    ("<up>" . helm-help-previous-line)
    ("M-a" . backward-sentence)
    ("M-e" . forward-sentence)
    ("M-f" . forward-word)
    ("M-b" . backward-word)
    ("M->" . end-of-buffer)
    ("M-<" . beginning-of-buffer)
    ("C-SPC" . helm-help-toggle-mark)
    ("C-M-SPC" . mark-sexp)
    ("TAB"   . org-cycle)
    ("C-m" . helm-help-org-open-at-point)
    ("C-&" . helm-help-org-mark-ring-goto)
    ("C-%" . org-mark-ring-push)
    ("M-TAB" . helm-help-org-cycle)
    ("M-w" . helm-help-copy-region-as-kill)
    ("q" . helm-help-quit))
  "Alist of (KEY . FUNCTION) for `helm-help'.

This is not a standard keymap, just an alist where it is possible to
define a simple KEY (a string with no spaces) associated with a
FUNCTION. More complex key like \"C-x C-x\" are not supported.
Interactive functions will be called interactively whereas other
functions will be called with funcall except commands that are in
`helm-help-not-interactive-command'.
For convenience you can add bindings here with `helm-help-define-key'."
  :type '(alist :key-type string :key-value symbol)
  :group 'helm)

(defvar helm-help-not-interactive-command '(isearch-forward isearch-backward)
  "Commands that we don't want to call interactively in `helm-help'.")

(defun helm-help-internal (bufname insert-content-fn)
  "Show long message during Helm session in BUFNAME.
INSERT-CONTENT-FN is the function that inserts text to be
displayed in BUFNAME."
  (let ((winconf (current-frame-configuration))
        (hframe (selected-frame)))
    (helm-log-run-hook "helm-help-internal" 'helm-help-mode-before-hook)
    (with-selected-frame helm-initial-frame
      (select-frame-set-input-focus helm-initial-frame)
      (unwind-protect
           (progn
             (setq helm-suspend-update-flag t)
             (set-buffer (get-buffer-create bufname))
             (switch-to-buffer bufname)
             (when helm-help-full-frame (delete-other-windows))
             (delete-region (point-min) (point-max))
             (org-mode)
             (save-excursion
               (funcall insert-content-fn)
               (goto-char (point-min))
               (while (re-search-forward "^[|]" nil t)
                 (when (org-table-p t)
                   (org-table-align)
                   (goto-char (org-table-end)))))
             (org-mark-ring-push) ; Put mark at bob
             (buffer-disable-undo)
             (helm-help-event-loop))
        (raise-frame hframe)
        (helm-log-run-hook "helm-help-internal" 'helm-help-mode-after-hook)
        (setq helm-suspend-update-flag nil)
        (set-frame-configuration winconf)))))

(cl-defun helm-help-scroll-up (&optional (amount helm-scroll-amount))
  "Scroll up in `helm-help'."
  (condition-case _err
      (scroll-up-command amount)
    (beginning-of-buffer nil)
    (end-of-buffer nil)))

(cl-defun helm-help-scroll-down (&optional (amount helm-scroll-amount))
  "Scroll down in `helm-help'."
  (condition-case _err
      (scroll-down-command amount)
    (beginning-of-buffer nil)
    (end-of-buffer nil)))

(defun helm-help-next-line ()
  "Next line function for `helm-help'."
  (condition-case _err
      (call-interactively #'next-line)
    (beginning-of-buffer nil)
    (end-of-buffer nil)))

(defun helm-help-previous-line ()
  "Previous line function for `helm-help'."
  (condition-case _err
      (call-interactively #'previous-line)
    (beginning-of-buffer nil)
    (end-of-buffer nil)))

(defun helm-help-toggle-mark ()
  "Toggle mark in `helm-help'."
  (if (region-active-p)
      (deactivate-mark)
      (push-mark nil nil t)))

(defun helm-help-org-cycle ()
  "Runs `org-cycle' in `helm-help'."
  (pcase (helm-iter-next helm-help--iter-org-state)
    ((pred numberp) (org-content))
    ((and state) (org-cycle state))))

(defun helm-help-copy-region-as-kill ()
  "Copy region function for `helm-help'"
  (copy-region-as-kill
   (region-beginning) (region-end))
  (deactivate-mark))

(defun helm-help-quit ()
  "Quit `helm-help'."
  (if (or (get-buffer-window helm-help-buffer-name 'visible)
          (get-buffer-window helm-debug-output-buffer 'visible))
      (throw 'helm-help-quit nil)
    (quit-window)))

(defun helm-help-org-open-at-point ()
  "Calls `org-open-at-point' ignoring errors."
  (ignore-errors
    (org-open-at-point)))

(defun helm-help-org-mark-ring-goto ()
  "Calls `org-mark-ring-goto' ignoring errors."
  (ignore-errors
    (org-mark-ring-goto)))

(defun helm-help-event-loop ()
  "The loop in charge of scanning keybindings in `helm-help'."
  (let ((prompt (propertize
                 helm-help-default-prompt
                 'face 'helm-helper))
        scroll-error-top-bottom
        (helm-help--iter-org-state (helm-iter-circular '(1 (16) (64)))))
    (catch 'helm-help-quit
      (helm-awhile (read-key prompt)
        (let ((fun (cl-loop for (k . v) in helm-help-hkmap
                            when (eql (aref (kbd k) 0) it)
                            return v)))
          (when fun
            (if (and (commandp fun)
                     (not (memq fun helm-help-not-interactive-command)))
                ;; For movement of cursor in help buffer we need to
                ;; call interactively commands for impaired people
                ;; using a synthetizer (Bug#1347).
                (call-interactively fun)
              (funcall fun))))))))

(defun helm-help-define-key (key function &optional override)
  "Add KEY bound to fUNCTION in `helm-help-hkmap'.

If OVERRIDE is non nil, all bindings associated with FUNCTION are
removed and only (KEY . FUNCTION) is kept.
If FUNCTION is nil (KEY . FUNCTION) is not added and removed from
alist if already present.
See `helm-help-hkmap' for supported keys and functions."
  (cl-assert (not (cdr (split-string key))) nil
             (format "Error: Unsuported key `%s'" key))
  (when override
    (helm-awhile (rassoc function helm-help-hkmap)
      (setq helm-help-hkmap (delete it helm-help-hkmap))))
  (helm-aif (and (null function) (assoc key helm-help-hkmap))
      (setq helm-help-hkmap (delete it helm-help-hkmap))
    (and function (add-to-list 'helm-help-hkmap `(,key . ,function)))))

;;; Multiline transformer
;;
(defun helm-multiline-transformer (candidates _source)
  (cl-loop with offset = (helm-interpret-value
                          (assoc-default 'multiline (helm-get-current-source)))
           for cand in candidates
           for disp = (or (car-safe cand) cand)
           for real = (or (cdr-safe cand) cand)
           if (numberp offset)
           collect (cons (helm--multiline-get-truncated-candidate disp offset)
                         real)
           else collect (cons disp real)))

(defun helm--multiline-get-truncated-candidate (candidate offset)
  "Truncate CANDIDATE when its length is > than OFFSET."
  (with-temp-buffer
    (insert candidate)
    (goto-char (point-min))
    (if (and offset
             (> (buffer-size) offset))
        (let ((end-str "[...]"))
          (concat
           (buffer-substring
            (point)
            (save-excursion
              (forward-char offset)
              (setq end-str (if (looking-at "\n")
                                end-str (concat "\n" end-str)))
              (point)))
           end-str))
        (buffer-string))))

;;; List processing
;;
(defun helm-flatten-list (seq)
  "Return a list of all single elements of sublists in SEQ.

    Example:
    (helm-flatten-list \\='(1 (2 . 3) nil (4 5 (6) 7) 8 (9 . 10)))
    => (1 2 3 4 5 6 7 8 9 10)"
  (let (result)
    (cl-labels ((flatten
                 (seq)
                 (cl-loop for elm in seq
                          if (consp elm)
                          do (flatten
                              (if (atom (cdr elm))
                                  (list (car elm) (cdr elm))
                                elm))
                          else do (and elm (push elm result)))))
      (flatten seq))
    (nreverse result)))

(defun helm-mklist (obj)
  "Return OBJ as a list.
Otherwise make a list with one element OBJ."
  (if (and (listp obj) (not (functionp obj)))
      obj
    (list obj)))

(cl-defun helm-fast-remove-dups (seq &key (test 'eq))
  "Remove duplicates elements in list SEQ.

This is same as `remove-duplicates' but with memoisation.
It is much faster, especially in large lists.
A test function can be provided with TEST argument key.
Default is `eq'.
NOTE: Comparison of special Elisp objects (e.g., markers etc.)
fails because their printed representations which are stored in
hash-table can't be compared with with the real object in SEQ.
This is a bug in `puthash' which store the printable
representation of object instead of storing the object itself,
this to provide at the end a printable representation of
hashtable itself."
  (let ((table (make-hash-table :test test)))
    (mapcan (lambda (x)
              (unless (gethash x table)
                (list (puthash x x table))))
            seq)))

(defsubst helm--string-join (strings &optional separator)
  "Join all STRINGS using SEPARATOR."
  (mapconcat 'identity strings separator))

(defun helm--concat-regexps (regexp-list)
  "Return a regexp which matches any of the regexps in REGEXP-LIST."
  (if regexp-list
      (concat "\\(?:" (helm--string-join regexp-list "\\)\\|\\(?:") "\\)")
    "\\`\\'"))                          ; Match nothing

(defun helm-skip-entries (seq black-regexp-list &optional white-regexp-list)
  "Remove entries which match one of REGEXP-LIST from SEQ."
  (let ((black-regexp (helm--concat-regexps black-regexp-list))
        (white-regexp (helm--concat-regexps white-regexp-list)))
    (cl-loop for i in seq
             unless (and (stringp i)
                         (string-match-p black-regexp i)
                         (null
                          (string-match-p white-regexp i)))
             collect i)))

(defun helm-boring-directory-p (directory black-list)
  "Check if one regexp in BLACK-LIST matches DIRECTORY."
  (helm-awhile (helm-basedir (directory-file-name
                              (expand-file-name directory)))
    ;; Break at root to avoid infloop, root is / or on Windows
    ;; C:/ i.e. <volume>:/ (Bug#2308).
    (when (string-match-p "\\`[A-Za-z]?:?/\\'" it)
      (cl-return nil))
    (when (cl-loop for r in black-list
                   thereis (string-match-p
                            r (directory-file-name directory)))
      (cl-return t))
    (setq directory it)))

(defun helm-shadow-entries (seq regexp-list)
  "Put shadow property on entries in SEQ matching a regexp in REGEXP-LIST."
  (let ((face 'italic))
    (cl-loop for i in seq
          if (cl-loop for regexp in regexp-list
                   thereis (and (stringp i)
                                (string-match regexp i)))
          collect (propertize i 'face face)
          else collect i)))

(defun helm-remove-if-not-match (regexp seq)
  "Remove all elements of SEQ that don't match REGEXP."
  (cl-loop for s in seq
           for str = (cond ((symbolp s)
                            (symbol-name s))
                           ((consp s)
                            (car s))
                           (t s))
           when (string-match-p regexp str)
           collect s))

(defun helm-remove-if-match (regexp seq)
  "Remove all elements of SEQ that match REGEXP."
  (cl-loop for s in seq
           for str = (cond ((symbolp s)
                            (symbol-name s))
                           ((consp s)
                            (car s))
                           (t s))
           unless (string-match-p regexp str)
           collect s))

(defun helm-transform-mapcar (fn seq)
  "Apply function FN on all elements of list SEQ.
When SEQ is a list of cons cells apply FN on the cdr of each element,
keeping their car unmodified.

Examples:

    (helm-transform-mapcar \\='upcase \\='(\"foo\" \"bar\"))
    => (\"FOO\" \"BAR\")
    (helm-transform-mapcar \\='upcase \\='((\"1st\" . \"foo\") (\"2nd\" . \"bar\")))
    => ((\"1st\" . \"FOO\") (\"2nd\" . \"BAR\"))
"
  (cl-loop for elm in seq
           if (consp elm)
           collect (cons (car elm) (funcall fn (cdr elm)))
           else
           collect (funcall fn elm)))

(defun helm-append-at-nth (seq elm index)
  "Append ELM at INDEX in SEQ.
When INDEX is > to the SEQ length ELM is added at end of SEQ.
When INDEX is 0 or negative, ELM is added at beginning of SEQ.

Examples:

    (helm-append-at-nth \\='(a b c d) \\='(z) 2)
    =>(a b z c d)

    (helm-append-at-nth \\='(a b c d) \\='((x . 1) (y . 2)) 2)
    =>(a b (x . 1) (y . 2) c d)

    But this is not working:
    (helm-append-at-nth \\='(a b c d) \\='(x . 1) 2)
    =>Wrong type argument: listp, 1

NOTE: This function uses `append' internally, so ELM is expected
to be a list to be appended to SEQ, even if for convenience an
atom is supported as ELM value."
  (setq index (min (max index 0) (length seq))
        elm   (helm-mklist elm))
  (if (zerop index)
      (append elm seq)
    (let* ((end-part (nthcdr index seq))
           (len      (length end-part))
           (beg-part (butlast seq len)))
      (append beg-part elm end-part))))

(cl-defgeneric helm-take (seq n)
  "Return the first N elements of SEQ if SEQ is longer than N.
It is used for narrowing list of candidates to the
`helm-candidate-number-limit'."
  (if (> (length seq) n) (cl-subseq seq 0 n) seq))

(cl-defmethod helm-take ((seq list) n)
  "`helm-take' optimized for lists."
  (let ((store '()))
    (if (> n (length seq))
        seq
      (while (> (1+ (cl-decf n)) 0)
        (push (pop seq) store))
      (nreverse store))))

(defalias 'helm-take-first-elements 'helm-take)
(make-obsolete 'helm-take-first-elements 'helm-take "3.9.1")

(defun helm-source-by-name (name &optional sources)
  "Get a Helm source in SOURCES by NAME.

Optional argument SOURCES is a list of Helm sources which default
to `helm-sources'."
  (cl-loop with src-list = (if sources
                               (cl-loop for src in sources
                                        collect (if (listp src)
                                                    src
                                                    (symbol-value src)))
                               helm-sources)
           for source in src-list
           thereis (and (string= name (assoc-default 'name source)) source)))

(defun helm-make-actions (&rest args)
  "Build an alist with (NAME . ACTION) elements with each pairs in ARGS.
Where NAME is a string or a function returning a string or nil
and ACTION a function.
If NAME returns nil the pair is skipped.

\(fn NAME ACTION ...)"
  (cl-loop for (name fn) on args by #'cddr
           when (functionp name)
           do (setq name (funcall name))
           when name
           collect (cons name fn)))

(defun helm-closest-number-in-list (num list)
  "Return closest number to NUM found in LIST.
LIST is a list of numbers and NUM a number."
  (cl-loop for i in list
           for diff = (if (> num i) (- num i) (- i num))
           collect (cons diff i) into res
           minimize diff into min
           finally return (cdr (assq min res))))

(defun helm-group-candidates-by (candidates function &optional selection separate)
  "Group similar items in CANDIDATES according to FUNCTION.
Items not matching FUNCTION are grouped as well in a separate group.

Example:

    (setq B \\='(1 2 3 4 5 6 7 8 9))

    (helm-group-candidates-by B #\\='cl-oddp 2 \\='separate)
    => ((2 4 6 8) (1 3 5 7 9))

SELECTION specify where to start in CANDIDATES.
Similar candidates to SELECTION will be listed on top.

If SEPARATE is non-nil returns a list of groups i.e. a list of lists,
otherwise a plain list is returned."
  (cl-loop with sel = (or selection (helm-get-selection) "")
           with lst = (copy-sequence candidates)
           while lst
           for group = (cl-loop for c in lst
                                when (equal (funcall function c)
                                            (funcall function sel))
                                collect c into grp
                                and do (setq lst (delete c lst))
                                finally return (prog1 grp
                                                 (setq sel (car lst))))
           if separate collect group
           else append group))

(defun helm-reorganize-sequence-from-elm (sequence elm &optional reverse)
  "Reorganize SEQUENCE from ELM.

Examples:

    (helm-reorganize-sequence-from-elm \\='(a b c d e f g h i j k l) \\='e)
    => (f g h i j k l a b c d e)
    (helm-reorganize-sequence-from-elm \\='(a b c d e f g h i j k l) \\='e t)
    => (d c b a l k j i h g f e)
"
  (let* ((new-seq  (if reverse
                       (reverse sequence)
                     sequence))
         (pos      (1+ (cl-position elm new-seq :test 'equal))))
    (append (nthcdr pos new-seq) (helm-take new-seq pos))))

;;; Strings processing.
;;
(defun helm-stringify (elm)
  "Return the representation of ELM as a string.
ELM can be a string, a number or a symbol."
  (pcase elm
    ((pred stringp) elm)
    ((pred numberp) (number-to-string elm))
    ((pred symbolp) (symbol-name elm))))

(defun helm-substring (str width)
  "Return the substring of string STR from 0 to WIDTH.
Handle multibyte characters by moving by columns."
  (with-temp-buffer
    (save-excursion
      (insert str))
    (move-to-column width)
    (buffer-substring (pos-bol) (point))))

(defun helm-substring-by-width (str width &optional endstr)
  "Truncate string STR to end at column WIDTH.
Similar to `truncate-string-to-width'.
Add ENDSTR at end of truncated STR.
Add spaces at end if needed to reach WIDTH when STR is shorter
than WIDTH."
  (cl-loop for ini-str = str
        then (substring ini-str 0 (1- (length ini-str)))
        for sw = (string-width ini-str)
        when (<= sw width) return
        (concat ini-str endstr (make-string (- width sw) ? ))))

(defun helm-string-multibyte-p (str)
  "Check if string STR contains multibyte characters."
  (cl-loop for c across str
        thereis (> (char-width c) 1)))

(defun helm-get-pid-from-process-name (process-name)
  "Get pid from running process PROCESS-NAME."
  ;; Protect system processes calls (Issue #2497)
  ;; Ensure `list-system-processes' and `process-attributes' don't run
  ;; on remote (only Emacs-28/29+).
  (cl-loop with default-directory = temporary-file-directory
           with process-list = (list-system-processes)
           for pid in process-list
           for process = (assoc-default 'comm (process-attributes pid))
           when (and process (string-match process-name process))
           return pid))

(defun helm-ff-find-printers ()
  "Return a list of available printers on Unix systems."
  (when (executable-find "lpstat")
    (let ((printer-list (with-temp-buffer
                          (call-process "lpstat" nil t nil "-a")
                          (split-string (buffer-string) "\n"))))
      (cl-loop for p in printer-list
            for printer = (car (split-string p))
            when printer
            collect printer))))

(defun helm-region-active-p ()
  (and transient-mark-mode mark-active (/= (mark) (point))))

(defun helm-quote-whitespace (candidate)
  "Quote whitespace, if some, in string CANDIDATE."
  (replace-regexp-in-string " " "\\\\ " candidate))

(defun helm-current-line-contents ()
  "Current line string without properties."
  (buffer-substring-no-properties (pos-bol) (pos-eol)))

(defun helm--replace-regexp-in-buffer-string (regexp rep str &optional fixedcase literal subexp start)
  "Replace REGEXP by REP in string STR.

Same as `replace-regexp-in-string' but handle properly REP as
function with SUBEXP specified.

E.g.:

    (helm--replace-regexp-in-buffer-string
     \"e\\\\(m\\\\)acs\" \\='upcase \"emacs\" t nil 1)
    => \"eMacs\"

    (replace-regexp-in-string
     \"e\\\\(m\\\\)acs\" \\='upcase \"emacs\" t nil 1)
    => \"eEMACSacs\"

Also START argument behaves as expected unlike
`replace-regexp-in-string'.

E.g.:

    (helm--replace-regexp-in-buffer-string \"f\" \"r\" \"foofoo\" t nil nil 3)
    => \"fooroo\"

    (replace-regexp-in-string \"f\" \"r\" \"foofoo\" t nil nil 3)
    => \"roo\"

Unlike `replace-regexp-in-string' this function is buffer-based
implemented i.e. replacement is computed inside a temp buffer, so
REGEXP should be used differently than with
`replace-regexp-in-string'.

NOTE: This function is used internally for
`helm-ff-query-replace-on-filenames' and builded for this.
You should use `replace-regexp-in-string' instead unless the
behaviour of this function is really needed."
  (with-temp-buffer
    (insert str)
    (goto-char (or start (point-min)))
    (while (re-search-forward regexp nil t)
      (replace-match (cond ((and (functionp rep) subexp)
                            (funcall rep (match-string subexp)))
                           ((functionp rep)
                            (funcall rep str))
                           (t rep))
                     fixedcase literal nil subexp))
    (buffer-string)))

(defun helm-url-unhex-string (str)
  "Same as `url-unhex-string' but ensure STR is completely decoded."
  (setq str (or str ""))
  (with-temp-buffer
    (save-excursion (insert str))
    (while (re-search-forward "%[A-Za-z0-9]\\{2\\}" nil t)
      (replace-match (byte-to-string (string-to-number
                                      (substring (match-string 0) 1)
                                      16))
                     t t)
      ;; Restart from beginning until string is completely decoded.
      (goto-char (point-min)))
    (decode-coding-string (buffer-string) 'utf-8)))

(defun helm-read-answer (prompt answer-list)
  "Prompt user for an answer.
Arg PROMPT is the prompt to present user the different possible
answers, ANSWER-LIST is a list of strings.
If user enters an answer which is one of ANSWER-LIST return this
answer, otherwise keep prompting for a valid answer.
Note that answer should be a single char, only short answer are
accepted.

Example:

    (pcase (helm-read-answer
             \"answer [y,n,!,q]: \"
             \\='(\"y\" \"n\" \"!\" \"q\"))
       (\"y\" \"yes\")
       (\"n\" \"no\")
       (\"!\" \"all\")
       (\"q\" \"quit\"))

"
  (helm-awhile (read-key (propertize prompt 'face 'minibuffer-prompt))
    (let ((str (and (characterp it) (string it))))
      (if (and str (member str answer-list))
          (cl-return str)
        (message "Please answer by %s" (mapconcat 'identity answer-list ", "))
        (sit-for 1)))))

(defun helm-read-answer-dolist-with-action (prompt list action)
  "Read answer with PROMPT and execute ACTION on each element of LIST.

Argument PROMPT is a format spec string e.g. \"Do this on %s?\"
which take each elements of LIST as argument, no need to provide
the help part i.e. [y,n,!,q] it will be already added.

While looping through LIST, ACTION is executed on each elements
differently depending of answer:

- y  Execute ACTION on element.
- n  Skip element.
- !  Don't ask anymore and execute ACTION on remaining elements.
- q  Skip all remaining elements."
  (let (dont-ask)
    (catch 'break
      (dolist (elm list)
        (if dont-ask
            (funcall action elm)
          (pcase (helm-read-answer
                  (format (concat prompt "[y,n,!,q]") elm)
                  '("y" "n" "!" "q"))
            ("y" (funcall action elm))
            ("n" (ignore))
            ("!" (prog1
                     (funcall action elm)
                   (setq dont-ask t)))
            ("q" (throw 'break nil))))))))

(defsubst helm-string-numberp (str)
  "Return non nil if string STR represent a number."
  (cl-assert (stringp str) t)
  (or (cl-loop for c across str always (char-equal c ?0))
      (not (zerop (string-to-number str)))))

;;; Symbols routines
;;
(defun helm-symbolify (str-or-sym)
  "Get symbol of STR-OR-SYM."
  (cond ((symbolp str-or-sym)
         str-or-sym)
        ((equal str-or-sym "") nil)
        (t (intern str-or-sym))))

(defun helm-symbol-name (obj)
  (if (or (and (consp obj) (functionp obj))
          (byte-code-function-p obj)
          (helm-subr-native-elisp-p obj))
      "Anonymous"
      (symbol-name obj)))

(defun helm-describe-class (class)
  "Display documentation of Eieio CLASS, a symbol or a string."
  (let ((advicep (advice-member-p #'helm-source--cl--print-table 'cl--print-table)))
    (unless advicep
      (advice-add 'cl--print-table :override #'helm-source--cl--print-table '((depth . 100))))
    (unwind-protect
         (if (fboundp 'cl-describe-type)
             (cl-describe-type (helm-symbolify class))
           (let ((helm-describe-function-function 'describe-function))
             (helm-describe-function (helm-symbolify class))))
      (unless advicep
        (advice-remove 'cl--print-table #'helm-source--cl--print-table)))))

(defun helm-describe-function (func)
  "Display documentation of FUNC, a symbol or string."
  (cl-letf (((symbol-function 'message) #'ignore))
    (funcall helm-describe-function-function (helm-symbolify func))))

(defun helm-describe-variable (var)
  "Display documentation of VAR, a symbol or a string."
  (cl-letf (((symbol-function 'message) #'ignore))
    (funcall helm-describe-variable-function (helm-symbolify var))))

(defun helm-describe-face (face)
  "Display documentation of FACE, a symbol or a string."
  (let ((faces (helm-marked-candidates)))
    (cl-letf (((symbol-function 'message) #'ignore))
      (describe-face (if (cdr faces)
                         (mapcar 'helm-symbolify faces)
                         (helm-symbolify face))))))

(defun helm-elisp--persistent-help (candidate fun &optional name)
  "Used to build persistent actions describing CANDIDATE with FUN.
Argument NAME is used internally to know which command to use
when symbol CANDIDATE refers at the same time to a variable and a
function.
See `helm-elisp-show-help'."
  (let ((hbuf (get-buffer (help-buffer))))
    (cond  ((helm-follow-mode-p)
            (if name
                (funcall fun candidate name)
                (funcall fun candidate)))
           ((or (and (helm-get-attr 'help-running-p)
                     (string= candidate (helm-get-attr 'help-current-symbol))))
            (progn
              ;; When started from a help buffer,
              ;; Don't kill this buffer as it is helm-current-buffer.
              (unless (equal hbuf helm-current-buffer)
                (kill-buffer hbuf)
                (set-window-buffer (get-buffer-window hbuf)
                                   ;; It is generally
                                   ;; helm-current-buffer but it may
                                   ;; be another buffer when helm have
                                   ;; been started from a dedicated window.
                                   (if helm--buffer-in-new-frame-p
                                       helm-current-buffer
                                     helm-persistent-action-window-buffer)))
              (helm-set-attr 'help-running-p nil))
            ;; Force running update hook to may be delete
            ;; helm-persistent-action-display-window, this is done in
            ;; helm-persistent-action-display-window (the function).
            (unless helm--buffer-in-new-frame-p
              (helm-update (regexp-quote (helm-get-selection)))))
           (t
            (if name
                (funcall fun candidate name)
                (funcall fun candidate))
            (helm-set-attr 'help-running-p t)))
    (helm-set-attr 'help-current-symbol candidate)))

(defcustom helm-find-function-default-project nil
  "Default directories to search symbols definitions from `helm-apropos'.
A list of directories or a single directory name.
Helm will allow you selecting one of those directories with `M-n' when
using a prefix arg with the `find-function' action in `helm-apropos'.
This is a good idea to add the directory names of the projects you are
working on to quickly jump to the definitions in the project source
files instead of jumping to the loaded files located in `load-path'."
  :type '(choice (repeat string)
                 string)
  :group 'helm-elisp)

(defun helm-find-function-noselect (func &optional root-dir type)
  "Find FUNC definition without selecting buffer.
FUNC can be a symbol or a string.
Instead of looking in LOAD-PATH to find library, this function
search in all subdirs of ROOT-DIR, if ROOT-DIR is unspecified ask for
it with completion.
TYPE when nil specify function, for other values see
`find-function-regexp-alist'."
  (require 'find-func)
  (let* ((sym (helm-symbolify func))
         (dir (or root-dir (helm-read-file-name
                            "Project directory: "
                            :test 'file-directory-p
                            :default (helm-mklist helm-find-function-default-project)
                            :must-match t)))
         (find-function-source-path
          (cons dir (helm-walk-directory dir
                                         :directories 'only
                                         :path 'full)))
         (symbol-lib (helm-acase type
                       ((defvar defface)
                        (or (symbol-file sym it)
                            (help-C-file-name sym 'var)))
                       ;; Sometimes e.g. with prefix key symbols
                       ;; `find-function-library' returns a list of only one
                       ;; element, the symbol itself i.e. no library.
                       (t (cdr (find-function-library sym)))))
         (library (and symbol-lib
                       (find-library-name
                        (helm-basename symbol-lib t)))))
    (if library
        (find-function-search-for-symbol sym type library)
      (error "Don't know where `%s' is defined" sym))))

(defun helm-find-function (func)
  "Try to jump to FUNC definition.
With a prefix arg ask for the project directory to search in instead of
using LOAD-PATH."
  (if (not helm-current-prefix-arg)
      (find-function (helm-symbolify func))
    (let ((place (helm-find-function-noselect func)))
      (if (cdr place)
          (progn
            (switch-to-buffer (car place)) (goto-char (cdr place)))
        (helm-aif (car place)
            (message "Couldn't find Function `%s' in `%s'"
                     func (buffer-name it))
          (message "Couldn't find Function `%s'" func))))))

(defun helm-find-variable (var)
  "Try to jump to VAR definition.
With a prefix arg ask for the project directory to search in instead of
using LOAD-PATH."
  (if (not helm-current-prefix-arg)
      (find-variable (helm-symbolify var))
    (let ((place (helm-find-function-noselect var nil 'defvar)))
      (when place
        (switch-to-buffer (car place)) (goto-char (cdr place))))))

(defun helm-find-face-definition (face)
  "Try to jump to FACE definition.
With a prefix arg ask for the project directory to search in instead of
using LOAD-PATH."
  (if (not helm-current-prefix-arg)
      (find-face-definition (helm-symbolify face))
    (let ((place (helm-find-function-noselect face nil 'defface)))
      (when place
        (switch-to-buffer (car place)) (goto-char (cdr place))))))

(defun helm-kill-new (candidate &optional replace)
  "CANDIDATE is symbol or string.
See `kill-new' for argument REPLACE."
  (kill-new (helm-stringify candidate) replace))

(defun helm-group-p (symbol)
  "Return non nil when SYMBOL is a group."
  (or (and (get symbol 'custom-loads)
           (not (get symbol 'custom-autoload)))
      (get symbol 'custom-group)))


;;; Modes
;;
(defun helm-same-major-mode-p (start-buffer alist)
  "Decide if current-buffer is related to START-BUFFER.
Argument ALIST is an alist of associated major modes."
  ;; START-BUFFER is the current-buffer where we start searching.
  ;; Determine the major-mode of START-BUFFER as `cur-maj-mode'.
  ;; Each time the loop go in another buffer we try from this buffer
  ;; to determine if its `major-mode' is:
  ;; - same as the `cur-maj-mode'
  ;; - derived from `cur-maj-mode' and from
  ;;   START-BUFFER if its mode is derived from the one in START-BUFFER.
  ;; - have an assoc entry (major-mode . cur-maj-mode)
  ;; - have an rassoc entry (cur-maj-mode . major-mode)
  ;; - check if one of these entries inherit from another one in
  ;;   `alist'.
  (let* ((cur-maj-mode  (with-current-buffer start-buffer major-mode))
         (maj-mode      major-mode)
         (c-assoc-mode  (assq cur-maj-mode alist))
         (c-rassoc-mode (rassq cur-maj-mode alist))
         (o-assoc-mode  (assq major-mode alist))
         (o-rassoc-mode (rassq major-mode alist))
         (cdr-c-assoc-mode (cdr c-assoc-mode))
         (cdr-o-assoc-mode (cdr o-assoc-mode)))
    (or (eq major-mode cur-maj-mode)
        (derived-mode-p cur-maj-mode)
        (with-current-buffer start-buffer
          (derived-mode-p maj-mode))
        (or (eq cdr-c-assoc-mode major-mode)
            (eq (car c-rassoc-mode) major-mode)
            (eq (cdr (assq cdr-c-assoc-mode alist))
                major-mode)
            (eq (car (rassq cdr-c-assoc-mode alist))
                major-mode))
        (or (eq cdr-o-assoc-mode cur-maj-mode)
            (eq (car o-rassoc-mode) cur-maj-mode)
            (eq (cdr (assq cdr-o-assoc-mode alist))
                cur-maj-mode)
            (eq (car (rassq cdr-o-assoc-mode alist))
                cur-maj-mode)))))

;;; Source processing
;;
(defun helm-map-candidates-in-source (src fn pred)
  "Map over all candidates in SRC and execute FN if PRED returns non nil.
Arg FN and PRED are functions called with current display part of
candidate as arg."
  (declare (indent 1))
  (save-excursion
    (goto-char (helm-get-previous-header-pos))
    (helm-next-line)
    (let* ((next-head (helm-get-next-header-pos))
           (end       (and next-head
                           (save-excursion
                             (goto-char next-head)
                             (forward-line -1)
                             (point))))
           (maxpoint  (or end (point-max))))
      (while (< (point) maxpoint)
        (helm-mark-current-line)
        (let ((cand (helm-get-selection nil 'withprop src)))
          (when (funcall pred cand)
            (funcall fn cand)))
        (forward-line 1) (end-of-line)))))

;;; Files routines
;;
(defun helm-file-name-sans-extension (filename)
  "Same as `file-name-sans-extension' but remove all extensions."
  (helm-aif (file-name-sans-extension filename)
      ;; Start searching at index 1 for files beginning with a dot
      ;; (bug#1335).
      (if (string-match "\\." (helm-basename it) 1)
          (helm-file-name-sans-extension it)
          it)))

(defsubst helm-file-name-extension (file)
  "Returns FILE extension if it is not a number."
  (helm-aif (file-name-extension file)
      (and (not (helm-string-numberp it)) it)))

(defun helm-basename (fname &optional ext)
  "Print FNAME with any leading directory components removed.
If specified, also remove filename extension EXT.
If FNAME is a directory EXT arg is ignored.

Arg EXT can be specified as a string, a number or `t' .
When specified as a string, this string is stripped from end of FNAME.
e.g. (helm-basename \"tutorial.el.gz\" \".el.gz\") => tutorial.
When `t' no checking of `file-name-extension' is done and the first
extension is removed unconditionally with `file-name-sans-extension'.
e.g. (helm-basename \"tutorial.el.gz\" t) => tutorial.el.
When a number, remove that many times extensions from FNAME until FNAME ends
with its real extension which is by default \".el\".
e.g. (helm-basename \"tutorial.el.gz\" 2) => tutorial
To specify the extension where to stop use a cons cell where the cdr is a regexp
matching extension e.g. (2 . \\\\.py$).
e.g. (helm-basename \"~/ucs-utils-6.0-delta.py.gz\" \\='(2 . \"\\\\.py\\\\\\='\"))
=>ucs-utils-6.0-delta."
  (let ((non-essential t)
        (ext-regexp (cond ((consp ext) (cdr ext))
                          ((numberp ext) "\\.el\\'")
                          (t ext)))
        result)
    (cond ((or (null ext) (file-directory-p fname))
           (file-name-nondirectory (directory-file-name fname)))
          ((or (numberp ext) (consp ext))
           (cl-dotimes (_ (if (consp ext) (car ext) ext))
             (let ((bn (file-name-nondirectory (or result fname))))
               (helm-aif (file-name-sans-extension bn)
                   (if (string-match-p ext-regexp bn)
                       (cl-return (setq result (file-name-sans-extension bn)))
                     (setq result (file-name-sans-extension bn))))))
           result)
          ((eq t ext)
           (file-name-sans-extension (file-name-nondirectory fname)))
          ((stringp ext)
           (replace-regexp-in-string (concat (regexp-quote ext) "\\'") ""
                                     (file-name-nondirectory fname))))))

(defun helm-basedir (fname &optional parent)
  "Return the base directory of FNAME ending by a slash.
If PARENT is specified and FNAME is a directory return the parent
directory of FNAME.
If PARENT is not specified but FNAME doesn't end by a slash, the returned value
is same as with PARENT."
  (helm-aif (and fname
                 (or (and (string= fname "~") "~")
                     (file-name-directory
                      (if parent
                          (directory-file-name fname)
                        fname))))
      (file-name-as-directory it)))

(defun helm-current-directory ()
  "Return current-directory name at point.
Useful in dired buffers when there is inserted subdirs."
  (expand-file-name
   (if (eq major-mode 'dired-mode)
       (dired-current-directory)
       default-directory)))

(defun helm-shadow-boring-files (files)
  "Files matching `helm-boring-file-regexp' will be
displayed with the `file-name-shadow' face if available."
  (helm-shadow-entries files helm-boring-file-regexp-list))

(defun helm-skip-boring-files (files)
  "Files matching `helm-boring-file-regexp' will be skipped."
  (helm-skip-entries files helm-boring-file-regexp-list))

(defun helm-skip-current-file (files)
  "Current file will be skipped."
  (remove (buffer-file-name helm-current-buffer) files))

(defun helm-w32-pathname-transformer (args)
  "Change undesirable features of windows pathnames to ones more acceptable to
other candidate transformers."
  (if (eq system-type 'windows-nt)
      (helm-transform-mapcar
       (lambda (x)
         (replace-regexp-in-string
          "/cygdrive/\\(.\\)" "\\1:"
          (replace-regexp-in-string "\\\\" "/" x)))
       args)
    args))

(defun helm-w32-prepare-filename (file)
  "Convert filename FILE to something usable by external w32 executables."
  (replace-regexp-in-string ; For UNC paths
   "/" "\\"
   (replace-regexp-in-string ; Strip cygdrive paths
    "/cygdrive/\\(.\\)" "\\1:"
    file nil nil) nil t))

(defun helm-w32-shell-execute-open-file (file)
  (with-no-warnings
    (w32-shell-execute "open" (helm-w32-prepare-filename file))))

;; Same as `vc-directory-exclusion-list'.
(defvar helm-walk-ignore-directories
  '("SCCS/" "RCS/" "CVS/" "MCVS/" ".svn/" ".git/" ".hg/" ".bzr/"
    "_MTN/" "_darcs/" "{arch}/" ".gvfs/"))

(defsubst helm--dir-file-name (file dir)
  (expand-file-name
   (substring file 0 (1- (length file))) dir))

(defsubst helm--dir-name-p (str)
  (char-equal (aref str (1- (length str))) ?/))

(cl-defun helm-walk-directory (directory &key (path 'basename)
                                           directories
                                           match skip-subdirs
                                           noerror)
  "Walk through DIRECTORY tree.

Argument PATH can be one of basename, relative, full, or a
function called on file name, default to basename.

Argument DIRECTORIES when t return also directories names,
otherwise skip directories names, with a value of `only' returns
only subdirectories, i.e. files are skipped.

Argument MATCH is a regexp matching files or directories.

Argument SKIP-SUBDIRS when t will skip
`helm-walk-ignore-directories', otherwise if it is given as a
list of directories, this list will be used instead of
`helm-walk-ignore-directories'.

Argument NOERROR when t will skip directories which are not
accessible."
  (let ((fn (cl-case path
               (basename 'file-name-nondirectory)
               (relative 'file-relative-name)
               (full     'identity)
               (t        path)))) ; A function.
    (setq skip-subdirs (if (listp skip-subdirs)
                           skip-subdirs
                           helm-walk-ignore-directories))
    (cl-labels ((ls-rec (dir)
                  (unless (file-symlink-p dir)
                    (cl-loop for f in (sort (file-name-all-completions "" dir)
                                            'string-lessp)
                             unless (member f '("./" "../"))
                             ;; A directory.
                             ;; Use `helm--dir-file-name' to remove the final slash.
                             ;; Needed to avoid infloop on directory symlinks.
                             if (and (helm--dir-name-p f)
                                     (helm--dir-file-name f dir))
                             nconc
                             (unless (or (member f skip-subdirs)
                                         (and noerror
                                              (not (file-accessible-directory-p it))))
                               (if (and directories
                                        (or (null match)
                                            (string-match match f)))
                                   (nconc (list (concat (funcall fn it) "/"))
                                          (ls-rec it))
                                   (ls-rec it)))
                             ;; A regular file.
                             else nconc
                             (when (and (null (eq directories 'only))
                                        (or (null match) (string-match match f)))
                               (list (funcall fn (expand-file-name f dir))))))))
      (ls-rec directory))))

(defun helm-file-expand-wildcards (pattern &optional full)
  "Same as `file-expand-wildcards' but allow recursion.
Recursion happens when PATTERN starts with two stars.
Directories expansion is not supported."
  (let ((bn (helm-basename pattern))
        (case-fold-search nil))
    (if (and helm-file-globstar
             (string-match "\\`\\*\\{2\\}\\(.*\\)" bn))
        (helm-walk-directory (helm-basedir pattern)
                             :path (cl-case full
                                     (full 'full)
                                     (relative 'relative)
                                     ((basename nil) 'basename)
                                     (t 'full))
                             :directories nil
                             :match (or (helm-wildcard-to-regexp bn)
                                        (wildcard-to-regexp bn))
                             :skip-subdirs t)
      (helm-aif (helm-wildcard-to-regexp bn)
          (directory-files (helm-basedir pattern) full it)
        ;; `file-expand-wildcards' fails to expand weird directories
        ;; like "[ foo.zz ] bar.*.avi", fallback to `directory-files'
        ;; in such cases.
        (or (file-expand-wildcards pattern full)
            (directory-files (helm-basedir pattern)
                             full (wildcard-to-regexp bn)))))))

(defun helm-wildcard-to-regexp (wc)
  "Transform wilcard WC like \"**.{jpg,jpeg}\" in REGEXP."
  (when (string-match ".*\\(\\*\\{1,2\\}\\)\\.[{]\\(.*\\)[}]\\'" wc)
    (format ".*\\.\\(%s\\)$"
            (replace-regexp-in-string
             "," "\\\\|" (match-string 2 wc)))))

(defun helm-locate-lib-get-summary (file)
  "Extract library description from FILE."
  (let* ((shell-file-name "sh")
         (shell-command-switch "-c")
         (cmd "%s %s | head -n1 | awk 'match($0,\"%s\",a) {print a[2]}'\
 | awk -F ' -*-' '{print $1}'")
         (regexp "^;;;(.*) ---? (.*)$")
         (desc (shell-command-to-string
                (format cmd
                        (if (string-match-p "\\.gz\\'" file)
                            "gzip -c -q -d" "cat")
                        (shell-quote-argument file)
                        regexp))))
    (if (string= desc "")
        "Not documented"
      (replace-regexp-in-string "\n" "" desc))))

;;; helm internals
;;
(defun helm-set-pattern (pattern &optional noupdate)
  "Set minibuffer contents to PATTERN.
If optional NOUPDATE is non-nil, the Helm buffer is not changed."
  (with-selected-window (or (active-minibuffer-window) (minibuffer-window))
    (delete-minibuffer-contents)
    (insert pattern))
  (when noupdate
    (setq helm-pattern pattern)))

(defun helm-minibuffer-completion-contents ()
  "Return the user input in a minibuffer before point as a string.
That is what completion commands operate on."
  (buffer-substring (field-beginning) (point)))

(defmacro with-helm-buffer (&rest body)
  "Eval BODY inside `helm-buffer'."
  (declare (indent 0) (debug t))
  `(with-current-buffer (helm-buffer-get)
     ,@body))

(defmacro with-helm-current-buffer (&rest body)
  "Eval BODY inside `helm-current-buffer'."
  (declare (indent 0) (debug t))
  `(with-current-buffer (or (and (buffer-live-p helm-current-buffer)
                                 helm-current-buffer)
                            (setq helm-current-buffer
                                  (current-buffer)))
     ,@body))

(defun helm-buffer-get ()
  "Return `helm-action-buffer' if shown otherwise `helm-buffer'."
  (if (helm-action-window)
      helm-action-buffer
    helm-buffer))

(defun helm-window ()
  "Window of `helm-buffer'."
  (get-buffer-window (helm-buffer-get) 0))

(defun helm-action-window ()
  "Window of `helm-action-buffer'."
  (get-buffer-window helm-action-buffer 'visible))

(defmacro with-helm-window (&rest body)
  "Be sure BODY is excuted in the helm window."
  (declare (indent 0) (debug t))
  `(with-selected-window (helm-window)
     ,@body))

(defmacro helm-without-follow (&rest body)
  "Ensure BODY runs without following.
I.e. when using `helm-next-line' and friends in BODY."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'helm-follow-mode-p)
             (lambda (&optional _) nil)))
    (let (helm-follow-mode-persistent)
      (progn ,@body))))

(defun helm-candidate-prefixed-p (candidate)
  "Return non nil when CANDIDATE is prefixed.

Candidates files are prefixed with [+] or a specific icon when candidate is a
non existing file, in other places candidates may be prefixed with an unknown
symbol [?], these candidate have the text property <helm-new-file> or <unknown>
property."
  (or (get-text-property 0 'helm-new-file candidate)
      (get-text-property 0 'unknown candidate)))

;; Completion styles related functions
;;
(defun helm--setup-completion-styles-alist ()
  (cl-pushnew '(helm helm-completion-try-completion
                     helm-completion-all-completions
                     "helm multi completion style.")
              completion-styles-alist
              :test 'equal)
  (unless (assq 'flex completion-styles-alist)
    ;; Add helm-fuzzy style only if flex is not available.
    (cl-pushnew '(helm-flex helm-flex-completion-try-completion
                            helm-flex-completion-all-completions
                            "helm flex completion style.\nProvide flex matching for emacs-26.")
                completion-styles-alist
                :test 'equal)))

(defvar helm-blacklist-completion-styles '(emacs21 emacs22))
(defun helm--prepare-completion-styles (&optional com-or-mode styles)
  "Return a suitable list of styles for `completion-styles'.

When `helm-completion-style' is not `emacs' the Emacs vanilla
default `completion-styles' is used except for
`helm-dynamic-completion' which uses inconditionally `emacs' as
value for `helm-completion-style'.

If styles are specified in `helm-completion-styles-alist' for a
particular mode, use these styles for the corresponding mode.
If COM-OR-MODE (a mode or a command) is specified it is used to find the
corresponding styles in `helm-completion-styles-alist'.

If STYLES is specified as a list of styles suitable for
`completion-styles' these styles are used in the given order.
Otherwise helm style is added to `completion-styles' always after
flex or helm-flex completion style if present."
  ;; For `helm-completion-style' and `helm-completion-styles-alist'.
  (require 'helm-mode)
  (let ((from (if com-or-mode com-or-mode major-mode)))
    (if (memq helm-completion-style '(helm helm-fuzzy))
        ;; Keep default settings, but probably nil is fine as well.
        '(basic partial-completion emacs22)
      (or
       styles
       (pcase (cdr (assq from helm-completion-styles-alist))
         (`(,_l . ,ll) ll))
       ;; We need to have flex always behind helm, otherwise
       ;; when matching against e.g. '(foo foobar foao frogo bar
       ;; baz) with pattern "foo" helm style if before flex will
       ;; return foo and foobar only defeating flex that would
       ;; return foo foobar foao and frogo.
       (let* ((wflex (car (or (assq 'flex completion-styles-alist)
                              (assq 'helm-flex completion-styles-alist))))
              (styles (append (and (memq wflex completion-styles)
                                   (list wflex))
                              (cl-loop for s in completion-styles
                                       unless (or (memq s helm-blacklist-completion-styles)
                                                  (memq wflex completion-styles))
                                       collect s))))
         (helm-append-at-nth
          styles '(helm)
          (if (memq wflex completion-styles)
              1 0)))))))

(defun helm-dynamic-completion (collection predicate &optional point metadata nomode styles)
  "Build a completion function for `helm-pattern' in COLLECTION.

Only the elements of COLLECTION that satisfy PREDICATE are considered.

Argument POINT is the same as in `completion-all-completions' and
is meaningful only when using some kind of `completion-at-point'.

The return value is a list of completions that may be sorted by
the sort function provided by the completion-style in
use (emacs-27 only), otherwise (emacs-26) the sort function has
to be provided if needed either with an FCT function in source or
by passing the sort function with METADATA
E.g.: (metadata (display-sort-function . foo)).

If you don't want the sort fn provided by style to kick
in (emacs-27) you can use as metadata value the symbol `nosort'.

Example:

    (helm :sources (helm-build-sync-source \"test\"
                     :candidates (helm-dynamic-completion
                                  \\='(foo bar baz foab)
                                  \\='symbolp)
                     :match-dynamic t)
          :buffer \"*helm test*\")

When argument NOMODE is non nil don't use `completion-styles' as
specified in `helm-completion-styles-alist' for specific modes.

When STYLES is specified use these `completion-styles', see
`helm--prepare-completion-styles'.

Also `helm-completion-style' settings have no effect here,
`emacs' being used inconditionally as value."
  (lambda ()
    (let* (;; Force usage of emacs style otherwise
           ;; helm--prepare-completion-styles will reset
           ;; completion-styles to default value i.e. (basic partial
           ;; emacs22).
           (helm-completion-style 'emacs)
           (completion-styles
            (with-helm-current-buffer
              (helm--prepare-completion-styles nomode styles)))
           (completion-flex-nospace t)
           (nosort (eq metadata 'nosort))
           (compsfn (lambda (str pred _action)
                      (let* ((completion-ignore-case (helm-set-case-fold-search))
                             (comps (completion-all-completions
                                     str
                                     (if (functionp collection)
                                         (funcall collection str pred t)
                                       collection)
                                     pred
                                     (or point 0)
                                     (or (and (listp metadata) metadata)
                                         (setq metadata '(metadata)))))
                             (last-data (last comps))
                             (sort-fn (unless nosort
                                        (completion-metadata-get
                                         metadata 'display-sort-function)))
                             all)
                        (when (cdr last-data)
                          (setcdr last-data nil))
                        (setq all (copy-sequence comps))
                        (if (and sort-fn (> (length str) 0))
                            (funcall sort-fn all)
                          all)))))
      ;; Ensure circular objects are removed.
      (complete-with-action t compsfn helm-pattern predicate))))

(defun helm-guess-filename-at-point ()
  (with-helm-current-buffer
    ;; Ensure to disable the evil `ffap-machine-at-point' which may run here as
    ;; `file-name-at-point-functions' contains by default
    ;; `ffap-guess-file-name-at-point' See bug#2574.
    ;; Use same value as in Emacs-29 for next 3 vars to ensure `ffap-machine-p'
    ;; never ping.
    (let ((ffap-machine-p-known 'accept)
          (ffap-machine-p-local 'reject)
          (ffap-machine-p-unknown 'reject))
      (run-hook-with-args-until-success 'file-name-at-point-functions))))

;; Yank text at point.
;;
;;
(defun helm-yank-text-at-point (arg)
  "Yank text at point in `helm-current-buffer' into minibuffer."
  (interactive "p")
  (with-helm-current-buffer
    (let ((fwd-fn (or helm-yank-text-at-point-function #'forward-word))
          diff)
      ;; Start to initial point if C-w have never been hit.
      (unless helm-yank-point
        (setq helm-yank-point (car helm-current-position)))
      (save-excursion
        (goto-char helm-yank-point)
        (helm-set-pattern
         (if (< arg 0)
             (with-temp-buffer
               (insert helm-pattern)
               (let ((end (point-max)))
                 (goto-char end)
                 (funcall fwd-fn -1)
                 (setq diff (- end (point)))
                 (delete-region (point) end)
                 (buffer-string)))
             (funcall fwd-fn arg)
             (concat
              ;; Allow yankink beyond eol allow inserting e.g long
              ;; urls in mail buffers.
              helm-pattern (replace-regexp-in-string
                            "\\`\n" ""
                            (buffer-substring-no-properties
                             helm-yank-point (point))))))
        (setq helm-yank-point (if diff (- (point) diff) (point)))))))
(put 'helm-yank-text-at-point 'helm-only t)

(defun helm-undo-yank-text-at-point ()
  "Undo last entry added by `helm-yank-text-at-point'."
  (interactive)
  (helm-yank-text-at-point -1))
(put 'helm-undo-yank-text-at-point 'helm-only t)

(defun helm-reset-yank-point ()
  (setq helm-yank-point nil))

(add-hook 'helm-cleanup-hook 'helm-reset-yank-point)
(add-hook 'helm-after-initialize-hook 'helm-reset-yank-point)

;;; Ansi
;;
;;
(defvar helm--ansi-color-regexp
  "\033\\[\\(K\\|[0-9;]*m\\)")
(defvar helm--ansi-color-drop-regexp
  "\033\\[\\([ABCDsuK]\\|[12][JK]\\|=[0-9]+[hI]\\|[0-9;]*[Hf]\\)")
(defun helm--ansi-color-apply (string)
  "A version of `ansi-color-apply' immune to upstream changes.

Similar to the emacs-24.5 version without support to
`ansi-color-context' which is buggy in Emacs.

Modify also `ansi-color-regexp' by using own variable
`helm--ansi-color-regexp' that matches whole STRING.

This is needed to provide compatibility for both emacs-25 and
emacs-24.5 as emacs-25 version of `ansi-color-apply' is partially
broken."
  (require 'ansi-color)
  (let ((start 0)
        codes end escape-sequence
        result colorized-substring)
    ;; Find the next escape sequence.
    (while (setq end (string-match helm--ansi-color-regexp string start))
      (setq escape-sequence (match-string 1 string))
      ;; Colorize the old block from start to end using old face.
      (when codes
        (put-text-property
         start end 'font-lock-face (ansi-color--find-face codes) string))
      (setq colorized-substring (substring string start end)
            start (match-end 0))
      ;; Eliminate unrecognized ANSI sequences.
      (while (string-match helm--ansi-color-drop-regexp colorized-substring)
        (setq colorized-substring
              (replace-match "" nil nil colorized-substring)))
      (push colorized-substring result)
      ;; Create new face, by applying escape sequence parameters.
      (setq codes (ansi-color-apply-sequence escape-sequence codes)))
    ;; If the rest of the string should have a face, put it there.
    (when codes
      (put-text-property
       start (length string)
       'font-lock-face (ansi-color--find-face codes) string))
    ;; Save the remainder of the string to the result.
    (if (string-match "\033" string start)
        (push (substring string start (match-beginning 0)) result)
        (push (substring string start) result))
    (apply 'concat (nreverse result))))

(when (< emacs-major-version 26)
  (advice-add 'ansi-color-apply :override #'helm--ansi-color-apply))


;;; Fontlock
(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   '(("(\\<\\(with-helm-after-update-hook\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-temp-hook\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-window\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-current-buffer\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-buffer\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-show-completion\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-default-directory\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(with-helm-restore-variables\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(helm-multi-key-defun\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(helm-while-no-input\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(helm-aif\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(helm-awhile\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(helm-acond\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(helm-aand\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(helm-with-gensyms\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(helm-read-answer-dolist-with-action\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(helm-read-answer\\)\\>" 1 font-lock-keyword-face))))

(provide 'helm-lib)

;;; helm-lib ends here
