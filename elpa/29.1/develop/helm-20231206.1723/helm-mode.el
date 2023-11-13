;;; helm-mode.el --- Enable helm completion everywhere. -*- lexical-binding: t -*-

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
(require 'helm-files)
(require 'helm-misc)

(defvar crm-separator)
(defvar ido-everywhere)
(defvar completion-flex-nospace)
(defvar helm-completion--sorting-done)
(defvar helm-mode)
(defvar password-cache)
(defvar package--builtins)
(defvar helm--locate-library-doc-cache)
(defvar helm--locate-library-cache)

;; No warnings in Emacs built --without-x
(declare-function x-file-dialog "xfns.c")

(declare-function ido-mode "ido.el")
(declare-function helm-apropos-init "helm-elisp")
(declare-function helm-lisp-completion-persistent-action "helm-elisp")
(declare-function helm-lisp-completion-persistent-help "helm-elisp")
(declare-function help--symbol-class "help-fns.el")
(declare-function helm-get-first-line-documentation "helm-elisp")
(declare-function package-desc-summary   "package")
(declare-function package-built-in-p "package")
(declare-function package-desc-status "package")
(declare-function package-get-descriptor "package")
(declare-function print-coding-system-briefly "mul-diag.el")
(declare-function color-rgb-to-hex "color.el")
(declare-function find-library-name "find-func.el")

(defgroup helm-mode nil
  "Enable helm completion."
  :group 'helm)

(defcustom helm-completing-read-handlers-alist
  '((find-tag . helm-completing-read-default-find-tag)
    (ggtags-find-tag-dwim . helm-completing-read-default-find-tag)
    (tmm-menubar . nil)
    (find-file . nil)
    (execute-extended-command . nil)
    (dired-do-rename . helm-read-file-name-handler-1)
    (dired-do-copy . helm-read-file-name-handler-1)
    (dired-do-symlink . helm-read-file-name-handler-1)
    (dired-do-relsymlink . helm-read-file-name-handler-1)
    (dired-do-hardlink . helm-read-file-name-handler-1)
    (basic-save-buffer . helm-read-file-name-handler-1)
    (write-file . (default helm-read-file-name-handler-1))
    (write-region . (default helm-read-file-name-handler-1))
    (all-the-icons-insert . helm-mode-all-the-icons-handler))
  "Completing read functions for specific Emacs commands.

By default `helm-mode' use `helm-completing-read-default-handler' to
provide helm completion in each `completing-read' or `read-file-name'
found, but other functions can be specified here for specific
commands. This also allows disabling helm completion for some commands
when needed.

Each entry is a cons cell like (EMACS_COMMAND . COMPLETING-READ_HANDLER)
where key and value are symbols.
However if a command is using in its definition both a `completing-read' AND
a `read-file-name' we may want to specify a handler for both of them,
this can be done by specifying value as a list of two symbols instead of
a single symbol where the 1st element of the list specify the handler for the
`completing-read' and the second the handler for the `read-file-name'.
Special symbol \\='default' means use the default helm handler for either
`completing-read' or `read-file-name'.
e.g. (write-region . (default helm-read-file-name-handler-1))
means helm will use `helm-completing-read-default-handler' when
`write-region' calls `completing-read' and
`helm-read-file-name-handler-1' when it calls `read-file-name'.  

Each key is an Emacs command that use originaly `completing-read'
or/and `read-file-name'.

Each value maybe a helm function that takes same arguments as
`completing-read' plus NAME and BUFFER, where NAME is the name of the new
helm source and BUFFER the name of the buffer we will use, but it can
be also a function not using helm, in this case the function should
take the same args as `completing-read' and not be prefixed by \"helm-\".

`helm' will use the name of the command calling `completing-read' as
NAME and BUFFER will be computed as well with NAME but prefixed with
\"*helm-mode-\".

This function prefix name must start by \"helm-\" when it uses helm,
otherwise `helm' assumes the function is not a helm function and
expects the same args as `completing-read', this allows you to define a
handler not using helm completion.

Example:

    (defun foo/test ()
      (interactive)
      (message \"%S\" (completing-read \"test: \" \\='(a b c d e))))

    (defun helm-foo/test-completing-read-handler (prompt collection
                                                  predicate require-match
                                                  initial-input hist def
                                                  inherit-input-method
                                                  name buffer)
      (helm-comp-read prompt collection :marked-candidates t
                                        :name name
                                        :buffer buffer))

    (add-to-list \\='helm-completing-read-handlers-alist
                 \\='(foo/test . helm-foo/test-completing-read-handler))


We want here to make the regular `completing-read' in `foo/test'
return a list of candidate(s) instead of a single candidate.

Note that this function will be reused for ALL the `completing-read'
of this command, so it should handle all cases. E.g.,
if first `completing-read' completes against symbols and
second `completing-read' should handle only buffer,
your specialized function should handle both.

If the value of an entry is nil completion will fall back to
Emacs vanilla behaviour.
Example:

If you want to disable helm completion for `describe-function', use:

    (describe-function . nil)

Ido is also supported, you can use `ido-completing-read' and
`ido-read-file-name' as value of an entry or just \\='ido.
Example:
Enable ido completion for `find-file':

    (find-file . ido)

same as

    (find-file . ido-read-file-name)

Note that you don't need to enable `ido-mode' for this to work, see
`helm-mode' documentation."
  :group 'helm-mode
  :type '(alist
          :key-type symbol
          :value-type (choice
                       function
                       (list :tag "Specify the completing-read and read-file-name handlers"
                        (choice
                         (const :tag "Use default helm completing-read handler" default)
                         (function :tag "Use this helm completing-read function"))
                        (function :tag "Use this helm read file name function"))
                       (other :tag "Disabled" nil))))

(defcustom helm-comp-read-case-fold-search helm-case-fold-search
  "Default Local setting of `helm-case-fold-search' for `helm-comp-read'.
See `helm-case-fold-search' for more info."
  :group 'helm-mode
  :type 'symbol)

(defcustom helm-mode-handle-completion-in-region t
  "Whether to replace or not `completion-in-region-function'.
This enables support for `completing-read-multiple' and `completion-at-point'
when non--nil."
  :group 'helm-mode
  :type 'boolean)

(defcustom helm-mode-no-completion-in-region-in-modes nil
  "A list of modes that do not want helm for `completion-in-region'."
  :group 'helm-mode
  :type 'boolean)

(defcustom helm-mode-reverse-history t
  "Display history source after current source when non nil.

Apply only in `helm-mode' handled commands."
  :group 'helm-mode
  :type 'boolean)

(defcustom helm-completion-in-region-default-sort-fn
  'helm-completion-in-region-sort-fn
  "The default sort function to sort candidates in completion-in-region.

When nil no sorting is done.
The function is a `filtered-candidate-transformer' function which takes
two args CANDIDATES and SOURCE.
The function must use the flag `helm-completion--sorting-done' and
return CANDIDATES unchanged when the flag is nil.
See default function `helm-completion-in-region-sort-fn' as example.
It will be used only when `helm-completion-style' is either Emacs or
helm, otherwise when helm-fuzzy style is used, the fuzzy sort function
will be used."
  :group 'helm-mode
  :type 'function)

(defcustom helm-mode-ignore-diacritics nil
  "Ignore diacritics in completing-read."
  :group 'helm-mode
  :type 'boolean)

(defcustom helm-completion-mark-suffix t
  "Push mark at end of suffix when non nil."
  :group 'helm-mode
  :type 'boolean)

(defcustom helm-read-file-name-use-default-arg-behavior nil
  "Use emacs vanilla `read-file-name' behavior for default arg.

The behavior of default arg in `read-file-name' and friends is using
the default arg as default value when initial input is not modified,
even if this initial input is a valid value i.e. an existing file.
We expect generally a default arg to be used if nothing is specified
in the prompt or if what is specified is invalid, but the emacs behavior
here is really weird, so we use this variable to disable this
behavior, letting user specify default if needed with `M-n'.
However we keep the emacs default for `read-file-name' and derived
fns, this variable affecting only `helm-read-file-name'."
  :type 'boolean
  :group 'helm-mode)

(defvar helm-mode-minibuffer-setup-hook-black-list '(minibuffer-completion-help)
  "Incompatible `minibuffer-setup-hook' functions go here.
A list of symbols.  `helm-mode' is rejecting all lambda's, byte-code fns
and all functions belonging in this list from `minibuffer-setup-hook'.
This is mainly needed to prevent \"*Completions*\" buffers to popup.")

(defvar helm-comp-read-require-match-overrides '((describe-function . t)
                                                 (describe-command . t)
                                                 (describe-minor-mode . t)
                                                 (load-theme . t)
                                                 (describe-theme . t))
  "Allow overriding REQUIRE-MATCH completing-read arg for a specific function.")

(defcustom helm-completions-detailed (and (boundp 'completions-detailed)
                                          completions-detailed)
  "Allow providing `completions-detailed' for Emacs < 28.
Not guaranteed to work with Emacs < 27."
  :type 'boolean
  :group 'helm-mode)

(defvar helm-mode-find-file-target-alist
  '(("switch-to-buffer" . helm-buffers-quit-and-find-file-fn))
  "An alist composed of (SOURCE_NAME . FUNCTION) elements.
Where FUNCTION is a function suitable for `helm-quit-and-find-file'.")

(defface helm-mode-prefix
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       (:background "red" :foreground "black")))
  "Face used for prefix completion."
  :group 'helm-mode)

(defface helm-completion-invalid
    '((t :inherit font-lock-property-name-face))
  "Face used to highlight invalid functions."
  :group 'helm-mode)

(defface helm-completions-detailed
    '((t :inherit font-lock-warning-face))
  "Face used to highlight completion-detailed informations."
  :group 'helm-mode)

(defface helm-completions-annotations
    '((t :inherit font-lock-property-name-face))
  "Face used to highlight annotations in completion."
  :group 'helm-mode)

(defvar helm-comp-read-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "<C-return>") 'helm-cr-empty-string)
    (define-key map (kbd "M-RET")      'helm-cr-empty-string)
    map)
  "Keymap for `helm-comp-read'.")

(defvar helm-comp-in-region-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-comp-read-map)
    map)
  "Keymap for completion-at-point and friends.")

(defun helm-mode-delete-char-backward-1 ()
  (interactive)
  (condition-case err
      (call-interactively 'delete-backward-char)
    (text-read-only
     (if (with-selected-window (minibuffer-window)
           (not (string= (minibuffer-contents) "")))
         (message "Trying to delete prefix completion, next hit will quit")
       (user-error "%s" (car err))))))
(put 'helm-mode-delete-char-backward-1 'helm-only t)

(defun helm-mode-delete-char-backward-2 ()
  (interactive)
  (condition-case _err
      (call-interactively 'delete-backward-char)
    (text-read-only
     (unless (with-selected-window (minibuffer-window)
               (string= (minibuffer-contents) ""))
       (with-helm-current-buffer
         (run-with-timer 0.1 nil (lambda ()
                                   (call-interactively 'delete-backward-char))))
       (helm-keyboard-quit)))))
(put 'helm-mode-delete-char-backward-2 'helm-only t)

(helm-multi-key-defun helm-mode-delete-char-backward-maybe
    "Delete char backward when text is not the prefix helm is completing against.
First call warns user about deleting prefix completion.
Second call deletes backward char in current-buffer and quits helm completion,
letting the user start a new completion with a new prefix."
  '(helm-mode-delete-char-backward-1 helm-mode-delete-char-backward-2) 1)

(defcustom helm-completion-style 'helm
  "Style of completion to use in `completion-in-region'.

This affects only `completion-at-point' and friends, and
the `completing-read' using the default handler
i.e. `helm-completing-read-default-handler'.

NB: This has nothing to do with `completion-styles', it is independent from
helm, but when using \\='emacs as helm-completion-style helm
will use the `completion-styles' for its completions.
Up to the user to configure `completion-styles'.

There are three possible values to use:

- helm, use multi match regular helm completion.

- helm-fuzzy, use fuzzy matching.  Note that as usual when
  entering a space helm switches to multi matching mode.

- emacs, use regular Emacs completion according to
  `completion-styles'.  Note that even in this style, helm allows using
  multi match.  Emacs-27 provides a style called `flex' that can be used
  aside `helm' style (see `completion-styles-alist').  When `flex' style
  is not available (Emacs<27) helm provides `helm-flex' style which is
  similar to `flex' and helm fuzzy matching.

For a better experience with emacs style, if you don't know what to use, set
`completion-styles' to \\='(flex) if you are using emacs-27 or to
\\='(helm-flex) if you are using emacs-26 and keep \\='emacs as default
value for `helm-completion-style'.  Advanced users can also have a
look to `completion-category-overrides' to set styles according to category.
You can as well use `helm-completion-styles-alist' to override
`helm-completion-style' in specific modes.

Of course when using `helm' or `helm-fuzzy' as `helm-completion-style'
emacs `completion-styles' have no effect.

Please use custom interface or `customize-set-variable' to set this,
NOT `setq'."
  :group 'helm-mode
  :type '(choice (const :tag "Emacs" emacs)
                 (const :tag "Helm" helm)
                 (const :tag "Helm-fuzzy" helm-fuzzy))
  :set (lambda (var val)
         (set var val)
         (if (memq val '(helm helm-fuzzy))
             (define-key helm-comp-in-region-map (kbd "DEL") 'helm-mode-delete-char-backward-maybe)
           (define-key helm-comp-in-region-map (kbd "DEL") 'delete-backward-char))))

(defconst helm-completion--all-styles
  (let ((flex (if (assq 'flex completion-styles-alist)
                  'flex 'helm-flex)))
    (helm-fast-remove-dups
     (append (list 'helm flex)
             (mapcar 'car completion-styles-alist)))))

(defconst helm-completion--styles-type
  `(repeat :tag "with other completion styles"
           (choice ,@(mapcar (lambda (x) (list 'const x))
                             helm-completion--all-styles))))

(defcustom helm-completion-styles-alist '((gud-mode . helm)
                                          ;; See https://github.com/djcb/mu/issues/2181.
                                          (mu4e-compose-mode . emacs))
  "Allow configuring `helm-completion-style' per mode or command.

NOTE: Use a mode for a completion that will be used in a buffer
i.e. completion-in-region, whereas you have to specify instead a
command to affect the completing-read trigerred by this
command. Commands specified in `helm-completing-read-handlers-alist' take
precedence on commands you put here.

Each entry is a cons cell like (mode . style) where style must be
a suitable value for `helm-completion-style'.  When specifying
emacs as style for a mode or a command, `completion-styles' can
be specified by using a cons cell specifying completion-styles to
use with helm emacs style, e.g. (foo-mode . (emacs helm flex))
will set `completion-styles' to \\='(helm flex) for foo-mode."
  :group 'helm-mode
  :type
  `(alist :key-type (symbol :tag "Major Mode")
          :value-type
          (choice :tag "Use helm style or completion styles"
                  (radio :tag "Helm Style"
                         (const helm)
                         (const helm-fuzzy)
                         (const emacs))
                  (cons :tag "Completion Styles"
                        (const :tag "Using Helm `emacs' style" emacs)
                        ,helm-completion--styles-type))))

;;; helm-comp-read
;;
;;
(defvar helm-comp-read-use-marked nil
  "[INTERNAL] When non nil `helm-comp-read' will return marked candidates.

Use this ONLY in `let', NOT globally, this allows third party packages
to use a list as return value when `helm-mode' is enabled, e.g.

    (let ((helm-comp-read-use-marked t))
      (completing-read \"test: \" \\='(a b c d e f g)))

")

(defun helm-cr-empty-string ()
  "Return empty string."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action
     (lambda (_candidate)
         (identity "")))))
(put 'helm-cr-empty-string 'helm-only t)

(defun helm-mode--keyboard-quit ()
  ;; Use this instead of `keyboard-quit'
  ;; to avoid deactivating mark in current-buffer.
  (let ((debug-on-quit nil))
    (signal 'quit nil)))

(cl-defun helm-comp-read-get-candidates (collection &optional
                                                    test sort-fn alistp
                                                    (input helm-pattern))
  "Convert COLLECTION to list removing elements that don't match TEST.
See `helm-comp-read' about supported COLLECTION arguments.

SORT-FN is a predicate to sort COLLECTION.

ALISTP when non--nil will not use `all-completions' to collect
candidates because it doesn't handle alists correctly for helm.
i.e In `all-completions' the car of each pair is used as value.
In helm we want to use the cdr instead like (display . real),
so we return the alist as it is with no transformation by
`all-completions'.

e.g

\(setq A \\='((a . 1) (b . 2) (c . 3)))
==>((a . 1) (b . 2) (c . 3))
\(helm-comp-read \"test: \" A :alistp nil
                              :exec-when-only-one t
                              :initial-input \"a\")
==>\"a\" Which is not what we expect.

\(helm-comp-read \"test: \" A :alistp t
                              :exec-when-only-one t
                              :initial-input \"1\")
==>\"1\"

See docstring of `all-completions' for more info.

INPUT is the string you want to complete against, defaulting to
`helm-pattern' which is the value of what you enter in minibuffer.
Note that when using a function as COLLECTION this value will be
available with the input argument of the function only when using a
sync source from `helm-comp-read', i.e. not using
`:candidates-in-buffer', otherwise the function is called only once
with an empty string as value for `helm-pattern' because
`helm-pattern' is not yet computed, which is what we want otherwise
data would not be fully collected at init time.

If COLLECTION is an `obarray', a TEST should be needed. See `obarray'."
  ;; Ensure COLLECTION is computed from `helm-current-buffer'
  ;; because some functions used as COLLECTION work
  ;; only in the context of current-buffer (Bug#1030) .
  (with-helm-current-buffer
    (let ((cands
           (cond ((and alistp (hash-table-p collection))
                  (cl-loop for k being the hash-keys of collection
                           using (hash-values v)
                           collect (cons k v)))
                 ((vectorp collection)
                  (all-completions input collection test))
                 ((and (symbolp collection) (boundp collection)
                       ;; Bug#324 history is let-bounded and given
                       ;; quoted as hist argument of completing-read.
                       ;; See example in `rcirc-browse-url'.
                       (symbolp (symbol-value collection)))
                  nil)
                 ;; When collection is a symbol, most of the time
                 ;; it should be a symbol used as a minibuffer-history.
                 ;; The value of this symbol in this case return a list
                 ;; of string which maybe are converted later as symbol
                 ;; in special cases.
                 ;; we treat here commandp as a special case as it return t
                 ;; also with a string unless its last arg is provided.
                 ;; Also, the history collections generally collect their
                 ;; elements as string, so intern them to call predicate.
                 ((and (symbolp collection) (boundp collection) test)
                  (let ((predicate (lambda (elm)
                                     (condition-case _err
                                         (if (eq test 'commandp)
                                             (funcall test (intern elm))
                                             (funcall test elm))
                                       (wrong-type-argument
                                        (funcall test (intern elm)))))))
                    (all-completions input (symbol-value collection) predicate)))
                 ((and (symbolp collection) (boundp collection))
                  (all-completions input (symbol-value collection)))
                 ;; Normally file completion should not be handled here,
                 ;; but special cases like `find-file-at-point' do it.
                 ;; Handle here specially such cases.
                 ((and (functionp collection) (not (string= input ""))
                       (or minibuffer-completing-file-name
                           (eq (completion-metadata-get
                                (completion-metadata input collection test)
                                'category)
                               'file)))
                  (cl-loop for f in (funcall collection input test t)
                           unless (member f '("./" "../"))
                           if (string-match helm--url-regexp input)
                           collect f
                           else
                           collect (concat (file-name-as-directory
                                            (helm-basedir input))
                                           f)))
                 ((functionp collection)
                  (funcall collection input test t))
                 ((and alistp (null test)) collection)
                 ;; Next test ensure circular objects are removed
                 ;; with `all-completions' (Bug#1530).
                 (t (all-completions input collection test)))))
      (if sort-fn (sort cands sort-fn) cands))))

(cl-defun helm-cr--pattern-in-candidates-p (candidates &optional (pattern helm-pattern))
  (or (assoc pattern candidates)
      (assoc (concat " " pattern) candidates)
      (assq (intern pattern) candidates)
      (member pattern candidates)
      (member (downcase pattern) candidates)
      (member (upcase pattern) candidates)))

(defun helm-cr-default-transformer (candidates source)
  "Default filter candidate function for `helm-comp-read'."
  (let ((must-match (helm-get-attr 'must-match source)))
    ;; Annotation and affixation are already handled in completion-in-region and
    ;; in helm-completing-read-default-2 when emacs style is in use.
    ;; For helm-completing-read-default-1 we handle them in an extra FCT; This
    ;; allows extracting annotation and affixation from metadata which is not
    ;; accessible from here.
    (cl-loop for c in candidates
             for cand = (let ((elm (if (stringp c)
                                       (replace-regexp-in-string "\\s\\" "" c)
                                     c)))
                          (cond ((and (stringp elm)
                                      (string-match "\n" elm))
                                 (cons (replace-regexp-in-string "\n" "->" elm) c))
                                (t c)))
             collect cand into lst
             finally return
             ;; Unquote helm-pattern when it is added as candidate
             ;; (Bug#2015).
             (let ((pat (replace-regexp-in-string "\\s\\" "" helm-pattern)))
               (if (or (string= pat "")
                       (eq must-match t)
                       (helm-cr--pattern-in-candidates-p lst pat))
                   lst
                 (append (list (cons (helm-aand (propertize "[?]" 'face 'helm-ff-prefix)
                                                (propertize " " 'display it 'unknown t)
                                                (concat it pat))
                                     pat))
                         lst))))))

(defun helm-comp-read--move-to-first-real-candidate ()
  (helm-aif (helm-get-selection nil 'withprop)
      ;; Avoid error with candidates with an image as display (Bug#2296).
      (when (helm-candidate-prefixed-p it)
        (helm-next-line))))

(defun helm-cr-default (default cands)
  (delq nil
        (cond ((and (consp default) (string= helm-pattern ""))
               (append (cl-loop for d in default
                                ;; Don't convert
                                ;; nil to "nil" (i.e the string)
                                ;; it will be delq'ed on top.
                                for str = (if (null d) d (helm-stringify d))
                                when (member str cands)
                                do (setq cands (delete d cands))
                                when str collect str)
                       cands))
              ;; Some functions like debug-on-entry use (symbol-name sym)
              ;; without checking if sym is non nil, so the return value become
              ;; "nil". 
              ((and (not (member default '("" "nil")))
                    (string= helm-pattern ""))
               (cons default (delete (helm-stringify default)
                                     cands)))
              (t cands))))

;;;###autoload
(cl-defun helm-comp-read (prompt collection
                          &key
                            test
                            initial-input
                            default
                            preselect
                            (buffer "*Helm Completions*")
                            must-match
                            fuzzy
                            reverse-history
                            (requires-pattern 0)
                            (history nil shistory)
                            raw-history
                            input-history
                            (case-fold helm-comp-read-case-fold-search)
                            (persistent-action nil)
                            (persistent-help "DoNothing")
                            (mode-line helm-comp-read-mode-line)
                            help-message
                            (keymap helm-comp-read-map)
                            (name "Helm Completions")
                            header-name
                            candidates-in-buffer
                            get-line
                            diacritics
                            match-part
                            match-dynamic
                            exec-when-only-one
                            quit-when-no-cand
                            (volatile t)
                            sort
                            fc-transformer
                            hist-fc-transformer
                            (marked-candidates helm-comp-read-use-marked)
                            nomark
                            (alistp t)
                            (candidate-number-limit helm-candidate-number-limit)
                            multiline
                            allow-nest
                            coerce
                            (group 'helm))
  "Read a string in the minibuffer, with helm completion.

It is helm `completing-read' equivalent.

- PROMPT is the prompt name to use.

- COLLECTION can be a list, alist, vector, obarray or hash-table.
  For alists and hash-tables their car are use as real value of
  candidate unless ALISTP is non-nil.
  It can be also a function that receives three arguments:
  the values string, predicate and t. See `all-completions' for more details.

Keys description:

- TEST: A predicate called with one arg i.e candidate.

- INITIAL-INPUT: Same as input arg in `helm'.

- PRESELECT: See preselect arg of `helm'.

- DEFAULT: This option is used only for compatibility with regular
  Emacs `completing-read' (Same as DEFAULT arg of `completing-read').

- BUFFER: Name of helm-buffer.

- MUST-MATCH: Candidate selected must be one of COLLECTION.

- FUZZY: Enable fuzzy matching.

- REVERSE-HISTORY: When non--nil display history source after current
  source completion.

- REQUIRES-PATTERN: Same as helm attribute, default is 0.

- HISTORY: A symbol where each result will be saved.
  If not specified as a symbol an error will popup.
  When specified, all elements of HISTORY are displayed in
  a special source before or after COLLECTION according to REVERSE-HISTORY.
  The main difference with INPUT-HISTORY is that the result of the
  completion is saved whereas in INPUT-HISTORY it is the minibuffer
  contents which is saved when you exit.
  Don't use the same symbol for INPUT-HISTORY and HISTORY.
  NOTE: As mentionned above this has nothing to do with
  `minibuffer-history-variable', therefore if you want to save this
  history persistently, you will have to add this variable to the
  relevant variable of your favorite tool for persistent emacs session
  i.e. psession, desktop etc...

- RAW-HISTORY: When non-nil do not remove backslashs if some in
  HISTORY candidates.

- INPUT-HISTORY: A symbol. The minibuffer input history will be
  stored there, if nil or not provided, `minibuffer-history'
  will be used instead.  You can navigate in this history with
  `M-p' and `M-n'.
  Don't use the same symbol for INPUT-HISTORY and HISTORY.

- CASE-FOLD: Same as `helm-case-fold-search'.

- PERSISTENT-ACTION: A function called with one arg i.e candidate.

- PERSISTENT-HELP: A string to document PERSISTENT-ACTION.

- MODE-LINE: A string or list to display in mode line.
  Default is `helm-comp-read-mode-line'.

- KEYMAP: A keymap to use in this `helm-comp-read'.
  (the keymap will be shared with history source)

- NAME: The name related to this local source.

- HEADER-NAME: A function to alter NAME, see `helm'.

- EXEC-WHEN-ONLY-ONE: Bound `helm-execute-action-at-once-if-one'
  to non--nil. (possibles values are t or nil).

- VOLATILE: Use volatile attribute.

- SORT: A predicate to give to `sort' e.g `string-lessp'
  Use this only on small data as it is inefficient.
  If you want to sort faster add a sort function to
  FC-TRANSFORMER.
  Note that FUZZY when enabled is already providing a sort function.

- FC-TRANSFORMER: A `filtered-candidate-transformer' function
  or a list of functions.

- HIST-FC-TRANSFORMER: A `filtered-candidate-transformer'
  function for the history source.

- MARKED-CANDIDATES: If non-nil return candidate or marked candidates as a list.

- NOMARK: When non--nil don't allow marking candidates.

- ALISTP:
  When non-nil (default) pass the value of (DISPLAY . REAL)
  candidate in COLLECTION to action when COLLECTION is an alist or a
  hash-table, otherwise DISPLAY is always returned as result on exit,
  which is the default when using `completing-read'.
  See `helm-comp-read-get-candidates'.

- CANDIDATES-IN-BUFFER: when non--nil use a source build with
  `helm-source-in-buffer' which is much faster.
  Argument VOLATILE have no effect when CANDIDATES-IN-BUFFER is non--nil.

- GET-LINE: Specify the :get-line slot of `helm-source-in-buffer', has no effect
  when CANDIDATES-IN-BUFFER is nil.
 
- MATCH-PART: Allow matching only one part of candidate.
  See match-part documentation in `helm-source'.

- MATCH-DYNAMIC: See match-dynamic in `helm-source-sync'
  It has no effect when used with CANDIDATES-IN-BUFFER.

- ALLOW-NEST: Allow nesting this `helm-comp-read' in a helm session.
  See `helm'.

- MULTILINE: See multiline in `helm-source'.

- COERCE: See coerce in `helm-source'.

- GROUP: See group in `helm-source'.

Any prefix args passed during `helm-comp-read' invocation will be recorded
in `helm-current-prefix-arg', otherwise if prefix args were given before
`helm-comp-read' invocation, the value of `current-prefix-arg' will be used.
That means you can pass prefix args before or after calling a command
that use `helm-comp-read'.  See `helm-M-x' for example."
  ;; Handle error with HISTORY:
  ;;
  ;; Should show helm with one source at first run and save result on
  ;; exit, should show the history source along candidates source on
  ;; next run as soon as `test-hist' value is feeded. 
  ;;     (setq test-hist nil)
  ;;     (helm-comp-read "test: " '(a b c d e)
  ;;                     :history 'test-hist)
  ;;
  ;; Should run normally as long as `test-hist' is bound and nil. As
  ;; soon `test-hist' becomes non-nil throw an error.
  ;;     (helm-comp-read "test: " '(a b c d e)
  ;;                     :history test-hist)
  ;;
  ;; Should run normally.
  ;;     (completing-read "test: " '(a b c d e))
  (cl-assert (if shistory
                 (or (null history)
                     (and history (symbolp history)))
               t)
             nil "Error: History should be specified as a symbol")
  (when (get-buffer helm-action-buffer)
    (kill-buffer helm-action-buffer))
  (unless (memq must-match '(confirm confirm-after-completion t nil))
    ;; Fix completing-read's using something else than `t' e.g. 1 or
    ;; whatever (bug #2527).
    (setq must-match t))
  (let ((action-fn `(("Sole action (Identity)"
                      . (lambda (candidate)
                          (if ,marked-candidates
                              (helm-marked-candidates)
                              (identity candidate)))))))
    (let* ((minibuffer-completion-predicate test)
           (minibuffer-completion-table
            (or minibuffer-completion-table collection))
           (helm-read-file-name-mode-line-string
            (replace-regexp-in-string "helm-maybe-exit-minibuffer"
                                      "helm-confirm-and-exit-minibuffer"
                                      helm-read-file-name-mode-line-string))
           (get-candidates
            (lambda ()
              (let ((cands (helm-comp-read-get-candidates
                            ;; If `helm-pattern' is passed as INPUT
                            ;; and :alistp is nil INPUT is passed to
                            ;; `all-completions' which defeat helm
                            ;; matching functions (multi match, fuzzy
                            ;; etc...) Bug#2134.
                            collection test sort alistp
                            (if (and match-dynamic (null candidates-in-buffer))
                                helm-pattern ""))))
                (helm-cr-default default cands))))
           (history-get-candidates
            (lambda ()
              (let ((cands (helm-comp-read-get-candidates
                            history test nil alistp)))
                (when cands
                  (delete "" (helm-cr-default default cands))))))
           (src-hist (helm-build-sync-source (format "%s History" name)
                       :candidates history-get-candidates
                       :fuzzy-match fuzzy
                       :multiline multiline
                       :match-part match-part
                       :filtered-candidate-transformer
                       (append `((lambda (candidates _source)
                                   (if ,raw-history
                                       candidates
                                     (cl-loop for i in candidates
                                              ;; Input is added to history in completing-read's
                                              ;; and may be regexp-quoted, so unquote it
                                              ;; but check if cand is a string (it may be at this stage
                                              ;; a symbol or nil) Bug#1553.
                                              when (stringp i)
                                              collect (replace-regexp-in-string "\\s\\" "" i)))))
                               (and hist-fc-transformer (helm-mklist hist-fc-transformer)))
                       :persistent-action persistent-action
                       :persistent-help persistent-help
                       :keymap keymap
                       :must-match must-match
                       :group group
                       :coerce coerce
                       :mode-line mode-line
                       :help-message help-message
                       :action action-fn))
           (src (helm-build-sync-source name
                  :candidates get-candidates
                  :match-part match-part
                  :multiline multiline
                  :header-name header-name
                  :filtered-candidate-transformer
                  (let ((transformers (helm-mklist fc-transformer)))
                    (append transformers
                            (unless (member 'helm-cr-default-transformer transformers)
                              '(helm-cr-default-transformer))))
                  :requires-pattern requires-pattern
                  :persistent-action persistent-action
                  :persistent-help persistent-help
                  :fuzzy-match fuzzy
                  :diacritics diacritics
                  :keymap keymap
                  :must-match must-match
                  :group group
                  :coerce coerce
                  :mode-line mode-line
                  :match-dynamic match-dynamic
                  :help-message help-message
                  :action action-fn
                  :volatile volatile))
           (src-1 (helm-build-in-buffer-source name
                    :data get-candidates
                    :match-part match-part
                    :get-line get-line
                    :multiline multiline
                    :header-name header-name
                    :filtered-candidate-transformer
                    (let ((transformers (helm-mklist fc-transformer)))
                      (append transformers
                              (unless (member 'helm-cr-default-transformer transformers)
                                '(helm-cr-default-transformer))))
                    :requires-pattern requires-pattern
                    :persistent-action persistent-action
                    :fuzzy-match fuzzy
                    :diacritics diacritics
                    :keymap keymap
                    :must-match must-match
                    :group group
                    :coerce coerce
                    :persistent-help persistent-help
                    :mode-line mode-line
                    :help-message help-message
                    :action action-fn))
           (src-list (list src-hist
                           (if candidates-in-buffer
                               src-1 src)))
           (helm-execute-action-at-once-if-one exec-when-only-one)
           (helm-quit-if-no-candidate quit-when-no-cand)
           result)
      (when nomark
        (setq src-list (cl-loop for src in src-list
                             collect (cons '(nomark) src))))
      (when reverse-history (setq src-list (nreverse src-list)))
      (add-hook 'helm-after-update-hook 'helm-comp-read--move-to-first-real-candidate)
      (unwind-protect
           (setq result (helm
                         :sources src-list
                         :input initial-input
                         :default default
                         :preselect preselect
                         :prompt prompt
                         :resume 'noresume
                         :keymap keymap ;; Needed with empty collection.
                         :allow-nest allow-nest
                         :candidate-number-limit candidate-number-limit
                         :case-fold-search case-fold
                         :history (and (symbolp input-history) input-history)
                         :buffer buffer))
        (remove-hook 'helm-after-update-hook 'helm-comp-read--move-to-first-real-candidate))
      ;; If `history' is a symbol save it, except when it is t.
      (when (and result history (symbolp history) (not (eq history t)))
        (set history
             ;; RESULT may be a a string or a list of strings bug #2461.
             (delete-dups (append (mapcar #'substring-no-properties (helm-mklist result))
                                  (symbol-value history)))))
      (or result (helm-mode--keyboard-quit)))))


;; Generic completing-read
;;
;; Support also function as collection.
;; e.g M-x man is supported.
;; Support hash-table and vectors as collection.
;; NOTE:
;; Some crap emacs functions may not be supported
;; like ffap-alternate-file (bad use of completing-read)
;; and maybe others.
;; Provide a mode `helm-mode' which turn on
;; helm in all `completing-read' and `read-file-name' in Emacs.
;;
(defvar helm-completion-mode-string " Helm")

(defvar helm-completion-mode-quit-message
  "Helm completion disabled")

(defvar helm-completion-mode-start-message
  "Helm completion enabled")

;;; Specialized handlers
;;
;;
(defun helm-completing-read-symbols
    (prompt _collection test _require-match init
     hist default _inherit-input-method name buffer)
  "Specialized function for fast symbols completion in `helm-mode'."
  (require 'helm-elisp)
  (or
   (helm
    :sources (helm-build-in-buffer-source name
               :init (lambda ()
                       (helm-apropos-init (lambda (x)
                                            (and (funcall test x)
                                                 (not (keywordp x))))
                                          (or (car-safe default) default)))
               :filtered-candidate-transformer 'helm-apropos-default-sort-fn
               :help-message #'helm-comp-read-help-message
               :fuzzy-match (eq helm-completion-style 'helm-fuzzy)
               :persistent-action
               (lambda (candidate)
                 (helm-lisp-completion-persistent-action
                  candidate name))
               :persistent-help (helm-lisp-completion-persistent-help))
    :prompt prompt
    :buffer buffer
    :input init
    :history hist
    :resume 'noresume
    :default (or default ""))
     (helm-mode--keyboard-quit)))


;;; Extra metadata for completions-detailed
;;
;;
(defvar helm-completing-read-extra-metadata
  '((buffer . (metadata
               (affixation-function . helm-completing-read-buffer-affixation)
               (category . buffer)
               (flags . (helm-completing-read--buffer-lgst-mode))))
    (symbol-help . (metadata
                    (affixation-function . helm-symbol-completion-table-affixation)
                    (category . symbol-help)))
    (package . (metadata
                (affixation-function . helm-completion-package-affixation)
                (category . package)))
    (theme . (metadata
              (affixation-function . helm-completion-theme-affixation)
              (category . theme)))
    (coding-system . (metadata
                      (affixation-function . helm-completion-coding-system-affixation)
                      (category . coding-system)))
    (color . (metadata
              (affixation-function . helm-completion-color-affixation)
              (category . color)))
    (library . (metadata
                (affixation-function . helm-completion-library-affixation)
                (category . library))))
  "Extra metadata for completing-read.

Alist composed of (CATEGORY . METADATA).
CATEGORY is extracted from original metadata and METADATA is a list composed
like this:
    (metadata (affixation-function . fun)
              (annotation-function . fun)
              (category . category)
              (flags . flags))

FLAGS is a list of variables to renitialize to nil when exiting or quitting.

It is used to add `affixation-function' or `annotation-function' if original
metadata doesn't have some and `completions-detailed' is non nil.
When using emacs as `helm-completion-style', this has no effect, keeping same
behavior as emacs vanilla.")

(defvar helm-completing-read-command-categories
  '(("customize-variable" . symbol-help)
    ("customize-set-variable" . symbol-help)
    ("customize-set-value" . symbol-help)
    ("customize-save-variable" . symbol-help)
    ("describe-function" . symbol-help); For Emacs-27.
    ("describe-variable" . symbol-help); For Emacs-27.
    ("describe-symbol" . symbol-help)  ; For Emacs-27.
    ("describe-command" . symbol-help) ; For Emacs-27.
    ("set-variable" . symbol-help)
    ("customize-group" . symbol-help)
    ("find-function" . symbol-help)
    ("find-variable" . symbol-help)
    ("trace-function" . symbol-help)
    ("trace-function-foreground" . symbol-help)
    ("trace-function-background" . symbol-help)
    ("describe-minor-mode" . symbol-help)
    ("where-is" . symbol-help)
    ("execute-extended-command" . symbol-help)
    ("find-library" . library)
    ("locate-library" . library)
    ("kill-buffer" . buffer)
    ("package-install" . package)
    ("package-vc-install" . package)
    ("package-vc-checkout" . package)
    ("describe-package" . package)
    ("load-theme" . theme)
    ("describe-theme" . theme)
    ("describe-coding-system" . coding-system)
    ("read-color" . color))
  "An alist to specify metadata category by command.

Some commands provide a completion-table with no category
specified in metadata, we allow here specifying the category of
the completion provided by a specific command.  The command
should be specified as a string and the category as a symbol.")

(defvar helm-completing-read--buffer-lgst-mode nil)
(defun helm-completing-read-buffer-affixation (completions)
  (let ((len-mode (or helm-completing-read--buffer-lgst-mode
                      (setq helm-completing-read--buffer-lgst-mode
                            (cl-loop for bn in completions
                                     maximize (with-current-buffer bn
                                                (length (symbol-name major-mode))))))))
    (lambda (comp)
      (let* ((buf (get-buffer comp))
             (fname (buffer-file-name buf))
             (modified (and fname (buffer-modified-p buf)))
             (prefix (cond (modified
                            (propertize
                             "fm " 'face 'font-lock-comment-face))
                           (fname
                            (propertize
                             " f " 'face 'helm-completions-annotations))
                           (t (propertize "nf " 'face 'font-lock-doc-face))))
             (mode (with-current-buffer comp
                     (propertize
                      (symbol-name major-mode) 'face 'helm-completions-detailed)))
             (size (helm-buffer-size buf))
             (max-len helm-buffer-max-length)
             (bname (truncate-string-to-width
                     comp helm-buffer-max-length nil nil
                     helm-buffers-end-truncated-string))
             (suffix (format "%s%s%s%s%s(in %s)"
                             (make-string (1+ (- max-len (length bname))) ? )
                             (propertize size
                                         'face 'helm-buffer-size)
                             (make-string (- 7 (length size)) ? )
                             mode
                             (make-string (1+ (- len-mode (length mode))) ? )
                             (helm-aif fname
                                 (propertize
                                  (abbreviate-file-name (file-name-directory it))
                                  'face 'font-lock-type-face)
                               (propertize
                                (with-current-buffer comp
                                  (abbreviate-file-name default-directory))
                                'face 'font-lock-doc-face)))))
        (list (propertize
               bname 'face (if fname
                              'font-lock-builtin-face
                            'font-lock-doc-face))
              (propertize " " 'display prefix)
              (propertize " " 'display suffix))))))

(defun helm-symbol-completion-table-affixation (_completions)
  "Override `help--symbol-completion-table-affixation'.

Normally affixation functions use COMPLETIONS as arg, and return a list of
modified COMPLETIONS. Now we allow affixations functions to return a
function instead, just like annotation functions. The function should return a
list of three elements like (comp prefix suffix). This increase significantly
the speed avoiding one useless loop on complete list of candidates.

Returns a function and not a list of completions.

It affects actually describe-variable/function/command/symbol functions.
It uses `helm-get-first-line-documentation' which allow providing documentation
for `describe-variable' symbols and align properly documentation when helm style
is used."
  (lambda (comp)
    (require 'help-fns)
    (let* ((sym (intern comp))
           ;; When using in-buffer implementation we should have the
           ;; longest len to align documentation for free.
           ;; Check for style as well in case user switches to emacs
           ;; style and a candidate buffer remains (with its local vars
           ;; still available).
           (max-len (and (memq helm-completion-style '(helm helm-fuzzy))
                         (helm-in-buffer-get-longest-candidate)))
           (sep (if (or (null max-len) (zerop max-len))
                    " --"               ; Default separator.
                  (helm-make-separator comp max-len)))
           (doc (ignore-errors
                  (helm-get-first-line-documentation sym)))
           (symbol-class (help--symbol-class sym))
           (group (helm-group-p sym))
           (key (helm-completion-get-key sym)))
      (list
       ;; Symbol (comp).
       (if (or (symbol-function sym) (boundp sym)
               (facep sym) group)
           comp
         ;; Not already defined function. To test add an advice on a non
         ;; existing function.
         (propertize comp 'face 'helm-completion-invalid))
       ;; Prefix.
       (helm-aand (cond ((and symbol-class group)
                         (concat "g" symbol-class))
                        ((and (not (string= symbol-class ""))
                              symbol-class))
                        (group "g")
                        (t "i"))
                  (propertize it 'face 'helm-completions-detailed)
                  (propertize
                   ;; (format "%-4s" it) makes spaces inheriting text props.
                   " " 'display (concat it (make-string (- 5 (length it)) ? ))))
       ;; Suffix.
       (if doc
           (helm-aand (propertize doc 'face 'helm-completions-detailed)
                      (propertize " " 'display (concat sep it key)))
         "")))))

(defun helm-completion-get-key (sym)
  "Return key description on symbol SYM."
  (with-helm-current-buffer
    (let* ((key     (and (commandp sym) (where-is-internal sym nil 'first-only)))
           (binding (and key (key-description key))))
      (when binding
        (propertize (format " (%s)" binding) 'face 'shadow)))))

(defun helm-completion-package-affixation (_completions)
  (lambda (comp)
    (let* ((sym (intern-soft comp))
           (id (package-get-descriptor sym))
           (built-in (package-built-in-p sym))
           (status (and id (package-desc-status id)))
           (desc (if built-in
                     (aref (assoc-default sym package--builtins) 2)
                   (and id (package-desc-summary id))))
           (sep (helm-make-separator comp)))
      (list comp
            (propertize
             (if status
                 (format "%s " (substring status 0 1))
               "b ")
              'face 'helm-completions-annotations)
            (or (helm-aand desc
                           (propertize it 'face 'helm-completions-detailed)
                           (propertize " " 'display (concat sep it)))
                "")))))

(defun helm-completion-theme-affixation (_completions)
  (lambda (comp)
    (let* ((sym (intern-soft comp))
           (sep (helm-make-separator comp))
           (doc (if (custom-theme-p sym)
                    (helm-get-first-line-documentation sym)
                  (helm--get-theme-doc-1 sym))))
      (list comp
            ""
            (helm-aand (propertize doc 'face 'helm-completions-detailed)
                       (propertize " " 'display (concat sep it)))))))

(defun helm--get-theme-doc-1 (sym)
  (let ((fn (locate-file (concat (symbol-name sym) "-theme.el")
			 (custom-theme--load-path)
			 '("" "c")))
        doc)
    ;; Avoid loading theme as much as possible.
    (when fn
      (with-temp-buffer
        (insert-file-contents fn)
        (helm-awhile (let ((read-circle nil))
		       (condition-case nil
			   (read (current-buffer))
		         (end-of-file nil)))
          (when (eq (car-safe it) 'deftheme)
            (cl-return (setq doc (car (split-string (nth 2 it) "\n"))))))
        (unless doc
          (setq doc (helm--get-theme-doc-from-header)))))
    doc))

(defun helm--get-theme-doc-from-header ()
  "Extract doc in first line of theme file."
  (goto-char (point-min))
  (let (beg end)
    (when (re-search-forward "--- " (pos-eol) t)
      (setq beg (point)))
    (if (re-search-forward " -\\*-" (pos-eol) t)
        (setq end (match-beginning 0))
      (setq end (pos-eol)))
    (when (and beg end)
      (buffer-substring beg end))))

(defun helm-completion-coding-system-affixation (_comps)
  (lambda (comp)
    (let ((doc (with-output-to-string
                 (with-current-buffer standard-output
                   (print-coding-system-briefly (intern comp) 'tightly))))
          (sep (helm-make-separator comp)))
      (list comp "" (helm-aand (replace-regexp-in-string "^ *" "" doc)
                               (replace-regexp-in-string "[\n]" "" it)
                               (propertize it 'face 'helm-completions-detailed)
                               (propertize " " 'display (concat sep it)))))))

(defun helm-completion-color-affixation (_comps)
  (lambda (comp)
    (let ((sep (helm-make-separator comp))
          (rgb (condition-case nil
                   (helm-acase comp
                     ("foreground at point" (with-helm-current-buffer
                                              (foreground-color-at-point)))
                     ("background at point" (with-helm-current-buffer
                                              (background-color-at-point)))
                     (t
                      (apply #'color-rgb-to-hex (color-name-to-rgb comp))))
                 (error "SAMPLE"))))
      (list comp
            ""
            (helm-aand (propertize rgb 'face `(:background ,rgb
                                               :distant-foreground "black"))
                       (propertize " " 'display (concat sep it)))))))

(defun helm-completion-library-affixation (_comps)
  (require 'helm-elisp)
  (lambda (comp)
    ;; Because find-library-include-other-files default to t, we have all the
    ;; unrelated files and directories coming in ... Even if this modify the
    ;; behavior of find-library-include-other-files remove them for the benefit
    ;; of everybody.
    (unless (or (string-match "\\(\\.elc\\|/\\)\\'" comp)
                (string-match "\\`\\.#" comp)) ; (bug#2526)
      (let* ((sep (helm-make-separator comp))
             (path (or (assoc-default comp helm--locate-library-cache)
                       (let ((p (find-library-name comp)))
                         (push (cons comp p) helm--locate-library-cache)
                         p)))
             (doc (or (gethash comp helm--locate-library-doc-cache)
                      (puthash comp (helm-locate-lib-get-summary path)
                               helm--locate-library-doc-cache))))
        (list comp
              ""
              (helm-aand (propertize doc 'face 'font-lock-warning-face)
                         (propertize " " 'display (concat sep it))))))))

;;; Generic completing read
;;
;;
(defun helm-completing-read-default-1
    (prompt collection test require-match
     init hist default _inherit-input-method
     name buffer &optional cands-in-buffer exec-when-only-one alistp get-line)
  "Helm `completing-read' handler not rebuilding its candidates dynamically.

It is used usually with helm or helm-fuzzy `helm-completion-style'.
Call `helm-comp-read' with same args as `completing-read'.

Extra optional arg CANDS-IN-BUFFER means use `candidates-in-buffer'
method which is faster.

EXEC-WHEN-ONLY-ONE allow exiting when COLLECTION contains only one candidate.

ALISTP is same as `helm-comp-read' :alistp slot.

When using CANDS-IN-BUFFER, GET-LINE can be specified to exit with candidate
handling properties, see `helm-comp-read'.

This handler should be used when candidate list doesn't need to be rebuilt
dynamically otherwise use `helm-completing-read-default-2'."
  (let* ((history (or (car-safe hist) hist))
         (initial-input (pcase init
                          ((pred (stringp)) init)
                          ;; INIT is a cons cell.
                          (`(,l . ,_ll) l)))
         (minibuffer-completion-table collection)
         (metadata (or (completion-metadata (or initial-input "") collection test)
                       '(metadata)))
         (afun (or (plist-get completion-extra-properties :annotation-function)
                   (completion-metadata-get metadata 'annotation-function)))
         (afix (or (plist-get completion-extra-properties :affixation-function)
                   (completion-metadata-get metadata 'affixation-function)))
         (category (completion-metadata-get metadata 'category))
         (sort-fn (unless (eq helm-completion-style 'helm-fuzzy)
                    (or
                     (completion-metadata-get
                      metadata 'display-sort-function)
                     (lambda (candidates)
                       (sort candidates #'helm-generic-sort-fn)))))
         flags)
    (helm-aif (and (null category)
                   (assoc-default name helm-completing-read-command-categories))
        (setq metadata `(metadata (category . ,it))
              category it))
    (helm-aif (and (or (and (boundp 'completions-detailed) completions-detailed)
                       helm-completions-detailed)
                   (assoc-default category helm-completing-read-extra-metadata))
        (progn
          (setq metadata it)
          (setq afun (completion-metadata-get metadata 'annotation-function)
                afix (completion-metadata-get metadata 'affixation-function)
                flags (completion-metadata-get metadata 'flags))))
    (unwind-protect
         (helm-comp-read
          prompt collection
          :test test
          :history history
          :reverse-history helm-mode-reverse-history
          :input-history history
          :must-match require-match
          :alistp alistp
          :diacritics helm-mode-ignore-diacritics
          :help-message #'helm-comp-read-help-message
          :name name
          :requires-pattern (if (and (stringp default)
                                     (string= default "")
                                     (memq require-match
                                           '(confirm confirm-after-completion)))
                                1 0)
          :fc-transformer (append (and (or afix afun (memq category '(file library)) sort-fn)
                                       (list (lambda (candidates _source)
                                               (helm-completion--initial-filter
                                                (if (and sort-fn (> (length helm-pattern) 0))
                                                    (funcall sort-fn candidates)
                                                  candidates)
                                                afun afix category))))
                                  '(helm-cr-default-transformer))
          :quit-when-no-cand (eq require-match t)
          :nomark (null helm-comp-read-use-marked)
          :candidates-in-buffer cands-in-buffer
          :get-line get-line
          :exec-when-only-one exec-when-only-one
          :fuzzy (eq helm-completion-style 'helm-fuzzy)
          :buffer buffer
          ;; If DEF is not provided, fallback to empty string
          ;; to avoid `thing-at-point' to be appended on top of list
          :default (or default "")
          ;; Fail with special characters (e.g in gnus "nnimap+gmail:")
          ;; if regexp-quote is not used.
          ;; when init is added to history, it will be unquoted by
          ;; helm-comp-read.
          :initial-input initial-input)
      (dolist (f flags) (set f nil)))))

(defun helm-completing-read-default-2
    (prompt collection predicate require-match
     init hist default _inherit-input-method
     name buffer &optional _cands-in-buffer exec-when-only-one)
  "Helm `completing-read' handler with dynamic matching.

Call `helm-comp-read' with same args as `completing-read'.
For the meaning of optional args see `helm-completing-read-default-1'.
This handler uses dynamic matching which allows honouring `completion-styles'."
  (let* ((history (or (car-safe hist) hist))
         (input (pcase init
                  ((pred (stringp)) init)
                  ;; INIT is a cons cell.
                  (`(,l . ,_ll) l)))
         (completion-flex-nospace t)
         (minibuffer-completion-table collection)
         ;; (completion-styles
         ;;  (helm--prepare-completion-styles 'nomode))
         (metadata (or (completion-metadata (or input "") collection predicate)
                       '(metadata)))
         (afun (or (plist-get completion-extra-properties :annotation-function)
                   (completion-metadata-get metadata 'annotation-function)))
         (afix (or (plist-get completion-extra-properties :affixation-function)
                   (completion-metadata-get metadata 'affixation-function)))
         (category (completion-metadata-get metadata 'category))
         (compfn (lambda (str _predicate _action)
                   (let* ((completion-ignore-case (helm-set-case-fold-search))
                          (comps
                           (completion-all-completions
                            str         ; This is helm-pattern
                            collection
                            predicate
                            (length str)
                            metadata))
                          (last-data (last comps))
                          ;; Helm syle sort fn is added to
                          ;; metadata only in emacs-27, so in
                          ;; emacs-26 use helm-generic-sort-fn
                          ;; which handle both helm and
                          ;; helm-flex styles. When
                          ;; helm-completion-style is helm or
                          ;; helm-fuzzy, sorting will be done
                          ;; later in FCT.
                          (sort-fn
                           (and (eq helm-completion-style 'emacs)
                                (or
                                 ;; Emacs-27
                                 (completion-metadata-get
                                  metadata 'display-sort-function)
                                 ;; Emacs-26
                                 (lambda (candidates)
                                   (sort candidates #'helm-generic-sort-fn)))))
                          all)
                     (when (cdr last-data)
                       ;; Remove the last element of
                       ;; comps by side-effect.
                       (setcdr last-data nil))
                     (setq helm-completion--sorting-done (and sort-fn t))
                     (setq all (copy-sequence comps))
                     ;; Default is passed here only with helm
                     ;; h-c-styles, otherwise with emacs style it is
                     ;; passed with the :default arg of helm-comp-read
                     ;; and computed in its get-candidates function.
                     (append (and default
                                  (memq helm-completion-style '(helm helm-fuzzy))
                                  (list default))
                             (helm-completion--initial-filter
                              (let ((lst (if (and sort-fn (> (length str) 0))
                                             (funcall sort-fn all)
                                           all)))
                                (if (and default afix)
                                    (prog1 (append (list default)
                                                   (delete default lst))
                                      (setq default nil))
                                  lst))
                              afun afix category)))))
         (data (if (memq helm-completion-style '(helm helm-fuzzy))
                   (funcall compfn (or input "") nil nil)
                 compfn))
         (helm-completion-in-region-default-sort-fn
          (lambda (candidates _source)
            (if (or helm-completion--sorting-done
                    (string= helm-pattern ""))
                candidates
              (sort candidates 'helm-generic-sort-fn)))))
    (unwind-protect
        (helm-comp-read
         ;; Completion-at-point and friends have no prompt.
         prompt
         data
         :name name
         :initial-input input
         :buffer buffer
         :history history
         :nomark (null helm-comp-read-use-marked)
         :reverse-history helm-mode-reverse-history
         ;; In helm h-c-styles default is passed directly in
         ;; candidates.
         :default (and (eq helm-completion-style 'emacs) (null afix) default)
         :fc-transformer
         ;; Ensure sort fn is at the end.
         (append '(helm-cr-default-transformer)
                 (and helm-completion-in-region-default-sort-fn
                      (list helm-completion-in-region-default-sort-fn)))
         :match-dynamic (eq helm-completion-style 'emacs)
         :diacritics helm-mode-ignore-diacritics
         :fuzzy (eq helm-completion-style 'helm-fuzzy)
         :exec-when-only-one exec-when-only-one
         :quit-when-no-cand (eq require-match t)
         :must-match require-match)
      (setq helm-completion--sorting-done nil))))

(defun helm-mode-all-the-icons-handler (prompt collection test require-match
                                        init hist default inherit-input-method
                                        name buffer)
  "Helm `completing-read' handler for `all-the-icons-insert'."
  (let* ((max-len 0)
         sname
         (cands (cl-loop for (desc . str) in collection
                         ;; When the FAMILY argument is passed to
                         ;; `all-the-icons-insert' DESC is the name of icon only
                         ;; otherwise it is "name  [family]" with unpredictable
                         ;; spaces or tab numbers between name and [family].
                         for descnp = (substring-no-properties desc)
                         for sdesc = (if (string-match
                                          "\\(.*\\)[[:blank:]]+\\(\\[.*\\]\\)" descnp)
                                         ;; This is all-the-icons-insert function.
                                         (match-string 1 descnp)
                                       ;; This is one of
                                       ;; all-the-icons-insert-<family>
                                       ;; functions, extract the family name.
                                       (prog1 descnp
                                         (unless sname
                                           (setq sname (plist-get
                                                        (get-text-property
                                                         0 'font-lock-face
                                                         (get-text-property 0 'display desc))
                                                        :family)))))
                         for sdesc2 = (match-string 2 descnp)
                         do (setq max-len (max max-len (string-width sdesc)))
                         collect (cons (concat sdesc " " str " " sdesc2) desc)))
         (fn (lambda ()
               (with-helm-buffer
                 (save-excursion
                   (goto-char (point-min))
                   (helm-skip-header-and-separator-line 'next)
                   (while (re-search-forward "^[[:alnum:]_-]+" nil t)
                     (insert (make-string (- max-len (current-column)) ? )))))))
         (helm-after-update-hook (append helm-after-update-hook `(,fn))))
    (helm-completing-read-default-1 prompt cands test require-match
                                    init hist default inherit-input-method
                                    (or sname name) buffer t nil t 'buffer-substring)))

(defun helm-completing-read-default-find-tag
    (prompt collection test require-match
     init hist default inherit-input-method
     name buffer)
  "Helm `completing-read' handler for `find-tag'."
  ;; Some commands like find-tag may use `read-file-name' from inside
  ;; the calculation of collection. in this case it clash with
  ;; candidates-in-buffer that reuse precedent data (files) which is wrong.
  ;; So (re)calculate collection outside of main helm-session.
  (let* ((cands (helm-comp-read-get-candidates
                 collection test nil nil)))
    (helm-completing-read-default-1 prompt cands test require-match
                                    init hist default inherit-input-method
                                    name buffer t)))

(defun helm-completing-read-sync-default-handler
    (prompt collection test require-match
     init hist default inherit-input-method
     name buffer)
  "Helm `completing-read' handler using sync source as backend."
  (helm-completing-read-default-1 prompt collection test require-match
                                  init hist default inherit-input-method
                                  name buffer))

(defun helm-completing-read-inbuffer-default-handler
    (prompt collection test require-match
     init hist default inherit-input-method
     name buffer)
  "Helm `completing-read' handler using inbuffer source as backend."
  (helm-completing-read-default-1 prompt collection test require-match
                                  init hist default inherit-input-method
                                  name buffer t))

(defun helm-completing-read-default-handler
    (prompt collection test require-match
     init hist default inherit-input-method
     name buffer)
  "Default Helm `completing-read' handler.

Use either `helm-completing-read-default-1' or `helm-completing-read-default-2'
according to `helm-completion-style'."
  (let* (;; Standard will be used as CANDS-IN-BUFFER arg.
         (standard (and (memq helm-completion-style '(helm helm-fuzzy)) t))
         (fn (if standard
                 #'helm-completing-read-default-1
               #'helm-completing-read-default-2)))
    (funcall fn
             prompt collection test require-match
             init hist default inherit-input-method name buffer
             ;; CANDS-IN-BUFFER
             standard)))

(defun helm-mode--read-buffer-to-switch (prompt)
  "[INTERNAL] This is used to advice `read-buffer-to-switch'.
Don't use it directly."
  ;; `read-buffer-to-switch' is passing `minibuffer-completion-table'
  ;; to `read-buffer' through `minibuffer-setup-hook' which is too
  ;; late to be known by `read-buffer-function', in our case
  ;; `helm--generic-read-buffer'.  It should let bind it to allow us
  ;; using it. 
  (let ((minibuffer-completion-table (internal-complete-buffer-except)))
    (read-buffer prompt (other-buffer (current-buffer))
                 (confirm-nonexistent-file-or-buffer))))

(defun helm--generic-read-buffer (prompt &optional default require-match predicate)
  "The `read-buffer-function' for `helm-mode'.
Affects `switch-to-buffer' `kill-buffer' and related."
  ;; `read-buffer' is using internally `Vbuffer_alist' which is an
  ;; alist with elements like (BUF-NAME . BUF-OBJ), therefore some
  ;; predicates in Emacs are working only on such cons cells.
  ;; However, helm is transforming COLLECTION in a list of strings and
  ;; such predicates are failing because they expect cons cells (see
  ;; bug#2506 with `project-switch-to-buffer'), even if they should
  ;; handle strings as well according to `read-buffer'
  ;; documentation.
  (let ((pred (when predicate
                (lambda (buffer)
                  (let ((buf (cons buffer (get-buffer buffer))))
                    (condition-case _err
                        (funcall predicate buffer)
                      (wrong-type-argument
                       (funcall predicate buf))))))))
    (helm--completing-read-default
     prompt (or minibuffer-completion-table
                (internal-complete-buffer "" nil t))
     pred require-match nil nil default)))

(defun helm-mode--get-default-handler-for (comp-or-file entry)
  ;; Use 'comp for completing-read and 'file for 'read-file-name as
  ;; COMP-OR-FILE value.
  (let ((val (cdr-safe entry))
        (reading-file (eq comp-or-file 'file)))
    (if (consp val)
        (helm-acase (if reading-file (cadr val) (car val))
          (default (if reading-file
                       #'helm-read-file-name
                     #'helm-completing-read-default-handler))
          (t it))
      val)))

(defun helm-mode--apply-helm-handler (handler arg-list)
  "Ensure `minibuffer-complete' is disabled when running HANDLER.
ARG-LIST is a list of arguments to pass to HANDLER."
  ;; Some functions are calling `minibuffer-complete'
  ;; within `minibuffer-setup-hook' when calling their
  ;; `completing-read', like `woman-file-name' (bug #2527).
  ;; This defeat Helm which is already
  ;; completing minibuffer, so deactivate
  ;; minibuffer-complete one time for all [1].
  (cl-letf (((symbol-function 'minibuffer-complete) #'ignore))
    (apply handler arg-list)))

(cl-defun helm--completing-read-default
    (prompt collection &optional
                         predicate require-match
                         initial-input hist def
                         inherit-input-method)
  "An helm replacement of `completing-read'.
This function should be used only as a `completing-read-function'.

Don't use it directly, use instead `helm-comp-read' in your programs.

See documentation of `completing-read' and `all-completions' for details."
  (let* ((current-command (or (helm-this-command) this-command))
         (str-command     (if current-command
                              (helm-symbol-name current-command)
                            "completing-read"))
         (buf-name        (format "*helm-mode-%s*" str-command))
         (entry           (assq current-command
                                helm-completing-read-handlers-alist))
         (def-com         (helm-mode--get-default-handler-for 'comp entry))
         (str-defcom      (and def-com (helm-symbol-name def-com)))
         (def-args        (list prompt collection predicate
                                (helm-aif (assq current-command
                                          helm-comp-read-require-match-overrides)
                                    (cdr it) require-match)
                                initial-input hist def inherit-input-method))
         ;; Append the two extra args needed to set the buffer and source name
         ;; in helm specialized functions.
         (others-args        (append def-args (list str-command buf-name)))
         helm-completion-mode-start-message ; Be quiet
         helm-completion-mode-quit-message
         ;; Be sure this pesty *completion* buffer doesn't popup.
         ;; Note: `minibuffer-with-setup-hook' may setup a lambda
         ;; calling `minibuffer-completion-help' or other minibuffer
         ;; functions we DONT WANT here, in these cases removing the hook
         ;; (a symbol) have no effect. Bug#448.
         ;; Because `minibuffer-completion-table' and
         ;; `minibuffer-completion-predicate' are not bound
         ;; anymore here, these functions should have no effect now,
         ;; except in some rare cases like in `woman-file-name',
         ;; so remove all incompatible functions
         ;; from `minibuffer-setup-hook' (Bug#1205, Bug#1240).
         ;; otherwise helm have not the time to close its initial session.
         (minibuffer-setup-hook
          (cl-loop for h in minibuffer-setup-hook
                   unless (or (consp h) ; a lambda.
                              (byte-code-function-p h)
                              (helm-subr-native-elisp-p h)
                              (memq h helm-mode-minibuffer-setup-hook-black-list))
                   collect h))
         ;; Disable hack that could be used before `completing-read'.
         ;; i.e (push ?\t unread-command-events).
         unread-command-events
         ;; Let-bounding here helm-completion-style according to
         ;; helm-completion-styles-alist allow using helm style per commands.
         (helm-completion-style (helm-aif (cdr (assq current-command helm-completion-styles-alist))
                                    (if (cdr-safe it) (car it) it)
                                  (default-value 'helm-completion-style)))
         (completion-styles
          (helm--prepare-completion-styles current-command))
         (default-handler
          ;; If nothing is found in
          ;; helm-completing-read-handlers-alist use default
          ;; handler which will itself use `helm-completion-style'.
          #'helm-completing-read-default-handler))
    (when (eq def-com 'ido) (setq def-com 'ido-completing-read))
    (unless (or (not entry) def-com)
      ;; An entry in *read-handlers-alist exists but have
      ;; a nil value, so we exit from here, disable `helm-mode'
      ;; and run the command again with it original behavior.
      ;; `helm-mode' will be restored on exit.
      (cl-return-from helm--completing-read-default
        (unwind-protect
             (progn
               (helm-mode -1)
               (apply completing-read-function def-args))
          (helm-mode 1))))
    ;; If we use now `completing-read' we MUST turn off `helm-mode'
    ;; to avoid infinite recursion and CRASH. It will be reenabled on exit.
    (when (or (eq def-com 'completing-read)
              ;; All specialized functions are prefixed by "helm"
              (and (stringp str-defcom)
                   (not (string-match "^helm" str-defcom))))
      (helm-mode -1))
    (unwind-protect
         (cond (;; An helm specialized function exists, run it.
                (and def-com helm-mode)
                ;; Disable `minibuffer-complete' for handlers using
                ;; helm (bug #2533).
                (helm-mode--apply-helm-handler
                 def-com others-args))
               (;; Try to handle `ido-completing-read' everywhere.
                (and def-com (eq def-com 'ido-completing-read))
                (setcar (memq collection def-args)
                        (all-completions "" collection predicate))
                (apply def-com def-args))
               (;; A non helm function specified in
                ;; `helm-completing-read-handlers-alist' use it with
                ;; exactly the same args as in `completing-read'.  If
                ;; we are here `helm-mode' is now disabled.
                def-com
                (apply def-com def-args))
               (;; Use by default a in-buffer handler unless
                ;; COLLECTION is a function.
                t
                ;; Disable `minibuffer-complete' for handlers using
                ;; helm (bug #2533).
                (helm-mode--apply-helm-handler
                 default-handler others-args)))
      (helm-mode 1)
      ;; When exiting minibuffer, `this-command' is set to
      ;; `helm-exit-minibuffer', which is unwanted when starting
      ;; on another `completing-read', so restore `this-command' to
      ;; initial value when exiting.
      (setq this-command current-command))))

;;; Generic read-file-name
;;
;;
;;;###autoload
(cl-defun helm-read-file-name
    (prompt
     &key
       (name "Read File Name")
       initial-input
       (buffer "*Helm file completions*")
       test
       noret
       (case-fold helm-file-name-case-fold-search)
       preselect
       history
       must-match
       (fuzzy t)
       default
       marked-candidates
       (candidate-number-limit helm-ff-candidate-number-limit)
       nomark
       (alistp t)
       (persistent-action-if 'helm-find-files-persistent-action-if)
       (persistent-help "Hit1 Expand Candidate, Hit2 or (C-u) Find file")
       (mode-line helm-read-file-name-mode-line-string))
  "Read a file name with helm completion.

It is helm `read-file-name' emulation.

Argument PROMPT is the default prompt to use.

Keys description:

- NAME: Source name, default to \"Read File Name\".

- INITIAL-INPUT: Where to start reading file name,
                 default to `default-directory' or $HOME.

- BUFFER: `helm-buffer' name, defaults to \"*Helm Completions*\".

- TEST: A predicate called with one arg \\='candidate'.

- NORET: Allow disabling helm-ff-RET (have no effect if helm-ff-RET
                                      isn't bound to RET).

- CASE-FOLD: Same as `helm-case-fold-search'.

- PRESELECT: helm preselection.

- HISTORY: Display HISTORY in a special source.

- MUST-MATCH: Can be \\='confirm, nil, or t.

- FUZZY: Enable fuzzy matching when non-nil (Enabled by default).

- MARKED-CANDIDATES: When non--nil return a list of marked candidates.

- NOMARK: When non--nil don't allow marking candidates.

- ALISTP: Don't use `all-completions' in history
          (take effect only on history).

- PERSISTENT-ACTION-IF: a persistent if action function.

- PERSISTENT-HELP: persistent help message.

- MODE-LINE: A mode line message, default is
             `helm-read-file-name-mode-line-string'."
  (require 'tramp)
  (unless initial-input
    (setq initial-input (or default-directory (getenv "HOME"))))
  (when (get-buffer helm-action-buffer)
    (kill-buffer helm-action-buffer))
  (mapc (lambda (hook)
          (add-hook 'helm-after-update-hook hook))
        '(helm-ff-move-to-first-real-candidate
          helm-ff-update-when-only-one-matched
          helm-ff-auto-expand-to-home-or-root))
  (let* ((action-fn `(("Sole action (Identity)"
                       . (lambda (candidate)
                           (if ,marked-candidates
                               (helm-marked-candidates :with-wildcard t)
                             (identity candidate))))))
         ;; Be sure we don't erase the underlying minibuffer if some.
         (helm-ff-auto-update-initial-value
          (and helm-ff-auto-update-initial-value
               (not (minibuffer-window-active-p (minibuffer-window)))))
         helm-follow-mode-persistent
         (helm-ff-fuzzy-matching
          (and fuzzy
               (not (memq helm-mm-matching-method '(multi1 multi3p)))))
         (hist (and history (helm-comp-read-get-candidates
                             history nil nil alistp)))
         (helm-ff--RET-disabled noret)
         (minibuffer-completion-predicate test)
         (minibuffer-completing-file-name t)
         ;; Ensure not being prompted for password each time we
         ;; navigate to a directory.
         (password-cache t)
         (helm--completing-file-name t)
         (helm-read-file-name-mode-line-string
          (replace-regexp-in-string "helm-maybe-exit-minibuffer"
                                    "helm-confirm-and-exit-minibuffer"
                                    helm-read-file-name-mode-line-string))
         (src-list
          (list
           ;; History source.
           (helm-build-sync-source (format "%s History" name)
             :header-name (lambda (name)
                            (concat name (substitute-command-keys
                                          helm-find-files-doc-header)))
             :mode-line mode-line
             :candidates hist
             :nohighlight t
             :fuzzy-match fuzzy
             :persistent-action-if persistent-action-if
             :persistent-help persistent-help
             :keymap helm-read-file-map
             :must-match must-match
             :nomark nomark
             :action action-fn)
           ;; Other source.
           (helm-build-sync-source name
             :header-name (lambda (name)
                            (concat name (substitute-command-keys
                                          helm-find-files-doc-header)))
             :init (lambda ()
                     (setq helm-ff-auto-update-flag
                           helm-ff-auto-update-initial-value)
                     (setq helm-ff--auto-update-state
                           helm-ff-auto-update-flag))
             :mode-line mode-line
             :help-message 'helm-read-file-name-help-message
             :nohighlight helm-ff-nohighlight-matches
             :candidate-number-limit 'helm-ff-candidate-number-limit
             :candidates
             (lambda ()
               (if test
                   (append (and (not (file-exists-p helm-pattern))
                                (not (helm-ff--invalid-tramp-name-p helm-pattern))
                                (list (helm-ff-filter-candidate-one-by-one
                                       helm-pattern nil t)))
                           (cl-loop with hn = (helm-ff--tramp-hostnames)
                                    ;; helm-find-files-get-candidates is
                                    ;; returning a list of cons cells.
                                    for (d . r) in (helm-find-files-get-candidates
                                                    must-match)
                                    when (or (member r hn) ; A tramp host
                                             (funcall test r)) ; Test ok
                                    collect (cons d r)))
                 (helm-find-files-get-candidates must-match)))
             :update (lambda ()
                       (remhash helm-ff-default-directory
                                helm-ff--list-directory-cache))
             :match-on-real t
             :filtered-candidate-transformer '(helm-ff-fct
                                               helm-ff-maybe-show-thumbnails
                                               helm-ff-sort-candidates)
             :persistent-action-if persistent-action-if
             :persistent-help persistent-help
             :volatile t
             :keymap helm-read-file-map
             :must-match must-match
             :cleanup 'helm-find-files-cleanup
             :nomark nomark
             :action action-fn)))
         ;; Helm result.
         (result (helm
                  :sources (if helm-mode-reverse-history
                               (reverse src-list) src-list)
                  :input (if (string-match helm-ff-url-regexp initial-input)
                             initial-input
                           (expand-file-name initial-input))
                  :prompt prompt
                  :candidate-number-limit candidate-number-limit
                  :resume 'noresume
                  :case-fold-search case-fold
                  :default default
                  :buffer buffer
                  :full-frame nil
                  :preselect preselect)))
    (or
     (cond ((and result (stringp result)
                 (string= result "") ""))
           ((and result
                 (stringp result)
                 (file-equal-p result initial-input)
                 helm-read-file-name-use-default-arg-behavior
                 default)
            (if (listp default) (car default) default))
           ((and result (listp result))
            (mapcar #'expand-file-name result))
           ((and result (file-directory-p result))
            (file-name-as-directory (expand-file-name result)))
           (result (expand-file-name result)))
     (helm-mode--keyboard-quit))))

(defun helm-mode--default-filename (fname dir initial)
  (unless dir (setq dir default-directory))
  (unless (file-name-absolute-p dir)
    (setq dir (expand-file-name dir)))
  (unless (or fname (consp fname))
    (setq fname (expand-file-name
                 (or initial buffer-file-name dir)
                 dir)))
  (if (and fname (consp fname))
      (setq fname (cl-loop for f in fname
                           collect (if (file-name-absolute-p fname)
                                       (expand-file-name
                                        f (helm-mode-root-dir dir))
                                     (expand-file-name fname dir))))
      (if (file-name-absolute-p fname)
          (if (file-remote-p fname)
              fname
            (substitute-in-file-name
             (concat (helm-mode-root-dir dir) fname)))
        (expand-file-name fname dir))))

(defun helm-mode-root-dir (dir)
  (if (file-remote-p dir)
      (let* ((host        (file-remote-p dir 'host))
             (method      (file-remote-p dir 'method))
             (user        (file-remote-p dir 'user)))
        (format "/%s:%s@%s:/" method user host))
    "/"))

(cl-defun helm--generic-read-file-name
    (prompt &optional dir default-filename mustmatch initial predicate)
  "Generic helm replacement of `read-file-name'.
Don't use it directly, use instead `helm-read-file-name' in your programs."
  (let* ((init (or initial dir default-directory))
         (helm-read-file-name-use-default-arg-behavior t)
         (current-command (or (helm-this-command) this-command))
         (str-command (if current-command
                          (helm-symbol-name current-command)
                        "read-file-name"))
         (helm--file-completion-sources
          (cons str-command
                (remove str-command helm--file-completion-sources)))
         (buf-name (format "*helm-mode-%s*" str-command))
         (entry (assq current-command
                      helm-completing-read-handlers-alist))
         (def-com    (helm-mode--get-default-handler-for 'file entry))
         (str-defcom (and def-com (helm-symbol-name def-com)))
         ;; Don't modify the original args list for emacs generic functions.
         (def-args (list prompt dir default-filename mustmatch initial predicate))
         ;; Append the two extra args needed to set the buffer and source name
         ;; in helm specialized functions.
         (others-args (append def-args (list str-command buf-name)))
         (reading-directory (eq predicate 'file-directory-p))
         (use-dialog (and (next-read-file-uses-dialog-p)
                          ;; Graphical file dialogs can't handle
                          ;; remote files.
                          (not (file-remote-p init))
                          use-file-dialog))
         helm-completion-mode-start-message ; Be quiet
         helm-completion-mode-quit-message  ; Same here
         add-to-history fname)
    ;; Build `default-filename' with `dir'+`initial' when
    ;; `default-filename' is not specified.
    ;; See `read-file-name' docstring for more infos.
    (setq default-filename (helm-mode--default-filename
                            default-filename dir initial))
    ;; Some functions that normally call `completing-read' can switch
    ;; brutally to `read-file-name' (e.g find-tag), in this case
    ;; the helm specialized function will fail because it is build
    ;; for `completing-read', so set it to 'incompatible to be sure
    ;; we switch to `helm-read-file-name' and don't try to call it
    ;; with wrong number of args.
    (when (eq def-com 'ido)
      (setq def-com 'ido-read-file-name))
    (when (and def-com (> (length (help-function-arglist def-com)) 8))
      (setq def-com 'incompatible))
    (unless (or (not entry) def-com)
      (cl-return-from helm--generic-read-file-name
        (unwind-protect
             (progn
               (helm-mode -1)
               (apply read-file-name-function def-args))
          (helm-mode 1))))
    ;; If we use now `read-file-name' or dialog we MUST turn off `helm-mode'
    ;; to avoid infinite recursion and CRASH. It will be reenabled on exit.
    (when (or (memq def-com '(read-file-name ido-read-file-name))
              use-dialog
              (and (stringp str-defcom)
                   (not (string-match "^helm" str-defcom))))
      (helm-mode -1))
    (unwind-protect
         (setq fname
               (cond (use-dialog
                      (let ((dialog-mustmatch
                             (not (memq mustmatch
                                        '(nil confirm confirm-after-completion)))))
                        ;; Dialogs don't support a list of default fnames.
                        (when (and default-filename (consp default-filename))
                          (setq default-filename
                                (expand-file-name (car default-filename) init)))
                        (setq add-to-history t)
                        (x-file-dialog prompt init default-filename
                                       dialog-mustmatch
                                       reading-directory)))
                     ;; A specialized function exists, run it
                     ;; with the two extra args specific to helm.
                     ;; Note that the helm handler should ensure
                     ;; :initial-input is not nil i.e. Use init
                     ;; which fallback to default-directory instead
                     ;; of INITIAL.
                     ((and def-com helm-mode
                           (not (eq def-com 'ido-read-file-name))
                           (not (eq def-com 'incompatible))
                           ;; The entry in
                           ;; `helm-completing-read-handlers-alist' is
                           ;; a cons cell specifying a completing-read
                           ;; and a read-file-name handler default
                           ;; e.g. (foo (default default)).
                           (not (eq def-com 'helm-read-file-name)))
                      (apply def-com others-args))
                     (;; Def-com value is `ido-read-file-name'
                      ;; run it with default args.
                      (and def-com (eq def-com 'ido-read-file-name))
                      (ido-mode 1)
                      (apply def-com def-args))
                     (;; Def-com value is `read-file-name'
                      ;; run it with default args.
                      (eq def-com 'read-file-name)
                      (apply def-com def-args))
                     (t  ; Fall back to classic `helm-read-file-name'.
                      (helm-read-file-name
                       prompt
                       :name str-command
                       :buffer buf-name
                       :default default-filename
                       ;; Helm handlers should always have a non nil INITIAL arg.
                       :initial-input (if (string-match helm-ff-url-regexp init)
                                          init
                                        (if (file-name-absolute-p init)
                                            (if (file-remote-p init)
                                                init
                                              (substitute-in-file-name
                                               (concat (helm-mode-root-dir
                                                        (or dir init))
                                                       init)))
                                          (expand-file-name init dir)))
                       :alistp nil
                       :nomark (null helm-comp-read-use-marked)
                       :marked-candidates helm-comp-read-use-marked
                       :must-match mustmatch
                       :test predicate
                       :noret reading-directory))))
      (and ido-mode (ido-mode -1))
      (helm-mode 1)
      ;; Same comment as in `helm--completing-read-default'.
      (setq this-command current-command))
    (when add-to-history
      (add-to-history 'file-name-history
                      (minibuffer-maybe-quote-filename fname)))
    (if (and
         ;; Using `read-directory-name'.
         reading-directory
         ;; `file-name-as-directory' return "./" when FNAME is
         ;; empty string.
         (not (string= fname "")))
        (file-name-as-directory fname)
      fname)))

;; Read file name handler with history (Bug#1652)
(defun helm-read-file-name-handler-1 (prompt dir default-filename
                                      mustmatch initial predicate
                                      name buffer)
  "A `read-file-name' handler with history.
Can be added to `helm-completing-read-handlers-alist' for functions
that need a `read-file-name' function with directory history.
The `helm-find-files' history `helm-ff-history' is used here."
  (let ((helm-always-two-windows t)
        (helm-split-window-default-side
         (if (eq helm-split-window-default-side 'same)
             'below helm-split-window-default-side))
        helm-reuse-last-window-split-state
        ;; Helm handlers should always have a non nil INITIAL arg.
        (init (or initial dir default-directory)))
    (helm-read-file-name
     prompt
     :name name
     :history helm-ff-history
     :buffer buffer
     :default default-filename
     :initial-input (expand-file-name init dir)
     :alistp nil
     :must-match mustmatch
     :test predicate)))


;;; Completion in region and Helm style
;;
(defun helm-mode--advice-lisp--local-variables (old--fn &rest args)
  (ignore-errors
    (apply old--fn args)))

(defvar helm-completion--sorting-done nil
  "Flag that notifies the FCT if sorting has been done in completion function.")
(defun helm-completion-in-region-sort-fn (candidates _source)
  "Default sort function for completion-in-region."
  (if helm-completion--sorting-done
      candidates
    (sort candidates 'helm-generic-sort-fn)))

(defun helm-mode--completion-in-region-initial-input (str)
  "Highlight prefix in helm and helm-fuzzy `helm-completion-styles'."
  (if (memq helm-completion-style '(helm helm-fuzzy))
      (propertize str 'read-only t 'face 'helm-mode-prefix 'rear-nonsticky t)
    str))

(defun helm-completion--initial-filter (comps afun afix category)
  "Compute COMPS with function AFIX or AFUN.

When CATEGORY is file or library remove dot files from COMPS.

If both AFUN and AFIX are provided, AFIX takes precedence.

When AFUN, AFIX are nil and CATEGORY is not file return COMPS unmodified."
  ;; Normally COMPS should be a list of
  ;; string but in some cases it is given as a list of strings containing a list
  ;; of string e.g. ("a" "b" "c" ("d" "e" "f")) ; This happen in rgrep
  ;; (bug#2607) and highlight-* fns (bug #2610), so ensure the list is flattened to
  ;; avoid e.g. wrong-type argument: stringp '("d" "e" "f")
  ;; FIXME: If this create a new bug with completion-in-region, flatten COMPS
  ;; directly in the caller i.e. helm-completing-read-default-1.
  (when (or afix afun (memq category '(file library)))
    (setq comps (helm-fast-remove-dups
                 (helm-flatten-list comps)
                 :test 'equal)))
  ;; Filter out dot files in file completion.
  ;; We were previously exiting directly without handling afix and afun, but
  ;; maybe some file completion tables have an afix or afun in their metadata so
  ;; let them a chance to run these functions if some.
  (when (memq category '(file library))
    (setq comps
          (cl-loop for f in comps
                   unless (string-match "\\`\\.\\{1,2\\}/\\'" f)
                   collect f)))
  (cond (afix (let ((affixations (funcall afix comps)))
                (if (functionp affixations)
                    (cl-loop for comp in comps
                             for cand = (funcall affixations comp)
                             when cand
                             collect (cons (propertize (concat (nth 1 cand) ;prefix
                                                               (nth 0 cand) ;comp
                                                               (nth 2 cand)) ;suffix
                                                       'match-part (nth 0 cand)) 
                                           comp))
                  (cl-loop for (comp prefix suffix) in affixations
                           collect (cons (propertize
                                          (concat prefix comp suffix)
                                          'match-part comp)
                                         comp)))))
        (afun
         ;; Add annotation at end of
         ;; candidate if needed, e.g. foo<f>, this happen when
         ;; completing against a quoted symbol.
         (mapcar (lambda (s)
                   (let ((ann (funcall afun s)))
                     (or (helm-aand
                          ann
                          (propertize ann 'face 'helm-completions-annotations)
                          (cons (concat s (propertize " " 'display it)) s))
                         s)))
                 comps))
        (t comps)))

;; Helm multi matching style

(defun helm-completion-try-completion (string table pred point)
  "The try completion function for `completing-styles-alist'.
Actually does nothing."
  ;; AFAIU the try-completions style functions
  ;; are here to check if what is at point is suitable for TABLE but
  ;; there is no way to pass a multiple pattern from what is at point
  ;; apart sending STRING in a minibuffer like helm does.  Perhaps
  ;; minibuffer-complete should benefit of this but for now just do
  ;; nothing as this is used nowhere.  It is anyway not clear what the
  ;; try-completions functions do in emacs so just do nothing for now.
  (ignore string table pred point))

(defun helm-completion-all-completions (string table pred point)
  "The all completions function for `completing-styles-alist'."
  ;; FIXME: No need to bind all these value.
  ;; (cl-multiple-value-bind (all _pattern prefix _suffix _carbounds)
  (pcase-let ((`(,all ,_pattern ,prefix ,_suffix ,_carbounds)
               (helm-completion--multi-all-completions string table pred point)))
    (when all (nconc all (length prefix)))))

(defun helm-completion--multi-all-completions-1 (string collection &optional predicate)
  "Allow `all-completions' multi matching on its candidates."
  ;; Doing an initial call of all-completions on the first element of
  ;; STRING speedup completion and fix file completion when CAPF
  ;; returns relative paths to initial pattern (eshell and shell).
  (let* ((split (helm-mm-split-pattern string))
         (fpat (or (car split) ""))
         (file-comp-p (or minibuffer-completing-file-name
                          (eq
                           (completion-metadata-get
                            (completion-metadata string collection predicate)
                            'category)
                           'file)))
         (all (and file-comp-p
                   (or (cdr split)
                       (and (not (cdr split))
                            ;; Kickin when STRING is a simple string.
                            ;; Handle as well "foo " (space at end).
                            (not (string= fpat "")))
                       (string= string ""))
                   (not (string-match "\\`!" fpat))
                   ;; all-completions should return nil if FPAT is a
                   ;; regexp, it is what we expect.
                   (all-completions fpat collection
                                    (lambda (x &optional _y)
                                      (let ((elm (if (listp x) (car x) x)))
                                        (funcall (or predicate #'identity) elm))))))
         (pattern (helm-aand all (string-match " " string)
                             ;; Returns the part of STRING after space
                             ;; e.g. "foo bar baz" => "bar baz".
                             (substring string (1+ it)))))
    (if (or (and all (not (cdr split)))
            (equal pattern "")) ; e.g. STRING == "foo ".
        all
      (all-completions "" (or all collection)
                       (lambda (x &optional _y)
                         ;; Second arg _y is needed when
                         ;; COLLECTION is a hash-table (Bug#2231)
                         ;; (C-x 8 RET).
                         ;; Elements of COLLECTION may be
                         ;; lists or alists, in this case consider the
                         ;; car of element (Bug#2219 org-refile).
                         (let ((elm (if (listp x) (car x) x)))
                           ;; PREDICATE have been already called in
                           ;; initial all-completions, no need to call
                           ;; it a second time, thus ALL is now a list
                           ;; of strings maybe not supported by
                           ;; PREDICATE (e.g. symbols vs strings).
                           (if (and predicate (null all))
                               (and (funcall predicate elm)
                                    ;; ALL is nil so use whole STRING
                                    ;; against COLLECTION.
                                    (helm-mm-match (helm-stringify elm) string))
                             (helm-mm-match (helm-stringify elm)
                                            (or (and all pattern) string)))))))))

(defun helm-completion--multi-all-completions (string table pred point)
  "Collect completions from TABLE for helm completion style."
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (bounds (completion-boundaries beforepoint table pred afterpoint))
         (prefix (substring beforepoint 0 (car bounds)))
         (suffix (substring afterpoint (cdr bounds)))
         (all (helm-completion--multi-all-completions-1
               ;; Using `regexp-quote' on STRING fixes bug#2355 but
               ;; breaks regexp matching in multi match, actually with
               ;; Helm-3.7.1 and emacs-27+ it seems using plain STRING
               ;; works for both so use it.
               ;;(regexp-quote string)
               string table pred)))
    (list all string prefix suffix point)))

;; The adjust-metadata functions run only in emacs-27, they are NOT
;; used otherwise.
(defun helm-completion--adjust-metadata (metadata)
  (if (memq helm-completion-style '(helm helm-fuzzy))
      metadata
    (let ((compose-helm-sort-fn
           (lambda (candidates)
             (sort candidates #'helm-generic-sort-fn))))
      `(metadata
        (display-sort-function
         . ,compose-helm-sort-fn)
        (cycle-sort-function
         . ,compose-helm-sort-fn)
        ,@(cdr metadata)))))
(put 'helm 'completion--adjust-metadata 'helm-completion--adjust-metadata)

;; Helm-flex style.
;; This is more or less the same as emacs-27 flex style.
(defun helm-flex-completion-try-completion (string table pred point)
  "The try completion function for `completing-styles-alist'."
  ;; It is needed here to make minibuffer-complete work in emacs-26,
  ;; e.g. with regular M-x.
  (unless (string-match-p " " string)
    (pcase-let ((`(,all ,pattern ,prefix ,suffix ,_carbounds)
                 (helm-completion--flex-all-completions string table pred point)))
      (when minibuffer-completing-file-name
        (setq all (completion-pcm--filename-try-filter all)))
      (completion-pcm--merge-try pattern all prefix suffix))))

(defun helm-flex-completion-all-completions (string table pred point)
  "The all completions function for `completing-styles-alist'."
  ;; FIXME: No need to bind all these value.
  (unless (string-match-p " " string)
    (pcase-let ((`(,all ,pattern ,prefix ,_suffix ,_carbounds)
                 (helm-completion--flex-all-completions
                  string table pred point
                  #'helm-completion--flex-transform-pattern)))
      (let ((regexp (completion-pcm--pattern->regex pattern 'group)))
        (when all (nconc (helm-flex-add-score-as-prop all regexp)
                         (length prefix)))))))

;; Same as emacs-27 completion-substring--all-completions.
(defun helm-completion--flex-all-completions
    (string table pred point &optional transform-pattern-fn)
  "Match the presumed substring STRING to the entries in TABLE.
Respect PRED and POINT.  The pattern used is a PCM-style substring
pattern, but it will be massaged by TRANSFORM-PATTERN-FN, if that
is non-nil."
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (bounds (completion-boundaries beforepoint table pred afterpoint))
         (suffix (substring afterpoint (cdr bounds)))
         (prefix (substring beforepoint 0 (car bounds)))
         (basic-pattern (completion-basic--pattern
                         beforepoint afterpoint bounds))
         (pattern (if (not (stringp (car basic-pattern)))
                      basic-pattern
                    (cons 'prefix basic-pattern)))
         (pattern (if transform-pattern-fn
                       (funcall transform-pattern-fn pattern)
                     pattern))
         (all (completion-pcm--all-completions prefix pattern table pred)))
    (list all pattern prefix suffix (car bounds))))

(defun helm-completion-in-region--selection ()
  (with-helm-buffer
    (setq helm-saved-selection (helm-get-selection nil 'withprop))))

;; Completion-in-region-function

(defvar helm--completing-region nil
  "[INTERNAL] flag let-bounded to nil when completing in region.")

(defun helm--completion-in-region (origfun start end collection &optional predicate)
  "Helm replacement of `completion--in-region'.

Can be used for `completion-in-region-function' by advicing it with an
:around advice to allow passing the old
`completion-in-region-function' value in ORIGFUN."
  (cl-declare (special require-match prompt))
  (if (memq major-mode helm-mode-no-completion-in-region-in-modes)
      (funcall origfun start end collection predicate)
    (advice-add
     'lisp--local-variables
     :around #'helm-mode--advice-lisp--local-variables)
    (let ((old--helm-completion-style helm-completion-style)
          (exit-fun (plist-get completion-extra-properties :exit-function))
          ;; Always start with prefix to allow completing without
          ;; the need of inserting a space after cursor or
          ;; relaying on crap old completion-styles emacs22 which
          ;; add suffix after prefix. e.g. def|else.
          (initial-input (buffer-substring-no-properties start (point)))
          string)
      (helm-aif (cdr (assq major-mode helm-completion-styles-alist))
          (customize-set-variable 'helm-completion-style
                                  (if (cdr-safe it) (car it) it)))
      ;; This hook force usage of the display part of candidate with
      ;; its properties, this is needed for lsp-mode in its
      ;; :exit-function see Bug#2265.
      (add-hook 'helm-before-action-hook 'helm-completion-in-region--selection)
      (unwind-protect
          (let* ((enable-recursive-minibuffers t)
                 (completion-flex-nospace t)
                 (helm--completing-region t)
                 (completion-styles (helm--prepare-completion-styles))
                 (input (buffer-substring-no-properties start end))
                 (prefix (and (eq helm-completion-style 'emacs) initial-input))
                 (point (point))
                 (current-command (or (helm-this-command)
                                      this-command
                                      ;; Some backends are async and
                                      ;; use a callback, in those
                                      ;; cases, we can't retrieve from
                                      ;; frames the last interactive
                                      ;; command, so fallback to
                                      ;; `last-command' which may be
                                      ;; the one that called the callback.
                                      last-command))
                 (crm (eq current-command 'crm-complete))
                 (str-command (helm-symbol-name current-command))
                 (buf-name (format "*helm-mode-%s*" str-command))
                 (require-match (or (and (boundp 'require-match) require-match)
                                    minibuffer-completion-confirm
                                    ;; If prompt have not been propagated here, that's
                                    ;; probably mean we have no prompt and we are in
                                    ;; completion-at-point or friend, so use a non--nil
                                    ;; value for require-match.
                                    (not (boundp 'prompt))))
                 (metadata (completion-metadata input collection predicate))
                 ;; `completion-extra-properties' is let-bounded in `completion-at-point'.
                 ;; `afun' is a closure to call against each string in `data'.
                 ;; it provide the annotation info for each string.
                 ;; e.g "foo" => "foo <f>" where foo is a function.
                 ;; See Bug#407.
                 (afun (or (plist-get completion-extra-properties :annotation-function)
                           (completion-metadata-get metadata 'annotation-function)))
                 ;; Not sure if affixations are provided in
                 ;; completion-in-region, try anyway never know.
                 (afix (or (plist-get completion-extra-properties :affixation-function)
                           (completion-metadata-get metadata 'affixation-function)))
                 (init-space-suffix (unless (or (memq helm-completion-style '(helm-fuzzy emacs))
                                                (string-suffix-p " " input)
                                                (string= input ""))
                                      " "))
                 (category (or (eq (completion-metadata-get metadata 'category) 'file)
                               (eq (plist-get completion-extra-properties :category) 'file)))
                 (file-comp-p (or (eq category 'file)
                                  (helm-guess-filename-at-point)))
                 ;; `completion-all-completions' store the base-size in the last `cdr',
                 ;; so data looks like this: '(a b c d . 0) and (last data) == (d . 0).
                 base-size
                 (compfn (lambda (str _predicate _action)
                           (let* ((completion-ignore-case (helm-set-case-fold-search))
                                  (comps
                                   (completion-all-completions
                                    str ; This is helm-pattern
                                    collection
                                    predicate
                                    ;; Use prefix length at first call to
                                    ;; allow styles matching
                                    ;; "prefix*suffix" to kick in.
                                    (length (or prefix str))
                                    metadata))
                                  (last-data (last comps))
                                  (bs (helm-aif (cdr last-data)
                                          (prog1 it
                                            ;; Remove the last element of
                                            ;; comps by side-effect.
                                            (setcdr last-data nil))
                                        0))
                                  ;; Helm syle sort fn is added to
                                  ;; metadata only in emacs-27, so in
                                  ;; emacs-26 use helm-generic-sort-fn
                                  ;; which handle both helm and
                                  ;; helm-flex styles. When
                                  ;; helm-completion-style is helm or
                                  ;; helm-fuzzy, sorting will be done
                                  ;; later in FCT.
                                  (sort-fn
                                   (and (eq helm-completion-style 'emacs)
                                        (or
                                         ;; Emacs-27
                                         (completion-metadata-get
                                          metadata 'display-sort-function)
                                         ;; Emacs-26
                                         (lambda (candidates)
                                           (sort candidates #'helm-generic-sort-fn)))))
                                  all)
                             ;; Reset prefix to allow using length of
                             ;; helm-pattern on next calls (this avoid
                             ;; args-out-of-range error).
                             (and prefix (setq prefix nil))
                             ;; base-size needs to be set only once at
                             ;; first call.
                             (unless base-size (setq base-size bs))
                             (setq helm-completion--sorting-done (and sort-fn t))
                             (setq all (copy-sequence comps))
                             (helm-completion--initial-filter
                              (if (and sort-fn (> (length str) 0))
                                  (funcall sort-fn all)
                                all)
                              afun afix category))))
                 (data (if (memq helm-completion-style '(helm helm-fuzzy))
                           (funcall compfn input nil nil)
                         compfn))
                 (result (if (stringp data)
                             data
                           (helm-comp-read
                            ;; Completion-at-point and friends have no prompt.
                            (or (and (boundp 'prompt) prompt) "Pattern: ")
                            data
                            :name str-command
                            :nomark (null crm)
                            :marked-candidates crm
                            :initial-input
                            (cond ((and file-comp-p
                                        (not (string-match "/\\'" initial-input)))
                                   (concat (helm-mode--completion-in-region-initial-input
                                            (if (memq helm-completion-style '(helm helm-fuzzy))
                                                (helm-basename initial-input)
                                              initial-input))
                                           init-space-suffix))
                                  ((string-match "/\\'" initial-input)
                                   (and (eq helm-completion-style 'emacs) initial-input))
                                  ((or (null require-match)
                                       (stringp require-match))
                                   (helm-mode--completion-in-region-initial-input initial-input))
                                  (t (concat (helm-mode--completion-in-region-initial-input initial-input)
                                             init-space-suffix)))
                            :buffer buf-name
                            :fc-transformer
                            ;; Ensure sort fn is at the end.
                            (append '(helm-cr-default-transformer)
                                    (and helm-completion-in-region-default-sort-fn
                                         (list helm-completion-in-region-default-sort-fn)))
                            :match-dynamic (eq helm-completion-style 'emacs)
                            :fuzzy (eq helm-completion-style 'helm-fuzzy)
                            :exec-when-only-one t
                            :keymap helm-comp-in-region-map
                            :quit-when-no-cand
                            (lambda ()
                              ;; Delay message to overwrite "Quit".
                              (run-with-timer
                               0.01 nil
                               (lambda ()
                                 (message "[No matches]")))
                              t)        ; exit minibuffer immediately.
                            :must-match require-match))))
            ;; `helm-completion-in-region--insert-result' is stripping
            ;; out properties on RESULT by side-effect (perhaps
            ;; `choose-completion-string'?) so make a copy of STRING
            ;; to not loose props.
            (setq string (copy-sequence result))
            (helm-completion-in-region--insert-result
             result start point end base-size))
        ;; Allow running extra property `:exit-function' (Bug#2265,
        ;; Bug#2356). Function is called with 'exact if for a unique
        ;; match which is exact, the return value of `try-completion'
        ;; is t or a string ending with "/" i.e. possibly a directory
        ;; (Bug#2274),
        ;; otherwise it is called with 'finished.
        (when (and (stringp string) exit-fun)
          (let ((tcomp (try-completion initial-input collection)))
            (funcall exit-fun string
                     (if (or (eq tcomp t) ; Unique.
                             (and (stringp tcomp)
                                  (string-match "/\\'" tcomp))) ; A directory.
                         'exact 'finished))))
        (remove-hook 'helm-before-action-hook 'helm-completion-in-region--selection)
        (customize-set-variable 'helm-completion-style old--helm-completion-style)
        (setq helm-completion--sorting-done nil)
        (advice-remove 'lisp--local-variables
                       #'helm-mode--advice-lisp--local-variables)))))

(defvar helm-crm-default-separator ","
  "Default separator for `completing-read-multiple'.

`crm-separator' will take precedence on this when it is a string composed
of a single character.
If used globally, it is a string composed of a single character,
if let-bounded, it can be also nil or a symbol which mean no
separator.  Don't set this to a string composed of more than one
character.
Be sure to know what you are doing when modifying this.")
(defun helm-completion-in-region--insert-result (result start point end base-size)
  (cond ((stringp result)
         ;; When RESULT have annotation, annotation is displayed
         ;; in it with a display property attached to a space
         ;; added at end of string, take care of removing this
         ;; space (Bug#2360). However keep RESULT intact to
         ;; pass it to `:exit-function' i.e. Don't store the
         ;; modified string in STRING.
         (choose-completion-string
          (replace-regexp-in-string " \\'" "" result)
          (current-buffer)
          (list (+ start base-size) point)
          completion-list-insert-choice-function)
         (when helm-completion-mark-suffix
           (run-with-idle-timer 0.01 nil
                                (lambda ()
                                  (helm-aand
                                   (+ (- (point) point) end)
                                   (and (> it (point)) it)
                                   (push-mark  it t t))))))
        ((consp result)                 ; crm.
         (let ((beg (+ start base-size))
               (sep (or (and
                         ;; If `crm-separator' is a string of length 1
                         ;; assume it can be used as separator (Bug#2298),
                         ;; otherwise it is a regexp and use the value
                         ;; it matches or default to "," if no match.
                         (eq (length crm-separator) 1)
                         crm-separator)
                        helm-crm-default-separator)))
           ;; Try to find a default separator. If `crm-separator' is a
           ;; regexp use the string the regexp is matching.
           ;; If SEP is not a string, it have been probably bound to a
           ;; symbol or nil through `helm-crm-default-separator' that serve
           ;; as a flag to say "Please no separator" (Bug#2353 with
           ;; `magit-completing-read-multiple').
           (if (stringp sep)
               (save-excursion
                 (goto-char beg)
                 (when (looking-back crm-separator (1- (point)))
                   (setq sep (match-string 0))))
             (setq sep nil))
           (funcall completion-list-insert-choice-function
                    beg end (mapconcat 'identity (append result '("")) sep))))
        (t nil)))

(defun helm-mode--disable-ido-maybe (&optional from-hook)
  (when (and (boundp 'ido-everywhere) ido-everywhere)
    (remove-function read-file-name-function #'ido-read-file-name)
    (remove-function read-buffer-function #'ido-read-buffer)
    (setq ido-everywhere nil)
    (if from-hook
        (user-error "Unable to turn on Ido-everywhere while Helm-mode is enabled")
      (user-error "Helm-mode enabled (Ido-everywhere is incompatible with Helm-mode, disabling it)"))))

(defun helm-mode--ido-everywhere-hook ()
  ;; Called only when user calls directly ido-everywhere
  ;; and helm-mode is enabled.
  (when helm-mode
    (helm-mode--disable-ido-maybe t)))

;;;###autoload
(define-minor-mode helm-mode
    "Toggle generic helm completion.

All functions in Emacs that use `completing-read',
`read-file-name', `completion-in-region' and friends will use helm
interface when this mode is turned on.

However you can modify this behavior for functions of your choice
with `helm-completing-read-handlers-alist'.

Called with a positive arg, turn on unconditionally, with a
negative arg turn off.
You can toggle it with M-x `helm-mode'.

About `ido-mode':
DO NOT enable `ido-everywhere' when using `helm-mode'.  Instead of
using `ido-mode', add the commands where you want to use ido to
`helm-completing-read-handlers-alist' with `ido' as value.

Note: This mode is incompatible with Emacs23."
  :group 'helm-mode
  :global t
  :lighter helm-completion-mode-string
  (cl-assert (boundp 'completing-read-function) nil
             "`helm-mode' not available, upgrade to Emacs-24")
  (if helm-mode
      (progn
        (add-function :override completing-read-function
                      #'helm--completing-read-default)
        (add-function :override read-file-name-function
                      #'helm--generic-read-file-name)
        (add-function :override read-buffer-function
                      #'helm--generic-read-buffer)
        (when helm-mode-handle-completion-in-region
          (add-function :around completion-in-region-function
                        #'helm--completion-in-region))
        ;; If user have enabled ido-everywhere BEFORE enabling
        ;; helm-mode disable it and warn user about its
        ;; incompatibility with helm-mode (Bug#2085).
        (helm-mode--disable-ido-maybe)
        ;; If ido-everywhere is not enabled yet anticipate and
        ;; disable it if user attempt to enable it while helm-mode
        ;; is running (Bug#2085).
        (add-hook 'ido-everywhere-hook #'helm-mode--ido-everywhere-hook)
        (when (fboundp 'ffap-read-file-or-url-internal)
          ;; `ffap-read-file-or-url-internal' have been removed in
          ;; emacs-27 and `ffap-read-file-or-url' is fixed, so no need
          ;; to advice it.
          (advice-add 'ffap-read-file-or-url :override #'helm-advice--ffap-read-file-or-url))
        (advice-add 'read-buffer-to-switch :override #'helm-mode--read-buffer-to-switch)
        (helm-minibuffer-history-mode 1))
    (progn
      (remove-function completing-read-function #'helm--completing-read-default)
      (remove-function read-file-name-function #'helm--generic-read-file-name)
      (remove-function read-buffer-function #'helm--generic-read-buffer)
      (remove-function completion-in-region-function #'helm--completion-in-region)
      (remove-hook 'ido-everywhere-hook #'helm-mode--ido-everywhere-hook)
      (when (fboundp 'ffap-read-file-or-url-internal)
        (advice-remove 'ffap-read-file-or-url #'helm-advice--ffap-read-file-or-url))
      (advice-remove 'read-buffer-to-switch #'helm-mode--read-buffer-to-switch)
      (helm-minibuffer-history-mode -1))))

(provide 'helm-mode)

;;; helm-mode.el ends here
