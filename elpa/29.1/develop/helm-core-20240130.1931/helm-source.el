;;; helm-source.el --- Helm source creation. -*- lexical-binding: t -*-

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

;; Interface to create helm sources easily.
;; Actually the eieo objects are transformed in alist for compatibility.
;; In the future this package should allow creating source as eieo objects
;; without conversion to alist, teaching helm to read such a structure.
;; The compatibility with alists would be kept.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'helm-lib)

(defvar helm-fuzzy-sort-fn)
(defvar helm-fuzzy-match-fn)
(defvar helm-fuzzy-search-fn)

(declare-function helm-init-candidates-in-buffer "helm-core.el")
(declare-function helm-interpret-value "helm-core.el")
(declare-function helm-fuzzy-highlight-matches "helm-core.el")
(declare-function helm-marked-candidates "helm-core.el")

;;; Advice Emacs fn
;;  Make Classes's docstrings more readable by removing the attempts to align
;;  unuseful stuff and add newline for separating slot documentation, as well
;;  slots are in bold characters.

(defun helm-source--cl--print-table (&rest args)
  "Advice for `cl--print-table' to make readable class slots docstrings."
  (let ((format "%s\n\n  Initform=%s\n\n%s"))
    (dolist (row (cadr args))
      (setcar row (propertize (car row) 'face 'bold))
      (setcdr row (nthcdr 1 (cdr row)))
      (insert "\n* " (apply #'format format row) "\n"))))

(cl-defgeneric helm--setup-source (source)
  "Prepare slots and handle slot errors before creating a helm source.")

(cl-defgeneric helm-setup-user-source (source)
  "Allow users modifying slots in SOURCE just before creation.")


;;; Classes for sources
;;
;;
(defclass helm-source ()
  ((name
    :initarg :name
    :initform nil
    :custom string
    :documentation
    "  The name of the source.
  A string which is also the heading which appears
  above the list of matches from the source. Must be unique.")

   (header-name
    :initarg :header-name
    :initform nil
    :custom function
    :documentation
    "  A function returning the display string of the header.
  Its argument is the name of the source. This attribute is useful to
  add an additional information with the source name.
  It doesn't modify the name of the source.")

   (init
    :initarg :init
    :initform nil
    :custom function
    :documentation
    "  Function called with no parameters when helm is started.
  It is useful for collecting current state information which can be
  used to create the list of candidates later.
  Initialization of `candidates-in-buffer' is done here
  with `helm-init-candidates-in-buffer'.")

   (candidates
    :initarg :candidates
    :initform nil
    :custom (choice function list)
    :documentation
    "  Specifies how to retrieve candidates from the source.
  It can either be a variable name, a function called with no parameters
  or the actual list of candidates.

  Do NOT use this for asynchronous sources, use `candidates-process'
  instead.

  The list must be a list whose members are strings, symbols
  or (DISPLAY . REAL) pairs.

  In case of (DISPLAY . REAL) pairs, the DISPLAY string is shown
  in the Helm buffer, but the REAL one is used as action
  argument when the candidate is selected. This allows a more
  readable presentation for candidates which would otherwise be,
  for example, too long or have a common part shared with other
  candidates which can be safely replaced with an abbreviated
  string for display purposes.

  Note that if the (DISPLAY . REAL) form is used then pattern
  matching is done on the displayed string, not on the real
  value.

  This function, generally should not compute candidates according to
  `helm-pattern' which defeat all the Helm's matching mechanism
  i.e. multiple pattern matching and/or fuzzy matching.
  If you want to do so, use :match-dynamic slot to be sure matching
  occur only in :candidates function and there is no conflict with
  other match functions.")

   (update
    :initarg :update
    :initform nil
    :custom function
    :documentation
    "  Function called with no parameters before :init function
  when `helm-force-update' is called.")

   (cleanup
    :initarg :cleanup
    :initform nil
    :custom function
    :documentation
    "  Function called with no parameters when *helm* buffer is
  closed. It is useful for killing unneeded candidates buffer.

  Note that the function is executed BEFORE performing action.")

   (keymap
    :initarg :keymap
    :initform 'helm-map
    :custom sexp
    :documentation
    "  Specific keymap for this source.
  default value is `helm-map'.")

   (action
    :initarg :action
    :initform 'identity
    :custom (alist :key-type string
                   :value-type function)
    :documentation
    "  An alist of (DISPLAY . FUNCTION) pairs, a variable name  or a function.
  FUNCTION is called with one parameter: the selected candidate.

  An action other than the default can be chosen from this list
  of actions for the currently selected candidate (by default
  with TAB). The DISPLAY string is shown in the completions
  buffer and the FUNCTION is invoked when an action is
  selected. The first action of the list is the default.

  You should use `helm-make-actions' to build this alist easily.")

   (persistent-action
    :initarg :persistent-action
    :initform nil
    :custom function
    :documentation
    "  Can be a either a Function called with one parameter (the
  selected candidate) or a cons cell where first element is this
  same function and second element a symbol (e.g never-split)
  that inform `helm-execute-persistent-action' to not split his
  window to execute this persistent action.
  Example:

    (defun foo-persistent-action (candidate)
       (do-something candidate))

    :persistent-action \\='(foo-persistent-action . never-split) ; Don't split
  or
    :persistent-action \\='foo-persistent-action ; Split

  When specifying :persistent-action by slot directly, foo-persistent-action
  will be executed without quitting helm when hitting `C-j'.

  Note that other persistent actions can be defined using other
  bindings than `C-j' by simply defining an interactive function bound
  to a key in the keymap source.
  The function should create a new attribute in source before calling
  `helm-execute-persistent-action' on this attribute.
  Example:

     (defun helm-ff-persistent-delete ()
       \"Delete current candidate without quitting.\"
       (interactive)
       (with-helm-alive-p
         (helm-set-attr \\='quick-delete \\='(helm-ff-quick-delete . never-split))
         (helm-execute-persistent-action \\='quick-delete)))

  This function is then bound in `helm-find-files-map'.")

   (persistent-action-if
    :initarg :persistent-action-if
    :initform nil
    :custom function
    :documentation
    "  Similar from persistent action but it is a function that should
  return an object suitable for persistent action when called , i.e. a
  function or a cons cell.
  Example:

     (defun foo-persistent-action (candidate)
       (cond (something
              ;; Don't split helm-window.
              (cons (lambda (_ignore)
                      (do-something candidate))
                    \\='no-split))
             ;; Split helm-window.
             (something-else
              (lambda (_ignore)
                (do-something-else candidate)))))

     :persistent-action-if \\='foo-persistent-action

  Here when hitting `C-j' one of the lambda's will be executed
  depending on something or something-else condition, splitting or not
  splitting as needed.
  See `helm-find-files-persistent-action-if' definition as another example.")

   (persistent-help
    :initarg :persistent-help
    :initform nil
    :custom string
    :documentation
    "  A string to explain persistent-action of this source.
  It is a facility to display what persistent action does in
  header-line, once your source is loaded don't use it directly, it will
  have no effect, use instead `header-line' attribute.
  It also accepts a function or a variable name.
  It will be displayed in `header-line' or in `minibuffer' depending
  of value of `helm-echo-input-in-header-line' and `helm-display-header-line'.")

   (help-message
    :initarg :help-message
    :initform nil
    :custom (choice string function)
    :documentation
    "  Help message for this source.
  If not present, `helm-help-message' value will be used.")

   (multiline
    :initarg :multiline
    :initform nil
    :custom (choice boolean integer)
    :documentation
    "  Allow multiline candidates.
  When non-nil candidates will be separated by `helm-candidate-separator'.
  You can customize the color of this separator with `helm-separator' face.
  Value of multiline can be an integer which specify the maximum size of the
  multiline string to display, if multiline string is longer than this value
  it will be truncated.")

   (requires-pattern
    :initarg :requires-pattern
    :initform 0
    :custom integer
    :documentation
    "  If present matches from the source are shown only if the
  pattern is not empty. Optionally, it can have an integer
  parameter specifying the required length of input which is
  useful in case of sources with lots of candidates.")

   (candidate-transformer
    :initarg :candidate-transformer
    :initform nil
    :custom (choice function list)
    :documentation
    "  It's a function or a list of functions called with one argument
  when the completion list from the source is built. The argument
  is the list of candidates retrieved from the source. The
  function should return a transformed list of candidates which
  will be used for the actual completion.  If it is a list of
  functions, it calls each function sequentially.

  This can be used to transform or remove items from the list of
  candidates.

  Note that `candidates' is run already, so the given transformer
  function should also be able to handle candidates with (DISPLAY
  . REAL) format.")

   (filtered-candidate-transformer
    :initarg :filtered-candidate-transformer
    :initform nil
    :custom (choice function list)
    :documentation
    "  It has the same format as `candidate-transformer', except the
  function is called with two parameters: the candidate list and
  the source.

  This transformer is run on the candidate list which is already
  filtered by the current pattern. While `candidate-transformer'
  is run only once, it is run every time the input pattern is
  changed.

  It can be used to transform the candidate list dynamically, for
  example, based on the current pattern.

  In some cases it may also be more efficent to perform candidate
  transformation here, instead of with `candidate-transformer'
  even if this transformation is done every time the pattern is
  changed.  For example, if a candidate set is very large then
  `candidate-transformer' transforms every candidate while only
  some of them will actually be displayed due to the limit
  imposed by `helm-candidate-number-limit'.

  Note that `candidates' and `candidate-transformer' is run
  already, so the given transformer function should also be able
  to handle candidates with (DISPLAY . REAL) format.")

   (filter-one-by-one
    :initarg :filter-one-by-one
    :initform nil
    :custom (choice function list)
    :documentation
    "  A transformer function that treat candidates one by one.
  It is called with one arg the candidate.
  It is faster than `filtered-candidate-transformer' or
  `candidate-transformer', but should be used only in sources
  that recompute constantly their candidates, e.g `helm-source-find-files'.
  Filtering happen early and candidates are treated
  one by one instead of re-looping on the whole list.
  If used with `filtered-candidate-transformer' or `candidate-transformer'
  these functions should treat the candidates transformed by the
  `filter-one-by-one' function in consequence.")

   (display-to-real
    :initarg :display-to-real
    :initform nil
    :custom function
    :documentation
    "  Transform the selected candidate when passing it to action.

  Function called with one parameter, the selected candidate.

  Avoid recomputing all candidates with candidate-transformer
  or filtered-candidate-transformer to give a new value to REAL,
  instead the selected candidate is transformed only when passing it
  to action. This works (and make sense) only with plain string
  candidates, it will NOT work when candidate is a cons cell, in this
  case the real value of candidate will be used.
  Example:

    (helm :sources (helm-build-sync-source \"test\"
                 :candidates \\='(a b c d e)
                 :display-to-real (lambda (c) (concat c \":modified by d-t-r\")))
      :buffer \"*helm test*\")

  Note that this is NOT a transformer,
  so the display will not be modified by this function.")

   (real-to-display
    :initarg :real-to-display
    :initform nil
    :custom function
    :documentation
    "  Recompute all candidates computed previously with other transformers.

  Function called with one parameter, the selected candidate.

  The real value of candidates will be shown in display and of course
  be used by action.
  Example:

    (helm :sources (helm-build-sync-source \"test\"
                 :candidates \\='((\"foo\" . 1) (\"bar\" . 2) (\"baz\". 3))
                 :real-to-display (lambda (c) (format \"%s\" (1+ c))))
      :buffer \"*helm test*\")

  Mostly deprecated, kept only for backward compatibility.")

   (marked-with-props
    :initarg :marked-with-props
    :initform nil
    :custom (choice boolean symbol)
    :documentation
    "  Get candidates with their properties in `helm-marked-candidates'.
  Allow using the FORCE-DISPLAY-PART of `helm-get-selection' in marked
  candidates, use t or \\='withprop to pass it to `helm-get-selection'.")

   (action-transformer
    :initarg :action-transformer
    :initform nil
    :custom (choice function list)
    :documentation
    "  It's a function or a list of functions called with two
  arguments when the action list from the source is
  assembled. The first argument is the list of actions, the
  second is the current selection.  If it is a list of functions,
  it calls each function sequentially.

  The function should return a transformed action list.

  This can be used to customize the list of actions based on the
  currently selected candidate.")

   (pattern-transformer
    :initarg :pattern-transformer
    :initform nil
    :custom (choice function list)
    :documentation
    "  It's a function or a list of functions called with one argument
  before computing matches. Its argument is `helm-pattern'.
  Functions should return transformed `helm-pattern'.

  It is useful to change interpretation of `helm-pattern'.")

   (candidate-number-limit
    :initarg :candidate-number-limit
    :initform nil
    :custom integer
    :documentation
    "  Override `helm-candidate-number-limit' only for this source.")

   (volatile
    :initarg :volatile
    :initform nil
    :custom boolean
    :documentation
    "  Indicates the source assembles the candidate list dynamically,
  so it shouldn't be cached within a single Helm
  invocation. It is only applicable to synchronous sources,
  because asynchronous sources are not cached.")

   (match
    :initarg :match
    :initform nil
    :custom (choice function list)
    :documentation
    "  List of functions called with one parameter: a candidate. The
  function should return non-nil if the candidate matches the
  current pattern (see variable `helm-pattern').

  When using `candidates-in-buffer' its default value is `identity' and
  don't have to be changed, use the `search' slot instead.

  This attribute allows the source to override the default
  pattern matching based on `string-match'. It can be used, for
  example, to implement a source for file names and do the
  pattern matching on the basename of files, since it's more
  likely one is typing part of the basename when searching for a
  file, instead of some string anywhere else in its path.

  If the list contains more than one function then the list of
  matching candidates from the source is constructed by appending
  the results after invoking the first function on all the
  potential candidates, then the next function, and so on. The
  matching candidates supplied by the first function appear first
  in the list of results and then results from the other
  functions, respectively.

  This attribute has no effect for asynchronous sources (see
  attribute `candidates'), and sources using `match-dynamic'
  since they perform pattern matching themselves.

  Note that FUZZY-MATCH slot will overhide value of this slot.")

   (diacritics
    :initarg :diacritics
    :initform nil
    :custom boolean
    :documentation
    "  Ignore diacritics when searching.")

   (match-on-real
    :initarg :match-on-real
    :initform nil
    :custom boolean
    :documentation
    "  Match the real value of candidates when non nil.")

   (fuzzy-match
    :initarg :fuzzy-match
    :initform nil
    :custom boolean
    :documentation
    "  Enable fuzzy matching in this source.
  This will overwrite settings in MATCH slot, and for
  sources built with child class `helm-source-in-buffer' the SEARCH slot.
  This also add a `filtered-candidate-transformer' function to sort candidates
  (see `helm-fuzzy-sort-fn') according to the score of each candidate which is
  computed with `helm-fuzzy-default-score-fn'. 
  This is an easy way of enabling fuzzy matching, but you can use the MATCH
  or SEARCH slots yourself if you want something more elaborated, mixing
  different type of match (See `helm-source-buffers' class for example), you
  will have in this case to provide as well a sort fn
  in `filtered-candidate-transformer' yourself.

  This attribute is not supported for asynchronous sources
  since they perform pattern matching themselves.")

   (redisplay
    :initarg :redisplay
    :initform 'identity
    :custom (choice list function)
    :documentation
    "  A function or a list of functions to apply to current list
  of candidates when redisplaying buffer with `helm-redisplay-buffer'.
  This is only interesting for modifying and redisplaying the whole list
  of candidates in async sources.
  It uses `identity' by default for when async sources are mixed with
  normal sources, in this case these normal sources are not modified and
  redisplayed as they are.")

   (nomark
    :initarg :nomark
    :initform nil
    :custom boolean
    :documentation
    "  Don't allow marking candidates when this attribute is present.")

   (nohighlight
    :initarg :nohighlight
    :initform nil
    :custom boolean
    :documentation
    "  Disable highlighting matches in this source.
  This will disable generic highlighting of matches,
  but some specialized highlighting can be done from elsewhere,
  i.e from `filtered-candidate-transformer' or `filter-one-by-one' slots.
  So use this to either disable completely highlighting in your source,
  or to disable highlighting and use a specialized highlighting matches
  function for this source.
  Remember that this function should run AFTER all filter functions if those
  filter functions are modifying face properties, though it is possible to
  avoid this by using new `add-face-text-property' in your filter functions.")

   (allow-dups
    :initarg :allow-dups
    :initform nil
    :custom boolean
    :documentation
    "  Allow helm collecting duplicates candidates.")

   (history
    :initarg :history
    :initform nil
    :custom symbol
    :documentation
    "  Allow passing history variable to helm from source.
  It should be a quoted symbol.
  Passing the history variable here have no effect
  so add it also in the `helm' call with the :history keyword.
  The main point of adding the variable here
  is to make it available when resuming.")

   (coerce
    :initarg :coerce
    :initform nil
    :custom function
    :documentation
    "  It's a function called with one argument: the selected candidate.
  This function is intended for type convertion. In normal case,
  the selected candidate (string) is passed to action
  function. If coerce function is specified, it is called just
  before action function.

  Example: converting string to symbol
    (coerce . intern)")

   (mode-line
    :initarg :mode-line
    :initform nil
    :custom (choice string sexp)
    :documentation
    "  Source local `helm-mode-line-string' (included in
  `mode-line-format'). It accepts also variable/function name.")

   (header-line
    :initarg :header-line
    :initform nil
    :custom (choice string function)
    :documentation
    "  Source local `header-line-format'.
  It will be displayed in `header-line' or in `minibuffer' depending
  of value of `helm-echo-input-in-header-line' and `helm-display-header-line'.
  It accepts also variable/function name.")

   (resume
    :initarg :resume
    :initform nil
    :custom function
    :documentation
    "  Function called with no parameters at end of initialization
  when `helm-resume' is started.
  If this function try to do something against `helm-buffer', (e.g updating,
  searching etc...) probably you should run it in a timer to ensure
  `helm-buffer' is ready.")

   (follow
    :initarg :follow
    :initform nil
    :custom integer
    :documentation
    "  Enable `helm-follow-mode' for this source only.
  With a value of 1 enable, a value of -1 or nil disable the mode.
  See `helm-follow-mode' for more infos.")

   (follow-delay
    :initarg :follow-delay
    :initform nil
    :custom integer
    :documentation
    "  `helm-follow-mode' will execute persistent-action after this delay.
  Otherwise value of `helm-follow-input-idle-delay' is used if non--nil,
  If none of these are found fallback to `helm-input-idle-delay'.")

   (multimatch
    :initarg :multimatch
    :initform t
    :custom boolean
    :documentation
    "  Use the multi-match algorithm when non-nil.
  I.e Allow specifying multiple patterns separated by spaces.
  When a pattern is prefixed by \"!\" the negation of this pattern is used,
  i.e match anything but this pattern.
  It is the standard way of matching in helm and is enabled by default.
  It can be used with fuzzy-matching enabled, but as soon helm detect a space,
  each pattern will match by regexp and will not be fuzzy.")

   (match-part
    :initarg :match-part
    :initform nil
    :custom function
    :documentation
    "  Allow matching only one part of candidate.
  If source contain match-part attribute, match is computed only
  on part of candidate returned by the call of function provided
  by this attribute. The function should have one arg, candidate,
  and return only a specific part of candidate.
  On async sources, as matching is done by the backend, this have
  no effect apart for highlighting matches.")

   (before-init-hook
    :initarg :before-init-hook
    :initform nil
    :custom symbol
    :documentation
    "  A local hook that run at beginning of initilization of this source.
  i.e Before the creation of `helm-buffer'.

  Should be a variable (a symbol) bound to a list of
  functions or a single function (see `run-hooks' documentation).
  Even better is to use `add-hook' to feed this variable.
  Usage of an anonymous function, or a list of functions is still
  supported but not recommended.")

   (after-init-hook
    :initarg :after-init-hook
    :initform nil
    :custom symbol
    :documentation
    "  A local hook that run at end of initilization of this source.
  i.e After the creation of `helm-buffer'.
  
  Should be a variable (a symbol) bound to a list of
  functions or a single function (see `run-hooks' documentation).
  Even better is to use `add-hook' to feed this variable.
  Usage of an anonymous function, or a list of functions is still
  supported but not recommended.")

   (delayed
    :initarg :delayed
    :initform nil
    :custom (choice null integer)
    :documentation
    "  This slot have no more effect and is just kept for backward compatibility.
  Please don't use it.")

   (must-match
    :initarg :must-match
    :initform nil
    :custom symbol
    :documentation
    "  Same as `completing-read' require-match arg.
  Possible values are:
  - `t' which prevent exiting with an empty helm-buffer i.e. no matches.
  - `confirm' which ask for confirmation i.e. need to press a second
     time RET.
  - `nil' is the default and is doing nothing i.e. returns nil when
    pressing RET with an empty helm-buffer.
  - Any other non nil values e.g. `ignore' allow exiting with
    minibuffer contents as candidate value (in this case helm-buffer
    is empty).")

   (find-file-target
    :initarg :find-file-target
    :initform nil
    :custom function
    :documentation
    "  Determine the target file when running `helm-quit-and-find-file'.
  It is a function called with one arg SOURCE.")

   (group
    :initarg :group
    :initform 'helm
    :custom symbol
    :documentation
    "  The current source group, default to `helm' when not specified."))

  "Main interface to define helm sources."
  :abstract t)

(defclass helm-source-sync (helm-source)
  ((candidates
    :initform '("ERROR: You must specify the `candidates' slot, either with a list or a function"))

   (migemo
    :initarg :migemo
    :initform nil
    :custom boolean
    :documentation
    "  Enable migemo.
  When multimatch is disabled, you can give the symbol \\='nomultimatch as value
  to force not using generic migemo matching function.
  In this case you have to provide your own migemo matching funtion
  that kick in when `helm-migemo-mode' is enabled.
  Otherwise it will be available for this source once `helm-migemo-mode'
  is enabled when non-nil.")

   (match-strict
    :initarg :match-strict
    :initform nil
    :custom function
    :documentation
    "  When specifying a match function within a source and
  helm-multi-match is enabled, the result of all matching
  functions will be concatened, which in some cases is not what
  is wanted. When using `match-strict' only this or these
  functions will be used. You can specify those functions as a
  list of functions or a single symbol function.

  NOTE: This have the same effect as using :MULTIMATCH nil.")

   (match-dynamic
    :initarg :match-dynamic
    :initform nil
    :custom boolean
    :documentation
    "  Disable all helm matching functions when non nil.
  The :candidates function in this case is in charge of fetching
  candidates dynamically according to `helm-pattern'.
  If you want to make your :candidates function working with `completion-styles'
  use the function `helm-dynamic-completion'. 
  Note that :volatile is automatically enabled when using this, so no
  need to specify it."))

  "Use this class to make helm sources using a list of candidates.
This list should be given as a normal list, a variable handling a list
or a function returning a list.
Matching is done basically with `string-match' against each candidate.")

(defclass helm-source-async (helm-source)
  ((candidates-process
    :initarg :candidates-process
    :initform nil
    :custom function
    :documentation
    "  This attribute is used to define a process as candidate.
  The function called with no arguments must return a process
  i.e. `processp', it use typically `start-process' or `make-process',
  see (info \"(elisp) Asynchronous Processes\").
  

  NOTE:
  When building the source at runtime you can give directly a process
  as value, otherwise wrap the process call into a function.
  The process buffer should be nil, otherwise, if you use
  `helm-buffer' give to the process a sentinel.")

   (multimatch :initform nil))

  "Use this class to define a helm source calling an external process.
The external process is called typically in a `start-process' call to be
asynchronous.

Note that using multiples asynchronous sources is not fully working,
expect weird behavior if you try this.

The :candidates slot is not allowed even if described because this class
inherit from `helm-source'.")

(defclass helm-source-in-buffer (helm-source)
  ((init
    :initform 'helm-default-init-source-in-buffer-function)

   (data
    :initarg :data
    :initform nil
    :custom (choice list string)
    :documentation
    "  A string, a list or a buffer that will be used to feed the `helm-candidates-buffer'.
  This data will be passed in a function added to the init slot and
  the buffer will be build with `helm-init-candidates-in-buffer' or directly
  with `helm-candidates-buffer' if data is a buffer.
  This is an easy and fast method to build a `candidates-in-buffer' source.")

   (migemo
    :initarg :migemo
    :initform nil
    :custom boolean
    :documentation
    "  Enable migemo.
  When multimatch is disabled, you can give the symbol \\='nomultimatch as value
  to force not using generic migemo matching function.
  In this case you have to provide your own migemo matching funtion
  that kick in when `helm-migemo-mode' is enabled.
  Otherwise it will be available for this source once `helm-migemo-mode'
  is enabled when non-nil.")

   (candidates
    :initform 'helm-candidates-in-buffer)

   (volatile
    :initform t)

   (match
    :initform '(identity))
   
   (get-line
    :initarg :get-line
    :initform 'buffer-substring-no-properties
    :custom function
    :documentation
    "  A function like `buffer-substring-no-properties' or `buffer-substring'.
  This function converts region from point at line-beginning and point
  at line-end in the `helm-candidate-buffer' to a string which will be displayed
  in the `helm-buffer', it takes two args BEG and END.
  By default, `helm-candidates-in-buffer' uses
  `buffer-substring-no-properties' which does no conversion and doesn't carry
  text properties.")

   (search
    :initarg :search
    :initform '(helm-candidates-in-buffer-search-default-fn)
    :custom (choice function list)
    :documentation
    "  List of functions like `re-search-forward' or `search-forward'.
  Buffer search function used by `helm-candidates-in-buffer'.
  By default, `helm-candidates-in-buffer' uses `re-search-forward'.
  The function should take one arg PATTERN.
  If your search function needs to handle negation like multimatch,
  this function should returns in such case a cons cell of two integers defining
  the beg and end positions to match in the line previously matched by
  `re-search-forward' or similar, and move point to next line
  (See how the `helm-mm-3-search-base' and `helm-fuzzy-search' functions are working).

  NOTE: FUZZY-MATCH slot will overhide value of this slot.")

   (search-strict
    :initarg :search-strict
    :initform nil
    :custom function
    :documentation
    "  When specifying a search function within a source and
  helm-multi-match is enabled, the result of all searching
  functions will be concatened, which in some cases is not what
  is wanted. When using `search-strict' only this or these
  functions will be used. You can specify those functions as a
  list of functions or a single symbol function.

  NOTE: This have the same effect as using a nil value for
        :MULTIMATCH slot."))

  "Use this source to make helm sources storing candidates inside a buffer.

The buffer storing candidates is generated by `helm-candidate-buffer' function
and all search are done in this buffer, results are transfered to the `helm-buffer'
when done.
Contrarily to `helm-source-sync' candidates are matched using a function
like `re-search-forward' (see below documentation of `:search' slot) which makes
the search much faster than matching candidates one by one.
If you want to add search functions to your sources, don't use `:match' which
will raise an error, but `:search'.
See `helm-candidates-in-buffer' for more infos.")

(defclass helm-source-dummy (helm-source)
  ((candidates
    :initform '("dummy"))

   (filtered-candidate-transformer
    :initform (lambda (_candidates _source) (list helm-pattern)))

   (multimatch
    :initform nil)

   (accept-empty
    :initarg :accept-empty
    :initform t
    :custom boolean
    :documentation
    "  Allow exiting with an empty string.
  You should keep the default value.")

   (match
    :initform 'identity)

   (volatile
    :initform t)))

(defclass helm-source-in-file (helm-source-in-buffer)
  ((init :initform (lambda ()
                     (let ((file (helm-get-attr 'candidates-file))
                           (count 1))
                       (with-current-buffer (helm-candidate-buffer 'global)
                         (insert-file-contents file)
                         (goto-char (point-min))
                         (when (helm-get-attr 'linum)
                           (while (not (eobp))
                             (add-text-properties
                              (pos-bol) (pos-eol)
                              `(helm-linum ,count))
                             (cl-incf count)
                             (forward-line 1)))))))
   (get-line :initform #'buffer-substring)
   (candidates-file
    :initarg :candidates-file
    :initform nil
    :custom string
    :documentation
    "  The file used to fetch candidates.")
   (linum
    :initarg :linum
    :initform nil
    :documentation
    "  Store line number in each candidate when non nil.
  Line number is stored in `helm-linum' text property."))

  "The contents of the FILE will be used as candidates in buffer.")


;;; Error functions
;;
;;
(defun helm-default-init-source-in-buffer-function ()
  (helm-init-candidates-in-buffer 'global
    '("ERROR: No buffer handling your data, use either the `init' slot or the `data' slot.")))


;;; Internal Builder functions.
;;
;;
(defun helm--create-source (object)
  "[INTERNAL] Build a helm source from OBJECT.
Where OBJECT is an instance of an eieio class."
  (cl-loop for sd in (eieio-class-slots (eieio-object-class object))
           for s = (eieio-slot-descriptor-name sd)
           for slot-val = (slot-value object s)
           when slot-val
           collect (cons s slot-val)))

(defun helm-make-source (name class &rest args)
  "Build a `helm' source named NAME with ARGS for CLASS.
Argument NAME is a string which define the source name, so no need to use
the keyword :name in your source, NAME will be used instead.
Argument CLASS is a symbol defining an eieio class object.
Arguments ARGS are keyword value pairs as defined in CLASS."
  (declare (indent 2))
  (let ((source (apply #'make-instance class name args)))
    (setf (slot-value source 'name) name)
    (helm--setup-source source)
    (helm-setup-user-source source)
    (helm--create-source source)))

(defun helm-make-type (class &rest args)
  (let ((source (apply #'make-instance class args)))
    (setf (slot-value source 'name) nil)
    (helm--setup-source source)
    (helm--create-source source)))

(defvar helm-mm-default-search-functions)
(defvar helm-mm-default-match-functions)

(defun helm-source-mm-get-search-or-match-fns (source method)
  "Prepare match or search functions for class SOURCE.
Argument METHOD is the matching method used by SOURCE either `match'
or `search'."
  (let* ((diacritics       (slot-value source 'diacritics))
         (defmatch         (helm-aif (slot-value source 'match)
                               (helm-mklist it)))
         (defmatch-strict  (helm-aif (and (eq method 'match)
                                          (slot-value source 'match-strict))
                               (helm-mklist it)))
         (defsearch        (helm-aif (and (eq method 'search)
                                          (slot-value source 'search))
                               (helm-mklist it)))
         (defsearch-strict (helm-aif (and (eq method 'search-strict)
                                          (slot-value source 'search-strict))
                               (helm-mklist it)))
         (migemo           (slot-value source 'migemo)))
    (cl-case method
      (match (cond (defmatch-strict)
                   ((and migemo diacritics)
                    (append (list 'helm-mm-exact-match
                                  'helm-mm-3-match-on-diacritics)
                            defmatch '(helm-mm-3-migemo-match)))
                   (migemo
                    (append helm-mm-default-match-functions
                            defmatch '(helm-mm-3-migemo-match)))
                   (diacritics
                    (delq nil
                          `(helm-mm-exact-match
                            ,@defmatch helm-mm-3-match-on-diacritics)))
                   (defmatch
                    (append helm-mm-default-match-functions defmatch))
                   (t helm-mm-default-match-functions)))
      (search (cond (defsearch-strict)
                    ((and migemo diacritics)
                     (append '(helm-mm-exact-search)
                             defsearch
                             '(helm-mm-3-migemo-search
                               helm-mm-3-search-on-diacritics)))
                    (migemo
                     (append helm-mm-default-search-functions
                             defsearch '(helm-mm-3-migemo-search)))
                    (diacritics
                     (delq nil
                           `(helm-mm-exact-search
                             ,@defsearch helm-mm-3-search-on-diacritics)))
                    (defsearch
                     (append helm-mm-default-search-functions defsearch))
                    (t helm-mm-default-search-functions))))))


;;; Modifiers
;;
(cl-defun helm-source-add-action-to-source-if (name fn source predicate
                                                    &optional (index 4))
  "Same as `helm-add-action-to-source-if' but for SOURCE defined as eieio object.
You can use this inside a `helm--setup-source' method for a SOURCE defined as
an eieio class."
  (let* ((actions     (slot-value source 'action))
         (action-transformers (slot-value source 'action-transformer))
         (new-action  (list (cons name fn)))
         (transformer (lambda (actions _candidate)
                        (let ((candidate (car (helm-marked-candidates))))
                          (cond ((funcall predicate candidate)
                                 (helm-append-at-nth
                                  actions new-action index))
                                (t actions))))))
    (cond ((functionp actions)
           (setf (slot-value source 'action) (list (cons "Default action" actions))))
          ((listp actions)
           (setf (slot-value source 'action) (helm-interpret-value actions source))))
    (when (or (symbolp action-transformers) (functionp action-transformers))
      (setq action-transformers (list action-transformers)))
    (setf (slot-value source 'action-transformer)
          (delq nil (append (list transformer) action-transformers)))))


;;; Methods to build sources.
;;
;;
(defun helm-source--persistent-help-string (value source)
  "Format `persistent-help' VALUE in SOURCE.
Argument VALUE can be a string, a variable or a function."
  (substitute-command-keys
   (format "\\<helm-map>\\[helm-execute-persistent-action]: %s (keeping session)"
           (helm-aif value
               (helm-interpret-value value source)
             (slot-value source 'header-line)))))

(defun helm-source--header-line (source)
  "Compute a default header line for SOURCE.

The header line is based on one of `persistent-action-if',
`persistent-action', or `action' (in this order of precedence)."
  (substitute-command-keys
   (concat "\\<helm-map>\\[helm-execute-persistent-action]: "
           (helm-acond
            ((slot-value source 'persistent-action-if)
             (helm-symbol-name it))
            ((or (slot-value source 'persistent-action)
                 (slot-value source 'action))
             (cond ((and (symbolp it)
                         (functionp it)
                         (eq it 'identity))
                    "Do Nothing")
                   ((and (symbolp it)
                         (boundp it)
                         (listp (symbol-value it))
                         (stringp (caar (symbol-value it))))
                    (caar (symbol-value it)))
                   ((or (symbolp it) (functionp it))
                    (helm-symbol-name it))
                   ((listp it)
                    (let ((action (car it)))
                      ;; It comes from :action ("foo" . function).
                      (if (stringp (car action))
                          (car action)
                        ;; It comes from :persistent-action
                        ;; (function . 'nosplit) Fix Bug#788.
                        (if (or (symbolp action)
                                (functionp action))
                            (helm-symbol-name action)))))
                   (t "")))
            (t ""))
           " (keeping session)")))

(cl-defmethod helm--setup-source ((_source helm-source)))

(cl-defmethod helm--setup-source :before ((source helm-source))
  (unless (slot-value source 'group)
    (setf (slot-value source 'group) 'helm))
  (when (slot-value source 'delayed)
    (warn "Deprecated usage of helm `delayed' slot in `%s'"
          (slot-value source 'name)))
  (helm-aif (slot-value source 'keymap)
      (let* ((map (if (symbolp it)
                      (symbol-value it)
                    it))
             (must-match-map (when (slot-value source 'must-match)
                               (let ((map (make-sparse-keymap)))
                                 (define-key map (kbd "RET")
                                   'helm-confirm-and-exit-minibuffer)
                                 map)))
             (loc-map (if must-match-map
                          (make-composed-keymap
                           must-match-map map)
                        map)))
        (setf (slot-value source 'keymap) loc-map)))
  (helm-aif (slot-value source 'persistent-help)
      (setf (slot-value source 'header-line)
            (helm-source--persistent-help-string it source))
    (setf (slot-value source 'header-line) (helm-source--header-line source)))
  (when (slot-value source 'fuzzy-match)
    (cl-assert helm-fuzzy-sort-fn nil "Wrong type argument functionp: nil")
    (setf (slot-value source 'filtered-candidate-transformer)
          (helm-aif (slot-value source 'filtered-candidate-transformer)
              (append (helm-mklist it)
                      (list helm-fuzzy-sort-fn))
            (list helm-fuzzy-sort-fn))))
  (unless (slot-value source 'nohighlight)
    (setf (slot-value source 'filtered-candidate-transformer)
          (helm-aif (slot-value source 'filtered-candidate-transformer)
              (append (helm-mklist it)
                      (list #'helm-fuzzy-highlight-matches))
            (list #'helm-fuzzy-highlight-matches))))
  (when (numberp (helm-interpret-value (slot-value source 'multiline)))
    (setf (slot-value source 'filtered-candidate-transformer)
          (helm-aif (slot-value source 'filtered-candidate-transformer)
              (append (helm-mklist it)
                      (list #'helm-multiline-transformer))
            (list #'helm-multiline-transformer))))
  (helm-aif (slot-value source 'requires-pattern)
      (let ((val (if (symbolp it)
                     (symbol-value it)
                   it)))
        (setf (slot-value source 'requires-pattern) val)))
  ;; Warn when hooks are defined as something else as a symbol i.e. a lambda or
  ;; a list, if a function an error will raise later anyway when this function
  ;; is called with `run-hooks'.
  (let ((sname (slot-value source 'name)))
    (helm-aif (slot-value source 'before-init-hook)
        (when (or (and (functionp it) (not (symbolp it)))
                  (consp it))
          (warn "Helm source `%s': before-init-hook Should be defined as a symbol" sname)))
    (helm-aif (slot-value source 'after-init-hook)
        (when (or (and (functionp it) (not (symbolp it)))
                  (consp it))
          (warn "Helm source `%s': after-init-hook Should be defined as a symbol" sname)))))

(cl-defmethod helm-setup-user-source ((_source helm-source)))

(cl-defmethod helm--setup-source ((source helm-source-sync))
  (when (slot-value source 'fuzzy-match)
    (helm-aif (slot-value source 'match)
        (setf (slot-value source 'match)
              (append (helm-mklist it)
                      (list helm-fuzzy-match-fn)))
      (setf (slot-value source 'match) helm-fuzzy-match-fn)))
  (when (slot-value source 'multimatch)
    (setf (slot-value source 'match)
          (helm-source-mm-get-search-or-match-fns source 'match)))
  (helm-aif (and (null (slot-value source 'multimatch))
                 (slot-value source 'migemo))
      (unless (eq it 'nomultimatch) ; Use own migemo fn.
        (setf (slot-value source 'match)
              (append (helm-mklist (slot-value source 'match))
                      '(helm-mm-3-migemo-match)))))
  (when (slot-value source 'match-dynamic)
    (setf (slot-value source 'match) 'identity)
    (setf (slot-value source 'match-part) nil)
    (setf (slot-value source 'multimatch) nil)
    (setf (slot-value source 'fuzzy-match) nil)
    (setf (slot-value source 'volatile) t)))

(cl-defmethod helm--setup-source ((source helm-source-in-buffer))
  (cl-assert (eq (slot-value source 'candidates) 'helm-candidates-in-buffer)
             nil
             (format "Wrong usage of `candidates' attr in `%s' use `data' or `init' instead"
                     (slot-value source 'name)))
  (let ((cur-init (slot-value source 'init)))
    (helm-aif (slot-value source 'data)
        (setf (slot-value source 'init)
              (delq
               nil
               (list
                (and (null (eq 'helm-default-init-source-in-buffer-function
                               cur-init))
                     cur-init)
                (lambda ()
                  (helm-init-candidates-in-buffer
                      'global
                    (cond ((functionp it) (funcall it))
                          ((and (bufferp it) (buffer-live-p it))
                           (with-current-buffer it (buffer-string)))
                          (t it)))))))))
  (when (slot-value source 'fuzzy-match)
    (helm-aif (slot-value source 'search)
        (setf (slot-value source 'search)
              (append (helm-mklist it)
                      (list helm-fuzzy-search-fn)))
      (setf (slot-value source 'search) (list helm-fuzzy-search-fn))))
  (when (slot-value source 'multimatch)
    (setf (slot-value source 'search)
          (helm-source-mm-get-search-or-match-fns source 'search)))
  (helm-aif (and (null (slot-value source 'multimatch))
                 (slot-value source 'migemo))
      (unless (eq it 'nomultimatch)
        (setf (slot-value source 'search)
              (append (helm-mklist (slot-value source 'search))
                      '(helm-mm-3-migemo-search)))))
  (let ((mtc (slot-value source 'match)))
    (cl-assert (or (equal '(identity) mtc)
                   (eq 'identity mtc))
               nil "Invalid slot value for `match'")
    (cl-assert (eq (slot-value source 'volatile) t)
               nil "Invalid slot value for `volatile'")))

(cl-defmethod helm--setup-source ((source helm-source-async))
  (cl-assert (null (slot-value source 'candidates))
             nil "Incorrect use of `candidates' use `candidates-process' instead")
  (cl-assert (null (slot-value source 'multimatch))
             nil "`multimatch' not allowed in async sources.")
  (cl-assert (null (slot-value source 'fuzzy-match))
             nil "`fuzzy-match' not supported in async sources."))

(cl-defmethod helm--setup-source ((source helm-source-dummy))
  (let ((mtc (slot-value source 'match)))
    (cl-assert (or (equal '(identity) mtc)
                   (eq 'identity mtc))
               nil "Invalid slot value for `match'")
    (cl-assert (eq (slot-value source 'volatile) t)
               nil "Invalid slot value for `volatile'")
    (cl-assert (equal (slot-value source 'candidates) '("dummy"))
               nil "Invalid slot value for `candidates'")
    (cl-assert (eq (slot-value source 'accept-empty) t)
               nil "Invalid slot value for `accept-empty'")))


;;; User functions
;;
;;  Sources
(defmacro helm-build-sync-source (name &rest args)
  "Build a synchronous helm source with name NAME.
Args ARGS are keywords provided by `helm-source-sync'."
  (declare (indent 1))
  `(helm-make-source ,name 'helm-source-sync ,@args))

(defmacro helm-build-async-source (name &rest args)
  "Build a asynchronous helm source with name NAME.
Args ARGS are keywords provided by `helm-source-async'."
  (declare (indent 1))
  `(helm-make-source ,name 'helm-source-async ,@args))

(defmacro helm-build-in-buffer-source (name &rest args)
  "Build a helm source with name NAME using `candidates-in-buffer' method.
Args ARGS are keywords provided by `helm-source-in-buffer'."
  (declare (indent 1))
  `(helm-make-source ,name 'helm-source-in-buffer ,@args))

(defmacro helm-build-dummy-source (name &rest args)
  "Build a helm source with name NAME using `dummy' method.
Args ARGS are keywords provided by `helm-source-dummy'."
  (declare (indent 1))
  `(helm-make-source ,name 'helm-source-dummy ,@args))

(defmacro helm-build-in-file-source (name file &rest args)
  "Build a helm source with NAME name using `candidates-in-files' method.
Arg FILE is a filename, the contents of this file will be
used as candidates in buffer.
Args ARGS are keywords provided by `helm-source-in-file'."
  (declare (indent 2))
  `(helm-make-source ,name 'helm-source-in-file
     :candidates-file ,file ,@args))


(provide 'helm-source)

;;; helm-source ends here
