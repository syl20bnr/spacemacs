;;; flycheck.el --- On-the-fly syntax checking -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2022 Flycheck contributors
;; Copyright (C) 2012-2016 Sebastian Wiesner and Flycheck contributors
;; Copyright (C) 2013, 2014 Free Software Foundation, Inc.
;;
;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; Maintainer: Clément Pit-Claudel <clement.pitclaudel@live.com>
;;             fmdkdd <fmdkdd@gmail.com>
;; URL: http://www.flycheck.org
;; Keywords: convenience, languages, tools
;; Version: 33-cvs
;; Package-Requires: ((emacs "25.1") (dash "2.12.1") (pkg-info "0.4") (let-alist "1.0.4") (seq "1.11"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; On-the-fly syntax checking for GNU Emacs 24.
;;
;; Flycheck is a modern on-the-fly syntax checking extension for GNU Emacs,
;; intended as replacement for the older Flymake extension which is part of GNU
;; Emacs.
;;
;; Flycheck automatically checks buffers for errors while you type, and reports
;; warnings and errors directly in the buffer and in an optional IDE-like error
;; list.
;;
;; It comes with a rich interface for custom syntax checkers and other
;; extensions, and has already many 3rd party extensions adding new features.
;;
;; Please read the online manual at http://www.flycheck.org for more
;; information.  You can open the manual directly from Emacs with `M-x
;; flycheck-manual'.
;;
;; # Setup
;;
;; Flycheck works best on Unix systems.  It does not officially support Windows,
;; but tries to maintain Windows compatibility and should generally work fine on
;; Windows, too.
;;
;; To enable Flycheck add the following to your init file:
;;
;;    (add-hook 'after-init-hook #'global-flycheck-mode)
;;
;; Flycheck will then automatically check buffers in supported languages, as
;; long as all necessary tools are present.  Use `flycheck-verify-setup' to
;; troubleshoot your Flycheck setup.

;;; Code:

(eval-when-compile
  (require 'let-alist)      ; `let-alist'
  (require 'compile)        ; Compile Mode integration
  (require 'jka-compr)      ; To inhibit compression of temp files
  (require 'pcase)          ; `pcase-dolist' (`pcase' itself is autoloaded)
  )

(require 'dash)

(require 'seq)                   ; Sequence functions
(require 'subr-x nil 'no-error)  ; Additional utilities, Emacs 24.4 and upwards
(require 'cl-lib)                ; `cl-defstruct' and CL utilities
(require 'tabulated-list)        ; To list errors
(require 'easymenu)              ; Flycheck Mode menu definition
(require 'rx)                    ; Regexp fanciness in `flycheck-define-checker'
(require 'help-mode)             ; `define-button-type'
(require 'find-func)             ; `find-function-regexp-alist'
(require 'json)                  ; `flycheck-parse-tslint'
(require 'ansi-color)            ; `flycheck-parse-with-patterns-without-color'


;; Declare a bunch of dynamic variables that we need from other modes
(defvar sh-shell)                       ; For shell script checker predicates
(defvar ess-language)                   ; For r-lintr predicate
(defvar markdown-hide-markup)                     ;
(defvar markdown-fontify-code-block-default-mode) ; For rust-error-explainer
(defvar markdown-fontify-code-blocks-natively)    ;

;; Tell the byte compiler about autoloaded functions from packages
(declare-function pkg-info-version-info "pkg-info" (package))


;;; Compatibility
(eval-and-compile
  (unless (fboundp 'string-suffix-p)
    ;; TODO: Remove when dropping support for Emacs 24.3 and earlier
    (defun string-suffix-p (suffix string &optional ignore-case)
      "Return non-nil if SUFFIX is a suffix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
      (let ((start-pos (- (length string) (length suffix))))
        (and (>= start-pos 0)
             (eq t (compare-strings suffix nil nil
                                    string start-pos nil ignore-case))))))

  (defalias 'flycheck--format-message
    (if (fboundp 'format-message) #'format-message #'format))

  ;; TODO: Remove when dropping support for Emacs 24.3 and earlier
  (unless (featurep 'subr-x)
    ;; `subr-x' function for Emacs 24.3 and below
    (defsubst string-join (strings &optional separator)
      "Join all STRINGS using SEPARATOR."
      (mapconcat 'identity strings separator))

    (defsubst string-trim-left (string)
      "Remove leading whitespace from STRING."
      (if (string-match "\\`[ \t\n\r]+" string)
          (replace-match "" t t string)
        string))

    (defsubst string-trim-right (string)
      "Remove trailing whitespace from STRING."
      (if (string-match "[ \t\n\r]+\\'" string)
          (replace-match "" t t string)
        string))

    (defsubst string-trim (string)
      "Remove leading and trailing whitespace from STRING."
      (string-trim-left (string-trim-right string)))

    (defsubst string-empty-p (string)
      "Check whether STRING is empty."
      (string= string ""))))


;;; Customization
(defgroup flycheck nil
  "Modern on-the-fly syntax checking for GNU Emacs."
  :prefix "flycheck-"
  :group 'tools
  :link '(url-link :tag "Website" "http://www.flycheck.org")
  :link '(url-link :tag "Github" "https://github.com/flycheck/flycheck"))

(defgroup flycheck-config-files nil
  "Configuration files for on-the-fly syntax checkers."
  :prefix "flycheck-"
  :group 'flycheck)

(defgroup flycheck-options nil
  "Options for on-the-fly syntax checkers."
  :prefix "flycheck-"
  :group 'flycheck)

(defgroup flycheck-executables nil
  "Executables of syntax checkers."
  :prefix "flycheck-"
  :group 'flycheck)

(defgroup flycheck-faces nil
  "Faces used by on-the-fly syntax checking."
  :prefix "flycheck-"
  :group 'flycheck)

(defcustom flycheck-checkers
  '(ada-gnat
    asciidoctor
    asciidoc
    awk-gawk
    bazel-build-buildifier
    bazel-module-buildifier
    bazel-starlark-buildifier
    bazel-workspace-buildifier
    c/c++-clang
    c/c++-gcc
    c/c++-cppcheck
    cfengine
    chef-foodcritic
    coffee
    coffee-coffeelint
    coq
    css-csslint
    css-stylelint
    cuda-nvcc
    cwl
    d-dmd
    dockerfile-hadolint
    elixir-credo
    emacs-lisp
    emacs-lisp-checkdoc
    ember-template
    erlang-rebar3
    erlang
    eruby-erubis
    eruby-ruumba
    fortran-gfortran
    go-gofmt
    go-golint
    go-vet
    go-build
    go-test
    go-errcheck
    go-unconvert
    go-staticcheck
    groovy
    haml
    handlebars
    haskell-stack-ghc
    haskell-ghc
    haskell-hlint
    html-tidy
    javascript-eslint
    javascript-jshint
    javascript-standard
    json-jsonlint
    json-python-json
    json-jq
    jsonnet
    less
    less-stylelint
    llvm-llc
    lua-luacheck
    lua
    markdown-markdownlint-cli
    markdown-mdl
    nix
    nix-linter
    opam
    perl
    perl-perlcritic
    php
    php-phpmd
    php-phpcs
    processing
    proselint
    protobuf-protoc
    protobuf-prototool
    pug
    puppet-parser
    puppet-lint
    python-flake8
    python-pylint
    python-pycompile
    python-pyright
    python-mypy
    r-lintr
    racket
    rpm-rpmlint
    rst-sphinx
    rst
    ruby-rubocop
    ruby-standard
    ruby-reek
    ruby-rubylint
    ruby
    ruby-jruby
    rust-cargo
    rust
    rust-clippy
    scala
    scala-scalastyle
    scheme-chicken
    scss-lint
    scss-stylelint
    sass/scss-sass-lint
    sass
    scss
    sh-bash
    sh-posix-dash
    sh-posix-bash
    sh-zsh
    sh-shellcheck
    slim
    slim-lint
    sql-sqlint
    systemd-analyze
    tcl-nagelfar
    terraform
    terraform-tflint
    tex-chktex
    tex-lacheck
    texinfo
    textlint
    typescript-tslint
    verilog-verilator
    vhdl-ghdl
    xml-xmlstarlet
    xml-xmllint
    yaml-jsyaml
    yaml-ruby
    yaml-yamllint)
  "Syntax checkers available for automatic selection.

A list of Flycheck syntax checkers to choose from when syntax
checking a buffer.  Flycheck will automatically select a suitable
syntax checker from this list, unless `flycheck-checker' is set,
either directly or with `flycheck-select-checker'.

You should not need to change this variable normally.  In order
to disable syntax checkers, please use
`flycheck-disabled-checkers'.  This variable is intended for 3rd
party extensions to tell Flycheck about new syntax checkers.

Syntax checkers in this list must be defined with
`flycheck-define-checker'."
  :group 'flycheck
  :type '(repeat (symbol :tag "Checker"))
  :risky t)

(defcustom flycheck-disabled-checkers nil
  "Syntax checkers excluded from automatic selection.

A list of Flycheck syntax checkers to exclude from automatic
selection.  Flycheck will never automatically select a syntax
checker in this list, regardless of the value of
`flycheck-checkers'.

However, syntax checkers in this list are still available for
manual selection with `flycheck-select-checker'.

Use this variable to disable syntax checkers, instead of removing
the syntax checkers from `flycheck-checkers'.  You may also use
this option as a file or directory local variable to disable
specific checkers in individual files and directories
respectively."
  :group 'flycheck
  :type '(repeat (symbol :tag "Checker"))
  :package-version '(flycheck . "0.16")
  :safe #'flycheck-symbol-list-p)
(make-variable-buffer-local 'flycheck-disabled-checkers)

(defvar-local flycheck--automatically-disabled-checkers nil
  "List of syntax checkers automatically disabled for this buffer.

A checker can be automatically disabled in two cases:

1. Its `:enabled' predicate returned false.
2. It returned too many errors (see `flycheck-checker-error-threshold').

To trigger a reverification from Emacs Lisp code, do not modify
this variable: use `flycheck-reset-enabled-checker'.")

(defvar-local flycheck-checker nil
  "Syntax checker to use for the current buffer.

If unset or nil, automatically select a suitable syntax checker
from `flycheck-checkers' on every syntax check.

If set to a syntax checker only use this syntax checker and never
select one from `flycheck-checkers' automatically.  The syntax
checker is used regardless of whether it is contained in
`flycheck-checkers' or `flycheck-disabled-checkers'.  If the
syntax checker is unusable in the current buffer an error is
signaled.

A syntax checker assigned to this variable must be defined with
`flycheck-define-checker'.

Use the command `flycheck-select-checker' to select a syntax
checker for the current buffer, or set this variable as file
local variable to always use a specific syntax checker for a
file.  See Info Node `(Emacs)Specifying File Variables' for more
information about file variables.")
(put 'flycheck-checker 'safe-local-variable 'flycheck-registered-checker-p)

(defcustom flycheck-locate-config-file-functions nil
  "Functions to locate syntax checker configuration files.

Each function in this hook must accept two arguments: The value
of the configuration file variable, and the syntax checker
symbol.  It must return either a string with an absolute path to
the configuration file, or nil, if it cannot locate the
configuration file.

The functions in this hook are called in order of appearance, until a
function returns non-nil.  The configuration file returned by that
function is then given to the syntax checker if it exists.

This variable is an abnormal hook.  See Info
node `(elisp)Hooks'."
  :group 'flycheck
  :type 'hook
  :risky t)

(defcustom flycheck-checker-error-threshold 400
  "Maximum errors allowed per syntax checker.

The value of this variable is either an integer denoting the
maximum number of errors per syntax checker and buffer, or nil to
not limit the errors reported from a syntax checker.

If this variable is a number and a syntax checker reports more
errors than the value of this variable, its errors are not
discarded, and not highlighted in the buffer or available in the
error list.  The affected syntax checker is also disabled for
future syntax checks of the buffer."
  :group 'flycheck
  :type '(choice (const :tag "Do not limit reported errors" nil)
                 (integer :tag "Maximum number of errors"))
  :risky t
  :package-version '(flycheck . "0.22"))

(defcustom flycheck-process-error-functions nil
  "Functions to process errors.

Each function in this hook must accept a single argument: A
Flycheck error to process.

All functions in this hook are called in order of appearance,
until a function returns non-nil.  Thus, a function in this hook
may return nil, to allow for further processing of the error, or
any non-nil value, to indicate that the error was fully processed
and inhibit any further processing.

The functions are called for each newly parsed error immediately
after the corresponding syntax checker finished.  At this stage,
the overlays from the previous syntax checks are still present,
and there may be further syntax checkers in the chain.

This variable is an abnormal hook.  See Info
node `(elisp)Hooks'."
  :group 'flycheck
  :type 'hook
  :package-version '(flycheck . "0.13")
  :risky t)

(defcustom flycheck-display-errors-delay 0.9
  "Delay in seconds before displaying errors at point.

Use floating point numbers to express fractions of seconds."
  :group 'flycheck
  :type 'number
  :package-version '(flycheck . "0.15")
  :safe #'numberp)

(defcustom flycheck-display-errors-function #'flycheck-display-error-messages
  "Function to display error messages.

If set to a function, call the function with the list of errors
to display as single argument.  Each error is an instance of the
`flycheck-error' struct.

If set to nil, do not display errors at all."
  :group 'flycheck
  :type '(choice (const :tag "Display error messages"
                        flycheck-display-error-messages)
                 (const :tag "Display error messages only if no error list"
                        flycheck-display-error-messages-unless-error-list)
                 (function :tag "Error display function"))
  :package-version '(flycheck . "0.13")
  :risky t)

(defcustom flycheck-help-echo-function #'flycheck-help-echo-all-error-messages
  "Function to compute the contents of the error tooltips.

If set to a function, call the function with the list of errors
to display as single argument.  Each error is an instance of the
`flycheck-error' struct.  The function is used to set the
help-echo property of flycheck error overlays.  It should return
a string, which is displayed when the user hovers over an error
or presses \\[display-local-help].

If set to nil, do not show error tooltips."
  :group 'flycheck
  :type '(choice (const :tag "Concatenate error messages to form a tooltip"
                        flycheck-help-echo-all-error-messages)
                 (function :tag "Help echo function"))
  :package-version '(flycheck . "0.25")
  :risky t)

(defcustom flycheck-command-wrapper-function #'identity
  "Function to modify checker commands before execution.

The value of this option is a function which is given a list
containing the full command of a syntax checker after
substitution through `flycheck-substitute-argument' but before
execution.  The function may return a new command for Flycheck to
execute.

The default value is `identity' which does not change the
command.  You may provide your own function to run Flycheck
commands through `bundle exec', `nix-shell' or similar wrappers."
  :group 'flycheck
  :type '(choice (const :tag "Do not modify commands" identity)
                 (function :tag "Modify command with a custom function"))
  :package-version '(flycheck . "0.25")
  :risky t)

(defcustom flycheck-executable-find #'flycheck-default-executable-find
  "Function to search for executables.

The value of this option is a function which is given the name or
path of an executable and shall return the full path to the
executable, or nil if the executable does not exit.

The default is `flycheck-default-executable-find', which searches
variable `exec-path' when given a command name, and resolves
paths to absolute ones.  You can customize this option to search
for checkers in other environments such as bundle or NixOS
sandboxes."
  :group 'flycheck
  :type '(choice
          (const :tag "Search executables in `exec-path'"
                 flycheck-default-executable-find)
          (function :tag "Search executables with a custom function"))
  :package-version '(flycheck . "32")
  :risky t)

(defun flycheck-default-executable-find (executable)
  "Resolve EXECUTABLE to a full path.

Like `executable-find', but supports relative paths.

Attempts invoking `executable-find' first; if that returns nil,
and EXECUTABLE contains a directory component, expands to a full
path and tries invoking `executable-find' again."
  ;; file-name-directory returns non-nil iff the given path has a
  ;; directory component.
  (or
   (executable-find executable)
   (when (file-name-directory executable)
     (executable-find (expand-file-name executable)))))

(defcustom flycheck-indication-mode 'left-fringe
  "The indication mode for Flycheck errors.

This variable controls how Flycheck indicates errors in buffers.
May be `left-fringe', `right-fringe', `left-margin',
`right-margin', or nil.

If set to `left-fringe' or `right-fringe', indicate errors via
icons in the left and right fringe respectively.  If set to
`left-margin' or `right-margin', use the margins instead.

If set to nil, do not indicate errors and warnings, but just
highlight them according to `flycheck-highlighting-mode'."
  :group 'flycheck
  :type '(choice (const :tag "Indicate in the left fringe" left-fringe)
                 (const :tag "Indicate in the right fringe" right-fringe)
                 (const :tag "Indicate in the left margin" left-margin)
                 (const :tag "Indicate in the right margin" right-margin)
                 (const :tag "Do not indicate" nil))
  :safe #'symbolp)

(defcustom flycheck-highlighting-mode 'symbols
  "The highlighting mode for Flycheck errors and warnings.

The highlighting mode controls how Flycheck highlights errors in
buffers when a checker only reports the starting position of an
error.  The following modes are known:

`columns'
     Highlight a single character.  If the error does not have a column,
     highlight the whole line.

`symbols'
     Highlight a full symbol if there is any, otherwise behave like `columns'.
     This is the default.

`sexps'
     Highlight a full expression, if there is any, otherwise behave like
     `columns'.  Note that this mode can be *very* slow in some major modes.

`lines'
     Highlight the whole line.

nil
     Do not highlight errors at all.  However, errors will still
     be reported in the mode line and in error message popups,
     and indicated according to `flycheck-indication-mode'."
  :group 'flycheck
  :type '(choice (const :tag "Highlight columns only" columns)
                 (const :tag "Highlight symbols" symbols)
                 (const :tag "Highlight expressions" sexps)
                 (const :tag "Highlight whole lines" lines)
                 (const :tag "Do not highlight errors" nil))
  :package-version '(flycheck . "0.14")
  :safe #'symbolp)

(defvar flycheck-current-errors)
(defun flycheck-refresh-fringes-and-margins ()
  "Refresh fringes and margins of all windows displaying the current buffer.

If any errors are currently shown, launch a new check, to adjust
to a potential new indication mode."
  (dolist (win (get-buffer-window-list))
    (set-window-margins win left-margin-width right-margin-width)
    (set-window-fringes win left-fringe-width right-fringe-width))
  (when flycheck-current-errors
    (flycheck-buffer)))

(defun flycheck-set-indication-mode (&optional mode)
  "Set `flycheck-indication-mode' to MODE and adjust margins and fringes.

When MODE is nil, adjust window parameters without changing the
mode.  This function can be useful as a `flycheck-mode-hook',
especially if you use margins only in Flycheck buffers.

When MODE is `left-margin', the left fringe is reduced to 1 pixel
to save space."
  (interactive (list (intern (completing-read
                              "Mode: " '("left-fringe" "right-fringe"
                                         "left-margin" "right-margin")
                              nil t nil nil
                              (prin1-to-string flycheck-indication-mode)))))
  (setq mode (or mode flycheck-indication-mode))
  (pcase mode
    ((or `left-fringe `right-fringe)
     (setq left-fringe-width 8 right-fringe-width 8
           left-margin-width 0 right-margin-width 0))
    (`left-margin
     (setq left-fringe-width 1 right-fringe-width 8
           left-margin-width 1 right-margin-width 0))
    (`right-margin
     (setq left-fringe-width 8 right-fringe-width 8
           left-margin-width 0 right-margin-width 1))
    (_ (user-error "Invalid indication mode")))
  (setq-local flycheck-indication-mode mode)
  (flycheck-refresh-fringes-and-margins))

(define-widget 'flycheck-highlighting-style 'lazy
  "A value for `flycheck-highlighting-style'."
  :offset 2
  :format "%t: Use %v"
  :type '(choice
          :format "%[Value Menu%] %v"
          (const :tag "no highlighting" nil)
          (const :tag "a face indicating the error level" level-face)
          (list :tag "a pair of delimiters"
                (const :format "" delimiters)
                (string :tag "Before")
                (string :tag "After"))
          (list :tag "a conditional mix of styles"
                (const :format "" conditional)
                (integer :tag "Up to this many lines")
                (flycheck-highlighting-style :format "Use %v")
                (flycheck-highlighting-style :format "Otherwise, use %v"))))

(defun flycheck--make-highlighting-delimiter (char)
  "Make a highlighting bracket symbol by repeating CHAR twice."
  (compose-chars ?\s
                 ;; '(Bl . Br) ?\s
                 '(Bc Br 30 0) char
                 '(Bc Bl -30 0) char))

(defcustom flycheck-highlighting-style
  `(conditional 4 level-face (delimiters "" ""))
  "The highlighting style for Flycheck errors and warnings.

The highlighting style controls how Flycheck highlights error
regions in buffers.  The following styles are supported:

nil
     Do not highlight errors.  Same as setting
     `flycheck-highlighting-mode' to nil.

`level-face'
     Chose a face depending on the severity of the error, and
     apply it to the whole error text.  See also the
     `flycheck-define-error-level' and `flycheck-error',
     `flycheck-warning', and `flycheck-info' faces.

\(`delimiters' BEFORE AFTER)
     Draw delimiters on each side of the error.  BEFORE and AFTER
     indicate which delimiters to use.  If they are strings, they
     are used as-is.  If they are characters, they are repeated
     twice and composed into a single character.  Delimiters use
     the fringe face corresponding to the severity of each error,
     as well as the `flycheck-error-delimiter' face.  Delimited
     text has the `flycheck-delimited-error' face.

\(`conditional' NLINES S1 S2)
     Use style S1 for errors spanning up to NLINES lines, and
     style S2 otherwise.

See also `flycheck-highlighting-mode' and
`flycheck-indication-mode'."
  :group 'flycheck
  :type 'flycheck-highlighting-style
  :package-version '(flycheck . "32")
  :safe t)

(defcustom flycheck-check-syntax-automatically '(save
                                                 idle-change
                                                 new-line
                                                 mode-enabled)
  "When Flycheck should check syntax automatically.

This variable is a list of events that may trigger syntax checks.
The following events are known:

`save'
     Check syntax immediately after the buffer was saved.

`idle-change'
     Check syntax a short time (see `flycheck-idle-change-delay')
     after the last change to the buffer.

`idle-buffer-switch'
     Check syntax a short time (see `flycheck-idle-buffer-switch-delay')
     after the user switches to a buffer.

`new-line'
     Check syntax immediately after a new line was inserted into
     the buffer.

`mode-enabled'
     Check syntax immediately when variable `flycheck-mode' is
     non-nil.

Flycheck performs a syntax checks only on events, which are
contained in this list.  For instance, if the value of this
variable is `(mode-enabled save)', Flycheck will only check if
the mode is enabled or the buffer was saved, but never after
changes to the buffer contents.

If nil, never check syntax automatically.  In this case, use
`flycheck-buffer' to start a syntax check manually."
  :group 'flycheck
  :type '(set (const :tag "After the buffer was saved" save)
              (const :tag "After the buffer was changed and idle" idle-change)
              (const
               :tag "After switching the current buffer" idle-buffer-switch)
              (const :tag "After a new line was inserted" new-line)
              (const :tag "After `flycheck-mode' was enabled" mode-enabled))
  :package-version '(flycheck . "0.12")
  :safe #'flycheck-symbol-list-p)

(defcustom flycheck-idle-change-delay 0.5
  "How many seconds to wait after a change before checking syntax.

After the buffer was changed, Flycheck will wait as many seconds
as the value of this variable before starting a syntax check.  If
the buffer is modified during this time, Flycheck will wait
again.

This variable has no effect, if `idle-change' is not contained in
`flycheck-check-syntax-automatically'."
  :group 'flycheck
  :type 'number
  :package-version '(flycheck . "0.13")
  :safe #'numberp)

(defcustom flycheck-idle-buffer-switch-delay 0.5
  "How many seconds to wait after switching buffers before checking syntax.

After the user switches to a new buffer, Flycheck will wait as
many seconds as the value of this variable before starting a
syntax check.  If the user switches to another buffer during this
time, whether a syntax check is still performed depends on the
value of `flycheck-buffer-switch-check-intermediate-buffers'.

This variable has no effect if `idle-buffer-switch' is not
contained in `flycheck-check-syntax-automatically'."
  :group 'flycheck
  :type 'number
  :package-version '(flycheck . "32")
  :safe #'numberp)

(defcustom flycheck-buffer-switch-check-intermediate-buffers nil
  "Whether to check syntax in a buffer you only visit briefly.

If nil, then when you switch to a buffer but switch to another
buffer before the syntax check is performed, then the check is
canceled.  If non-nil, then syntax checks due to switching
buffers are always performed.  This only affects buffer switches
that happen less than `flycheck-idle-buffer-switch-delay' seconds
apart.

This variable has no effect if `idle-buffer-switch' is not
contained in `flycheck-check-syntax-automatically'."
  :group 'flycheck
  :type 'boolean
  :package-version '(flycheck . "32")
  :safe #'booleanp)

(defcustom flycheck-standard-error-navigation t
  "Whether to support error navigation with `next-error'.

If non-nil, enable navigation of Flycheck errors with
`next-error', `previous-error' and `first-error'.  Otherwise,
these functions just navigate errors from compilation modes.

Flycheck error navigation with `flycheck-next-error',
`flycheck-previous-error' and `flycheck-first-error' is always
enabled, regardless of the value of this variable.

Note that this setting only takes effect when variable
`flycheck-mode' is non-nil.  Changing it will not affect buffers
where variable `flycheck-mode' is already non-nil."
  :group 'flycheck
  :type 'boolean
  :package-version '(flycheck . "0.15")
  :safe #'booleanp)

(define-widget 'flycheck-minimum-level 'lazy
  "A radio-type choice of minimum error levels.

See `flycheck-navigation-minimum-level' and
`flycheck-error-list-minimum-level'."
  :type '(radio (const :tag "All locations" nil)
                (const :tag "Informational messages" info)
                (const :tag "Warnings" warning)
                (const :tag "Errors" error)
                (symbol :tag "Custom error level")))

(defcustom flycheck-navigation-minimum-level nil
  "The minimum level of errors to navigate.

If set to an error level, only navigate errors whose error level
is at least as severe as this one.  If nil, navigate all errors."
  :group 'flycheck
  :type 'flycheck-minimum-level
  :safe #'flycheck-error-level-p
  :package-version '(flycheck . "0.21"))

(defcustom flycheck-error-list-minimum-level nil
  "The minimum level of errors to display in the error list.

If set to an error level, only display errors whose error level
is at least as severe as this one in the error list.  If nil,
display all errors.

This is the default level, used when the error list is opened.
You can temporarily change the level using
\\[flycheck-error-list-set-filter], or reset it to this value
using \\[flycheck-error-list-reset-filter]."
  :group 'flycheck
  :type 'flycheck-minimum-level
  :safe #'flycheck-error-level-p
  :package-version '(flycheck . "0.24"))

(defcustom flycheck-relevant-error-other-file-minimum-level 'error
  "The minimum level of errors from other files to display in this buffer.

If set to an error level, only display errors from other files
whose error level is at least as severe as this one.  If nil,
display all errors from other files."
  :group 'flycheck
  :type 'flycheck-minimum-level
  :safe #'flycheck-error-level-p
  :package-version '(flycheck . "32"))

(defcustom flycheck-relevant-error-other-file-show t
  "Whether to show errors from other files."
  :group 'flycheck
  :type 'boolean
  :package-version '(flycheck . "32")
  :safe #'booleanp)

(defcustom flycheck-completing-read-function #'completing-read
  "Function to read from minibuffer with completion.

The function must be compatible to the built-in `completing-read'
function."
  :group 'flycheck
  :type '(choice (const :tag "Default" completing-read)
                 (const :tag "IDO" ido-completing-read)
                 (function :tag "Custom function"))
  :risky t
  :package-version '(flycheck . "26"))

(defcustom flycheck-temp-prefix "flycheck"
  "Prefix for temporary files created by Flycheck."
  :group 'flycheck
  :type 'string
  :package-version '(flycheck . "0.19")
  :risky t)

(defcustom flycheck-mode-hook nil
  "Hooks to run after command `flycheck-mode' is toggled."
  :group 'flycheck
  :type 'hook
  :risky t)

(defcustom flycheck-after-syntax-check-hook nil
  "Functions to run after each syntax check.

This hook is run after a syntax check was finished.

At this point, *all* chained checkers were run, and all errors
were parsed, highlighted and reported.  The variable
`flycheck-current-errors' contains all errors from all syntax
checkers run during the syntax check, so you can apply any error
analysis functions.

Note that this hook does *not* run after each individual syntax
checker in the syntax checker chain, but only after the *last
checker*.

This variable is a normal hook.  See Info node `(elisp)Hooks'."
  :group 'flycheck
  :type 'hook
  :risky t)

(defcustom flycheck-before-syntax-check-hook nil
  "Functions to run before each syntax check.

This hook is run right before a syntax check starts.

Error information from the previous syntax check is *not*
cleared before this hook runs.

Note that this hook does *not* run before each individual syntax
checker in the syntax checker chain, but only before the *first
checker*.

This variable is a normal hook.  See Info node `(elisp)Hooks'."
  :group 'flycheck
  :type 'hook
  :risky t)

(defcustom flycheck-syntax-check-failed-hook nil
  "Functions to run if a syntax check failed.

This hook is run whenever an error occurs during Flycheck's
internal processing.  No information about the error is given to
this hook.

You should use this hook to conduct additional cleanup actions
when Flycheck failed.

This variable is a normal hook.  See Info node `(elisp)Hooks'."
  :group 'flycheck
  :type 'hook
  :risky t)

(defcustom flycheck-status-changed-functions nil
  "Functions to run if the Flycheck status changed.

This hook is run whenever the status of Flycheck changes.  Each
hook function takes the status symbol as single argument, as
given to `flycheck-report-status', which see.

This variable is an abnormal hook.  See Info
node `(elisp)Hooks'."
  :group 'flycheck
  :type 'hook
  :risky t
  :package-version '(flycheck . "0.20"))

(defcustom flycheck-error-list-after-refresh-hook nil
  "Functions to run after the error list was refreshed.

This hook is run whenever the error list is refreshed.

This variable is a normal hook.  See Info node `(elisp)Hooks'."
  :group 'flycheck
  :type 'hook
  :risky t
  :package-version '(flycheck . "0.21"))

(defface flycheck-error-delimiter
  `((t))
  "Flycheck face for errors spanning multiple lines.

See `flycheck-highlighting-style' for details on when this face
is used."
  :package-version '(flycheck . "32")
  :group 'flycheck-faces)

(defface flycheck-delimited-error
  `((t))
  "Flycheck face for errors spanning multiple lines.

See `flycheck-highlighting-style' for details on when this face
is used."
  :package-version '(flycheck . "32")
  :group 'flycheck-faces)

(defface flycheck-error
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t
     :underline t :inherit error))
  "Flycheck face for errors."
  :package-version '(flycheck . "0.13")
  :group 'flycheck-faces)

(defface flycheck-warning
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "DarkOrange"))
    (t
     :underline t :inherit warning))
  "Flycheck face for warnings."
  :package-version '(flycheck . "0.13")
  :group 'flycheck-faces)

(defface flycheck-info
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "ForestGreen"))
    (t
     :underline t :inherit success))
  "Flycheck face for informational messages."
  :package-version '(flycheck . "0.15")
  :group 'flycheck-faces)

(defface flycheck-fringe-error
  '((t :inherit error))
  "Flycheck face for fringe error indicators."
  :package-version '(flycheck . "0.13")
  :group 'flycheck-faces)

(defface flycheck-fringe-warning
  '((t :inherit warning))
  "Flycheck face for fringe warning indicators."
  :package-version '(flycheck . "0.13")
  :group 'flycheck-faces)

(defface flycheck-fringe-info
  ;; Semantically `success' is probably not the right face, but it looks nice as
  ;; a base face
  '((t :inherit success))
  "Flycheck face for fringe info indicators."
  :package-version '(flycheck . "0.15")
  :group 'flycheck-faces)

(defface flycheck-error-list-error
  '((t :inherit error))
  "Flycheck face for error messages in the error list."
  :package-version '(flycheck . "0.16")
  :group 'flycheck-faces)

(defface flycheck-error-list-warning
  '((t :inherit warning))
  "Flycheck face for warning messages in the error list."
  :package-version '(flycheck . "0.16")
  :group 'flycheck-faces)

(defface flycheck-error-list-info
  '((t :inherit success))
  "Flycheck face for info messages in the error list."
  :package-version '(flycheck . "0.16")
  :group 'flycheck-faces)

(defface flycheck-error-list-line-number
  '((t))
  "Face for line numbers in the error list."
  :group 'flycheck-faces
  :package-version '(flycheck . "0.16"))

(defface flycheck-error-list-column-number
  '((t))
  "Face for line numbers in the error list."
  :group 'flycheck-faces
  :package-version '(flycheck . "0.16"))

(defface flycheck-error-list-filename
  '((t :inherit mode-line-buffer-id :bold nil))
  "Face for filenames in the error list."
  :group 'flycheck-faces
  :package-version '(flycheck . "32"))

(defface flycheck-error-list-id
  '((t :inherit font-lock-type-face))
  "Face for the error ID in the error list."
  :group 'flycheck-faces
  :package-version '(flycheck . "0.22"))

(defface flycheck-error-list-id-with-explainer
  '((t :inherit flycheck-error-list-id
       :box (:style released-button)))
  "Face for the error ID in the error list, for errors that have an explainer."
  :group 'flycheck-faces
  :package-version '(flycheck . "30"))

(defface flycheck-error-list-checker-name
  '((t :inherit font-lock-function-name-face))
  "Face for the syntax checker name in the error list."
  :group 'flycheck-faces
  :package-version '(flycheck . "0.21"))

(defface flycheck-error-list-error-message
  '((t))
  "Face for the error message in the error list."
  :group 'flycheck-faces
  :package-version '(flycheck . "33"))

(defface flycheck-error-list-highlight
  '((t :bold t))
  "Flycheck face to highlight errors in the error list."
  :package-version '(flycheck . "0.15")
  :group 'flycheck-faces)

(defface flycheck-verify-select-checker
  '((t :box (:style released-button)))
  "Flycheck face for the `select' button in the verify setup buffer."
  :package-version '(flycheck . "32")
  :group 'flycheck-faces)

(defvar flycheck-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c"         #'flycheck-buffer)
    (define-key map "C"         #'flycheck-clear)
    (define-key map (kbd "C-c") #'flycheck-compile)
    (define-key map "n"         #'flycheck-next-error)
    (define-key map "p"         #'flycheck-previous-error)
    (define-key map "l"         #'flycheck-list-errors)
    (define-key map (kbd "C-w") #'flycheck-copy-errors-as-kill)
    (define-key map "s"         #'flycheck-select-checker)
    (define-key map "?"         #'flycheck-describe-checker)
    (define-key map "h"         #'flycheck-display-error-at-point)
    (define-key map "e"         #'flycheck-explain-error-at-point)
    (define-key map "H"         #'display-local-help)
    (define-key map "i"         #'flycheck-manual)
    (define-key map "V"         #'flycheck-version)
    (define-key map "v"         #'flycheck-verify-setup)
    (define-key map "x"         #'flycheck-disable-checker)
    map)
  "Keymap of Flycheck interactive commands.")

(defcustom flycheck-keymap-prefix (kbd "C-c !")
  "Prefix for key bindings of Flycheck.

Changing this variable outside Customize does not have any
effect.  To change the keymap prefix from Lisp, you need to
explicitly re-define the prefix key:

    (define-key flycheck-mode-map flycheck-keymap-prefix nil)
    (setq flycheck-keymap-prefix (kbd \"C-c f\"))
    (define-key flycheck-mode-map flycheck-keymap-prefix
                flycheck-command-map)

Please note that Flycheck's manual documents the default
keybindings.  Changing this variable is at your own risk."
  :group 'flycheck
  :package-version '(flycheck . "0.19")
  :type 'string
  :risky t
  :set
  (lambda (variable key)
    (when (and (boundp variable) (boundp 'flycheck-mode-map))
      (define-key flycheck-mode-map (symbol-value variable) nil)
      (define-key flycheck-mode-map key flycheck-command-map))
    (set-default variable key)))

(defcustom flycheck-mode-line '(:eval (flycheck-mode-line-status-text))
  "Mode line lighter for Flycheck.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for
more information.  Note that it should contain a _single_ mode
line construct only.

Customize this variable to change how Flycheck reports its status
in the mode line.  You may use `flycheck-mode-line-status-text'
to obtain a human-readable status text, including an
error/warning count.

You may also assemble your own status text.  The current status
of Flycheck is available in `flycheck-last-status-change'.  The
errors in the current buffer are stored in
`flycheck-current-errors', and the function
`flycheck-count-errors' may be used to obtain the number of
errors grouped by error level.

Set this variable to nil to disable the mode line completely."
  :group 'flycheck
  :type 'sexp
  :risky t
  :package-version '(flycheck . "0.20"))

(defcustom flycheck-mode-line-prefix "FlyC"
  "Base mode line lighter for Flycheck.

This will have an effect only with the default
`flycheck-mode-line'.

If you've customized `flycheck-mode-line' then the customized
function must be updated to use this variable."
  :group 'flycheck
  :type 'string
  :package-version '(flycheck . "26"))

(defcustom flycheck-error-list-mode-line
  `(,(propertized-buffer-identification "%12b")
    " for buffer "
    (:eval (flycheck-error-list-propertized-source-name))
    (:eval (flycheck-error-list-mode-line-filter-indicator)))
  "Mode line construct for Flycheck error list.

The value of this variable is a mode line template as in
`mode-line-format', to be used as
`mode-line-buffer-identification' in `flycheck-error-list-mode'.
See Info Node `(elisp)Mode Line Format' for more information.

Customize this variable to change how the error list appears in
the mode line.  The default shows the name of the buffer and the
name of the source buffer, i.e. the buffer whose errors are
currently listed."
  :group 'flycheck
  :type 'sexp
  :risky t
  :package-version '(flycheck . "0.20"))

(defcustom flycheck-global-modes t
  "Modes for which option `flycheck-mode' is turned on.

If t, Flycheck Mode is turned on for all major modes.  If a list,
Flycheck Mode is turned on for all `major-mode' symbols in that
list.  If the `car' of the list is `not', Flycheck Mode is turned
on for all `major-mode' symbols _not_ in that list.  If nil,
Flycheck Mode is never turned on by command
`global-flycheck-mode'.

Note that Flycheck is never turned on for modes whose
`mode-class' property is `special' (see Info node `(elisp)Major
Mode Conventions'), regardless of the value of this option.

Only has effect when variable `global-flycheck-mode' is non-nil."
  :group 'flycheck
  :type '(choice (const :tag "none" nil)
                 (const :tag "all" t)
                 (set :menu-tag "mode specific" :tag "modes"
                      :value (not)
                      (const :tag "Except" not)
                      (repeat :inline t (symbol :tag "mode"))))
  :risky t
  :package-version '(flycheck . "0.23"))

;; Add built-in functions to our hooks, via `add-hook', to make sure that our
;; functions are really present, even if the variable was implicitly defined by
;; another call to `add-hook' that occurred before Flycheck was loaded.  See
;; http://lists.gnu.org/archive/html/emacs-devel/2015-02/msg01271.html for why
;; we don't initialize the hook variables right away.  We append our own
;; functions, because a user likely expects that their functions come first,
;; even if they added them before Flycheck was loaded.
(dolist (hook (list #'flycheck-locate-config-file-by-path
                    #'flycheck-locate-config-file-ancestor-directories
                    #'flycheck-locate-config-file-home))
  (add-hook 'flycheck-locate-config-file-functions hook 'append))

(add-hook 'flycheck-process-error-functions #'flycheck-add-overlay 'append)


;;; Global Flycheck menu
(defvar flycheck-mode-menu-map
  (easy-menu-create-menu
   "Syntax Checking"
   '(["Enable on-the-fly syntax checking" flycheck-mode
      :style toggle :selected flycheck-mode
      :enable (or flycheck-mode
                  ;; Don't let users toggle the mode if there is no syntax
                  ;; checker for this buffer
                  (seq-find #'flycheck-checker-supports-major-mode-p
                            flycheck-checkers))]
     ["Check current buffer" flycheck-buffer flycheck-mode]
     ["Clear errors in buffer" flycheck-clear t]
     "---"
     ["Go to next error" flycheck-next-error flycheck-mode]
     ["Go to previous error" flycheck-previous-error flycheck-mode]
     ["Show all errors" flycheck-list-errors flycheck-mode]
     "---"
     ["Copy messages at point" flycheck-copy-errors-as-kill
      (flycheck-overlays-at (point))]
     ["Explain error at point" flycheck-explain-error-at-point]
     "---"
     ["Select syntax checker" flycheck-select-checker flycheck-mode]
     ["Disable syntax checker" flycheck-disable-checker flycheck-mode]
     ["Set executable of syntax checker" flycheck-set-checker-executable
      flycheck-mode]
     "---"
     ["Describe syntax checker" flycheck-describe-checker t]
     ["Verify setup" flycheck-verify-setup t]
     ["Show Flycheck version" flycheck-version t]
     ["Read the Flycheck manual" flycheck-info t]))
  "Menu of command `flycheck-mode'.")

(easy-menu-add-item nil '("Tools") flycheck-mode-menu-map "Spell Checking")


;;; Version information, manual and loading of Flycheck
(defun flycheck-version (&optional show-version)
  "Get the Flycheck version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (let ((version (pkg-info-version-info 'flycheck)))
    (when show-version
      (message "Flycheck version: %s" version))
    version))

(defun flycheck-unload-function ()
  "Unload function for Flycheck."
  (global-flycheck-mode -1)
  (easy-menu-remove-item nil '("Tools") (cadr flycheck-mode-menu-map))
  (remove-hook 'kill-emacs-hook #'flycheck-global-teardown)
  (setq find-function-regexp-alist
        (assq-delete-all 'flycheck-checker find-function-regexp-alist)))

;;;###autoload
(defun flycheck-manual ()
  "Open the Flycheck manual."
  (interactive)
  (browse-url "http://www.flycheck.org"))

(define-obsolete-function-alias 'flycheck-info
  'flycheck-manual "Flycheck 26" "Open the Flycheck manual.")


;;; Utility functions
(defun flycheck-sexp-to-string (sexp)
  "Convert SEXP to a string.

Like `prin1-to-string' but ensure that the returned string
is loadable."
  (let ((print-quoted t)
        (print-length nil)
        (print-level nil))
    (prin1-to-string sexp)))

(defun flycheck-string-to-number-safe (string)
  "Safely convert STRING to a number.

If STRING is of string type and a numeric string, convert STRING
to a number and return it.  Otherwise return nil."
  (let ((number-re (rx string-start (one-or-more (any digit)) string-end)))
    (when (and (stringp string) (string-match-p number-re string))
      (string-to-number string))))

(defun flycheck-string-or-nil-p (obj)
  "Determine if OBJ is a string or nil."
  (or (null obj) (stringp obj)))

(defun flycheck-string-list-p (obj)
  "Determine if OBJ is a list of strings."
  (and (listp obj) (seq-every-p #'stringp obj)))

(defun flycheck-string-or-string-list-p (obj)
  "Determine if OBJ is a string or a list of strings."
  (or (stringp obj) (flycheck-string-list-p obj)))

(defun flycheck-symbol-list-p (obj)
  "Determine if OBJ is a list of symbols."
  (and (listp obj) (seq-every-p #'symbolp obj)))

(defvar-local flycheck--file-truename-cache nil)

(defun flycheck--file-truename (file)
  "Memoize the result of `file-truename' on (directory-file-name FILE)."
  ;; `file-truename' is slow, but alternatives are incomplete, so memoizing is
  ;; our best bet.  See https://github.com/flycheck/flycheck/pull/1698.
  (unless flycheck--file-truename-cache
    (setq-local flycheck--file-truename-cache (make-hash-table :test 'equal)))
  (or (gethash file flycheck--file-truename-cache)
      (puthash file (file-truename (directory-file-name file))
               flycheck--file-truename-cache)))

(defun flycheck-same-files-p (file-a file-b)
  "Determine whether FILE-A and FILE-B refer to the same file.

Files are the same if (in the order checked) they are equal, or
if they resolve to the same canonical paths."
  (or (string= file-a file-b)
      (string= (flycheck--file-truename file-a)
               (flycheck--file-truename file-b))))

(defvar-local flycheck-temporaries nil
  "Temporary files and directories created by Flycheck.")

(defun flycheck-temp-dir-system ()
  "Create a unique temporary directory.

Use `flycheck-temp-prefix' as prefix, and add the directory to
`flycheck-temporaries'.

Return the path of the directory"
  (let* ((tempdir (make-temp-file flycheck-temp-prefix 'directory)))
    (push tempdir flycheck-temporaries)
    tempdir))

(defun flycheck-temp-file-system (filename &optional suffix)
  "Create a temporary file named after FILENAME.

If FILENAME is non-nil, this function creates a temporary
directory with `flycheck-temp-dir-system', and creates a file
with the same name as FILENAME in this directory.

Otherwise this function creates a temporary file starting with
`flycheck-temp-prefix'.  If present, SUFFIX is appended;
otherwise, a random suffix is used.  The path of the file is
added to `flycheck-temporaries'.

Return the path of the file."
  (let ((tempfile (convert-standard-filename
                   (if filename
                       (expand-file-name (file-name-nondirectory filename)
                                         (flycheck-temp-dir-system))
                     (make-temp-file flycheck-temp-prefix nil suffix)))))
    (push tempfile flycheck-temporaries)
    tempfile))

(defun flycheck-temp-file-inplace (filename &optional suffix)
  "Create an in-place copy of FILENAME.

Prefix the file with `flycheck-temp-prefix' and add the path of
the file to `flycheck-temporaries'.

If FILENAME is nil, fall back to `flycheck-temp-file-system' with
the specified SUFFIX.

Return the path of the file."
  (if filename
      (let* ((tempname (format "%s_%s"
                               flycheck-temp-prefix
                               (file-name-nondirectory filename)))
             (tempfile (convert-standard-filename
                        (expand-file-name tempname
                                          (file-name-directory filename)))))
        (push tempfile flycheck-temporaries)
        tempfile)
    (flycheck-temp-file-system filename suffix)))

(defun flycheck-temp-directory (checker)
  "Return the directory where CHECKER writes temporary files.

Return nil if the CHECKER does not write temporary files."
  (let ((args (flycheck-checker-arguments checker)))
    (cond
     ((memq 'source args) temporary-file-directory)
     ((memq 'source-inplace args)
      (if buffer-file-name (file-name-directory buffer-file-name)
        temporary-file-directory))
     (t nil))))

(defun flycheck-temp-files-writable-p (checker)
  "Whether CHECKER can write temporary files.

If CHECKER has `source' or `source-inplace' in its `:command',
return whether flycheck has the permissions to create the
respective temporary files.

Return t if CHECKER does not use temporary files."
  (let ((dir (flycheck-temp-directory checker)))
    (or (not dir) (file-writable-p dir))))

(defun flycheck-save-buffer-to-file (file-name)
  "Save the contents of the current buffer to FILE-NAME."
  (make-directory (file-name-directory file-name) t)
  (let ((jka-compr-inhibit t))
    (write-region nil nil file-name nil 0)))

(defun flycheck-save-buffer-to-temp (temp-file-fn)
  "Save buffer to temp file returned by TEMP-FILE-FN.

Return the name of the temporary file."
  (let ((filename (funcall temp-file-fn (buffer-file-name))))
    ;; Do not flush short-lived temporary files onto disk
    (let ((write-region-inhibit-fsync t))
      (flycheck-save-buffer-to-file filename))
    filename))

(defun flycheck-prepend-with-option (option items &optional prepend-fn)
  "Prepend OPTION to each item in ITEMS, using PREPEND-FN.

Prepend OPTION to each item in ITEMS.

ITEMS is a list of strings to pass to the syntax checker.  OPTION
is the option, as string.  PREPEND-FN is a function called to
prepend OPTION to each item in ITEMS.  It receives the option and
a single item from ITEMS as argument, and must return a string or
a list of strings with OPTION prepended to the item.  If
PREPEND-FN is nil or omitted, use `list'.

Return a list of strings where OPTION is prepended to each item
in ITEMS using PREPEND-FN.  If PREPEND-FN returns a list, it is
spliced into the resulting list."
  (unless (stringp option)
    (error "Option %S is not a string" option))
  (unless prepend-fn
    (setq prepend-fn #'list))
  (let ((prepend
         (lambda (item)
           (let ((result (funcall prepend-fn option item)))
             (cond
              ((and (listp result) (seq-every-p #'stringp result)) result)
              ((stringp result) (list result))
              (t (error "Invalid result type for option: %S" result)))))))
    (apply #'append (seq-map prepend items))))

(defun flycheck-find-in-buffer (pattern)
  "Find PATTERN in the current buffer.

Return the result of the first matching group of PATTERN, or nil,
if PATTERN did not match."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward pattern nil 'no-error)
        (match-string-no-properties 1)))))

(defun flycheck-buffer-empty-p (&optional buffer)
  "Check whether a BUFFER is empty, defaulting to the current one."
  (= (buffer-size buffer) 0))

(defun flycheck-buffer-nonempty-p (&optional buffer)
  "Check whether a BUFFER is nonempty, defaulting to the current one."
  (> (buffer-size buffer) 0))

(defun flycheck-ephemeral-buffer-p ()
  "Determine whether the current buffer is an ephemeral buffer.

See Info node `(elisp)Buffer Names' for information about
ephemeral buffers."
  (string-prefix-p " " (buffer-name)))

(defun flycheck-encrypted-buffer-p ()
  "Determine whether the current buffer is an encrypted file.

See Info node `(epa)Top' for Emacs' interface to encrypted
files."
  ;; The EPA file handler sets this variable locally to remember the recipients
  ;; of the encrypted file for re-encryption.  Hence, a local binding of this
  ;; variable is a good indication that the buffer is encrypted.  I haven't
  ;; found any better indicator anyway.
  (local-variable-p 'epa-file-encrypt-to))

(defun flycheck-autoloads-file-p ()
  "Determine whether the current buffer is an autoloads file.

Autoloads are generated by package.el during installation."
  (string-suffix-p "-autoloads.el" (buffer-name)))

(defun flycheck-in-user-emacs-directory-p (filename)
  "Whether FILENAME is in `user-emacs-directory'."
  (string-prefix-p (file-name-as-directory
                    (flycheck--file-truename user-emacs-directory))
                   (flycheck--file-truename filename)))

(defun flycheck-safe-delete (file-or-dir)
  "Safely delete FILE-OR-DIR."
  (ignore-errors
    (if (file-directory-p file-or-dir)
        (delete-directory file-or-dir 'recursive)
      (delete-file file-or-dir))))

(defun flycheck-safe-delete-temporaries ()
  "Safely delete all temp files and directories of Flycheck.

Safely delete all files and directories listed in
`flycheck-temporaries' and set the variable's value to nil."
  (seq-do #'flycheck-safe-delete flycheck-temporaries)
  (setq flycheck-temporaries nil))

(defun flycheck-rx-file-name (form)
  "Translate the `(file-name)' FORM into a regular expression."
  (let ((body (or (cdr form) '((minimal-match
                                (one-or-more not-newline))))))
    (rx-to-string `(group-n 1 ,@body) t)))

(defun flycheck-rx-message (form)
  "Translate the `(message)' FORM into a regular expression."
  (let ((body (or (cdr form) '((one-or-more not-newline)))))
    (rx-to-string `(group-n 4 ,@body) t)))

(defun flycheck-rx-id (form)
  "Translate the `(id)' FORM into a regular expression."
  (rx-to-string `(group-n 5 ,@(cdr form)) t))

(defun flycheck-rx-to-string (form &optional no-group)
  "Like `rx-to-string' for FORM, but with special keywords:

`line'
     matches the initial line number.

`column'
     matches the initial column number.

`end-line'
     matches the final line number.

`end-column'
     matches the final column number (exclusive).


`(file-name SEXP ...)'
     matches the file name.  SEXP describes the file name.  If no
     SEXP is given, use a default body of `(minimal-match
     (one-or-more not-newline))'.

`(message SEXP ...)'
     matches the message.  SEXP constitutes the body of the
     message.  If no SEXP is given, use a default body
     of `(one-or-more not-newline)'.

`(id SEXP ...)'
     matches an error ID.  SEXP describes the ID.

NO-GROUP is passed to `rx-to-string'.

See `rx' for a complete list of all built-in `rx' forms."
  (let ((rx-constituents
         (append
          `((file-name flycheck-rx-file-name 0 nil) ;; group 1
            (line . ,(rx (group-n 2 (one-or-more digit))))
            (column . ,(rx (group-n 3 (one-or-more digit))))
            (message flycheck-rx-message 0 nil) ;; group 4
            (id flycheck-rx-id 0 nil) ;; group 5
            (end-line . ,(rx (group-n 6 (one-or-more digit))))
            (end-column . ,(rx (group-n 7 (one-or-more digit)))))
          rx-constituents nil)))
    (rx-to-string form no-group)))

(defun flycheck-current-load-file ()
  "Get the source file currently being loaded.

Always return the name of the corresponding source file, never
any byte-compiled file.

Return nil, if the currently loaded file cannot be determined."
  (-when-let* ((this-file (cond
                           (load-in-progress load-file-name)
                           ((bound-and-true-p byte-compile-current-file))
                           (t (buffer-file-name))))
               ;; A best guess for the source file of a compiled library. Works
               ;; well in most cases, and especially for ELPA packages
               (source-file (concat (file-name-sans-extension this-file)
                                    ".el")))
    (when (file-exists-p source-file)
      source-file)))

(defun flycheck-module-root-directory (module &optional file-name)
  "Get the root directory for a MODULE in FILE-NAME.

MODULE is a qualified module name, either a string with
components separated by a dot, or as list of components.
FILE-NAME is the name of the file or directory containing the
module as string.  When nil or omitted, defaults to the return
value of function `buffer-file-name'.

Return the root directory of the module, that is, the directory,
from which FILE-NAME can be reached by descending directories
along each part of MODULE.

If the MODULE name does not match the directory hierarchy upwards
from FILE-NAME, return the directory containing FILE-NAME.  When
FILE-NAME is nil, return `default-directory'."
  (let ((file-name (or file-name (buffer-file-name)))
        (module-components (if (stringp module)
                               (split-string module (rx "."))
                             (copy-sequence module))))
    (if (and module-components file-name)
        (let ((parts (nreverse module-components))
              (base-directory (directory-file-name
                               (file-name-sans-extension file-name))))
          (while (and parts
                      (string= (file-name-nondirectory base-directory)
                               (car parts)))
            (pop parts)
            (setq base-directory (directory-file-name
                                  (file-name-directory base-directory))))
          (file-name-as-directory base-directory))
      (if file-name
          (file-name-directory file-name)
        (expand-file-name default-directory)))))

(cl-defstruct (flycheck-line-cache
               (:constructor flycheck-line-cache-new))
  "Cache structure used to speed up `flycheck-goto-line'."
  tick point line)

(defvar-local flycheck--line-cache nil
  "Cache used to speed ip `flycheck-goto-line'.")

(defsubst flycheck--init-line-cache ()
  "Initialize or reinitialize `flycheck--line-cache'."
  (let ((tick (buffer-modified-tick)))
    (if flycheck--line-cache
        (unless (= (flycheck-line-cache-tick flycheck--line-cache) tick)
          (setf (flycheck-line-cache-tick flycheck--line-cache) tick
                (flycheck-line-cache-point flycheck--line-cache) 1
                (flycheck-line-cache-line flycheck--line-cache) 1))
      (setq-local flycheck--line-cache
                  (flycheck-line-cache-new :tick tick :point 1 :line 1)))))

(defun flycheck-goto-line (line)
  "Move point to beginning of line number LINE.

This function assumes that the current buffer is not narrowed."
  (flycheck--init-line-cache)
  (goto-char (flycheck-line-cache-point flycheck--line-cache))
  (let ((delta (- line (flycheck-line-cache-line flycheck--line-cache))))
    (when (= 0 (forward-line delta))
      (setf (flycheck-line-cache-point flycheck--line-cache) (point))
      (setf (flycheck-line-cache-line flycheck--line-cache) line))))

(defun flycheck-line-column-to-position (line column)
  "Return the point closest to LINE, COLUMN on line LINE.

COLUMN is one-based."
  (save-excursion
    (flycheck-goto-line line)
    (min (+ (point) (1- column)) (line-end-position))))

(defun flycheck-line-column-at-point ()
  "Return the line and column number at point."
  (cons (line-number-at-pos) (1+ (- (point) (line-beginning-position)))))

(defun flycheck-line-column-at-pos (pos)
  "Return the line and column number at position POS.

COLUMN is one-based."
  (let ((inhibit-field-text-motion t))
    (save-excursion
      (goto-char pos)
      (flycheck-line-column-at-point))))


;;; Minibuffer tools
(defvar flycheck-read-checker-history nil
  "`completing-read' history of `flycheck-read-checker'.")

(defun flycheck-completing-read (prompt candidates default &optional history)
  "Read a value from the minibuffer.

Use `flycheck-completing-read-function' to read input from the
minibuffer with completion.

Show PROMPT and read one of CANDIDATES, defaulting to DEFAULT.
HISTORY is passed to `flycheck-completing-read-function'.

Note that `flycheck-completing-read-function' may return an empty
string instead of nil, even when \"\" isn't among the candidates.
See `completing-read' for more details."
  (funcall flycheck-completing-read-function
           prompt candidates nil 'require-match nil history default))

(defun flycheck-read-checker (prompt &optional default property candidates)
  "Read a flycheck checker from minibuffer with PROMPT and DEFAULT.

PROMPT is a string to show in the minibuffer as prompt.  It
should end with a single space.  DEFAULT is a symbol denoting the
default checker to use, if the user did not select any checker.
PROPERTY is a symbol denoting a syntax checker property.  If
non-nil, only complete syntax checkers which have a non-nil value
for PROPERTY.  CANDIDATES is an optional list of all syntax
checkers available for completion, defaulting to all defined
checkers.  If given, PROPERTY is ignored.

Return the checker as symbol, or DEFAULT if no checker was
chosen.  If DEFAULT is nil and no checker was chosen, signal a
`user-error' if the underlying completion system does not provide
a default on its own."
  (when (and default (not (flycheck-valid-checker-p default)))
    (error "%S is no valid Flycheck checker" default))
  (let* ((candidates (seq-map #'symbol-name
                              (or candidates
                                  (flycheck-defined-checkers property))))
         (default (and default (symbol-name default)))
         (input (flycheck-completing-read
                 prompt candidates default
                 'flycheck-read-checker-history)))
    (when (string-empty-p input)
      (unless default
        (user-error "No syntax checker selected"))
      (setq input default))
    (let ((checker (intern input)))
      (unless (flycheck-valid-checker-p checker)
        (error "%S is not a valid Flycheck syntax checker" checker))
      checker)))

(defun flycheck-read-error-level (prompt)
  "Read an error level from the user with PROMPT.

Only offers level for which errors currently exist, in addition
to the default levels."
  (let* ((levels (seq-map #'flycheck-error-level
                          (flycheck-error-list-current-errors)))
         (levels-with-defaults (append '(info warning error) levels))
         (uniq-levels (seq-uniq levels-with-defaults))
         (level (flycheck-completing-read prompt uniq-levels nil)))
    (when (string-empty-p level) (setq level nil))
    (and level (intern level))))


;;; Checker API
(defun flycheck-defined-checkers (&optional property)
  "Find all defined syntax checkers, optionally with PROPERTY.

PROPERTY is a symbol.  If given, only return syntax checkers with
a non-nil value for PROPERTY.

The returned list is sorted alphapetically by the symbol name of
the syntax checkers."
  (let (defined-checkers)
    (mapatoms (lambda (symbol)
                (when (and (flycheck-valid-checker-p symbol)
                           (or (null property)
                               (flycheck-checker-get symbol property)))
                  (push symbol defined-checkers))))
    (sort defined-checkers #'string<)))

(defun flycheck-registered-checker-p (checker)
  "Determine whether CHECKER is registered.

A checker is registered if it is contained in
`flycheck-checkers'."
  (and (flycheck-valid-checker-p checker)
       (memq checker flycheck-checkers)))

(defun flycheck-disabled-checker-p (checker)
  "Determine whether CHECKER is disabled, manually or automatically."
  (or (flycheck-manually-disabled-checker-p checker)
      (flycheck-automatically-disabled-checker-p checker)))

(defun flycheck-manually-disabled-checker-p (checker)
  "Determine whether CHECKER has been manually disabled.

A checker has been manually disabled if it is contained in
`flycheck-disabled-checkers'."
  (memq checker flycheck-disabled-checkers))

(defun flycheck-automatically-disabled-checker-p (checker)
  "Determine whether CHECKER has been automatically disabled.

A checker has been automatically disabled if it is contained in
`flycheck--automatically-disabled-checkers'."
  (memq checker flycheck--automatically-disabled-checkers))


;;; Generic syntax checkers
(defconst flycheck-generic-checker-version 2
  "The internal version of generic syntax checker declarations.

Flycheck will not use syntax checkers whose generic version is
less than this constant.")

(defsubst flycheck--checker-property-name (property)
  "Return the SYMBOL property for checker PROPERTY."
  (intern (concat "flycheck-" (symbol-name property))))

(defun flycheck-checker-get (checker property)
  "Get the value of CHECKER's PROPERTY."
  (get checker (flycheck--checker-property-name property)))

(gv-define-setter flycheck-checker-get (value checker property)
  `(setf (get ,checker (flycheck--checker-property-name ,property)) ,value))

(defun flycheck-validate-next-checker (next &optional strict)
  "Validate NEXT checker.

With STRICT non-nil, also check whether the syntax checker and
the error level in NEXT are valid.  Otherwise just check whether
these are symbols.

Signal an error if NEXT is not a valid entry for
`:next-checkers'."
  (when (symbolp next)
    (setq next (cons t next)))
  (pcase next
    (`(,level . ,checker)
     (if strict
         (progn
           (unless (or (eq level t) (flycheck-error-level-p level))
             (error "%S is not a valid Flycheck error level" level))
           (unless (flycheck-valid-checker-p checker)
             (error "%s is not a valid Flycheck syntax checker" checker)))
       (unless (symbolp level)
         (error "Error level %S must be a symbol" level))
       (unless (symbolp checker)
         (error "Checker %S must be a symbol" checker))))
    (_ (error "%S must be a symbol or cons cell" next)))
  t)

(defun flycheck-define-generic-checker (symbol docstring &rest properties)
  "Define SYMBOL as generic syntax checker.

Any syntax checker defined with this macro is eligible for manual
syntax checker selection with `flycheck-select-checker'.  To make
the new syntax checker available for automatic selection, it must
be registered in `flycheck-checkers'.

DOCSTRING is the documentation of the syntax checker, for
`flycheck-describe-checker'.  The following PROPERTIES constitute
a generic syntax checker.  Unless otherwise noted, all properties
are mandatory.

`:start FUNCTION'
     A function to start the syntax checker.

     FUNCTION shall take two arguments and return a context
     object if the checker is started successfully.  Otherwise it
     shall signal an error.

     The first argument is the syntax checker being started.  The
     second is a callback function to report state changes to
     Flycheck.  The callback takes two arguments STATUS DATA,
     where STATUS is a symbol denoting the syntax checker status
     and DATA an optional argument with additional data for the
     status report.  See `flycheck-report-buffer-checker-status'
     for more information about STATUS and DATA.

     FUNCTION may be synchronous or asynchronous, i.e. it may
     call the given callback either immediately, or at some later
     point (e.g. from a process sentinel).

     A syntax checker _must_ call CALLBACK at least once with a
     STATUS that finishes the current syntax checker.  Otherwise
     Flycheck gets stuck at the current syntax check with this
     syntax checker.

     The context object returned by FUNCTION is passed to
     `:interrupt'.

`:interrupt FUNCTION'
     A function to interrupt the syntax check.

     FUNCTION is called with the syntax checker and the context
     object returned by the `:start' function and shall try to
     interrupt the syntax check.  The context may be nil, if the
     syntax check is interrupted before actually started.
     FUNCTION should handle this situation.

     If it cannot interrupt the syntax check, it may either
     signal an error or silently ignore the attempt to interrupt
     the syntax checker, depending on the severity of the
     situation.

     If interrupting the syntax check failed, Flycheck will let
     the syntax check continue, but ignore any status reports.
     Notably, it won't highlight any errors reported by the
     syntax check in the buffer.

     This property is optional.  If omitted, Flycheck won't
     attempt to interrupt syntax checks with this syntax checker,
     and simply ignore their results.

`:print-doc FUNCTION'
     A function to print additional documentation into the Help
     buffer of this checker.

     FUNCTION is called when creating the Help buffer for the
     syntax checker, with the syntax checker as single argument,
     after printing the name of the syntax checker and its modes
     and predicate, but before printing DOCSTRING.  It may insert
     additional documentation into the current buffer.

     The call occurs within `with-help-window'.  Hence
     `standard-output' points to the current buffer, so you may
     use `princ' and friends to add content.  Also, the current
     buffer is put into Help mode afterwards, which automatically
     turns symbols into references, if possible.

     This property is optional.  If omitted, no additional
     documentation is printed for this syntax checker.

:verify FUNCTION
     A function to verify the checker for the current buffer.

     FUNCTION is called with the syntax checker as single
     argument, and shall return a list of
     `flycheck-verification-result' objects indicating whether
     the syntax checker could be used in the current buffer, and
     highlighting potential setup problems.

     This property is optional.  If omitted, no additional
     verification occurs for this syntax checker.  It is however
     absolutely recommended that you add a `:verify' function to
     your syntax checker, because it will help users to spot
     potential setup problems.

`:modes MODES'
     A major mode symbol or a list thereof, denoting major modes
     to use this syntax checker in.

     This syntax checker will only be used in buffers whose
     `major-mode' is contained in MODES.

     If `:predicate' is also given the syntax checker will only
     be used in buffers for which the `:predicate' returns
     non-nil.

`:predicate FUNCTION'
     A function to determine whether to use the syntax checker in
     the current buffer.

     FUNCTION is called without arguments and shall return
     non-nil if this syntax checker shall be used to check the
     current buffer.  Otherwise it shall return nil.

     If this checker has a `:working-directory' FUNCTION is
     called with `default-directory' bound to the checker's
     working directory.

     FUNCTION is only called in matching major modes.

     This property is optional.

`:enabled FUNCTION'
     A function to determine whether to use the syntax checker in
     the current buffer.

     This property behaves as `:predicate', except that it's only
     called the first time a syntax checker is to be used in a buffer.

     FUNCTION is called without arguments and shall return
     non-nil if this syntax checker shall be used to check the
     current buffer.  Otherwise it shall return nil.

     If FUNCTION returns a non-nil value the checker is put in a
     whitelist in `flycheck--automatically-enabled-checkers' to
     prevent further invocations of `:enabled'.  Otherwise it is
     disabled via `flycheck--automatically-disabled-checkers' to
     prevent any further use of it.

     If this checker has a `:working-directory' FUNCTION is
     called with `default-directory' bound to the checker's
     working directory.

     FUNCTION is only called in matching major modes.

     This property is optional.

`:error-filter FUNCTION'
     A function to filter the errors returned by this checker.

     FUNCTION is called with the list of `flycheck-error' objects
     returned by the syntax checker and shall return another list
     of `flycheck-error' objects, which is considered the final
     result of this syntax checker.

     FUNCTION is free to add, remove or modify errors, whether in
     place or by copying.

     This property is optional.  The default filter is
     `identity'.

`:error-explainer FUNCTION'
     A function to return an explanation text for errors
     generated by this checker.

     FUNCTION is called with a `flycheck-error' object, in the
     buffer of that error.  It shall return an explanation
     message for the error.

     The message can take any of the following forms:
     - A string, which will be displayed to the user
     - A function (likely a closure), which will be called with
       `standard-output' set to a `flycheck-explain-error-mode'
       buffer, and should write to it.
     - A cons `(url . ,URL), indicating that the explanation can
       be found online at URL.
     - nil if there is no explanation for this error.

     If URL is provided by the checker, and cannot be composed
     from other elements in the `flycheck-error' object, consider
     passing the URL via text properties:

       ;; During the error object creation
       (put-text-property 0 1 \\='explainer-url .url .check_id)

       ;; In the error-explainer FUNCTION
       (let ((id (flycheck-error-id err)))
         (and id `(url . ,(get-text-property 0 \\='explainer-url id))))

     This property is optional.

`:next-checkers NEXT-CHECKERS'
     A list denoting syntax checkers to apply after this syntax
     checker, in what we call \"chaining\" of syntax checkers.

     Each ITEM is a cons cell `(LEVEL . CHECKER)'.  CHECKER is a
     syntax checker to run after this syntax checker.  LEVEL is
     an error level.  CHECKER will only be used if there are no
     current errors of at least LEVEL.  LEVEL may also be t, in
     which case CHECKER is used regardless of the current errors.

     ITEM may also be a syntax checker symbol, which is
     equivalent to `(t . ITEM)'.

     Flycheck tries all items in order of declaration, and uses
     the first whose LEVEL matches and whose CHECKER is
     registered and can be used for the current buffer.

     This feature is typically used to apply more than one syntax
     checker to a buffer.  For instance, you might first use a
     compiler to check a buffer for syntax and type errors, and
     then run a linting tool that checks for insecure code, or
     questionable style.

     This property is optional.  If omitted, it defaults to the
     nil, i.e. no other syntax checkers are applied after this
     syntax checker.

`:working-directory FUNCTION'
     The value of `default-directory' when invoking `:start'.

     FUNCTION is a function taking the syntax checker as sole
     argument.  It shall return the absolute path to an existing
     directory to use as `default-directory' for `:start' or
     nil to fall back to the `default-directory' of the current
     buffer.

     This property is optional.  If omitted, invoke `:start'
     from the `default-directory' of the buffer being checked.

Signal an error, if any property has an invalid value."
  (declare (indent 1)
           (doc-string 2))
  (let ((start (plist-get properties :start))
        (interrupt (plist-get properties :interrupt))
        (print-doc (plist-get properties :print-doc))
        (modes (plist-get properties :modes))
        (predicate (plist-get properties :predicate))
        (verify (plist-get properties :verify))
        (enabled (plist-get properties :enabled))
        (filter (or (plist-get properties :error-filter) #'identity))
        (explainer (plist-get properties :error-explainer))
        (next-checkers (plist-get properties :next-checkers))
        (file (flycheck-current-load-file))
        (working-directory (plist-get properties :working-directory)))

    (unless (listp modes)
      (setq modes (list modes)))

    (unless (functionp start)
      (error ":start %S of syntax checker %s is not a function" start symbol))
    (unless (or (null interrupt) (functionp interrupt))
      (error ":interrupt %S of syntax checker %s is not a function"
             interrupt symbol))
    (unless (or (null print-doc) (functionp print-doc))
      (error ":print-doc %S of syntax checker %s is not a function"
             print-doc symbol))
    (unless (or (null verify) (functionp verify))
      (error ":verify %S of syntax checker %S is not a function"
             verify symbol))
    (unless (or (null enabled) (functionp enabled))
      (error ":enabled %S of syntax checker %S is not a function"
             enabled symbol))
    (unless modes
      (error "Missing :modes in syntax checker %s" symbol))
    (dolist (mode modes)
      (unless (symbolp mode)
        (error "Invalid :modes %s in syntax checker %s, %s must be a symbol"
               modes symbol mode)))
    (unless (or (null predicate) (functionp predicate))
      (error ":predicate %S of syntax checker %s  is not a function"
             predicate symbol))
    (unless (functionp filter)
      (error ":error-filter %S of syntax checker %s is not a function"
             filter symbol))
    (unless (or (null explainer) (functionp explainer))
      (error ":error-explainer %S of syntax checker %S is not a function"
             explainer symbol))
    (dolist (checker next-checkers)
      (flycheck-validate-next-checker checker))

    (let ((real-predicate
           (and predicate
                (lambda ()
                  ;; Run predicate in the checker's default directory
                  (let ((default-directory
                          (flycheck-compute-working-directory symbol)))
                    (funcall predicate)))))
          (real-enabled
           (lambda ()
             (if (flycheck-valid-checker-p symbol)
                 (or (null enabled)
                     ;; Run enabled in the checker's default directory
                     (let ((default-directory
                             (flycheck-compute-working-directory symbol)))
                       (funcall enabled)))
               (lwarn 'flycheck
                      :warning "%S is no valid Flycheck syntax checker.
Try to reinstall the package defining this syntax checker." symbol)
               nil))))
      (pcase-dolist (`(,prop . ,value)
                     `((start             . ,start)
                       (interrupt         . ,interrupt)
                       (print-doc         . ,print-doc)
                       (modes             . ,modes)
                       (predicate         . ,real-predicate)
                       (verify            . ,verify)
                       (enabled           . ,real-enabled)
                       (error-filter      . ,filter)
                       (error-explainer   . ,explainer)
                       (next-checkers     . ,next-checkers)
                       (documentation     . ,docstring)
                       (file              . ,file)
                       (working-directory . ,working-directory)))
        (setf (flycheck-checker-get symbol prop) value)))

    ;; Track the version, to avoid breakage if the internal format changes
    (setf (flycheck-checker-get symbol 'generic-checker-version)
          flycheck-generic-checker-version)))

(defun flycheck-valid-checker-p (checker)
  "Check whether a CHECKER is valid.

A valid checker is a symbol defined as syntax checker with
`flycheck-define-checker'."
  (and (symbolp checker)
       (= (or (get checker 'flycheck-generic-checker-version) 0)
          flycheck-generic-checker-version)))

(defun flycheck-checker-supports-major-mode-p (checker &optional mode)
  "Whether CHECKER supports the given major MODE.

CHECKER is a syntax checker symbol and MODE a major mode symbol.
Look at the `modes' property of CHECKER to determine whether
CHECKER supports buffers in the given major MODE.

MODE defaults to the value of `major-mode' if omitted or nil.

Return non-nil if CHECKER supports MODE and nil otherwise."
  (let ((mode (or mode major-mode)))
    (memq mode (flycheck-checker-get checker 'modes))))

(define-obsolete-variable-alias 'flycheck-enabled-checkers
  'flycheck--automatically-enabled-checkers "32")

(defvar flycheck--automatically-enabled-checkers nil
  "Syntax checkers included in automatic selection.

A list of Flycheck syntax checkers included in automatic
selection for the current buffer.")
(make-variable-buffer-local 'flycheck--automatically-enabled-checkers)

(defun flycheck-may-enable-checker (checker)
  "Whether a generic CHECKER may be enabled for current buffer.

Return non-nil if CHECKER may be used for the current buffer, and
nil otherwise.  The result of the `:enabled' check, if any, is
cached."
  (and
   ;; May only enable valid checkers
   (flycheck-valid-checker-p checker)
   ;; Don't run the :enabled check if the checker is already disabled…
   (not (flycheck-disabled-checker-p checker))
   (or
    ;; …or if we've already cached the result
    (memq checker flycheck--automatically-enabled-checkers)
    (let* ((enabled (flycheck-checker-get checker 'enabled))
           (may-enable (or (null enabled) (funcall enabled))))
      ;; Cache the result
      (if may-enable
          (cl-pushnew checker flycheck--automatically-enabled-checkers)
        (cl-pushnew checker flycheck--automatically-disabled-checkers))
      may-enable))))

(defun flycheck-reset-enabled-checker (checker)
  "Reset the `:enabled' test of CHECKER.

Forget that CHECKER has been enabled or automatically disabled
from a previous `:enabled' test.  The result of the `:enabled'
test is cached in `flycheck-may-enable-checker': if you wish to
test the `:enabled' predicate again, you must first reset its
state using this function."
  (when (memq checker flycheck--automatically-disabled-checkers)
    (setq flycheck--automatically-disabled-checkers
          (remq checker flycheck--automatically-disabled-checkers)))
  (when (memq checker flycheck--automatically-enabled-checkers)
    (setq flycheck--automatically-enabled-checkers
          (remq checker flycheck--automatically-enabled-checkers)))
  (flycheck-buffer))

(defun flycheck-may-use-checker (checker)
  "Whether a generic CHECKER may be used.

Return non-nil if CHECKER may be used for the current buffer, and
nil otherwise."
  (let ((predicate (flycheck-checker-get checker 'predicate)))
    (and (flycheck-valid-checker-p checker)
         (flycheck-checker-supports-major-mode-p checker)
         (flycheck-may-enable-checker checker)
         (or (null predicate) (funcall predicate)))))

(defun flycheck-may-use-next-checker (next-checker)
  "Determine whether NEXT-CHECKER may be used."
  (when (symbolp next-checker)
    (push t next-checker))
  (let ((level (car next-checker))
        (next-checker (cdr next-checker)))
    (and (or (eq level t)
             (flycheck-has-max-current-errors-p level))
         (flycheck-registered-checker-p next-checker)
         (flycheck-may-use-checker next-checker))))


;;; Help for generic syntax checkers
(define-button-type 'help-flycheck-checker-def
  :supertype 'help-xref
  'help-function #'flycheck-goto-checker-definition
  'help-echo "mouse-1, RET: find Flycheck checker definition")

(defconst flycheck-find-checker-regexp
  (rx line-start (zero-or-more (syntax whitespace))
      "(" symbol-start
      (or "flycheck-define-checker" "flycheck-define-command-checker")
      symbol-end
      (eval (list 'regexp find-function-space-re))
      (? "'")
      symbol-start "%s" symbol-end
      (or (syntax whitespace) line-end))
  "Regular expression to find a checker definition.")

(add-to-list 'find-function-regexp-alist
             '(flycheck-checker . flycheck-find-checker-regexp))

(defun flycheck-goto-checker-definition (checker file)
  "Go to to the definition of CHECKER in FILE."
  (let ((location (find-function-search-for-symbol
                   checker 'flycheck-checker file)))
    (pop-to-buffer (car location))
    (if (cdr location)
        (goto-char (cdr location))
      (message "Unable to find checker location in file"))))

(defun flycheck-checker-at-point ()
  "Return the Flycheck checker found at or before point.

Return nil if there is no checker."
  (let ((symbol (variable-at-point 'any-symbol)))
    (when (flycheck-valid-checker-p symbol)
      symbol)))

(defun flycheck-describe-checker (checker)
  "Display the documentation of CHECKER.

CHECKER is a checker symbol.

Pop up a help buffer with the documentation of CHECKER."
  (interactive
   (let* ((enable-recursive-minibuffers t)
          (default (or (flycheck-checker-at-point)
                       (ignore-errors (flycheck-get-checker-for-buffer))))
          (prompt (if default
                      (format "Describe syntax checker (default %s): " default)
                    "Describe syntax checker: ")))
     (list (flycheck-read-checker prompt default))))
  (unless (flycheck-valid-checker-p checker)
    (user-error "You didn't specify a Flycheck syntax checker"))
  (let ((filename (flycheck-checker-get checker 'file))
        (modes (flycheck-checker-get checker 'modes))
        (predicate (flycheck-checker-get checker 'predicate))
        (print-doc (flycheck-checker-get checker 'print-doc))
        (next-checkers (flycheck-checker-get checker 'next-checkers))
        (help-xref-following
         ;; Ensure that we don't reuse buffers like `flycheck-verify-checker',
         ;; and that we don't error out if a `help-flycheck-checker-doc' button
         ;; is added outside of a documentation window.
         (and help-xref-following (eq major-mode 'help-mode))))
    (help-setup-xref (list #'flycheck-describe-checker checker)
                     (called-interactively-p 'interactive))
    (save-excursion
      (with-help-window (help-buffer)
        (princ (format "%s is a Flycheck syntax checker" checker))
        (when filename
          (princ (format " in `%s'" (file-name-nondirectory filename)))
          (with-current-buffer standard-output
            (save-excursion
              (re-search-backward "`\\([^`']+\\)'" nil t)
              (help-xref-button 1 'help-flycheck-checker-def
                                checker filename))))
        (princ ".\n\n")

        (let ((modes-start (with-current-buffer standard-output (point-max))))
          ;; Track the start of the modes documentation, to properly re-fill
          ;; it later
          (princ "  This syntax checker checks syntax in the major mode(s) ")
          (princ (string-join
                  (seq-map (apply-partially #'format "`%s'") modes)
                  ", "))
          (when predicate
            (princ ", and uses a custom predicate"))
          (princ ".")
          (when next-checkers
            (princ "  It runs the following checkers afterwards:"))
          (with-current-buffer standard-output
            (save-excursion
              (fill-region-as-paragraph modes-start (point-max))))
          (princ "\n")

          ;; Print the list of next checkers
          (when next-checkers
            (princ "\n")
            (let ((beg-checker-list (with-current-buffer standard-output
                                      (point))))
              (dolist (next-checker next-checkers)
                (if (symbolp next-checker)
                    (princ (format "     * `%s'\n" next-checker))
                  (princ (format "     * `%s' (maximum level `%s')\n"
                                 (cdr next-checker) (car next-checker)))))
              ;;
              (with-current-buffer standard-output
                (save-excursion
                  (while (re-search-backward "`\\([^`']+\\)'"
                                             beg-checker-list t)
                    (let ((checker (intern-soft (match-string 1))))
                      (when (flycheck-valid-checker-p checker)
                        (help-xref-button 1 'help-flycheck-checker-doc
                                          checker)))))))))
        ;; Call the custom print-doc function of the checker, if present
        (when print-doc
          (funcall print-doc checker))
        ;; Ultimately, print the docstring
        (princ "\nDocumentation:\n")
        (princ (flycheck-checker-get checker 'documentation))))))


;;; Syntax checker verification
(cl-defstruct (flycheck-verification-result
               (:constructor flycheck-verification-result-new))
  "Structure for storing a single verification result.

Slots:

`label'
     A label for this result, as string

`message'
     A message for this result, as string

`face'
     The face to use for the `message'.

     You can either use a face symbol, or a list of face symbols."
  label message face)

(defun flycheck-verify-generic-checker (checker)
  "Verify a generic CHECKER in the current buffer.

Return a list of `flycheck-verification-result' objects."
  (let (results
        (predicate (flycheck-checker-get checker 'predicate))
        (enabled (flycheck-checker-get checker 'enabled))
        (verify (flycheck-checker-get checker 'verify)))
    (when enabled
      (let ((result (funcall enabled)))
        (push (flycheck-verification-result-new
               :label (propertize "may enable" 'help-echo ":enable")
               :message (if result "yes" "no")
               :face (if result 'success '(bold warning)))
              results)))
    (when predicate
      (let ((result (funcall predicate)))
        (push (flycheck-verification-result-new
               :label (propertize "may run" 'help-echo ":predicate")
               :message (prin1-to-string (not (null result)))
               :face (if result 'success '(bold warning)))
              results)))
    (append (nreverse results)
            (and verify (funcall verify checker)))))

(define-button-type 'help-flycheck-checker-doc
  :supertype 'help-xref
  'help-function #'flycheck-describe-checker
  'help-echo "mouse-1, RET: describe Flycheck checker")

(define-button-type 'flycheck-button
  'follow-link t
  'action (lambda (pos)
            (apply (get-text-property pos 'flycheck-action)
                   (get-text-property pos 'flycheck-data))
            ;; Revert the verify-setup buffer since it is now stale
            (revert-buffer))
  'face 'flycheck-verify-select-checker)

(define-button-type 'flycheck-checker-select
  :supertype 'flycheck-button
  'flycheck-action (lambda (buffer checker)
                     (with-current-buffer buffer
                       (flycheck-select-checker checker)))
  'help-echo "mouse-1, RET: select this checker")

(define-button-type 'flycheck-checker-enable
  :supertype 'flycheck-button
  'flycheck-action (lambda (buffer checker)
                     (interactive)
                     (with-current-buffer buffer
                       (flycheck--toggle-checker checker t)
                       (flycheck-buffer)))
  'help-echo "mouse-1, RET: re-enable this checker in this buffer")

(define-button-type 'flycheck-checker-reset-enabled
  :supertype 'flycheck-button
  'flycheck-action (lambda (buffer checker)
                     (with-current-buffer buffer
                       (flycheck-reset-enabled-checker checker)))
  'help-echo "mouse-1, RET: try to re-enable this checker")

(defun flycheck--verify-princ-checker (checker buffer
                                               &optional with-mm with-select)
  "Print verification result of CHECKER for BUFFER.

When WITH-MM is given and non-nil, also include the major mode
into the verification results.

When WITH-SELECT is non-nil, add a button to select this checker."
  (princ "  ")
  (insert-button (symbol-name checker)
                 'type 'help-flycheck-checker-doc
                 'help-args (list checker))
  (cond
   ((with-current-buffer buffer
      (flycheck-manually-disabled-checker-p checker))
    (insert (propertize " (manually disabled) " 'face '(bold error)))
    (insert-text-button "enable"
                        'type 'flycheck-checker-enable
                        'flycheck-data (list buffer checker)))
   ((with-current-buffer buffer
      (flycheck-automatically-disabled-checker-p checker))
    (insert (propertize " (automatically disabled) " 'face '(bold error)))
    (insert-text-button "reset"
                        'type 'flycheck-checker-reset-enabled
                        'flycheck-data (list buffer checker))))
  (when (eq checker (buffer-local-value 'flycheck-checker buffer))
    (insert (propertize " (explicitly selected)" 'face 'bold)))
  (when with-select
    (princ "  ")
    (insert-text-button "select"
                        'type 'flycheck-checker-select
                        'flycheck-data (list buffer checker)))
  (princ "\n")
  (let ((results (with-current-buffer buffer
                   (append (flycheck-verify-generic-checker checker)
                           (flycheck--verify-next-checkers checker)))))
    (when with-mm
      (with-current-buffer buffer
        (let ((message-and-face
               (if (flycheck-checker-supports-major-mode-p checker)
                   (cons (format "`%s' supported" major-mode) 'success)
                 (cons (format "`%s' not supported" major-mode) 'error))))
          (push (flycheck-verification-result-new
                 :label "major mode"
                 :message (car message-and-face)
                 :face (cdr message-and-face))
                results))))
    (let* ((label-length
            (seq-max (mapcar
                      (lambda (res)
                        (length (flycheck-verification-result-label res)))
                      results)))
           (message-column (+ 8 label-length)))
      (dolist (result results)
        (princ "    - ")
        (princ (flycheck-verification-result-label result))
        (princ ": ")
        (princ (make-string (- message-column (current-column)) ?\ ))
        (let ((message (flycheck-verification-result-message result))
              (face (flycheck-verification-result-face result)))
          ;; If face is nil, using propertize erases the face already contained
          ;; by the message.  We don't want that, since this would remove the
          ;; button face from the checker chain result.
          (insert (if face (propertize message 'face face) message)))
        (princ "\n"))))
  (princ "\n"))

(defun flycheck--get-next-checker-symbol (next)
  "Get the checker symmbol of NEXT checker.

NEXT should be either a cons (NEXT-CHECKER . LEVEL) or a
symbol."
  (if (consp next) (cdr next) next))

(defun flycheck-get-next-checkers (checker)
  "Return the immediate next checkers of CHECKER.

This is a list of checker symbols.  The error levels of the
`:next-checker' property are ignored."
  (mapcar #'flycheck--get-next-checker-symbol
          (flycheck-checker-get checker 'next-checkers)))

(defun flycheck-all-next-checkers (checker)
  "Return all checkers that may follow CHECKER.

Return the transitive closure of the next-checker relation.  The
return value is a list of checkers, not including CHECKER."
  (let ((next-checkers)
        (visited)
        (queue (list checker)))
    (while queue
      (let ((c (pop queue)))
        (push c visited)
        (dolist (n (flycheck-get-next-checkers c))
          (push n next-checkers)
          (unless (memq n visited)
            (cl-pushnew n queue)))))
    (seq-uniq next-checkers)))

(defun flycheck--verify-next-checkers (checker)
  "Return a verification result for the next checkers of CHECKER."
  (-when-let (next (flycheck-get-next-checkers checker))
    (list
     (flycheck-verification-result-new
      :label "next checkers"
      ;; We use `make-text-button' to preserve the button properties in the
      ;; string
      :message (mapconcat
                (lambda (checker)
                  (make-text-button (symbol-name checker) nil
                                    'type 'help-flycheck-checker-doc
                                    'help-args (list checker)))
                next
                ", ")))))

(defun flycheck--verify-print-header (desc buffer)
  "Print a title with DESC for BUFFER in the current buffer.

DESC is an arbitrary string containing a description, and BUFFER
is the buffer being verified.  The name and the major mode mode
of BUFFER are printed.

DESC and information about BUFFER are printed in the current
buffer."
  (princ desc)
  (insert (propertize (buffer-name buffer) 'face 'bold))
  (princ " in ")
  (let ((mode (buffer-local-value 'major-mode buffer)))
    (insert-button (symbol-name mode)
                   'type 'help-function
                   'help-args (list mode)))
  (princ ":\n\n"))

(defun flycheck--verify-print-footer (buffer)
  "Print a footer for BUFFER in the current buffer.

BUFFER is the buffer being verified."
  (princ "Flycheck Mode is ")
  (let ((enabled (buffer-local-value 'flycheck-mode buffer)))
    (insert (propertize (if enabled "enabled" "disabled")
                        'face (if enabled 'success '(warning bold)))))
  (princ
   (with-current-buffer buffer
     ;; Use key binding state in the verified buffer to print the help.
     (substitute-command-keys
      ".  Use \\[universal-argument] \\[flycheck-disable-checker] \
to enable disabled checkers.")))
  (save-excursion
    (let ((end (point)))
      (backward-paragraph)
      (fill-region-as-paragraph (point) end)))

  (princ "\n\n--------------------\n\n")
  (princ (format "Flycheck version: %s\n" (flycheck-version)))
  (princ (format "Emacs version:    %s\n" emacs-version))
  (princ (format "System:           %s\n" system-configuration))
  (princ (format "Window system:    %S\n" window-system)))

(define-derived-mode flycheck-verify-mode help-mode
  "Flycheck verification"
  "Major mode to display Flycheck verification results."
  ;; `help-mode-finish' will restore `buffer-read-only'
  (setq buffer-read-only nil))

(defun flycheck-verify-checker (checker)
  "Check whether a CHECKER can be used in this buffer.

Show a buffer listing possible problems that prevent CHECKER from
being used for the current buffer.

Note: Do not use this function to check whether a syntax checker
is applicable from Emacs Lisp code.  Use
`flycheck-may-use-checker' instead."
  (interactive (list (flycheck-read-checker "Checker to verify: ")))
  (unless (flycheck-valid-checker-p checker)
    (user-error "%s is not a syntax checker" checker))

  ;; Save the buffer to make sure that all predicates are good
  ;; FIXME: this may be surprising to users, with unintended side-effects.
  (when (and (buffer-file-name) (buffer-modified-p))
    (save-buffer))

  (let ((buffer (current-buffer)))
    (with-help-window "*Flycheck checker*"
      (with-current-buffer standard-output
        (flycheck-verify-mode)
        (flycheck--verify-print-header "Syntax checker in buffer " buffer)
        (flycheck--verify-princ-checker checker buffer 'with-mm)
        (if (with-current-buffer buffer (flycheck-may-use-checker checker))
            (insert (propertize
                     "Flycheck can use this syntax checker for this buffer.\n"
                     'face 'success))
          (insert (propertize
                   "Flycheck cannot use this syntax checker for this buffer.\n"
                   'face 'error)))
        (insert "\n")
        (flycheck--verify-print-footer buffer)))))

(defun flycheck-verify-setup ()
  "Check whether Flycheck can be used in this buffer.

Display a new buffer listing all syntax checkers that could be
applicable in the current buffer.  For each syntax checkers,
possible problems are shown."
  (interactive)
  ;; Save to make sure checkers that only work on saved buffers will pass the
  ;; verification
  (when (and (buffer-file-name) (buffer-modified-p))
    (save-buffer))

  (let* ((buffer (current-buffer))
         (first-checker (flycheck-get-checker-for-buffer))
         (valid-checkers
          (remq first-checker
                (seq-filter #'flycheck-may-use-checker flycheck-checkers)))
         (valid-next-checkers
          (when first-checker
            (seq-intersection valid-checkers
                              (flycheck-all-next-checkers first-checker))))
         (valid-remaining (seq-difference valid-checkers valid-next-checkers))
         (other-checkers
          (seq-difference (seq-filter #'flycheck-checker-supports-major-mode-p
                                      flycheck-checkers)
                          (cons first-checker valid-checkers))))

    ;; Print all applicable checkers for this buffer
    (with-help-window "*Flycheck checkers*"
      (with-current-buffer standard-output
        (flycheck-verify-mode)

        (flycheck--verify-print-header "Syntax checkers for buffer " buffer)

        (if first-checker
            (progn
              (princ "First checker to run:\n\n")
              (flycheck--verify-princ-checker first-checker buffer))
          (insert (propertize
                   "No checker to run in this buffer.\n\n"
                   'face '(bold error))))

        (when valid-next-checkers
          (princ
           "Checkers that may run as part of the first checker's chain:\n\n")
          (dolist (checker valid-next-checkers)
            (flycheck--verify-princ-checker checker buffer)))

        (when valid-remaining
          (princ "Checkers that could run if selected:\n\n")
          (dolist (checker valid-remaining)
            (flycheck--verify-princ-checker checker buffer nil 'with-select)))

        (when other-checkers
          (princ
           "Checkers that are compatible with this mode, \
but will not run until properly configured:\n\n")
          (dolist (checker other-checkers)
            (flycheck--verify-princ-checker checker buffer)))

        ;; If we have no checkers at all, that's worth mentioning
        (unless (or first-checker valid-checkers other-checkers)
          (insert (propertize
                   "No checkers are available for this buffer.\n\n"
                   'face '(bold error))))

        (let ((unregistered-checkers
               (seq-difference (flycheck-defined-checkers) flycheck-checkers)))
          (when unregistered-checkers
            (insert (propertize
                     "The following syntax checkers are not registered:\n"
                     'face '(bold warning)))
            (dolist (checker unregistered-checkers)
              (princ "  - ")
              (princ checker)
              (princ "\n"))
            (princ
             "Try adding these syntax checkers to `flycheck-checkers'.\n\n")))

        (flycheck--verify-print-footer buffer)

        (setq-local revert-buffer-function
                    (lambda (_ignore-auto _noconfirm)
                      (with-current-buffer buffer (flycheck-verify-setup))))))))


;;; Predicates for generic syntax checkers
(defun flycheck-buffer-saved-p (&optional buffer)
  "Determine whether BUFFER is saved to a file.

BUFFER is the buffer to check.  If omitted or nil, use the
current buffer as BUFFER.

Return non-nil if the BUFFER is backed by a file, and not
modified, or nil otherwise."
  (let ((file-name (buffer-file-name buffer)))
    (and file-name (file-exists-p file-name) (not (buffer-modified-p buffer)))))


;;; Extending generic checkers
(defun flycheck-remove-next-checker (checker next)
  "After CHECKER remove a NEXT checker.

CHECKER is a syntax checker symbol, from which to remove NEXT
checker.

NEXT is a cons or a symbol, as documented in
`flycheck-add-next-checker'."
  (unless (flycheck-valid-checker-p checker)
    (error "%s is not a valid syntax checker" checker))
  (let* ((next-symbol (flycheck--get-next-checker-symbol next)))
    (setf
     (flycheck-checker-get checker 'next-checkers)
     (seq-remove
      (lambda (next) (eq (flycheck--get-next-checker-symbol next) next-symbol))
      (flycheck-checker-get checker 'next-checkers)))))

(defun flycheck-add-next-checker (checker next &optional append)
  "After CHECKER add a NEXT checker.

CHECKER is a syntax checker symbol, to which to add NEXT checker.

NEXT is a cons cell `(LEVEL . NEXT-CHECKER)'.  NEXT-CHECKER is a
symbol denoting the syntax checker to run after CHECKER.  LEVEL
is an error level.  NEXT-CHECKER will only be used if there is no
current error whose level is more severe than LEVEL.  LEVEL may
also be t, in which case NEXT-CHECKER is used regardless of the
current errors.

NEXT can also be a syntax checker symbol only, which is
equivalent to `(t . NEXT)'.

NEXT-CHECKER is prepended before other next checkers, unless
APPEND is non-nil."
  (unless (flycheck-valid-checker-p checker)
    (error "%s is not a valid syntax checker" checker))
  (flycheck-validate-next-checker next 'strict)
  (flycheck-remove-next-checker checker next)
  (let ((next-checkers (flycheck-checker-get checker 'next-checkers)))
    (setf (flycheck-checker-get checker 'next-checkers)
          (if append (append next-checkers (list next))
            (cons next next-checkers)))))

(defun flycheck-add-mode (checker mode)
  "To CHECKER add a new major MODE.

CHECKER and MODE are symbols denoting a syntax checker and a
major mode respectively.

Add MODE to the `:modes' property of CHECKER, so that CHECKER
will be used in buffers with MODE."
  (unless (flycheck-valid-checker-p checker)
    (error "%s is not a valid syntax checker" checker))
  (unless (symbolp mode)
    (error "%s is not a symbol" mode))
  (push mode (flycheck-checker-get checker 'modes)))


;;; Generic syntax checks
(cl-defstruct (flycheck-syntax-check
               (:constructor flycheck-syntax-check-new))
  "Structure for storing syntax check state.

Slots:

`buffer'
     The buffer being checked.

`checker'
     The syntax checker being used.

`context'
     The context object.

`working-directory'
     Working directory for the syntax checker. Serve as a value for
     `default-directory' for a checker."
  buffer checker context working-directory)

(defun flycheck-syntax-check-start (syntax-check callback)
  "Start a SYNTAX-CHECK with CALLBACK."
  (let ((checker (flycheck-syntax-check-checker syntax-check))
        (default-directory
          (flycheck-syntax-check-working-directory syntax-check)))
    (setf (flycheck-syntax-check-context syntax-check)
          (funcall (flycheck-checker-get checker 'start) checker callback))))

(defun flycheck-syntax-check-interrupt (syntax-check)
  "Interrupt a SYNTAX-CHECK."
  (let* ((checker (flycheck-syntax-check-checker syntax-check))
         (interrupt-fn (flycheck-checker-get checker 'interrupt))
         (context (flycheck-syntax-check-context syntax-check)))
    (when interrupt-fn
      (funcall interrupt-fn checker context))))


;;; Syntax checking mode

(defvar flycheck-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map flycheck-keymap-prefix flycheck-command-map)
    ;; We place the menu under a custom menu key.  Since this menu key is not
    ;; present in the menu of the global map, no top-level menu entry is added
    ;; to the global menu bar.  However, it still appears on the mode line
    ;; lighter.
    (define-key map [menu-bar flycheck] flycheck-mode-menu-map)
    map)
  "Keymap of command `flycheck-mode'.")

(defvar-local flycheck-old-next-error-function nil
  "Remember the old `next-error-function'.")

(defconst flycheck-hooks-alist
  '(
    ;; Handle events that may start automatic syntax checks
    (after-save-hook        . flycheck-handle-save)
    (after-change-functions . flycheck-handle-change)
    ;; Handle events that may triggered pending deferred checks
    (window-configuration-change-hook . flycheck-perform-deferred-syntax-check)
    (post-command-hook                . flycheck-perform-deferred-syntax-check)
    ;; Teardown Flycheck whenever the buffer state is about to get lost, to
    ;; clean up temporary files and directories.
    (kill-buffer-hook       . flycheck-teardown)
    (change-major-mode-hook . flycheck-teardown)
    (before-revert-hook     . flycheck-teardown)
    ;; Update the error list if necessary
    (post-command-hook . flycheck-error-list-update-source)
    (post-command-hook . flycheck-error-list-highlight-errors)
    ;; Display errors.  Show errors at point after commands (like movements) and
    ;; when Emacs gets focus.  Cancel the display timer when Emacs looses focus
    ;; (as there's no need to display errors if the user can't see them), and
    ;; hide the error buffer (for large error messages) if necessary.  Note that
    ;; the focus hooks only work on Emacs 24.4 and upwards, but since undefined
    ;; hooks are perfectly ok we don't need a version guard here.  They'll just
    ;; not work silently.
    (post-command-hook . flycheck-maybe-display-error-at-point-soon)
    (focus-in-hook     . flycheck-display-error-at-point-soon)
    (focus-out-hook    . flycheck-cancel-error-display-error-at-point-timer)
    (post-command-hook . flycheck-hide-error-buffer)
    ;; Immediately show error popups when navigating to an error
    (next-error-hook . flycheck-display-error-at-point))
  "Hooks which Flycheck needs to hook in.

The `car' of each pair is a hook variable, the `cdr' a function
to be added or removed from the hook variable if Flycheck mode is
enabled and disabled respectively.")

;;;###autoload
(define-minor-mode flycheck-mode
  "Flycheck is a minor mode for on-the-fly syntax checking.

In `flycheck-mode' the buffer is automatically syntax-checked
using the first suitable syntax checker from `flycheck-checkers'.
Use `flycheck-select-checker' to select a checker for the current
buffer manually.

If you run into issues, use `\\[flycheck-verify-setup]' to get help.

Flycheck supports many languages out of the box, and many
additional ones are available on MELPA.  Adding new ones is very
easy.  Complete documentation is available online at URL
`https://www.flycheck.org/en/latest/'.  Please report issues and
request features at URL `https://github.com/flycheck/flycheck'.

Flycheck displays its status in the mode line.  In the default
configuration, it looks like this:

`FlyC'     This buffer has not been checked yet.
`FlyC-'    Flycheck doesn't have a checker for this buffer.
`FlyC*'    Flycheck is running.  Expect results soon!
`FlyC:3|2' This buffer contains three warnings and two errors.
           Use `\\[flycheck-list-errors]' to see the list.

You may also see the following icons:
`FlyC!'    The checker crashed.
`FlyC.'    The last syntax check was manually interrupted.
`FlyC?'    The checker did something unexpected, like exiting with 1
           but returning no errors.

The following keybindings are available in `flycheck-mode':

\\{flycheck-mode-map}
\(you can change the prefix by customizing
`flycheck-keymap-prefix')

If called interactively, enable Flycheck mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is ‘toggle’; disable the mode otherwise."
  :init-value nil
  :keymap flycheck-mode-map
  :lighter flycheck-mode-line
  :after-hook (flycheck-buffer-automatically 'mode-enabled 'force-deferred)
  (cond
   (flycheck-mode
    (flycheck-clear)

    (pcase-dolist (`(,hook . ,fn) (reverse flycheck-hooks-alist))
      (add-hook hook fn nil 'local))

    (setq flycheck-old-next-error-function
          (if flycheck-standard-error-navigation
              next-error-function
            :unset))
    (when flycheck-standard-error-navigation
      (setq next-error-function #'flycheck-next-error-function))

    ;; This hook must be added globally since otherwise we cannot
    ;; detect a change from a buffer where Flycheck is enabled to a
    ;; buffer where Flycheck is not enabled, and therefore cannot
    ;; notice that there has been any change when the user switches
    ;; back to the buffer where Flycheck is enabled.
    (add-hook 'buffer-list-update-hook #'flycheck-handle-buffer-switch))
   (t
    (unless (eq flycheck-old-next-error-function :unset)
      (setq next-error-function flycheck-old-next-error-function))

    (pcase-dolist (`(,hook . ,fn) flycheck-hooks-alist)
      (remove-hook hook fn 'local))

    (flycheck-teardown))))


;;; Syntax checker selection for the current buffer
(defun flycheck-get-checker-for-buffer ()
  "Find the checker for the current buffer.

Use the selected checker for the current buffer, if any,
otherwise search for the best checker from `flycheck-checkers'.

Return checker if there is a checker for the current buffer, or
nil otherwise."
  (if flycheck-checker
      (when (flycheck-may-use-checker flycheck-checker)
        flycheck-checker)
    (seq-find #'flycheck-may-use-checker flycheck-checkers)))

(defun flycheck-get-next-checker-for-buffer (checker)
  "Get the checker to run after CHECKER for the current buffer."
  (let ((next (seq-find #'flycheck-may-use-next-checker
                        (flycheck-checker-get checker 'next-checkers))))
    (when next
      (if (symbolp next) next (cdr next)))))

(defun flycheck-select-checker (checker)
  "Select CHECKER for the current buffer.

CHECKER is a syntax checker symbol (see `flycheck-checkers') or
nil.  In the former case, use CHECKER for the current buffer,
otherwise deselect the current syntax checker (if any) and use
automatic checker selection via `flycheck-checkers'.

If called interactively prompt for CHECKER.  With prefix arg
deselect the current syntax checker and enable automatic
selection again.

Set `flycheck-checker' to CHECKER and automatically start a new
syntax check if the syntax checker changed.

CHECKER will be used, even if it is not contained in
`flycheck-checkers', or if it is disabled via
`flycheck-disabled-checkers'."
  (interactive
   (if current-prefix-arg
       (list nil)
     (list (flycheck-read-checker "Select checker: "
                                  (flycheck-get-checker-for-buffer)))))
  (when (not (eq checker flycheck-checker))
    (unless (or (not checker) (flycheck-may-use-checker checker))
      (flycheck-verify-checker checker)
      (user-error "Can't use syntax checker %S in this buffer" checker))
    (setq flycheck-checker checker)
    (when flycheck-mode
      (flycheck-buffer))))

(defun flycheck--toggle-checker (checker enable)
  "Enable or disable CHECKER for the current buffer.

If ENABLE, re-enable CHECKER by removing it from the buffer-local
value of `flycheck-disabled-checkers'.  Otherwise, add the syntax
checker to the buffer-local value of `flycheck-disabled-checkers'."
  (cond
   (enable
    ;; We must use `remq' instead of `delq', because we must _not_ modify the
    ;; list.  Otherwise we could potentially modify the global default value,
    ;; in case the list is the global default.
    (when (memq checker flycheck-disabled-checkers)
      (setq flycheck-disabled-checkers
            (remq checker flycheck-disabled-checkers)))
    (when (memq checker flycheck--automatically-disabled-checkers)
      (setq flycheck--automatically-disabled-checkers
            (remq checker flycheck--automatically-disabled-checkers))))
   (t (unless (memq checker flycheck-disabled-checkers)
        (push checker flycheck-disabled-checkers)))))

(defun flycheck-disable-checker (checker &optional enable)
  "Interactively disable CHECKER for the current buffer.

Prompt for a syntax checker to disable, and add the syntax
checker to the buffer-local value of
`flycheck-disabled-checkers'.

With non-nil ENABLE or with prefix arg, prompt for a disabled
syntax checker and re-enable it by removing it from the
buffer-local value of `flycheck-disabled-checkers'."
  (declare
   (interactive-only "Directly set `flycheck-disabled-checkers' instead"))
  (interactive
   (let* ((enable current-prefix-arg)
          (candidates (if enable
                          (append flycheck-disabled-checkers
                                  flycheck--automatically-disabled-checkers)
                        flycheck-checkers))
          (prompt (if enable "Enable syntax checker: "
                    "Disable syntax checker: ")))
     (when (and enable (not candidates))
       (user-error "No syntax checkers disabled in this buffer"))
     (list (flycheck-read-checker prompt nil nil candidates) enable)))
  (unless checker
    (user-error "No syntax checker given"))
  (flycheck--toggle-checker checker enable)
  (flycheck-buffer))


;;; Syntax checks for the current buffer
(defvar-local flycheck-current-syntax-check nil
  "The current syntax check in the this buffer.")
(put 'flycheck-current-syntax-check 'permanent-local t)

(defun flycheck-start-current-syntax-check (checker)
  "Start a syntax check in the current buffer with CHECKER.

Set `flycheck-current-syntax-check' accordingly."
  ;; Allocate the current syntax check *before* starting it.  This allows for
  ;; synchronous checks, which call the status callback immediately in their
  ;; start function.
  (let* ((check
          (flycheck-syntax-check-new
           :buffer (current-buffer)
           :checker checker
           :context nil
           :working-directory (flycheck-compute-working-directory checker)))
         (callback (flycheck-buffer-status-callback check)))
    (setq flycheck-current-syntax-check check)
    (flycheck-report-status 'running)
    (flycheck-syntax-check-start check callback)))

(defun flycheck-running-p ()
  "Determine whether a syntax check is running in the current buffer."
  (not (null flycheck-current-syntax-check)))

(defun flycheck-stop ()
  "Stop any ongoing syntax check in the current buffer."
  (when (flycheck-running-p)
    (flycheck-syntax-check-interrupt flycheck-current-syntax-check)
    ;; Remove the current syntax check, to reset Flycheck into a non-running
    ;; state, and to make `flycheck-report-buffer-checker-status' ignore any
    ;; status reports from the current syntax check.
    (setq flycheck-current-syntax-check nil)
    (flycheck-report-status 'interrupted)))

(defun flycheck-buffer-status-callback (syntax-check)
  "Create a status callback for SYNTAX-CHECK in the current buffer."
  (lambda (&rest args)
    (apply #'flycheck-report-buffer-checker-status
           syntax-check args)))

(defun flycheck-buffer ()
  "Start checking syntax in the current buffer.

Get a syntax checker for the current buffer with
`flycheck-get-checker-for-buffer', and start it."
  (interactive)
  (flycheck-clean-deferred-check)
  (if flycheck-mode
      (unless (flycheck-running-p)
        ;; Clear error list and mark all overlays for deletion.  We do not
        ;; delete all overlays immediately to avoid excessive re-displays and
        ;; flickering, if the same errors gets highlighted again after the check
        ;; completed.
        (run-hooks 'flycheck-before-syntax-check-hook)
        (flycheck-clear-errors)
        (flycheck-mark-all-overlays-for-deletion)
        (condition-case err
            (let* ((checker (flycheck-get-checker-for-buffer)))
              (if checker
                  (flycheck-start-current-syntax-check checker)
                (flycheck-clear)
                (flycheck-report-status 'no-checker)))
          (error
           (flycheck-report-failed-syntax-check)
           (signal (car err) (cdr err)))))
    (user-error "Flycheck mode disabled")))

(defun flycheck-report-buffer-checker-status
    (syntax-check status &optional data)
  "In BUFFER, report a SYNTAX-CHECK STATUS with DATA.

SYNTAX-CHECK is the `flycheck-syntax-check' which reported
STATUS.  STATUS denotes the status of CHECKER, with an optional
DATA.  STATUS may be one of the following symbols:

`errored'
     The syntax checker has errored.  DATA is an optional error
     message.

     This report finishes the current syntax check.

`interrupted'
     The syntax checker was interrupted.  DATA is ignored.

     This report finishes the current syntax check.

`finished'
     The syntax checker has finished with a proper error report
     for the current buffer.  DATA is the (potentially empty)
     list of `flycheck-error' objects reported by the syntax
     check.

     This report finishes the current syntax check.

`suspicious'
     The syntax checker encountered a suspicious state, which the
     user needs to be informed about.  DATA is an optional
     message.

A syntax checker _must_ report a status at least once with any
symbol that finishes the current syntax checker.  Otherwise
Flycheck gets stuck with the current syntax check.

If CHECKER is not the currently used syntax checker in
`flycheck-current-syntax-check', the status report is largely
ignored.  Notably, any errors reported by the checker are
discarded."
  (let ((buffer (flycheck-syntax-check-buffer syntax-check)))
    ;; Ignore the status report if the buffer is gone, or if this syntax check
    ;; isn't the current one in buffer (which can happen if this is an old
    ;; report of an interrupted syntax check, and a new syntax check was started
    ;; since this check was interrupted)
    (when (and (buffer-live-p buffer)
               (eq syntax-check
                   (buffer-local-value 'flycheck-current-syntax-check buffer)))
      (with-current-buffer buffer
        (let ((checker (flycheck-syntax-check-checker syntax-check)))
          (pcase status
            ((or `errored `interrupted)
             (flycheck-report-failed-syntax-check status)
             (when (eq status 'errored)
               ;; In case of error, show the error message
               (message "Error from syntax checker %s: %s"
                        checker (or data "UNKNOWN!"))))
            (`suspicious
             (when flycheck-mode
               (message "Suspicious state from syntax checker %s: %s"
                        checker (or data "UNKNOWN!")))
             (flycheck-report-status 'suspicious))
            (`finished
             (when flycheck-mode
               ;; Only report errors from the checker if Flycheck Mode is
               ;; still enabled.
               (flycheck-finish-current-syntax-check
                data
                (flycheck-syntax-check-working-directory syntax-check))))
            (_
             (error "Unknown status %s from syntax checker %s"
                    status checker))))))))

(defun flycheck-finish-current-syntax-check (errors working-dir)
  "Finish the current syntax-check in the current buffer with ERRORS.

ERRORS is a list of `flycheck-error' objects reported by the
current syntax check in `flycheck-current-syntax-check'.

Report all ERRORS and potentially start any next syntax checkers.

If the current syntax checker reported excessive errors, it is
disabled via `flycheck-disable-excessive-checker' for subsequent
syntax checks.

Relative file names in ERRORS will be expanded relative to
WORKING-DIR."
  (let* ((syntax-check flycheck-current-syntax-check)
         (checker (flycheck-syntax-check-checker syntax-check))
         (errors (flycheck-relevant-errors
                  (flycheck-fill-and-expand-error-file-names
                   (flycheck-filter-errors
                    (flycheck-assert-error-list-p errors) checker)
                   working-dir))))
    (unless (flycheck-disable-excessive-checker checker errors)
      (flycheck-report-current-errors errors))
    (let ((next-checker (flycheck-get-next-checker-for-buffer checker)))
      (if next-checker
          (flycheck-start-current-syntax-check next-checker)
        (setq flycheck-current-syntax-check nil)
        (flycheck-report-status 'finished)
        ;; Delete overlays only after the very last checker has run, to avoid
        ;; flickering on intermediate re-displays
        (flycheck-delete-marked-overlays)
        (flycheck-error-list-refresh)
        (run-hooks 'flycheck-after-syntax-check-hook)
        (when (eq (current-buffer) (window-buffer))
          (flycheck-display-error-at-point))
        ;; Immediately try to run any pending deferred syntax check, which
        ;; were triggered by intermediate automatic check event, to make sure
        ;; that we quickly refine outdated error information
        (flycheck-perform-deferred-syntax-check)))))

(defun flycheck-disable-excessive-checker (checker errors)
  "Disable CHECKER if it reported excessive ERRORS.

If ERRORS has more items than `flycheck-checker-error-threshold',
add CHECKER to `flycheck--automatically-disabled-checkers', and
show a warning.

Return t when CHECKER was disabled, or nil otherwise."
  (when (and flycheck-checker-error-threshold
             (> (length errors) flycheck-checker-error-threshold))
    ;; Disable CHECKER for this buffer
    ;; (`flycheck--automatically-disabled-checkers' is a local variable).
    (lwarn '(flycheck syntax-checker) :warning
           (substitute-command-keys
            "Syntax checker %s reported too many errors (%s) and is disabled.
Use `\\[customize-variable] RET flycheck-checker-error-threshold' to
change the threshold or `\\[universal-argument] \
\\[flycheck-disable-checker]' to re-enable the checker.")
           checker (length errors))
    (push checker flycheck--automatically-disabled-checkers)
    t))

(defun flycheck-clear (&optional shall-interrupt)
  "Clear all errors in the current buffer.

With prefix arg or SHALL-INTERRUPT non-nil, also interrupt the
current syntax check."
  (interactive "P")
  (when shall-interrupt
    (flycheck-stop))
  (flycheck-delete-all-overlays)
  (flycheck-clear-errors)
  (flycheck-error-list-refresh)
  (flycheck-hide-error-buffer))

(defun flycheck--empty-variables ()
  "Empty variables used by Flycheck."
  (kill-local-variable 'flycheck--file-truename-cache)
  (kill-local-variable 'flycheck--idle-trigger-timer)
  (kill-local-variable 'flycheck--idle-trigger-conditions)
  (kill-local-variable 'flycheck--last-error-display-tick))

(defun flycheck-teardown (&optional ignore-global)
  "Teardown Flycheck in the current buffer.

Completely clear the whole Flycheck state.  Remove overlays, kill
running checks, and empty all variables used by Flycheck.

Unless optional argument IGNORE-GLOBAL is non-nil, check to see
if no more Flycheck buffers remain (aside from the current
buffer), and if so then clean up global hooks."
  (flycheck-safe-delete-temporaries)
  (flycheck-stop)
  (flycheck-clean-deferred-check)
  (flycheck-clear)
  (flycheck-cancel-error-display-error-at-point-timer)
  (flycheck--clear-idle-trigger-timer)
  (flycheck--empty-variables)
  (unless (or ignore-global
              (seq-some (lambda (buf)
                          (and (not (equal buf (current-buffer)))
                               (buffer-local-value 'flycheck-mode buf)))
                        (buffer-list)))
    (flycheck-global-teardown 'ignore-local)))


;;; Automatic syntax checking in a buffer
(defun flycheck-may-check-automatically (&rest conditions)
  "Determine whether the buffer may be checked under one of CONDITIONS.

Read-only buffers may never be checked automatically.

If CONDITIONS are given, determine whether syntax may be checked
under at least one of them, according to
`flycheck-check-syntax-automatically'."
  (and (not (or buffer-read-only (flycheck-ephemeral-buffer-p)))
       (file-exists-p default-directory)
       (or (not conditions)
           (seq-some
            (lambda (condition)
              (memq condition flycheck-check-syntax-automatically))
            conditions))))

(defvar-local flycheck--idle-trigger-timer nil
  "Timer used to trigger a syntax check after an idle delay.")

(defvar-local flycheck--idle-trigger-conditions nil
  "List of conditions under which an idle syntax check will be triggered.
This will be some subset of the allowable values for
`flycheck-check-syntax-automatically'.

For example, if the user switches to a buffer and then makes an
edit, this list will have the values `idle-change' and
`idle-buffer-switch' in it, at least until the idle timer
expires.")

(defun flycheck-buffer-automatically (&optional condition force-deferred)
  "Automatically check syntax at CONDITION.

Syntax is not checked if `flycheck-may-check-automatically'
returns nil for CONDITION.  (CONDITION may be a single condition
or a list of them.)

The syntax check is deferred if FORCE-DEFERRED is non-nil, or if
`flycheck-must-defer-check' returns t."
  (when (and flycheck-mode (if (listp condition)
                               (apply #'flycheck-may-check-automatically
                                      condition)
                             (flycheck-may-check-automatically condition)))
    (flycheck--clear-idle-trigger-timer)
    (setq flycheck--idle-trigger-conditions nil)
    (if (or force-deferred (flycheck-must-defer-check))
        (flycheck-buffer-deferred)
      (with-demoted-errors "Error while checking syntax automatically: %S"
        (flycheck-buffer)))))

(defun flycheck--clear-idle-trigger-timer ()
  "Clear the idle trigger timer."
  (when flycheck--idle-trigger-timer
    (cancel-timer flycheck--idle-trigger-timer)
    (setq flycheck--idle-trigger-timer nil)))

(defun flycheck--handle-idle-trigger (buffer)
  "Run a syntax check in BUFFER if appropriate.
This function is called by `flycheck--idle-trigger-timer'."
  (let ((current-buffer (current-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unless (or flycheck-buffer-switch-check-intermediate-buffers
                    (eq buffer current-buffer))
          (setq flycheck--idle-trigger-conditions
                (delq 'idle-buffer-switch
                      flycheck--idle-trigger-conditions)))
        (when flycheck--idle-trigger-conditions
          (flycheck-buffer-automatically flycheck--idle-trigger-conditions)
          (setq flycheck--idle-trigger-conditions nil))))))

(defun flycheck-handle-change (beg end _len)
  "Handle a buffer change between BEG and END.

BEG and END mark the beginning and end of the change text.  _LEN
is ignored.

Start a syntax check if a new line has been inserted into the
buffer."
  ;; Save and restore the match data, as recommended in (elisp)Change Hooks
  (save-match-data
    (when flycheck-mode
      (if (string-match-p (rx "\n") (buffer-substring beg end))
          (flycheck-buffer-automatically 'new-line 'force-deferred)
        (when (memq 'idle-change flycheck-check-syntax-automatically)
          (flycheck--clear-idle-trigger-timer)
          (cl-pushnew 'idle-change flycheck--idle-trigger-conditions)
          (setq flycheck--idle-trigger-timer
                (run-at-time flycheck-idle-change-delay nil
                             #'flycheck--handle-idle-trigger
                             (current-buffer))))))))

(defvar flycheck--last-buffer (current-buffer)
  "The current buffer or the buffer that was previously current.
This is usually equal to the current buffer, unless the user just
switched buffers.  After a buffer switch, it is the previous
buffer.")

(defun flycheck-handle-buffer-switch ()
  "Handle a possible switch to another buffer.

If a buffer switch actually happened, schedule a syntax check."
  ;; Switching buffers here is weird, but unfortunately necessary.  It
  ;; turns out that `with-temp-buffer' triggers
  ;; `buffer-list-update-hook' twice, and the value of
  ;; `current-buffer' is bogus in one of those triggers (the one just
  ;; after the temp buffer is killed).  If we rely on the bogus value,
  ;; Flycheck will think that the user is switching back and forth
  ;; between different buffers during the `with-temp-buffer' call
  ;; (note: two different normal buffers, not the current buffer and
  ;; the temp buffer!), and that would trigger spurious syntax checks.
  ;; It seems that reading (window-buffer) gets us the correct current
  ;; buffer in all important real-life situations (although it doesn't
  ;; necessarily catch uses of `set-buffer').
  (with-current-buffer (window-buffer)
    (unless (or (equal flycheck--last-buffer (current-buffer))
                ;; Don't bother keeping track of changes to and from
                ;; the minibuffer, as they will never require us to
                ;; run a syntax check.
                (minibufferp))
      (setq flycheck--last-buffer (current-buffer))
      (when (and flycheck-mode
                 (memq 'idle-buffer-switch flycheck-check-syntax-automatically))
        (flycheck--clear-idle-trigger-timer)
        (cl-pushnew 'idle-buffer-switch flycheck--idle-trigger-conditions)
        (setq flycheck--idle-trigger-timer
              (run-at-time flycheck-idle-buffer-switch-delay nil
                           #'flycheck--handle-idle-trigger
                           (current-buffer)))))))

(defun flycheck-handle-save ()
  "Handle a save of the buffer."
  (flycheck-buffer-automatically 'save))


;;; Deferred syntax checking
(defvar-local flycheck-deferred-syntax-check nil
  "If non-nil, a deferred syntax check is pending.")

(defun flycheck-must-defer-check ()
  "Determine whether the syntax check has to be deferred.

A check has to be deferred if the buffer is not visible, or if the buffer is
currently being reverted.

Return t if the check is to be deferred, or nil otherwise."
  (or (not (get-buffer-window))
      ;; We defer the syntax check if Flycheck is already running, to
      ;; immediately start a new syntax check after the current one finished,
      ;; because the result of the current check will most likely be outdated by
      ;; the time it is finished.
      (flycheck-running-p)
      ;; We must defer checks while a buffer is being reverted, to avoid race
      ;; conditions while the buffer contents are being restored.
      revert-buffer-in-progress-p))

(defun flycheck-deferred-check-p ()
  "Determine whether the current buffer has a deferred check.

Return t if so, or nil otherwise."
  flycheck-deferred-syntax-check)

(defun flycheck-buffer-deferred ()
  "Defer syntax check for the current buffer."
  (setq flycheck-deferred-syntax-check t))

(defun flycheck-clean-deferred-check ()
  "Clean a deferred syntax checking state."
  (setq flycheck-deferred-syntax-check nil))

(defun flycheck-perform-deferred-syntax-check ()
  "Perform the deferred syntax check."
  (when (flycheck-deferred-check-p)
    (flycheck-clean-deferred-check)
    (flycheck-buffer-automatically)))


;;; Syntax checking in all buffers
(defun flycheck-may-enable-mode ()
  "Determine whether Flycheck mode may be enabled.

Flycheck mode is not enabled for

- the minibuffer,
- `fundamental-mode'
- major modes whose `mode-class' property is `special',
- ephemeral buffers (see `flycheck-ephemeral-buffer-p'),
- encrypted buffers (see `flycheck-encrypted-buffer-p'),
- remote files (see `file-remote-p'),
- and major modes excluded by `flycheck-global-modes'.

Return non-nil if Flycheck mode may be enabled, and nil
otherwise."
  (and (pcase flycheck-global-modes
         ;; Whether `major-mode' is disallowed by `flycheck-global-modes'
         (`t t)
         (`(not . ,modes) (not (memq major-mode modes)))
         (modes (memq major-mode modes)))
       (not (or (minibufferp)
                (eq major-mode 'fundamental-mode)
                (eq (get major-mode 'mode-class) 'special)
                (flycheck-ephemeral-buffer-p)
                (flycheck-encrypted-buffer-p)
                (and (buffer-file-name)
                     (file-remote-p (buffer-file-name) 'method))))))

(defun flycheck-mode-on-safe ()
  "Enable command `flycheck-mode' if it is safe to do so.

Command `flycheck-mode' is only enabled if
`flycheck-may-enable-mode' returns a non-nil result."
  (when (flycheck-may-enable-mode)
    (flycheck-mode)))

;;;###autoload
(define-globalized-minor-mode global-flycheck-mode flycheck-mode
  flycheck-mode-on-safe
  :init-value nil
  :group 'flycheck)

(defun flycheck-global-teardown (&optional ignore-local)
  "Teardown Flycheck in all buffers.

Completely clear the whole Flycheck state in all buffers, stop
all running checks, remove all temporary files, and empty all
variables of Flycheck.

Also remove global hooks.  (If optional argument IGNORE-LOCAL is
non-nil, then only do this and skip per-buffer teardown.)"
  (unless ignore-local
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when flycheck-mode
            (flycheck-teardown 'ignore-global))))))
  (remove-hook 'buffer-list-update-hook #'flycheck-handle-buffer-switch))

;; Clean up the entire state of Flycheck when Emacs is killed, to get rid of any
;; pending temporary files.
(add-hook 'kill-emacs-hook #'flycheck-global-teardown)


;;; Errors from syntax checks
(cl-defstruct (flycheck-error
               (:constructor nil)
               (:constructor
                flycheck-error-new
                (&key
                 line column end-line end-column
                 buffer checker filename message level id group
                 &aux (-end-line end-line) (-end-column end-column)))
               (:constructor
                flycheck-error-new-at
                (line
                 column
                 &optional level message
                 &key end-line end-column checker id group
                 (filename (buffer-file-name)) (buffer (current-buffer))
                 &aux (-end-line end-line) (-end-column end-column)))
               (:constructor
                flycheck-error-new-at-pos
                (pos
                 &optional level message
                 &key end-pos checker id group
                 (filename (buffer-file-name)) (buffer (current-buffer))
                 &aux
                 ((line . column)
                  (if pos (flycheck-line-column-at-pos pos)
                    '(nil . nil)))
                 ((-end-line . -end-column)
                  (if end-pos (flycheck-line-column-at-pos end-pos)
                    '(nil . nil))))))
  "Structure representing an error reported by a syntax checker.
Slots:

`buffer'
     The buffer that the error was reported for, as buffer object.

`checker'
     The syntax checker which reported this error, as symbol.

`filename'
     The file name the error refers to, as string.

`line'
     The line on which the error starts, as number.

`column' (optional)
     The column at which the error starts, as number.

     For compatibility with external tools and unlike Emacs
     itself (e.g. in Compile Mode) Flycheck uses _1-based_
     columns: The first character on a line is column 1.

     Occasionally some tools try to proactively adapt to Emacs
     and emit 0-based columns automatically.  In these cases, the
     columns must be adjusted for Flycheck, see
     `flycheck-increment-error-columns'.

     If nil, the whole line is highlighted.

`end-line' (optional)
    The line on which the error ends.  If nil, this is computed according to
    `flycheck-highlighting-mode'.

`end-column'
    The column at which the error ends.  If nil, this is computed according to
    `flycheck-highlighting-mode'.  Error intervals are right-open: the
    end-column points to the first character not included in the error.  For
    example, 1:1 is an empty range. and in \"line-number-at-pos\", the range
    6:12 covers the word \"number\".

`message' (optional)
     The error message as a string, if any.

`level'
     The error level, as either `info', `warning' or `error'.

`id' (optional)
     An ID identifying the kind of error.

`group' (optional)
     A symbol identifying the group the error belongs to.

     Some tools will emit multiple errors that relate to the same
     issue (e.g., lifetime errors in Rust).  All related errors
     collected by a checker should have the same `group` value,
     in order to be able to present them to the user.

     See `flycheck-related-errors`."
  buffer checker filename line column message level id group
  ;; The fields below are at the end of the record to preserve backwards
  ;; compatibility; see https://github.com/flycheck/flycheck/pull/1400 and
  ;; https://lists.gnu.org/archive/html/emacs-devel/2018-07/msg00436.html
  -end-line -end-column)

;; These accessors are defined for backwards compatibility
;; FIXME: Clean up once package.el learns how to recompile dependencies.

(defun flycheck-error-end-line (err)
  "Return the end line of a Flycheck error ERR."
  (condition-case nil (flycheck-error--end-line err)
    (args-out-of-range nil)))

(defun flycheck-error-end-column (err)
  "Return the end column of a Flycheck error ERR."
  (condition-case nil (flycheck-error--end-column err)
    (args-out-of-range nil)))

(defun flycheck-error--set-end-line (err line)
  "Set the end line of a Flycheck error ERR to LINE."
  (condition-case nil (setf (flycheck-error--end-line err) line)
    (args-out-of-range nil)))

(defun flycheck-error--set-end-column (err column)
  "Set the end column of a Flycheck error ERR to COLUMN."
  (condition-case nil (setf (flycheck-error--end-column err) column)
    (args-out-of-range nil)))

(gv-define-simple-setter flycheck-error-end-line
                         flycheck-error--set-end-line)
(gv-define-simple-setter flycheck-error-end-column
                         flycheck-error--set-end-column)

(defmacro flycheck-error-with-buffer (err &rest forms)
  "Switch to the buffer of ERR and evaluate FORMS.

If the buffer of ERR is not live, FORMS are not evaluated."
  (declare (indent 1) (debug t))
  `(when (buffer-live-p (flycheck-error-buffer ,err))
     (with-current-buffer (flycheck-error-buffer ,err)
       ,@forms)))

(defun flycheck--exact-region (err)
  "Get the region of ERR, if ERR specifies a range.

Return a cons cell `(BEG . END)'.  If the input range is empty,
it is expanded to cover at least one character so that END is
always greater than BEG.  If ERR doesn't specify an end-column
return nil."
  (-if-let* ((line (flycheck-error-line err))
             (column (flycheck-error-column err))
             (end-line (or (flycheck-error-end-line err) line))
             (end-column (flycheck-error-end-column err)))
      ;; Ignoring fields speeds up calls to `line-end-position'.
      (let* ((inhibit-field-text-motion t)
             (beg (flycheck-line-column-to-position line column))
             (end (flycheck-line-column-to-position end-line end-column)))
        (cond
         ((< beg end) (cons beg end))
         ((= end (point-max)) (cons (1- end) end))
         (t (cons end (1+ end)))))))

(defun flycheck--line-region (pos)
  "Get the line region of position POS.

Return a cons cell `(BEG . END)' where BEG is the first
non-whitespace character on the line ERR refers to, and END the
end of the line."
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (let ((bol (point))
          (end (line-end-position)))
      ;; Move to the beginning of this line's indentation, similar to
      ;; `back-to-indentation'
      (skip-syntax-forward " " end)
      (backward-prefix-chars)
      ;; If the current line is blank, highlight it in full; if it's
      ;; empty, include the previous line break character(s) to have
      ;; any region at all (when called with 0, `line-end-position'
      ;; gives us the end of the previous line).
      (cons (if (eolp) (if (= bol end) (line-end-position 0) bol) (point))
            end))))

(defun flycheck--column-region (pos)
  "Get the column region of position POS.

Return a cons cell `(BEG . END)' where BEG is the character
before the column, and END the actual column."
  (save-excursion
    (goto-char pos)
    ;; (eobp): No enough lines in the buffer
    (if (eobp) (cons (1- (point-max)) (point-max))
      (cons pos (1+ pos)))))

(defun flycheck-bounds-of-thing-at-point (thing pos)
  "Get the region of THING at position POS.

THING is a understood by `thing-at-point'.

Return a cons cell `(BEG . END)' where BEG is the beginning of
the THING at the column, and END the end of the THING."
  (save-excursion
    (goto-char pos)
    (bounds-of-thing-at-point thing)))

(defun flycheck--approximate-region (err mode)
  "Compute the region of ERR based on MODE and ERR's line and column."
  ;; Ignoring fields speeds up calls to `line-end-position'.
  (let* ((inhibit-field-text-motion t)
         (line (flycheck-error-line err))
         (column (flycheck-error-column err))
         (beg (flycheck-line-column-to-position line (or column 1))))
    (if (or (null column)
            (eq mode 'lines))
        (flycheck--line-region beg)
      (or (pcase mode
            (`symbols
             ;; Ensure that we're on a word or symbol.  See
             ;; https://github.com/flycheck/flycheck/issues/1519
             (and (<= (point-min) beg) (< beg (point-max))
                  (memq (char-syntax (char-after beg)) '(?w ?_))
                  (flycheck-bounds-of-thing-at-point 'symbol beg)))
            (`sexps
             (flycheck-bounds-of-thing-at-point 'sexp beg)))
          (flycheck--column-region beg)))))

(defun flycheck-error-region-for-mode (err mode)
  "Get the region of ERR for the highlighting MODE.

ERR is a Flycheck error.  If its position is fully specified, use
that to compute a region; otherwise, use MODE, as documented in
`flycheck-highlighting-mode'.  If MODE is nil, signal an error."
  (flycheck-error-with-buffer err
    (save-restriction
      (widen)
      (or (flycheck--exact-region err)
          (flycheck--approximate-region err mode)))))

(defun flycheck-error-pos (err)
  "Get the buffer position of ERR.

ERR is a Flycheck error whose position to get.

The error position is the error column, or the first
non-whitespace character of the error line, if ERR has no error column."
  (car (flycheck-error-region-for-mode
        err flycheck-highlighting-mode)))

(defun flycheck-error-format-snippet (err &optional max-length)
  "Extract the text that ERR refers to from the buffer.

Newlines and blanks are replaced by single spaces.  If ERR
doesn't include an end-position, return nil.

MAX-LENGTH is how many characters to read from the buffer, at
most.  It defaults to 20."
  (flycheck-error-with-buffer err
    (save-restriction
      (widen)
      (pcase (flycheck--exact-region err)
        (`(,beg . ,end)
         (truncate-string-to-width
          (replace-regexp-in-string
           "\\s-+" " " (buffer-substring beg (min end (point-max))))
          (or max-length 20) nil nil t))))))

(defun flycheck-error-format-message-and-id (err &optional include-snippet)
  "Format the message and id of ERR as human-readable string.

If INCLUDE-SNIPPET is non-nil, prepend the message with a snippet
of the text that the error applies to (such text can only be
determined if the error contains a full span, not just a
beginning position)."
  (let* ((id (flycheck-error-id err))
         (fname (flycheck-error-filename err))
         (other-file-p (and fname (not (equal fname (buffer-file-name))))))
    (concat (and other-file-p (format "In %S:\n" (file-relative-name fname)))
            (and include-snippet
                 (-when-let* ((snippet (flycheck-error-format-snippet err)))
                   (flycheck--format-message "`%s': " snippet)))
            (or (flycheck-error-message err)
                (format "Unknown %S" (flycheck-error-level err)))
            (and id (format " [%s]" id)))))

(defun flycheck-error-format-position (err)
  "Format the position of ERR as a human-readable string."
  (let ((line (flycheck-error-line err))
        (column (flycheck-error-column err))
        (end-line (flycheck-error-end-line err))
        (end-column (flycheck-error-end-column err)))
    (if (and line column)
        (if (or (null end-line) (equal line end-line))
            (if (or (null end-column) (equal column (1- end-column)))
                (format "%d:%d" line column)
              (format "%d:%d-%d" line column end-column))
          (format "(%d:%d)-(%d:%d)" line column end-line end-column))
      (if (or (null end-line) (equal line end-line))
          (format "%d" line)
        (format "%d-%d" line end-line)))))

(defun flycheck-error-format (err &optional with-file-name)
  "Format ERR as human-readable string, optionally WITH-FILE-NAME.

Return a string that represents the given ERR.  If WITH-FILE-NAME
is given and non-nil, include the file-name as well, otherwise
omit it."
  (let* ((level (symbol-name (flycheck-error-level err)))
         (checker (symbol-name (flycheck-error-checker err)))
         (format `(,@(when with-file-name
                       (list (flycheck-error-filename err) ":"))
                   ,(flycheck-error-format-position err) ":"
                   ,level ": "
                   ,(flycheck-error-format-message-and-id err)
                   " (" ,checker ")")))
    (apply #'concat format)))

(defun flycheck-error-< (err1 err2)
  "Determine whether ERR1 is less than ERR2 by location."
  (let ((l1 (flycheck-error-line err1))
        (l2 (flycheck-error-line err2)))
    (if (/= l1 l2)
        (< l1 l2)
      (let ((c1 (or (flycheck-error-column err1) 1))
            (c2 (or (flycheck-error-column err2) 1)))
        (if (/= c1 c2)
            (< c1 c2)
          (let ((el1 (or (flycheck-error-end-line err1) l1))
                (el2 (or (flycheck-error-end-line err2) l2)))
            (if (/= el1 el2)
                (< el1 el2)
              (let ((cl1 (or (flycheck-error-end-column err1) 1))
                    (cl2 (or (flycheck-error-end-column err2) 1)))
                (< cl1 cl2)))))))))

(defun flycheck-error-level-< (err1 err2)
  "Determine whether ERR1 is less than ERR2 by error level.

Like `flycheck-error-<', but compares by error level severity
first.  Levels of the same severity are compared by name."
  (let* ((level1 (flycheck-error-level err1))
         (level2 (flycheck-error-level err2))
         (severity1 (flycheck-error-level-severity level1))
         (severity2 (flycheck-error-level-severity level2)))
    (cond
     ((= severity1 severity2)
      (if (string= level1 level2)
          (flycheck-error-< err1 err2)
        (string< level1 level2)))
     (t (< severity1 severity2)))))

(defun flycheck-assert-error-list-p (errors)
  "Assert that all items in ERRORS are of `flycheck-error' type.

Signal an error if any item in ERRORS is not a `flycheck-error'
object, as by `flycheck-error-p'.  Otherwise return ERRORS
again."
  (unless (listp errors)
    (signal 'wrong-type-argument (list 'listp errors)))
  (dolist (err errors)
    (unless (flycheck-error-p err)
      (signal 'wrong-type-argument (list 'flycheck-error-p err))))
  errors)


;;; Errors in the current buffer
(defvar-local flycheck-current-errors nil
  "A list of all errors and warnings in the current buffer.")

(defun flycheck-report-current-errors (errors)
  "Report ERRORS in the current buffer.

Add ERRORS to `flycheck-current-errors' and process each error
with `flycheck-process-error-functions'."
  (setq flycheck-current-errors (append errors flycheck-current-errors))
  (overlay-recenter (point-max))
  ;; We can't use `seq-sort-by' because it's not in Emacs 25's built-in `seq',
  ;; and installing an updated version doesn't help (this is a package.el bug;
  ;; see https://lists.gnu.org/archive/html/emacs-devel/2020-04/msg01974.html).
  (seq-do (lambda (err)
            (run-hook-with-args-until-success 'flycheck-process-error-functions
                                              err))
          (seq-sort (lambda (e1 e2)
                      (< (flycheck-error-line e1) (flycheck-error-line e2)))
                    errors)))

(defun flycheck-clear-errors ()
  "Remove all error information from the current buffer."
  (setq flycheck-current-errors nil)
  (flycheck-report-status 'not-checked))

(defun flycheck-fill-and-expand-error-file-names (errors directory)
  "Fill and expand file names in ERRORS relative to DIRECTORY.

Expand all file names of ERRORS against DIRECTORY.  If the file
name of an error is nil fill in the result of function
`buffer-file-name' in the current buffer.

Return ERRORS, modified in-place."
  (seq-do (lambda (err)
            (setf (flycheck-error-filename err)
                  (-if-let (filename (flycheck-error-filename err))
                      (expand-file-name filename directory)
                    (buffer-file-name))))
          errors)
  errors)

(defun flycheck-relevant-error-other-file-p (err)
  "Determine whether ERR is a relevant error for another file."
  (let ((file-name (flycheck-error-filename err)))
    (and file-name
         flycheck-relevant-error-other-file-show
         (or (null buffer-file-name)
             (not (flycheck-same-files-p buffer-file-name file-name)))
         (<= (flycheck-error-level-severity
              flycheck-relevant-error-other-file-minimum-level)
             (flycheck-error-level-severity (flycheck-error-level err))))))

(defun flycheck-relevant-error-p (err)
  "Determine whether ERR is relevant for the current buffer.

Return t if ERR may be shown for the current buffer, or nil
otherwise."
  (flycheck-error-with-buffer err
    (let ((file-name (flycheck-error-filename err))
          (message (flycheck-error-message err)))
      (and
       (or
        ;; Neither the error nor buffer have a file name
        (and (not file-name) (not buffer-file-name))
        ;; Both have files, and they match
        (and buffer-file-name file-name
             (flycheck-same-files-p file-name buffer-file-name))
        ;; This is a significant error from another file
        (flycheck-relevant-error-other-file-p err))
       message
       (not (string-empty-p message))
       ;; Errors without line numbers are discarded.  If a linter
       ;; reports relevant errors without line numbers, use
       ;; `flycheck-fill-empty-line-numbers' as the checker's
       ;; `:error-filter' to set them to line 0.
       (flycheck-error-line err)))))

(defun flycheck-relevant-errors (errors)
  "Filter the relevant errors from ERRORS.

Return a list of all errors that are relevant for their
corresponding buffer."
  (seq-filter #'flycheck-relevant-error-p errors))

(defun flycheck-related-errors (err &optional error-set)
  "Get all the errors that are in the same group as ERR.

Return a list of all errors (from ERROR-SET) that have the same
`flycheck-error-group' as ERR, including ERR itself.

If ERROR-SET is nil, `flycheck-current-errors' is used instead."
  (let ((group (flycheck-error-group err))
        (checker (flycheck-error-checker err)))
    (if group
        (seq-filter (lambda (e)
                      (and (eq (flycheck-error-checker e) checker)
                           (eq (flycheck-error-group e) group)))
                    (or error-set flycheck-current-errors))
      (list err))))


;;; Status reporting for the current buffer
(defvar-local flycheck-last-status-change 'not-checked
  "The last status change in the current buffer.")

(defun flycheck-report-failed-syntax-check (&optional status)
  "Report a failed Flycheck syntax check with STATUS.

STATUS is a status symbol for `flycheck-report-status',
defaulting to `errored'.

Clear Flycheck state, run `flycheck-syntax-check-failed-hook' and
report an error STATUS."
  (flycheck-clear)
  (setq flycheck-current-syntax-check nil)
  (run-hooks 'flycheck-syntax-check-failed-hook)
  (flycheck-report-status (or status 'errored)))

(defun flycheck-report-status (status)
  "Report Flycheck STATUS.

STATUS is one of the following symbols:

`not-checked'
     The current buffer was not checked.

`no-checker'
     Automatic syntax checker selection did not find a suitable
     syntax checker.

`running'
     A syntax check is now running in the current buffer.

`errored'
     The current syntax check has errored.

`finished'
     The current syntax check was finished normally.

`interrupted'
     The current syntax check was interrupted.

`suspicious'
     The last syntax check had a suspicious result.

Set `flycheck-last-status-change' and call
`flycheck-status-changed-functions' with STATUS.  Afterwards
refresh the mode line."
  (setq flycheck-last-status-change status)
  (run-hook-with-args 'flycheck-status-changed-functions status)
  (force-mode-line-update))

(defun flycheck-mode-line-status-text (&optional status)
  "Get a text describing STATUS for use in the mode line.

STATUS defaults to `flycheck-last-status-change' if omitted or
nil."
  (let ((text (pcase (or status flycheck-last-status-change)
                (`not-checked "")
                (`no-checker "-")
                (`running "*")
                (`errored "!")
                (`finished
                 (let-alist (flycheck-count-errors flycheck-current-errors)
                   (if (or .error .warning)
                       (format ":%s|%s" (or .error 0) (or .warning 0))
                     "")))
                (`interrupted ".")
                (`suspicious "?"))))
    (concat " " flycheck-mode-line-prefix text)))


;;; Error levels
(defun flycheck-make-margin-spec (margin-str face)
  "Make a display spec to indicate errors in the margins.

Returns MARGIN-STR with FACE applied."
  (propertize margin-str 'face `(,face default)))

(defconst flycheck-default-margin-str "»"
  "String used to indicate errors in the margins.")

(defconst flycheck-default-margin-continuation-str "⋮"
  "String used to indicate continuation lines in the margins.")

;;;###autoload
(defun flycheck-define-error-level (level &rest properties)
  "Define a new error LEVEL with PROPERTIES.

The following PROPERTIES constitute an error level:

`:severity SEVERITY'
     A number denoting the severity of this level.  The higher
     the number, the more severe is this level compared to other
     levels.  Defaults to 0; info is -10, warning is 10, and
     error is 100.

     The severity is used by `flycheck-error-level-<' to
     determine the ordering of errors according to their levels.

`:compilation-level LEVEL'

     A number indicating the broad class of messages that errors
     at this level belong to: one of 0 (info), 1 (warning), or
     2 or nil (error).  Defaults to nil.

     This is used by `flycheck-checker-pattern-to-error-regexp'
     to map error levels into `compilation-mode''s hierarchy and
     to get proper highlighting of errors in `compilation-mode'.

`:overlay-category CATEGORY'
     A symbol denoting the overlay category to use for error
     highlight overlays for this level.  See Info
     node `(elisp)Overlay Properties' for more information about
     overlay categories.

     A category for an error level overlay should at least define
     the `face' property, for error highlighting.  Another useful
     property for error level categories is `priority', to
     influence the stacking of multiple error level overlays.

`:fringe-bitmap BITMAPS'
     A fringe bitmap symbol denoting the bitmap to use for fringe
     indicators for this level, or a cons of two bitmaps (one for
     narrow fringes and one for wide fringes).  See Info node
     `(elisp)Fringe Bitmaps' for more information about fringe
     bitmaps, including a list of built-in fringe bitmaps.

`:fringe-face FACE'
     A face symbol denoting the face to use for fringe indicators
     for this level.

`:margin-spec SPEC'
     A display specification indicating what to display in the
     margin when `flycheck-indication-mode' is `left-margin' or
     `right-margin'.  See Info node `(elisp)Displaying in the
     Margins'.  If omitted, Flycheck generates an image spec from
     the fringe bitmap.

`:error-list-face FACE'
     A face symbol denoting the face to use for messages of this
     level in the error list.  See `flycheck-list-errors'."
  (declare (indent 1))
  (setf (get level 'flycheck-error-level) t)
  (setf (get level 'flycheck-error-severity)
        (or (plist-get properties :severity) 0))
  (setf (get level 'flycheck-compilation-level)
        (plist-get properties :compilation-level))
  (setf (get level 'flycheck-overlay-category)
        (plist-get properties :overlay-category))
  (setf (get level 'flycheck-fringe-bitmaps)
        (let ((bitmap (plist-get properties :fringe-bitmap)))
          (if (consp bitmap) bitmap (cons bitmap bitmap))))
  ;; Kept for compatibility
  (setf (get level 'flycheck-fringe-bitmap-double-arrow)
        (car (get level 'flycheck-fringe-bitmaps)))
  (setf (get level 'flycheck-fringe-face)
        (plist-get properties :fringe-face))
  (setf (get level 'flycheck-margin-spec)
        (or (plist-get properties :margin-spec)
            (flycheck-make-margin-spec
             flycheck-default-margin-str
             (or (get level 'flycheck-fringe-face) 'default))))
  (setf (get level 'flycheck-margin-continuation)
        (flycheck-make-margin-spec
         flycheck-default-margin-continuation-str
         (or (get level 'flycheck-fringe-face) 'default)))
  (setf (get level 'flycheck-error-list-face)
        (plist-get properties :error-list-face)))

(defun flycheck-error-level-p (level)
  "Determine whether LEVEL is a Flycheck error level."
  (get level 'flycheck-error-level))

(defun flycheck-error-level-severity (level)
  "Get the numeric severity of LEVEL."
  (or (get level 'flycheck-error-severity) 0))

(defun flycheck-error-level-compilation-level (level)
  "Get the compilation level for LEVEL."
  (get level 'flycheck-compilation-level))

(defun flycheck-error-level-overlay-category (level)
  "Get the overlay category for LEVEL."
  (get level 'flycheck-overlay-category))

(defun flycheck-error-level-margin-spec (level)
  "Get the margin spec for LEVEL."
  (get level 'flycheck-margin-spec))

(defun flycheck-error-level-margin-continuation-spec (level)
  "Get the margin continuation spec for LEVEL."
  (get level 'flycheck-margin-continuation))

(defun flycheck-error-level-fringe-bitmap (level &optional hi-res)
  "Get the fringe bitmap for LEVEL.

Optional argument HI-RES non-nil means that the returned bitmap
will be the high resolution version."
  (let ((bitmaps (get level 'flycheck-fringe-bitmaps)))
    (if hi-res (cdr bitmaps) (car bitmaps))))

(defun flycheck-error-level-fringe-face (level)
  "Get the fringe face for LEVEL."
  (get level 'flycheck-fringe-face))

(defun flycheck-error-level-error-list-face (level)
  "Get the error list face for LEVEL."
  (get level 'flycheck-error-list-face))

(defun flycheck-error-level-make-indicator (level side &optional continuation)
  "Create the fringe or margin icon for LEVEL at SIDE.

Return a propertized string that shows an indicator according
to LEVEL and the given fringe or margin SIDE.

LEVEL is a Flycheck error level defined with
`flycheck-define-error-level', and SIDE is either `left-fringe',
`right-fringe', `left-margin', or `right-margin'.

CONTINUATION indicates which fringe bitmap or margin spec to use:
either the `:fringe-bitmap' and `:margin-spec' properties of
LEVEL when CONTINUATION is nil or omitted, or bitmaps and specs
indicating an error spanning more than one line.

Return a propertized string representing the fringe icon,
intended for use as `before-string' of an overlay to actually
show the indicator."
  (propertize
   "!" 'display
   (pcase side
     ((or `left-fringe `right-fringe)
      (list side
            (if continuation 'flycheck-fringe-bitmap-continuation
              (let* ((fringe-width
                      (pcase side
                        (`left-fringe (car (window-fringes)))
                        (`right-fringe (cadr (window-fringes)))))
                     (high-res (>= fringe-width 16)))
                (flycheck-error-level-fringe-bitmap level high-res)))
            (flycheck-error-level-fringe-face level)))
     ((or `left-margin `right-margin)
      `((margin ,side)
        ,(or (if continuation
                 (flycheck-error-level-margin-continuation-spec level)
               (flycheck-error-level-margin-spec level))
             "")))
     (_ (error "Invalid fringe side: %S" side)))))

(define-obsolete-function-alias
  'flycheck-error-level-make-fringe-icon
  'flycheck-error-level-make-indicator
  "33")


;;; Built-in error levels
(defconst flycheck-fringe-bitmap-double-arrow
  [#b11011000
   #b01101100
   #b00110110
   #b00011011
   #b00110110
   #b01101100
   #b11011000]
  "Bitmaps used to indicate errors in the left fringes.")

(defconst flycheck-fringe-bitmap-double-left-arrow
  [#b00011011
   #b00110110
   #b01101100
   #b11011000
   #b01101100
   #b00110110
   #b00011011]
  "Bitmaps used to indicate errors in the right fringes.")

(defconst flycheck-fringe-bitmap-double-arrow-hi-res
  [#b1111001111000000
   #b0111100111100000
   #b0011110011110000
   #b0001111001111000
   #b0000111100111100
   #b0000011110011110
   #b0000011110011110
   #b0000111100111100
   #b0001111001111000
   #b0011110011110000
   #b0111100111100000
   #b1111001111000000]
  "High-resolution bitmap used to indicate errors in the left fringes.")

(defconst flycheck-fringe-bitmap-double-left-arrow-hi-res
  [#b0000001111001111
   #b0000011110011110
   #b0000111100111100
   #b0001111001111000
   #b0011110011110000
   #b0111100111100000
   #b0111100111100000
   #b0011110011110000
   #b0001111001111000
   #b0000111100111100
   #b0000011110011110
   #b0000001111001111]
  "High-resolution bitmap used to indicate errors in the right fringes.")

(defconst flycheck-fringe-bitmap-continuation
  [#b1000000010000000
   #b0010000000100000
   #b0000100000001000
   #b0000001000000010]
  "Bitmap used to indicate continuation lines in the fringes.")

(when (fboundp 'define-fringe-bitmap) ;; #ifdef HAVE_WINDOW_SYSTEM
  (define-fringe-bitmap
    'flycheck-fringe-bitmap-double-arrow
    flycheck-fringe-bitmap-double-arrow)
  (define-fringe-bitmap
    'flycheck-fringe-bitmap-double-arrow-hi-res
    flycheck-fringe-bitmap-double-arrow-hi-res
    nil 16)
  (define-fringe-bitmap
    'flycheck-fringe-bitmap-double-left-arrow
    flycheck-fringe-bitmap-double-left-arrow)
  (define-fringe-bitmap
    'flycheck-fringe-bitmap-double-left-arrow-hi-res
    flycheck-fringe-bitmap-double-left-arrow-hi-res
    nil 16)
  (define-fringe-bitmap
    'flycheck-fringe-bitmap-continuation
    flycheck-fringe-bitmap-continuation
    nil 16 '(top repeat)))

(defun flycheck-redefine-standard-error-levels
    (&optional margin-str fringe-bitmap)
  "Redefine Flycheck's standard error levels.

This is useful to change the character drawn in the
margins (MARGIN-STR, a string) or the bitmap drawn in the
fringes (FRINGE-BITMAP, a fringe bitmap symbol or a cons of such
symbols, as in `flycheck-define-error-level')."
  (unless margin-str
    (setq margin-str flycheck-default-margin-str))

  (unless fringe-bitmap
    (setq fringe-bitmap
          (cons 'flycheck-fringe-bitmap-double-arrow
                'flycheck-fringe-bitmap-double-arrow-hi-res)))

  (setf (get 'flycheck-error-overlay 'face) 'flycheck-error)
  (setf (get 'flycheck-error-overlay 'priority) 110)

  (flycheck-define-error-level 'error
    :severity 100
    :compilation-level 2
    :overlay-category 'flycheck-error-overlay
    :margin-spec (flycheck-make-margin-spec margin-str 'flycheck-fringe-error)
    :fringe-bitmap fringe-bitmap
    :fringe-face 'flycheck-fringe-error
    :error-list-face 'flycheck-error-list-error)

  (setf (get 'flycheck-warning-overlay 'face) 'flycheck-warning)
  (setf (get 'flycheck-warning-overlay 'priority) 100)

  (flycheck-define-error-level 'warning
    :severity 10
    :compilation-level 1
    :overlay-category 'flycheck-warning-overlay
    :margin-spec (flycheck-make-margin-spec margin-str 'flycheck-fringe-warning)
    :fringe-bitmap fringe-bitmap
    :fringe-face 'flycheck-fringe-warning
    :error-list-face 'flycheck-error-list-warning)

  (setf (get 'flycheck-info-overlay 'face) 'flycheck-info)
  (setf (get 'flycheck-info-overlay 'priority) 90)

  (flycheck-define-error-level 'info
    :severity -10
    :compilation-level 0
    :overlay-category 'flycheck-info-overlay
    :margin-spec (flycheck-make-margin-spec margin-str 'flycheck-fringe-info)
    :fringe-bitmap fringe-bitmap
    :fringe-face 'flycheck-fringe-info
    :error-list-face 'flycheck-error-list-info))

(flycheck-redefine-standard-error-levels)


;;; Error filtering
(defun flycheck-filter-errors (errors checker)
  "Filter ERRORS from CHECKER.

Apply the error filter of CHECKER to ERRORs and return the
result.  If CHECKER has no error filter, fall back to
`flycheck-sanitize-errors'."
  (let ((filter (or (flycheck-checker-get checker 'error-filter)
                    #'flycheck-sanitize-errors)))
    (funcall filter errors)))

(defun flycheck-sanitize-errors (errors)
  "Sanitize ERRORS.

Sanitize ERRORS by trimming leading and trailing whitespace in
all error messages, and by replacing 0 columns and empty error
messages with nil.

Returns sanitized ERRORS."
  (dolist (err errors)
    (flycheck-error-with-buffer err
      (let ((message (flycheck-error-message err))
            (id (flycheck-error-id err)))
        (when message
          (setq message (string-trim message))
          (setf (flycheck-error-message err)
                (if (string-empty-p message) nil message)))
        (when (and id (string-empty-p id))
          (setf (flycheck-error-id err) nil))
        (when (eq (flycheck-error-column err) 0)
          (setf (flycheck-error-column err) nil))
        (when (eq (flycheck-error-end-column err) 0)
          (setf (flycheck-error-end-column err) nil)))))
  errors)

(defun flycheck-remove-error-file-names (file-name errors)
  "Remove matching FILE-NAME from ERRORS.

Use as `:error-filter' for syntax checkers that output faulty
filenames.  Flycheck will later fill in the buffer file name.

Return ERRORS."
  (seq-do (lambda (err)
            (when (and (flycheck-error-filename err)
                       (string= (flycheck-error-filename err) file-name))
              (setf (flycheck-error-filename err) nil)))
          errors)
  errors)

(defun flycheck-increment-error-columns (errors &optional offset)
  "Increment all columns of ERRORS by OFFSET (default: 1).

  Use this as `:error-filter' if a syntax checker outputs 0-based
  columns."
  (setq offset (or offset 1)) ;; Emacs bug #31715
  (seq-do (lambda (err)
            (when (flycheck-error-column err)
              (cl-incf (flycheck-error-column err) offset))
            (when (flycheck-error-end-column err)
              (cl-incf (flycheck-error-end-column err) offset)))
          errors)
  errors)

(defun flycheck-collapse-error-message-whitespace (errors)
  "Collapse whitespace in all messages of ERRORS.

Return ERRORS."
  (dolist (err errors)
    (-when-let (message (flycheck-error-message err))
      (setf (flycheck-error-message err)
            (replace-regexp-in-string (rx (one-or-more (any space "\n" "\r")))
                                      " " message 'fixed-case 'literal))))
  errors)

(defun flycheck-dedent-error-messages (errors)
  "Dedent all messages of ERRORS.

For each error in ERRORS, determine the indentation offset from
the leading whitespace of the first line, and dedent all further
lines accordingly.

Return ERRORS, with in-place modifications."
  (dolist (err errors)
    (-when-let (message (flycheck-error-message err))
      (with-temp-buffer
        (insert message)
        ;; Determine the indentation offset
        (goto-char (point-min))
        (back-to-indentation)
        (let* ((indent-offset (- (point) (point-min))))
          ;; Now iterate over all lines and dedent each according to
          ;; `indent-offset'
          (while (not (eobp))
            (back-to-indentation)
            ;; If the current line starts with sufficient whitespace, delete the
            ;; indentation offset.  Otherwise keep the line intact, as we might
            ;; loose valuable information
            (when (>= (- (point) (line-beginning-position)) indent-offset)
              (delete-char (- indent-offset)))
            (forward-line 1)))
        (delete-trailing-whitespace (point-min) (point-max))
        (setf (flycheck-error-message err)
              (buffer-substring-no-properties (point-min) (point-max))))))
  errors)

(defun flycheck-fold-include-levels (errors sentinel-message)
  "Fold levels of ERRORS from included files.

ERRORS is a list of `flycheck-error' objects.  SENTINEL-MESSAGE
is a regular expression matched against the error message to
determine whether the error denotes errors from an included
file.  Alternatively, it is a function that is given an error and
shall return non-nil, if the error denotes errors from an
included file."
  (unless (or (stringp sentinel-message) (functionp sentinel-message))
    (error "Sentinel must be string or function: %S" sentinel-message))
  (let ((sentinel (if (functionp sentinel-message)
                      sentinel-message
                    (lambda (err)
                      (string-match-p sentinel-message
                                      (flycheck-error-message err)))))
        (remaining-errors errors))
    (while remaining-errors
      (let* ((current-error (pop remaining-errors)))
        (when (funcall sentinel current-error)
          ;; We found an error denoting errors in the included file:
          ;; 1. process all subsequent errors until faulty include file is found
          ;; 2. process again all subsequent errors until an error has the
          ;;    current file name again
          ;; 3. find the most severe error level
          (let ((current-filename (flycheck-error-filename current-error))
                (current-level nil)
                (faulty-include-filename nil)
                (filename nil)
                (done (null remaining-errors)))

            (while (not done)
              (setq filename (flycheck-error-filename (car remaining-errors)))
              (unless faulty-include-filename
                (unless (string= filename current-filename)
                  (setq faulty-include-filename filename)))

              (let* ((error-in-include (pop remaining-errors))
                     (in-include-level (flycheck-error-level error-in-include)))
                (unless (funcall sentinel error-in-include)
                  ;; Ignore nested "included file" errors, we are only
                  ;; interested in real errors because these define our level
                  (when (or (not current-level)
                            (> (flycheck-error-level-severity in-include-level)
                               (flycheck-error-level-severity current-level)))
                    (setq current-level in-include-level))))

              (setq done (or (null remaining-errors)
                             (and faulty-include-filename
                                  (string= filename current-filename)))))

            (setf (flycheck-error-level current-error) current-level
                  (flycheck-error-message current-error)
                  (format "In include %s" faulty-include-filename))))))
    errors))

(defun flycheck-dequalify-error-ids (errors)
  "De-qualify error ids in ERRORS.

Remove all qualifications from error ids in ERRORS, by stripping
all leading dotted components from error IDs.  For instance, if
the error ID is com.foo.E100, replace it with E100.

This error filter is mainly useful to simplify error IDs obtained
from parsing Checkstyle XML, which frequently has very verbose
IDs, that include the name of the tool."
  (seq-do (lambda (err)
            (let ((id (flycheck-error-id err)))
              (when id
                (setf (flycheck-error-id err)
                      (replace-regexp-in-string
                       (rx string-start
                           (group
                            (optional (zero-or-more not-newline) "."))
                           (one-or-more (not (any ".")))
                           string-end)
                       "" id 'fixedcase 'literal 1)))))
          errors)
  errors)

(defun flycheck-remove-error-ids (errors)
  "Remove all error ids from ERRORS."
  (seq-do (lambda (err) (setf (flycheck-error-id err) nil)) errors)
  errors)

(defun flycheck-fill-empty-line-numbers (errors)
  "Set ERRORS without lines to line 0.

Use as `:error-filter' for syntax checkers that output errors
without line numbers.

Return ERRORS."
  (seq-do (lambda (err)
            (unless (flycheck-error-line err)
              (setf (flycheck-error-line err) 0)))
          errors)
  errors)


;;; Error analysis
(defun flycheck-count-errors (errors)
  "Count the number of ERRORS, grouped by level.

Return an alist, where each ITEM is a cons cell whose `car' is an
error level, and whose `cdr' is the number of errors of that
level."
  (let (counts-by-level)
    (dolist (err errors)
      (let* ((level (flycheck-error-level err))
             (item (assq level counts-by-level)))
        (if item
            (cl-incf (cdr item))
          (push (cons level 1) counts-by-level))))
    counts-by-level))

(defun flycheck-has-max-errors-p (errors level)
  "Check if there is no error in ERRORS more severe than LEVEL."
  (let ((severity (flycheck-error-level-severity level)))
    (seq-every-p (lambda (e) (<= (flycheck-error-level-severity
                                  (flycheck-error-level e))
                                 severity))
                 errors)))

(defun flycheck-has-max-current-errors-p (level)
  "Check if there is no current error more severe than LEVEL."
  (flycheck-has-max-errors-p flycheck-current-errors level))

(defun flycheck-has-errors-p (errors level)
  "Determine if there are any ERRORS with LEVEL."
  (seq-some (lambda (e) (eq (flycheck-error-level e) level)) errors))

(defun flycheck-has-current-errors-p (&optional level)
  "Determine if the current buffer has errors with LEVEL.

If LEVEL is omitted if the current buffer has any errors at all."
  (if level
      (flycheck-has-errors-p flycheck-current-errors level)
    (and flycheck-current-errors t)))


;;; Error overlays in the current buffer
(defvar-local flycheck--last-overlay-index 0
  "Last index given to a Flycheck overlay.

These indices are used to preserve error order (Emacs doesn't
preserve overlay order when calling `overlays-at').")

(defun flycheck--next-overlay-index ()
  "Compute the index to assign to a new Flycheck overlay."
  (cl-incf flycheck--last-overlay-index))

(defun flycheck--highlighting-style (err)
  "Determine the highlighting style to apply to ERR.

Styles are documented in `flycheck-highlighting-style'; this
functions resolves `conditional' style specifications."
  (let* ((style flycheck-highlighting-style)
         (first-line (flycheck-error-line err))
         (end-line (or (flycheck-error-end-line err) first-line))
         (nlines (- end-line first-line)))
    (while (eq (car-safe style) 'conditional)
      (pcase-let ((`(,threshold ,s1 ,s2) (cdr style)))
        (setq style (if (< nlines threshold) s1 s2))))
    (pcase style
      (`(delimiters ,before ,after)
       (when (characterp before)
         (setq before (flycheck--make-highlighting-delimiter before)))
       (when (characterp after)
         (setq after (flycheck--make-highlighting-delimiter after)))
       (setq style `(delimiters ,before ,after))))
    style))

(defun flycheck--setup-highlighting (err overlay)
  "Apply properties to OVERLAY to highlight ERR."
  (let ((level (flycheck-error-level err)))
    (unless flycheck-highlighting-mode
      ;; Erase the highlighting from the overlay if requested by the user
      (setf (overlay-get overlay 'face) nil))
    (when flycheck-indication-mode
      (setf (overlay-get overlay 'before-string)
            (flycheck-error-level-make-indicator
             level flycheck-indication-mode))
      (setf (overlay-get overlay 'line-prefix)
            (flycheck-error-level-make-indicator
             level flycheck-indication-mode t)))
    (pcase (flycheck--highlighting-style err)
      ((or `nil (guard (null flycheck-highlighting-mode)))
       ;; Erase the highlighting
       (setf (overlay-get overlay 'face) nil))
      (`level-face)
      (`(delimiters ,before ,after)
       ;; Replace the highlighting with delimiters
       (let* ((fringe-face (flycheck-error-level-fringe-face level))
              (delim-face `(flycheck-error-delimiter ,fringe-face)))
         (setf (overlay-get overlay 'face) 'flycheck-delimited-error)
         (setf (overlay-get overlay 'before-string)
               (concat (propertize before 'face delim-face)
                       (or (overlay-get overlay 'before-string) "")))
         (setf (overlay-get overlay 'after-string)
               (propertize after 'face delim-face))))
      (other (error "Unsupported highlighting style: %S" other)))))

(defun flycheck-add-overlay (err)
  "Add overlay for ERR.

Return the created overlay."
  ;; We must have a proper error region for the sake of fringe indication,
  ;; error display and error navigation, even if the highlighting is disabled.
  ;; We erase the highlighting later on in this case
  (pcase-let* ((`(,beg . ,end)
                (if (flycheck-relevant-error-other-file-p err)
                    ;; Display overlays for other-file errors on the first line
                    (cons (point-min)
                          (save-excursion (goto-char (point-min))
                                          (line-end-position)))
                  (flycheck-error-region-for-mode
                   err (or flycheck-highlighting-mode 'lines))))
               (overlay (make-overlay beg end))
               (level (flycheck-error-level err))
               (category (flycheck-error-level-overlay-category level))
               (index (flycheck--next-overlay-index)))
    (unless (flycheck-error-level-p level)
      (error "Undefined error level: %S" level))
    (setf (overlay-get overlay 'flycheck-error-index) index)
    (setf (overlay-get overlay 'flycheck-overlay) t)
    (setf (overlay-get overlay 'flycheck-error) err)
    (setf (overlay-get overlay 'category) category)
    (setf (overlay-get overlay 'help-echo) #'flycheck-help-echo)
    (flycheck--setup-highlighting err overlay)
    overlay))

(defun flycheck-help-echo (_window object pos)
  "Construct a tooltip message.

Most of the actual work is done by calling
`flycheck-help-echo-function' with the appropriate list of
errors.  Arguments WINDOW, OBJECT and POS are as described in
info node `(elisp)Special properties', as this function is
intended to be used as the \\='help-echo property of flycheck error
overlays."
  (-when-let (buf (cond ((bufferp object) object)
                        ((overlayp object) (overlay-buffer object))))
    (with-current-buffer buf
      (-when-let* ((fn flycheck-help-echo-function)
                   (errs (flycheck-overlay-errors-at pos)))
        (propertize (funcall fn errs) 'help-echo-inhibit-substitution t)))))

(defun flycheck-help-echo-all-error-messages (errs)
  "Concatenate error messages and ids from ERRS."
  (pcase (delq nil errs) ;; FIXME why would errors be nil here?
    (`(,err) ;; A single error
     (flycheck-error-format-message-and-id err))
    (_ ;; Zero or multiple errors
     (mapconcat
      (lambda (err)
        (flycheck-error-format-message-and-id err 'include-snippet))
      errs "\n"))))

(defun flycheck-filter-overlays (overlays)
  "Get all Flycheck overlays from OVERLAYS, in original order."
  ;; The order of errors returned from overlays is not stable, so we sort
  ;; them again using the internal index to guarantee errors are always
  ;; displayed in the same order.
  (seq-sort
   ;; We can't use `seq-sort-by' here; see above
   (lambda (o1 o2) (< (overlay-get o1 'flycheck-error-index)
                      (overlay-get o2 'flycheck-error-index)))
   (seq-filter (lambda (o) (overlay-get o 'flycheck-overlay)) overlays)))

(defun flycheck-overlays-at (pos)
  "Get all Flycheck overlays at POS."
  (flycheck-filter-overlays (overlays-at pos)))

(defun flycheck-overlays-in (beg end)
  "Get all Flycheck overlays between BEG and END."
  (flycheck-filter-overlays (overlays-in beg end)))

(defun flycheck-overlay-errors-at (pos)
  "Return a list of all flycheck errors overlaid at POS."
  (seq-map (lambda (o) (overlay-get o 'flycheck-error))
           (flycheck-overlays-at pos)))

(defun flycheck-overlay-errors-in (beg end)
  "Return a list of all flycheck errors overlaid between BEG and END."
  (seq-map (lambda (o) (overlay-get o 'flycheck-error))
           (flycheck-overlays-in beg end)))

(defvar-local flycheck-overlays-to-delete nil
  "Overlays mark for deletion after all syntax checks completed.")
(put 'flycheck-overlays-to-delete 'permanent-local t)

(defun flycheck-delete-all-overlays ()
  "Remove all flycheck overlays in the current buffer."
  (overlay-recenter (point-max))
  (flycheck-delete-marked-overlays)
  (setq flycheck--last-overlay-index 0)
  (save-restriction
    (widen)
    (seq-do #'delete-overlay (flycheck-overlays-in (point-min) (point-max)))))

(defun flycheck-mark-all-overlays-for-deletion ()
  "Mark all current overlays for deletion."
  (setq flycheck-overlays-to-delete
        (append (flycheck-overlays-in (point-min) (point-max))
                flycheck-overlays-to-delete)))

(defun flycheck-delete-marked-overlays ()
  "Delete all overlays marked for deletion."
  (overlay-recenter (point-max))
  (seq-do #'delete-overlay flycheck-overlays-to-delete)
  (setq flycheck-overlays-to-delete nil))


;;; Error navigation in the current buffer
(defun flycheck-error-level-interesting-at-pos-p (pos)
  "Check if error severity at POS passes `flycheck-error-level-interesting-p'."
  (flycheck-error-level-interesting-p (get-char-property pos 'flycheck-error)))

(defun flycheck-error-level-interesting-p (err)
  "Check if ERR severity is >= `flycheck-navigation-minimum-level'.

ERR is also interesting (the function returns true) if there are
no errors as or more severe than `flycheck-navigation-minimum-level'."
  (when (flycheck-error-p err)
    (-if-let (min-level flycheck-navigation-minimum-level)
        (or (<= (flycheck-error-level-severity min-level)
                (flycheck-error-level-severity (flycheck-error-level err)))
            (not (flycheck-has-current-errors-p min-level)))
      t)))

(defun flycheck-next-error-pos (n &optional reset)
  "Get the position of the N-th next error.

With negative N, get the position of the (-N)-th previous error
instead.  With non-nil RESET, search from `point-min', otherwise
search from the current point.

Return the position of the next or previous error, or nil if
there is none.  If N is zero, return `point', or `point-min' if
RESET is non-nil."
  (let ((n (or n 1))
        (pos (if reset (point-min) (point))))
    (if (>= n 0)
        ;; Search forwards
        (while (and pos (> n 0))
          (setq n (1- n))
          (when (get-char-property pos 'flycheck-error)
            ;; Move beyond from the current error if any
            (setq pos (next-single-char-property-change pos 'flycheck-error)))
          (while (not (or (= pos (point-max))
                          (flycheck-error-level-interesting-at-pos-p pos)))
            ;; Scan for the next error
            (setq pos (next-single-char-property-change pos 'flycheck-error)))
          (when (and (= pos (point-max))
                     (not (flycheck-error-level-interesting-at-pos-p pos)))
            ;; If we reached the end of the buffer, but no error, we didn't find
            ;; any
            (setq pos nil)))
      ;; Search backwards
      (while (and pos (< n 0))
        (setq n (1+ n))
        ;; Loop until we find an error.  We need to check the position *before*
        ;; the current one, because `previous-single-char-property-change'
        ;; always moves to the position *of* the change.
        (while (not (or (= pos (point-min))
                        (flycheck-error-level-interesting-at-pos-p (1- pos))))
          (setq pos (previous-single-char-property-change pos 'flycheck-error)))
        (when (and (= pos (point-min))
                   (not (flycheck-error-level-interesting-at-pos-p pos)))
          ;; We didn't find any error.
          (setq pos nil))
        (when pos
          ;; We found an error, so move to its beginning
          (setq pos (previous-single-char-property-change pos
                                                          'flycheck-error)))))
    pos))

(defun flycheck-next-error-function (n reset)
  "Visit the N-th error from the current point.

N is the number of errors to advance by, where a negative N
advances backwards.  With non-nil RESET, advance from the
beginning of the buffer, otherwise advance from the current
position.

Intended for use with `next-error-function'."
  (-if-let* ((pos (flycheck-next-error-pos n reset))
             (err (get-char-property pos 'flycheck-error)))
      (flycheck-jump-to-error err)
    (user-error "No more Flycheck errors")))

(defun flycheck-next-error (&optional n reset)
  "Visit the N-th error from the current point.

N is the number of errors to advance by, where a negative N
advances backwards.  With non-nil RESET, advance from the
beginning of the buffer, otherwise advance from the current
position."
  (interactive "P")
  (when (consp n)
    ;; Universal prefix argument means reset
    (setq reset t n nil))
  (flycheck-next-error-function n reset)
  (flycheck-display-error-at-point))

(defun flycheck-previous-error (&optional n)
  "Visit the N-th previous error.

If given, N specifies the number of errors to move backwards by.
If N is negative, move forwards instead."
  (interactive "P")
  (flycheck-next-error (- (or n 1))))

(defun flycheck-first-error (&optional n)
  "Visit the N-th error from beginning of the buffer.

If given, N specifies the number of errors to move forward from
the beginning of the buffer."
  (interactive "P")
  (flycheck-next-error n 'reset))


;;; Listing errors in buffers
(defconst flycheck-error-list-buffer "*Flycheck errors*"
  "The name of the buffer to show error lists.")

(defmacro flycheck-error-list-with-buffer (&rest body)
  "Evaluate BODY in flycheck-error-list-buffer, if it exists."
  (declare (indent 0) (debug t))
  `(when (get-buffer flycheck-error-list-buffer)
     (with-current-buffer flycheck-error-list-buffer
       ,@body)))

(defvar flycheck-error-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'flycheck-error-list-set-filter)
    (define-key map (kbd "F") #'flycheck-error-list-reset-filter)
    (define-key map (kbd "n") #'flycheck-error-list-next-error)
    (define-key map (kbd "p") #'flycheck-error-list-previous-error)
    (define-key map (kbd "g") #'flycheck-error-list-check-source)
    (define-key map (kbd "e") #'flycheck-error-list-explain-error)
    (define-key map (kbd "RET") #'flycheck-error-list-goto-error)
    map)
  "The keymap of `flycheck-error-list-mode'.")

(defun flycheck-error-list-make-last-column (message checker)
  "Compute contents of the last error list cell.

MESSAGE and CHECKER are displayed in a single column to allow the
message to stretch arbitrarily far."
  (let ((checker-name (propertize (symbol-name checker)
                                  'face 'flycheck-error-list-checker-name))
        (message (propertize message
                             'face 'flycheck-error-list-error-message)))
    (format "%s (%s)" message checker-name)))

(defconst flycheck-error-list-format
  `[("File" 6)
    ("Line" 5 flycheck-error-list-entry-< :right-align t)
    ("Col" 3 nil :right-align t)
    ("Level" 8 flycheck-error-list-entry-level-<)
    ("ID" 6 t)
    (,(flycheck-error-list-make-last-column "Message" 'Checker) 0 t)]
  "Table format for the error list.")

(defconst flycheck-error-list-padding 1
  "Padding used in error list.")

(defconst flycheck--error-list-msg-offset
  (seq-reduce
   (lambda (offset fmt)
     (pcase-let* ((`(,_ ,width ,_ . ,props) fmt)
                  (padding (or (plist-get props :pad-right) 1)))
       (+ offset width padding)))
   (seq-subseq flycheck-error-list-format 0 -1)
   flycheck-error-list-padding)
  "Amount of space to use in `flycheck-flush-multiline-message'.")

(define-derived-mode flycheck-error-list-mode tabulated-list-mode
  "Flycheck errors"
  "Major mode for listing Flycheck errors.

\\{flycheck-error-list-mode-map}"
  (setq tabulated-list-format flycheck-error-list-format
        ;; Sort by location initially
        tabulated-list-sort-key (cons "Line" nil)
        tabulated-list-padding flycheck-error-list-padding
        tabulated-list-entries #'flycheck-error-list-entries
        ;; `revert-buffer' updates the mode line for us, so all we need to do is
        ;; set the corresponding mode line construct.
        mode-line-buffer-identification flycheck-error-list-mode-line)
  ;; Guard `truncate-string-ellipsis' for Emacs 24.
  ;; TODO: Remove when dropping Emacs 24 compatibility
  (when (boundp 'truncate-string-ellipsis)
    ;; See https://github.com/flycheck/flycheck/issues/1101
    (setq-local truncate-string-ellipsis "…"))
  (tabulated-list-init-header))

(defvar-local flycheck-error-list-source-buffer nil
  "The current source buffer of the error list.")
;; Needs to permanently local to preserve the source buffer across buffer
;; reversions
(put 'flycheck-error-list-source-buffer 'permanent-local t)

(defun flycheck-error-list-set-source (buffer)
  "Set BUFFER as the source buffer of the error list."
  (flycheck-error-list-with-buffer
    (setq flycheck-error-list-source-buffer buffer)
    (flycheck-error-list-refresh)))

(defun flycheck-error-list-update-source ()
  "Make the error list display errors from the current buffer.

The update is skipped if the current buffer is the error list or
if the error list is already pointing to the current buffer."
  (unless (memq (current-buffer)
                (list (get-buffer flycheck-error-list-buffer)
                      (flycheck-error-list-with-buffer
                        flycheck-error-list-source-buffer)))
    (flycheck-error-list-set-source (current-buffer))))

(defun flycheck-error-list-check-source ()
  "Trigger a syntax check in the source buffer of the error list."
  (interactive)
  (let ((buffer (get-buffer flycheck-error-list-source-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (flycheck-buffer)))))

(define-button-type 'flycheck-error-list
  'action #'flycheck-error-list-goto-error
  'help-echo "mouse-1, RET: goto error"
  'face nil)

(define-button-type 'flycheck-error-list-explain-error
  'action #'flycheck-error-list-explain-error
  'help-echo "mouse-1, RET: explain error")

(defsubst flycheck-error-list-make-cell (text &optional face help-echo type)
  "Make an error list cell with TEXT and FACE.

If FACE is nil don't set a FACE on TEXT.  If TEXT already has
face properties, do not specify a FACE.  Note though, that if
TEXT gets truncated it will not inherit any previous face
properties.  If you expect TEXT to be truncated in the error
list, do specify a FACE explicitly!

If HELP-ECHO is non-nil, set a help-echo property on TEXT, with
value HELP-ECHO.  This is convenient if you expect TEXT to be
truncated.

The cell will have the type TYPE unless TYPE is nil, and the
default type `flycheck-error-list' will be used instead."
  (append (list text 'type (if type type
                             'flycheck-error-list))
          (and face (list 'face face))
          (and help-echo (list 'help-echo help-echo))))

(defsubst flycheck-error-list-make-number-cell (number face)
  "Make a table cell for a NUMBER with FACE.

Convert NUMBER to string, fontify it with FACE and return the
string with attached text properties."
  (flycheck-error-list-make-cell
   (if (numberp number) (number-to-string number) "")
   face))

(defun flycheck-error-list-make-entry (error)
  "Make a table cell for the given ERROR.

Return a list with the contents of the table cell."
  (let* ((level (flycheck-error-level error))
         (level-face (flycheck-error-level-error-list-face level))
         (filename (flycheck-error-filename error))
         (line (flycheck-error-line error))
         (column (flycheck-error-column error))
         (message (or (flycheck-error-message error)
                      (format "Unknown %S" level)))
         (flushed-msg (flycheck-flush-multiline-message message))
         (id (flycheck-error-id error))
         (id-str (if id (format "%s" id) ""))
         (checker (flycheck-error-checker error))
         (msg-and-checker
          (flycheck-error-list-make-last-column flushed-msg checker))
         (explainer (flycheck-checker-get checker 'error-explainer)))
    (list error
          (vector (flycheck-error-list-make-cell
                   (if filename
                       (file-name-nondirectory filename)
                     "")
                   'flycheck-error-list-filename)
                  (flycheck-error-list-make-number-cell
                   line 'flycheck-error-list-line-number)
                  (flycheck-error-list-make-number-cell
                   column 'flycheck-error-list-column-number)
                  (flycheck-error-list-make-cell
                   (symbol-name (flycheck-error-level error)) level-face)
                  ;; Error ID use a different face when an error-explainer is
                  ;; present
                  (flycheck-error-list-make-cell
                   id-str (if explainer 'flycheck-error-list-id-with-explainer
                            'flycheck-error-list-id)
                   id-str 'flycheck-error-list-explain-error)
                  (flycheck-error-list-make-cell
                   msg-and-checker nil msg-and-checker)))))

(defun flycheck-flush-multiline-message (msg)
  "Prepare error message MSG for display in the error list.

Prepend all lines of MSG except the first with enough space to
ensure that they line up properly once the message is displayed."
  (let* ((spc-spec `(space . (:width ,flycheck--error-list-msg-offset)))
         (spc (propertize " " 'display spc-spec))
         (rep (concat "\\1" spc "\\2")))
    (replace-regexp-in-string "\\([\r\n]+\\)\\(.\\)" rep msg)))

(defun flycheck-error-list-current-errors ()
  "Read the list of errors in `flycheck-error-list-source-buffer'."
  (when (buffer-live-p flycheck-error-list-source-buffer)
    (buffer-local-value 'flycheck-current-errors
                        flycheck-error-list-source-buffer)))

(defun flycheck-error-list-entries ()
  "Create the entries for the error list."
  (-when-let* ((errors (flycheck-error-list-current-errors))
               (filtered (flycheck-error-list-apply-filter errors)))
    (seq-map #'flycheck-error-list-make-entry filtered)))

(defun flycheck-error-list-entry-< (entry1 entry2)
  "Determine whether ENTRY1 is before ENTRY2 by location.

See `flycheck-error-<'."
  (flycheck-error-< (car entry1) (car entry2)))

(defun flycheck-error-list-entry-level-< (entry1 entry2)
  "Determine whether ENTRY1 is before ENTRY2 by level.

See `flycheck-error-level-<'."
  (not (flycheck-error-level-< (car entry1) (car entry2))))

(defvar flycheck-error-list-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      #'flycheck-error-list-mouse-switch-to-source)
    map)
  "Keymap for error list mode line.")

(defun flycheck-error-list-propertized-source-name ()
  "Get the name of the current source buffer for the mode line.

Propertize the name of the current source buffer for use in the
mode line indication of `flycheck-error-list-mode'."
  (let ((name (replace-regexp-in-string
               (rx "%") "%%"
               (buffer-name flycheck-error-list-source-buffer)
               'fixed-case 'literal)))
    (propertize name 'face 'mode-line-buffer-id
                'mouse-face 'mode-line-highlight
                'help-echo "mouse-1: switch to source"
                'local-map flycheck-error-list-mode-line-map)))

(defun flycheck-error-list-mouse-switch-to-source (event)
  "Switch to the error list source buffer of the EVENT window."
  (interactive "e")
  (save-selected-window
    (when (eventp event)
      (select-window (posn-window (event-start event))))
    (when (buffer-live-p flycheck-error-list-source-buffer)
      (switch-to-buffer flycheck-error-list-source-buffer))))

(defun flycheck-get-error-list-window-list (&optional all-frames)
  "Get all windows displaying the error list.

ALL-FRAMES specifies the frames to consider, as in
`get-buffer-window-list'."
  (-when-let (buf (get-buffer flycheck-error-list-buffer))
    (get-buffer-window-list buf nil all-frames)))

(defun flycheck-get-error-list-window (&optional all-frames)
  "Get a window displaying the error list, or nil if none.

ALL-FRAMES specifies the frames to consider, as in
`get-buffer-window'."
  (-when-let (buf (get-buffer flycheck-error-list-buffer))
    (get-buffer-window buf all-frames)))

(defun flycheck-error-list-recenter-at (pos)
  "Recenter the error list at POS."
  (dolist (window (flycheck-get-error-list-window-list t))
    (with-selected-window window
      (goto-char pos)
      (let ((recenter-redisplay nil))
        (recenter)))))

(defun flycheck-error-list-refresh ()
  "Refresh the current error list.

Add all errors currently reported for the current
`flycheck-error-list-source-buffer', and recenter the error
list."
  ;; We only refresh the error list, when it is visible in a window, and we
  ;; select this window while reverting, because Tabulated List mode attempts to
  ;; recenter the error at the old location, so it must have the proper window
  ;; selected.
  (-when-let (window (flycheck-get-error-list-window t))
    (with-selected-window window
      (revert-buffer))
    (run-hooks 'flycheck-error-list-after-refresh-hook)
    (let ((preserve-pos (eq (current-buffer)
                            (get-buffer flycheck-error-list-buffer))))
      ;; If the error list is the current buffer, don't recenter when
      ;; highlighting
      (flycheck-error-list-highlight-errors preserve-pos))))

(defun flycheck-error-list-mode-line-filter-indicator ()
  "Create a string representing the current error list filter."
  (if flycheck-error-list-minimum-level
      (format " [>= %s]" flycheck-error-list-minimum-level)
    ""))

(defun flycheck-error-list-set-filter (level)
  "Restrict the error list to errors at level LEVEL or higher.

LEVEL is either an error level symbol, or nil, to remove the filter."
  (interactive
   (list (flycheck-read-error-level
          "Minimum error level (errors at lower levels will be hidden): ")))
  (when (and level (not (flycheck-error-level-p level)))
    (user-error "Invalid level: %s" level))
  (flycheck-error-list-with-buffer
    (setq-local flycheck-error-list-minimum-level level)
    (force-mode-line-update)
    (flycheck-error-list-refresh)
    (flycheck-error-list-recenter-at (point-min))))

(defun flycheck-error-list-reset-filter (&optional refresh)
  "Remove local error filters and reset to the default filter.

Interactively, or with non-nil REFRESH, refresh the error list."
  (interactive '(t))
  (flycheck-error-list-with-buffer
    (kill-local-variable 'flycheck-error-list-minimum-level)
    (when refresh
      (flycheck-error-list-refresh)
      (flycheck-error-list-recenter-at (point-min))
      (force-mode-line-update))))

(defun flycheck-error-list-apply-filter (errors)
  "Filter ERRORS according to `flycheck-error-list-minimum-level'."
  (-if-let* ((min-level flycheck-error-list-minimum-level)
             (min-severity (flycheck-error-level-severity min-level)))
      (seq-filter (lambda (err) (>= (flycheck-error-level-severity
                                     (flycheck-error-level err))
                                    min-severity))
                  errors)
    errors))

(defun flycheck-error-list-goto-error (&optional pos)
  "Go to the location of the error at POS in the error list.

POS defaults to `point'."
  (interactive)
  (-when-let* ((error (tabulated-list-get-id pos)))
    (flycheck-jump-to-error error)))

(defun flycheck-jump-to-error (error)
  "Go to the location of ERROR."
  (let* ((error-copy (copy-flycheck-error error))
         (filename (flycheck-error-filename error))
         (other-file-error (flycheck-relevant-error-other-file-p error))
         (buffer (if filename
                     (find-file-noselect filename)
                   (flycheck-error-buffer error))))
    (when (buffer-live-p buffer)
      (setf (flycheck-error-buffer error-copy) buffer)
      (flycheck-jump-in-buffer buffer error-copy)
      ;; When jumping to an error in another file, it may not have
      ;; this error available for highlighting yet, so we trigger a check
      ;; if necessary.
      (when other-file-error
        (with-current-buffer buffer
          ;; `seq-contains-p' is only in seq >= 2.21
          (unless (with-no-warnings
                    (seq-contains flycheck-current-errors error-copy 'equal))
            (when flycheck-mode
              (flycheck-buffer))))))))

(defun flycheck-jump-in-buffer (buffer error)
  "In BUFFER, jump to ERROR."
  ;; FIXME: we assume BUFFER and the buffer of ERROR are the same.  We don't
  ;; need the first argument then.
  (if (eq (window-buffer) (get-buffer flycheck-error-list-buffer))
      ;; When called from within the error list, keep the error list,
      ;; otherwise replace the current buffer.
      (pop-to-buffer buffer 'other-window)
    (switch-to-buffer buffer))
  (let ((pos (flycheck-error-pos error)))
    (unless (eq (goto-char pos) (point))
      ;; If widening gets in the way of moving to the right place, remove it
      ;; and try again
      (widen)
      (goto-char pos)))
  ;; Re-highlight the errors.  We have post-command-hook for that, but calls to
  ;; `flycheck-jump-in-buffer' that come from other buffers (e.g. from the error
  ;; list) won't trigger it.
  (flycheck-error-list-highlight-errors 'preserve-pos))

(defun flycheck-error-list-explain-error (&optional pos)
  "Explain the error at POS in the error list.

POS defaults to `point'."
  (interactive)
  (-when-let* ((error (tabulated-list-get-id pos))
               (explainer (flycheck-checker-get (flycheck-error-checker error)
                                                'error-explainer)))
    (flycheck-error-with-buffer error
      (-when-let (explanation (funcall explainer error))
        (flycheck-display-error-explanation explanation)))))

(defun flycheck-error-list-next-error-pos (pos &optional n)
  "Starting from POS get the N'th next error in the error list.

N defaults to 1.  If N is negative, search for the previous error
instead.

Get the beginning position of the N'th next error from POS, or
nil, if there is no next error."
  (let ((n (or n 1)))
    (if (>= n 0)
        ;; Search forward
        (while (and pos (/= n 0))
          (setq n (1- n))
          (setq pos (next-single-property-change pos 'tabulated-list-id)))
      ;; Search backwards
      (while (/= n 0)
        (setq n (1+ n))
        ;; We explicitly give the limit here to explicitly have the minimum
        ;; point returned, to be able to move to the first error (which starts
        ;; at `point-min')
        (setq pos (previous-single-property-change pos 'tabulated-list-id
                                                   nil (point-min)))))
    pos))

(defun flycheck-error-list-previous-error (n)
  "Go to the N'th previous error in the error list."
  (interactive "P")
  (flycheck-error-list-next-error (- (or n 1))))

(defun flycheck-error-list-next-error (n)
  "Go to the N'th next error in the error list."
  (interactive "P")
  (let ((pos (flycheck-error-list-next-error-pos (point) n)))
    (when (and pos (/= pos (point)))
      (goto-char pos)
      (save-selected-window
        ;; Keep the error list selected, so that the user can navigate errors by
        ;; repeatedly pressing n/p, without having to re-select the error list
        ;; window.
        (flycheck-error-list-goto-error)))))

(defvar-local flycheck-error-list-highlight-overlays nil
  "Error highlight overlays in the error list buffer.")
(put 'flycheck-error-list-highlight-overlays 'permanent-local t)

(defun flycheck-error-list-highlight-errors (&optional preserve-pos)
  "Highlight errors in the error list.

Highlight all errors in the error list that are at point in the
source buffer, and on the same line as point.  Then recenter the
error list to the highlighted error, unless PRESERVE-POS is
non-nil."
  (when (get-buffer flycheck-error-list-buffer)
    (with-current-buffer flycheck-error-list-buffer
      (let ((current-errors
             (when (buffer-live-p flycheck-error-list-source-buffer)
               (with-current-buffer flycheck-error-list-source-buffer
                 (flycheck-overlay-errors-in (line-beginning-position)
                                             (line-end-position))))))
        (let ((old-overlays flycheck-error-list-highlight-overlays)
              (min-point (point-max))
              (max-point (point-min)))
          ;; Display the new overlays first, to avoid re-display flickering
          (setq flycheck-error-list-highlight-overlays nil)
          (when current-errors
            (let ((next-error-pos (point-min)))
              (while next-error-pos
                (let* ((beg next-error-pos)
                       (end (flycheck-error-list-next-error-pos beg))
                       (err (tabulated-list-get-id beg)))
                  (when (member err current-errors)
                    (setq min-point (min min-point beg)
                          max-point (max max-point beg))
                    (let ((ov (make-overlay beg
                                            ;; Extend overlay to the beginning
                                            ;; of the next line, to highlight
                                            ;; the whole line
                                            (or end (point-max)))))
                      (push ov flycheck-error-list-highlight-overlays)
                      (setf (overlay-get ov 'flycheck-error-highlight-overlay)
                            t)
                      (setf (overlay-get ov 'face)
                            'flycheck-error-list-highlight)))
                  (setq next-error-pos end)))))
          ;; Delete the old overlays
          (seq-do #'delete-overlay old-overlays)
          (when (and (not preserve-pos) current-errors)
            ;; Move point to the middle error
            (goto-char (+ min-point (/ (- max-point min-point) 2)))
            (beginning-of-line)
            ;; And recenter the error list at this position
            (flycheck-error-list-recenter-at (point))))))))

(defun flycheck-list-errors ()
  "Show the error list for the current buffer."
  (interactive)
  (unless flycheck-mode
    (user-error "Flycheck mode not enabled"))
  ;; Create and initialize the error list
  (unless (get-buffer flycheck-error-list-buffer)
    (with-current-buffer (get-buffer-create flycheck-error-list-buffer)
      (flycheck-error-list-mode)))
  ;; Reset the error filter
  (flycheck-error-list-reset-filter)
  (let ((source (current-buffer)))
    ;; Show the error list in a side window.  Under some configurations of
    ;; `display-buffer', this may select `flycheck-error-list-buffer' (see URL
    ;; `https://github.com/flycheck/flycheck/issues/1776').
    (display-buffer flycheck-error-list-buffer)
    ;; Adjust the source, causing a refresh
    (flycheck-error-list-set-source source)))

(defalias 'list-flycheck-errors 'flycheck-list-errors)


;;; Displaying errors in the current buffer
(defun flycheck-display-errors (errors)
  "Display ERRORS using `flycheck-display-errors-function'."
  (when flycheck-display-errors-function
    (funcall flycheck-display-errors-function errors)))

(defvar-local flycheck-display-error-at-point-timer nil
  "Timer to automatically show errors.")

(defun flycheck-cancel-error-display-error-at-point-timer ()
  "Cancel the error display timer for the current buffer."
  (when flycheck-display-error-at-point-timer
    (cancel-timer flycheck-display-error-at-point-timer)
    (setq flycheck-display-error-at-point-timer nil)))

(defun flycheck--error-display-tick ()
  "Return point and tick counter of current buffer."
  (cons (point) (buffer-modified-tick)))

(defvar-local flycheck--last-error-display-tick nil
  "Value of `flycheck--error-display-tick' when errors were last displayed.")

(defun flycheck-display-error-at-point ()
  "Display all the error messages at point."
  (interactive)
  ;; This function runs from a timer, so we must take care to not ignore any
  ;; errors
  (with-demoted-errors "Flycheck error display error: %s"
    (flycheck-cancel-error-display-error-at-point-timer)
    (setq flycheck--last-error-display-tick (flycheck--error-display-tick))
    (when flycheck-mode
      (-when-let (errors (flycheck-overlay-errors-at (point)))
        (flycheck-display-errors errors)))))

(defun flycheck-display-error-at-point-soon ()
  "Display error messages at point, with a delay."
  (setq flycheck--last-error-display-tick nil)
  (flycheck-maybe-display-error-at-point-soon))

(defun flycheck-maybe-display-error-at-point-soon ()
  "Display error message at point with a delay, unless already displayed."
  (flycheck-cancel-error-display-error-at-point-timer)
  (when (and (not (equal flycheck--last-error-display-tick
                         (setq flycheck--last-error-display-tick
                               (flycheck--error-display-tick))))
             (flycheck-overlays-at (point)))
    (setq flycheck-display-error-at-point-timer
          (run-at-time flycheck-display-errors-delay nil
                       'flycheck-display-error-at-point))))


;;; Functions to display errors
(defconst flycheck-error-message-buffer "*Flycheck error messages*"
  "The name of the buffer to show long error messages in.")

(defun flycheck-error-message-buffer ()
  "Get the buffer object to show long error messages in.

Get the buffer named by variable `flycheck-error-message-buffer',
or nil if the buffer does not exist."
  (get-buffer flycheck-error-message-buffer))

(defun flycheck-may-use-echo-area-p ()
  "Determine whether the echo area may be used.

The echo area may be used if the cursor is not in the echo area,
and if the echo area is not occupied by minibuffer input."
  (not (or cursor-in-echo-area (active-minibuffer-window))))

(define-derived-mode flycheck-error-message-mode text-mode
  "Flycheck error messages"
  "Major mode for extended error messages.")

(defun flycheck-display-error-messages (errors)
  "Display the messages of ERRORS.

Concatenate all non-nil messages of ERRORS as with
`flycheck-help-echo-all-error-messages', and display them with
`display-message-or-buffer', which shows the messages either in
the echo area or in a separate buffer, depending on the number of
lines.  See Info node `(elisp)Displaying Messages' for more
information.

In the latter case, show messages in the buffer denoted by
variable `flycheck-error-message-buffer'."
  (when (and errors (flycheck-may-use-echo-area-p))
    (let ((message (flycheck-help-echo-all-error-messages errors)))
      (display-message-or-buffer
       message flycheck-error-message-buffer 'not-this-window)
      ;; We cannot rely on `display-message-or-buffer' returning the right
      ;; window. See URL `https://github.com/flycheck/flycheck/issues/1643'.
      (-when-let (buf (get-buffer flycheck-error-message-buffer))
        (with-current-buffer buf
          (unless (derived-mode-p 'flycheck-error-message-mode)
            (flycheck-error-message-mode)))))))

(defun flycheck-display-error-messages-unless-error-list (errors)
  "Show messages of ERRORS unless the error list is visible.

Like `flycheck-display-error-messages', but only if the error
list (see `flycheck-list-errors') is not visible in any window in
the current frame."
  (unless (flycheck-get-error-list-window 'current-frame)
    (flycheck-display-error-messages errors)))

(defun flycheck-hide-error-buffer ()
  "Hide the Flycheck error buffer if necessary.

Hide the error buffer if there is no error under point."
  (-when-let* ((buffer (flycheck-error-message-buffer))
               (window (get-buffer-window buffer)))
    (unless (flycheck-overlays-at (point))
      ;; save-selected-window prevents `quit-window' from changing the current
      ;; buffer (see https://github.com/flycheck/flycheck/issues/648).
      (save-selected-window
        (quit-window nil window)))))


;;; Working with errors
(defun flycheck-copy-errors-as-kill (pos &optional formatter)
  "Copy each error at POS into kill ring, using FORMATTER.

FORMATTER is a function to turn an error into a string,
defaulting to `flycheck-error-message'.

Interactively, use `flycheck-error-format-message-and-id' as
FORMATTER with universal prefix arg, and `flycheck-error-id' with
normal prefix arg, i.e. copy the message and the ID with
universal prefix arg, and only the id with normal prefix arg."
  (interactive (list (point)
                     (pcase current-prefix-arg
                       ((pred not) #'flycheck-error-message)
                       ((pred consp) #'flycheck-error-format-message-and-id)
                       (_ #'flycheck-error-id))))
  (let ((messages (delq nil (seq-map (or formatter #'flycheck-error-message)
                                     (flycheck-overlay-errors-at pos)))))
    (when messages
      (seq-do #'kill-new (reverse messages))
      (message (string-join messages "\n")))))

(defun flycheck-explain-error-at-point ()
  "Display an explanation for the first explainable error at point.

The first explainable error at point is the first error at point
with a non-nil `:error-explainer' function defined in its
checker.  The `:error-explainer' function is then called with
this error to produce the explanation to display."
  (interactive)
  (-when-let* ((first-error
                ;; Get the first error at point that has an `error-explainer'.
                (seq-find (lambda (error)
                            (flycheck-checker-get
                             (flycheck-error-checker error) 'error-explainer))
                          (flycheck-overlay-errors-at (point))))
               (explainer
                (flycheck-checker-get (flycheck-error-checker first-error)
                                      'error-explainer))
               (explanation (funcall explainer first-error)))
    (flycheck-display-error-explanation explanation)))

(defconst flycheck-explain-error-buffer "*Flycheck error explanation*"
  "The name of the buffer to show error explanations.")

(define-derived-mode flycheck-explain-error-mode help-mode
  "Error explanation"
  "Major mode for displaying error explanations."
  (setq buffer-read-only t))

(defun flycheck-display-error-explanation (explanation)
  "Display the EXPLANATION for an error."
  (pcase explanation
    (`nil)
    (`(url . ,url) (browse-url url))
    (_ (let ((inhibit-read-only t)
             (standard-output (temp-buffer-window-setup
                               flycheck-explain-error-buffer)))
         (with-current-buffer standard-output
           (flycheck-explain-error-mode))
         (cond
          ((functionp explanation) (funcall explanation))
          ((stringp explanation) (princ explanation))
          (t (error "Unsupported error explanation: %S" explanation)))
         (display-message-or-buffer standard-output nil 'not-this-window)))))


;;; Syntax checkers using external commands
(defun flycheck-command-argument-p (arg)
  "Check whether ARG is a valid command argument."
  (pcase arg
    ((pred stringp) t)
    ((or `source `source-inplace `source-original) t)
    (`(,(or `source `source-inplace) ,suffix)
     (stringp suffix))
    ((or `temporary-directory `temporary-file-name) t)
    (`null-device t)
    (`(config-file ,option-name ,config-file-var)
     (and (stringp option-name)
          (symbolp config-file-var)))
    (`(config-file ,option-name ,config-file-var ,prepender)
     (and (stringp option-name)
          (symbolp config-file-var)
          (symbolp prepender)))
    (`(,(or `option `option-list) ,option-name ,option-var)
     (and (stringp option-name)
          (symbolp option-var)))
    (`(,(or `option `option-list) ,option-name ,option-var ,prepender)
     (and (stringp option-name)
          (symbolp option-var)
          (symbolp prepender)))
    (`(,(or `option `option-list) ,option-name ,option-var ,prepender ,filter)
     (and (stringp option-name)
          (symbolp option-var)
          (symbolp prepender)
          (symbolp filter)))
    (`(option-flag ,option-name ,option-var)
     (and (stringp option-name)
          (symbolp option-var)))
    (`(eval ,_) t)
    (_ nil)))

(defun flycheck-compute-working-directory (checker)
  "Get the default working directory for CHECKER.

Compute the value of `default-directory' for the invocation of
the syntax checker command, by calling the function in the
`working-directory' property of CHECKER, with CHECKER as sole
argument, and returning its value.  Signal an error if the
function returns a non-existing working directory.

If the property is undefined or if the function returns nil
return the `default-directory' of the current buffer."
  (let* ((def-directory-fn (flycheck-checker-get checker 'working-directory))
         (directory (or (and def-directory-fn
                             (funcall def-directory-fn checker))
                        ;; Default to the `default-directory' of the current
                        ;; buffer
                        default-directory)))
    (unless (file-exists-p directory)
      (error ":working-directory %s of syntax checker %S does not exist"
             directory checker))
    directory))

;;;###autoload
(defun flycheck-define-command-checker (symbol docstring &rest properties)
  "Define SYMBOL as syntax checker to run a command.

Define SYMBOL as generic syntax checker via
`flycheck-define-generic-checker', which uses an external command
to check the buffer.  SYMBOL and DOCSTRING are the same as for
`flycheck-define-generic-checker'.

In addition to the properties understood by
`flycheck-define-generic-checker', the following PROPERTIES
constitute a command syntax checker.  Unless otherwise noted, all
properties are mandatory.  Note that the default `:error-filter'
of command checkers is `flycheck-sanitize-errors'.

`:command COMMAND'
     The command to run for syntax checking.

     COMMAND is a list of the form `(EXECUTABLE [ARG ...])'.

     EXECUTABLE is a string with the executable of this syntax
     checker.  It can be overridden with the variable
     `flycheck-SYMBOL-executable'.  Note that this variable is
     NOT implicitly defined by this function.  Use
     `flycheck-def-executable-var' to define this variable.

     Each ARG is an argument to the executable, either as string,
     or as special symbol or form for
     `flycheck-substitute-argument', which see.

`:error-patterns PATTERNS'
     A list of patterns to parse the output of the `:command'.

     Each ITEM in PATTERNS is a list `(LEVEL SEXP ...)', where
     LEVEL is a Flycheck error level (see
     `flycheck-define-error-level'), followed by one or more RX
     `SEXP's which parse an error of that level and extract line,
     column, file name and the message.

     See `rx' for general information about RX, and
     `flycheck-rx-to-string' for some special RX forms provided
     by Flycheck.

     All patterns are applied in the order of declaration to the
     whole output of the syntax checker.  Output already matched
     by a pattern will not be matched by subsequent patterns.  In
     other words, the first pattern wins.

     This property is optional.  If omitted, however, an
     `:error-parser' is mandatory.

`:error-parser FUNCTION'
     A function to parse errors with.

     The function shall accept three arguments OUTPUT CHECKER
     BUFFER.  OUTPUT is the syntax checker output as string,
     CHECKER the syntax checker that was used, and BUFFER a
     buffer object representing the checked buffer.  The function
     must return a list of `flycheck-error' objects parsed from
     OUTPUT.

     This property is optional.  If omitted, it defaults to
     `flycheck-parse-with-patterns'.  In this case,
     `:error-patterns' is mandatory.

`:standard-input t'
     Whether to send the buffer contents on standard input.

     If this property is given and has a non-nil value, send the
     contents of the buffer on standard input.

     Defaults to nil.

Note that you may not give `:start', `:interrupt', and
`:print-doc' for a command checker.  You can give a custom
`:verify' function, though, whose results will be appended to the
default `:verify' function of command checkers."
  (declare (indent 1)
           (doc-string 2))
  (dolist (prop '(:start :interrupt :print-doc))
    (when (plist-get properties prop)
      (error "%s not allowed in definition of command syntax checker %s"
             prop symbol)))

  (unless (plist-get properties :error-filter)
    ;; Default to `flycheck-sanitize-errors' as error filter
    (setq properties (plist-put properties :error-filter
                                #'flycheck-sanitize-errors)))
  (let ((verify-fn (plist-get properties :verify)))
    (setq properties
          (plist-put properties :verify
                     (lambda (checker)
                       (append (flycheck-verify-command-checker checker)
                               (and verify-fn
                                    (funcall verify-fn checker)))))))

  (let ((command (plist-get properties :command))
        (patterns (plist-get properties :error-patterns))
        (parser (or (plist-get properties :error-parser)
                    #'flycheck-parse-with-patterns))
        (enabled (plist-get properties :enabled))
        (standard-input (plist-get properties :standard-input)))
    (unless command
      (error "Missing :command in syntax checker %s" symbol))
    (unless (stringp (car command))
      (error "Command executable for syntax checker %s must be a string: %S"
             symbol (car command)))
    (dolist (arg (cdr command))
      (unless (flycheck-command-argument-p arg)
        (error "Invalid command argument %S in syntax checker %s" arg symbol)))
    (when (and (eq parser 'flycheck-parse-with-patterns)
               (not patterns))
      (error "Missing :error-patterns in syntax checker %s" symbol))

    (setq properties
          ;; Automatically disable command checkers if the executable does not
          ;; exist.
          (plist-put properties :enabled
                     (lambda ()
                       (and (flycheck-find-checker-executable symbol)
                            (flycheck-temp-files-writable-p symbol)
                            (or (not enabled) (funcall enabled))))))

    (apply #'flycheck-define-generic-checker symbol docstring
           :start #'flycheck-start-command-checker
           :interrupt #'flycheck-interrupt-command-checker
           :print-doc #'flycheck-command-checker-print-doc
           properties)

    ;; Pre-compile all errors patterns into strings, so that we don't need to do
    ;; that on each error parse
    (let ((patterns (seq-map (lambda (p)
                               (cons (flycheck-rx-to-string `(and ,@(cdr p))
                                                            'no-group)
                                     (car p)))
                             patterns)))
      (pcase-dolist (`(,prop . ,value)
                     `((command        . ,command)
                       (error-parser   . ,parser)
                       (error-patterns . ,patterns)
                       (standard-input . ,standard-input)))
        (setf (flycheck-checker-get symbol prop) value)))))

(eval-and-compile
  ;; Make this function available during byte-compilation, since we need it
  ;; at macro expansion of `flycheck-def-executable-var'.
  (defun flycheck-checker-executable-variable (checker)
    "Get the executable variable of CHECKER.

The executable variable is named `flycheck-CHECKER-executable'."
    (intern (format "flycheck-%s-executable" checker))))

(defun flycheck-checker-default-executable (checker)
  "Get the default executable of CHECKER."
  (car (flycheck-checker-get checker 'command)))

(defun flycheck-checker-executable (checker)
  "Get the command executable of CHECKER.

The executable is either the value of the variable
`flycheck-CHECKER-executable', or the default executable given in
the syntax checker definition, if the variable is nil."
  (let ((var (flycheck-checker-executable-variable checker)))
    (or (and (boundp var) (symbol-value var))
        (flycheck-checker-default-executable checker))))

(defun flycheck-find-checker-executable (checker)
  "Get the full path of the executable of CHECKER.

Return the full absolute path to the executable of CHECKER, or
nil if the executable does not exist."
  (funcall flycheck-executable-find (flycheck-checker-executable checker)))

(defun flycheck-call-checker-process
    (checker infile destination error &rest args)
  "Call CHECKER's executable with ARGS.

Return nil (or raise an error if ERROR is non-nil) when CHECKER's
executable cannot be found, and return a numeric exit status or a
signal description string otherwise.  CHECKER's input is taken
from INFILE, and its output is sent to DESTINATION, as in
`call-process'."
  (-if-let (executable (flycheck-find-checker-executable checker))
      (condition-case err
          (apply #'call-process executable infile destination nil args)
        (error (when error (signal (car err) (cdr err)))))
    (when error
      (user-error "Cannot find `%s' using `flycheck-executable-find'"
                  (flycheck-checker-executable checker)))))

(defun flycheck-call-checker-process-for-output
    (checker infile error &rest args)
  "Call CHECKER's executable with ARGS and return its output.

Call `flycheck-call-checker-process' with INFILE, ERROR, and
ARGS.  If it returns 0, return the process' output.  Otherwise,
return nil or throw an error.

This function is similar to `flycheck-call-checker-process'
called in a `with-output-to-string' block, but it takes care of
the error checking automatically."
  (let ((temp (generate-new-buffer " *temp*")))
    (unwind-protect
        ;; We need to call the checker process in the right buffer, so that it
        ;; uses the right exec-path, checker executable, etc.  See URL
        ;; `https://github.com/flycheck/flycheck/issues/1770'.
        (let ((exit-code (apply #'flycheck-call-checker-process
                                checker infile temp error args))
              (output (with-current-buffer temp (buffer-string))))
          (if (eql 0 exit-code) output
            (when error
              (error "Process %s failed with %S (%s)"
                     checker exit-code output))))
      (kill-buffer temp))))

(defun flycheck-checker-arguments (checker)
  "Get the command arguments of CHECKER."
  (cdr (flycheck-checker-get checker 'command)))

(defun flycheck-substitute-argument (arg checker)
  "Substitute ARG for CHECKER.

Return a list of real arguments for the executable of CHECKER,
substituted for the symbolic argument ARG.  Single arguments,
e.g. if ARG is a literal strings, are wrapped in a list.

ARG may be one of the following forms:

STRING
     Return ARG unchanged.

`source', `source-inplace'
     Create a temporary file to check and return its path.  With
     `source-inplace' create the temporary file in the same
     directory as the original file.  The value of
     `flycheck-temp-prefix' is used as prefix of the file name.

     With `source', try to retain the non-directory component of
     the buffer's file name in the temporary file.

     `source' is the preferred way to pass the input file to a
     syntax checker.  `source-inplace' should only be used if the
     syntax checker needs other files from the source directory,
     such as include files in C.

`(source SUFFIX)', `(source-inplace SUFFIX)'
     Like `source' and `source-inplace', but ensure generated
     file names end with the given suffix.  Use this when the
     checker requires that file names on its command line have a
     certain suffix (file extension).

`source-original'
     Return the path of the actual file to check, or an empty
     string if the buffer has no file name.

     Note that the contents of the file may not be up to date
     with the contents of the buffer to check.  Do not use this
     as primary input to a checker, unless absolutely necessary.

     When using this symbol as primary input to the syntax
     checker, add `flycheck-buffer-saved-p' to the `:predicate'.

`temporary-directory'
     Create a unique temporary directory and return its path.

`temporary-file-name'
     Return a unique temporary filename.  The file is *not*
     created.

     To ignore the output of syntax checkers, try symbol
     `null-device' first.

symbol `null-device'
     Return the value of variable `null-device', i.e the system
     null device.

     Use this option to ignore the output of a syntax checker.
     If the syntax checker cannot handle the null device, or
     won't write to an existing file, try `temporary-file-name'
     instead.

`(config-file OPTION VARIABLE [PREPEND-FN])'
     Search the configuration file bound to VARIABLE with
     `flycheck-locate-config-file' and return a list of arguments
     that pass this configuration file to the syntax checker, or
     nil if the configuration file was not found.

     PREPEND-FN is called with the OPTION and the located
     configuration file, and should return OPTION prepended
     before the file, either a string or as list.  If omitted,
     PREPEND-FN defaults to `list'.

`(option OPTION VARIABLE [PREPEND-FN [FILTER]])'
     Retrieve the value of VARIABLE and return a list of
     arguments that pass this value as value for OPTION to the
     syntax checker.

     PREPEND-FN is called with the OPTION and the value of
     VARIABLE, and should return OPTION prepended before the
     file, either a string or as list.  If omitted, PREPEND-FN
     defaults to `list'.

     FILTER is an optional function to be applied to the value of
     VARIABLE before prepending.  This function must return nil
     or a string.  In the former case, return nil.  In the latter
     case, return a list of arguments as described above.

`(option-list OPTION VARIABLE [PREPEND-FN [FILTER]])'
     Retrieve the value of VARIABLE, which must be a list,
     and prepend OPTION before each item in this list, using
     PREPEND-FN.

     PREPEND-FN is called with the OPTION and each item of the
     list as second argument, and should return OPTION prepended
     before the item, either as string or as list.  If omitted,
     PREPEND-FN defaults to `list'.

     FILTER is an optional function to be applied to each item in
     the list before prepending OPTION.  It shall return the
     option value for each item as string, or nil, if the item is
     to be ignored.

`(option-flag OPTION VARIABLE)'
     Retrieve the value of VARIABLE and return OPTION, if the
     value is non-nil.  Otherwise return nil.

`(eval FORM)'
     Return the result of evaluating FORM in the buffer to be
     checked.  FORM must either return a string or a list of
     strings, or nil to indicate that nothing should be
     substituted for CELL.  For all other return types, signal an
     error

     _No_ further substitutions are performed, neither in FORM
     before it is evaluated, nor in the result of evaluating
     FORM.

In all other cases, signal an error.

Note that substitution is *not* recursive.  No symbols or cells
are substituted within the body of cells!"
  (pcase arg
    ((pred stringp) (list arg))
    (`source
     (list (flycheck-save-buffer-to-temp #'flycheck-temp-file-system)))
    (`source-inplace
     (list (flycheck-save-buffer-to-temp #'flycheck-temp-file-inplace)))
    (`(source ,suffix)
     (list (flycheck-save-buffer-to-temp
            (lambda (filename) (flycheck-temp-file-system filename suffix)))))
    (`(source-inplace ,suffix)
     (list (flycheck-save-buffer-to-temp
            (lambda (filename) (flycheck-temp-file-inplace filename suffix)))))
    (`source-original (list (or (buffer-file-name) "")))
    (`temporary-directory (list (flycheck-temp-dir-system)))
    (`temporary-file-name
     (let ((directory (flycheck-temp-dir-system)))
       (list (make-temp-name (expand-file-name "flycheck" directory)))))
    (`null-device (list null-device))
    (`(config-file ,option-name ,file-name-var)
     (-when-let* ((value (symbol-value file-name-var))
                  (file-name (flycheck-locate-config-file value checker)))
       (flycheck-prepend-with-option option-name (list file-name))))
    (`(config-file ,option-name ,file-name-var ,prepend-fn)
     (-when-let* ((value (symbol-value file-name-var))
                  (file-name (flycheck-locate-config-file value checker)))
       (flycheck-prepend-with-option option-name (list file-name) prepend-fn)))
    (`(option ,option-name ,variable)
     (-when-let (value (symbol-value variable))
       (unless (stringp value)
         (error "Value %S of %S for option %s is not a string"
                value variable option-name))
       (flycheck-prepend-with-option option-name (list value))))
    (`(option ,option-name ,variable ,prepend-fn)
     (-when-let (value (symbol-value variable))
       (unless (stringp value)
         (error "Value %S of %S for option %s is not a string"
                value variable option-name))
       (flycheck-prepend-with-option option-name (list value) prepend-fn)))
    (`(option ,option-name ,variable ,prepend-fn ,filter)
     (-when-let (value (funcall filter (symbol-value variable)))
       (unless (stringp value)
         (error "Value %S of %S (filter: %S) for option %s is not a string"
                value variable filter option-name))
       (flycheck-prepend-with-option option-name (list value) prepend-fn)))
    (`(option-list ,option-name ,variable)
     (let ((value (symbol-value variable)))
       (unless (and (listp value) (seq-every-p #'stringp value))
         (error "Value %S of %S for option %S is not a list of strings"
                value variable option-name))
       (flycheck-prepend-with-option option-name value)))
    (`(option-list ,option-name ,variable ,prepend-fn)
     (let ((value (symbol-value variable)))
       (unless (and (listp value) (seq-every-p #'stringp value))
         (error "Value %S of %S for option %S is not a list of strings"
                value variable option-name))
       (flycheck-prepend-with-option option-name value prepend-fn)))
    (`(option-list ,option-name ,variable ,prepend-fn ,filter)
     (let ((value (delq nil (seq-map filter (symbol-value variable)))))
       (unless (and (listp value) (seq-every-p #'stringp value))
         (error "Value %S of %S for option %S is not a list of strings"
                value variable option-name))
       (flycheck-prepend-with-option option-name value prepend-fn)))
    (`(option-flag ,option-name ,variable)
     (when (symbol-value variable)
       (list option-name)))
    (`(eval ,form)
     (let ((result (eval form)))
       (cond
        ((and (listp result) (seq-every-p #'stringp result)) result)
        ((stringp result) (list result))
        (t (error "Invalid result from evaluation of %S: %S" form result)))))
    (_ (error "Unsupported argument %S" arg))))

(defun flycheck-checker-substituted-arguments (checker)
  "Get the substituted arguments of a CHECKER.

Substitute each argument of CHECKER using
`flycheck-substitute-argument'.  This replaces any special
symbols in the command."
  (apply #'append
         (seq-map (lambda (arg) (flycheck-substitute-argument arg checker))
                  (flycheck-checker-arguments checker))))

(defun flycheck--process-send-buffer-contents-chunked (process)
  "Send contents of current buffer to PROCESS in small batches.

Send the entire buffer to the standard input of PROCESS in chunks
of 4096 characters.  Chunking is done in Emacs Lisp, hence this
function is probably far less efficient than
`send-process-region'.  Use only when required."
  (let ((from (point-min)))
    (while (< from (point-max))
      (let ((to (min (+ from 4096) (point-max))))
        (process-send-region process from to)
        (setq from to)))))

(defvar flycheck-chunked-process-input
  ;; Chunk process output on Windows to work around
  ;; https://github.com/flycheck/flycheck/issues/794 and
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=22344.  The presence of
  ;; `w32-pipe-buffer-size' denotes an Emacs version (> Emacs 25.1) where pipe
  ;; writes on Windows are fixed.
  ;;
  ;; TODO: Remove option and chunking when dropping Emacs 24 support, see
  ;; https://github.com/flycheck/flycheck/issues/856
  (and (eq system-type 'windows-nt) (not (boundp 'w32-pipe-buffer-size)))
  "If non-nil send process input in small chunks.

If this variable is non-nil `flycheck-process-send-buffer' sends
buffer contents in small chunks.

Defaults to nil, except on Windows to work around Emacs bug
#22344.")

(defun flycheck-process-send-buffer (process)
  "Send all contents of current buffer to PROCESS.

Sends all contents of the current buffer to the standard input of
PROCESS, and terminates standard input with EOF.

If `flycheck-chunked-process-input' is non-nil, send buffer
contents in chunks via
`flycheck--process-send-buffer-contents-chunked', which see.
Otherwise use `process-send-region' to send all contents at once
and rely on Emacs' own buffering and chunking."
  (save-restriction
    (widen)
    (if flycheck-chunked-process-input
        (flycheck--process-send-buffer-contents-chunked process)
      (process-send-region process (point-min) (point-max))))
  (process-send-eof process))

(defun flycheck--wrap-command (prog args)
  "Wrap PROG and ARGS using `flycheck-command-wrapper-function'."
  ;; We don't call `flycheck-executable-find' on the output of the wrapper
  ;; function, since it might not expect it (an executable-find function
  ;; designed to find binaries in a sandbox could get confused if we asked it
  ;; about the sandboxing program itself).
  (funcall flycheck-command-wrapper-function (cons prog args)))

(defun flycheck-start-command-checker (checker callback)
  "Start a command CHECKER with CALLBACK."
  (let (process)
    (condition-case err
        (let* ((program (flycheck-find-checker-executable checker))
               (args (flycheck-checker-substituted-arguments checker))
               (command (flycheck--wrap-command program args))
               (sentinel-events nil)
               ;; Use pipes to receive output from the syntax checker.  They are
               ;; more efficient and more robust than PTYs, which Emacs uses by
               ;; default, and since we don't need any job control features, we
               ;; can easily use pipes.
               (process-connection-type nil))
          ;; We pass do not associate the process with any buffer, by
          ;; passing nil for the BUFFER argument of `start-process'.
          ;; Instead, we just remember the buffer being checked in a
          ;; process property (see below).  This neatly avoids all
          ;; side-effects implied by attached a process to a buffer, which
          ;; may cause conflicts with other packages.
          ;;
          ;; See https://github.com/flycheck/flycheck/issues/298 for an
          ;; example for such a conflict.
          (setq process (apply 'start-process (format "flycheck-%s" checker)
                               nil command))
          ;; Process sentinels can be called while sending input to the process.
          ;; We want to record errors raised by process-send before calling
          ;; `flycheck-handle-signal', so initially just accumulate events.
          (setf (process-sentinel process)
                (lambda (_ event) (push event sentinel-events)))
          (setf (process-filter process) #'flycheck-receive-checker-output)
          (set-process-query-on-exit-flag process nil)
          ;; Remember the syntax checker, the buffer and the callback
          (process-put process 'flycheck-checker checker)
          (process-put process 'flycheck-callback callback)
          (process-put process 'flycheck-buffer (current-buffer))
          ;; The default directory is bound in the `flycheck-syntax-check-start'
          ;; function.
          (process-put process 'flycheck-working-directory default-directory)
          ;; Track the temporaries created by argument substitution in the
          ;; process itself, to get rid of the global state ASAP.
          (process-put process 'flycheck-temporaries flycheck-temporaries)
          (setq flycheck-temporaries nil)
          ;; Send the buffer to the process on standard input, if enabled.
          (when (flycheck-checker-get checker 'standard-input)
            (condition-case err
                (flycheck-process-send-buffer process)
              ;; Some checkers exit before reading all input, causing errors
              ;; such as a `file-error' for a closed pipe, or a plain “no longer
              ;; connected to pipe; closed it” error for a disconnection.  We
              ;; report them if needed in `flycheck-finish-checker-process' (see
              ;; `https://github.com/flycheck/flycheck/issues/1278').
              (error (process-put process 'flycheck-error err))))
          ;; Set the actual sentinel and process any events that might have
          ;; happened while we were sending input.
          (setf (process-sentinel process) #'flycheck-handle-signal)
          (dolist (event (nreverse sentinel-events))
            (flycheck-handle-signal process event))
          ;; Return the process.
          process)
      (error
       ;; In case of error, clean up our resources, and report the error back to
       ;; Flycheck.
       (flycheck-safe-delete-temporaries)
       (when process
         ;; No need to explicitly delete the temporary files of the process,
         ;; because deleting runs the sentinel, which will delete them anyway.
         (delete-process process))
       (signal (car err) (cdr err))))))

(defun flycheck-interrupt-command-checker (_checker process)
  "Interrupt a PROCESS."
  ;; Deleting the process always triggers the sentinel, which does the cleanup
  (when process
    (delete-process process)))

(defun flycheck-command-checker-print-doc (checker)
  "Print additional documentation for a command CHECKER."
  (let ((executable (flycheck-checker-default-executable checker))
        (config-file-var (flycheck-checker-get checker 'config-file-var))
        (option-vars (seq-sort #'string<
                               (flycheck-checker-get checker 'option-vars))))
    (princ "\n")

    (let ((doc-start (with-current-buffer standard-output (point-max))))
      ;; Track the start of our documentation so that we can re-indent it
      ;; properly
      (princ "  This syntax checker executes \"")
      (princ executable)
      (princ "\"")
      (when config-file-var
        (princ ", using a configuration file from `")
        (princ (symbol-name config-file-var))
        (princ "'"))
      (princ ". The executable can be overridden with `")
      (princ (symbol-name (flycheck-checker-executable-variable checker)))
      (princ "'.")

      (with-current-buffer standard-output
        (save-excursion
          (fill-region-as-paragraph doc-start (point-max)))))
    (princ "\n")
    (when option-vars
      (princ
       "\n  This syntax checker can be configured with these options:\n\n")
      (dolist (var option-vars)
        (princ (format "     * `%s'\n" var))))))

(defun flycheck-verify-command-checker (checker)
  "Verify a command CHECKER in the current buffer.

Return a list of `flycheck-verification-result' objects for
CHECKER."
  (let ((executable (flycheck-find-checker-executable checker))
        (config-file-var (flycheck-checker-get checker 'config-file-var)))
    `(
      ,(flycheck-verification-result-new
        :label "executable"
        :message (if executable (format "Found at %s" executable) "Not found")
        :face (if executable 'success '(bold error)))
      ,@(when config-file-var
          (let* ((value (symbol-value config-file-var))
                 (path (and value (flycheck-locate-config-file value checker))))
            (list (flycheck-verification-result-new
                   :label "configuration file"
                   :message (if path (format "Found at %S" path) "Not found")
                   :face (if path 'success 'warning)))))
      ,@(when (not (flycheck-temp-files-writable-p checker))
          (list (flycheck-verification-result-new
                 :label "temp directory"
                 :message (format "%s is not writable"
                                  (flycheck-temp-directory checker))
                 :face 'error))))))


;;; Process management for command syntax checkers
(defun flycheck-receive-checker-output (process output)
  "Receive a syntax checking PROCESS OUTPUT."
  (push output (process-get process 'flycheck-pending-output)))

(defun flycheck-get-output (process)
  "Get the complete output of PROCESS."
  (with-demoted-errors "Error while retrieving process output: %S"
    (let ((pending-output (process-get process 'flycheck-pending-output)))
      (apply #'concat (nreverse pending-output)))))

(defun flycheck-handle-signal (process _event)
  "Handle a signal from the syntax checking PROCESS.

_EVENT is ignored."
  (when (memq (process-status process) '(signal exit))
    (let ((files (process-get process 'flycheck-temporaries))
          (buffer (process-get process 'flycheck-buffer))
          (callback (process-get process 'flycheck-callback))
          (cwd (process-get process 'flycheck-working-directory))
          (err (process-get process 'flycheck-error)))
      ;; Delete the temporary files
      (seq-do #'flycheck-safe-delete files)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (condition-case err
              (pcase (process-status process)
                (`signal
                 (funcall callback 'interrupted))
                (`exit
                 (flycheck-finish-checker-process
                  (process-get process 'flycheck-checker)
                  (or err (process-exit-status process))
                  files
                  (flycheck-get-output process) callback cwd)))
            ((debug error)
             (funcall callback 'errored (error-message-string err)))))))))

(defun flycheck-finish-checker-process
    (checker exit-status files output callback cwd)
  "Finish a checker process from CHECKER with EXIT-STATUS.

EXIT-STATUS can be a number or an arbitrary form (if it is not 0,
a `suspicious' status is reported to CALLBACK).

FILES is a list of files given as input to the checker.  OUTPUT
is the output of the syntax checker.  CALLBACK is the status
callback to use for reporting.

Parse the OUTPUT and report an appropriate error status.

Resolve all errors in OUTPUT using CWD as working directory."
  (let ((errors (flycheck-parse-output output checker (current-buffer))))
    (when (and (not (equal exit-status 0)) (null errors))
      ;; Warn about a suspicious result from the syntax checker.  We do right
      ;; after parsing the errors, before filtering, because a syntax checker
      ;; might report errors from other files (e.g. includes) even if there
      ;; are no errors in the file being checked.
      (funcall callback 'suspicious
               (format "Flycheck checker %S returned %S, but \
its output contained no errors: %s\nTry installing a more \
recent version of %S, and please open a bug report if the issue \
persists in the latest release.  Thanks!"  checker exit-status
output checker)))
    (funcall callback 'finished
             ;; Fix error file names, by substituting them backwards from the
             ;; temporaries.
             (seq-map (lambda (e) (flycheck-fix-error-filename e files cwd))
                      errors))))


;;; Executables of command checkers.
(defmacro flycheck-def-executable-var (checker default-executable)
  "Define the executable variable for CHECKER.

DEFAULT-EXECUTABLE is the default executable.  It is only used in
the docstring of the variable.

The variable is defined with `defcustom' in the
`flycheck-executables' group.  It's also defined to be risky as
file-local variable, to avoid arbitrary executables being used
for syntax checking."
  (let ((executable-var (flycheck-checker-executable-variable checker)))
    `(progn
       (defcustom ,executable-var nil
         ,(format "The executable of the %s syntax checker.

Either a string containing the name or the path of the
executable, or nil to use the default executable from the syntax
checker declaration.

The default executable is %S." checker default-executable)
         :type '(choice (const :tag "Default executable" nil)
                        (string :tag "Name or path"))
         :group 'flycheck-executables
         :risky t))))

(defun flycheck-set-checker-executable (checker &optional executable)
  "Set the executable of CHECKER in the current buffer.

CHECKER is a syntax checker symbol.  EXECUTABLE is a string with
the name of an executable or the path to an executable file, which
is to be used as executable for CHECKER.  If omitted or nil,
reset the executable of CHECKER.

Interactively, prompt for a syntax checker and an executable
file, and set the executable of the selected syntax checker.
With prefix arg, prompt for a syntax checker only, and reset the
executable of the select checker to the default.

Set the executable variable of CHECKER, that is,
`flycheck-CHECKER-executable' to EXECUTABLE.  Signal
`user-error', if EXECUTABLE does not denote a command or an
executable file.

This command is intended for interactive use only.  In Lisp, just
`let'-bind the corresponding variable, or set it directly.  Use
`flycheck-checker-executable-variable' to obtain the executable
variable symbol for a syntax checker."
  (declare (interactive-only "Set the executable variable directly instead"))
  (interactive
   (let* ((checker (flycheck-read-checker "Syntax checker: "))
          (default-executable (flycheck-checker-default-executable checker))
          (executable (if current-prefix-arg
                          nil
                        (read-file-name "Executable: " nil default-executable
                                        nil nil flycheck-executable-find))))
     (list checker executable)))
  (when (and executable (not (funcall flycheck-executable-find executable)))
    (user-error "%s is no executable" executable))
  (let ((variable (flycheck-checker-executable-variable checker)))
    (set (make-local-variable variable) executable)))


;;; Configuration files and options for command checkers
(defun flycheck-register-config-file-var (var checkers)
  "Register VAR as config file var for CHECKERS.

CHECKERS is a single syntax checker or a list thereof."
  (when (symbolp checkers)
    (setq checkers (list checkers)))
  (dolist (checker checkers)
    (setf (flycheck-checker-get checker 'config-file-var) var)))

;;;###autoload
(defmacro flycheck-def-config-file-var (symbol checker &optional file-name
                                               &rest custom-args)
  "Define SYMBOL as config file variable for CHECKER, with default FILE-NAME.

SYMBOL is declared as customizable variable using `defcustom', to
provide configuration files for the given syntax CHECKER.
CUSTOM-ARGS are forwarded to `defcustom'.

FILE-NAME is the initial value of the new variable.  If omitted,
the default value is nil.  It can be either a string or a list of
strings.

Use this together with the `config-file' form in the `:command'
argument to `flycheck-define-checker'."
  (declare (indent 3))
  `(progn
     (defcustom ,symbol ,file-name
       ,(format "Configuration file for `%s'.

If set to a string, locate the configuration file using the
functions from `flycheck-locate-config-file-functions'.  If the
file is found pass it to the syntax checker as configuration
file.

If no configuration file is found, or if this variable is set to
nil, invoke the syntax checker without a configuration file.

Use this variable as file-local variable if you need a specific
configuration file for a buffer." checker)
       :type '(choice (const :tag "No configuration file" nil)
                      (string :tag "File name or path")
                      (repeat :tag "File names or paths" string))
       :safe #'flycheck-string-or-string-list-p
       :group 'flycheck-config-files
       ,@custom-args)
     (flycheck-register-config-file-var ',symbol ',checker)))

(defun flycheck-locate-config-file (filenames checker)
  "Locate the configuration file for CHECKER, based on FILENAMES.

FILENAMES can be either a single file, or a list.  Each filename
is passed to all `flycheck-locate-config-file-functions', until
one returns non-nil.

Return the absolute path of the configuration file, or nil if no
configuration file was found."
  (when (stringp filenames)
    (setq filenames (list filenames)))
  (let ((config-file nil))
    (while (and filenames (null config-file))
      (setq config-file (run-hook-with-args-until-success
                         'flycheck-locate-config-file-functions
                         (pop filenames) checker)))
    (when (and config-file (file-exists-p config-file))
      config-file)))

(defun flycheck-locate-config-file-by-path (filepath _checker)
  "Locate a configuration file by a FILEPATH.

If FILEPATH is a contains a path separator, expand it against the
default directory and return it if it points to an existing file.
Otherwise return nil.

_CHECKER is ignored."
  ;; If the path is just a plain file name, skip it.
  (unless (string= (file-name-nondirectory filepath) filepath)
    (let ((file-name (expand-file-name filepath)))
      (and (file-exists-p file-name) file-name))))

(defun flycheck-locate-config-file-ancestor-directories (filename _checker)
  "Locate a configuration FILENAME in ancestor directories.

If the current buffer has a file name, search FILENAME in the
directory of the current buffer and all ancestors thereof (see
`locate-dominating-file').  If the file is found, return its
absolute path.  Otherwise return nil.

_CHECKER is ignored."
  (-when-let* ((basefile (buffer-file-name))
               (directory (locate-dominating-file basefile filename)))
    (expand-file-name filename directory)))

(defun flycheck-locate-config-file-home (filename _checker)
  "Locate a configuration FILENAME in the home directory.

Return the absolute path, if FILENAME exists in the user's home
directory, or nil otherwise."
  (let ((path (expand-file-name filename "~")))
    (when (file-exists-p path)
      path)))

(seq-do (apply-partially #'custom-add-frequent-value
                         'flycheck-locate-config-file-functions)
        '(flycheck-locate-config-file-by-path
          flycheck-locate-config-file-ancestor-directories
          flycheck-locate-config-file-home))

(defun flycheck-register-option-var (var checkers)
  "Register an option VAR with CHECKERS.

VAR is an option symbol, and CHECKERS a syntax checker symbol or
a list thereof.  Register VAR with all CHECKERS so that it
appears in the help output."
  (when (symbolp checkers)
    (setq checkers (list checkers)))
  (dolist (checker checkers)
    (cl-pushnew var (flycheck-checker-get checker 'option-vars))))

;;;###autoload
(defmacro flycheck-def-option-var (symbol init-value checkers docstring
                                          &rest custom-args)
  "Define SYMBOL as option variable with INIT-VALUE for CHECKER.

SYMBOL is declared as customizable variable using `defcustom', to
provide an option for the given syntax CHECKERS (a checker or a
list of checkers).  INIT-VALUE is the initial value of the
variable, and DOCSTRING is its docstring.  CUSTOM-ARGS are
forwarded to `defcustom'.

Use this together with the `option', `option-list' and
`option-flag' forms in the `:command' argument to
`flycheck-define-checker'."
  (declare (indent 3)
           (doc-string 4))
  `(progn
     (defcustom ,symbol ,init-value
       ,(concat docstring "

This variable is an option for the following syntax checkers:

"
                (mapconcat (lambda (c) (format "  - `%s'" c))
                           (if (symbolp checkers) (list checkers) checkers)
                           "\n"))
       :group 'flycheck-options
       ,@custom-args)
     (flycheck-register-option-var ',symbol ',checkers)))

(defun flycheck-option-int (value)
  "Convert an integral option VALUE to a string.

If VALUE is nil, return nil.  Otherwise return VALUE converted to
a string."
  (and value (number-to-string value)))

(defun flycheck-option-symbol (value)
  "Convert a symbol option VALUE to string.

If VALUE is nil return nil.  Otherwise return VALUE converted to
a string."
  (and value (symbol-name value)))

(defun flycheck-option-comma-separated-list (value &optional separator filter)
  "Convert VALUE into a list separated by SEPARATOR.

SEPARATOR is a string to separate items in VALUE, defaulting to
\",\".  FILTER is an optional function, which takes a single
argument and returns either a string or nil.

If VALUE is a list, apply FILTER to each item in VALUE, remove
all nil items, and return a single string of all remaining items
separated by SEPARATOR.

Otherwise, apply FILTER to VALUE and return the result.
SEPARATOR is ignored in this case."
  (let ((filter (or filter #'identity))
        (separator (or separator ",")))
    (if (listp value)
        (-when-let (value (delq nil (seq-map filter value)))
          (string-join value separator))
      (funcall filter value))))

(defmacro flycheck-def-args-var (symbol checkers &rest custom-args)
  "Define SYMBOL as argument variable for CHECKERS.

SYMBOL is declared as customizable, risky and buffer-local
variable using `defcustom' to provide an option for arbitrary
arguments for the given syntax CHECKERS (either a single checker
or a list of checkers).  CUSTOM-ARGS is forwarded to `defcustom'.

Use the `eval' form to splice this variable into the
`:command'."
  (declare (indent 2))
  `(flycheck-def-option-var ,symbol nil ,checkers
     "A list of additional command line arguments.

The value of this variable is a list of strings with additional
command line arguments."
     :risky t
     :type '(repeat (string :tag "Argument"))
     ,@custom-args))


;;; Command syntax checkers as compile commands
(defun flycheck-checker-pattern-to-error-regexp (pattern)
  "Convert PATTERN into an error regexp for compile.el.

Return a list representing PATTERN, suitable as element in
`compilation-error-regexp-alist'."
  (let* ((regexp (car pattern))
         (level (cdr pattern))
         (level-no (flycheck-error-level-compilation-level level)))
    `(,regexp 1 (2 . 6) (3 . 7) ,level-no)))

(defun flycheck-checker-compilation-error-regexp-alist (checker)
  "Convert error patterns of CHECKER for use with compile.el.

Return an alist of all error patterns of CHECKER, suitable for
use with `compilation-error-regexp-alist'."
  (seq-map #'flycheck-checker-pattern-to-error-regexp
           (flycheck-checker-get checker 'error-patterns)))

(defun flycheck--substitute-shell-command-argument (arg checker)
  "Substitute ARG for CHECKER.

Like `flycheck-substitute-argument', except for source,
source-inplace, and source-original."
  (if (memq arg '(source source-inplace source-original))
      (list buffer-file-name)
    (flycheck-substitute-argument arg checker)))

(defun flycheck--checker-substituted-shell-command-arguments (checker)
  "Get the substituted arguments of a CHECKER to run as a shell command.

Substitute each argument of CHECKER using
`flycheck-substitute-shell-command-argument'."
  (apply #'append
         (seq-map (lambda (arg)
                    (flycheck--substitute-shell-command-argument arg checker))
                  (flycheck-checker-arguments checker))))

(defun flycheck-checker-shell-command (checker)
  "Get a shell command for CHECKER.

Perform substitution in the arguments of CHECKER, but with
`flycheck--substitute-shell-command-argument'.

Return the command of CHECKER as single string, suitable for
shell execution."
  ;; Note: Do NOT use `combine-and-quote-strings' here.  Despite it's name it
  ;; does not properly quote shell arguments, and actually breaks for special
  ;; characters.  See https://github.com/flycheck/flycheck/pull/522
  (let* ((args (flycheck--checker-substituted-shell-command-arguments checker))
         (program
          (or (flycheck-find-checker-executable checker)
              (user-error "Cannot find `%s' using `flycheck-executable-find'"
                          (flycheck-checker-executable checker))))
         (wrapped (flycheck--wrap-command program args))
         (abs-prog
          ;; The executable path returned by `flycheck-command-wrapper-function'
          ;; may not be absolute, so expand it here.  See URL
          ;; `https://github.com/flycheck/flycheck/issues/1461'.
          (or (executable-find (car wrapped))
              (user-error "Cannot find `%s' using `executable-find'"
                          (car wrapped))))
         (command (mapconcat #'shell-quote-argument
                             (cons abs-prog (cdr wrapped)) " ")))
    (if (flycheck-checker-get checker 'standard-input)
        ;; If the syntax checker expects the source from standard input add an
        ;; appropriate shell redirection
        (concat command " < " (shell-quote-argument (buffer-file-name)))
      command)))

(defun flycheck-compile-name (_name)
  "Get a name for a Flycheck compilation buffer.

_NAME is ignored."
  (format "*Flycheck %s*" (buffer-file-name)))

(defun flycheck-compile (checker)
  "Run CHECKER via `compile'.

CHECKER must be a valid syntax checker.  Interactively, prompt
for a syntax checker to run.

Instead of highlighting errors in the buffer, this command pops
up a separate buffer with the entire output of the syntax checker
tool, just like `compile' (\\[compile])."
  (interactive
   (let ((default (flycheck-get-checker-for-buffer)))
     (list (flycheck-read-checker "Run syntax checker as compile command: "
                                  (when (flycheck-checker-get default 'command)
                                    default)
                                  'command))))
  (unless (flycheck-valid-checker-p checker)
    (user-error "%S is not a valid syntax checker" checker))
  (unless (buffer-file-name)
    (user-error "Cannot compile a buffer without a backing file"))
  (unless (flycheck-may-use-checker checker)
    (user-error "Cannot use syntax checker %S in this buffer" checker))
  (unless (flycheck-checker-executable checker)
    (user-error "Cannot run checker %S as shell command" checker))
  (save-some-buffers)
  (let* ((default-directory (flycheck-compute-working-directory checker))
         (command (flycheck-checker-shell-command checker))
         (buffer (compilation-start command nil #'flycheck-compile-name)))
    (with-current-buffer buffer
      (setq-local compilation-error-regexp-alist
                  (flycheck-checker-compilation-error-regexp-alist checker)))))


;;; General error parsing for command checkers
(defun flycheck-parse-output (output checker buffer)
  "Parse OUTPUT from CHECKER in BUFFER.

OUTPUT is a string with the output from the checker symbol
CHECKER.  BUFFER is the buffer which was checked.

Return the errors parsed with the error patterns of CHECKER."
  (funcall (flycheck-checker-get checker 'error-parser) output checker buffer))

(defun flycheck-fix-error-filename (err buffer-files cwd)
  "Fix the file name of ERR from BUFFER-FILES.

Resolves error file names relative to CWD directory.

Make the file name of ERR absolute.  If the absolute file name of
ERR is in BUFFER-FILES, replace it with the value of variable
`buffer-file-name'."
  (flycheck-error-with-buffer err
    (-when-let (filename (flycheck-error-filename err))
      (when (seq-some (apply-partially #'flycheck-same-files-p
                                       (expand-file-name filename cwd))
                      buffer-files)
        (setf (flycheck-error-filename err) buffer-file-name)
        (when (and buffer-file-name (flycheck-error-message err))
          (setf (flycheck-error-message err)
                (replace-regexp-in-string
                 (regexp-quote filename) buffer-file-name
                 (flycheck-error-message err) 'fixed-case 'literal))))))
  err)


;;; Error parsers for command syntax checkers
(defun flycheck-parse-xml-region (beg end)
  "Parse the xml region between BEG and END.

Wrapper around `xml-parse-region' which transforms the return
value of this function into one compatible to
`libxml-parse-xml-region' by simply returning the first element
from the node list."
  (ignore-errors (car (xml-parse-region beg end))))

(defun flycheck-parse-xml-region-with-fallback (beg end)
  "Parse the xml region between BEG and END.

Try parsing with libxml first; if that fails, revert to
`flycheck-parse-xml-region'.  Failures can be caused by incorrect
XML (see URL `https://github.com/flycheck/flycheck/issues/1298'),
or on Windows by a missing libxml DLL with a libxml-enabled Emacs
\(see URL `https://github.com/flycheck/flycheck/issues/1330')."
  ;; FIXME use `libxml-available-p' when it gets implemented.
  (or (and (fboundp 'libxml-parse-xml-region)
           (libxml-parse-xml-region beg end))
      (flycheck-parse-xml-region beg end)))

(defvar flycheck-xml-parser 'flycheck-parse-xml-region-with-fallback
  "Function used to parse an xml string from a region.

The default uses libxml if available, and falls back to
`flycheck-parse-xml-region' otherwise.")

(defun flycheck-parse-xml-string (xml)
  "Parse an XML string.

Return the document tree parsed from XML in the form `(ROOT ATTRS
BODY...)'.  ROOT is a symbol identifying the name of the root
element.  ATTRS is an alist of the attributes of the root node.
BODY is zero or more body elements, either as strings (in case of
text nodes) or as XML nodes, in the same for as the root node."
  (with-temp-buffer
    (insert xml)
    (funcall flycheck-xml-parser (point-min) (point-max))))

(defun flycheck-parse-checkstyle (output checker buffer)
  "Parse Checkstyle errors from OUTPUT.

Parse Checkstyle-like XML output.  Use this error parser for
checkers that have an option to output errors in this format.

CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `http://checkstyle.sourceforge.net/' for information
about Checkstyle."
  (pcase (flycheck-parse-xml-string output)
    (`(checkstyle ,_ . ,file-nodes)
     (let (errors)
       (dolist (node file-nodes)
         (pcase node
           (`(file ,file-attrs . ,error-nodes)
            (dolist (node error-nodes)
              (pcase node
                (`(error ,error-attrs . ,_)
                 (let-alist error-attrs
                   (push (flycheck-error-new-at
                          (flycheck-string-to-number-safe .line)
                          (flycheck-string-to-number-safe .column)
                          (pcase .severity
                            (`"error"   'error)
                            (`"warning" 'warning)
                            (`"info"    'info)
                            ;; Default to error for unknown .severity
                            (_          'error))
                          .message
                          :checker checker :id .source
                          :buffer buffer
                          :filename (cdr (assq 'name file-attrs)))
                         errors))))))))
       (nreverse errors)))))

(defun flycheck-parse-cppcheck (output checker buffer)
  "Parse Cppcheck errors from OUTPUT.

Parse Cppcheck XML v2 output.

CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `http://cppcheck.sourceforge.net/' for more information
about Cppcheck."
  (pcase (flycheck-parse-xml-string output)
    (`(results ,_ . ,body)
     (let (errors)
       (dolist (node body)
         (pcase node
           (`(errors ,_ . ,error-nodes)
            (dolist (node error-nodes)
              (pcase node
                (`(error ,error-attrs . ,loc-nodes)
                 (let ((id (cdr (assq 'id error-attrs)))
                       (message (cdr (assq 'verbose error-attrs)))
                       (level (pcase (cdr (assq 'severity error-attrs))
                                (`"error" 'error)
                                (`"style" 'info)
                                (`"information" 'info)
                                (_ 'warning))))
                   (dolist (node loc-nodes)
                     (pcase node
                       (`(location ,loc-attrs . ,_)
                        (let-alist loc-attrs
                          (push (flycheck-error-new-at
                                 (flycheck-string-to-number-safe .line)
                                 nil
                                 level
                                 ;; cppcheck return newline characters as "\012"
                                 (replace-regexp-in-string "\\\\012" "\n"
                                                           message)
                                 :id id
                                 :checker checker
                                 :buffer buffer
                                 :filename .file)
                                errors))))))))))))
       (nreverse errors)))))

(defun flycheck-parse-phpmd (output checker buffer)
  "Parse phpmd errors from OUTPUT.

CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `http://phpmd.org/' for more information about phpmd."
  (pcase (flycheck-parse-xml-string output)
    (`(pmd ,_ . ,body)
     (let (errors)
       (dolist (node body)
         (pcase node
           (`(file ,file-attrs . ,violation-nodes)
            (let ((filename (cdr (assq 'name file-attrs))))
              (dolist (node violation-nodes)
                (pcase node
                  (`(violation ,vio-attrs ,(and message (pred stringp)))
                   (let-alist vio-attrs
                     (push
                      (flycheck-error-new-at
                       (flycheck-string-to-number-safe .beginline)
                       nil
                       'warning (string-trim message)
                       ;; Ignore .endline (phpmd marks giant spans as errors)
                       ;; :end-line (flycheck-string-to-number-safe .endline)
                       :id .rule
                       :checker checker
                       :buffer buffer
                       :filename filename)
                      errors)))))))))
       (nreverse errors)))))

(defun flycheck-parse-reek (output checker buffer)
  "Parse Reek warnings from JSON OUTPUT.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://github.com/troessner/reek' for more information
about Reek."
  (let ((errors nil))
    (dolist (message (car (flycheck-parse-json output)))
      (let-alist message
        (dolist (line (delete-dups .lines))
          (push
           (flycheck-error-new-at
            line
            nil
            'warning (concat .context " " .message)
            :id .smell_type
            :checker checker
            :buffer buffer
            :filename .source)
           errors))))
    (nreverse errors)))

(defun flycheck-parse-go-staticcheck (output checker buffer)
  "Parse staticheck warnings from JSON OUTPUT.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://staticcheck.io/docs/formatters' for more
information about staticheck."
  (let ((errors nil))
    (dolist (msg (flycheck-parse-json output))
      (let-alist msg
        (push
         (flycheck-error-new-at
          .location.line
          .location.column
          (pcase .severity
            (`"error"   'error)
            (`"warning" 'warning)
            (`"ignored" 'info)
            ;; Default to warning for unknown .severity
            (_          'warning))
          .message
          :id .code
          :checker checker
          :buffer buffer
          :filename .location.file)
         errors)))
    (nreverse errors)))

(defun flycheck-parse-tslint (output checker buffer)
  "Parse TSLint errors from JSON OUTPUT.

CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://palantir.github.io/tslint/' for more information
about TSLint."
  (seq-map (lambda (message)
             (let-alist message
               (flycheck-error-new-at
                (+ 1 .startPosition.line)
                (+ 1 .startPosition.character)
                (pcase .ruleSeverity
                  ("ERROR"   'error)
                  ("WARNING" 'warning)
                  (_         'warning))
                .failure
                :id .ruleName
                :checker checker
                :buffer buffer
                :filename .name
                :end-line (+ 1 .endPosition.line)
                :end-column (+ 1 .endPosition.character))))
           (car (flycheck-parse-json output))))

(defun flycheck-parse-rust-collect-spans (span)
  "Return a list of spans contained in a SPAN object."
  (let ((spans))
    (let-alist span
      ;; With macro expansion errors, some spans will point to phony file names
      ;; to indicate an error inside the std rust lib.  We skip these spans as
      ;; they won't appear in flycheck anyway.
      (unless (string= .file_name "<std macros>")
        (push span spans))

      ;; Macro expansion errors will have a span in the 'expansion' field, so we
      ;; recursively collect it.
      (if .expansion.span
          (append (flycheck-parse-rust-collect-spans .expansion.span)
                  spans)
        spans))))

(defun flycheck-parse-rustc-diagnostic (diagnostic checker buffer)
  "Turn a rustc DIAGNOSTIC into a `flycheck-error'.

CHECKER and BUFFER denote the CHECKER that returned DIAGNOSTIC
and the BUFFER that was checked respectively.

DIAGNOSTIC should be a parsed JSON object describing a rustc
diagnostic, following the format described there:

https://github.com/rust-lang/rust/blob/master/src/librustc_errors/json.rs#L154"
  (let ((error-message)
        (error-level)
        (error-code)
        (primary-filename)
        (primary-line)
        (primary-column)
        (primary-end-line)
        (primary-end-column)
        (group (make-symbol "group"))
        (spans)
        (children)
        (errors))
    ;; The diagnostic format is described in the link above.  The gist of it is
    ;; that a diagnostic can have several causes in the source text; these
    ;; causes are represented by spans.  The diagnostic has a message and a
    ;; level (error, warning), while the spans have a filename, line, column,
    ;; and an optional label.  The primary span points to the root cause of the
    ;; error in the source text, while non-primary spans point to related
    ;; causes.  Spans may have an 'expansion' field for macro expansion errors;
    ;; these expansion fields will contain another span (and so on).  In
    ;; addition, a diagnostic can also have children diagnostics that are used
    ;; to provide additional information through their message field, but do not
    ;; seem to contain any spans (yet).
    ;;
    ;; We first gather spans in order to turn every span into a flycheck error
    ;; object, that we collect into the `errors' list.

    ;; Nested `let-alist' cause compilation warnings, hence we `setq' all
    ;; these values here first to avoid nesting.
    (let-alist diagnostic
      (setq error-message .message
            error-level (pcase .level
                          (`"error" 'error)
                          (`"warning" 'warning)
                          (`"note" 'info)
                          (_ 'error))
            ;; The 'code' field of the diagnostic contains the actual error
            ;; code and an optional explanation that we ignore
            error-code .code.code
            ;; Collect all spans recursively
            spans (seq-mapcat #'flycheck-parse-rust-collect-spans .spans)
            children .children))

    ;; Turn each span into a flycheck error
    (dolist (span spans)
      (let-alist span
        ;; Children may not have filename/line/column information, so we use
        ;; those from the primary span
        (when .is_primary
          (setq primary-filename .file_name
                primary-line .line_start
                primary-column .column_start
                primary-end-line .line_end
                primary-end-column .column_end))
        (push
         (flycheck-error-new-at
          .line_start
          .column_start
          ;; Non-primary spans are used for notes
          (if .is_primary error-level 'info)
          (if .is_primary
              ;; Primary spans may have labels with additional information
              (concat error-message (when .label
                                      (format " (%s)" .label)))
            ;; If the label is empty, fallback on the error message,
            ;; otherwise we won't be able to display anything
            (or .label error-message))
          :id error-code
          :checker checker
          :buffer buffer
          :filename .file_name
          :group group
          :end-line .line_end
          :end-column .column_end)
         errors)))

    ;; Then we turn children messages into flycheck errors pointing to the
    ;; location of the primary span.
    (dolist (child children)
      (let ((message (let-alist child .message)))
        (let-alist (car (let-alist child .spans))
          (push
           (flycheck-error-new-at
            ;; Use the line/column from the first span if there is one, or
            ;; fallback to the line/column information from the primary span of
            ;; the diagnostic.
            (or .line_start primary-line)
            (or .column_start primary-column)
            'info
            ;; Messages from `cargo clippy' may suggest replacement code.  In
            ;; these cases, the `message' field itself is an unhelpful `try' or
            ;; `change this to'.  We add the `suggested_replacement' field in
            ;; these cases.
            (if .suggested_replacement
                (format "%s: `%s`" message .suggested_replacement)
              message)
            :id error-code
            :checker checker
            :buffer buffer
            :filename primary-filename
            :group group
            :end-line (or .line_end primary-end-line)
            :end-column (or .column_end primary-end-column))
           errors))))

    ;; If there are no spans, the error is not associated with a specific
    ;; file but with the project as a whole.  We still need to report it to
    ;; the user by emitting a corresponding flycheck-error object.
    ;; Check whether the code is non-nil because Rust≥1.44 includes the
    ;; warning count upon completion.
    (when (and error-code (not spans))
      (push (flycheck-error-new-at
             ;; We have no specific position to attach the error to, so
             ;; let's use the top of the file.
             1 1
             error-level
             error-message
             :id error-code
             :checker checker
             :buffer buffer
             :group group)
            errors))
    (nreverse errors)))

(defconst flycheck--json-parser
  (if (and (functionp 'json-parse-buffer)
           ;; json-parse-buffer only supports keyword arguments in Emacs 27+
           (>= emacs-major-version 27))
      (lambda ()
        (json-parse-buffer
         :object-type 'alist :array-type 'list
         :null-object nil :false-object nil))
    #'json-read)
  "Function to use to parse JSON strings.")

(defun flycheck-parse-json (output)
  "Return parsed JSON data from OUTPUT.

OUTPUT is a string that contains JSON data.  Each line of OUTPUT
may be either plain text, a JSON array (starting with `['), or a
JSON object (starting with `{').

This function ignores the plain text lines, parses the JSON
lines, and returns the parsed JSON lines in a list."
  (let ((objects nil)
        (json-array-type 'list)
        (json-false nil))
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (while (not (eobp))
        (when (memq (char-after) '(?\{ ?\[))
          (push (funcall flycheck--json-parser) objects))
        (forward-line)))
    (nreverse objects)))

(defun flycheck-parse-rustc (output checker buffer)
  "Parse rustc errors from OUTPUT and return a list of `flycheck-error'.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

The expected format for OUTPUT is a mix of plain text lines and
JSON lines.  This function ignores the plain text lines and
parses only JSON lines.  Each JSON line is expected to be a JSON
object that corresponds to a diagnostic from the compiler.  The
expected diagnostic format is described there:

https://github.com/rust-lang/rust/blob/master/src/libsyntax/json.rs#L67-L139"
  (seq-mapcat (lambda (msg)
                (flycheck-parse-rustc-diagnostic msg checker buffer))
              (flycheck-parse-json output)))

(defun flycheck-parse-cargo-rustc (output checker buffer)
  "Parse Cargo errors from OUTPUT and return a list of `flycheck-error'.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

The expected format for OUTPUT is a mix of plain text lines and
JSON lines.  This function ignores the plain text lines and
parses only JSON lines.  Each JSON line is expected to be a JSON
object that represents a message from Cargo.  The format of
messages emitted by Cargo is described in cargo's
machine_message.rs at URL `https://git.io/vh24R'."
  (let ((errors))
    (dolist (msg (flycheck-parse-json output))
      (let-alist msg
        ;; Errors and warnings from rustc are wrapped by cargo, so we filter and
        ;; unwrap them, and delegate the actual construction of `flycheck-error'
        ;; objects to `flycheck-parse-rustc-diagnostic'.
        ;; We put the error record with nil code since flycheck regards
        ;; the case of nonzero return code without any error report
        ;; as abnormal result.
        (when (string= .reason "compiler-message")
          (push (flycheck-parse-rustc-diagnostic .message checker buffer)
                errors))))
    (apply #'nconc errors)))

;; Some checkers output ANSI terminal colors, which don't match up
;; with :error-patterns, so we strip those color codes from the output
;; here before passing it along to the default behavior. This is
;; originally only used in the rebar3 checker, but the systemd checker
;; now also makes use of it.
;;
;; The relevant discussion can be found at
;; https://github.com/flycheck/flycheck/pull/1144
(defun flycheck-parse-with-patterns-without-color (output checker buffer)
  "Strip color codes from OUTPUT before passing it to the default behavior.

CHECKER and BUFFER are passed along as well."
  (flycheck-parse-with-patterns
   (and (fboundp 'ansi-color-filter-apply) (ansi-color-filter-apply output))
   checker buffer))


;;; Error parsing with regular expressions
(defun flycheck-get-regexp (patterns)
  "Create a single regular expression from PATTERNS."
  (rx-to-string `(or ,@(seq-map (lambda (p) (list 'regexp (car p))) patterns))
                'no-group))

(defun flycheck-tokenize-output-with-patterns (output patterns)
  "Tokenize OUTPUT with PATTERNS.

Split the output into error tokens, using all regular expressions
from the error PATTERNS.  An error token is simply a string
containing a single error from OUTPUT.  Such a token can then be
parsed into a structured error by applying the PATTERNS again,
see `flycheck-parse-error-with-patterns'.

Return a list of error tokens."
  (let ((regexp (flycheck-get-regexp patterns))
        (last-match 0)
        errors)
    (while (string-match regexp output last-match)
      (push (match-string 0 output) errors)
      (setq last-match (match-end 0)))
    (reverse errors)))

(defun flycheck-try-parse-error-with-pattern (err pattern checker)
  "Try to parse a single ERR with a PATTERN for CHECKER.

Return the parsed error if PATTERN matched ERR, or nil
otherwise.

`end-line' defaults to the value of `line' when `end-column' is
set, since checkers often omit redundant end lines (as in
<file>:<line>:<column>-<end-column>)."
  (let ((regexp (car pattern))
        (level (cdr pattern)))
    (when (string-match regexp err)
      (let ((filename (match-string 1 err))
            (line (flycheck-string-to-number-safe (match-string 2 err)))
            (column (flycheck-string-to-number-safe (match-string 3 err)))
            (message (match-string 4 err))
            (id (match-string 5 err))
            (end-line (flycheck-string-to-number-safe (match-string 6 err)))
            (end-column (flycheck-string-to-number-safe (match-string 7 err))))
        (flycheck-error-new-at
         line
         column
         level
         (unless (string-empty-p message) message)
         :id (unless (string-empty-p id) id)
         :checker checker
         :filename (if (or (null filename) (string-empty-p filename))
                       (buffer-file-name)
                     filename)
         :end-line (or end-line (and end-column line))
         :end-column end-column)))))

(defun flycheck-parse-error-with-patterns (err patterns checker)
  "Parse a single ERR with error PATTERNS for CHECKER.

Apply each pattern in PATTERNS to ERR, in the given order, and
return the first parsed error."
  ;; Try to parse patterns in the order of declaration to make sure that the
  ;; first match wins.
  (let (parsed-error)
    (while (and patterns
                (not (setq parsed-error
                           (flycheck-try-parse-error-with-pattern
                            err (car patterns) checker))))
      (setq patterns (cdr patterns)))
    parsed-error))

(defun flycheck-parse-with-patterns (output checker buffer)
  "Parse OUTPUT from CHECKER with error patterns.

Uses the error patterns of CHECKER to tokenize the output and
tries to parse each error token with all patterns, in the order
of declaration.  Hence an error is never matched twice by two
different patterns.  The pattern declared first always wins.

_BUFFER is ignored.

Return a list of parsed errors and warnings (as `flycheck-error'
objects)."
  (with-current-buffer buffer
    (let ((patterns (flycheck-checker-get checker 'error-patterns)))
      (seq-map (lambda (err)
                 (flycheck-parse-error-with-patterns err patterns checker))
               (flycheck-tokenize-output-with-patterns output patterns)))))


;;; Convenience definition of command-syntax checkers

;; This macro is autoloaded to prevent `with-eval-after-load' from expanding its
;; arguments.  See https://github.com/flycheck/flycheck/issues/1398.
;;;###autoload
(defmacro flycheck-define-checker (symbol docstring &rest properties)
  "Define SYMBOL as command syntax checker with DOCSTRING and PROPERTIES.

Like `flycheck-define-command-checker', but PROPERTIES must not
be quoted.  Also, implicitly define the executable variable for
SYMBOL with `flycheck-def-executable-var'."
  (declare (indent 1)
           (doc-string 2))
  (let ((command (plist-get properties :command))
        (parser (plist-get properties :error-parser))
        (filter (plist-get properties :error-filter))
        (explainer (plist-get properties :error-explainer))
        (predicate (plist-get properties :predicate))
        (enabled-fn (plist-get properties :enabled))
        (verify-fn (plist-get properties :verify)))

    `(progn
       (flycheck-def-executable-var ,symbol ,(car command))

       (flycheck-define-command-checker ',symbol
         ,docstring
         :command ',command
         ,@(when parser
             `(:error-parser #',parser))
         :error-patterns ',(plist-get properties :error-patterns)
         ,@(when filter
             `(:error-filter #',filter))
         ,@(when explainer
             `(:error-explainer #',explainer))
         :modes ',(plist-get properties :modes)
         ,@(when predicate
             `(:predicate #',predicate))
         :next-checkers ',(plist-get properties :next-checkers)
         ,@(when enabled-fn
             `(:enabled #',enabled-fn))
         ,@(when verify-fn
             `(:verify #',verify-fn))
         :standard-input ',(plist-get properties :standard-input)
         :working-directory ',(plist-get properties :working-directory)))))


;;; Built-in checkers
(flycheck-def-args-var flycheck-gnat-args ada-gnat
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gnat-include-path nil ada-gnat
  "A list of include directories for GNAT.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of gcc.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gnat-language-standard "2012" ada-gnat
  "The language standard to use in GNAT.

The value of this variable is either a string denoting a language
standard, or nil, to use the default standard. When non-nil, pass
the language standard via the `-std' option."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Language standard"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gnat-warnings
    '("wa") ada-gnat
  "A list of additional Ada warnings to enable in GNAT.

The value of this variable is a list of strings, where each
string is the name of a warning category to enable. By default,
most optional warnings are recommended, as in `-gnata'.

Refer to Info Node `(gnat_ugn_unw)Warning Message Control' for
more information about GNAT warnings."
  :type '(repeat :tag "Warnings" (string :tag "Warning name"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.20"))

(flycheck-define-checker ada-gnat
  "An Ada syntax checker using GNAT.

Uses the GNAT compiler from GCC.  See URL
`https://www.adacore.com/community/'."
  :command ("gnatmake"
            "-c"                        ; Just compile, don't bind
            "-f"                        ; Force re-compilation
            "-u"                        ; Compile the main file only
            "-gnatf"                    ; Full error information
            "-gnatef"                   ; Full source file name
            "-D" temporary-directory
            (option-list "-gnat" flycheck-gnat-warnings concat)
            (option-list "-I" flycheck-gnat-include-path concat)
            (option "-gnat" flycheck-gnat-language-standard concat)
            (eval flycheck-gnat-args)
            source)
  :error-patterns
  ((error line-start
          (message "In file included from") " " (file-name) ":" line ":"
          column ":"
          line-end)
   (info line-start (file-name) ":" line ":" column
         ": note: " (message) line-end)
   (warning line-start (file-name) ":" line ":" column
            ": warning: " (message) line-end)
   ;; no specific error prefix in Ada
   (error line-start (file-name) ":" line ":" column
          ": " (message) line-end))
  :modes ada-mode)

(flycheck-define-checker asciidoc
  "A AsciiDoc syntax checker using the AsciiDoc compiler.

See URL `http://www.methods.co.nz/asciidoc'."
  :command ("asciidoc" "-o" null-device "-")
  :standard-input t
  :error-patterns
  ((error line-start
          "asciidoc: ERROR: <stdin>: Line " line ": " (message)
          line-end)
   (warning line-start
            "asciidoc: WARNING: <stdin>: Line " line ": " (message)
            line-end)
   (info line-start
         "asciidoc: DEPRECATED: <stdin>: Line " line ": " (message)
         line-end))
  :modes adoc-mode)

(flycheck-define-checker asciidoctor
  "An AsciiDoc syntax checker using the Asciidoctor compiler.

See URL `http://asciidoctor.org'."
  :command ("asciidoctor" "-o" null-device "-")
  :standard-input t
  :error-patterns
  ((error line-start
          "asciidoctor: ERROR: <stdin>: Line " line ": " (message)
          line-end)
   (warning line-start
            "asciidoctor: WARNING: <stdin>: Line " line ": " (message)
            line-end))
  :modes adoc-mode)

(defun flycheck-awk-gawk-fix-message (err)
  "Remove the repeated file-name/line from the error message of ERR."
  (setf (flycheck-error-message err)
        (replace-regexp-in-string
         (rx line-start
             (group (zero-or-more (any " " "\t")))
             (group (zero-or-more nonl) "\n")
             (backref 1))
         "\\2"
         (replace-regexp-in-string
          (rx "\ngawk: " (zero-or-more (not (any " "))) ":")
          "\n"
          (flycheck-error-message err))))
  err)

(defun flycheck-awk-gawk-error-filter (errors)
  "Remove repeated file-name/line from ERRORS."
  (seq-do #'flycheck-awk-gawk-fix-message errors)
  errors)

(flycheck-define-checker awk-gawk
  "GNU awk's built-in --lint checker."
  :command ("gawk"
            ;; Avoid code execution.  See https://github.com/w0rp/ale/pull/1411
            "--source" "'BEGIN{exit} END{exit 1}'"
            "-f" source
            "--lint"
            "/dev/null")
  :standard-input nil
  :error-patterns
  ((warning line-start
            "gawk: "
            (file-name) ":" line ":" (optional column ":")
            (message (one-or-more not-newline)
                     (optional "\n"
                               (one-or-more not-newline)
                               " ^ "
                               (one-or-more not-newline)))
            line-end))
  :error-filter flycheck-awk-gawk-error-filter
  :modes awk-mode)

(flycheck-define-checker bazel-build-buildifier
  "A checker for Bazel BUILD and BUILD.bazel files using buildifier.

See URL `https://github.com/bazelbuild/buildtools/blob/master/buildifier'."
  :command ("buildifier" "-lint=warn" "--type=build")
  :standard-input t
  :error-patterns
  ((error line-start
          "<stdin>:" line ":" column ": " (message)
          line-end)
   (warning line-start
            "<stdin>:" line ": " (id (one-or-more (in word "-"))) ": " (message)
            line-end))
  :modes bazel-build-mode)

(flycheck-define-checker bazel-module-buildifier
  "A checker for Bazel MODULE.bazel files using buildifier.

See URL `https://github.com/bazelbuild/buildtools/blob/master/buildifier'."
  :command ("buildifier" "-lint=warn" "--type=default")
  :standard-input t
  :error-patterns
  ((error line-start
          "<stdin>:" line ":" column ": " (message)
          line-end)
   (warning line-start
            "<stdin>:" line ": " (id (one-or-more (in word "-"))) ": " (message)
            line-end))
  :modes bazel-module-mode)

(flycheck-define-checker bazel-starlark-buildifier
  "A checker for Starlark bzl files using buildifier.

See URL `https://github.com/bazelbuild/buildtools/blob/master/buildifier'."
  :command ("buildifier" "-lint=warn" "--type=bzl")
  :standard-input t
  :error-patterns
  ((error line-start
          "<stdin>:" line ":" column ": " (message)
          line-end)
   (warning line-start
            "<stdin>:" line ": " (id (one-or-more (in word "-"))) ": " (message)
            line-end))
  :modes bazel-starlark-mode)

(flycheck-define-checker bazel-workspace-buildifier
  "A checker for Bazel WORKSPACE and WORKSPACE.bazel files using buildifier.

See URL `https://github.com/bazelbuild/buildtools/blob/master/buildifier'."
  :command ("buildifier" "-lint=warn" "--type=workspace")
  :standard-input t
  :error-patterns
  ((error line-start
          "<stdin>:" line ":" column ": " (message)
          line-end)
   (warning line-start
            "<stdin>:" line ": " (id (one-or-more (in word "-"))) ": " (message)
            line-end))
  :modes bazel-workspace-mode)

(flycheck-def-args-var flycheck-clang-args c/c++-clang
  :package-version '(flycheck . "0.22"))

(flycheck-def-option-var flycheck-clang-blocks nil c/c++-clang
  "Enable blocks in Clang.

When non-nil, enable blocks in Clang with `-fblocks'.  See URL
`http://clang.llvm.org/docs/BlockLanguageSpec.html' for more
information about blocks."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-clang-definitions nil c/c++-clang
  "Additional preprocessor definitions for Clang.

The value of this variable is a list of strings, where each
string is an additional definition to pass to Clang, via the `-D'
option."
  :type '(repeat (string :tag "Definition"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.15"))

(flycheck-def-option-var flycheck-clang-include-path nil c/c++-clang
  "A list of include directories for Clang.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of Clang.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.14"))

(flycheck-def-option-var flycheck-clang-includes nil c/c++-clang
  "A list of additional include files for Clang.

The value of this variable is a list of strings, where each
string is a file to include before syntax checking.  Relative
paths are relative to the file being checked."
  :type '(repeat (file :tag "Include file"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.15"))

(flycheck-def-option-var flycheck-clang-language-standard nil c/c++-clang
  "The language standard to use in Clang.

The value of this variable is either a string denoting a language
standard, or nil, to use the default standard.  When non-nil,
pass the language standard via the `-std' option."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Language standard"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "0.15"))
(make-variable-buffer-local 'flycheck-clang-language-standard)

(flycheck-def-option-var flycheck-clang-ms-extensions nil c/c++-clang
  "Whether to enable Microsoft extensions to C/C++ in Clang.

When non-nil, enable Microsoft extensions to C/C++ via
`-fms-extensions'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.16"))

(flycheck-def-option-var flycheck-clang-no-exceptions nil c/c++-clang
  "Whether to disable exceptions in Clang.

When non-nil, disable exceptions for syntax checks, via
`-fno-exceptions'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-clang-no-rtti nil c/c++-clang
  "Whether to disable RTTI in Clang.

When non-nil, disable RTTI for syntax checks, via `-fno-rtti'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.15"))

(flycheck-def-option-var flycheck-clang-pedantic nil c/c++-clang
  "Whether to warn about language extensions in Clang.

For ISO C, follows the version specified by any -std option used.
When non-nil, disable non-ISO extensions to C/C++ via
`-pedantic'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.23"))

(flycheck-def-option-var flycheck-clang-pedantic-errors nil c/c++-clang
  "Whether to error on language extensions in Clang.

For ISO C, follows the version specified by any -std option used.
When non-nil, disable non-ISO extensions to C/C++ via
`-pedantic-errors'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.23"))

(flycheck-def-option-var flycheck-clang-standard-library nil c/c++-clang
  "The standard library to use for Clang.

The value of this variable is the name of a standard library as
string, or nil to use the default standard library.

Refer to the Clang manual at URL
`http://clang.llvm.org/docs/UsersManual.html' for more
information about the standard library."
  :type '(choice (const :tag "Default standard library" nil)
                 (const "libc++")
                 (const :tag "GNU libstdc++" "libstdc++")
                 (string :tag "Library name"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "0.15"))

(flycheck-def-option-var flycheck-clang-warnings '("all" "extra") c/c++-clang
  "A list of additional warnings to enable in Clang.

The value of this variable is a list of strings, where each string
is the name of a warning category to enable.  By default, all
recommended warnings and some extra warnings are enabled (as by
`-Wall' and `-Wextra' respectively).

Refer to the Clang manual at URL
`http://clang.llvm.org/docs/UsersManual.html' for more
information about warnings."
  :type '(choice (const :tag "No additional warnings" nil)
                 (repeat :tag "Additional warnings"
                         (string :tag "Warning name")))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.14"))

(defun flycheck-c/c++-quoted-include-directory ()
  "Get the directory for quoted includes.

C/C++ compilers typically look up includes with quotation marks
in the directory of the file being compiled.  However, since
Flycheck uses temporary copies for syntax checking, it needs to
explicitly determine the directory for quoted includes.

This function determines the directory by looking at function
`buffer-file-name', or if that is nil, at `default-directory'."
  (-if-let (fn (buffer-file-name))
      (file-name-directory fn)
    ;; If the buffer has no file name, fall back to its default directory
    default-directory))

(flycheck-define-checker c/c++-clang
  "A C/C++ syntax checker using Clang.

See URL `http://clang.llvm.org/'."
  :command ("clang"
            "-fsyntax-only"
            "-fno-color-diagnostics"    ; Do not include color codes in output
            "-fno-caret-diagnostics"    ; Do not visually indicate the source
                                        ; location
            "-fno-diagnostics-show-option" ; Do not show the corresponding
                                        ; warning group
            "-iquote" (eval (flycheck-c/c++-quoted-include-directory))
            (option "-std=" flycheck-clang-language-standard concat)
            (option-flag "-pedantic" flycheck-clang-pedantic)
            (option-flag "-pedantic-errors" flycheck-clang-pedantic-errors)
            (option "-stdlib=" flycheck-clang-standard-library concat)
            (option-flag "-fms-extensions" flycheck-clang-ms-extensions)
            (option-flag "-fno-exceptions" flycheck-clang-no-exceptions)
            (option-flag "-fno-rtti" flycheck-clang-no-rtti)
            (option-flag "-fblocks" flycheck-clang-blocks)
            (option-list "-include" flycheck-clang-includes)
            (option-list "-W" flycheck-clang-warnings concat)
            (option-list "-D" flycheck-clang-definitions concat)
            (option-list "-I" flycheck-clang-include-path)
            (eval flycheck-clang-args)
            "-x" (eval
                  (pcase major-mode
                    ((or `c++-mode `c++-ts-mode) "c++")
                    ((or `c-mode `c-ts-mode) "c")))
            ;; Read from standard input
            "-")
  :standard-input t
  :error-patterns
  ((info line-start (or "<stdin>" (file-name)) ":" line ":" column
         ": note: " (optional (message)) line-end)
   (warning line-start (or "<stdin>" (file-name)) ":" line ":" column
            ": warning: " (optional (message)) line-end)
   (error line-start (or "<stdin>" (file-name)) ":" line ":" column
          ": " (or "fatal error" "error") ": " (optional (message)) line-end))
  :error-filter
  (lambda (errors)
    (let ((errors (flycheck-sanitize-errors errors)))
      (dolist (err errors)
        ;; Clang will output empty messages for #error/#warning pragmas without
        ;; messages.  We fill these empty errors with a dummy message to get
        ;; them past our error filtering
        (setf (flycheck-error-message err)
              (or (flycheck-error-message err) "no message")))
      errors))
  :modes (c-mode c++-mode c-ts-mode c++-ts-mode)
  :next-checkers ((warning . c/c++-cppcheck)))

(flycheck-def-args-var flycheck-gcc-args c/c++-gcc
  :package-version '(flycheck . "0.22"))

(flycheck-def-option-var flycheck-gcc-definitions nil c/c++-gcc
  "Additional preprocessor definitions for GCC.

The value of this variable is a list of strings, where each
string is an additional definition to pass to GCC, via the `-D'
option."
  :type '(repeat (string :tag "Definition"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gcc-include-path nil c/c++-gcc
  "A list of include directories for GCC.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of gcc.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gcc-includes nil c/c++-gcc
  "A list of additional include files for GCC.

The value of this variable is a list of strings, where each
string is a file to include before syntax checking.  Relative
paths are relative to the file being checked."
  :type '(repeat (file :tag "Include file"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gcc-language-standard nil c/c++-gcc
  "The language standard to use in GCC.

The value of this variable is either a string denoting a language
standard, or nil, to use the default standard.  When non-nil,
pass the language standard via the `-std' option."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Language standard"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "0.20"))
(make-variable-buffer-local 'flycheck-gcc-language-standard)

(flycheck-def-option-var flycheck-gcc-no-exceptions nil c/c++-gcc
  "Whether to disable exceptions in GCC.

When non-nil, disable exceptions for syntax checks, via
`-fno-exceptions'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gcc-no-rtti nil c/c++-gcc
  "Whether to disable RTTI in GCC.

When non-nil, disable RTTI for syntax checks, via `-fno-rtti'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gcc-openmp nil c/c++-gcc
  "Whether to enable OpenMP in GCC.

When non-nil, enable OpenMP for syntax checkers, via
`-fopenmp'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.21"))

(flycheck-def-option-var flycheck-gcc-pedantic nil c/c++-gcc
  "Whether to warn about language extensions in GCC.

For ISO C, follows the version specified by any -std option used.
When non-nil, disable non-ISO extensions to C/C++ via
`-pedantic'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.23"))

(flycheck-def-option-var flycheck-gcc-pedantic-errors nil c/c++-gcc
  "Whether to error on language extensions in GCC.

For ISO C, follows the version specified by any -std option used.
When non-nil, disable non-ISO extensions to C/C++ via
`-pedantic-errors'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.23"))

(flycheck-def-option-var flycheck-gcc-warnings '("all" "extra") c/c++-gcc
  "A list of additional warnings to enable in GCC.

The value of this variable is a list of strings, where each string
is the name of a warning category to enable.  By default, all
recommended warnings and some extra warnings are enabled (as by
`-Wall' and `-Wextra' respectively).

Refer to the gcc manual at URL
`https://gcc.gnu.org/onlinedocs/gcc/' for more information about
warnings."
  :type '(choice (const :tag "No additional warnings" nil)
                 (repeat :tag "Additional warnings"
                         (string :tag "Warning name")))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.20"))

(flycheck-define-checker c/c++-gcc
  "A C/C++ syntax checker using GCC.

Requires GCC 4.4 or newer.  See URL `https://gcc.gnu.org/'."
  :command ("gcc"
            "-fshow-column"
            "-iquote" (eval (flycheck-c/c++-quoted-include-directory))
            (option "-std=" flycheck-gcc-language-standard concat)
            (option-flag "-pedantic" flycheck-gcc-pedantic)
            (option-flag "-pedantic-errors" flycheck-gcc-pedantic-errors)
            (option-flag "-fno-exceptions" flycheck-gcc-no-exceptions)
            (option-flag "-fno-rtti" flycheck-gcc-no-rtti)
            (option-flag "-fopenmp" flycheck-gcc-openmp)
            (option-list "-include" flycheck-gcc-includes)
            (option-list "-W" flycheck-gcc-warnings concat)
            (option-list "-D" flycheck-gcc-definitions concat)
            (option-list "-I" flycheck-gcc-include-path)
            (eval flycheck-gcc-args)
            "-x" (eval
                  (pcase major-mode
                    ((or `c++-mode `c++-ts-mode) "c++")
                    ((or `c-mode `c-ts-mode) "c")))
            ;; GCC performs full checking only when actually compiling, so
            ;; `-fsyntax-only' is not enough. Just let it generate assembly
            ;; code.
            "-S" "-o" null-device
            ;; Read from standard input
            "-")
  :standard-input t
  :error-patterns
  ((info line-start (or "<stdin>" (file-name))
         ":" line (optional ":" column)
         ": note: " (message) line-end)
   (warning line-start (or "<stdin>" (file-name))
            ":" line (optional ":" column)
            ": warning: " (message (one-or-more (not (any "\n["))))
            (optional "[" (id (one-or-more not-newline)) "]") line-end)
   (error line-start (or "<stdin>" (file-name))
          ":" line (optional ":" column)
          ": " (or "fatal error" "error") ": " (message) line-end))
  :modes (c-mode c++-mode c-ts-mode c++-ts-mode)
  :next-checkers ((warning . c/c++-cppcheck)))

(flycheck-def-option-var flycheck-cppcheck-checks '("style") c/c++-cppcheck
  "Enabled checks for Cppcheck.

The value of this variable is a list of strings, where each
string is the name of an additional check to enable.  By default,
all coding style checks are enabled.

See section \"Enable message\" in the Cppcheck manual at URL
`http://cppcheck.sourceforge.net/manual.pdf', and the
documentation of the `--enable' option for more information,
including a list of supported checks."
  :type '(repeat :tag "Additional checks"
                 (string :tag "Check name"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.14"))

(flycheck-def-option-var flycheck-cppcheck-standards nil c/c++-cppcheck
  "The standards to use in cppcheck.

The value of this variable is either a list of strings denoting
the standards to use, or nil to pass nothing to cppcheck.  When
non-nil, pass the standards via one or more `--std=' options."
  :type '(choice (const :tag "Default" nil)
                 (repeat :tag "Custom standards"
                         (string :tag "Standard name")))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "28"))
(make-variable-buffer-local 'flycheck-cppcheck-standards)

(flycheck-def-option-var flycheck-cppcheck-suppressions-file nil c/c++-cppcheck
  "The suppressions file to use in cppcheck.

The value of this variable is a file with the suppressions to
use, or nil to pass nothing to cppcheck.  When non-nil, pass the
suppressions file via the `--suppressions-list=' option."
  :type '(choice (const :tag "Default" nil)
                 (file :tag "Suppressions file"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "32"))
(make-variable-buffer-local 'flycheck-cppcheck-suppressions-file)

(flycheck-def-option-var flycheck-cppcheck-suppressions nil c/c++-cppcheck
  "The suppressions to use in cppcheck.

The value of this variable is either a list of strings denoting
the suppressions to use, or nil to pass nothing to cppcheck.
When non-nil, pass the suppressions via one or more `--suppress='
options."
  :type '(choice (const :tag "Default" nil)
                 (repeat :tag "Additional suppressions"
                         (string :tag "Suppression")))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "28"))

(flycheck-def-option-var flycheck-cppcheck-inconclusive nil c/c++-cppcheck
  "Whether to enable Cppcheck inconclusive checks.

When non-nil, enable Cppcheck inconclusive checks.  This allows Cppcheck to
report warnings it's not certain of, but it may result in false positives.

This will have no effect when using Cppcheck 1.53 and older."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.19"))

(flycheck-def-option-var flycheck-cppcheck-include-path nil c/c++-cppcheck
  "A list of include directories for cppcheck.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of cppcheck.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.24"))

(flycheck-define-checker c/c++-cppcheck
  "A C/C++ checker using cppcheck.

See URL `http://cppcheck.sourceforge.net/'."
  :command ("cppcheck" "--quiet" "--xml-version=2" "--inline-suppr"
            (option "--enable=" flycheck-cppcheck-checks concat
                    flycheck-option-comma-separated-list)
            (option-flag "--inconclusive" flycheck-cppcheck-inconclusive)
            (option-list "-I" flycheck-cppcheck-include-path)
            (option-list "--std=" flycheck-cppcheck-standards concat)
            (option-list "--suppress=" flycheck-cppcheck-suppressions concat)
            (option "--suppressions-list="
                    flycheck-cppcheck-suppressions-file concat)
            "-x" (eval
                  (pcase major-mode
                    ((or `c++-mode `c++-ts-mode) "c++")
                    ((or `c-mode `c-ts-mode) "c")))
            source)
  :error-parser flycheck-parse-cppcheck
  :modes (c-mode c++-mode c-ts-mode c++-ts-mode))

(flycheck-define-checker cfengine
  "A CFEngine syntax checker using cf-promises.

See URL `https://cfengine.com/'."
  :command ("cf-promises" "-Wall" "-f"
            ;; We must stay in the same directory to resolve @include
            source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column
            ": warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column
          ": error: " (message) line-end))
  :modes (cfengine-mode cfengine3-mode))

(flycheck-def-option-var flycheck-foodcritic-tags nil chef-foodcritic
  "A list of tags to select for Foodcritic.

The value of this variable is a list of strings where each string
is a tag expression describing Foodcritic rules to enable or
disable, via the `--tags' option.  To disable a tag, prefix it
with `~'."
  :type '(repeat :tag "Tags" (string :tag "Tag expression"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.23"))

(flycheck-define-checker chef-foodcritic
  "A Chef cookbooks syntax checker using Foodcritic.

See URL `http://www.foodcritic.io'."
  ;; Use `source-inplace' to allow resource discovery with relative paths.
  ;; foodcritic interprets these as relative to the source file, so we need to
  ;; stay within the source tree.  See
  ;; https://github.com/flycheck/flycheck/pull/556
  :command ("foodcritic"
            (option-list "--tags" flycheck-foodcritic-tags)
            source-inplace)
  :error-patterns
  ((error line-start (id (one-or-more alnum)) ": "
          (message) ": " (file-name) ":" line line-end))
  :modes (enh-ruby-mode ruby-mode ruby-ts-mode)
  :predicate
  (lambda ()
    (let ((parent-dir (file-name-directory
                       (directory-file-name
                        (expand-file-name default-directory)))))
      (or
       ;; Chef CookBook
       ;; http://docs.opscode.com/chef/knife.html#id38
       (locate-dominating-file parent-dir "recipes")
       ;; Knife Solo
       ;; http://matschaffer.github.io/knife-solo/#label-Init+command
       (locate-dominating-file parent-dir "cookbooks")))))

(flycheck-define-checker coffee
  "A CoffeeScript syntax checker using coffee.

See URL `https://coffeescript.org/'."
  ;; --print suppresses generation of compiled .js files
  :command ("coffee" "--compile" "--print" "--stdio")
  :standard-input t
  :error-patterns
  ((error line-start "[stdin]:" line ":" column
          ": error: " (message) line-end))
  :modes coffee-mode
  :next-checkers ((warning . coffee-coffeelint)))

(flycheck-def-config-file-var flycheck-coffeelintrc coffee-coffeelint
                              ".coffeelint.json")

(flycheck-define-checker coffee-coffeelint
  "A CoffeeScript style checker using coffeelint.

See URL `http://www.coffeelint.org/'."
  :command
  ("coffeelint"
   (config-file "--file" flycheck-coffeelintrc)
   "--stdin" "--reporter" "checkstyle")
  :standard-input t
  :error-parser flycheck-parse-checkstyle
  :error-filter (lambda (errors)
                  (flycheck-remove-error-file-names
                   "stdin" (flycheck-remove-error-ids
                            (flycheck-sanitize-errors errors))))
  :modes coffee-mode)

(defun flycheck-coq-error-filter (errors)
  "Sanitize Coq ERRORS and compute end-lines and end-columns."
  (flycheck-increment-error-columns errors)
  (dolist (err errors)
    (setf (flycheck-error-message err)
          (replace-regexp-in-string (rx (1+ (syntax whitespace)) line-end)
                                    "" (flycheck-error-message err)
                                    'fixedcase 'literal))
    (-when-let* ((end-col (flycheck-error-end-column err)))
      ;; Coq reports an offset (potentially past eol), not an end column
      (let* ((line (flycheck-error-line err))
             (end-lc (save-excursion
                       (flycheck-goto-line line)
                       (goto-char (+ (point) (1- end-col)))
                       (flycheck-line-column-at-point))))
        (setf (flycheck-error-end-line err) (car end-lc))
        (setf (flycheck-error-end-column err) (cdr end-lc)))))
  (flycheck-sanitize-errors errors))

(flycheck-define-checker coq
  "A Coq syntax checker using the Coq compiler.

See URL `https://coq.inria.fr/'."
  ;; We use coqtop in batch mode, because coqc is picky about file names.
  :command ("coqtop" "-batch" "-load-vernac-source" source)
  :error-patterns
  ((error line-start "File \"" (file-name) "\", line " line
          ", characters " column "-" end-column ":\n"
          (or "Syntax error:" "Error:")
          ;; Most Coq error messages span multiple lines, and end with a dot.
          ;; There are simple one-line messages, too, though.
          (message (or (and (one-or-more (or not-newline "\n")) ".")
                       (one-or-more not-newline)))
          line-end))
  :error-filter flycheck-coq-error-filter
  :modes coq-mode)

(flycheck-define-checker css-csslint
  "A CSS syntax and style checker using csslint.

See URL `https://github.com/CSSLint/csslint'."
  :command ("csslint" "--format=checkstyle-xml" source)
  :error-parser flycheck-parse-checkstyle
  :error-filter flycheck-dequalify-error-ids
  :modes (css-mode css-ts-mode))

(defconst flycheck-stylelint-args '("--formatter" "json")
  "Common arguments to stylelint invocations.")

(flycheck-def-config-file-var flycheck-stylelintrc
    (css-stylelint scss-stylelint less-stylelint) nil)

(flycheck-def-option-var flycheck-stylelint-quiet
    nil (css-stylelint scss-stylelint less-stylelint)
  "Whether to run stylelint in quiet mode.

When non-nil, enable quiet mode, via `--quiet'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . 26))

(defconst flycheck-stylelint-error-re
  (flycheck-rx-to-string
   '(: line-start (id (one-or-more word)) ": " (message) line-end)))

(defun flycheck-parse-stylelint (output checker buffer)
  "Parse stylelint errors from OUTPUT.

CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

The CHECKER usually returns the errors as JSON.

If the CHECKER throws an Error it returns an Error message with a stacktrace."
  (condition-case nil
      (flycheck-parse-stylelint-json output checker buffer)

    ;; The output could not be parsed as JSON
    (json-error

     ;; Extract a flycheck error from the output (with a regular expression)
     ;; For match-string 4/5 see flycheck-rx-message/flycheck-rx-id
     (when (string-match flycheck-stylelint-error-re output)
       (list (flycheck-error-new-at
              1 nil 'error
              (match-string 4 output)
              :id (match-string 5 output)
              :checker checker
              :buffer buffer
              :filename (buffer-file-name buffer)))))))

(defun flycheck-parse-stylelint-json (output checker buffer)
  "Parse stylelint JSON errors from OUTPUT.

CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `http://stylelint.io/developer-guide/formatters/' for information
about the JSON format of stylelint."
  (let ((json-object-type 'plist))

    ;; stylelint returns a vector of result objects
    ;; Since we only passed one file, the first element is enough
    (let* ((stylelint-output (elt (json-read-from-string output) 0))
           (filename (buffer-file-name buffer))

           ;; Turn all deprecations into warnings
           (deprecations
            (mapcar (lambda (d)
                      (flycheck-error-new-at
                       1 nil 'warning
                       (plist-get d :text)
                       :id "Deprecation Warning"
                       :checker checker
                       :buffer buffer
                       :filename filename))
                    (plist-get stylelint-output :deprecations)))

           ;; Turn all invalid options into errors
           (invalid-options
            (mapcar (lambda (io)
                      (flycheck-error-new-at
                       1 nil 'error
                       (plist-get io :text)
                       :id "Invalid Option"
                       :checker checker
                       :buffer buffer
                       :filename filename))
                    (plist-get stylelint-output :invalidOptionWarnings)))

           ;; Read all linting warnings
           (warnings
            (mapcar (lambda (w)
                      (flycheck-error-new-at
                       (plist-get w :line) (plist-get w :column)
                       (pcase (plist-get w :severity)
                         (`"error"   'error)
                         (`"warning" 'warning)
                         ;; Default to info for unknown .severity
                         (_          'info))
                       (plist-get w :text)
                       :id (plist-get w :rule)
                       :checker checker
                       :buffer buffer
                       :filename filename))
                    (plist-get stylelint-output :warnings))))

      ;; Return the combined errors (deprecations, invalid options, warnings)
      (append deprecations invalid-options warnings))))

(flycheck-define-checker css-stylelint
  "A CSS syntax and style checker using stylelint.

See URL `http://stylelint.io/'."
  :command ("stylelint"
            (eval flycheck-stylelint-args)
            (option-flag "--quiet" flycheck-stylelint-quiet)
            (config-file "--config" flycheck-stylelintrc)
            "--stdin-filename" (eval (or (buffer-file-name) "style.css")))
  :standard-input t
  :error-parser flycheck-parse-stylelint
  :predicate flycheck-buffer-nonempty-p
  :modes (css-mode css-ts-mode))

(flycheck-def-option-var flycheck-cuda-language-standard nil cuda-nvcc
  "Our CUDA Language Standard."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Language standard"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "32"))
(make-variable-buffer-local 'flycheck-cuda-language-standard)

(flycheck-def-option-var flycheck-cuda-gencodes nil cuda-nvcc
  "Our real and virtual GPU architectures to pass to nvcc."
  :type '(repeat (file :tag "GPU architecture"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "32"))

(flycheck-def-option-var flycheck-cuda-includes nil cuda-nvcc
  "Our include directories to pass to nvcc."
  :type '(repeat (file :tag "Include file"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "32"))

(flycheck-def-option-var flycheck-cuda-definitions nil cuda-nvcc
  "Additional preprocessor definitions for nvcc.
A list of strings to pass to cuda, a la flycheck-clang"
  :type '(repeat (string :tag "Definitions"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "32"))

(flycheck-def-option-var flycheck-cuda-include-path nil cuda-nvcc
  "A list of include directories for nvcc."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "32"))

(flycheck-define-checker cuda-nvcc
  "A CUDA C/C++ syntax checker using nvcc.

See URL `https://developer.nvidia.com/cuda-llvm-compiler'."
  :command ("nvcc"
            "-c" ;; Compile Only
            "--output-file" "/dev/null" ;; avoid creating output .o
            "--x=cu" ;; explicitly specify it's a CUDA language file
            "-rdc=true" ;; Allow linking with external cuda funcions
            (option "-std=" flycheck-cuda-language-standard concat)
            (option-list "-include" flycheck-cuda-includes)
            (option-list "-gencode" flycheck-cuda-gencodes)
            (option-list "-D" flycheck-cuda-definitions concat)
            (option-list "-I" flycheck-cuda-include-path)
            source)
  :error-patterns
  ((error line-start
          (message "In file included from")
          " " (or "<stdin>" (file-name))
          ":" line ":" line-end)
   (error line-start (or "<stdin>" (file-name))
          "(" line "): error: " (message) line-end)
   (error line-start (or "<stdin>" (file-name))
          ":" line ":" column
          ": fatal error: " (optional (message)) line-end)
   (warning line-start (or "<stdin>" (file-name))
            "(" line "): warning: " (message) line-end))
  :modes cuda-mode)


(flycheck-def-option-var flycheck-cwl-schema-path nil cwl
  "A path for the schema file for Common Workflow Language.

The value of this variable is a string that denotes a path for
the schema file of Common Workflow Language."
  :type '(choice (const :tag "None" nil)
                 (file :tag "Schema file"))
  :safe #'flycheck-string-or-nil-p)

(flycheck-define-checker cwl
  "A CWL syntax checker using Schema Salad validator.

Requires Schema Salad 2.6.20171101113912 or newer.
See URL `https://www.commonwl.org/v1.0/SchemaSalad.html'."
  :command ("schema-salad-tool"
            "--quiet"
            "--print-oneline"
            (eval flycheck-cwl-schema-path)
            source-inplace)
  :error-patterns
  ((error line-start
          (file-name) ":" line ":" column ":" (zero-or-more blank)
          (message (one-or-more not-newline))
          line-end))
  :modes cwl-mode)

(defconst flycheck-d-module-re
  (rx "module" (one-or-more (syntax whitespace))
      (group (one-or-more (not (syntax whitespace))))
      (zero-or-more (syntax whitespace))
      ";")
  "Regular expression to match a D module declaration.")

(defun flycheck-d-base-directory ()
  "Get the relative base directory path for this module."
  (let* ((file-name (buffer-file-name))
         (module-file (if (and file-name
                               (string= (file-name-nondirectory file-name)
                                        "package.d"))
                          (directory-file-name (file-name-directory file-name))
                        file-name)))
    (flycheck-module-root-directory
     (flycheck-find-in-buffer flycheck-d-module-re)
     module-file)))

(flycheck-def-option-var flycheck-dmd-include-path nil d-dmd
  "A list of include directories for dmd.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of dmd.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.18"))

(flycheck-def-args-var flycheck-dmd-args d-dmd
  :package-version '(flycheck . "0.24"))

(flycheck-define-checker d-dmd
  "A D syntax checker using the DMD compiler.

Requires DMD 2.066 or newer.  See URL `https://dlang.org/'."
  :command ("dmd"
            "-debug"                    ; Compile in debug mode
            "-o-"                       ; Don't generate an object file
            "-vcolumns"                 ; Add columns in output
            "-wi" ; Compilation will continue even if there are warnings
            (eval (concat "-I" (flycheck-d-base-directory)))
            (option-list "-I" flycheck-dmd-include-path concat)
            (eval flycheck-dmd-args)
            (source ".d"))
  :error-patterns
  ((error line-start
          (file-name) "(" line "," column "): Error: " (message)
          line-end)
   (warning line-start (file-name) "(" line "," column "): "
            (or "Warning" "Deprecation") ": " (message) line-end)
   (info line-start (file-name) "(" line "," column "): "
         (one-or-more " ") (message) line-end))
  :modes d-mode)

(flycheck-define-checker dockerfile-hadolint
  "A Dockerfile syntax checker using the hadolint.

See URL `http://github.com/hadolint/hadolint/'."
  :command ("hadolint" "--no-color" "-")
  :standard-input t
  :error-patterns
  ((error line-start
          (file-name) ":" line " " (id (one-or-more alnum)) " error: " (message)
          line-end)
   (warning line-start
            (file-name) ":" line " " (id (one-or-more alnum))
            " warning: " (message) line-end)
   (info line-start
         (file-name) ":" line " " (id (one-or-more alnum)) " info: " (message)
         line-end)
   (error line-start
          (file-name) ":" line ":" column " " (message)
          line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors
     (flycheck-remove-error-file-names "-" errors)))
  :modes (dockerfile-mode dockerfile-ts-mode))

(defun flycheck-credo--working-directory (&rest _ignored)
  "Check if `credo' is installed as dependency in the application."
  (and buffer-file-name
       (locate-dominating-file buffer-file-name "deps/credo")))

(flycheck-def-option-var flycheck-elixir-credo-strict nil elixir-credo
  "Enable strict mode in `credo'.

When non-nil, pass the `--strict' flag to credo."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "32"))

(flycheck-define-checker elixir-credo
  "An Elixir checker for static code analysis using Credo.

See `http://credo-ci.org/'."
  :command ("mix" "credo"
            (option-flag "--strict" flycheck-elixir-credo-strict)
            "--format" "flycheck"
            "--read-from-stdin" source-original)
  :standard-input t
  :working-directory flycheck-credo--working-directory
  :enabled flycheck-credo--working-directory
  :error-patterns
  ((info line-start
         (file-name) ":" line (optional ":" column) ": "
         (or "F" "R" "C")  ": " (message) line-end)
   (warning line-start
            (file-name) ":" line (optional ":" column) ": "
            (or "D" "W")  ": " (message) line-end))
  :modes elixir-mode)

(defconst flycheck-this-emacs-executable
  (concat invocation-directory invocation-name)
  "The path to the currently running Emacs executable.")

(defconst flycheck-emacs-args '("-Q" "--batch")
  "Common arguments to Emacs invocations.")

(defmacro flycheck-prepare-emacs-lisp-form (&rest body)
  "Prepare BODY for use as check form in a subprocess."
  (declare (indent 0))
  `(flycheck-sexp-to-string
    '(progn
       (defvar jka-compr-inhibit)
       (unwind-protect
           ;; Flycheck inhibits compression of temporary files, thus we
           ;; must not attempt to decompress.
           (let ((jka-compr-inhibit t))
             ;; Strip option-argument separator from arguments, if present
             (when (equal (car command-line-args-left) "--")
               (setq command-line-args-left (cdr command-line-args-left)))
             ,@body)
         ;; Prevent Emacs from processing the arguments on its own, see
         ;; https://github.com/flycheck/flycheck/issues/319
         (setq command-line-args-left nil)))))

(defun flycheck-emacs-lisp-bytecomp-config-form ()
  "Prepare an Emacs Lisp form to set byte-compiler variables."
  (flycheck-sexp-to-string
   `(progn
      (require 'bytecomp)
      (setq byte-compile-root-dir
            ,(if buffer-file-name
                 (file-name-directory buffer-file-name)
               default-directory)))))

(defconst flycheck-emacs-lisp-check-form
  (flycheck-prepare-emacs-lisp-form
    ;; Keep track of the generated bytecode files, to delete them after byte
    ;; compilation.
    (require 'bytecomp)
    (defvar flycheck-byte-compiled-files nil)
    (let ((byte-compile-dest-file-function
           (lambda (source)
             (let ((temp-file (make-temp-file (file-name-nondirectory source))))
               (push temp-file flycheck-byte-compiled-files)
               temp-file))))
      (unwind-protect
          (byte-compile-file (car command-line-args-left))
        (mapc (lambda (f) (ignore-errors (delete-file f)))
              flycheck-byte-compiled-files))
      (when (bound-and-true-p flycheck-emacs-lisp-check-declare)
        (check-declare-file (car command-line-args-left))))))

(flycheck-def-option-var flycheck-emacs-lisp-load-path nil emacs-lisp
  "Load path to use in the Emacs Lisp syntax checker.

When set to `inherit', use the `load-path' of the current Emacs
session during syntax checking.

When set to a list of strings, add each directory in this list to
the `load-path' before invoking the byte compiler.  Relative
paths in this list are expanded against the `default-directory'
of the buffer to check.

When nil, do not explicitly set the `load-path' during syntax
checking.  The syntax check only uses the built-in `load-path' of
Emacs in this case.

Note that changing this variable can lead to wrong results of the
syntax check, e.g. if an unexpected version of a required library
is used."
  :type '(choice (const :tag "Inherit current `load-path'" inherit)
                 (repeat :tag "Load path" directory))
  :risky t
  :package-version '(flycheck . "0.14"))

(flycheck-def-option-var flycheck-emacs-lisp-initialize-packages
    'auto emacs-lisp
  "Whether to initialize packages in the Emacs Lisp syntax checker.

When nil, never initialize packages.  When `auto', initialize
packages only when checking `user-init-file' or files from
`user-emacs-directory'.  For any other non-nil value, always
initialize packages.

When initializing packages is enabled the `emacs-lisp' syntax
checker calls `package-initialize' before byte-compiling the file
to be checked.  It also sets `package-user-dir' according to
`flycheck-emacs-lisp-package-user-dir'."
  :type '(choice (const :tag "Do not initialize packages" nil)
                 (const :tag "Initialize packages for configuration only" auto)
                 (const :tag "Always initialize packages" t))
  :risky t
  :package-version '(flycheck . "0.14"))

(defconst flycheck-emacs-lisp-package-initialize-form
  (flycheck-sexp-to-string
   '(with-demoted-errors "Error during package initialization: %S"
      (package-initialize)))
  "Form used to initialize packages.")

(defun flycheck-option-emacs-lisp-package-initialize (value)
  "Option VALUE filter for `flycheck-emacs-lisp-initialize-packages'."
  (let ((shall-initialize
         (if (eq value 'auto)
             (or (flycheck-in-user-emacs-directory-p
                  (or buffer-file-name default-directory))
                 ;; `user-init-file' is nil in non-interactive sessions.  Now,
                 ;; no user would possibly use Flycheck in a non-interactive
                 ;; session, but our unit tests run non-interactively, so we
                 ;; have to handle this case anyway
                 (and user-init-file buffer-file-name
                      (flycheck-same-files-p buffer-file-name user-init-file)))
           value)))
    (when shall-initialize
      ;; If packages shall be initialized, return the corresponding form,
      ;; otherwise make Flycheck ignore the option by returning nil.
      flycheck-emacs-lisp-package-initialize-form)))

(flycheck-def-option-var flycheck-emacs-lisp-package-user-dir nil emacs-lisp
  "Package directory for the Emacs Lisp syntax checker.

If set to a string set `package-user-dir' to the value of this
variable before initializing packages. If set to nil just inherit
the value of `package-user-dir' from the running Emacs session.

This variable has no effect, if
`flycheck-emacs-lisp-initialize-packages' is nil."
  :type '(choice (const :tag "Default package directory" nil)
                 (directory :tag "Custom package directory"))
  :risky t
  :package-version '(flycheck . "0.14"))

(defun flycheck-option-emacs-lisp-package-user-dir (value)
  "Option VALUE filter for `flycheck-emacs-lisp-package-user-dir'."
  ;; Inherit the package directory from our Emacs session
  (let ((value (or value (bound-and-true-p package-user-dir))))
    (when value
      (flycheck-sexp-to-string `(setq package-user-dir ,value)))))

(flycheck-def-option-var flycheck-emacs-lisp-check-declare nil emacs-lisp
  "If non-nil, check ‘declare-function’ forms using ‘check-declare-file’."
  :type '(choice (const :tag "Do not check declare forms" nil)
                 (const :tag "Check declare forms" t))
  :risky t
  :package-version '(flycheck . "31"))

(defun flycheck-option-emacs-lisp-check-declare (value)
  "Option VALUE filter for `flycheck-emacs-lisp-check-declare'."
  (when value
    (flycheck-sexp-to-string
     `(progn
        (defvar flycheck-emacs-lisp-check-declare)
        (setq flycheck-emacs-lisp-check-declare ,value)))))

(defun flycheck--emacs-lisp-enabled-p ()
  "Check whether to enable Emacs Lisp checker in the current buffer."
  (not
   (or
    ;; Do not check buffers used for autoloads generation during package
    ;; installation.  These buffers are too short-lived for being checked, and
    ;; doing so causes spurious errors.  See
    ;; https://github.com/flycheck/flycheck/issues/45 and
    ;; https://github.com/bbatsov/prelude/issues/248.  We must also not check
    ;; compilation buffers, but as these are ephemeral, Flycheck won't check
    ;; them anyway.
    (flycheck-autoloads-file-p)
    ;; Cask/Carton and dir-locals files contain data, not code, and don't need
    ;; to follow Checkdoc conventions either.
    (and (buffer-file-name)
         (member (file-name-nondirectory (buffer-file-name))
                 '("Cask" "Carton" ".dir-locals.el" ".dir-locals-2.el"))))))

(defun flycheck--emacs-lisp-checkdoc-enabled-p ()
  "Check whether to enable Emacs Lisp Checkdoc in the current buffer."
  (and (flycheck--emacs-lisp-enabled-p)
       ;; These files are valid Lisp, but don't contain "standard" comments.
       (not (member (buffer-file-name) '("Eldev" "Eldev-local")))))

(flycheck-define-checker emacs-lisp
  "An Emacs Lisp syntax checker using the Emacs Lisp Byte compiler.

See Info Node `(elisp)Byte Compilation'."
  :command ("emacs" (eval flycheck-emacs-args)
            (eval
             (let ((path (pcase flycheck-emacs-lisp-load-path
                           (`inherit load-path)
                           (p (seq-map #'expand-file-name p)))))
               (flycheck-prepend-with-option "--directory" path)))
            (option "--eval" flycheck-emacs-lisp-package-user-dir nil
                    flycheck-option-emacs-lisp-package-user-dir)
            (option "--eval" flycheck-emacs-lisp-initialize-packages nil
                    flycheck-option-emacs-lisp-package-initialize)
            (option "--eval" flycheck-emacs-lisp-check-declare nil
                    flycheck-option-emacs-lisp-check-declare)
            "--eval" (eval (flycheck-emacs-lisp-bytecomp-config-form))
            "--eval" (eval flycheck-emacs-lisp-check-form)
            "--"
            source-inplace)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":"
          (zero-or-more whitespace) "Error:" (zero-or-more whitespace)
          (message (zero-or-more not-newline)
                   (zero-or-more "\n    " (zero-or-more not-newline)))
          line-end)
   (warning line-start (file-name) ":" line ":" column ":"
            (zero-or-more whitespace) "Warning:" (zero-or-more whitespace)
            (message (zero-or-more not-newline)
                     (zero-or-more "\n    " (zero-or-more not-newline)))
            line-end)
   (warning line-start (file-name) ":" line (optional ":" column) ":"
            (zero-or-more whitespace) "Warning (check-declare): said\n"
            (message (zero-or-more "    " (zero-or-more not-newline))
                     (zero-or-more "\n    " (zero-or-more not-newline)))
            line-end)
   ;; The following is for Emacs 24 ‘check-declare-file’, which uses a
   ;; less informative format.
   (warning line-start "Warning (check-declare): " (file-name) " said "
            (message (zero-or-more not-newline))
            line-end))
  :error-filter
  (lambda (errors)
    (flycheck-fill-empty-line-numbers
     (flycheck-collapse-error-message-whitespace
      (flycheck-sanitize-errors errors))))
  :modes (emacs-lisp-mode lisp-interaction-mode)
  :enabled flycheck--emacs-lisp-enabled-p
  :predicate
  (lambda ()
    ;; Do not check buffers that should not be byte-compiled.  The checker
    ;; process will refuse to compile these, which would confuse Flycheck
    (not (bound-and-true-p no-byte-compile)))
  :next-checkers (emacs-lisp-checkdoc))

(defconst flycheck-emacs-lisp-checkdoc-form
  (flycheck-prepare-emacs-lisp-form
    (unless (require 'elisp-mode nil 'no-error)
      ;; TODO: Fallback for Emacs 24, remove when dropping support for 24
      (require 'lisp-mode))
    (require 'checkdoc)

    (let ((source (car command-line-args-left))
          ;; Remember the default directory of the process
          (process-default-directory default-directory))
      ;; Note that we deliberately use our custom approach even despite of
      ;; `checkdoc-file' which was added to Emacs 25.1.  While it's conceptually
      ;; the better thing, its implementation has too many flaws to be of use
      ;; for us.
      (with-temp-buffer
        (insert-file-contents source 'visit)
        (setq buffer-file-name source)
        ;; And change back to the process default directory to make file-name
        ;; back-substutition work
        (setq default-directory process-default-directory)
        (with-demoted-errors "Error in checkdoc: %S"
          ;; Checkdoc needs the Emacs Lisp syntax table and comment syntax to
          ;; parse sexps and identify docstrings correctly; see
          ;; https://github.com/flycheck/flycheck/issues/833
          (delay-mode-hooks (emacs-lisp-mode))
          (setq delayed-mode-hooks nil)
          (checkdoc-current-buffer t)
          (with-current-buffer checkdoc-diagnostic-buffer
            (princ (buffer-substring-no-properties (point-min) (point-max)))
            (kill-buffer)))))))

(defconst flycheck-emacs-lisp-checkdoc-variables
  `(checkdoc-symbol-words
    checkdoc-arguments-in-order-flag
    checkdoc-force-history-flag
    checkdoc-permit-comma-termination-flag
    checkdoc-force-docstrings-flag
    checkdoc-package-keywords-flag
    checkdoc-spellcheck-documentation-flag
    checkdoc-verb-check-experimental-flag
    checkdoc-max-keyref-before-warn
    sentence-end-double-space
    ,@(and (>= emacs-major-version 28)
           '(checkdoc-column-zero-backslash-before-paren)))
  "Variables inherited by the checkdoc subprocess.")

(defun flycheck-emacs-lisp-checkdoc-variables-form ()
  "Make a sexp to pass relevant variables to a checkdoc subprocess.

Variables are taken from `flycheck-emacs-lisp-checkdoc-variables'."
  `(progn
     ,@(seq-map (lambda (opt) `(setq-default ,opt ',(symbol-value opt)))
                (seq-filter #'boundp flycheck-emacs-lisp-checkdoc-variables))))

(flycheck-define-checker emacs-lisp-checkdoc
  "An Emacs Lisp style checker using CheckDoc.

The checker runs `checkdoc-current-buffer'."
  :command ("emacs" (eval flycheck-emacs-args)
            "--eval" (eval (flycheck-sexp-to-string
                            (flycheck-emacs-lisp-checkdoc-variables-form)))
            "--eval" (eval flycheck-emacs-lisp-checkdoc-form)
            "--" source)
  :error-patterns
  ((info line-start (file-name) ":" line ": " (message) line-end))
  :modes (emacs-lisp-mode)
  :enabled flycheck--emacs-lisp-checkdoc-enabled-p)

(dolist (checker '(emacs-lisp emacs-lisp-checkdoc))
  (setf (car (flycheck-checker-get checker 'command))
        flycheck-this-emacs-executable))

(defun flycheck-ember-template--check-for-config (&rest _ignored)
  "Check the required config file is available up the file system."
  (and buffer-file-name
       (locate-dominating-file buffer-file-name ".template-lintrc.js")))

(defun flycheck-ember-template--parse-error (output checker buffer)
  "Parse Ember-template-lint errors/warnings from JSON OUTPUT.
CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively."
  (mapcar (lambda (err)
            (let-alist err
              (flycheck-error-new-at
               .line
               .column
               (pcase .severity
                 (2 'error)
                 (1 'warning)
                 (_ 'warning))
               .message
               :id .rule
               :checker checker
               :buffer buffer
               :filename (buffer-file-name buffer))))
          (cdr (car (car (flycheck-parse-json output))))))

(flycheck-def-config-file-var flycheck-ember-template-lintrc
    ember-template
    ".template-lintrc.js")

(flycheck-define-checker ember-template
  "An Ember template checker using ember-template-lint."
  :command ("ember-template-lint"
            (config-file "--config-path" flycheck-ember-template-lintrc)
            "--filename" source-original
            "--format=json")
  :standard-input t
  :error-parser flycheck-ember-template--parse-error
  :modes web-mode
  :enabled flycheck-ember-template--check-for-config
  :working-directory flycheck-ember-template--check-for-config)

(flycheck-def-option-var flycheck-erlang-include-path nil erlang
  "A list of include directories for Erlang.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of erlc.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.24"))

(flycheck-def-option-var flycheck-erlang-library-path nil erlang
  "A list of library directories for Erlang.

The value of this variable is a list of strings, where each
string is a directory to add to the library path of erlc.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Library directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.24"))

(flycheck-define-checker erlang
  "An Erlang syntax checker using the Erlang interpreter.

See URL `http://www.erlang.org/'."
  :command ("erlc"
            "-o" temporary-directory
            (option-list "-I" flycheck-erlang-include-path)
            (option-list "-pa" flycheck-erlang-library-path)
            "-Wall"
            source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" (optional column ":")
            " Warning:" (message) line-end)
   (error line-start (file-name) ":" line ":" (optional column ":") " "
          (message) line-end))
  :modes erlang-mode
  :enabled (lambda () (string-suffix-p ".erl" (buffer-file-name))))

(defun flycheck--contains-rebar-config (dir-name)
  "Return DIR-NAME if rebar config file exists in DIR-NAME, nil otherwise."
  (when (or (file-exists-p (expand-file-name "rebar.config" dir-name))
            (file-exists-p (expand-file-name "rebar.config.script" dir-name)))
    dir-name))

(defun flycheck--locate-rebar3-project-root
    (file-name &optional prev-file-name acc)
  "Find the top-most rebar project root for source FILE-NAME.

A project root directory is any directory containing a
rebar.config file.  Find the top-most directory to move out of any
nested dependencies.

FILE-NAME is a source file for which to find the project.

PREV-FILE-NAME helps us prevent infinite looping

ACC is an accumulator that keeps the list of results, the first
non-nil of which will be our project root.

Return the absolute path to the directory"
  (if (string= file-name prev-file-name)
      (car (remove nil acc))
    (let ((current-dir (file-name-directory file-name)))
      (flycheck--locate-rebar3-project-root
       (directory-file-name current-dir)
       file-name
       (cons (flycheck--contains-rebar-config current-dir) acc)))))

(defun flycheck-rebar3-project-root (&optional _checker)
  "Return directory where rebar.config is located."
  (flycheck--locate-rebar3-project-root buffer-file-name))

(flycheck-def-option-var flycheck-erlang-rebar3-profile nil erlang-rebar3
  "The rebar3 profile to use.

The profile used when compiling, if VALUE is nil \"test\" will be used
when the file is located in test directory, otherwise \"default\" will be
used as profile."
  :type '(choice (const :tag "Automatic" nil)
                 (string :tag "Profile"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "32"))

(defun flycheck-erlang-rebar3-get-profile ()
  "Return rebar3 profile.

Use flycheck-erlang-rebar3-profile if set, otherwise use test or eqc profile if
directory name is \"test\" or \"eqc\", or else \"default\"."
  (or
   flycheck-erlang-rebar3-profile
   (with-no-warnings
     ;; `seq-contains-p' is only in seq >= 2.21
     (seq-contains '("test" "eqc")
                   (and buffer-file-name
                        (file-name-base
                         (directory-file-name
                          (file-name-directory buffer-file-name))))))
   "default"))

(flycheck-define-checker erlang-rebar3
  "An Erlang syntax checker using the rebar3 build tool."
  :command ("rebar3" "as" (eval (flycheck-erlang-rebar3-get-profile)) "compile")
  :error-parser flycheck-parse-with-patterns-without-color
  :error-patterns
  ((warning line-start (file-name) ":" line ":" (optional column ":")
            " Warning:" (message) line-end)
   (error line-start (file-name) ":" line ":" (optional column ":") " "
          (message) line-end))
  :modes erlang-mode
  :enabled flycheck-rebar3-project-root
  :predicate flycheck-buffer-saved-p
  :working-directory flycheck-rebar3-project-root)

(flycheck-define-checker eruby-erubis
  "An eRuby syntax checker using the `erubis' command.

See URL `http://www.kuwata-lab.com/erubis/'."
  :command ("erubis" "-z" source)
  :error-patterns
  ((error line-start (file-name) ":" line ": " (message) line-end))
  :modes (html-erb-mode rhtml-mode)
  :next-checkers ((warning . eruby-ruumba)))

(flycheck-def-config-file-var flycheck-ruumbarc eruby-ruumba ".ruumba.yml")

(flycheck-def-option-var flycheck-ruumba-lint-only nil eruby-ruumba
  "Whether to only report code issues in Ruumba.

When non-nil, only report code issues in Ruumba, via `--lint'.
Otherwise report style issues as well."
  :safe #'booleanp
  :type 'boolean
  :package-version '(flycheck . "32"))

(flycheck-define-checker eruby-ruumba
  "An eRuby syntax and style checker using the Ruumba tool.

You need at least Ruumba 0.1.7 for this syntax checker.

See URL `https://github.com/ericqweinstein/ruumba'."
  :command ("ruumba"
            "--display-cop-names"
            "--force-exclusion"
            "--format" "emacs"
            "--cache" "false"
            (config-file "--config" flycheck-ruumbarc)
            (option-flag "--lint" flycheck-ruumba-lint-only)
            ;; Ruumba takes the original file name as argument when reading
            ;; from standard input
            "--stdin" source-original)
  :standard-input t
  :working-directory flycheck-ruby--find-project-root
  :error-patterns
  ((info line-start (file-name) ":" line ":" column ": C: "
         (optional (id (one-or-more (not (any ":")))) ": ") (message) line-end)
   (warning line-start (file-name) ":" line ":" column ": W: "
            (optional (id (one-or-more (not (any ":")))) ": ") (message)
            line-end)
   (error line-start (file-name) ":" line ":" column ": " (or "E" "F") ": "
          (optional (id (one-or-more (not (any ":")))) ": ") (message)
          line-end))
  :modes (html-erb-mode rhtml-mode))

(flycheck-def-args-var flycheck-gfortran-args fortran-gfortran
  :package-version '(flycheck . "0.22"))

(flycheck-def-option-var flycheck-gfortran-include-path nil fortran-gfortran
  "A list of include directories for GCC Fortran.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of gcc.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gfortran-language-standard "f95"
                         fortran-gfortran
  "The language standard to use in GFortran.

The value of this variable is either a string denoting a language
standard, or nil, to use the default standard.  When non-nil,
pass the language standard via the `-std' option."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Language standard"))
  :package-version '(flycheck . "0.20"))

(flycheck-def-option-var flycheck-gfortran-layout nil fortran-gfortran
  "The source code layout to use in GFortran.

The value of this variable is one of the following symbols:

nil
     Let gfortran determine the layout from the extension

`free'
     Use free form layout


`fixed'
     Use fixed form layout

In any other case, an error is signaled."
  :type '(choice (const :tag "Guess layout from extension" nil)
                 (const :tag "Free form layout" free)
                 (const :tag "Fixed form layout" fixed))
  :safe (lambda (value) (or (not value) (memq value '(free fixed))))
  :package-version '(flycheck . "0.20"))

(defun flycheck-option-gfortran-layout (value)
  "Option VALUE filter for `flycheck-gfortran-layout'."
  (pcase value
    (`nil nil)
    (`free "free-form")
    (`fixed "fixed-form")
    (_ (error "Invalid value for flycheck-gfortran-layout: %S" value))))

(flycheck-def-option-var flycheck-gfortran-warnings '("all" "extra")
                         fortran-gfortran
  "A list of warnings for GCC Fortran.

The value of this variable is a list of strings, where each string
is the name of a warning category to enable.  By default, all
recommended warnings and some extra warnings are enabled (as by
`-Wall' and `-Wextra' respectively).

Refer to the gfortran manual at URL
`https://gcc.gnu.org/onlinedocs/gfortran/' for more information
about warnings"
  :type '(choice (const :tag "No additional warnings" nil)
                 (repeat :tag "Additional warnings"
                         (string :tag "Warning name")))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.20"))

(flycheck-define-checker fortran-gfortran
  "An Fortran syntax checker using GCC.

Uses GCC's Fortran compiler gfortran.  See URL
`https://gcc.gnu.org/onlinedocs/gfortran/'."
  :command ("gfortran"
            "-fsyntax-only"
            "-fshow-column"
            ;; Do not visually indicate the source location
            "-fno-diagnostics-show-caret"
            ;; Do not show the corresponding warning group
            "-fno-diagnostics-show-option"
            ;; Fortran has similar include processing as C/C++
            "-iquote" (eval (flycheck-c/c++-quoted-include-directory))
            (option "-std=" flycheck-gfortran-language-standard concat)
            (option "-f" flycheck-gfortran-layout concat
                    flycheck-option-gfortran-layout)
            (option-list "-W" flycheck-gfortran-warnings concat)
            (option-list "-I" flycheck-gfortran-include-path concat)
            (eval flycheck-gfortran-args)
            source)
  :error-patterns
  ((error line-start (file-name) ":" line (or ":" ".") column (or ": " ":\n")
          (or (= 3 (zero-or-more not-newline) "\n") "")
          (or "Error" "Fatal Error") ": "
          (message) line-end)
   (warning line-start (file-name) ":" line (or ":" ".") column (or ": " ":\n")
            (or (= 3 (zero-or-more not-newline) "\n") "")
            "Warning: " (message) line-end))
  :modes (fortran-mode f90-mode))

(flycheck-define-checker go-gofmt
  "A Go syntax and style checker using the gofmt utility.

See URL `https://golang.org/cmd/gofmt/'."
  :command ("gofmt")
  :standard-input t
  :error-patterns
  ((error line-start "<standard input>:" line ":" column ": "
          (message) line-end))
  :modes (go-mode go-ts-mode)
  :next-checkers ((warning . go-golint)
                  ;; Fall back, if go-golint doesn't exist
                  (warning . go-vet)
                  ;; Fall back, if go-vet doesn't exist
                  (warning . go-build) (warning . go-test)
                  (warning . go-errcheck)
                  (warning . go-unconvert)
                  (warning . go-staticcheck)))

(flycheck-define-checker go-golint
  "A Go style checker using Golint.

See URL `https://github.com/golang/lint'."
  :command ("golint" source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": " (message) line-end))
  :modes (go-mode go-ts-mode)
  :next-checkers (go-vet
                  ;; Fall back, if go-vet doesn't exist
                  go-build go-test go-errcheck go-unconvert))

(flycheck-def-option-var flycheck-go-vet-print-functions nil go-vet
  "A list of print-like functions for `go vet'.

Go vet will check these functions for format string problems and
issues, such as a mismatch between the number of formats used,
and the number of arguments given.

Each entry is in the form Name:N where N is the zero-based
argument position of the first argument involved in the print:
either the format or the first print argument for non-formatted
prints.  For example, if you have Warn and Warnf functions that
take an io.Writer as their first argument, like Fprintf,
-printfuncs=Warn:1,Warnf:1 "
  :type '(repeat :tag "print-like functions"
                 (string :tag "function"))
  :safe #'flycheck-string-list-p)

(flycheck-define-checker go-vet
  "A Go syntax checker using the `go vet' command.

See URL `https://golang.org/cmd/go/' and URL
`https://golang.org/cmd/vet/'."
  :command ("go" "vet"
            (option "-printf.funcs=" flycheck-go-vet-print-functions concat
                    flycheck-option-comma-separated-list)
            (source ".go"))
  :error-patterns
  ((warning line-start (file-name) ":" line ": " (message) line-end))
  :modes (go-mode go-ts-mode)
  :next-checkers (go-build
                  go-test
                  ;; Fall back if `go build' or `go test' can be used
                  go-errcheck
                  go-unconvert
                  go-staticcheck)
  :verify (lambda (_)
            (let* ((go (flycheck-checker-executable 'go-vet))
                   (have-vet (member "vet" (ignore-errors
                                             (process-lines go "tool")))))
              (list
               (flycheck-verification-result-new
                :label "go tool vet"
                :message (if have-vet "present" "missing")
                :face (if have-vet 'success '(bold error)))))))

(flycheck-def-option-var flycheck-go-build-install-deps nil (go-build go-test)
  "Whether to install dependencies in `go build' and `go test'.

If non-nil automatically install dependencies with `go build'
while syntax checking."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.25"))

(flycheck-def-option-var flycheck-go-build-tags nil
                         (go-build go-test go-errcheck go-staticcheck)
  "A list of tags for `go build'.

Each item is a string with a tag to be given to `go build'."
  :type '(repeat (string :tag "Tag"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.25"))


(flycheck-def-option-var flycheck-go-version nil go-staticcheck
  "The version of go that should be targeted by `staticcheck'.

Should be a string representing a version, like 1.6 or 1.11.4.
See `https://staticcheck.io/docs/#targeting-go-versions' for
details."
  :type '(choice (const :tag "Unspecified" nil)
                 (string :tag "Version"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "0.32"))

(flycheck-define-checker go-build
  "A Go syntax and type checker using the `go build' command.

Requires Go 1.6 or newer.  See URL `https://golang.org/cmd/go'."
  :command ("go" "build"
            (option-flag "-i" flycheck-go-build-install-deps)
            ;; multiple tags are listed as "dev debug ..."
            (option-list "-tags=" flycheck-go-build-tags concat)
            "-o" null-device)
  :error-patterns
  ((error line-start (file-name) ":" line ":"
          (optional column ":") " "
          (message (one-or-more not-newline)
                   (zero-or-more "\n\t" (one-or-more not-newline)))
          line-end)
   ;; Catch error message about multiple packages in a directory, which doesn't
   ;; follow the standard error message format.
   (info line-start
         (message "can't load package: package "
                  (one-or-more (not (any ?: ?\n)))
                  ": found packages "
                  (one-or-more not-newline))
         line-end))
  :error-filter
  (lambda (errors)
    (dolist (error errors)
      (unless (flycheck-error-line error)
        ;; Flycheck ignores errors without line numbers, but the error
        ;; message about multiple packages in a directory doesn't come with a
        ;; line number, so inject a fake one.
        (setf (flycheck-error-line error) 1)))
    errors)
  :modes (go-mode go-ts-mode)
  :predicate (lambda ()
               (and (flycheck-buffer-saved-p)
                    (not (string-suffix-p "_test.go" (buffer-file-name)))))
  :next-checkers ((warning . go-errcheck)
                  (warning . go-unconvert)
                  (warning . go-staticcheck)))

(flycheck-define-checker go-test
  "A Go syntax and type checker using the `go test' command.

Requires Go 1.6 or newer.  See URL `https://golang.org/cmd/go'."
  :command ("go" "test"
            (option-flag "-i" flycheck-go-build-install-deps)
            (option-list "-tags=" flycheck-go-build-tags concat)
            "-c" "-o" null-device)
  :error-patterns
  ((error line-start (file-name) ":" line ":"
          (optional column ":") " "
          (message (one-or-more not-newline)
                   (zero-or-more "\n\t" (one-or-more not-newline)))
          line-end))
  :modes (go-mode go-ts-mode)
  :predicate
  (lambda () (and (flycheck-buffer-saved-p)
                  (string-suffix-p "_test.go" (buffer-file-name))))
  :next-checkers ((warning . go-errcheck)
                  (warning . go-unconvert)
                  (warning . go-staticcheck)))

(flycheck-define-checker go-errcheck
  "A Go checker for unchecked errors.

Requires errcheck newer than commit 8515d34 (Aug 28th, 2015).

See URL `https://github.com/kisielk/errcheck'."
  :command ("errcheck"
            "-abspath"
            (option-list "-tags=" flycheck-go-build-tags concat)
            ".")
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" column (or (one-or-more "\t") ": " ":\t")
            (message)
            line-end))
  :error-filter
  (lambda (errors)
    (let ((errors (flycheck-sanitize-errors errors)))
      (dolist (err errors)
        (-when-let (message (flycheck-error-message err))
          ;; Improve the messages reported by errcheck to make them more clear.
          (setf (flycheck-error-message err)
                (format "Ignored `error` returned from `%s`" message)))))
    errors)
  :modes (go-mode go-ts-mode)
  :predicate (lambda () (flycheck-buffer-saved-p))
  :next-checkers ((warning . go-unconvert)
                  (warning . go-staticcheck)))

(flycheck-define-checker go-unconvert
  "A Go checker looking for unnecessary type conversions.

See URL `https://github.com/mdempsky/unconvert'."
  :command ("unconvert" ".")
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": " (message) line-end))
  :modes (go-mode go-ts-mode)
  :predicate (lambda () (flycheck-buffer-saved-p)))

(flycheck-define-checker go-staticcheck
  "A Go checker that performs static analysis and linting using
the `staticcheck' command.

`staticcheck' is explicitly fully compatible with \"the last two
versions of go\". `staticheck' can target earlier versions (with
limited features) if `flycheck-go-version' is set. See URL
`https://staticcheck.io/'."
  :command ("staticcheck" "-f" "json"
            (option-list "-tags" flycheck-go-build-tags concat)
            (option "-go" flycheck-go-version))

  :error-parser flycheck-parse-go-staticcheck
  :modes (go-mode go-ts-mode))

(flycheck-define-checker groovy
  "A groovy syntax checker using groovy compiler API.

See URL `http://www.groovy-lang.org'."
  :command ("groovy" "-e"
            "import org.codehaus.groovy.control.*

unit = new CompilationUnit()
unit.addSource(\"input\", System.in)

try {
    unit.compile(Phases.CONVERSION)
} catch (MultipleCompilationErrorsException e) {
    e.errorCollector.write(new PrintWriter(System.out, true), null)
}")
  :standard-input t
  :error-patterns
  ((error line-start "input: " line ":" (message)
          " @ line " line ", column " column "." line-end))
  :modes groovy-mode)

(flycheck-define-checker haml
  "A Haml syntax checker using the Haml compiler.

See URL `http://haml.info'."
  :command ("haml" "-c" "--stdin")
  :standard-input t
  :error-patterns
  ((error line-start "Syntax error on line " line ": " (message) line-end)
   (error line-start ":" line ": syntax error, " (message) line-end))
  :modes haml-mode)

(flycheck-define-checker handlebars
  "A Handlebars syntax checker using the Handlebars compiler.

See URL `http://handlebarsjs.com/'."
  :command ("handlebars" "-i-")
  :standard-input t
  :error-patterns
  ((error line-start
          "Error: Parse error on line " line ":" (optional "\r") "\n"
          (zero-or-more not-newline) "\n" (zero-or-more not-newline) "\n"
          (message) line-end))
  :modes (handlebars-mode handlebars-sgml-mode web-mode)
  :predicate
  (lambda ()
    (if (eq major-mode 'web-mode)
        ;; Check if this is a handlebars file since web-mode does not store the
        ;; non-canonical engine name
        (let* ((regexp-alist (bound-and-true-p web-mode-engine-file-regexps))
               (pattern (cdr (assoc "handlebars" regexp-alist))))
          (and pattern (buffer-file-name)
               (string-match-p pattern (buffer-file-name))))
      t)))

(defconst flycheck-haskell-module-re
  (rx line-start (zero-or-more (or "\n" (any space)))
      "module" (one-or-more (or "\n" (any space)))
      (group (one-or-more (not (any space "(" "\n")))))
  "Regular expression for a Haskell module name.")

(flycheck-def-args-var flycheck-ghc-args (haskell-stack-ghc haskell-ghc)
  :package-version '(flycheck . "0.22"))

(flycheck-def-option-var flycheck-ghc-stack-use-nix nil haskell-stack-ghc
  "Whether to enable nix support in stack.

When non-nil, stack will append '--nix' flag to any call."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "26"))

(flycheck-def-option-var flycheck-ghc-stack-project-file nil haskell-stack-ghc
  "Override project stack.yaml file.

The value of this variable is a file path that refers to a yaml
file for the current stack project. Relative file paths are
resolved against the checker's working directory. When non-nil,
stack will get overridden value via `--stack-yaml'."
  :type '(choice (const :tag "Unspecified" nil)
                 (file :tag "Project file"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "32"))

(flycheck-def-option-var flycheck-ghc-no-user-package-database nil haskell-ghc
  "Whether to disable the user package database in GHC.

When non-nil, disable the user package database in GHC, via
`-no-user-package-db'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.16"))

(flycheck-def-option-var flycheck-ghc-package-databases nil haskell-ghc
  "Additional module databases for GHC.

The value of this variable is a list of strings, where each
string is a directory of a package database.  Each package
database is given to GHC via `-package-db'."
  :type '(repeat (directory :tag "Package database"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.16"))

(flycheck-def-option-var flycheck-ghc-search-path nil
                         (haskell-stack-ghc haskell-ghc)
  "Module search path for (Stack) GHC.

The value of this variable is a list of strings, where each
string is a directory containing Haskell modules.  Each directory
is added to the GHC search path via `-i'."
  :type '(repeat (directory :tag "Module directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.16"))

(flycheck-def-option-var flycheck-ghc-language-extensions nil
                         (haskell-stack-ghc haskell-ghc)
  "Language extensions for (Stack) GHC.

The value of this variable is a list of strings, where each
string is a Haskell language extension, as in the LANGUAGE
pragma.  Each extension is enabled via `-X'."
  :type '(repeat (string :tag "Language extension"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.19"))

(defvar flycheck-haskell-ghc-cache-directory nil
  "The cache directory for `ghc' output.")

(defun flycheck-haskell-ghc-cache-directory ()
  "Get the cache location for `ghc' output.

If no cache directory exists yet, create one and return it.
Otherwise return the previously used cache directory."
  (setq flycheck-haskell-ghc-cache-directory
        (or flycheck-haskell-ghc-cache-directory
            (make-temp-file "flycheck-haskell-ghc-cache" 'directory))))

(defun flycheck--locate-dominating-file-matching (directory regexp)
  "Search for a file in directory hierarchy starting at DIRECTORY.

Look up the directory hierarchy from DIRECTORY for a directory
containing a file that matches REGEXP."
  (locate-dominating-file
   directory
   (lambda (dir)
     (directory-files dir nil regexp t))))

(defun flycheck-haskell--find-stack-default-directory ()
  "Find a directory to run haskell-stack-ghc.

Return a parent directory with a stack*.y[a]ml file, or the
directory returned by \"stack path --project-root\"."
  (or
   (when (buffer-file-name)
     (flycheck--locate-dominating-file-matching
      (file-name-directory (buffer-file-name))
      (rx "stack" (* any) "." (or "yml" "yaml") eos)))
   (-when-let* ((stack (funcall flycheck-executable-find "stack"))
                (output (ignore-errors
                          (process-lines stack
                                         "--no-install-ghc"
                                         "path" "--project-root")))
                (stack-dir (car output)))
     (and (file-directory-p stack-dir) stack-dir))))

(defun flycheck-haskell--ghc-find-default-directory (_checker)
  "Find a parent directory containing a cabal or package.yaml file."
  (when (buffer-file-name)
    (flycheck--locate-dominating-file-matching
     (file-name-directory (buffer-file-name))
     "\\.cabal\\'\\|\\`package\\.yaml\\'")))

(flycheck-define-checker haskell-stack-ghc
  "A Haskell syntax and type checker using `stack ghc'.

See URL `https://github.com/commercialhaskell/stack'."
  :command ("stack"
            "--no-install-ghc"
            (option "--stack-yaml" flycheck-ghc-stack-project-file)
            (option-flag "--nix" flycheck-ghc-stack-use-nix)
            "ghc" "--" "-Wall" "-no-link"
            "-outputdir" (eval (flycheck-haskell-ghc-cache-directory))
            (option-list "-X" flycheck-ghc-language-extensions concat)
            (option-list "-i" flycheck-ghc-search-path concat)
            (eval (concat
                   "-i"
                   (flycheck-module-root-directory
                    (flycheck-find-in-buffer flycheck-haskell-module-re))))
            (eval flycheck-ghc-args)
            "-x" (eval
                  (pcase major-mode
                    (`haskell-mode "hs")
                    (`haskell-literate-mode "lhs")))
            source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":"
            (or " " "\n    ") (in "Ww") "arning:"
            (optional " " "[" (id (one-or-more not-newline)) "]")
            (optional "\n")
            (message
             (one-or-more " ") (one-or-more not-newline)
             (zero-or-more "\n"
                           (one-or-more " ")
                           (one-or-more (not (any ?\n ?|)))))
            line-end)
   (error line-start (file-name) ":" line ":" column ":" (optional " error:")
          (or (message (one-or-more not-newline))
              (and "\n"
                   (message
                    (one-or-more " ") (one-or-more not-newline)
                    (zero-or-more "\n"
                                  (one-or-more " ")
                                  (one-or-more (not (any ?\n ?|)))))))
          line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors (flycheck-dedent-error-messages errors)))
  :modes (haskell-mode haskell-literate-mode)
  :next-checkers ((warning . haskell-hlint))
  :working-directory (lambda (_)
                       (flycheck-haskell--find-stack-default-directory))
  :enabled flycheck-haskell--find-stack-default-directory
  :verify (lambda (_)
            (let* ((stack (flycheck-haskell--find-stack-default-directory)))
              (list
               (flycheck-verification-result-new
                :label "stack config"
                :message (or stack "Not found")
                :face (if stack 'success '(bold error)))))))

(flycheck-define-checker haskell-ghc
  "A Haskell syntax and type checker using ghc.

See URL `https://www.haskell.org/ghc/'."
  :command ("ghc" "-Wall" "-no-link"
            "-outputdir" (eval (flycheck-haskell-ghc-cache-directory))
            (option-flag "-no-user-package-db"
                         flycheck-ghc-no-user-package-database)
            (option-list "-package-db" flycheck-ghc-package-databases)
            (option-list "-i" flycheck-ghc-search-path concat)
            ;; Include the parent directory of the current module tree, to
            ;; properly resolve local imports
            (eval (concat
                   "-i"
                   (flycheck-module-root-directory
                    (flycheck-find-in-buffer flycheck-haskell-module-re))))
            (option-list "-X" flycheck-ghc-language-extensions concat)
            (eval flycheck-ghc-args)
            "-x" (eval
                  (pcase major-mode
                    (`haskell-mode "hs")
                    (`haskell-literate-mode "lhs")))
            source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":"
            (or " " "\n    ") (in "Ww") "arning:"
            (optional " " "[" (id (one-or-more not-newline)) "]")
            (optional "\n")
            (message
             (one-or-more " ") (one-or-more not-newline)
             (zero-or-more "\n"
                           (one-or-more " ")
                           (one-or-more (not (any ?\n ?|)))))
            line-end)
   (error line-start (file-name) ":" line ":" column ":" (optional " error:")
          (or (message (one-or-more not-newline))
              (and "\n"
                   (message
                    (one-or-more " ") (one-or-more not-newline)
                    (zero-or-more "\n"
                                  (one-or-more " ")
                                  (one-or-more (not (any ?\n ?|)))))))
          line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors (flycheck-dedent-error-messages errors)))
  :modes (haskell-mode haskell-literate-mode)
  :next-checkers ((warning . haskell-hlint))
  :working-directory flycheck-haskell--ghc-find-default-directory)

(flycheck-def-config-file-var flycheck-hlintrc haskell-hlint "HLint.hs")

(flycheck-def-args-var flycheck-hlint-args haskell-hlint
  :package-version '(flycheck . "0.25"))

(flycheck-def-option-var flycheck-hlint-language-extensions
    nil haskell-hlint
  "Extensions list to enable for hlint.

The value of this variable is a list of strings, where each
string is a name of extension to enable in
hlint (e.g. \"QuasiQuotes\")."
  :type '(repeat :tag "Extensions" (string :tag "Extension"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.24"))

(flycheck-def-option-var flycheck-hlint-ignore-rules
    nil haskell-hlint
  "Ignore rules list for hlint checks.

The value of this variable is a list of strings, where each
string is an ignore rule (e.g. \"Use fmap\")."
  :type '(repeat :tag "Ignore rules" (string :tag "Ignore rule"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.24"))

(flycheck-def-option-var flycheck-hlint-hint-packages
    nil haskell-hlint
  "Hint packages to include for hlint checks.

The value of this variable is a list of strings, where each
string is a default hint package (e.g. (\"Generalise\"
\"Default\" \"Dollar\"))."
  :type '(repeat :tag "Hint packages" (string :tag "Hint package"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.24"))

(flycheck-define-checker haskell-hlint
  "A Haskell style checker using hlint.

See URL `https://github.com/ndmitchell/hlint'."
  :command ("hlint"
            (option-list "-X" flycheck-hlint-language-extensions concat)
            (option-list "-i=" flycheck-hlint-ignore-rules concat)
            (option-list "-h" flycheck-hlint-hint-packages concat)
            (config-file "-h" flycheck-hlintrc)
            (eval flycheck-hlint-args)
            source-inplace)
  :error-patterns
  ((info line-start
         (file-name) ":"
         (or (seq line ":" column (optional "-" end-column))
             (seq "(" line "," column ")-(" end-line "," end-column ")"))
         ": Suggestion: "
         (message (one-or-more (and (one-or-more (not (any ?\n))) ?\n)))
         line-end)
   (warning line-start
            (file-name) ":"
            (or (seq line ":" column (optional "-" end-column))
                (seq "(" line "," column ")-(" end-line "," end-column ")"))
            ": Warning: "
            (message (one-or-more (and (one-or-more (not (any ?\n))) ?\n)))
            line-end)
   (error line-start
          (file-name) ":"
          (or (seq line ":" column (optional "-" end-column))
              (seq "(" line "," column ")-(" end-line "," end-column ")"))
          ": Error: "
          (message (one-or-more (and (one-or-more (not (any ?\n))) ?\n)))
          line-end))
  :modes (haskell-mode haskell-literate-mode))

(flycheck-def-config-file-var flycheck-tidyrc html-tidy ".tidyrc")

(flycheck-define-checker html-tidy
  "A HTML syntax and style checker using Tidy.

See URL `https://github.com/htacg/tidy-html5'."
  :command ("tidy" (config-file "-config" flycheck-tidyrc)
            "-lang" "en"
            "-e" "-q")
  :standard-input t
  :error-patterns
  ((error line-start
          "line " line
          " column " column
          " - Error: " (message) line-end)
   (warning line-start
            "line " line
            " column " column
            " - Warning: " (message) line-end))
  :modes (html-mode mhtml-mode nxhtml-mode))

(flycheck-def-config-file-var flycheck-jshintrc javascript-jshint ".jshintrc")

(flycheck-def-option-var flycheck-jshint-extract-javascript nil
                         javascript-jshint
  "Whether jshint should extract Javascript from HTML.

If nil no extract rule is given to jshint.  If `auto' only
extract Javascript if a HTML file is detected.  If `always' or
`never' extract Javascript always or never respectively.

Refer to the jshint manual at the URL
`http://jshint.com/docs/cli/#flags' for more information."
  :type
  '(choice (const :tag "No extraction rule" nil)
           (const :tag "Try to extract Javascript when detecting HTML files"
                  auto)
           (const :tag "Always try to extract Javascript" always)
           (const :tag "Never try to extract Javascript" never))
  :safe #'symbolp
  :package-version '(flycheck . "26"))

(flycheck-define-checker javascript-jshint
  "A Javascript syntax and style checker using jshint.

See URL `http://www.jshint.com'."
  :command ("jshint" "--reporter=checkstyle"
            "--filename" source-original
            (config-file "--config" flycheck-jshintrc)
            (option "--extract=" flycheck-jshint-extract-javascript
                    concat flycheck-option-symbol)
            "-")
  :standard-input t
  :error-parser flycheck-parse-checkstyle
  :error-filter
  (lambda (errors)
    (flycheck-remove-error-file-names
     "stdin" (flycheck-dequalify-error-ids errors)))
  :modes (js-mode js2-mode js3-mode rjsx-mode js-ts-mode))

(flycheck-def-args-var flycheck-eslint-args javascript-eslint
  :package-version '(flycheck . "32"))

(flycheck-def-option-var flycheck-eslint-rules-directories nil javascript-eslint
  "A list of directories with custom rules for ESLint.

The value of this variable is a list of strings, where each
string is a directory with custom rules for ESLint.

Refer to the ESLint manual at URL
`http://eslint.org/docs/user-guide/command-line-interface#--rulesdir'
for more information about the custom directories."
  :type '(repeat (directory :tag "Custom rules directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "29"))

(defun flycheck-eslint-config-exists-p ()
  "Whether there is a valid eslint config for the current buffer."
  (eql 0 (flycheck-call-checker-process
          'javascript-eslint nil nil nil
          "--print-config" (or buffer-file-name "index.js"))))

(defun flycheck-parse-eslint (output checker buffer)
  "Parse ESLint errors/warnings from JSON OUTPUT.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://eslint.org' for more information about ESLint."
  (mapcar (lambda (err)
            (let-alist err
              (flycheck-error-new-at
               .line
               .column
               (pcase .severity
                 (2 'error)
                 (1 'warning)
                 (_ 'warning))
               .message
               :id .ruleId
               :checker checker
               :buffer buffer
               :filename (buffer-file-name buffer)
               :end-line .endLine
               :end-column .endColumn)))
          (let-alist (caar (flycheck-parse-json output))
            .messages)))

(defun flycheck-eslint--find-working-directory (_checker)
  "Look for a working directory to run ESLint CHECKER in.

This will be the directory that contains the `node_modules'
directory.  If no such directory is found in the directory
hierarchy, it looks first for `.eslintignore' and then for
`.eslintrc' files to detect the project root."
  (let* ((regex-config (concat "\\`\\.eslintrc"
                               "\\(\\.\\(js\\|ya?ml\\|json\\)\\)?\\'")))
    (when buffer-file-name
      (or (locate-dominating-file buffer-file-name "node_modules")
          (locate-dominating-file buffer-file-name ".eslintignore")
          (locate-dominating-file
           (file-name-directory buffer-file-name)
           (lambda (directory)
             (> (length (directory-files directory nil regex-config t)) 0)))))))

(flycheck-define-checker javascript-eslint
  "A Javascript syntax and style checker using eslint.

See URL `https://eslint.org/'."
  :command ("eslint" "--format=json"
            (option-list "--rulesdir" flycheck-eslint-rules-directories)
            (eval flycheck-eslint-args)
            "--stdin" "--stdin-filename" source-original)
  :standard-input t
  :error-parser flycheck-parse-eslint
  :enabled (lambda () (flycheck-eslint-config-exists-p))
  :modes (js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode rjsx-mode
                  typescript-mode js-ts-mode typescript-ts-mode tsx-ts-mode)
  :working-directory flycheck-eslint--find-working-directory
  :verify
  (lambda (_)
    (let* ((default-directory
             (flycheck-compute-working-directory 'javascript-eslint))
           (have-config (flycheck-eslint-config-exists-p)))
      (list
       (flycheck-verification-result-new
        :label "config file"
        :message (if have-config "found" "missing or incorrect")
        :face (if have-config 'success '(bold error))))))
  :error-explainer
  (lambda (err)
    (let ((error-code (flycheck-error-id err))
          (url "https://eslint.org/docs/rules/%s"))
      (and error-code
           ;; skip non-builtin rules
           (not ;; `seq-contains-p' is only in seq >= 2.21
            (with-no-warnings (seq-contains error-code ?/)))
           `(url . ,(format url error-code))))))

(flycheck-define-checker javascript-standard
  "A Javascript code and style checker for the (Semi-)Standard Style.

This checker works with `standard' and `semistandard', defaulting
to the former.  To use it with the latter, set
`flycheck-javascript-standard-executable' to `semistandard'.

See URL `https://github.com/standard/standard' and URL
`https://github.com/Flet/semistandard'."
  :command ("standard" "--stdin")
  :standard-input t
  :error-patterns
  ((error line-start "  <text>:" line ":" column ":" (message) line-end))
  :modes (js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode rjsx-mode
                  js-ts-mode))

(flycheck-define-checker json-jsonlint
  "A JSON syntax and style checker using jsonlint.

See URL `https://github.com/zaach/jsonlint'."
  ;; We can't use standard input for jsonlint, because it doesn't output errors
  ;; anymore when using -c -q with standard input :/
  :command ("jsonlint" "-c" "-q" source)
  :error-patterns
  ((error line-start
          (file-name)
          ": line " line
          ", col " column ", "
          (message) line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors (flycheck-increment-error-columns errors)))
  :modes (json-mode js-json-mode json-ts-mode))

(flycheck-define-checker json-python-json
  "A JSON syntax checker using Python json.tool module.

See URL `https://docs.python.org/3.5/library/json.html#command-line-interface'."
  :command ("python3" "-m" "json.tool" source
            ;; Send the pretty-printed output to the null device
            null-device)
  :error-patterns
  ((error line-start
          (message) ": line " line " column " column
          ;; Ignore the rest of the line which shows the char position.
          (one-or-more not-newline)
          line-end))
  :modes (json-mode js-json-mode json-ts-mode)
  ;; The JSON parser chokes if the buffer is empty and has no JSON inside
  :predicate flycheck-buffer-nonempty-p)

(flycheck-define-checker json-jq
  "JSON checker using the jq tool.

This checker accepts multiple consecutive JSON values in a
single input, which is useful for jsonlines data.

See URL `https://stedolan.github.io/jq/'."
  :command ("jq" "." source null-device)
  ;; Example error message:
  ;;   parse error: Expected another key-value pair at line 3, column 1
  :error-patterns
  ((error line-start
          (optional "parse error: ")
          (message) "at line " line ", column " column
          (zero-or-more not-newline) line-end))
  :modes (json-mode js-json-mode json-ts-mode))

(flycheck-define-checker jsonnet
  "A Jsonnet syntax checker using the jsonnet binary.

See URL `https://jsonnet.org'."
  :command ("jsonnet" source-inplace)
  :error-patterns
  ((error line-start "STATIC ERROR: " (file-name) ":"
          (or (seq line ":" column (zero-or-one (seq "-" end-column)))
              (seq "(" line ":" column ")" "-"
                   "(" end-line ":" end-column ")"))
          ": " (message) line-end)
   (error line-start "RUNTIME ERROR: " (message) "\n"
          (? "\t" (file-name) ":" ;; first line of the backtrace
             (or (seq line ":" column (zero-or-one (seq "-" end-column)))
                 (seq "(" line ":" column ")" "-"
                      "(" end-line ":" end-column ")")))))
  :error-filter
  (lambda (errs)
    ;; Some errors are missing line numbers. See URL
    ;; `https://github.com/google/jsonnet/issues/786'.
    (dolist (err errs)
      (unless (flycheck-error-line err)
        (setf (flycheck-error-line err) 1)))
    (flycheck-sanitize-errors errs))
  :modes jsonnet-mode)

(flycheck-define-checker less
  "A LESS syntax checker using lessc.

Requires lessc 1.4 or newer.

See URL `http://lesscss.org'."
  :command ("lessc" "--lint" "--no-color"
            "-")
  :standard-input t
  :error-patterns
  ((error line-start (one-or-more word) ":"
          (message)
          " in - on line " line
          ", column " column ":"
          line-end))
  :modes less-css-mode)

(flycheck-define-checker less-stylelint
  "A LESS syntax and style checker using stylelint.

See URL `http://stylelint.io/'."
  :command ("stylelint"
            (eval flycheck-stylelint-args)
            "--syntax" "less"
            (option-flag "--quiet" flycheck-stylelint-quiet)
            (config-file "--config" flycheck-stylelintrc))
  :standard-input t
  :error-parser flycheck-parse-stylelint
  :predicate flycheck-buffer-nonempty-p
  :modes (less-css-mode))

(flycheck-define-checker llvm-llc
  "Flycheck LLVM IR checker using llc.

See URL `http://llvm.org/docs/CommandGuide/llc.html'."
  :command ("llc" "-o" null-device source)
  :error-patterns
  ((error line-start
          ;; llc prints the executable path
          (zero-or-one (minimal-match (one-or-more not-newline)) ": ")
          (file-name) ":" line ":" column ": error: " (message)
          line-end))
  :error-filter
  (lambda (errors)
    ;; sanitize errors occurring in inline assembly
    (flycheck-sanitize-errors
     (flycheck-remove-error-file-names "<inline asm>" errors)))
  :modes llvm-mode)

(flycheck-def-config-file-var flycheck-luacheckrc lua-luacheck ".luacheckrc")

(flycheck-def-option-var flycheck-luacheck-standards nil lua-luacheck
  "The standards to use in luacheck.

The value of this variable is either a list of strings denoting
the standards to use, or nil to pass nothing to luacheck.  When
non-nil, pass the standards via one or more `--std' options."
  :type '(choice (const :tag "Default" nil)
                 (repeat :tag "Custom standards"
                         (string :tag "Standard name")))
  :safe #'flycheck-string-list-p)
(make-variable-buffer-local 'flycheck-luacheck-standards)

(flycheck-define-checker lua-luacheck
  "A Lua syntax checker using luacheck.

See URL `https://github.com/mpeterv/luacheck'."
  :command ("luacheck"
            "--formatter" "plain"
            "--codes"                   ; Show warning codes
            "--no-color"
            (option-list "--std" flycheck-luacheck-standards)
            (config-file "--config" flycheck-luacheckrc)
            "--filename" source-original
            ;; Read from standard input
            "-")
  :standard-input t
  :error-patterns
  ((warning line-start
            (optional (file-name))
            ":" line ":" column
            ": (" (id "W" (one-or-more digit)) ") "
            (message) line-end)
   (error line-start
          (optional (file-name))
          ":" line ":" column ":"
          ;; `luacheck' before 0.11.0 did not output codes for errors, hence
          ;; the ID is optional here
          (optional " (" (id "E" (one-or-more digit)) ") ")
          (message) line-end))
  :modes lua-mode)

(flycheck-define-checker lua
  "A Lua syntax checker using the Lua compiler.

See URL `http://www.lua.org/'."
  :command ("luac" "-p" "-")
  :standard-input t
  :error-patterns
  ((error line-start
          ;; Skip the name of the luac executable.
          (minimal-match (zero-or-more not-newline))
          ": stdin:" line ": " (message) line-end))
  :modes lua-mode)

(flycheck-define-checker opam
  "A Opam syntax and style checker using opam lint.

See URL `https://opam.ocaml.org/doc/man/opam-lint.html'."
  :command ("opam" "lint" "-")
  :standard-input t
  :error-patterns
  ((error line-start                    ; syntax error
          (one-or-more space) "error  " (id ?2)
          ": File format error"
          (or (and " at line " line ", column " column ": " (message))
              (and ": " (message)))
          line-end)
   (error line-start
          (one-or-more space) "error  " (id ?3)
          (minimal-match (zero-or-more not-newline))
          "at line " line ", column " column ": " (message)
          line-end)
   (error line-start
          (one-or-more space) "error " (id (one-or-more num))
          ": " (message (one-or-more not-newline))
          line-end)
   (warning line-start
            (one-or-more space) "warning " (id (one-or-more num))
            ": " (message)
            line-end))
  :error-filter
  (lambda (errors)
    (flycheck-increment-error-columns
     (flycheck-fill-empty-line-numbers errors)))
  :modes tuareg-opam-mode)

(flycheck-def-option-var flycheck-perl-include-path nil perl
  "A list of include directories for Perl.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of Perl.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.24"))

(flycheck-def-option-var flycheck-perl-module-list nil perl
  "A list of modules to use for Perl.

The value of this variable is a list of strings, where each
string is a module to `use' in Perl."
  :type '(repeat :tag "Module")
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "32"))

(flycheck-define-checker perl
  "A Perl syntax checker using the Perl interpreter.

See URL `https://www.perl.org'."
  :command ("perl" "-w" "-c"
            (option-list "-I" flycheck-perl-include-path)
            (option-list "-M" flycheck-perl-module-list concat))
  :standard-input t
  :error-patterns
  ((error line-start (minimal-match (message))
          " at - line " line
          (or "." (and ", " (zero-or-more not-newline))) line-end))
  :modes (perl-mode cperl-mode)
  :next-checkers (perl-perlcritic))

(flycheck-def-option-var flycheck-perlcritic-severity nil perl-perlcritic
  "The message severity for Perl Critic.

The value of this variable is a severity level as integer, for
the `--severity' option to Perl Critic."
  :type '(integer :tag "Severity level")
  :safe #'integerp
  :package-version '(flycheck . "0.18"))

(flycheck-def-option-var flycheck-perlcritic-theme nil perl-perlcritic
  "The theme expression for Perl Critic.

The value of this variable is passed as the `--theme' option to
`Perl::Critic'.  See the documentation of `Perl::Critic' for
details."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Theme expression"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "32-csv"))

(flycheck-def-config-file-var flycheck-perlcriticrc perl-perlcritic
                              ".perlcriticrc"
  :package-version '(flycheck . "26"))

(flycheck-define-checker perl-perlcritic
  "A Perl syntax checker using Perl::Critic.

See URL `https://metacpan.org/pod/Perl::Critic'."
  :command ("perlcritic" "--no-color" "--verbose" "%f/%l/%c/%s/%p/%m (%e)\n"
            (config-file "--profile" flycheck-perlcriticrc)
            (option "--severity" flycheck-perlcritic-severity nil
                    flycheck-option-int)
            (option "--theme" flycheck-perlcritic-theme))
  :standard-input t
  :error-patterns
  ((info line-start
         "STDIN/" line "/" column "/" (any "1") "/"
         (id (one-or-more (not (any "/")))) "/" (message)
         line-end)
   (warning line-start
            "STDIN/" line "/" column "/" (any "234") "/"
            (id (one-or-more (not (any "/")))) "/" (message)
            line-end)
   (error line-start
          "STDIN/" line "/" column "/" (any "5") "/"
          (id (one-or-more (not (any "/")))) "/" (message)
          line-end))
  :modes (cperl-mode perl-mode))

(flycheck-define-checker php
  "A PHP syntax checker using the PHP command line interpreter.

See URL `http://php.net/manual/en/features.commandline.php'."
  :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
            "-d" "log_errors=0" source)
  :error-patterns
  ((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
          (message) " in " (file-name) " on line " line line-end))
  :modes (php-mode php+-mode)
  :next-checkers ((warning . php-phpmd)
                  (warning . php-phpcs)))

(flycheck-def-option-var flycheck-phpmd-rulesets
    '("cleancode" "codesize" "controversial" "design" "naming" "unusedcode")
    php-phpmd
  "The rule sets for PHP Mess Detector.

Set default rule sets and custom rule set files.

See section \"Using multiple rule sets\" in the PHP Mess Detector
manual at URL `https://phpmd.org/documentation/index.html'."
  :type '(repeat :tag "rule sets"
                 (string :tag "A filename or rule set"))
  :safe #'flycheck-string-list-p)

(flycheck-define-checker php-phpmd
  "A PHP style checker using PHP Mess Detector.

See URL `https://phpmd.org/'."
  :command ("phpmd" source "xml"
            (eval (flycheck-option-comma-separated-list
                   flycheck-phpmd-rulesets)))
  :error-parser flycheck-parse-phpmd
  :modes (php-mode php+-mode)
  :next-checkers (php-phpcs))

(flycheck-def-option-var flycheck-phpcs-standard nil php-phpcs
  "The coding standard for PHP CodeSniffer.

When nil, use the default standard from the global PHP
CodeSniffer configuration.  When set to a string, pass the string
to PHP CodeSniffer which will interpret it as name as a standard,
or as path to a standard specification."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Standard name or file"))
  :safe #'flycheck-string-or-nil-p)

(flycheck-define-checker php-phpcs
  "A PHP style checker using PHP Code Sniffer.

Needs PHP Code Sniffer 2.6 or newer.

See URL `http://pear.php.net/package/PHP_CodeSniffer/'."
  :command ("phpcs" "--report=checkstyle"
            ;; Use -q flag to force quiet mode
            ;; Quiet mode prevents errors from extra output when phpcs has
            ;; been configured with show_progress enabled
            "-q"
            (option "--standard=" flycheck-phpcs-standard concat)
            ;; Some files are not detected correctly
            ;; so it is necessary to pass the extension.
            (eval
             (-when-let* ((fname buffer-file-name)
                          (ext (file-name-extension fname)))
               (concat "--extensions=" ext)))

            ;; Pass original file name to phpcs.  We need to concat explicitly
            ;; here, because phpcs really insists to get option and argument as
            ;; a single command line argument :|
            (eval (when (buffer-file-name)
                    (concat "--stdin-path=" (buffer-file-name))))
            ;; Read from standard input
            "-")
  :standard-input t
  :error-parser flycheck-parse-checkstyle
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors
     (flycheck-remove-error-file-names "STDIN" errors)))
  :modes (php-mode php+-mode)
  ;; phpcs seems to choke on empty standard input, hence skip phpcs if the
  ;; buffer is empty, see https://github.com/flycheck/flycheck/issues/907
  :predicate flycheck-buffer-nonempty-p)

(flycheck-define-checker processing
  "Processing command line tool.

See https://github.com/processing/processing/wiki/Command-Line"
  :command ("processing-java" "--force"
            ;; Don't change the order of these arguments, processing is pretty
            ;; picky
            (eval (concat "--sketch=" (file-name-directory (buffer-file-name))))
            (eval (concat "--output=" (flycheck-temp-dir-system)))
            "--build")
  :error-patterns
  ((error line-start (file-name) ":" line ":" column
          (zero-or-more (or digit ":")) (message) line-end))
  :modes processing-mode
  ;; This syntax checker needs a file name
  :predicate (lambda () (buffer-file-name)))

(defun flycheck-proselint-parse-errors (output checker buffer)
  "Parse proselint json output errors from OUTPUT.

CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `http://proselint.com/' for more information about proselint."
  (mapcar (lambda (err)
            (let-alist err
              (flycheck-error-new-at-pos
               .start
               (pcase .severity
                 (`"suggestion" 'info)
                 (`"warning"    'warning)
                 (`"error"      'error)
                 ;; Default to error
                 (_             'error))
               .message
               :id .check
               :buffer buffer
               :checker checker
               ;; See https://github.com/amperser/proselint/issues/1048
               :end-pos .end)))
          (let-alist (car (flycheck-parse-json output))
            .data.errors)))

(flycheck-define-checker proselint
  "Flycheck checker using Proselint.

See URL `http://proselint.com/'."
  :command ("proselint" "--json" "-")
  :standard-input t
  :error-parser flycheck-proselint-parse-errors
  :modes (text-mode markdown-mode gfm-mode message-mode org-mode))

(flycheck-def-option-var flycheck-protoc-import-path nil protobuf-protoc
  "A list of directories to resolve import directives.

The value of this variable is a list of strings, where each
string is a directory to add to the import path.  Relative paths
are relative to the file being checked."
  :type '(repeat (directory :tag "Import directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "32"))

(flycheck-define-checker protobuf-protoc
  "A protobuf syntax checker using the protoc compiler.

See URL `https://developers.google.com/protocol-buffers/'."
  :command ("protoc" "--error_format" "gcc"
            (eval (concat "--java_out=" (flycheck-temp-dir-system)))
            ;; Add the current directory to resolve imports
            (eval (concat "--proto_path="
                          (file-name-directory (buffer-file-name))))
            ;; Add other import paths; this needs to be after the current
            ;; directory to produce the right output.  See URL
            ;; `https://github.com/flycheck/flycheck/pull/1655'
            (option-list "--proto_path=" flycheck-protoc-import-path concat)
            source-inplace)
  :error-patterns
  ((info line-start (file-name) ":" line ":" column
         ": note: " (message) line-end)
   (error line-start (file-name) ":" line ":" column
          ": " (message) line-end)
   (error line-start
          (message "In file included from") " " (file-name) ":" line ":"
          column ":" line-end))
  :modes protobuf-mode
  :predicate (lambda () (buffer-file-name)))

(defun flycheck-prototool-project-root (&optional _checker)
  "Return the nearest directory holding the prototool.yaml configuration."
  (and buffer-file-name
       (locate-dominating-file buffer-file-name "prototool.yaml")))

(flycheck-define-checker protobuf-prototool
  "A protobuf syntax checker using prototool.

See URL `https://github.com/uber/prototool'."
  :command ("prototool" "lint" source-original)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":" (message) line-end))
  :modes protobuf-mode
  :enabled flycheck-prototool-project-root
  :predicate flycheck-buffer-saved-p)

(flycheck-define-checker pug
  "A Pug syntax checker using the pug compiler.

See URL `https://pugjs.org/'."
  :command ("pug" "-p" (eval (expand-file-name (buffer-file-name))))
  :standard-input t
  :error-patterns
  ;; errors with includes/extends (e.g. missing files)
  ((error "Error: " (message) (zero-or-more not-newline) "\n"
          (zero-or-more not-newline) "at "
          (zero-or-more not-newline) " line " line)
   ;; error when placing anything other than a mixin or
   ;; block at the top-level of an extended template
   ;; also unknown filters
   (error line-start "Error: " (file-name) ":"
          line ":" column "\n\n" (message) line-end)
   ;; syntax/runtime errors (e.g. type errors, bad indentation, etc.)
   (error line-start
          (optional "Type") "Error: "  (file-name) ":"
          line (optional ":" column)
          (zero-or-more not-newline) "\n"
          (one-or-more (or (zero-or-more not-newline) "|"
                           (zero-or-more not-newline) "\n")
                       (zero-or-more "-")  (zero-or-more not-newline) "|"
                       (zero-or-more not-newline) "\n")
          (zero-or-more not-newline) "\n"
          (one-or-more
           (zero-or-more not-newline) "|"
           (zero-or-more not-newline) "\n")
          (zero-or-more not-newline) "\n"
          (message)
          line-end))
  :modes pug-mode)

(flycheck-define-checker puppet-parser
  "A Puppet DSL syntax checker using puppet's own parser.

See URL `https://puppet.com/'."
  :command ("puppet" "parser" "validate" "--color=false")
  :standard-input t
  :error-patterns
  (
   ;; Patterns for Puppet 4
   (error line-start "Error: Could not parse for environment "
          (one-or-more (in "a-z" "0-9" "_")) ":"
          (message) "(line: " line ", column: " column ")" line-end)
   ;; Errors from Puppet < 4
   (error line-start "Error: Could not parse for environment "
          (one-or-more (in "a-z" "0-9" "_")) ":"
          (message (minimal-match (one-or-more anything)))
          " at line " line line-end)
   (error line-start
          ;; Skip over the path of the Puppet executable
          (minimal-match (zero-or-more not-newline))
          ": Could not parse for environment " (one-or-more word)
          ": " (message (minimal-match (zero-or-more anything)))
          " at " (file-name "/" (zero-or-more not-newline)) ":" line line-end))
  :modes puppet-mode
  :next-checkers ((warning . puppet-lint)))

(flycheck-def-config-file-var flycheck-puppet-lint-rc puppet-lint
                              ".puppet-lint.rc"
  :package-version '(flycheck . "26"))

(flycheck-def-option-var flycheck-puppet-lint-disabled-checks nil puppet-lint
  "Disabled checkers for `puppet-lint'.

The value of this variable is a list of strings, where each
string is the name of a check to disable (e.g. \"80chars\" or
\"double_quoted_strings\").

See URL `http://puppet-lint.com/checks/' for a list of all checks
and their names."
  :type '(repeat (string :tag "Check Name"))
  :package-version '(flycheck . "26"))

(defun flycheck-puppet-lint-disabled-arg-name (check)
  "Create an argument to disable a puppetlint CHECK."
  (concat "--no-" check "-check"))

(flycheck-define-checker puppet-lint
  "A Puppet DSL style checker using puppet-lint.

See URL `http://puppet-lint.com/'."
  ;; We must check the original file, because Puppetlint is quite picky on the
  ;; names of files and there place in the directory structure, to comply with
  ;; Puppet's autoload directory layout.  For instance, a class foo::bar is
  ;; required to be in a file foo/bar.pp.  Any other place, such as a Flycheck
  ;; temporary file will cause an error.
  :command ("puppet-lint"
            (config-file "--config" flycheck-puppet-lint-rc)
            "--log-format"
            "%{path}:%{line}:%{kind}: %{message} (%{check})"
            (option-list "" flycheck-puppet-lint-disabled-checks concat
                         flycheck-puppet-lint-disabled-arg-name)
            source-original)
  :error-patterns
  ((warning line-start (file-name) ":" line ":warning: " (message) line-end)
   (error line-start (file-name) ":" line ":error: " (message) line-end))
  :modes puppet-mode
  ;; Since we check the original file, we can only use this syntax checker if
  ;; the buffer is actually linked to a file, and if it is not modified.
  :predicate flycheck-buffer-saved-p)

(defun flycheck-python-run-snippet (checker snippet)
  "Run a python SNIPPET and return the output.

CHECKER's executable is assumed to be a Python REPL."
  (-when-let (output (flycheck-call-checker-process-for-output
                      checker nil nil "-c" snippet))
    (string-trim output)))

(defun flycheck-python-get-path (checker)
  "Compute the current Python path (CHECKER is a Python REPL) ."
  (flycheck-python-run-snippet checker "import sys; print(sys.path[1:])"))

(defun flycheck-python-find-module (checker module)
  "Check if a Python MODULE is available (CHECKER is a Python REPL)."
  (flycheck-python-run-snippet
   checker (concat "import sys; sys.path.pop(0);"
                   (format "import %s; print(%s.__file__)" module module))))

(defun flycheck-python-needs-module-p (checker)
  "Determine whether CHECKER needs to be invoked through Python.

Previous versions of Flycheck called pylint and flake8 directly,
while new version call them through `python -c'.  This check
ensures that we don't break existing code; it also allows people
who use virtualenvs to run globally-installed checkers."
  (not (string-match-p (rx (or "pylint" "pylint3" "flake8")
                           (or "-script.pyw" ".exe" ".bat" "")
                           eos)
                       (flycheck-checker-executable checker))))

(defun flycheck-python-verify-module (checker module)
  "Verify that a Python MODULE is available.

Return nil if CHECKER's executable is not a Python REPL.  This
function's is suitable for a checker's :verify."
  (when (flycheck-python-needs-module-p checker)
    (let ((mod-path (flycheck-python-find-module checker module)))
      (list (flycheck-verification-result-new
             :label (format "`%s' module" module)
             :message (if mod-path (format "Found at %S" mod-path)
                        (format "Missing; sys.path is %s"
                                (flycheck-python-get-path checker)))
             :face (if mod-path 'success '(bold error)))))))

(defun flycheck-python-module-args (checker module-name)
  "Compute arguments to pass to CHECKER's executable to run MODULE-NAME.

Return nil if CHECKER's executable is not a Python REPL.
Otherwise, return a list starting with -c (-m is not enough
because it adds the current directory to Python's path)."
  (when (flycheck-python-needs-module-p checker)
    `("-c" ,(concat "import sys;sys.path.pop(0);import runpy;"
                    (format "runpy.run_module(%S, run_name='__main__')" module-name )))))

(defcustom flycheck-python-project-files
  '("pyproject.toml" "setup.cfg" "mypy.ini" "pyrightconfig.json")
  "Files used to find where to run Python checkers from.
Currently used for pylint, flake8, and pyright.

The presence of one in these files indicates the root of the
current project; `.pylintrc' is not part of the list because it
is commonly found in ~/."
  :group 'flycheck
  :type '(repeat (string :tag "File name"))
  :package-version '(flycheck . "0.33")
  :safe #'flycheck-string-list-p)

(defun flycheck-python-find-project-root (_checker)
  "Find the root directory of a Python project.

The root directory is assumed to be the nearest parent directory
that contains one of `flycheck-python-project-files'.  If no such
file is found, we use the same heuristic as epylint: the nearest
parent directory that doesn't have a __init__.py file."
  (let ((start (if buffer-file-name
                   (file-name-directory buffer-file-name)
                 default-directory)))
    (or (flycheck--locate-dominating-file-matching
         start (regexp-opt flycheck-python-project-files))
        (locate-dominating-file
         start (lambda (dir)
                 (not (file-exists-p (expand-file-name "__init__.py" dir))))))))

(flycheck-def-config-file-var flycheck-flake8rc python-flake8
                              '(".flake8" "setup.cfg" "tox.ini"))

(flycheck-def-option-var flycheck-flake8-error-level-alist
    '(("^E9.*$"  . error)               ; Syntax errors from pep8
      ("^F82.*$" . error)               ; undefined variables from pyflakes
      ("^F83.*$" . error)               ; Duplicate arguments from flake8
      ("^D.*$"   . info)                ; Docstring issues from flake8-pep257
      ("^N.*$"   . info)                ; Naming issues from pep8-naming
      )
    python-flake8
  "An alist mapping flake8 error IDs to Flycheck error levels.

Each item in this list is a cons cell `(PATTERN . LEVEL)' where
PATTERN is a regular expression matched against the error ID, and
LEVEL is a Flycheck error level symbol.

Each PATTERN is matched in the order of appearance in this list
against the error ID.  If it matches the ID, the level of the
corresponding error is set to LEVEL.  An error that is not
matched by any PATTERN defaults to warning level.

The default value of this option matches errors from flake8
itself and from the following flake8 plugins:

- pep8-naming
- flake8-pep257

You may add your own mappings to this option in order to support
further flake8 plugins."
  :type '(repeat (cons (regexp :tag "Error ID pattern")
                       (symbol :tag "Error level")))
  :package-version '(flycheck . "0.22"))

(flycheck-def-option-var flycheck-flake8-maximum-complexity nil python-flake8
  "The maximum McCabe complexity of methods.

If nil, do not check the complexity of methods.  If set to an
integer, report any complexity greater than the value of this
variable as warning.

If set to an integer, this variable overrules any similar setting
in the configuration file denoted by `flycheck-flake8rc'."
  :type '(choice (const :tag "Do not check McCabe complexity" nil)
                 (integer :tag "Maximum complexity"))
  :safe #'integerp)

(flycheck-def-option-var flycheck-flake8-maximum-line-length nil python-flake8
  "The maximum length of lines.

If set to an integer, the value of this variable denotes the
maximum length of lines, overruling any similar setting in the
configuration file denoted by `flycheck-flake8rc'.  An error will
be reported for any line longer than the value of this variable.

If set to nil, use the maximum line length from the configuration
file denoted by `flycheck-flake8rc', or the PEP 8 recommendation
of 79 characters if there is no configuration with this setting."
  :type '(choice (const :tag "Default value")
                 (integer :tag "Maximum line length in characters"))
  :safe #'integerp)

(defun flycheck-flake8-fix-error-level (err)
  "Fix the error level of ERR.

Update the error level of ERR according to
`flycheck-flake8-error-level-alist'."
  (pcase-dolist (`(,pattern . ,level) flycheck-flake8-error-level-alist)
    (when (string-match-p pattern (flycheck-error-id err))
      (setf (flycheck-error-level err) level)))
  err)

(defun flycheck-flake8--find-project-root (_checker)
  "Find setup.cfg in a parent directory of the current buffer."
  ;; This is a workaround for `https://gitlab.com/pycqa/flake8/issues/517'; see
  ;; also `https://github.com/flycheck/flycheck/issues/1722'
  (locate-dominating-file (or buffer-file-name default-directory) "setup.cfg"))

(flycheck-define-checker python-flake8
  "A Python syntax and style checker using Flake8.

Requires Flake8 3.0 or newer. See URL
`https://flake8.readthedocs.io/'."
  ;; Not calling flake8 directly makes it easier to switch between different
  ;; Python versions; see https://github.com/flycheck/flycheck/issues/1055.
  :command ("python3"
            (eval (flycheck-python-module-args 'python-flake8 "flake8"))
            "--format=default"
            (config-file "--append-config" flycheck-flake8rc)
            (option "--max-complexity" flycheck-flake8-maximum-complexity nil
                    flycheck-option-int)
            (option "--max-line-length" flycheck-flake8-maximum-line-length nil
                    flycheck-option-int)
            (eval (when buffer-file-name
                    (concat "--stdin-display-name=" buffer-file-name)))
            "-")
  :standard-input t
  :working-directory flycheck-python-find-project-root
  :error-filter (lambda (errors)
                  (let ((errors (flycheck-sanitize-errors errors)))
                    (seq-map #'flycheck-flake8-fix-error-level errors)))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" (optional column ":") " "
            (id (one-or-more (any alpha)) (one-or-more digit)) " "
            (message (one-or-more not-newline))
            line-end))
  :enabled (lambda ()
             (or (not (flycheck-python-needs-module-p 'python-flake8))
                 (flycheck-python-find-module 'python-flake8 "flake8")))
  :verify (lambda (_) (flycheck-python-verify-module 'python-flake8 "flake8"))
  :modes (python-mode python-ts-mode)
  :next-checkers ((warning . python-pylint)
                  (warning . python-mypy)))

(flycheck-def-config-file-var
    flycheck-pylintrc python-pylint
    '("pylintrc" ".pylintrc" "pyproject.toml" "setup.cfg"))

(flycheck-def-option-var flycheck-pylint-use-symbolic-id t python-pylint
  "Whether to use pylint message symbols or message codes.

A pylint message has both an opaque identifying code (such as `F0401') and a
more meaningful symbolic code (such as `import-error').  This option governs
which should be used and reported to the user."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.25"))

(defun flycheck-parse-pylint (output checker buffer)
  "Parse JSON OUTPUT of CHECKER on BUFFER as Pylint errors."
  (mapcar (lambda (err)
            (let-alist err
              ;; Pylint can return -1 as a line or a column, hence the call to
              ;; `max'.  See `https://github.com/flycheck/flycheck/issues/1383'.
              (flycheck-error-new-at
               (and .line (max .line 1))
               (and .column (max (1+ .column) 1))
               (pcase .type
                 ;; See "pylint/utils.py"
                 ((or "fatal" "error") 'error)
                 ((or "info" "convention") 'info)
                 ((or "warning" "refactor" _) 'warning))
               ;; Drop lines showing the error in context
               (and (string-match (rx (*? nonl) eol) .message)
                    (match-string 0 .message))
               :id (if flycheck-pylint-use-symbolic-id .symbol .message-id)
               :checker checker
               :buffer buffer
               :filename .path)))
          (car (flycheck-parse-json output))))

(flycheck-define-checker python-pylint
  "A Python syntax and style checker using Pylint.

This syntax checker requires Pylint 1.0 or newer.

See URL `https://www.pylint.org/'."
  ;; --reports=n disables the scoring report.
  ;; Not calling pylint directly makes it easier to switch between different
  ;; Python versions; see https://github.com/flycheck/flycheck/issues/1055.
  :command ("python3"
            (eval (flycheck-python-module-args 'python-pylint "pylint"))
            "--reports=n"
            "--output-format=json"
            (config-file "--rcfile=" flycheck-pylintrc concat)
            ;; Need `source-inplace' for relative imports (e.g. `from .foo
            ;; import bar'), see https://github.com/flycheck/flycheck/issues/280
            source-inplace)
  :error-parser flycheck-parse-pylint
  :working-directory flycheck-python-find-project-root
  :enabled (lambda ()
             (or (not (flycheck-python-needs-module-p 'python-pylint))
                 (flycheck-python-find-module 'python-pylint "pylint")))
  :verify (lambda (_) (flycheck-python-verify-module 'python-pylint "pylint"))
  :error-explainer (lambda (err)
                     (-when-let (id (flycheck-error-id err))
                       (apply
                        #'flycheck-call-checker-process-for-output
                        'python-pylint nil t
                        (append
                         (flycheck-python-module-args 'python-pylint "pylint")
                         (list (format "--help-msg=%s" id))))))
  :modes (python-mode python-ts-mode)
  :next-checkers ((warning . python-mypy)))

(flycheck-define-checker python-pycompile
  "A Python syntax checker using Python's builtin compiler.

See URL `https://docs.python.org/3.4/library/py_compile.html'."
  :command ("python3" "-m" "py_compile" source)
  :error-patterns
  ;; Python 2.7
  ((error line-start "  File \"" (file-name) "\", line " line "\n"
          (>= 2 (zero-or-more not-newline) "\n")
          "SyntaxError: " (message) line-end)
   (error line-start "Sorry: IndentationError: "
          (message) "(" (file-name) ", line " line ")"
          line-end)
   ;; 2.6
   (error line-start "SyntaxError: ('" (message (one-or-more (not (any "'"))))
          "', ('" (file-name (one-or-more (not (any "'")))) "', "
          line ", " column ", " (one-or-more not-newline) line-end))
  :working-directory flycheck-python-find-project-root
  :modes (python-mode python-ts-mode)
  :next-checkers ((warning . python-mypy)))

(defun flycheck-pyright--parse-error (output checker buffer)
  "Parse pyright errors/warnings from JSON OUTPUT.
CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively."
  (seq-map
   (lambda (err)
     (let-alist err
       (flycheck-error-new-at
        (+ 1 .range.start.line)
        (+ 1 .range.start.character)
        (pcase .severity
          ("error" 'error)
          ("warning" 'warning)
          (_ 'warning))
        .message
        :end-line (+ 1 .range.end.line)
        :end-column (+ 1 .range.end.character)
        :checker checker
        :buffer buffer
        :filename (buffer-file-name buffer))))
   (cdr (nth 2 (car (flycheck-parse-json output))))))

(flycheck-define-checker python-pyright
  "Static type checker for Python

See URL https://github.com/microsoft/pyright."
  :command ("pyright"
            "--outputjson"
            source-inplace)
  :working-directory flycheck-python-find-project-root
  :error-parser flycheck-pyright--parse-error
  :modes (python-mode python-ts-mode))

(define-obsolete-variable-alias 'flycheck-python-mypy-ini
  'flycheck-python-mypy-config "32")

(flycheck-def-config-file-var flycheck-python-mypy-config python-mypy
                              '("mypy.ini" "pyproject.toml" "setup.cfg"))

(flycheck-def-option-var flycheck-python-mypy-cache-dir nil python-mypy
  "Directory used to write .mypy_cache directories."
  :type '(choice
          (const :tag "Write to the working directory" nil)
          (const :tag "Never write .mypy_cache directories" null-device)
          (string :tag "Path"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "32"))

(flycheck-def-option-var flycheck-python-mypy-python-executable nil python-mypy
  "Python executable to find the installed PEP 561 packages."
  :type '(choice (const :tag "Same as mypy's" nil)
                 (string :tag "Path"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "33"))

(flycheck-define-checker python-mypy
  "Mypy syntax and type checker.  Requires mypy>=0.730.

See URL `http://mypy-lang.org/'."
  :command ("mypy"
            "--show-column-numbers"
            "--no-pretty"
            (config-file "--config-file" flycheck-python-mypy-config)
            (option "--cache-dir" flycheck-python-mypy-cache-dir)
            (option "--python-executable" flycheck-python-mypy-python-executable)
            source-original)
  :error-patterns
  ((error line-start (file-name) ":" line (optional ":" column)
          ": error:" (message) line-end)
   (warning line-start (file-name) ":" line (optional ":" column)
            ": warning:" (message) line-end)
   (info line-start (file-name) ":" line (optional ":" column)
         ": note:" (message) line-end))
  :working-directory flycheck-python-find-project-root
  :modes (python-mode python-ts-mode)
  ;; Ensure the file is saved, to work around
  ;; https://github.com/python/mypy/issues/4746.
  :predicate flycheck-buffer-saved-p)

(flycheck-def-option-var flycheck-lintr-caching t r-lintr
  "Whether to enable caching in lintr.

By default, lintr caches all expressions in a file and re-checks
only those that have changed.  Setting this option to nil
disables caching in case there are problems."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.23"))

(flycheck-def-option-var flycheck-lintr-linters "default_linters" r-lintr
  "Linters to use with lintr.

The value of this variable is a string containing an R
expression, which selects linters for lintr."
  :type 'string
  :risky t
  :package-version '(flycheck . "0.23"))

(defun flycheck-r-has-lintr (checker)
  "Whether CHECKER (R) has installed the `lintr' library."
  (eql 0 (flycheck-call-checker-process
          checker nil nil nil
          "--slave" "--no-restore" "--no-save" "-e"
          "library('lintr')")))

(flycheck-define-checker r-lintr
  "An R style and syntax checker using the lintr package.

See URL `https://github.com/jimhester/lintr'."
  :command ("R" "--slave" "--no-restore" "--no-save" "-e"
            (eval (concat
                   "library(lintr);"
                   "try(lint(commandArgs(TRUE)"
                   ", cache=" (if flycheck-lintr-caching "TRUE" "FALSE")
                   ", " flycheck-lintr-linters
                   "))"))
            "--args" source)
  :error-patterns
  ((info line-start (file-name) ":" line ":" column ": style: " (message)
         line-end)
   (warning line-start (file-name) ":" line ":" column ": warning: " (message)
            line-end)
   (error line-start (file-name) ":" line ":" column ": error: " (message)
          line-end))
  :modes (ess-mode ess-r-mode)
  :predicate
  ;; Don't check ESS files which do not contain R, and make sure that lintr is
  ;; actually available
  (lambda ()
    (and (equal ess-language "S")
         (flycheck-r-has-lintr 'r-lintr)))
  :verify (lambda (checker)
            (let ((has-lintr (flycheck-r-has-lintr checker)))
              (list
               (flycheck-verification-result-new
                :label "lintr library"
                :message (if has-lintr "present" "missing")
                :face (if has-lintr 'success '(bold error)))))))

(defun flycheck-racket-has-expand-p (checker)
  "Whether the executable of CHECKER provides the `expand' command."
  (eql 0 (flycheck-call-checker-process checker nil nil nil "expand")))

(flycheck-define-checker racket
  "A Racket syntax checker with `raco expand'.

The `compiler-lib' racket package is required for this syntax
checker.

See URL `https://racket-lang.org/'."
  :command ("raco" "expand" source-inplace)
  :predicate
  (lambda ()
    (and (or (not (eq major-mode 'scheme-mode))
             ;; In `scheme-mode' we must check the current Scheme implementation
             ;; being used
             (and (boundp 'geiser-impl--implementation)
                  (eq geiser-impl--implementation 'racket)))
         (flycheck-racket-has-expand-p 'racket)))
  :verify
  (lambda (checker)
    (let ((has-expand (flycheck-racket-has-expand-p checker))
          (in-scheme-mode (eq major-mode 'scheme-mode))
          (geiser-impl (bound-and-true-p geiser-impl--implementation)))
      (list
       (flycheck-verification-result-new
        :label "compiler-lib package"
        :message (if has-expand "present" "missing")
        :face (if has-expand 'success '(bold error)))
       (flycheck-verification-result-new
        :label "Geiser Implementation"
        :message (cond
                  ((not in-scheme-mode) "Using Racket Mode")
                  ((eq geiser-impl 'racket) "Racket")
                  (geiser-impl (format "Other: %s" geiser-impl))
                  (t "Geiser not active"))
        :face (cond
               ((or (not in-scheme-mode) (eq geiser-impl 'racket)) 'success)
               (t '(bold error)))))))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors
     (flycheck-increment-error-columns
      (seq-remove
       (lambda (err)
         (string-suffix-p
          "/share/racket/pkgs/compiler-lib/compiler/commands/expand.rkt"
          (flycheck-error-filename err)))
       errors))))
  :error-patterns
  ((error line-start (zero-or-more space)
          (file-name) ":" line ":" column ":" (message) line-end))
  :modes (racket-mode scheme-mode))

(flycheck-define-checker rpm-rpmlint
  "A RPM SPEC file syntax checker using rpmlint.

See URL `https://github.com/rpm-software-management/rpmlint'."
  :command ("rpmlint" source)
  :error-patterns
  ((error line-start
          (file-name) ":" (optional line ":") " E: " (message)
          line-end)
   (warning line-start
            (file-name) ":" (optional line ":") " W: " (message)
            line-end))
  :error-filter
  ;; rpmlint 1.1 outputs a spurious error for the temp file created by flycheck
  (lambda (errors)
    (dolist (err (seq-remove
                  (lambda (err)
                    (string-suffix-p "(none)" (flycheck-error-filename err)))
                  errors))
      ;; Add fake line numbers if they are missing in the lint output
      (unless (flycheck-error-line err)
        (setf (flycheck-error-line err) 1)))
    errors)
  :error-explainer
  (lambda (error)
    (-when-let* ((error-message (flycheck-error-message error))
                 (message-id (save-match-data
                               (string-match "\\([^ ]+\\)" error-message)
                               (match-string 1 error-message))))
      (flycheck-call-checker-process-for-output
       'rpm-rpmlint nil t "-I" message-id)))
  :modes (sh-mode rpm-spec-mode)
  :predicate (lambda () (or (not (eq major-mode 'sh-mode))
                            ;; In `sh-mode', we need the proper shell
                            (eq sh-shell 'rpm))))

(flycheck-def-config-file-var flycheck-markdown-markdownlint-cli-config
    markdown-markdownlint-cli
    '(".markdownlint.json" ".markdownlint.jsonc" ".markdownlint.yaml")
  :package-version '(flycheck . "33"))

(flycheck-def-option-var flycheck-markdown-markdownlint-cli-disable-rules
    nil markdown-markdownlint-cli
  "Rules to disable for markdownlint-cli."
  :type '(repeat :tag "Disabled rule"
                 (string :tag "Rule name"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "33"))

(flycheck-def-option-var flycheck-markdown-markdownlint-cli-enable-rules
    nil markdown-markdownlint-cli
  "Rules to enable for markdownlint-cli."
  :type '(repeat :tag "Enabled rule"
                 (string :tag "Rule name"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "33"))

(flycheck-define-checker markdown-markdownlint-cli
  "Markdown checker using markdownlint-cli.

See URL `https://github.com/DavidAnson/markdownlint-cli'."
  :command ("markdownlint"
            (config-file "--config" flycheck-markdown-markdownlint-cli-config)
            (option-list "--disable" flycheck-markdown-markdownlint-cli-disable-rules)
            (option-list "--enable" flycheck-markdown-markdownlint-cli-enable-rules)
            "--"
            source)
  :error-patterns
  ((error line-start
          (file-name) ":" line
          (? ":" column) " " (id (one-or-more (not (any space))))
          " " (message) line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors
     (flycheck-remove-error-file-names "(string)" errors)))
  :modes (markdown-mode gfm-mode))

(flycheck-def-option-var flycheck-markdown-mdl-rules nil markdown-mdl
  "Rules to enable for mdl.

The value of this variable is a list of strings each of which is
the name of a rule to enable.

By default all rules are enabled.

See URL `https://git.io/vhi2t'."
  :type '(repeat :tag "Enabled rules"
                 (string :tag "rule name"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "27"))

(flycheck-def-option-var flycheck-markdown-mdl-tags nil markdown-mdl
  "Rule tags to enable for mdl.

The value of this variable is a list of strings each of which is
the name of a rule tag.  Only rules with these tags are enabled.

By default all rules are enabled.

See URL `https://git.io/vhi2t'."
  :type '(repeat :tag "Enabled tags"
                 (string :tag "tag name"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "27"))

(flycheck-def-config-file-var flycheck-markdown-mdl-style markdown-mdl nil
  :package-version '(flycheck . "27"))

(flycheck-define-checker markdown-mdl
  "Markdown checker using mdl.

See URL `https://github.com/markdownlint/markdownlint'."
  :command ("mdl"
            (config-file "--style" flycheck-markdown-mdl-style)
            (option "--tags=" flycheck-markdown-mdl-tags concat
                    flycheck-option-comma-separated-list)
            (option "--rules=" flycheck-markdown-mdl-rules concat
                    flycheck-option-comma-separated-list))
  :standard-input t
  :error-patterns
  ((error line-start
          (file-name) ":" line ": " (id (one-or-more alnum)) " " (message)
          line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors
     (flycheck-remove-error-file-names "(stdin)" errors)))
  :modes (markdown-mode gfm-mode))

(flycheck-define-checker nix
  "Nix checker using nix-instantiate.

See URL `https://nixos.org/nix/manual/#sec-nix-instantiate'."
  :command ("nix-instantiate" "--parse" "-")
  :standard-input t
  :error-patterns
  ((error line-start
          "error: " (message)
          (one-or-more "\n")
          (zero-or-more space) "at «stdin»:" line ":" column ":" line-end)
   (error line-start
          "at: (" line ":" column ") from stdin"
          (one-or-more "\n" (zero-or-more space (one-or-more not-newline)))
          (message) line-end)
   (error line-start
          "error: " (message) " at " (file-name) ":" line ":" column
          line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors
     (flycheck-remove-error-file-names "(string)" errors)))
  :next-checkers ((warning . nix-linter))
  :modes nix-mode)

(defun flycheck-parse-nix-linter (output checker buffer)
  "Parse nix-linter warnings from JSON OUTPUT.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://github.com/Synthetica9/nix-linter' for more
information about nix-linter."
  (mapcar (lambda (err)
            (let-alist err
              (flycheck-error-new-at
               .pos.spanBegin.sourceLine
               .pos.spanBegin.sourceColumn
               'warning
               .description
               :id .offense
               :checker checker
               :buffer buffer
               :filename (buffer-file-name buffer)
               :end-line .pos.spanEnd.sourceLine
               :end-column .pos.spanEnd.sourceColumn)))
          (flycheck-parse-json output)))

(flycheck-define-checker nix-linter
  "Nix checker using nix-linter.

See URL `https://github.com/Synthetica9/nix-linter'."
  :command ("nix-linter" "--json-stream" "-")
  :standard-input t
  :error-parser flycheck-parse-nix-linter
  :error-explainer
  (lambda (error)
    (-when-let (error-code (flycheck-error-id error))
      (flycheck-call-checker-process-for-output
       'nix-linter nil t "--help-for" error-code)))
  :modes nix-mode)

(defun flycheck-locate-sphinx-source-directory ()
  "Locate the Sphinx source directory for the current buffer.

Return the source directory, or nil, if the current buffer is not
part of a Sphinx project."
  (-when-let* ((filename (buffer-file-name))
               (dir (locate-dominating-file filename "conf.py")))
    (expand-file-name dir)))

(flycheck-define-checker rst
  "A ReStructuredText (RST) syntax checker using Docutils.

See URL `http://docutils.sourceforge.net/'."
  ;; include:: directives
  :command ("rst2pseudoxml.py" "--report=2" "--halt=5"
            ;; Read from standard input and throw output away
            "-" null-device)
  :standard-input t
  :error-patterns
  ((warning line-start "<stdin>:" line ": (WARNING/2) " (message) line-end)
   (error line-start "<stdin>:" line
          ": (" (or "ERROR/3" "SEVERE/4") ") "
          (message) line-end))
  :modes rst-mode)

(flycheck-def-option-var flycheck-sphinx-warn-on-missing-references t rst-sphinx
  "Whether to warn about missing references in Sphinx.

When non-nil (the default), warn about all missing references in
Sphinx via `-n'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.17"))

(flycheck-define-checker rst-sphinx
  "A ReStructuredText (RST) syntax checker using Sphinx.

Requires Sphinx 1.2 or newer.  See URL `http://sphinx-doc.org'."
  :command ("sphinx-build" "-b" "pseudoxml"
            "-q" "-N"                   ; Reduced output and no colors
            (option-flag "-n" flycheck-sphinx-warn-on-missing-references)
            (eval (flycheck-locate-sphinx-source-directory))
            temporary-directory         ; Redirect the output to a temporary
                                        ; directory
            source-original)            ; Sphinx needs the original document
  :error-patterns
  ((warning line-start (file-name) ":" line ": WARNING: " (message) line-end)
   (error line-start
          (file-name) ":" line
          ": " (or "ERROR" "SEVERE") ": "
          (message) line-end))
  :modes rst-mode
  :predicate (lambda () (and (flycheck-buffer-saved-p)
                             (flycheck-locate-sphinx-source-directory))))

(defun flycheck-ruby--find-project-root (_checker)
  "Compute an appropriate working-directory for flycheck-ruby.

This is either a parent directory containing a Gemfile, or nil."
  (and
   buffer-file-name
   (locate-dominating-file buffer-file-name "Gemfile")))

(flycheck-def-config-file-var flycheck-rubocoprc ruby-rubocop ".rubocop.yml")

(flycheck-def-option-var flycheck-rubocop-lint-only nil
                         (ruby-rubocop ruby-standard)
  "Whether to only report code issues in Rubocop and Standard.

When non-nil, only report code issues, via `--lint'.  Otherwise
report style issues as well."
  :safe #'booleanp
  :type 'boolean
  :package-version '(flycheck . "0.16"))

(defconst flycheck-ruby-rubocop-error-patterns
  '((info line-start (file-name) ":" line ":" column ": C: "
          (optional (id (one-or-more (not (any ":")))) ": ") (message) line-end)
    (warning line-start (file-name) ":" line ":" column ": W: "
             (optional (id (one-or-more (not (any ":")))) ": ") (message)
             line-end)
    (error line-start (file-name) ":" line ":" column ": " (or "E" "F") ": "
           (optional (id (one-or-more (not (any ":")))) ": ") (message)
           line-end)))

(flycheck-def-executable-var ruby-rubocop "rubocop")
(flycheck-define-command-checker 'ruby-rubocop
  "A Ruby syntax and style checker using the RuboCop tool.

You need at least RuboCop 0.34 for this syntax checker.

See URL `https://rubocop.org/'."
  ;; ruby-standard is defined based on this checker
  :command '("rubocop"
             "--display-cop-names"
             "--force-exclusion"
             "--format" "emacs"
             ;; Explicitly disable caching to prevent Rubocop 0.35.1 and earlier
             ;; from caching standard input.  Later versions of Rubocop
             ;; automatically disable caching with --stdin, see
             ;; https://github.com/flycheck/flycheck/issues/844 and
             ;; https://github.com/bbatsov/rubocop/issues/2576
             "--cache" "false"
             (config-file "--config" flycheck-rubocoprc)
             (option-flag "--lint" flycheck-rubocop-lint-only)
             ;; Rubocop takes the original file name as argument when reading
             ;; from standard input
             "--stdin" source-original)
  :standard-input t
  :working-directory #'flycheck-ruby--find-project-root
  :error-patterns flycheck-ruby-rubocop-error-patterns
  :modes '(enh-ruby-mode ruby-mode ruby-ts-mode)
  :next-checkers '((warning . ruby-reek)
                   (warning . ruby-rubylint)))

(flycheck-def-config-file-var flycheck-ruby-standardrc ruby-standard
                              ".standard.yml")

(flycheck-def-executable-var ruby-standard "standardrb")
(flycheck-define-command-checker 'ruby-standard
  "A Ruby syntax and style checker using the StandardRB gem.

See URL `https://github.com/testdouble/standard' for more information."
  ;; This checker is derived from ruby-rubocop; see above
  :command '("standardrb"
             "--display-cop-names"
             "--force-exclusion"
             "--format" "emacs"
             "--cache" "false"
             (config-file "--config" flycheck-ruby-standardrc)
             (option-flag "--lint" flycheck-rubocop-lint-only)
             "--stdin" source-original)
  :standard-input t
  :working-directory #'flycheck-ruby--find-project-root
  :error-patterns flycheck-ruby-rubocop-error-patterns
  :modes '(enh-ruby-mode ruby-mode ruby-ts-mode)
  :next-checkers '((warning . ruby-reek)
                   (warning . ruby-rubylint)))

(flycheck-def-config-file-var flycheck-reekrc ruby-reek ".reek.yml"
  :safe #'string-or-null-p
  :package-version '(flycheck . "30"))

(flycheck-define-checker ruby-reek
  "A Ruby smell checker using reek.

See URL `https://github.com/troessner/reek'."
  :command ("reek" "--format" "json"
            (config-file "--config" flycheck-reekrc)
            source)
  :error-parser flycheck-parse-reek
  :modes (enh-ruby-mode ruby-mode ruby-ts-mode)
  :next-checkers ((warning . ruby-rubylint)))

;; Default to `nil' to let Rubylint find its configuration file by itself, and
;; to maintain backwards compatibility with older Rubylint and Flycheck releases
(flycheck-def-config-file-var flycheck-rubylintrc ruby-rubylint nil)

(flycheck-define-checker ruby-rubylint
  "A Ruby syntax and code analysis checker using ruby-lint.

Requires ruby-lint 2.0.2 or newer.  See URL
`https://github.com/YorickPeterse/ruby-lint'."
  :command ("ruby-lint" "--presenter=syntastic"
            (config-file "--config" flycheck-rubylintrc)
            source)
  ;; Ruby Lint can't read from standard input
  :error-patterns
  ((info line-start
         (file-name) ":I:" line ":" column ": " (message) line-end)
   (warning line-start
            (file-name) ":W:" line ":" column ": " (message) line-end)
   (error line-start
          (file-name) ":E:" line ":" column ": " (message) line-end))
  :modes (enh-ruby-mode ruby-mode ruby-ts-mode))

(flycheck-define-checker ruby
  "A Ruby syntax checker using the standard Ruby interpreter.

Please note that the output of different Ruby versions and
implementations varies wildly.  This syntax checker supports
current versions of MRI and JRuby, but may break when used with
other implementations or future versions of these
implementations.

Please consider using `ruby-rubocop' or `ruby-reek' instead.

See URL `https://www.ruby-lang.org/'."
  :command ("ruby" "-w" "-c")
  :standard-input t
  :error-patterns
  ;; These patterns support output from JRuby, too, to deal with RVM or Rbenv
  ((error line-start "SyntaxError in -:" line ": " (message) line-end)
   (warning line-start "-:" line ":" (optional column ":")
            " warning: " (message) line-end)
   (error line-start "-:" line ": " (message) line-end))
  :modes (enh-ruby-mode ruby-mode ruby-ts-mode)
  :next-checkers ((warning . ruby-rubylint)))

(flycheck-define-checker ruby-jruby
  "A Ruby syntax checker using the JRuby interpreter.

This syntax checker is very primitive, and may break on future
versions of JRuby.

Please consider using `ruby-rubocop' or `ruby-rubylint' instead.

See URL `http://jruby.org/'."
  :command ("jruby" "-w" "-c")
  :standard-input t
  :error-patterns
  ((error   line-start "SyntaxError in -:" line ": " (message) line-end)
   (warning line-start "-:" line ": warning: " (message) line-end)
   (error   line-start "-:" line ": "          (message) line-end))
  :modes (enh-ruby-mode ruby-mode ruby-ts-mode)
  :next-checkers ((warning . ruby-rubylint)))

(flycheck-def-args-var flycheck-cargo-check-args (rust-cargo)
  :package-version '(flycheck . "32"))

(flycheck-def-args-var flycheck-rust-args (rust)
  :package-version '(flycheck . "0.24"))

(flycheck-def-option-var flycheck-rust-check-tests t (rust-cargo rust)
  "Whether to check test code in Rust.

For the `rust' checker: When non-nil, `rustc' is passed the
`--test' flag, which will check any code marked with the
`#[cfg(test)]' attribute and any functions marked with
`#[test]'. Otherwise, `rustc' is not passed `--test' and test
code will not be checked.  Skipping `--test' is necessary when
using `#![no_std]', because compiling the test runner requires
`std'.

For the `rust-cargo' checker: When non-nil, calls `cargo test
--no-run' instead of `cargo check'."
  :type 'boolean
  :safe #'booleanp
  :package-version '("flycheck" . "0.19"))

(flycheck-def-option-var flycheck-rust-crate-root nil rust
  "A path to the crate root for the current buffer.

The value of this variable is either a string with the path to
the crate root for the current buffer, or nil if the current buffer
is a crate.  A relative path is relative to the current buffer.

If this variable is non nil the current buffer will only be checked
if it is not modified, i.e. after it has been saved."
  :type '(choice (const :tag "Unspecified" nil)
                 (file :tag "Root"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "0.20"))
(make-variable-buffer-local 'flycheck-rust-crate-root)

(flycheck-def-option-var flycheck-rust-crate-type "lib" (rust-cargo rust)
  "The type of the Rust Crate to check.

For `rust-cargo', the value should be a string denoting the
target type passed to Cargo.  See
`flycheck-rust-valid-crate-type-p' for the list of allowed
values.

For `rust', the value should be a string denoting the crate type
for the `--crate-type' flag of rustc."
  :type '(choice (const :tag "nil (rust/rust-cargo)" nil)
                 (const :tag "lib (rust/rust-cargo)" "lib")
                 (const :tag "bin (rust/rust-cargo)" "bin")
                 (const :tag "example (rust-cargo)" "example")
                 (const :tag "test (rust-cargo)" "test")
                 (const :tag "bench (rust-cargo)" "bench")
                 (const :tag "rlib (rust)" "rlib")
                 (const :tag "dylib (rust)" "dylib")
                 (const :tag "cdylib (rust)" "cdylib")
                 (const :tag "staticlib (rust)" "staticlib")
                 (const :tag "metadata (rust)" "metadata"))
  :safe #'stringp
  :package-version '(flycheck . "0.20"))
(make-variable-buffer-local 'flycheck-rust-crate-type)

(flycheck-def-option-var flycheck-rust-binary-name nil rust-cargo
  "The name of the binary to pass to `cargo check --CRATE-TYPE'.

The value of this variable is a string denoting the name of the
target to check: usually the name of the crate, or the name of
one of the files under `src/bin', `tests', `examples' or
`benches'.

This always requires a non-nil value, unless
`flycheck-rust-crate-type' is `lib' or nil, in which case it is
ignored."
  :type '(choice (const :tag "Unspecified" nil)
                 (string :tag "Binary name"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "28"))
(make-variable-buffer-local 'flycheck-rust-binary-name)

(flycheck-def-option-var flycheck-rust-features nil rust-cargo
  "List of features to activate during build or check.

The value of this variable is a list of strings denoting features
that will be activated to build the target to check. Features will
be passed to `cargo check --features=FEATURES'."
  :type '(repeat :tag "Features to activate"
                 (string :tag "Feature"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "32"))
(make-variable-buffer-local 'flycheck-rust-features)

(flycheck-def-option-var flycheck-rust-library-path nil rust
  "A list of library directories for Rust.

The value of this variable is a list of strings, where each
string is a directory to add to the library path of Rust.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Library directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.18"))

(defun flycheck--fontify-as-markdown ()
  "Place current buffer in `markdown-view-mode' and fontify it."
  (when (fboundp 'markdown-view-mode)
    (let ((markdown-fontify-code-block-default-mode 'rust-mode)
          (markdown-fontify-code-blocks-natively t)
          (markdown-hide-markup t))
      (markdown-view-mode)
      (font-lock-flush)
      (font-lock-ensure))))

(defun flycheck-rust-error-explainer (error)
  "Return an explanation for the given `flycheck-error' ERROR."
  (-when-let (error-code (flycheck-error-id error))
    (lambda ()
      (flycheck-call-checker-process
       'rust nil standard-output t "--explain" error-code)
      (with-current-buffer standard-output
        (flycheck--fontify-as-markdown)))))

(defun flycheck-rust-error-filter (errors)
  "Filter ERRORS from rustc output that have no explanatory value."
  (seq-remove
   (lambda (err)
     (or
      ;; Macro errors emit a diagnostic in a phony file,
      ;; e.g. "<println macros>".
      (-when-let (filename (flycheck-error-filename err))
        (string-match-p (rx "macros>" line-end) filename))
      ;; Redundant message giving the number of failed errors
      (-when-let (msg (flycheck-error-message err))
        (string-match-p
         (rx
          (or (: "aborting due to " (optional (one-or-more num) " ")
                 "previous error")
              (: "For more information about this error, try `rustc --explain "
                 (one-or-more alnum) "`.")))
         msg))))
   errors))

(defun flycheck-rust-manifest-directory ()
  "Return the nearest directory holding the Cargo manifest.

Return the nearest directory containing the `Cargo.toml' manifest
file, starting from the current buffer and using
`locate-dominating-file'.  Return nil if there is no such file,
or if the current buffer has no file name."
  (and buffer-file-name
       (locate-dominating-file buffer-file-name "Cargo.toml")))

(defun flycheck-rust-cargo-metadata ()
  "Run `cargo metadata' and return the result as parsed JSON object."
  (car (flycheck-parse-json
        (flycheck-call-checker-process-for-output
         'rust-cargo nil t
         "metadata" "--no-deps" "--format-version" "1"))))

(defun flycheck-rust-cargo-workspace-root ()
  "Return the path to the workspace root of a Rust Cargo project.

Return nil if the workspace root does not exist (for Rust
versions inferior to 1.25)."
  (let-alist (flycheck-rust-cargo-metadata)
    .workspace_root))

(defun flycheck-rust-cargo-has-command-p (command)
  "Whether Cargo has COMMAND in its list of commands.

Execute `cargo --list' to find out whether COMMAND is present."
  (let ((cargo (funcall flycheck-executable-find "cargo")))
    (member command
      (mapcar (lambda (line)
                (replace-regexp-in-string "\\s-*\\(\\S-+\\).*\\'" "\\1" line))
              (ignore-errors (process-lines cargo "--list"))))))

(defun flycheck-rust-valid-crate-type-p (crate-type)
  "Whether CRATE-TYPE is a valid target type for Cargo.

A valid Cargo target type is one of `lib', `bin', `example',
`test' or `bench'."
  (member crate-type '(nil "lib" "bin" "example" "test" "bench")))

(flycheck-define-checker rust-cargo
  "A Rust syntax checker using Cargo.

This syntax checker requires Rust 1.17 or newer.  See URL
`https://www.rust-lang.org'."
  :command ("cargo"
            (eval (if flycheck-rust-check-tests
                      "test"
                    "check"))
            (eval (when flycheck-rust-check-tests
                    "--no-run"))
            (eval (when flycheck-rust-crate-type
                    (concat "--" flycheck-rust-crate-type)))
            ;; All crate targets except "lib" need a binary name
            (eval (when (and flycheck-rust-crate-type
                             (not (string= flycheck-rust-crate-type "lib")))
                    flycheck-rust-binary-name))
            (option "--features=" flycheck-rust-features concat
                    flycheck-option-comma-separated-list)
            (eval flycheck-cargo-check-args)
            "--message-format=json")
  :error-parser flycheck-parse-cargo-rustc
  :error-filter (lambda (errors)
                  ;; In Rust 1.25+, filenames are relative to the workspace
                  ;; root.
                  (let ((root (flycheck-rust-cargo-workspace-root)))
                    (seq-do (lambda (err)
                              ;; Some errors are crate level and do not have a
                              ;; filename
                              (when (flycheck-error-filename err)
                                (setf (flycheck-error-filename err)
                                      (expand-file-name
                                       (flycheck-error-filename err) root))))
                            (flycheck-rust-error-filter errors))))
  :error-explainer flycheck-rust-error-explainer
  :modes (rust-mode rust-ts-mode)
  :predicate flycheck-buffer-saved-p
  :enabled flycheck-rust-manifest-directory
  :working-directory (lambda (_) (flycheck-rust-manifest-directory))
  :verify
  (lambda (_)
    (and buffer-file-name
         (let* ((has-toml (flycheck-rust-manifest-directory))
                (valid-crate-type (flycheck-rust-valid-crate-type-p
                                   flycheck-rust-crate-type))
                (need-binary-name
                 (and flycheck-rust-crate-type
                      (not (string= flycheck-rust-crate-type "lib")))))
           (list
            (flycheck-verification-result-new
             :label "Cargo.toml"
             :message (if has-toml "Found" "Missing")
             :face (if has-toml 'success '(bold warning)))
            (flycheck-verification-result-new
             :label "Crate type"
             :message (if valid-crate-type
                          (format "%s" flycheck-rust-crate-type)
                        (format "%s (invalid, should be one of 'lib', 'bin', \
'test', 'example' or 'bench')"
                                flycheck-rust-crate-type))
             :face (if valid-crate-type 'success '(bold error)))
            (flycheck-verification-result-new
             :label "Binary name"
             :message (cond
                       ((not need-binary-name) "Not required")
                       ((not flycheck-rust-binary-name) "Required")
                       (t (format "%s" flycheck-rust-binary-name)))
             :face (cond
                    ((not need-binary-name) 'success)
                    ((not flycheck-rust-binary-name) '(bold error))
                    (t 'success))))))))

(flycheck-define-checker rust
  "A Rust syntax checker using Rust compiler.

This syntax checker needs Rust 1.18 or newer.  See URL
`https://www.rust-lang.org'."
  :command ("rustc"
            (option "--crate-type" flycheck-rust-crate-type)
            "--emit=mir" "-o" "/dev/null" ; avoid creating binaries
            "--error-format=json"
            (option-flag "--test" flycheck-rust-check-tests)
            (option-list "-L" flycheck-rust-library-path concat)
            (eval flycheck-rust-args)
            (eval (or flycheck-rust-crate-root
                      (flycheck-substitute-argument 'source-original 'rust))))
  :error-parser flycheck-parse-rustc
  :error-filter flycheck-rust-error-filter
  :error-explainer flycheck-rust-error-explainer
  :modes (rust-mode rust-ts-mode)
  :predicate flycheck-buffer-saved-p)

(flycheck-define-checker rust-clippy
  "A Rust syntax checker using clippy.

See URL `https://github.com/rust-lang-nursery/rust-clippy'."
  :command ("cargo" "clippy" "--message-format=json")
  :error-parser flycheck-parse-cargo-rustc
  :error-filter flycheck-rust-error-filter
  :error-explainer flycheck-rust-error-explainer
  :modes (rust-mode rust-ts-mode)
  :predicate flycheck-buffer-saved-p
  :enabled (lambda ()
             (and (flycheck-rust-cargo-has-command-p "clippy")
                  (flycheck-rust-manifest-directory)))
  :working-directory (lambda (_) (flycheck-rust-manifest-directory))
  :verify
  (lambda (_)
    (and buffer-file-name
         (let ((has-toml (flycheck-rust-manifest-directory))
               (has-clippy (flycheck-rust-cargo-has-command-p "clippy")))
           (list
            (flycheck-verification-result-new
             :label "Clippy"
             :message (if has-clippy "Found"
                        "Cannot find the `cargo clippy' command")
             :face (if has-clippy 'success '(bold warning)))
            (flycheck-verification-result-new
             :label "Cargo.toml"
             :message (if has-toml "Found" "Missing")
             :face (if has-toml 'success '(bold warning))))))))

(defvar flycheck-sass-scss-cache-directory nil
  "The cache directory for `sass' and `scss'.")

(defun flycheck-sass-scss-cache-location ()
  "Get the cache location for `sass' and `scss'.

If no cache directory exists yet, create one and return it.
Otherwise return the previously used cache directory."
  (setq flycheck-sass-scss-cache-directory
        (or flycheck-sass-scss-cache-directory
            (make-temp-file "flycheck-sass-scss-cache" 'directory))))

(flycheck-def-option-var flycheck-sass-compass nil sass
  "Whether to enable the Compass CSS framework.

When non-nil, enable the Compass CSS framework, via `--compass'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.16"))

(flycheck-define-checker sass
  "A Sass syntax checker using the Sass compiler.

See URL `http://sass-lang.com'."
  :command ("sass"
            "--cache-location" (eval (flycheck-sass-scss-cache-location))
            (option-flag "--compass" flycheck-sass-compass)
            "--check" "--stdin")
  :standard-input t
  :error-patterns
  ((error line-start
          (or "Syntax error: " "Error: ")
          (message (one-or-more not-newline)
                   (zero-or-more "\n"
                                 (one-or-more " ")
                                 (one-or-more not-newline)))
          (optional "\r") "\n" (one-or-more " ") "on line " line
          " of standard input"
          line-end)
   (warning line-start
            "WARNING: "
            (message (one-or-more not-newline)
                     (zero-or-more "\n"
                                   (one-or-more " ")
                                   (one-or-more not-newline)))
            (optional "\r") "\n" (one-or-more " ") "on line " line
            " of " (one-or-more not-newline)
            line-end))
  :modes sass-mode)

(flycheck-def-config-file-var flycheck-sass-lintrc sass/scss-sass-lint
                              ".sass-lint.yml"
  :package-version '(flycheck . "30"))

(flycheck-define-checker sass/scss-sass-lint
  "A SASS/SCSS syntax checker using sass-Lint.

See URL `https://github.com/sasstools/sass-lint'."
  :command ("sass-lint"
            "--verbose"
            "--no-exit"
            "--format" "Checkstyle"
            (config-file "--config" flycheck-sass-lintrc)
            source)
  :error-parser flycheck-parse-checkstyle
  :modes (sass-mode scss-mode))

(flycheck-define-checker scala
  "A Scala syntax checker using the Scala compiler.

See URL `https://www.scala-lang.org/'."
  :command ("scalac" "-Ystop-after:parser" source)
  :error-patterns
  ((error line-start (file-name) ":" line ": error: " (message) line-end))
  :modes scala-mode
  :next-checkers ((warning . scala-scalastyle)))

(flycheck-def-config-file-var flycheck-scalastylerc scala-scalastyle nil
  :package-version '(flycheck . "0.20"))

(flycheck-define-checker scala-scalastyle
  "A Scala style checker using scalastyle.

Note that this syntax checker is not used if
`flycheck-scalastylerc' is nil or refers to a non-existing file.

See URL `http://www.scalastyle.org'."
  :command ("scalastyle"
            (config-file "-c" flycheck-scalastylerc)
            source)
  :error-patterns
  ((error line-start "error file=" (file-name) " message="
          (message) " line=" line (optional " column=" column) line-end)
   (warning line-start "warning file=" (file-name) " message="
            (message) " line=" line (optional " column=" column) line-end))
  :error-filter (lambda (errors)
                  (flycheck-sanitize-errors
                   (flycheck-increment-error-columns errors)))
  :modes scala-mode
  :predicate
  ;; Inhibit this syntax checker if the JAR or the configuration are unset or
  ;; missing
  (lambda () (and flycheck-scalastylerc
                  (flycheck-locate-config-file flycheck-scalastylerc
                                               'scala-scalastyle)))
  :verify (lambda (checker)
            (let ((config-file (and flycheck-scalastylerc
                                    (flycheck-locate-config-file
                                     flycheck-scalastylerc checker))))
              (list
               (flycheck-verification-result-new
                :label "Configuration file"
                :message (cond
                          ((not flycheck-scalastylerc)
                           "`flycheck-scalastyletrc' not set")
                          ((not config-file)
                           (format "file %s not found" flycheck-scalastylerc))
                          (t (format "found at %s" config-file)))
                :face (cond
                       ((not flycheck-scalastylerc) '(bold warning))
                       ((not config-file) '(bold error))
                       (t 'success)))))))

(flycheck-def-args-var flycheck-scheme-chicken-args scheme-chicken
  :package-version '(flycheck . "32"))

(flycheck-define-checker scheme-chicken
  "A CHICKEN Scheme syntax checker using the CHICKEN compiler `csc'.

See URL `http://call-cc.org/'."
  :command ("csc" "-analyze-only" "-local"
            (eval flycheck-scheme-chicken-args)
            source)
  :error-patterns
  ((info line-start
         "Note: " (zero-or-more not-newline) ":\n"
         (one-or-more (any space)) "(" (file-name) ":" line ") " (message)
         line-end)
   (warning line-start
            "Warning: " (zero-or-more not-newline) ",\n"
            (one-or-more (any space)) (zero-or-more not-newline) ":\n"
            (one-or-more (any space)) "(" (file-name) ":" line ") " (message)
            line-end)
   (warning line-start
            "Warning: " (zero-or-more not-newline) ":\n"
            (one-or-more (any space)) "(" (file-name) ":" line ") " (message)
            line-end)
   (error line-start "Error: (line " line ") " (message) line-end)
   (error line-start "Syntax error: (" (file-name) ":" line ")"
          (zero-or-more not-newline) " - "
          (message (one-or-more not-newline)
                   (zero-or-more "\n"
                                 (zero-or-more space)
                                 (zero-or-more not-newline))
                   (one-or-more space) "<--")
          line-end)
   ;; A of version 4.12.0, the chicken compiler doesn't provide a
   ;; line number for this error.
   (error line-start "Syntax error: "
          (message (one-or-more not-newline)
                   (zero-or-more "\n"
                                 (zero-or-more space)
                                 (zero-or-more not-newline))
                   (one-or-more space) "<--")
          line-end)
   (error line-start
          "Error: " (zero-or-more not-newline) ":\n"
          (one-or-more (any space)) "(" (file-name) ":" line ") " (message)
          line-end)
   ;; A of version 4.12.0, the chicken compiler doesn't provide a
   ;; line number for this error.
   (error line-start "Error: "
          (message (one-or-more not-newline)
                   (zero-or-more "\n"
                                 (zero-or-more space)
                                 (zero-or-more not-newline))
                   (one-or-more space) "<--")))
  :error-filter flycheck-fill-empty-line-numbers
  :predicate
  (lambda ()
    ;; In `scheme-mode' we must check the current Scheme implementation
    ;; being used
    (and (boundp 'geiser-impl--implementation)
         (eq geiser-impl--implementation 'chicken)))
  :verify
  (lambda (_checker)
    (let ((geiser-impl (bound-and-true-p geiser-impl--implementation)))
      (list
       (flycheck-verification-result-new
        :label "Geiser Implementation"
        :message (cond
                  ((eq geiser-impl 'chicken) "Chicken Scheme")
                  (geiser-impl (format "Other: %s" geiser-impl))
                  (t "Geiser not active"))
        :face (cond
               ((eq geiser-impl 'chicken) 'success)
               (t '(bold error)))))))
  :modes scheme-mode)

(defconst flycheck-scss-lint-checkstyle-re
  (rx "cannot load such file" (1+ not-newline) "scss_lint_reporter_checkstyle")
  "Regular expression to parse missing checkstyle error.")

(defun flycheck-parse-scss-lint (output checker buffer)
  "Parse SCSS-Lint OUTPUT from CHECKER and BUFFER.

Like `flycheck-parse-checkstyle', but catches errors about
missing checkstyle reporter from SCSS-Lint."
  (if (string-match-p flycheck-scss-lint-checkstyle-re output)
      (list (flycheck-error-new-at
             1 nil 'error "Checkstyle reporter for SCSS-Lint missing.
Please run gem install scss_lint_reporter_checkstyle"
             :checker checker
             :buffer buffer
             :filename (buffer-file-name buffer)))
    (flycheck-parse-checkstyle output checker buffer)))

(flycheck-def-config-file-var flycheck-scss-lintrc scss-lint ".scss-lint.yml"
  :package-version '(flycheck . "0.23"))

(flycheck-define-checker scss-lint
  "A SCSS syntax checker using SCSS-Lint.

Needs SCSS-Lint 0.43.2 or newer.

See URL `https://github.com/brigade/scss-lint'."
  :command ("scss-lint"
            "--require=scss_lint_reporter_checkstyle"
            "--format=Checkstyle"
            (config-file "--config" flycheck-scss-lintrc)
            "--stdin-file-path" source-original "-")
  :standard-input t
  ;; We cannot directly parse Checkstyle XML, since for some mysterious reason
  ;; SCSS-Lint doesn't have a built-in Checkstyle reporter, and instead ships it
  ;; as an addon which might not be installed.  We use a custom error parser to
  ;; check whether the addon is missing and turn that into a special kind of
  ;; Flycheck error.
  :error-parser flycheck-parse-scss-lint
  :modes scss-mode
  :verify
  (lambda (checker)
    (-when-let
        (output (flycheck-call-checker-process-for-output
                 checker nil nil "--require=scss_lint_reporter_checkstyle"))
      (let ((reporter-missing
             (string-match-p flycheck-scss-lint-checkstyle-re output)))
        (list
         (flycheck-verification-result-new
          :label "checkstyle reporter"
          :message (if reporter-missing
                       "scss_lint_reporter_checkstyle plugin missing"
                     "present")
          :face (if reporter-missing
                    '(bold error)
                  'success)))))))

(flycheck-define-checker scss-stylelint
  "A SCSS syntax and style checker using stylelint.

See URL `http://stylelint.io/'."
  :command ("stylelint"
            (eval flycheck-stylelint-args)
            "--syntax" "scss"
            (option-flag "--quiet" flycheck-stylelint-quiet)
            (config-file "--config" flycheck-stylelintrc))
  :standard-input t
  :error-parser flycheck-parse-stylelint
  :predicate flycheck-buffer-nonempty-p
  :modes (scss-mode))

(flycheck-def-option-var flycheck-scss-compass nil scss
  "Whether to enable the Compass CSS framework.

When non-nil, enable the Compass CSS framework, via `--compass'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "0.16"))

(flycheck-define-checker scss
  "A SCSS syntax checker using the SCSS compiler.

See URL `http://sass-lang.com'."
  :command ("scss"
            "--cache-location" (eval (flycheck-sass-scss-cache-location))
            (option-flag "--compass" flycheck-scss-compass)
            "--check" "--stdin")
  :standard-input t
  :error-patterns
  ((error line-start
          (or "Syntax error: " "Error: ")
          (message (one-or-more not-newline)
                   (zero-or-more "\n"
                                 (one-or-more " ")
                                 (one-or-more not-newline)))
          (optional "\r") "\n" (one-or-more " ") "on line " line
          " of standard input"
          line-end)
   (warning line-start
            "WARNING: "
            (message (one-or-more not-newline)
                     (zero-or-more "\n"
                                   (one-or-more " ")
                                   (one-or-more not-newline)))
            (optional "\r") "\n" (one-or-more " ") "on line " line
            " of an unknown file"
            line-end))
  :modes scss-mode)

(flycheck-def-args-var flycheck-sh-bash-args (sh-bash)
  :package-version '(flycheck . "32"))

(flycheck-define-checker sh-bash
  "A Bash syntax checker using the Bash shell.

See URL `http://www.gnu.org/software/bash/'."
  :command ("bash" "--norc" "-n"
            (eval flycheck-sh-bash-args)
            "--")
  :standard-input t
  :error-patterns
  ((error line-start
          ;; The name/path of the bash executable
          (one-or-more (not (any ":"))) ":"
          ;; A label "line", possibly localized
          (one-or-more (not (any digit)))
          line (zero-or-more " ") ":" (zero-or-more " ")
          (message) line-end))
  :modes (sh-mode bash-ts-mode)
  :predicate (lambda () (eq sh-shell 'bash))
  :next-checkers ((warning . sh-shellcheck)))

(flycheck-define-checker sh-posix-dash
  "A POSIX Shell syntax checker using the Dash shell.

See URL `http://gondor.apana.org.au/~herbert/dash/'."
  :command ("dash" "-n")
  :standard-input t
  :error-patterns
  ((error line-start (one-or-more (not (any ":"))) ": " line ": " (message)))
  :modes sh-mode
  :predicate (lambda () (eq sh-shell 'sh))
  :next-checkers ((warning . sh-shellcheck)))

(flycheck-define-checker sh-posix-bash
  "A POSIX Shell syntax checker using the Bash shell.

See URL `http://www.gnu.org/software/bash/'."
  :command ("bash" "--posix" "--norc" "-n" "--")
  :standard-input t
  :error-patterns
  ((error line-start
          ;; The name/path of the bash executable
          (one-or-more (not (any ":"))) ":"
          ;; A label "line", possibly localized
          (one-or-more (not (any digit)))
          line (zero-or-more " ") ":" (zero-or-more " ")
          (message) line-end))
  :modes sh-mode
  :predicate (lambda () (eq sh-shell 'sh))
  :next-checkers ((warning . sh-shellcheck)))

(flycheck-define-checker sh-zsh
  "A Zsh syntax checker using the Zsh shell.

See URL `http://www.zsh.org/'."
  :command ("zsh" "--no-exec" "--no-globalrcs" "--no-rcs" source)
  :error-patterns
  ((error line-start (file-name) ":" line ": " (message) line-end))
  :modes sh-mode
  :predicate (lambda () (eq sh-shell 'zsh))
  :next-checkers ((warning . sh-shellcheck)))

(defconst flycheck-shellcheck-supported-shells '(bash ksh88 sh)
  "Shells supported by ShellCheck.")

(flycheck-def-option-var flycheck-shellcheck-excluded-warnings nil sh-shellcheck
  "A list of excluded warnings for ShellCheck.

The value of this variable is a list of strings, where each
string is a warning code to be excluded from ShellCheck reports.
By default, no warnings are excluded."
  :type '(repeat :tag "Excluded warnings"
                 (string :tag "Warning code"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.21"))

(flycheck-def-option-var flycheck-shellcheck-follow-sources t sh-shellcheck
  "Whether to follow external sourced files in scripts.

Shellcheck will follow and parse sourced files so long as a
pre-runtime resolvable path to the file is present.  This can
either be part of the source command itself:
   source /full/path/to/file.txt
or added as a shellcheck directive before the source command:
   # shellcheck source=/full/path/to/file.txt."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . "31"))

(flycheck-define-checker sh-shellcheck
  "A shell script syntax and style checker using Shellcheck.

See URL `https://github.com/koalaman/shellcheck/'."
  :command ("shellcheck"
            "--format" "checkstyle"
            "--shell" (eval (symbol-name sh-shell))
            (option-flag "--external-sources"
                         flycheck-shellcheck-follow-sources)
            (option "--exclude" flycheck-shellcheck-excluded-warnings list
                    flycheck-option-comma-separated-list)
            "-")
  :standard-input t
  :error-parser flycheck-parse-checkstyle
  :error-filter
  (lambda (errors)
    (flycheck-remove-error-file-names
     "-" (flycheck-dequalify-error-ids errors)))
  :modes (sh-mode bash-ts-mode)
  :predicate (lambda () (memq sh-shell flycheck-shellcheck-supported-shells))
  :verify (lambda (_)
            (let ((supports-shell (memq sh-shell
                                        flycheck-shellcheck-supported-shells)))
              (list
               (flycheck-verification-result-new
                :label (format "Shell %s supported" sh-shell)
                :message (if supports-shell "yes" "no")
                :face (if supports-shell 'success '(bold warning))))))
  :error-explainer
  (lambda (err)
    (let ((error-code (flycheck-error-id err))
          (url "https://github.com/koalaman/shellcheck/wiki/%s"))
      (and error-code `(url . ,(format url error-code))))))

(flycheck-define-checker slim
  "A Slim syntax checker using the Slim compiler.

See URL `http://slim-lang.com'."
  :command ("slimrb" "--compile")
  :standard-input t
  :error-patterns
  ((error line-start
          "Slim::Parser::SyntaxError:" (message) (optional "\r") "\n  "
          "STDIN, Line " line (optional ", Column " column)
          line-end))
  :modes slim-mode
  :next-checkers ((warning . slim-lint)))

(flycheck-define-checker slim-lint
  "A Slim linter.

See URL `https://github.com/sds/slim-lint'."
  :command ("slim-lint" "--reporter=checkstyle" source)
  :error-parser flycheck-parse-checkstyle
  :modes slim-mode)

(flycheck-define-checker sql-sqlint
  "A SQL syntax checker using the sqlint tool.

See URL `https://github.com/purcell/sqlint'."
  :command ("sqlint")
  :standard-input t
  :error-patterns
  ((warning line-start "stdin:" line ":" column ":WARNING "
            (message (one-or-more not-newline)
                     (zero-or-more "\n"
                                   (one-or-more "  ")
                                   (one-or-more not-newline)))
            line-end)
   (error line-start "stdin:" line ":" column ":ERROR "
          (message (one-or-more not-newline)
                   (zero-or-more "\n"
                                 (one-or-more "  ")
                                 (one-or-more not-newline)))
          line-end))
  :modes (sql-mode))

(flycheck-define-checker systemd-analyze
  "A systemd unit checker using systemd-analyze(1).

See URL
`https://www.freedesktop.org/software/systemd/man/systemd-analyze.html'."
  :command ("systemd-analyze" "verify" source)
  :error-parser flycheck-parse-with-patterns-without-color
  :error-patterns
  ((error line-start (file-name) ":" (optional line ":") (message) line-end)
   (error line-start "[" (file-name) ":" line "]" (message) line-end))
  :error-filter (lambda (errors)
                  (flycheck-sanitize-errors
                   (flycheck-fill-empty-line-numbers errors)))
  :modes (systemd-mode))

(flycheck-def-config-file-var flycheck-chktexrc tex-chktex ".chktexrc")

(flycheck-define-checker tcl-nagelfar
  "An extensible tcl syntax checker

See URL `http://nagelfar.sourceforge.net/'."
  :command ("nagelfar" "-H" source)
  :error-patterns
  ;; foo.tcl: 29: E Wrong number of arguments (4) to "set"
  ;; foo.tcl: 29: W Expr without braces
  ((info    line-start (file-name) ": " line ": N " (message) line-end)
   (warning line-start (file-name) ": " line ": W " (message) line-end)
   (error   line-start (file-name) ": " line ": E " (message) line-end))
  :modes tcl-mode)

(flycheck-define-checker terraform
  "A Terraform syntax checker with `terraform fmt'.

See URL `https://www.terraform.io/docs/commands/fmt.html'."
  :command ("terraform" "fmt" "-no-color" "-")
  :standard-input t
  :error-patterns
  ((error line-start "Error: " (one-or-more not-newline)
          "\n\n  on <stdin> line " line ", in " (one-or-more not-newline) ":"
          (one-or-more "\n" (zero-or-more space (one-or-more not-newline)))
          (message (one-or-more (and (one-or-more (not (any ?\n))) ?\n)))
          line-end)
   (error line-start "Error: " (one-or-more not-newline)
          "\n\n  on <stdin> line " line ":\n  (source code not available)\n\n"
          (message (one-or-more (and (one-or-more (not (any ?\n))) ?\n)))
          line-end))
  :next-checkers ((warning . terraform-tflint))
  :modes terraform-mode)

(flycheck-def-option-var flycheck-tflint-variable-files nil terraform-tflint
  "A list of files to resolve terraform variables.

The value of this variable is a list of strings, where each
string is a file to add to the terraform variables files.
Relative files are relative to the file being checked."
  :type '(repeat (directory :tag "Variable file"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "32"))

(defun flycheck-parse-tflint-linter (output checker buffer)
  "Parse tflint warnings from JSON OUTPUT.

CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.

See URL `https://github.com/terraform-linters/tflint' for more
information about tflint."
  (mapcar (lambda (err)
            (let-alist err
              (flycheck-error-new-at
               .range.start.line
               .range.start.column
               (pcase .rule.severity
                 ("error"   'error)
                 ("warning" 'warning)
                 (_         'error))
               .message
               :end-line .range.end.line
               :end-column .range.end.column
               :id .rule.name
               :checker checker
               :buffer buffer
               :filename (buffer-file-name buffer))))
          (cdr (assq 'issues (car (flycheck-parse-json output))))))

(flycheck-define-checker terraform-tflint
  "A Terraform checker using tflint.

See URL `https://github.com/terraform-linters/tflint'."
  :command ("tflint" "--format=json"
            (option-list "--var-file=" flycheck-tflint-variable-files concat)
            source-original)
  :error-parser flycheck-parse-tflint-linter
  :predicate flycheck-buffer-saved-p
  :modes terraform-mode)

(flycheck-define-checker tex-chktex
  "A TeX and LaTeX syntax and style checker using chktex.

See URL `http://www.nongnu.org/chktex/'."
  :command ("chktex"
            (config-file "--localrc" flycheck-chktexrc)
            ;; Compact error messages, and no version information, and execute
            ;; \input statements
            "--verbosity=0" "--quiet" "--inputfiles")
  :standard-input t
  :error-patterns
  ((warning line-start "stdin:" line ":" column ":"
            (id (one-or-more digit)) ":" (message) line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors (flycheck-increment-error-columns errors)))
  :modes (latex-mode plain-tex-mode))

(flycheck-define-checker tex-lacheck
  "A LaTeX syntax and style checker using lacheck.

See URL `http://www.ctan.org/pkg/lacheck'."
  :command ("lacheck" source-inplace)
  :error-patterns
  ((warning line-start
            "\"" (file-name) "\", line " line ": " (message)
            line-end))
  :modes latex-mode)

(flycheck-define-checker texinfo
  "A Texinfo syntax checker using makeinfo.

See URL `http://www.gnu.org/software/texinfo/'."
  :command ("makeinfo" "-o" null-device "-")
  :standard-input t
  :error-patterns
  ((warning line-start
            "-:" line (optional ":" column) ": " "warning: " (message)
            line-end)
   (error line-start
          "-:" line (optional ":" column) ": " (message)
          line-end))
  :modes texinfo-mode)

(flycheck-def-config-file-var flycheck-textlint-config
    textlint "textlintrc.json")

;; This needs to be set because textlint plugins are installed separately,
;; and there is no way to check their installation status -- textlint simply
;; prints a backtrace.
(flycheck-def-option-var flycheck-textlint-plugin-alist
    '((markdown-mode . "@textlint/markdown")
      (gfm-mode . "@textlint/markdown")
      (t . "@textlint/text"))
    textlint
  "An alist mapping major modes to textlint plugins.

Each item is a cons cell `(MAJOR-MODE . PLUGIN)', where MAJOR-MODE is a mode
`flycheck-textlint' supports and PLUGIN is a textlint plugin. As a catch-all,
when MAJOR-MODE is t, that PLUGIN will be used for any supported mode that
isn't specified.

See URL `https://npms.io/search?q=textlint-plugin' for all textlint plugins
published on NPM."
  :type '(repeat (choice (cons symbol string)
                         (cons (const t) string))))

(defun flycheck--textlint-get-plugin ()
  "Return the textlint plugin for the current mode."
  (cdr (-first
        (lambda (arg)
          (pcase-let ((`(,mode . _) arg))
            (or (and (booleanp mode) mode) ; mode is t
                (derived-mode-p mode))))
        flycheck-textlint-plugin-alist)))

(flycheck-define-checker textlint
  "A text prose linter using textlint.

See URL `https://textlint.github.io/'."
  :command ("textlint"
            (config-file "--config" flycheck-textlint-config)
            "--format" "json"
            ;; get the first matching plugin from plugin-alist
            "--plugin"
            (eval (flycheck--textlint-get-plugin))
            source)
  ;; textlint seems to say that its json output is compatible with ESLint.
  ;; https://textlint.github.io/docs/formatter.html
  :error-parser flycheck-parse-eslint
  ;; textlint can support different formats with textlint plugins, but
  ;; only text and markdown formats are installed by default. Ask the
  ;; user to add mode->plugin mappings manually in
  ;; `flycheck-textlint-plugin-alist'.
  :modes
  (text-mode markdown-mode gfm-mode message-mode adoc-mode
             mhtml-mode latex-mode org-mode rst-mode)
  :enabled
  (lambda () (flycheck--textlint-get-plugin))
  :verify
  (lambda (_)
    (let ((plugin (flycheck--textlint-get-plugin)))
      (list
       (flycheck-verification-result-new
        :label "textlint plugin"
        :message plugin
        :face 'success)))))

(flycheck-def-config-file-var flycheck-typescript-tslint-config
    typescript-tslint "tslint.json"
  :package-version '(flycheck . "27"))

(flycheck-def-option-var flycheck-typescript-tslint-rulesdir
    nil typescript-tslint
  "The directory of custom rules for TSLint.

The value of this variable is either a string containing the path
to a directory with custom rules, or nil, to not give any custom
rules to TSLint.

Refer to the TSLint manual at URL
`http://palantir.github.io/tslint/usage/cli/'
for more information about the custom directory."
  :type '(choice (const :tag "No custom rules directory" nil)
                 (directory :tag "Custom rules directory"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "27"))

(flycheck-def-args-var flycheck-tslint-args (typescript-tslint)
  :package-version '(flycheck . "31"))

(flycheck-define-checker typescript-tslint
  "TypeScript style checker using TSLint.

Note that this syntax checker is not used if
`flycheck-typescript-tslint-config' is nil or refers to a
non-existing file.

See URL `https://github.com/palantir/tslint'."
  :command ("tslint" "--format" "json"
            (config-file "--config" flycheck-typescript-tslint-config)
            (option "--rules-dir" flycheck-typescript-tslint-rulesdir)
            (eval flycheck-tslint-args)
            source-inplace)
  :error-parser flycheck-parse-tslint
  :modes (typescript-mode typescript-ts-mode tsx-ts-mode))

(flycheck-def-option-var flycheck-verilator-include-path nil verilog-verilator
  "A list of include directories for Verilator.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of Verilator.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.24"))

(flycheck-define-checker verilog-verilator
  "A Verilog syntax checker using the Verilator Verilog HDL simulator.

See URL `https://www.veripool.org/wiki/verilator'."
  :command ("verilator" "--lint-only" "-Wall" "--quiet-exit"
            (option-list "-I" flycheck-verilator-include-path concat)
            source)
  :error-patterns
  ((warning line-start "%Warning"
            (? "-" (id (+ (any "0-9A-Z_")))) ": "
            (? (file-name) ":" line ":" (? column ":") " ")
            (message) line-end)
   (error line-start "%Error"
          (? "-" (id (+ (any "0-9A-Z_")))) ": "
          (? (file-name) ":" line ":" (? column ":") " ")
          (message) line-end))
  :modes verilog-mode)

(flycheck-def-option-var flycheck-ghdl-language-standard nil vhdl-ghdl
  "The language standard to use in GHDL.

The value of this variable is either a string denoting a language
standard, or nil, to use the default standard.  When non-nil,
pass the language standard via the `--std' option."
  :type '(choice (const :tag "Default standard" nil)
                 (string :tag "Language standard"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "32"))
(make-variable-buffer-local 'flycheck-ghdl-language-standard)

(flycheck-def-option-var flycheck-ghdl-workdir nil vhdl-ghdl
  "The directory to use for the file library.

The value of this variable is either a string with the directory
to use for the file library, or nil, to use the default value.
When non-nil, pass the directory via the `--workdir' option."
  :type '(choice (const :tag "Default directory" nil)
                 (string :tag "Directory for the file library"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "32"))
(make-variable-buffer-local 'flycheck-ghdl-workdir)

(flycheck-def-option-var flycheck-ghdl-ieee-library nil vhdl-ghdl
  "The standard to use for the IEEE library.

The value of this variable is either a string denoting an ieee library
standard, or nil, to use the default standard.  When non-nil,
pass the ieee library standard via the `--ieee' option."
  :type '(choice (const :tag "Default standard" nil)
                 (const :tag "No IEEE Library" "none")
                 (const :tag "IEEE standard" "standard")
                 (const :tag "Synopsys standard" "synopsys")
                 (const :tag "Mentor standard" "mentor"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "32"))
(make-variable-buffer-local 'flycheck-ghdl-ieee-library)

(flycheck-define-checker vhdl-ghdl
  "A VHDL syntax checker using GHDL.

See URL `https://github.com/ghdl/ghdl'."
  :command ("ghdl"
            "-s" ; only do the syntax checking
            (option "--std=" flycheck-ghdl-language-standard concat)
            (option "--workdir=" flycheck-ghdl-workdir concat)
            (option "--ieee=" flycheck-ghdl-ieee-library concat)
            source)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": " (message) line-end))
  :modes vhdl-mode)

(flycheck-def-option-var flycheck-xml-xmlstarlet-xsd-path nil xml-xmlstarlet
  "An XSD schema to validate against."
  :type '(choice (const :tag "None" nil)
                 (file :tag "XSD schema"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "31"))

(flycheck-define-checker xml-xmlstarlet
  "A XML syntax checker and validator using the xmlstarlet utility.

See URL `http://xmlstar.sourceforge.net/'."
  ;; Validate standard input with verbose error messages, and do not dump
  ;; contents to standard output
  :command ("xmlstarlet" "val" "--err" "--quiet"
            (option "--xsd" flycheck-xml-xmlstarlet-xsd-path)
            "-")
  :standard-input t
  :error-patterns
  ((error line-start "-:" line "." column ": " (message) line-end))
  :modes (xml-mode nxml-mode))

(flycheck-def-option-var flycheck-xml-xmllint-xsd-path nil xml-xmllint
  "An XSD schema to validate against."
  :type '(choice (const :tag "None" nil)
                 (file :tag "XSD schema"))
  :safe #'flycheck-string-or-nil-p
  :package-version '(flycheck . "31"))

(flycheck-define-checker xml-xmllint
  "A XML syntax checker and validator using the xmllint utility.

The xmllint is part of libxml2, see URL
`http://www.xmlsoft.org/'."
  :command ("xmllint" "--noout"
            (option "--schema" flycheck-xml-xmllint-xsd-path)
            "-")
  :standard-input t
  :error-patterns
  ((error line-start "-:" line ": " (message) line-end))
  :modes (xml-mode nxml-mode))

(flycheck-define-checker yaml-jsyaml
  "A YAML syntax checker using JS-YAML.

See URL `https://github.com/nodeca/js-yaml'."
  :command ("js-yaml")
  :standard-input t
  :error-patterns
  ((error line-start
          (or "JS-YAML" "YAMLException") ": "
          (message) " at line " line ", column " column ":"
          line-end)
   (error line-start
          (or "JS-YAML" "YAMLException") ": "
          (message) " (" line ":" column ")"
          line-end))
  :modes (yaml-mode yaml-ts-mode)
  :next-checkers ((warning . yaml-yamllint)
                  (warning . cwl)))

(flycheck-define-checker yaml-ruby
  "A YAML syntax checker using Ruby's YAML parser.

This syntax checker uses the YAML parser from Ruby's standard
library.

See URL `http://www.ruby-doc.org/stdlib-2.0.0/libdoc/yaml/rdoc/YAML.html'."
  :command ("ruby" "-ryaml" "-e" "begin;
   YAML.load(STDIN); \
 rescue Exception => e; \
   STDERR.puts \"stdin:#{e}\"; \
 end")
  :standard-input t
  :error-patterns
  ((error line-start "stdin:" (zero-or-more not-newline) ":" (message)
          "at line " line " column " column line-end))
  :modes (yaml-mode yaml-ts-mode)
  :next-checkers ((warning . yaml-yamllint)
                  (warning . cwl)))

(flycheck-def-config-file-var flycheck-yamllintrc yaml-yamllint ".yamllint")

(flycheck-define-checker yaml-yamllint
  "A YAML syntax checker using YAMLLint.
See URL `https://github.com/adrienverge/yamllint'."
  :standard-input t
  :command ("yamllint" "-f" "parsable" "-"
            (config-file "-c" flycheck-yamllintrc))
  :error-patterns
  ((error line-start
          "stdin:" line ":" column ": [error] " (message) line-end)
   (warning line-start
            "stdin:" line ":" column ": [warning] " (message) line-end))
  :modes (yaml-mode yaml-ts-mode)
  :next-checkers ((warning . cwl)))

(provide 'flycheck)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; flycheck.el ends here
