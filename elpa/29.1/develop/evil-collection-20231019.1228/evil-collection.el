;;; evil-collection.el --- A set of keybindings for Evil mode -*- lexical-binding: t -*-

;; Copyright (C) 2017, 2023 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.2
;; Package-Requires: ((emacs "26.3") (evil "1.2.13") (annalist "1.0"))
;; Keywords: evil, tools

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
;; A set of keybindings for Evil mode.
;;
;; If you want to use Evil in the minibuffer, you'll have to enable it by
;; setting `evil-collection-setup-minibuffer' to t before loading this package.
;; This is so because many users find it confusing.
;; Some minibuffer-related packages such as Helm rely on this option.

;;; Code:

;; `evil' requires `seq-into'?
;; This require on `seq' before loading `evil 'prevents `evil' from erroring
;; out with the below message on Emacs 29.
;; Symbol's function definition is void: seq-into
;; Looks like this error can be traced through evil ->
;; Look at the commit that moved this line above `evil' to see the error message.
;; evil -> evil-vars -> read-kbd-macro -> seq-into -> error.
;; https://github.com/emacs-evil/evil/issues/1627
(require 'seq)
(require 'cl-lib)
(require 'evil)
(require 'annalist)

(defvar evil-collection-base-dir (file-name-directory load-file-name)
  "Store the directory evil-collection.el was loaded from.")

(defvar evil-want-integration)
(defvar evil-want-keybinding)
(if (featurep 'evil-keybindings)
    (if evil-want-keybinding
        (display-warning
         '(evil-collection)
         "Make sure to set `evil-want-keybinding' to nil before loading evil \
or evil-collection.\
\n
See https://github.com/emacs-evil/evil-collection/issues/60 for more details.")
      (display-warning
       '(evil-collection)
       "`evil-want-keybinding' was set to nil but not before loading evil.\
\n
Make sure to set `evil-want-keybinding' to nil before loading evil \
or evil-collection.\
\n
See https://github.com/emacs-evil/evil-collection/issues/60 for more details.")))

(unless (featurep 'evil-integration)
  (message "Requiring evil-integration. Set evil-want-integration to t to\
 remove this message.\
\n
See https://github.com/emacs-evil/evil-collection/issues/60 for more details.")
  (require 'evil-integration))

(defgroup evil-collection nil
  "A set of keybindings for Evil mode."
  :group 'evil)

(defcustom evil-collection-setup-minibuffer nil
  "Whether to setup Evil bindings in the minibuffer."
  :type 'boolean
  :group 'evil-collection)

(defcustom evil-collection-calendar-want-org-bindings nil
  "Whether to bind Org functions in calendar keymap."
  :type 'boolean
  :group 'evil-collection)

(defcustom evil-collection-setup-debugger-keys t
  "Whether to bind debugger keys when debugger is active.

Debugger in this case is dependent on mode.

This is only relevant for debug modes that are part of another mode,

e.g. `indium'. Modes like `edebug' or `realgud' needs to be explicitly disabled

through removing their entry from `evil-collection-mode-list'."
  :type 'boolean
  :group 'evil-collection)

(defcustom evil-collection-want-unimpaired-p t
  "Whether to enable unimpaired style bindings globally."
  :type 'boolean
  :group 'evil-collection)

(defcustom evil-collection-want-find-usages-bindings t
  "Whether to bind `xref-find-references'-like bindings.

This will bind additional find-* type commands, e.g. usages, assignments, etc.."
  :type 'boolean
  :group 'evil-collection)

(defvar evil-collection--modes-with-delayed-setup
  `(emms
    eshell)
  "List of modes whose keybinds aren't completely set up after the mode is
loaded. This can be a problem for cases where we're doing key translations
using `evil-collection-setup-hook' which would result in an empty keymap.

Normally we run `evil-collection-setup-hook' right away after the mode
is loaded in `with-eval-after-load' (see `evil-collection-init') but for these
modes, we skip running that hook and let the corresponding `evil-collection'
package handle running `evil-collection-setup-hook'.

Elements in this list either match a target mode symbol or the car of a list in
`evil-collection--supported-modes'.

If `evil-collection-always-run-setup-hook-after-load' is t, this list isn't
read and `evil-collection-setup-hook' will be ran in the
`with-eval-after-load' block in `evil-collection-init'.")

(defcustom evil-collection-always-run-setup-hook-after-load nil
  "Whether to always run `evil-collection-setup-hook' after mode is loaded.

See `evil-collection-init' and `evil-collection--modes-with-delayed-setup'."
  :type 'boolean
  :group 'evil-collection)

(defvar evil-collection--supported-modes
  `(2048-game
    ag
    alchemist
    anaconda-mode
    apropos
    arc-mode
    atomic-chrome
    auto-package-update
    beginend
    bluetooth
    bm
    bookmark
    (buff-menu "buff-menu")
    calc
    calendar
    cider
    cmake-mode
    color-rg
    comint
    company
    compile
    consult
    corfu
    crdt
    (custom cus-edit)
    cus-theme
    dashboard
    daemons
    deadgrep
    debbugs
    debug
    devdocs
    dictionary
    diff-hl
    diff-mode
    dired
    dired-sidebar
    disk-usage
    distel
    doc-view
    docker
    ebib
    ebuku
    edbi
    edebug
    ediff
    eglot
    elpaca
    ement
    explain-pause-mode
    eldoc
    elfeed
    elisp-mode
    elisp-refs
    elisp-slime-nav
    embark
    emms
    ,@(when (>= emacs-major-version 29) '(emoji))
    epa
    ert
    eshell
    eval-sexp-fu
    evil-mc
    eww
    fanyi
    finder
    flycheck
    flymake
    forge
    free-keys
    geiser
    ggtags
    git-timemachine
    gited
    gnus
    go-mode
    grep
    guix
    hackernews
    helm
    help
    helpful
    hg-histedit
    hungry-delete
    ibuffer
    (image image-mode)
    image-dired
    image+
    imenu
    imenu-list
    (indent "indent")
    indium
    info
    ivy
    js2-mode
    leetcode
    lispy
    lms
    log-edit
    log-view
    lsp-ui-imenu
    lua-mode
    kotlin-mode
    macrostep
    man
    (magit magit-repos magit-submodule)
    magit-section
    magit-todos
    markdown-mode
    ,@(when evil-collection-setup-minibuffer '(minibuffer))
    monky
    mpc
    mpdel
    mu4e
    mu4e-conversation
    neotree
    newsticker
    notmuch
    nov
    omnisharp
    org
    org-present
    org-roam
    osx-dictionary
    outline
    p4
    (package-menu package)
    pass
    (pdf pdf-view)
    popup
    proced
    (process-menu simple)
    prodigy
    profiler
    python
    quickrun
    racer
    racket-describe
    realgud
    reftex
    replace ;; For `occur'.
    restclient
    rg
    ripgrep
    rjsx-mode
    robe
    rtags
    ruby-mode
    scheme
    scroll-lock
    selectrum
    sh-script
    ,@(when (>= emacs-major-version 28) '(shortdoc))
    simple
    simple-mpc
    slime
    sly
    snake
    so-long
    speedbar
    ,@(when (>= emacs-major-version 27) '(tab-bar))
    tablist
    tabulated-list
    tar-mode
    telega
    (term term ansi-term multi-term)
    tetris
    ,@(when (>= emacs-major-version 27) '(thread))
    tide
    timer-list
    transmission
    trashed
    tuareg
    typescript-mode
    vc-annotate
    vc-dir
    vc-git
    vdiff
    vertico
    view
    vlf
    vterm
    vundo
    w3m
    wdired
    wgrep
    which-key
    woman
    xref
    xwidget
    yaml-mode
    youtube-dl
    zmusic
    (ztree ztree-diff ztree-dir))
  "List of modes supported by evil-collection. Elements are
either target mode symbols or lists which `car' is the mode
symbol and `cdr' the packages to register.")

(dolist (mode evil-collection--supported-modes)
  (let ((ec-mode-name (if (listp mode) (car mode) mode)))
    (autoload
      (intern (format "evil-collection-%s-setup" ec-mode-name))
      (expand-file-name
       (format "modes/%s/evil-collection-%s" ec-mode-name ec-mode-name)
       evil-collection-base-dir))))

(defcustom evil-collection-mode-list evil-collection--supported-modes
  "The list of modes which will be evilified by `evil-collection-init'.
Elements are either target mode symbols or lists which `car' is the
mode symbol and `cdr' the packages to register.

By default, `minibuffer' is not included because many users find
this confusing. It will be included if
`evil-collection-setup-minibuffer' is set to t."
  :type '(repeat (choice symbol sexp))
  :group 'evil-collection)

(defcustom evil-collection-config
  '((buff-menu :defer t)
    (calc :defer t)
    (comint :defer t)
    (debug :defer t)
    (diff-mode :defer t)
    (dired :defer t)
    (edebug :defer t)
    (eldoc :defer t)
    (help :defer t)
    (image :defer t)
    (indent :defer t)
    (dired :defer t)
    (info :defer t)
    (replace :defer t)
    (outline :defer t)
    (package :defer t)
    (package-menu :defer t)
    (process-menu :defer t)
    (simple :defer t)
    (tab-bar :defer t)
    (tabulated-list :defer t)
    (xref :defer t))
  "The list of modes with special configuration.

These modes should match entries within `evil-collection-mode-list'.

This variable is consumed only by `evil-collection-setup'.


NOTE: The API of this variable may change drastically.

Currently supported keys:

:defer t or TIME in seconds to defer loading mode."
  :type '(repeat (choice symbol sexp))
  :group 'evil-collection)

(defcustom evil-collection-key-whitelist '()
  "List of keys that may be used by Evil Collection.
This is a list of strings that are suitable for input to
`kbd'.  If there are no keys in the list, the whitelist will be ignored."
  :type '(repeat string)
  :group 'evil-collection)

(defcustom evil-collection-key-blacklist '()
  "List of keys that may not be used by Evil Collection.
This is a list of strings that are suitable for input to `kbd'."
  :type '(repeat string)
  :group 'evil-collection)

(defcustom evil-collection-state-passlist '()
  "List of evil states that may be used by Evil Collection.
This is a list of symbols that are suitable for input to
 `evil-define-key'. Ignore when there are no states in the list."
  :type '(repeat symbol)
  :group 'evil-collection)

(defcustom evil-collection-state-denylist
  (if (bound-and-true-p evil-disable-insert-state-bindings)
      '(insert)
    '())
  "List of evil states that may not be used by Evil Collection.
This is a list of symbols that are suitable for input to
 `evil-define-key'."
  :type '(repeat symbol)
  :group 'evil-collection)

(defvar evil-collection-setup-hook nil
  "Hook run by `evil-collection-init' for each mode that is evilified.
This hook runs after all setup (including keybindings) for a mode has already
taken place. The arguments passed to functions for this hook are the name of the
mode and a list of keymap names (i.e. symbols, not actual keymaps) customized by
Evil Collection for that mode. More arguments may be added in the future, so
functions added to this hook should include a \"&rest _rest\" for forward
compatibility.")

(defun evil-collection-define-operator-key (operator map-sym &rest bindings)
  "Define a key on a specific OPERATOR e.g. yank or delete.

This function is useful for adding specific binds to operator maps
\(e.g. `evil-yank' or `evil-delete') without erasing the original bind.

For example, say one wants to bind \"yf\" to something but also wants to keep
\"yy\".

This function takes care of checking the whitelist/blacklist against the full
binding.

For example:
\(evil-collection-define-operator-key \='yank
  \='pass-mode-map \"f\" \='pass-copy-field)

This will check \"yf\" against a user's white/blacklist and also record the
binding in `annalist' as so."
  (declare (indent defun))
  (let* ((prefix (if (eq operator 'yank) "y" "d"))
         (operators (if (eq operator 'yank)
                        'evil-collection-yank-operators
                      'evil-collection-delete-operators))
         (remap (if (eq operator 'yank) [remap evil-yank] [remap evil-delete]))
         (whitelist (mapcar 'kbd evil-collection-key-whitelist))
         (blacklist (mapcar 'kbd evil-collection-key-blacklist))
         filtered-bindings)
    (while bindings
      (let* ((key (pop bindings))
             (key-with-prefix (concat prefix key))
             (def (pop bindings))
             (def-with-menu-item
               `(menu-item
                 ""
                 nil
                 :filter
                 (lambda (&optional _)
                   (when (or
                          (eq evil-this-operator (key-binding ,remap))
                          (memq evil-this-operator ,operators))
                     (setq evil-inhibit-operator t)
                     ',def)))))
        (when (or (and whitelist (member key-with-prefix whitelist))
                  (not (member key-with-prefix blacklist)))
          (annalist-record 'evil-collection 'keybindings
                           ;; Record the binding as if it was in 'normal mode
                           ;; instead of 'operator mode as the user would be in
                           ;; normal mode when triggering the operator.
                           (list map-sym 'normal key-with-prefix def)
                           :local (or (eq map-sym 'local)
                                      (local-variable-p map-sym)))
          ;; Use the original key declared when actually setting the binding.
          (push key filtered-bindings)
          ;; Use the definition attached to the menu-item when setting the
          ;; binding.
          (push def-with-menu-item filtered-bindings))))
    (setq filtered-bindings (nreverse filtered-bindings))
    (evil-collection--define-key 'operator map-sym filtered-bindings)))

(defun evil-collection--filter-states (state)
  "Return a list states after filtering STATE (a single symbol or list of symbols).
The return value adheres to `evil-collection-state-passlist' and
`evil-collection-state-denylist'. When the STATE is nil, which
means all states for `evil-define-key', return nil."
  (let ((states (if (listp state) state (list state))))
    (seq-difference
     (if evil-collection-state-passlist
         (seq-intersection states evil-collection-state-passlist)
       states)
     evil-collection-state-denylist)))

(defun evil-collection-define-key (state map-sym &rest bindings)
  "Wrapper for `evil-define-key*' with additional features.
Unlike `evil-define-key*' MAP-SYM should be a quoted keymap other than the
unquoted keymap required for `evil-define-key*'. This function adds the ability
to filter keys on the basis of `evil-collection-key-whitelist' and
`evil-collection-key-blacklist'. It also records bindings with annalist.el."
  (declare (indent defun))
  (let* ((whitelist (mapcar 'kbd evil-collection-key-whitelist))
         (blacklist (mapcar 'kbd evil-collection-key-blacklist))
         (states-to-bind (evil-collection--filter-states state))
         filtered-bindings)
    (when (or states-to-bind (null state))
      (while bindings
        (let ((key (pop bindings))
              (def (pop bindings)))
          (when (or (and whitelist (member key whitelist))
                    (not (member key blacklist)))
            (annalist-record 'evil-collection 'keybindings
                             (list map-sym state key def)
                             :local (or (eq map-sym 'local)
                                        (local-variable-p map-sym)))
            (push key filtered-bindings)
            (push def filtered-bindings))))
      (setq filtered-bindings (nreverse filtered-bindings))
      (evil-collection--define-key states-to-bind map-sym filtered-bindings))))

(defun evil-collection-can-bind-key (key)
  "Return whether or not we should bind KEY."
  (let* ((whitelist (mapcar 'kbd evil-collection-key-whitelist))
         (blacklist (mapcar 'kbd evil-collection-key-blacklist)))
    (or (and whitelist (member key whitelist))
        (not (member key blacklist)))))

(defun evil-collection--define-key (state map-sym bindings)
  "Workhorse function for `evil-collection-define-key'.

See `evil-collection-define-key' docstring for more details."
  (cond ((null bindings))
        ((and (boundp map-sym) (keymapp (symbol-value map-sym)))
         (condition-case-unless-debug err
             (apply #'evil-define-key*
                    state (symbol-value map-sym) bindings)
           (error
            (message "evil-collection: error setting key in %s %S"
                     map-sym err))))
        ((boundp map-sym)
         (user-error "evil-collection: %s is not a keymap" map-sym))
        (t
         (let* ((fname (format "evil-collection-define-key-in-%s" map-sym))
                (fun (make-symbol fname)))
           (fset fun `(lambda (&rest args)
                        (when (and (boundp ',map-sym) (keymapp ,map-sym))
                          (remove-hook 'after-load-functions #',fun)
                          (condition-case-unless-debug err
                              (apply #'evil-define-key*
                                     ',state ,map-sym ',bindings)
                            (error
                             (message
                              ,(format
                                "evil-collection: error setting key in %s %%S"
                                map-sym)
                              err))))))
           (add-hook 'after-load-functions fun t)))))

(defun evil-collection-inhibit-insert-state (map-sym)
  "Unmap insertion keys from normal state.
This is particularly useful for read-only modes."
  (evil-collection-define-key 'normal map-sym
    [remap evil-append] #'ignore
    [remap evil-append-line] #'ignore
    [remap evil-insert] #'ignore
    [remap evil-insert-line] #'ignore
    [remap evil-change] #'ignore
    [remap evil-change-line] #'ignore
    [remap evil-substitute] #'ignore
    [remap evil-change-whole-line] #'ignore
    [remap evil-delete] #'ignore
    [remap evil-delete-line] #'ignore
    [remap evil-delete-char] #'ignore
    [remap evil-delete-backward-char] #'ignore
    [remap evil-replace] #'ignore
    [remap evil-replace-state] #'ignore
    [remap evil-open-below] #'ignore
    [remap evil-open-above] #'ignore
    [remap evil-paste-after] #'ignore
    [remap evil-paste-before] #'ignore
    [remap evil-join] #'ignore
    [remap evil-indent] #'ignore
    [remap evil-shift-left] #'ignore
    [remap evil-shift-right] #'ignore
    [remap evil-invert-char] #'ignore))

(defun evil-collection-set-readonly-bindings (map-sym)
  "Unmap insertion keys from normal state. Additionally q can `quit-window'.
This is particularly useful for read-only modes. Make sure it's
called before setting up other evil bindings so that it can be
overriden."
  (evil-collection-inhibit-insert-state map-sym)
  (evil-collection-define-key 'normal map-sym
    "q"  #'quit-window
    "ZZ" #'quit-window
    "ZQ" #'evil-quit))

(defun evil-collection--binding-lessp (a b)
  "Comparison function used to sort bindings of the form (state key def)."
  (let ((a-state (symbol-name (nth 0 a)))
        (b-state (symbol-name (nth 0 b)))
        (a-key (nth 1 a))
        (b-key (nth 1 b)))
    (if (not (string= a-state b-state))
        (string-lessp a-state b-state)
      (string-lessp a-key b-key))))

(annalist-define-view 'keybindings 'evil-collection-valid
  (list (list 'keymap :sort #'annalist-string-<)
        (list 'state :sort #'annalist-string-<))
  :inherit 'valid)

(annalist-define-view 'keybindings 'evil-collection-active
  (list (list 'keymap :sort #'annalist-string-<)
        (list 'state :sort #'annalist-string-<))
  :inherit 'active)

(defun evil-collection-describe-bindings (&optional arg)
  "Print bindings made by Evil Collection to separate buffer.

With non-nil ARG, restrict to bindings corresponding to active
modes in the current buffer."
  (interactive "P")
  (annalist-describe 'evil-collection 'keybindings
                     (if arg
                         'evil-collection-active
                       'evil-collection-valid)))

(defun evil-collection--mode-file (mode file)
  "Return path to FILE for MODE. Return nil if it doesn't exist."
  (let ((path (expand-file-name
               (format "modes/%s/%s" mode file) evil-collection-base-dir)))
    (when (file-exists-p path)
      path)))

(defun evil-collection-open-config-file (mode)
  "Open configuration file corresponding to MODE."
  (interactive
   (list
    (completing-read
     "Mode: "
     (cl-remove-if-not
      (lambda (mode)
        (evil-collection--mode-file mode (format "evil-collection-%s.el" mode)))
      (directory-files
       (expand-file-name "modes" evil-collection-base-dir)
       nil "^[^.]")))))
  (find-file (evil-collection--mode-file mode (format "evil-collection-%s.el" mode))))

(defun evil-collection-open-readme (mode)
  "Open README.org corresponding to MODE."
  (interactive
   (list
    (completing-read
     "Mode: "
     (cl-remove-if-not
      (lambda (mode)
        (evil-collection--mode-file mode "README.org"))
      (directory-files
       (expand-file-name "modes" evil-collection-base-dir)
       nil "^[^.]")))))
  (find-file (evil-collection--mode-file mode "README.org")))

(defun evil-collection--delay (condition form hook &optional append local name)
  "Execute FORM when CONDITION becomes true, checking with HOOK.
NAME specifies the name of the entry added to HOOK.  If APPEND is
non-nil, the entry is appended to the hook.  If LOCAL is non-nil,
the buffer-local value of HOOK is modified.

This is a backport of `evil-delay' without the deprecation notice to deal with CI until migration can be done.
Ref: https://github.com/emacs-evil/evil-collection/issues/750"
  (eval `(evil-with-delay ,condition (,hook ,append ,local ,name) ,form) t))

;;;###autoload
(cl-defun evil-collection-translate-minor-mode-key (states modes
                                                           &rest translations
                                                           &key destructive
                                                           &allow-other-keys)
  "Translate keys in the keymap(s) corresponding to STATES and MODES.

Similar to `evil-collection-translate-key' but for minor modes.
STATES should be the name of an evil state, a list of states, or nil. MODES
should be a symbol corresponding to minor-mode to make the translations in or a
list of minor-mode symbols. TRANSLATIONS corresponds to a list of
key replacement pairs. For example, specifying \"a\" \"b\" will bind \"a\" to
\"b\"'s definition in the keymap. Specifying nil as a replacement will unbind a
key. If DESTRUCTIVE is nil, a backup of the keymap will be stored on the initial
invocation, and future invocations will always look up keys in the backup
keymap. When no TRANSLATIONS are given, this function will only create the
backup keymap without making any translations. On the other hand, if DESTRUCTIVE
is non-nil, the keymap will be destructively altered without creating a backup.
For example, calling this function multiple times with \"a\" \"b\" \"b\" \"a\"
would continue to swap and unswap the definitions of these keys. This means that
when DESTRUCTIVE is non-nil, all related swaps/cycles should be done in the same
invocation."
  (declare (indent defun))
  (unless (listp modes)
    (setq modes (list modes)))
  (unless (and (listp states)
               (not (null states)))
    (setq states (list states)))
  (dolist (mode-symbol modes)
    (let ((keymap-symbol (intern (format "%S-map" mode-symbol))))
      (dolist (state states)
        (let ((hook-name
               (symbol-name
                (cl-gensym
                 (format "evil-collection-translate-key-in-%s" keymap-symbol)))))
          (evil-collection--delay `(and (boundp ',keymap-symbol)
                                       (keymapp ,keymap-symbol))
              `(evil-collection--translate-minor-mode-key
                 ',state
                 ',mode-symbol
                 ',translations
                 ,destructive)
              'after-load-functions
              t
              nil
              hook-name))))))


(defun evil-collection--translate-minor-mode-key (state
                                                  mode-symbol
                                                  translations
                                                  destructive)
  "Helper function for `evil-collection-translate-minor-mode-key'.
In the minor mode keymap corresponding to STATE and MODE-SYMBOL, make the key
TRANSLATIONS. When DESTRUCTIVE is non-nil, make the TRANSLATIONS destructively
without creating/referencing a backup keymap."
  (let* ((keymap-symbol (intern (format "%S-map" mode-symbol)))
         (backup-keymap-symbol (intern (format "evil-collection-%s%s-backup-map"
                                               mode-symbol
                                               (if state
                                                   (format "-%s-state" state)
                                                 ""))))
         (keymap (symbol-value keymap-symbol))
         (lookup-keymap (if (and (not destructive)
                                 (boundp backup-keymap-symbol))
                            (symbol-value backup-keymap-symbol)
                          (copy-keymap
                           (if state
                               (evil-get-minor-mode-keymap state mode-symbol)
                             keymap))))
         (maps (cl-loop for (key replacement) on translations by 'cddr
                        ;; :destructive can be in TRANSLATIONS
                        unless (keywordp key)
                        collect key
                        and collect (when replacement
                                      (evil-lookup-key lookup-keymap replacement)))))
    (unless (or destructive
                (boundp backup-keymap-symbol))
      (set backup-keymap-symbol lookup-keymap))
    (apply #'evil-define-minor-mode-key state mode-symbol maps)))

(defun evil-collection--translate-key (state keymap-symbol
                                             translations
                                             destructive)
  "Helper function for `evil-collection-translate-key'.
In the keymap corresponding to STATE and KEYMAP-SYMBOL, make the key
TRANSLATIONS. When DESTRUCTIVE is non-nil, make the TRANSLATIONS destructively
without creating/referencing a backup keymap."
  (let* ((backup-keymap-symbol (intern (format "evil-collection-%s%s-backup-map"
                                               keymap-symbol
                                               (if state
                                                   (format "-%s-state" state)
                                                 ""))))
         (keymap (symbol-value keymap-symbol))
         (lookup-keymap (if (and (not destructive)
                                 (boundp backup-keymap-symbol))
                            (symbol-value backup-keymap-symbol)
                          (copy-keymap
                           (if state
                               (evil-get-auxiliary-keymap keymap state t t)
                             keymap))))
         (maps (cl-loop for (key replacement) on translations by 'cddr
                        ;; :destructive can be in TRANSLATIONS
                        unless (keywordp key)
                        collect key
                        and collect (when replacement
                                      (evil-lookup-key lookup-keymap replacement)))))
    (unless (or destructive
                (boundp backup-keymap-symbol))
      (set backup-keymap-symbol lookup-keymap))
    (apply #'evil-define-key* state keymap maps)))

;;;###autoload
(cl-defun evil-collection-translate-key (states keymaps
                                                &rest translations
                                                &key destructive
                                                &allow-other-keys)
  "Translate keys in the keymap(s) corresponding to STATES and KEYMAPS.
STATES should be the name of an evil state, a list of states, or nil. KEYMAPS
should be a symbol corresponding to the keymap to make the translations in or a
list of keymap symbols. Like `evil-define-key', when a keymap does not exist,
the keybindings will be deferred until the keymap is defined, so
`with-eval-after-load' is not necessary. TRANSLATIONS corresponds to a list of
key replacement pairs. For example, specifying \"a\" \"b\" will bind \"a\" to
\"b\"'s definition in the keymap. Specifying nil as a replacement will unbind a
key. If DESTRUCTIVE is nil, a backup of the keymap will be stored on the initial
invocation, and future invocations will always look up keys in the backup
keymap. When no TRANSLATIONS are given, this function will only create the
backup keymap without making any translations. On the other hand, if DESTRUCTIVE
is non-nil, the keymap will be destructively altered without creating a backup.
For example, calling this function multiple times with \"a\" \"b\" \"b\" \"a\"
would continue to swap and unswap the definitions of these keys. This means that
when DESTRUCTIVE is non-nil, all related swaps/cycles should be done in the same
invocation."
  (declare (indent defun))
  (unless (listp keymaps)
    (setq keymaps (list keymaps)))
  (unless (and (listp states)
               (not (null states)))
    (setq states (list states)))
  (dolist (keymap-symbol keymaps)
    (dolist (state states)
      (let ((hook-name
             (symbol-name
              (cl-gensym
               (format "evil-collection-translate-key-in-%s" keymap-symbol)))))
        (evil-collection--delay `(and (boundp ',keymap-symbol)
                                      (keymapp ,keymap-symbol))
            `(evil-collection--translate-key
               ',state
               ',keymap-symbol
               ',translations
               ,destructive)
            'after-load-functions
             t
             nil
             hook-name)))))


;;;###autoload
(defmacro evil-collection-swap-key (states keymaps &rest args)
  "Wrapper around `evil-collection-translate-key' for swapping keys.
STATES, KEYMAPS, and ARGS are passed to `evil-collection-translate-key'. ARGS
should consist of key swaps (e.g. \"a\" \"b\" is equivalent to \"a\" \"b\" \"b\"
\"a\" with `evil-collection-translate-key') and optionally keyword arguments for
`evil-collection-translate-key'."
  (declare (indent defun))
  (setq args (cl-loop for (key replacement) on args by 'cddr
                      collect key and collect replacement
                      and unless (keywordp key)
                      collect replacement and collect key))
  `(evil-collection-translate-key ,states ,keymaps ,@args))

;;;###autoload
(defmacro evil-collection-swap-minor-mode-key (states modes &rest args)
  "Wrapper around `evil-collection-translate-minor-mode-key' for swapping keys.
STATES, MODES, and ARGS are passed to
`evil-collection-translate-minor-mode-key'. ARGS should consist of key swaps
\(e.g. \"a\" \"b\" is equivalent to \"a\" \"b\" \"b\" \"a\"
with `evil-collection-translate-minor-mode-key') and optionally keyword
arguments for `evil-collection-translate-minor-mode-key'."
  (declare (indent defun))
  (setq args (cl-loop for (key replacement) on args by 'cddr
                      collect key and collect replacement
                      and unless (keywordp key)
                      collect replacement and collect key))
  `(evil-collection-translate-minor-mode-key ,states ,modes ,@args))

;;;###autoload
(defun evil-collection-require (mode &optional noerror)
  "Require the evil-collection-MODE file, but do not activate it.

MODE should be a symbol. This requires the evil-collection-MODE
feature without needing to manipulate `load-path'. NOERROR is
forwarded to `require'."
  (let* ((mode-name (symbol-name mode))
         (feature (intern (format "evil-collection-%s" mode-name)))
         (file (expand-file-name
                (format "modes/%s/evil-collection-%s" mode-name mode-name)
                evil-collection-base-dir)))
    (require feature file noerror)))

(declare-function evil-collection-unimpaired-setup "evil-collection-unimpaired")

;;;###autoload
(defun evil-collection-init (&optional modes)
  "Register the Evil bindings for all modes in `evil-collection-mode-list'.

Alternatively, you may register select bindings manually, for
instance:

  (with-eval-after-load \='calendar
    (evil-collection-calendar-setup))

If MODES is specified (as either one mode or a list of modes), use those modes
instead of the modes in `evil-collection-mode-list'."
  (interactive)
  (if modes
      (or (listp modes) (setq modes (list modes)))
    (setq modes evil-collection-mode-list))
  (dolist (mode modes)
    (let ((m mode)
          (reqs (list mode)))
      (when (listp mode)
        (setq m (car mode)
              reqs (cdr mode)))
      (dolist (req reqs)
        (with-eval-after-load req
          ;; (message (format "Loaded %S..." req))
          (evil-collection-require m)
          (funcall (intern (concat "evil-collection-" (symbol-name m)
                                   "-setup")))
          (let ((mode-keymaps
                 (ignore-errors
                   (symbol-value
                    (intern (format "evil-collection-%s-maps" m))))))
            (when (or evil-collection-always-run-setup-hook-after-load
                      (not (memq m evil-collection--modes-with-delayed-setup)))
              (run-hook-with-args 'evil-collection-setup-hook
                                  m mode-keymaps)))))))
  (when evil-collection-want-unimpaired-p
    (evil-collection-require 'unimpaired)
    (evil-collection-unimpaired-setup)))

(defun evil-collection-setup (&optional modes)
  "Register the Evil bindings for all modes in `evil-collection-mode-list'.

----------------------EXPERIMENTAL------------------------------------------

This is a special wrapper over `evil-collection-init' that respects
configuration from `evil-collection-config'. This function is experimental,
so don't use if you don't want breakages or API changes.

If MODES is specified (as either one mode or a list of modes), use those modes
instead of the modes in `evil-collection-mode-list'.

----------------------EXPERIMENTAL------------------------------------------"
  (if modes
      (or (listp modes) (setq modes (list modes)))
    (setq modes evil-collection-mode-list))
  (let ((configs evil-collection-config)
        (deferred-modes)
        (delays))
    (dolist (config configs)
      (let ((defer (plist-get (cdr config) :defer)))
        (when defer
          (push (car config) deferred-modes)
          (push defer delays))))
    (let ((filtered-modes
           (cl-remove-if
            (lambda (mode)
              (let ((filterp nil)
                    (modes (if (consp mode) mode (list mode))))
                (dolist (m modes)
                  (let ((x (memq m deferred-modes)))
                    (when x
                      (setf filterp t)
                      ;; `evil-collection-config' format is slightly different
                      ;; than `evil-collection-mode-list', so use the mode
                      ;; entry from the mode list instead.
                      (setf (car x) mode))))
                filterp))
            modes)))
      (evil-collection-init filtered-modes))
    (message (format "Deferring: %S" deferred-modes))
    (dotimes (i (length deferred-modes))
      (let ((mode (nth i deferred-modes))
            (delay (nth i delays)))
        ;; (message (format "Delaying %S..."
        ;;                  (if (consp mode) (car mode) mode)))
        (run-with-idle-timer
         (if (numberp delay) delay 3) nil
         (apply-partially 'evil-collection-init (list mode)))))))

(defvar evil-collection-delete-operators '(evil-delete
                                           evil-cp-delete
                                           evil-sp-delete
                                           lispyville-delete)
  "List of delete operators.")

(defvar evil-collection-yank-operators '(evil-yank
                                         evil-cp-yank
                                         evil-sp-yank
                                         lispyville-yank)
  "List of yank operators.")

;;* Search

(defun evil-collection-evil-search-enabled ()
  (eq evil-search-module 'evil-search))

(defvar evil-collection-evil-search-forward
  '(menu-item "" nil :filter (lambda (&optional _)
                               (if (eq evil-search-module 'evil-search)
                                   #'evil-ex-search-forward
                                 #'evil-search-forward))))

(defvar evil-collection-evil-search-backward
  '(menu-item "" nil :filter (lambda (&optional _)
                               (if (eq evil-search-module 'evil-search)
                                   #'evil-ex-search-backward
                                 #'evil-search-backward))))

(defvar evil-collection-evil-search-next
  '(menu-item "" nil :filter (lambda (&optional _)
                               (if (eq evil-search-module 'evil-search)
                                   #'evil-ex-search-next
                                 #'evil-search-next))))

(defvar evil-collection-evil-search-previous
  '(menu-item "" nil :filter (lambda (&optional _)
                               (if (eq evil-search-module 'evil-search)
                                   #'evil-ex-search-previous
                                 #'evil-search-previous))))

(provide 'evil-collection)
;;; evil-collection.el ends here
