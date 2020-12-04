;;; core-dotspace-macs.el --- Space-macs Core File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3
(defconst dotspace-macs-template-directory
  (expand-file-name (concat space-macs-core-directory "templates/"))
  "Templates directory.")

(defconst dotspace-macs-test-results-buffer "*dotfile-test-results*"
  "Name of the buffer to display dotfile test results.")

(defvar dotspace-macs--user-config-elapsed-time 0
  "Time spent in `dotspace-macs/user-config' function.
Useful for users in order to given them a hint of potential bottleneck in
their configuration.")

(let* ((env (getenv "SPACe-macsDIR"))
       (env-dir (when env (expand-file-name (concat env "/"))))
       (env-init (and env-dir (expand-file-name "init.el" env-dir)))
       (no-env-dir-default (expand-file-name
                            (concat user-home-directory
                                    ".space-macs.d/")))
       (default-init (expand-file-name ".space-macs" user-home-directory)))
  (defconst dotspace-macs-directory
    (cond
     ((and env (file-exists-p env-dir)) env-dir)
     ((file-exists-p no-env-dir-default) no-env-dir-default)
     (t nil))
    "Optional space-macs directory, which defaults to
~/.space-macs.d. This setting can be overridden using the
SPACe-macsDIR environment variable. If neither of these
directories exist, this variable will be nil.")

  (defvar dotspace-macs-filepath
    (let ((space-macs-dir-init (when dotspace-macs-directory
                                (concat dotspace-macs-directory
                                        "init.el"))))
      (cond
       (env-init)
       ((file-exists-p default-init) default-init)
       ((and dotspace-macs-directory (file-exists-p space-macs-dir-init))
        space-macs-dir-init)
       (t default-init)))
    "Filepath to the installed dotfile. If SPACe-macsDIR is given
then SPACe-macsDIR/init.el is used. Otherwise, if ~/.space-macs
exists, then this is used. If ~/.space-macs does not exist, then
check for init.el in dotspace-macs-directory and use this if it
exists. Otherwise, fallback to ~/.space-macs"))

(defvar dotspace-macs-distribution 'space-macs
  "Base distribution to use. This is a layer contained in the directory
`+distributions'. For now available distributions are `space-macs-base'
or `space-macs'.")

(defvar dotspace-macs-enable-e-macs-pdumper nil
  "If non-nil then enable support for the portable dumper. You'll need
to compile e-macs 27 from source following the instructions in file
EXPERIMENTAL.org at the root of the git repository.")

(defvar dotspace-macs-e-macs-pdumper-executable-file "e-macs"
  "File path pointing to e-macs 27 or later executable.")

(defvar dotspace-macs-e-macs-dumper-dump-file
  (format "space-macs-%s.pdmp" e-macs-version)
  "Name of the Space-macs dump file. This is the file will be created by the
portable dumper in the cache directory under dumps sub-directory.
To load it when starting e-macs add the parameter `--dump-file'
when invoking e-macs 27.1 executable on the command line, for instance:
./e-macs --dump-file=$HOME/.e-macs.d/.cache/dumps/space-macs-27.1.pdmp")

(defvar dotspace-macs-gc-cons '(100000000 0.1)
  "Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
This is an advanced option and should not be changed unless you suspect
performance issues due to garbage collection operations.")

(defvar dotspace-macs-read-process-output-max (* 1024 1024)
  "Set `read-process-output-max' when startup finishes.
This defines how much data is read from a foreign process.
Setting this >= 1 MB should increase performance for lsp servers
in e-macs 27.")

(defvar dotspace-macs-elpa-https t
  "If non nil ELPA repositories are contacted via HTTPS whenever it's
possible. Set it to nil if you have no way to use HTTPS in your
environment, otherwise it is strongly recommended to let it set to t.")

(defvar dotspace-macs-elpa-timeout 5
  "Maximum allowed time in seconds to contact an ELPA repository.")

(defvar dotspace-macs-use-spacelpa nil
  "If non-nil then Spacelpa repository is the primary source to install
a locked version of packages. If nil then Space-macs will install the latest
version of packages from MELPA. Spacelpa is currently in experimental
state and should only be used for testing.")

(defvar dotspace-macs-verify-spacelpa-archives nil
  "If non-nil then verify the signature for downloaded Spacelpa archives.")

(defvar dotspace-macs-elpa-subdirectory 'e-macs-version
  "If non-nil, a form that evaluates to a package directory. For
example, to use different package directories for different e-macs
versions, set this to `e-macs-version'.")

(defvar dotspace-macs-configuration-layer-path '()
  "List of additional paths where to look for configuration layers.
Paths must have a trailing slash (ie. `~/.mycontribs/')")

(defvar dotspace-macs-install-packages 'used-only
  "Defines the behaviour of Space-macs when installing packages.
Possible values are `used-only', `used-but-keep-unused' and `all'. `used-only'
installs only explicitly used packages and deletes any unused packages as well
as their unused dependencies. `used-but-keep-unused' installs only the used
packages but won't delete unused ones. `all' installs *all*
packages supported by Space-macs and never uninstalls them.")

(defvar dotspace-macs-enable-lazy-installation 'unused
  "Lazy installation of layers (i.e. layers are installed only when a file
with a supported type is opened). Possible values are `all', `unused' and `nil'.
`unused' will lazy install only unused layers (i.e. layers not listed in
variable `dotspace-macs-configuration-layers'), `all' will lazy install any layer
that support lazy installation even the layers listed in
`dotspace-macs-configuration-layers'. `nil' disable the lazy installation feature
and you have to explicitly list a layer in the variable
`dotspace-macs-configuration-layers' to install it.")

(defvar dotspace-macs-ask-for-lazy-installation t
  "If non-nil then Space-macs will ask for confirmation before installing
a layer lazily.")

(defvar dotspace-macs-additional-packages '()
  "List of additional packages that will be installed wihout being
wrapped in a layer. If you need some configuration for these
packages then consider to create a layer, you can also put the
configuration in `dotspace-macs/user-config'.")

(defvar dotspace-macs--additional-theme-packages '()
  "Same as `dotspace-macs-additional-packages' but reserved for themes declared
in `dotspace-macs-themes'.")

(defvar dotspace-macs-editing-style 'vim
  "One of `vim', `e-macs' or `hybrid'.
`hybrid' is like `vim' except that `insert state' is replaced by the
`hybrid state' with `e-macs' key bindings. The value can also be a list
 with `:variables' keyword (similar to layers). Check the editing styles
 section of the documentation for details on available variables.")

(defvar dotspace-macs-startup-banner 'official
  "Specify the startup banner. Default value is `official', it displays
the official space-macs logo. An integer value is the index of text
banner, `random' chooses a random text banner in `core/banners'
directory. A string value must be a path to a .PNG file.
If the value is nil then no banner is displayed.")

(defvar dotspace-macs-startup-buffer-show-version t
  "If bound, show Space-macs and e-macs version at the top right of the
Space-macs buffer.")

(defvar dotspace-macs-scratch-mode 'text-mode
  "Default major mode of the scratch buffer.")

(defvar dotspace-macs-initial-scratch-message 'nil
  "Initial message in the scratch buffer.")

(defvar dotspace-macs-check-for-update nil
  "If non nil then space-macs will check for updates at startup
when the current branch is not `develop'. Note that checking for
new versions works via git commands, thus it calls GitHub services
whenever you start e-macs.")

(defvar dotspace-macs-configuration-layers '(e-macs-lisp)
  "List of configuration layers to load.")

(defvar dotspace-macs--configuration-layers-saved nil
  "Saved value of `dotspace-macs-configuration-layers' after sync.")

(defvar dotspace-macs-themes '(space-macs-dark
                              space-macs-light)
  "List of themes, the first of the list is loaded when space-macs starts.
Press `SPC T n' to cycle to the next theme in the list (works great
with 2 themes variants, one dark and one light")

(defvar dotspace-macs-mode-line-theme '(space-macs
                                       :separator wave
                                       :separator-scale 1.5)
  "Set the theme for the Spaceline. Supported themes are `space-macs',
`all-the-icons', `custom', `doom',`vim-powerline' and `vanilla'. The first three
are spaceline themes. `doom' is the doom-e-macs mode-line. `vanilla' is default
e-macs mode-line. `custom' is a user defined themes, refer to the
DOCUMENTATION.org for more info on how to create your own spaceline theme. Value
can be a symbol or a list with additional properties like '(all-the-icons
:separator-scale 1.5).")

(defvar dotspace-macs-frame-title-format "%I@%S"
  "Default format string for a frame title bar, using the
original format spec, and additional customizations.")

(defvar dotspace-macs-icon-title-format nil
  "Default format string for a icon title bar, using the
original format spec, and additional customizations.")

(defvar dotspace-macs-colorize-cursor-according-to-state t
  "If non nil the cursor color matches the state color in GUI e-macs.")

(defvar dotspace-macs-leader-key "SPC"
  "The leader key.")

(defvar dotspace-macs-e-macs-leader-key "M-m"
  "The leader key accessible in `e-macs state' and `insert state'")

(defvar dotspace-macs-major-mode-leader-key ","
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`. Set it to `nil` to disable it.")

(defvar dotspace-macs-major-mode-e-macs-leader-key (if window-system "<M-return>" "C-M-m")
  "Major mode leader key accessible in `e-macs state' and `insert state'")

(defvar dotspace-macs-ex-command-key ":"
  "The key used for Vim Ex commands.")

(defvar dotspace-macs-command-key "SPC"
  "The key used for e-macs commands (M-x) (after pressing on the leader key).")
(defvaralias 'dotspace-macs-e-macs-command-key 'dotspace-macs-command-key
  "New official name for `dotspace-macs-command-key'")

(defvar dotspace-macs-distinguish-gui-tab nil
  "If non nil, distinguish C-i and tab in the GUI version of e-macs.")

;; (defvar dotspace-macs-distinguish-gui-ret nil
;;   "If non nil, distinguish C-m and return in the GUI version of
;; e-macs.")

(defvar dotspace-macs-default-font '("Source Code Pro"
                                    :size 10.0
                                    :weight normal
                                    :width normal)
  "Default font, or prioritized list of fonts. This setting has no effect when
running e-macs in terminal.")

(defvar dotspace-macs-folding-method 'evil
  "Code folding method. Possible values are `evil', `origami' and `vimish'.")

(defvar dotspace-macs-default-layout-name "Default"
  "Name of the default layout.")

(defvar dotspace-macs-display-default-layout nil
  "If non nil the default layout name is displayed in the mode-line.")

(defvar dotspace-macs-auto-resume-layouts nil
  "If non nil then the last auto saved layouts are resume automatically upon
start.")

(defvar dotspace-macs-auto-generate-layout-names nil
  "If non-nil, auto-generate layout name when creating new layouts.
Only has effect when using the \"jump to layout by number\" commands.")

(defvar dotspace-macs-max-rollback-slots 5
  "Maximum number of rollback slots to keep in the cache.")

(defvar dotspace-macs-large-file-size 1
  "Size (in MB) above which space-macs will prompt to open the large file
literally to avoid performance issues. Opening a file literally means that
no major mode or minor modes are active.")

(defvar dotspace-macs-auto-save-file-location 'cache
  "Location where to auto-save files. Possible values are `original' to
auto-save the file in-place, `cache' to auto-save the file to another
file stored in the cache directory and `nil' to disable auto-saving.")

(defvar dotspace-macs-enable-paste-transient-state nil
  "If non-nil, the paste transient-state is enabled. While enabled, after you
paste something, pressing `C-j' and `C-k' several times cycles through the
elements in the `kill-ring'.")
(defvaralias
  'dotspace-macs-enable-paste-micro-state
  'dotspace-macs-enable-paste-transient-state
  "Old name of `dotspace-macs-enable-paste-transient-state'.")

(defvar dotspace-macs-which-key-delay 0.4
  "Delay in seconds starting from the last keystroke after which
the which-key buffer will be shown if you have not completed a
key sequence. Setting this variable is equivalent to setting
`which-key-idle-delay'.")

(defvar dotspace-macs-which-key-position 'bottom
  "Location of the which-key popup buffer. Possible choices are bottom,
right, and right-then-bottom. The last one will display on the
right if possible and fallback to bottom if not.")

(defvar dotspace-macs-switch-to-buffer-prefers-purpose nil
  "Control where `switch-to-buffer' displays the buffer.
If nil, `switch-to-buffer' displays the buffer in the current
window even if another same-purpose window is available. If non
nil, `switch-to-buffer' displays the buffer in a same-purpose
window even if the buffer can be displayed in the current
window.")

(defvar dotspace-macs-loading-progress-bar t
  "If non nil a progress bar is displayed when space-macs is loading. This
may increase the boot time on some systems and e-macs builds, set it to nil
to boost the loading time.")

(defvar dotspace-macs-fullscreen-at-startup nil
  "If non nil the frame is fullscreen when e-macs starts up (e-macs 24.4+ only).")

(defvar dotspace-macs-fullscreen-use-non-native nil
  "If non nil `space-macs/toggle-fullscreen' will not use native fullscreen. Use
to disable fullscreen animations on macOS.")

(defvar dotspace-macs-maximized-at-startup nil
  "If non nil the frame is maximized when e-macs starts up (e-macs 24.4+ only).
Takes effect only if `dotspace-macs-fullscreen-at-startup' is nil.")

(defvar dotspace-macs-undecorated-at-startup nil
  "If non nil the frame is undecorated when e-macs starts up.")

(defvar dotspace-macs-active-transparency 90
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's active or selected. Transparency
can be toggled through `toggle-transparency'.")

(defvar dotspace-macs-inactive-transparency 90
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's inactive or deselected. Transparency
can be toggled through `toggle-transparency'.")

(defvar dotspace-macs-show-transient-state-title t
  "If non nil show the titles of transient states.")

(defvar dotspace-macs-show-transient-state-color-guide t
  "If non nil show the color guide hint for transient state keys.")

(defvar dotspace-macs-mode-line-unicode-symbols t
  "If non nil unicode symbols are displayed in the mode-line (eg. for lighters).
If you use e-macs as a daemon and wants unicode characters only in GUI set
the value to quoted `display-graphic-p'. (default t)")

(defvar dotspace-macs-smooth-scrolling t
  "If non nil smooth scrolling (native-scrolling) is enabled.
Smooth scrolling overrides the default behavior of e-macs which
recenters point when it reaches the top or bottom of the
screen.")

(defvar dotspace-macs-line-numbers nil
  "Control line numbers activation.
If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
`text-mode' derivatives. If set to `relative', line numbers are relative.
This variable can also be set to a property list for finer control:
'(:relative nil
  :disabled-for-modes dired-mode
                      doc-view-mode
                      markdown-mode
                      org-mode
                      pdf-view-mode
                      text-mode
  :size-limit-kb 1000)
The property `:enabled-for-modes' takes priority over `:disabled-for-modes' and
restricts line-number to the specified list of major-mode.")

(defvar dotspace-macs-enable-server nil
  "If non-nil, start an e-macs server if one is not already running.")

(defvar dotspace-macs-persistent-server nil
  "If non nil advises quit functions to keep server open when quitting.")

(defvar dotspace-macs-server-socket-dir nil
  "Set the e-macs server socket location.
If nil, uses whatever the e-macs default is,
otherwise a directory path like \"~/.e-macs.d/server\".
Has no effect if `dotspace-macs-enable-server' is nil.")

(defvar dotspace-macs-smartparens-strict-mode nil
  "If non-nil smartparens-strict-mode will be enabled in programming modes.")

(defvar dotspace-macs-smart-closing-parenthesis nil
  "If non-nil pressing the closing parenthesis `)' key in insert mode passes
over any automatically added closing parenthesis, bracket, quote, etc...
This can be temporary disabled by pressing `C-q' before `)'. (default nil)")

(defvar dotspace-macs-zone-out-when-idle nil
  "Either nil or a number of seconds.
If non-nil zone out after the specified number of seconds.")

(defvar dotspace-macs-highlight-delimiters 'all
  "Select a scope to highlight delimiters.
Possible values are `any', `current', `all' or `nil'.
Default is `all' (highlight any scope and emphasize the current one.")

(defvar dotspace-macs-whitespace-cleanup nil
  "Delete whitespace while saving buffer.

Possible values are:
`all' to aggressively delete empty lines and long sequences of whitespace,
`trailing' to delete only the whitespace at end of lines,
`changed' to delete only whitespace for changed lines or
`nil' to disable cleanup.")

(defvar dotspace-macs-search-tools '("rg" "ag" "pt" "ack" "grep")
  "List of search tool executable names. Space-macs uses the first installed
tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.")

(defvar dotspace-macs-startup-lists '((recents  . 5)
                                     (projects . 7))
  "Association list of items to show in the startup buffer of the form
`(list-type . list-size)`. If nil it is disabled.

Possible values for list-type are:
`recents' `bookmarks' `projects' `agenda' `todos'.
List sizes may be nil, in which case
`space-macs--buffer-startup-lists-length' takes effect.")

(defvar dotspace-macs-startup-buffer-responsive t
  "True if the home buffer should respond to resize events.")

(defvar dotspace-macs-excluded-packages '()
  "A list of packages that will not be installed and loaded.")

(defvar dotspace-macs-frozen-packages '()
  "A list of packages that cannot be updated.")

(defvar dotspace-macs-pretty-docs nil
  "Run `space-macs/prettify-org-buffer' when
visiting README.org files of Space-macs.")

(defvar dotspace-macs-new-empty-buffer-major-mode nil
  "Set the major mode for a new empty buffer.")

(defvar dotspace-macs-use-clean-aindent-mode t
  "Correct indentation for simple modes.

If non nil activate `clean-aindent-mode' which tries to correct
virtual indentation of simple modes. This can interfer with mode specific
indent handling like has been reported for `go-mode'.
If it does deactivate it here. (default t)")

(defvar dotspace-macs-swap-number-row nil
  "Shift number row for easier access.

If non-nil shift your number row to match the entered keyboard layout
(only in insert mode). Currently the keyboard layouts
(qwerty-us qwertz-de) are supported.
New layouts can be added in `space-macs-editing' layer.
(default nil)")

(defvar dotspace-macs-home-shorten-agenda-source nil
  "If nil the home buffer shows the full path of agenda items
and todos. If non nil only the file name is shown.")

(defvar dotspace-macs--pretty-ignore-subdirs
  '(".cache/junk")
  "Subdirectories of `space-macs-start-directory' to ignore when
prettifying Org files.")

(defvar dotspace-macs-scratch-buffer-persistent nil
  "If non-nil, *scratch* buffer will be persistent. Things you write down in
   *scratch* buffer will be saved automatically.")

(defvar dotspace-macs-scratch-buffer-unkillable nil
  "If non-nil, `kill-buffer' on *scratch* buffer
will bury it instead of killing.")

(defun dotspace-macs//prettify-space-macs-docs ()
  "Run `space-macs/prettify-org-buffer' if `buffer-file-name'
looks like Space-macs documentation."
  (when (and dotspace-macs-pretty-docs
             space-macs-start-directory
             buffer-file-name)
    (let ((start-dir (expand-file-name space-macs-start-directory))
          (buf-path (expand-file-name buffer-file-name)))
      (when (and (string-prefix-p start-dir buf-path)
                 (not (--any? (string-prefix-p (expand-file-name it start-dir) buf-path)
                              dotspace-macs--pretty-ignore-subdirs)))
        (space-macs/prettify-org-buffer)))))

;; only for backward compatibility
(defalias 'dotspace-macs-mode 'e-macs-lisp-mode)

(defmacro dotspace-macs|call-func (func &optional msg)
  "Call the function from the dotfile only if it is bound.
If MSG is not nil then display a message in `*Messages*'. Errors
are caught and signaled to user in space-macs buffer."
  `(progn
     (when ,msg (space-macs-buffer/message ,msg))
     (when (fboundp ',func)
       (condition-case-unless-debug err
           (,func)
         (error
          (configuration-layer//increment-error-count)
          (space-macs-buffer/append (format "Error in %s: %s\n"
                                           ',(symbol-name func)
                                           (error-message-string err))
                                   t))))))

(defun dotspace-macs/call-user-env ()
  "Call the function `dotspace-macs/user-env'."
  (interactive)
  (dotspace-macs|call-func dotspace-macs/user-env "Calling dotfile user env..."))

(defun dotspace-macs/go-to-function (func)
  "Open the dotfile and goes to FUNC function."
  (interactive)
  (find-function func))

(defun dotspace-macs/go-to-user-env ()
  "Go to the `dotspace-macs/user-env' function."
  (interactive)
  (dotspace-macs/go-to-function 'dotspace-macs/user-env))

(defun dotspace-macs//check-layers-changed ()
  "Check if the value of `dotspace-macs-configuration-layers'
changed, and issue a warning if it did."
  (unless (eq dotspace-macs-configuration-layers
              dotspace-macs--configuration-layers-saved)
    (space-macs-buffer/warning (concat
                               "`dotspace-macs-configuration-layers' was "
                               "changed outside of `dotspace-macs/layers'."))))
(add-hook 'space-macs-post-user-config-hook
          'dotspace-macs//check-layers-changed)

(defun dotspace-macs//read-editing-style-config (config)
  "Read editing style CONFIG: apply variables and return the editing style.
CONFIG can be the symbol of an editing style or a list where the car is
the symbol of an editing style and the cdr is a list of keyword arguments like
  `:variables'."
  (cond
   ((symbolp config) config)
   ((listp config)
    (let ((variables (space-macs/mplist-get-values config :variables)))
      (while variables
        (let ((var (pop variables)))
          (if (consp variables)
              (condition-case-unless-debug err
                  (set-default var (eval (pop variables)))
                ('error
                 (space-macs-buffer/append
                  (format (concat "\nAn error occurred while reading the "
                                  "editing style variable %s "
                                  "(error: %s). Be sure to quote the value "
                                  "if needed.\n")
                          var err))))
            (space-macs-buffer/warning "Missing value for variable %s !" var)))))
    (car config))))

(defun dotspace-macs/add-layer (layer-name)
  "Add LAYER_NAME to dotfile and reload the it.
Returns non nil if the layer has been effectively inserted."
  (unless (configuration-layer/layer-used-p layer-name)
    (with-current-buffer (find-file-noselect (dotspace-macs/location))
      (goto-char (point-min))
      (let ((insert-point
             (re-search-forward
              "[^`]dotspace-macs-configuration-layers\\s-*\n?[^(]*\\((\\)")))
        (insert (format "%S" layer-name))
        (unless (equal (point) (point-at-eol))
          (insert "\n"))
        (indent-region insert-point (min (point-max)
                                         (+ insert-point 2
                                            (length (symbol-name layer-name)))))
        (save-buffer)))
    (load-file (dotspace-macs/location))
    t))

(defun dotspace-macs//profile-user-config (f &rest args)
  "Compute time taken by the `dotspace-macs/user-config' function."
  (let ((stime (current-time)))
    (apply f args)
    (setq dotspace-macs--user-config-elapsed-time
          (float-time (time-subtract (current-time) stime)))))

(defun dotspace-macs/sync-configuration-layers (&optional arg)
  "Synchronize declared layers in dotfile with space-macs.

Called with `C-u' skips `dotspace-macs/user-config'.
Called with `C-u C-u' skips `dotspace-macs/user-config' _and_ preliminary tests."
  (interactive "P")
  (when (file-exists-p dotspace-macs-filepath)
    (with-current-buffer (find-file-noselect dotspace-macs-filepath)
      (let ((dotspace-macs-loading-progress-bar nil))
        (setq space-macs-loading-string "")
        (save-buffer)
        (let ((tests-ok (or (equal arg '(16)) (dotspace-macs/test-dotfile t))))
          (if tests-ok
              (progn
                (load-file buffer-file-name)
                (dotspace-macs|call-func dotspace-macs/init
                                        "Calling dotfile init...")
                (dotspace-macs|call-func dotspace-macs/user-init
                                        "Calling dotfile user init...")
                (setq dotspace-macs-editing-style
                      (dotspace-macs//read-editing-style-config
                       dotspace-macs-editing-style))
                (dotspace-macs/call-user-env)
                ;; try to force a redump when reloading the configuration
                (let ((space-macs-force-dump t))
                  (configuration-layer/load))
                (if (member arg '((4) (16)))
                    (message (concat "Done (`dotspace-macs/user-config' "
                                     "function has been skipped)."))
                  (dotspace-macs|call-func dotspace-macs/user-config
                                          "Calling dotfile user config...")
                  (run-hooks 'space-macs-post-user-config-hook)
                  (message "Done.")))
            (switch-to-buffer-other-window dotspace-macs-test-results-buffer)
            (space-macs-buffer/warning "Some tests failed, check `%s' buffer"
                                      dotspace-macs-test-results-buffer))))))
  (when (configuration-layer/package-used-p 'spaceline)
    (space-macs//restore-buffers-powerline)))

(defun dotspace-macs/get-variable-string-list ()
  "Return a list of all the dotspace-macs variables as strings."
  (all-completions "" obarray
                   (lambda (x)
                     (and (boundp x)
                          (not (keywordp x))
                          ;; avoid private variables to show up
                          (not (string-match-p "--" (symbol-name x)))
                          (string-prefix-p "dotspace-macs" (symbol-name x))))))

(defun dotspace-macs/get-variable-list ()
  "Return a list of all dotspace-macs variable symbols."
  (mapcar 'intern (dotspace-macs/get-variable-string-list)))

(defmacro dotspace-macs|symbol-value (symbol)
  "Return the value of SYMBOL corresponding to a dotspace-macs variable.
If SYMBOL value is `display-graphic-p' then return the result of
`(display-graphic-p)', otherwise return the value of the symbol."
  `(if (eq 'display-graphic-p ,symbol) (display-graphic-p) ,symbol))

(defun dotspace-macs/location ()
  "Return the absolute path to the space-macs dotfile."
  dotspace-macs-filepath)

(defun dotspace-macs/copy-template ()
  "Copy `.space-macs.template' in home directory. Ask for confirmation
before copying the file if the destination already exists."
  (interactive)
  (let* ((copy? (if (file-exists-p dotspace-macs-filepath)
                    (y-or-n-p
                     (format "%s already exists. Do you want to overwrite it ? "
                             dotspace-macs-filepath)) t)))
    (when copy?
      (copy-file (concat dotspace-macs-template-directory
                         ".space-macs.template")
                 dotspace-macs-filepath t)
      (message "%s has been installed." dotspace-macs-filepath))))

(defun dotspace-macs//ido-completing-read (prompt candidates)
  "Call `ido-completing-read' with a CANDIDATES alist where the key is
a display strng and the value is the actual value to return."
  (let ((ido-max-window-height (1+ (length candidates))))
    (cadr (assoc (ido-completing-read prompt (mapcar 'car candidates))
                 candidates))))

(defun dotspace-macs/maybe-install-dotfile ()
  "Install the dotfile if it does not exist."
  (unless (file-exists-p dotspace-macs-filepath)
    (space-macs-buffer/set-mode-line "Dotfile wizard installer" t)
    (when (dotspace-macs/install 'with-wizard)
      (configuration-layer/load))))

(defun dotspace-macs/install (arg)
  "Install the dotfile, return non nil if the doftile has been installed.

If ARG is non nil then ask questions to the user before installing the dotfile."
  (interactive "P")
  ;; preferences is an alist where the key is the text to replace by
  ;; the value in the dotfile
  (let ((preferences
         (when arg
           ;; editing style
           `(("dotspace-macs-editing-style 'vim"
              ,(format
                "dotspace-macs-editing-style '%S"
                (dotspace-macs//ido-completing-read
                 "What is your preferred editing style? "
                 '(("Among the stars aboard the Evil flagship (vim)"
                    vim)
                   ("On the planet e-macs in the Holy control tower (e-macs)"
                    e-macs)))))
             ("dotspace-macs-distribution 'space-macs"
              ,(format
                "dotspace-macs-distribution '%S"
                (dotspace-macs//ido-completing-read
                 "What distribution of space-macs would you like to start with? "
                 `(("The standard distribution, recommended (space-macs)"
                    space-macs)
                   (,(concat "A minimalist distribution that you can build on "
                             "(space-macs-base)")
                    space-macs-base)))))))))
    (with-current-buffer (find-file-noselect
                          (concat dotspace-macs-template-directory
                                  ".space-macs.template"))
      (dolist (p preferences)
        (goto-char (point-min))
        (re-search-forward (car p))
        (replace-match (cadr p)))
      (let ((install
             (if (file-exists-p dotspace-macs-filepath)
                 (y-or-n-p
                  (format "%s already exists. Do you want to overwrite it ? "
                          dotspace-macs-filepath)) t)))
        (when install
          (write-file dotspace-macs-filepath)
          (message "%s has been installed." dotspace-macs-filepath)
          t))))
  (dotspace-macs/load-file)
  ;; force new wizard values to be applied
  (dotspace-macs/init))

(defun dotspace-macs/load-file ()
  "Load ~/.space-macs if it exists."
  (let ((dotspace-macs (dotspace-macs/location)))
    (if (file-exists-p dotspace-macs)
        (unless (with-demoted-errors "Error loading .space-macs: %S"
                  (load dotspace-macs))
          (dotspace-macs/safe-load))))
  (advice-add 'dotspace-macs/user-config
              :around 'dotspace-macs//profile-user-config))

(defun space-macs/title-prepare (title-format)
  "A string is printed verbatim except for %-constructs.
  %a -- prints the `abbreviated-file-name', or `buffer-name'
  %t -- prints `projectile-project-name'
  %I -- prints `invocation-name'
  %S -- prints `system-name'
  %U -- prints contents of $USER
  %b -- prints buffer name
  %f -- prints visited file name
  %F -- prints frame name
  %s -- prints process status
  %p -- prints percent of buffer above top of window, or Top, Bot or All
  %P -- prints percent of buffer above bottom of window, perhaps plus Top, or
  print Bottom or All
  %m -- prints mode name
  %n -- prints Narrow if appropriate
  %z -- prints mnemonics of buffer, terminal, and keyboard coding systems
  %Z -- like %z, but including the end-of-line format"
  ;; save-match-data to work around e-macs bug, see
  ;; https://github.com/syl20bnr/space-macs/issues/9700
  (save-match-data
    ;; disable buffer-list-update-hook to work around recursive invocations
    ;; caused by the temp-buffer used by `format-spec' below, see
    ;; https://github.com/syl20bnr/space-macs/issues/12387
    (let* ((buffer-list-update-hook nil)
           (project-name
            (when (string-match-p "%t" title-format)
              (if (boundp 'space-macs--buffer-project-name)
                  space-macs--buffer-project-name
                (set (make-local-variable 'space-macs--buffer-project-name)
                     (if (fboundp 'projectile-project-name)
                         (projectile-project-name)
                       "-")))))
           (abbreviated-file-name
            (when (string-match-p "%a" title-format)
              (if (boundp 'space-macs--buffer-abbreviated-filename)
                  space-macs--buffer-abbreviated-filename
                (set (make-local-variable
                      'space-macs--buffer-abbreviated-filename)
                     (abbreviate-file-name (or (buffer-file-name)
                                               (buffer-name)))))))
           (fs (format-spec-make
                ?a abbreviated-file-name
                ?t project-name
                ?S (system-name)
                ?I invocation-name
                ?U (or (getenv "USER") "")
                ?b "%b"
                ?f "%f"
                ?F "%F"
                ?* "%*"
                ?+ "%+"
                ?s "%s"
                ?l "%l"
                ?c "%c"
                ?p "%p"
                ?P "%P"
                ?m "%m"
                ?n "%n"
                ?z "%z"
                ?Z "%Z"
                ?\[ "%["
                ?\] "%]"
                ?% "%%"
                ?- "%-"
                )))
      (format-spec title-format fs))))

(defun dotspace-macs/safe-load ()
  "Error recovery from malformed .space-macs.
Loads default .space-macs template and suspends pruning of orphan packages.
Informs users of error and prompts for default editing style for use during
error recovery."
  (load (concat dotspace-macs-template-directory
                ".space-macs.template"))
  (defadvice dotspace-macs/layers
      (after error-recover-preserve-packages activate)
    (progn
      (setq-default dotspace-macs-install-packages 'used-but-keep-unused)
      (ad-disable-advice 'dotspace-macs/layers 'after
                         'error-recover-preserve-packages)
      (ad-activate 'dotspace-macs/layers)))
  (defadvice dotspace-macs/init
      (after error-recover-prompt-for-style activate)
    (progn
      (setq-default dotspace-macs-editing-style
                    (intern
                     (ido-completing-read
                      (format
                       (concat
                        "Space-macs encountered an error while "
                        "loading your `%s' file.\n"
                        "Pick your editing style for recovery "
                        "(use left and right arrows): ")
                       dotspace-macs-filepath)
                      '(("vim" vim)
                        ("e-macs" e-macs)
                        ("hybrid" hybrid))
                      nil t nil nil 'vim)))
      (ad-disable-advice 'dotspace-macs/init 'after
                         'error-recover-prompt-for-style)
      (ad-activate 'dotspace-macs/init))))

(defun dotspace-macs//test-dotspace-macs/layers ()
  "Tests for `dotspace-macs/layers'"
  (insert
   (format (concat "\n* Testing settings in dotspace-macs/layers "
                   "[[file:%s::dotspace-macs/layers][Show in File]]\n")
           dotspace-macs-filepath))
  ;; protect global values of these variables
  (let (dotspace-macs-additional-packages
        dotspace-macs-configuration-layer-path
        dotspace-macs-configuration-layers
        dotspace-macs-excluded-packages
        dotspace-macs-install-packages
        (passed-tests 0)
        (total-tests 0))
    (load dotspace-macs-filepath)
    (dotspace-macs/layers)
    (space-macs//test-list 'stringp
                          'dotspace-macs-configuration-layer-path
                          "is a string" "path")
    (space-macs//test-list 'file-directory-p
                          'dotspace-macs-configuration-layer-path
                          "exists in filesystem" "path")
    (setq dotspace-macs-configuration-layers
          (mapcar (lambda (l) (if (listp l) (car l) l))
                  (remove nil dotspace-macs-configuration-layers)))
    (space-macs//test-list 'configuration-layer/get-layer-path
                          'dotspace-macs-configuration-layers
                          "can be found" "layer")
    (insert (format
             (concat "** RESULTS: "
                     "[[file:%s::dotspace-macs/layers][dotspace-macs/layers]] "
                     "passed %s out of %s tests\n")
             dotspace-macs-filepath passed-tests total-tests))
    (equal passed-tests total-tests)))

(defmacro dotspace-macs||let-init-test (&rest body)
  "Macro to protect dotspace-macs variables"
  `(let ((fpath dotspace-macs-filepath)
         ,@(dotspace-macs/get-variable-list)
         (passed-tests 0) (total-tests 0))
     (setq dotspace-macs-filepath fpath)
     (load dotspace-macs-filepath)
     ,@body))

(defun dotspace-macs//test-dotspace-macs/init ()
  "Tests for `dotspace-macs/init'"
  (insert
   (format (concat "\n* Testing settings in dotspace-macs/init "
                   "[[file:%s::dotspace-macs/init][Show in File]]\n")
           dotspace-macs-filepath))
  (dotspace-macs||let-init-test
   (dotspace-macs/init)
   (space-macs//test-var
    (lambda (x)
      (or (member x '(vim
                      e-macs
                      hybrid))
          (and (listp x)
               (member (car x) '(vim e-macs hybrid))
               (space-macs/mplist-get-values x :variables))))
    'dotspace-macs-editing-style
    "is \'vim, \'e-macs or \'hybrid or and list with `:variables' keyword")
   (space-macs//test-var
    (lambda (x)
      (let ((themes '(space-macs
                      all-the-icons
                      custom
                      doom
                      vim-powerline
                      vanilla)))
        (or (member x themes)
            (and (listp x)
                 (memq (car x) themes)
                 ;; TODO define a function to remove several properties at once
                 (null (space-macs/mplist-remove
                        (space-macs/mplist-remove (cdr x) :separator)
                        :separator-scale))))))
    'dotspace-macs-mode-line-theme
    (concat
     "is \'space-macs, \'all-the-icons, \'custom, \'vim-powerline or 'vanilla "
     "or a list with `car' one of the previous values and properties one of "
     "the following: `:separator' or `:separator-scale'"))
   (space-macs//test-var
    (lambda (x) (member x '(original cache nil)))
    'dotspace-macs-auto-save-file-location (concat "is one of \'original, "
                                                  "\'cache or nil"))
   (space-macs//test-var
    (lambda (x) (member x '(all any current nil)))
    'dotspace-macs-highlight-delimiters
    "is one of \'all, \'any, \'current or nil")
   (space-macs//test-list
    (lambda (x)
      (let ((el (or (car-safe x) x))
            (list-size (cdr-safe x)))
        (member el '(recents bookmarks projects todos agenda))))
    'dotspace-macs-startup-lists (concat "includes \'recents, "
                                        "\'bookmarks, \'todos, "
                                        "\'agenda or \'projects"))
   (space-macs//test-list
    (lambda (x)
      (let ((el (or (car-safe x) x))
            (list-size (cdr-safe x)))
        (or (null list-size)(numberp list-size))))
    'dotspace-macs-startup-lists (concat "list size is a number"))
   (space-macs//test-var 'stringp 'dotspace-macs-leader-key "is a string")
   (space-macs//test-var 'stringp 'dotspace-macs-e-macs-leader-key "is a string")
   (space-macs//test-var
    (lambda (x) (or (null x) (stringp x)))
    'dotspace-macs-major-mode-leader-key "is a string or nil")
   (space-macs//test-var
    (lambda (x) (or (null x) (stringp x)))
    'dotspace-macs-major-mode-e-macs-leader-key "is a string or nil")
   (space-macs//test-var 'stringp 'dotspace-macs-e-macs-command-key "is a string")
   (insert (format
            (concat "** RESULTS: "
                    "[[file:%s::dotspace-macs/init][dotspace-macs/init]] "
                    "passed %s out of %s tests\n")
            dotspace-macs-filepath passed-tests total-tests))
   (equal passed-tests total-tests)))

(defun dotspace-macs/test-dotfile (&optional hide-buffer)
  "Test settings in dotfile for correctness.
Return non-nil if all the tests passed."
  (interactive)
  (configuration-layer/discover-layers 'refresh-index)
  (let ((min-version "0.0"))
    ;; dotspace-macs-version not implemented yet
    ;; (if (version< dotspace-macs-version min-version)
    (if nil
        (error (format (concat "error: dotspace-macs/test-dotfile requires "
                               "dotspace-macs-version %s")
                       min-version))
      (with-current-buffer (get-buffer-create dotspace-macs-test-results-buffer)
        (unless hide-buffer
          (switch-to-buffer-other-window dotspace-macs-test-results-buffer))
        (org-mode)
        (org-indent-mode)
        (view-mode)
        (when (bound-and-true-p flyspell-mode)
          (flyspell-mode -1))
        (let (buffer-read-only)
          (erase-buffer)
          (insert (format "* Running tests on [[file:%s][%s]] (v%s)\n"
                          dotspace-macs-filepath dotspace-macs-filepath "0.0"))
          ;; dotspace-macs-version not implemented yet
          ;; (insert (format "* Running tests on %s (v%s)\n"
          ;;                 dotspace-macs-filepath dotspace-macs-version))
          (prog1
              ;; execute all tests no matter what
              (cl-reduce (lambda (x y)
                           (and (funcall y) x))
                         '(dotspace-macs//test-dotspace-macs/layers
                           dotspace-macs//test-dotspace-macs/init)
                         :initial-value t)
            (goto-char (point-min))))))))

(provide 'core-dotspace-macs)


