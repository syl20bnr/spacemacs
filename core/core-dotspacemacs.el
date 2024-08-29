;;; core-dotspacemacs.el --- Spacemacs Core File -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'core-load-paths)
(require 'core-customization)

(defconst dotspacemacs-template-directory
  (concat spacemacs-core-directory "templates/")
  "Templates directory.")

(defconst dotspacemacs-test-results-buffer "*dotfile-test-results*"
  "Name of the buffer to display dotfile test results.")

(defvar dotspacemacs--user-config-elapsed-time 0
  "Time spent in `dotspacemacs/user-config' function.
Useful for users in order to given them a hint of potential bottleneck in
their configuration.")

(defconst dotspacemacs-directory
  (let* ((spacemacs-dir-env (getenv "SPACEMACSDIR"))
         (spacemacs-dir (if spacemacs-dir-env
                            (file-name-as-directory spacemacs-dir-env)
                          "~/.spacemacs.d/")))
    (when (file-directory-p spacemacs-dir)
      spacemacs-dir))
  "Directory containing Spacemacs customizations (defaults to nil).
- If environment variable SPACEMACSDIR is set and that directory exists,
  use that value.
- Otherwise use ~/.spacemacs.d if it exists.")

(defconst dotspacemacs-filepath
  (let* ((spacemacs-dir-env (getenv "SPACEMACSDIR"))
         (spacemacs-init (if spacemacs-dir-env
                             (concat (file-name-as-directory spacemacs-dir-env)
                                     "init.el")
                           "~/.spacemacs")))
    (if (file-regular-p spacemacs-init)
        spacemacs-init
      (let ((fallback-init "~/.spacemacs.d/init.el"))
        (if (file-regular-p fallback-init)
            fallback-init
          spacemacs-init))))
  "Filepath to Spacemacs configuration file (defaults to ~/.spacemacs).
- If environment variable SPACEMACSDIR is set and $SPACEMACSDIR/init.el
  exists, use that value.
- Otherwise use ~/.spacemacs if it exists.
- Otherwise use ~/.spacemacs.d/init.el if it exists.")

(spacemacs|defc dotspacemacs-distribution 'spacemacs
  "Base distribution to use. This is a layer contained in the directory
`+distributions'. For now available distributions are `spacemacs-base'
or `spacemacs'."
  '(choice (const spacemacs-base) (const spacemacs))
  'spacemacs-dotspacemacs-layers)

(spacemacs|defc dotspacemacs-enable-emacs-pdumper nil
  "If non-nil then enable support for the portable dumper. You'll need
to compile Emacs 27 from source following the instructions in file
EXPERIMENTAL.org at the root of the git repository."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-emacs-pdumper-executable-file "emacs"
  "File path pointing to emacs 27 or later executable."
  'string
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-emacs-dumper-dump-file
  (format "spacemacs-%s.pdmp" emacs-version)
  "Name of the Spacemacs dump file. This is the file will be created by the
portable dumper in the cache directory under dumps sub-directory.
To load it when starting Emacs add the parameter `--dump-file'
when invoking Emacs 27.1 executable on the command line, for instance:
./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp"
  'string
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-gc-cons '(100000000 0.1)
  "Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
This is an advanced option and should not be changed unless you suspect
performance issues due to garbage collection operations."
  '(list integer float)
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-read-process-output-max (* 1024 1024)
  "Set `read-process-output-max' when startup finishes.
This defines how much data is read from a foreign process.
Setting this >= 1 MB should increase performance for lsp servers
in emacs 27."
  'integer
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-elpa-https t
  "If non nil ELPA repositories are contacted via HTTPS whenever it's
possible. Set it to nil if you have no way to use HTTPS in your
environment, otherwise it is strongly recommended to let it set to t."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-elpa-timeout 5
  "Maximum allowed time in seconds to contact an ELPA repository."
  'integer
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-use-spacelpa nil
  "If non-nil then Spacelpa repository is the primary source to install
a locked version of packages. If nil then Spacemacs will install the latest
version of packages from MELPA. Spacelpa is currently in experimental
state and should only be used for testing."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-verify-spacelpa-archives nil
  "If non-nil then verify the signature for downloaded Spacelpa archives."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-elpa-subdirectory 'emacs-version
  "If non-nil, a form that evaluates to a package directory. For
example, to use different package directories for different Emacs
versions, set this to `emacs-version'."
  'sexp
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-configuration-layer-path '()
  "List of additional paths where to look for configuration layers.
Paths must have a trailing slash (ie. `~/.mycontribs/')"
  '(repeat string)
  'spacemacs-dotspacemacs-layers)

(spacemacs|defc dotspacemacs-install-packages 'used-only
  "Defines the behaviour of Spacemacs when installing packages.
Possible values are `used-only', `used-but-keep-unused' and `all'. `used-only'
installs only explicitly used packages and deletes any unused packages as well
as their unused dependencies. `used-but-keep-unused' installs only the used
packages but won't delete unused ones. `all' installs *all*
packages supported by Spacemacs and never uninstalls them."
  '(choice (const used-only) (const used-but-keep-unused) (const all))
  'spacemacs-dotspacemacs-layers)

(spacemacs|defc dotspacemacs-enable-lazy-installation 'unused
  "Lazy installation of layers (i.e. layers are installed only when a file
with a supported type is opened). Possible values are `all', `unused' and `nil'.
`unused' will lazy install only unused layers (i.e. layers not listed in
variable `dotspacemacs-configuration-layers'), `all' will lazy install any layer
that support lazy installation even the layers listed in
`dotspacemacs-configuration-layers'. `nil' disable the lazy installation feature
and you have to explicitly list a layer in the variable
`dotspacemacs-configuration-layers' to install it."
  '(choice (const all) (const unused) (const nil))
  'spacemacs-dotspacemacs-layers)

(spacemacs|defc dotspacemacs-ask-for-lazy-installation t
  "If non-nil then Spacemacs will ask for confirmation before installing
a layer lazily."
  'boolean
  'spacemacs-dotspacemacs-layers)

(spacemacs|defc dotspacemacs-additional-packages '()
  "List of additional packages that will be installed wihout being
wrapped in a layer. If you need some configuration for these
packages then consider to create a layer, you can also put the
configuration in `dotspacemacs/user-config'."
  '(repeat (choice symbol (cons symbol sexp)))
  'spacemacs-dotspacemacs-layers)

(defvar dotspacemacs--additional-theme-packages '()
  "Same as `dotspacemacs-additional-packages' but reserved for themes declared
in `dotspacemacs-themes'.")

(spacemacs|defc dotspacemacs-editing-style 'vim
  "One of `vim', `emacs' or `hybrid'.
`hybrid' is like `vim' except that `insert state' is replaced by the
`hybrid state' with `emacs' key bindings. The value can also be a list
 with `:variables' keyword (similar to layers). Check the editing styles
 section of the documentation for details on available variables."
  '(choice (const vim) (cons symbol sexp)
           (const emacs) (cons symbol sexp)
           (const hybrid) (cons symbol sexp))
  'spacemacs-dotspacemacs-init)

(defun spacemacs//support-evilified-buffer-p ()
  "Returns non-nil if buffers should use evilified states."
  (or (eq dotspacemacs-editing-style 'vim)
      (and (eq dotspacemacs-editing-style 'hybrid)
           hybrid-style-enable-evilified-state)))

(defun spacemacs//support-hjkl-navigation-p ()
  "Returns non-nil if navigation keys should be evilified."
  (or (eq dotspacemacs-editing-style 'vim)
      (and (eq dotspacemacs-editing-style 'hybrid)
           hybrid-style-enable-hjkl-bindings)))

(spacemacs|defc dotspacemacs-startup-banner 'official
  "Specify the startup banner. Default value is `official', it displays
the official spacemacs logo. An integer value is the index of text
banner, `random' chooses a random text banner in `core/banners'
directory. A string value must be a path to a .PNG file.
If the value is nil then no banner is displayed."
  '(choice (const official) (const random) (const nil) string integer)
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-startup-banner-scale 'auto
  "Specify the scale value for the startup banner. Default value is `auto',
it displays the spacemacs logo with the scale value. A (0, 1] float value
will be applied to scale the banner."
  '(choice (const auto) (const nil) number)
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-startup-buffer-show-version t
  "If true, show Spacemacs and Emacs version at the top right of the
Spacemacs buffer."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-startup-buffer-show-icons t
  "If true, show file icons for entries and headings on spacemacs buffer.
This has no effect in terminal or if \"all-the-icons\" is not installed."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-scratch-mode 'text-mode
  "Default major mode of the scratch buffer."
  'symbol
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-initial-scratch-message 'nil
  "Initial message in the scratch buffer."
  '(choice (const nil) string)
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-check-for-update nil
  "If non nil then spacemacs will check for updates at startup
when the current branch is not `develop'. Note that checking for
new versions works via git commands, thus it calls GitHub services
whenever you start Emacs."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-configuration-layers '(emacs-lisp)
  "List of configuration layers to load."
  '(repeat (choice symbol (cons symbol sexp)))
  'spacemacs-dotspacemacs-layers)

(defvar dotspacemacs--configuration-layers-saved nil
  "Saved value of `dotspacemacs-configuration-layers' after sync.")

(spacemacs|defc dotspacemacs-themes '(spacemacs-dark
                                      spacemacs-light)
  "List of themes, the first of the list is loaded when spacemacs starts.
Press `SPC T n' to cycle to the next theme in the list (works great
with 2 themes variants, one dark and one light"
  '(repeat (choice symbol (cons symbol sexp)))
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-mode-line-theme '(spacemacs
                                               :separator wave
                                               :separator-scale 1.5)
  "Set the theme for the Spaceline. Supported themes are `spacemacs',
`all-the-icons', `custom', `doom',`vim-powerline' and `vanilla'. The first three
are spaceline themes. `doom' is the doom-emacs mode-line. `vanilla' is default
Emacs mode-line. `custom' is a user defined themes, refer to the
DOCUMENTATION.org for more info on how to create your own spaceline theme. Value
can be a symbol or a list with additional properties like '(all-the-icons
:separator-scale 1.5)."
  '(choice (const spacemacs)
           (const all-the-icons)
           (const custom)
           (const doom)
           (const vim-powerline)
           (const vanilla)

           (cons (choice (const spacemacs)
                         (const all-the-icons)
                         (const custom)
                         (const doom)
                         (const vim-powerline)
                         (const vanilla))
                 sexp))
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-frame-title-format "%I@%S"
  "Default format string for a frame title bar, using the
original format spec, and additional customizations.

If nil then Spacemacs uses default `frame-title-format' instead of
calculating the frame title by `spacemacs/title-prepare' all the time.
This can help to avoid performance issues."
  '(choice (const nil) string)
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-icon-title-format nil
  "Default format string for a icon title bar, using the
original format spec, and additional customizations."
  '(choice (const nil) string)
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-colorize-cursor-according-to-state t
  "If non nil the cursor color matches the state color in GUI Emacs."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-leader-key "SPC"
  "The leader key."
  'string
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'"
  'string
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-major-mode-leader-key ","
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`. Set it to `nil` to disable it."
  '(choice (const nil) string)
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-major-mode-emacs-leader-key
  (if window-system "<M-return>" "C-M-m")
  "Major mode leader key accessible in `emacs state' and `insert state'"
  'string
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-ex-command-key ":"
  "The key used for Vim Ex commands."
  'string
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-command-key "SPC"
  "The key used for Emacs commands (M-x) (after pressing on the leader key)."
  'string
  'spacemacs-dotspacemacs-init)
(defvaralias 'dotspacemacs-emacs-command-key 'dotspacemacs-command-key
  "New official name for `dotspacemacs-command-key'")

(spacemacs|defc dotspacemacs-distinguish-gui-tab nil
  "If non nil, distinguish C-i and tab in the GUI version of Emacs."
  'boolean
  'spacemacs-dotspacemacs-init)

;; (defvar dotspacemacs-distinguish-gui-ret nil
;;   "If non nil, distinguish C-m and return in the GUI version of
;; emacs.")

(spacemacs|defc dotspacemacs-default-font '("Source Code Pro"
                                            :size 10.0
                                            :weight normal
                                            :width normal)
  "Default font or prioritized list of fonts. This setting has no effect when
running Emacs in terminal. The font set here will be used for default and
fixed-pitch faces. The `:size' can be specified as
a non-negative integer (pixel size), or a floating-point (point size).
Point size is recommended, because it's device independent. (default 10.0)"
  '(choice (cons string sexp)
           (repeat (cons string sexp)))
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-folding-method 'evil
  "Code folding method. Possible values are `evil', `origami' and `vimish'."
  '(choice (const evil) (const origami) (const vimish))
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-undo-system 'undo-fu
  "The backend used for undo/redo functionality. Possible values are
`undo-fu', `undo-redo' and `undo-tree' see also `evil-undo-system'.
Note that saved undo history does not get transferred when changing
your undo system. The default is currently `undo-fu' as `undo-tree'
is not maintained anymore and `undo-redo' is very basic."
  '(choice (const undo-fu) (const undo-redo) (const undo-tree))
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-default-layout-name "Default"
  "Name of the default layout."
  'string
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-display-default-layout nil
  "If non nil the default layout name is displayed in the mode-line."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-auto-resume-layouts nil
  "If non nil then the last auto saved layouts are resume automatically upon
start."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-auto-generate-layout-names nil
  "If non-nil, auto-generate layout name when creating new layouts.
Only has effect when using the \"jump to layout by number\" commands."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-max-rollback-slots 5
  "Maximum number of rollback slots to keep in the cache."
  'integer
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-large-file-size 1
  "Size (in MB) above which spacemacs will prompt to open the large file
literally to avoid performance issues. Opening a file literally means that
no major mode or minor modes are active."
  'integer
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-auto-save-file-location 'cache
  "Location where to auto-save files. Possible values are `original' to
auto-save the file in-place, `cache' to auto-save the file to another
file stored in the cache directory and `nil' to disable auto-saving."
  '(choice (const cache) (const original) (const nil))
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-enable-paste-transient-state nil
  "If non-nil, the paste transient-state is enabled. While enabled, after you
paste something, pressing `C-j' and `C-k' several times cycles through the
elements in the `kill-ring'."
  'boolean
  'spacemacs-dotspacemacs-init)
(defvaralias
  'dotspacemacs-enable-paste-micro-state
  'dotspacemacs-enable-paste-transient-state
  "Old name of `dotspacemacs-enable-paste-transient-state'.")

(spacemacs|defc dotspacemacs-which-key-delay 0.4
  "Delay in seconds starting from the last keystroke after which
the which-key buffer will be shown if you have not completed a
key sequence. Setting this variable is equivalent to setting
`which-key-idle-delay'."
  'number
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-which-key-position 'bottom
  "Which-key frame position. Possible values are `right', `bottom' and
`right-then-bottom'. right-then-bottom tries to display the frame to the
right; if there is insufficient space it displays it at the bottom.
It is also possible to use a posframe with the following cons cell
`(posframe . position)' where position can be one of `center',
`top-center', `bottom-center', `top-left-corner', `top-right-corner',
`top-right-corner', `bottom-left-corner' or `bottom-right-corner'"
  '(choice (const right) (const bottom) (const right-then-bottom)
           (cons (const posframe)
                 (choice (const center)
                         (const top-center)
                         (const bottom-center)
                         (const top-left-corner)
                         (const top-right-corner)
                         (const bottom-left-corner)
                         (const bottom-right-corner))))
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-switch-to-buffer-prefers-purpose nil
  "Control where `switch-to-buffer' displays the buffer.
If nil, `switch-to-buffer' displays the buffer in the current
window even if another same-purpose window is available. If non
nil, `switch-to-buffer' displays the buffer in a same-purpose
window even if the buffer can be displayed in the current
window."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-maximize-window-keep-side-windows t
  "Whether side windows (such as those created by treemacs or neotree)
are kept or minimized by `spacemacs/toggle-maximize-window' (SPC w m)."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-loading-progress-bar t
  "If non nil a progress bar is displayed when spacemacs is loading. This
may increase the boot time on some systems and emacs builds, set it to nil
to boost the loading time."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-fullscreen-at-startup nil
  "If non nil the frame is fullscreen when Emacs starts up (Emacs 24.4+ only)."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-fullscreen-use-non-native nil
  "If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen. Use
to disable fullscreen animations on macOS."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-maximized-at-startup t
  "If non nil the frame is maximized when Emacs starts up (Emacs 24.4+ only).
Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-undecorated-at-startup nil
  "If non nil the frame is undecorated when Emacs starts up."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-active-transparency 90
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's active or selected. Transparency
can be toggled through `toggle-transparency'."
  'integer
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-inactive-transparency 90
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's inactive or deselected. Transparency
can be toggled through `toggle-transparency'."
  'integer
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-background-transparency 90
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame background when it's active or selected. Transparency
can be toggled through `toggle-background-transparency'."
  'integer
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-show-transient-state-title t
  "If non nil show the titles of transient states."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-show-transient-state-color-guide t
  "If non nil show the color guide hint for transient state keys."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-mode-line-unicode-symbols t
  "If non nil unicode symbols are displayed in the mode-line (eg. for lighters).
If you use Emacs as a daemon and wants unicode characters only in GUI set
the value to quoted `display-graphic-p'. (default t)"
  '(choice boolean (const display-graphic-p))
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-smooth-scrolling t
  "If non nil smooth scrolling (native-scrolling) is enabled.
Smooth scrolling overrides the default behavior of Emacs which
recenters point when it reaches the top or bottom of the
screen."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-scroll-bar-while-scrolling t
  "Show the scroll bar while scrolling. The auto hide time can be configured by
setting this variable to a number."
  '(choice boolean number)
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-line-numbers nil
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
restricts line-number to the specified list of major-mode."
  '(choice boolean
           (const relative)
           (const visual)
           (const prog-mode)
           (repeat sexp))
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-enable-server nil
  "If non-nil, start an Emacs server if one is not already running."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-persistent-server nil
  "If non nil advises quit functions to keep server open when quitting."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-server-socket-dir nil
  "Set the emacs server socket location.
If nil, uses whatever the Emacs default is,
otherwise a directory path like \"~/.emacs.d/server\".
Has no effect if `dotspacemacs-enable-server' is nil."
  '(choice (const nil) string)
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-smartparens-strict-mode nil
  "If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
smartparens-strict-mode will be enabled in programming modes."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-activate-smartparens-mode t
  "If non-nil smartparens-mode will be enabled in programming modes."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-smart-closing-parenthesis nil
  "If non-nil pressing the closing parenthesis `)' key in insert mode passes
over any automatically added closing parenthesis, bracket, quote, etc...
This can be temporary disabled by pressing `C-q' before `)'. (default nil)"
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-zone-out-when-idle nil
  "Either nil or a number of seconds.
If non-nil zone out after the specified number of seconds."
  '(choice (const nil) integer)
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-highlight-delimiters 'all
  "Select a scope to highlight delimiters.
Possible values are `any', `current', `all' or `nil'.
Default is `all' (highlight any scope and emphasize the current one."
  '(choice (const all) (const any) (const current) (const nil))
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-show-trailing-whitespace t
  "Show trailing whitespace. Default is `t'."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-whitespace-cleanup nil
  "Delete whitespace while saving buffer.

Possible values are:
`all' to aggressively delete empty lines and long sequences of whitespace,
`trailing' to delete only the whitespace at end of lines,
`changed' to delete only whitespace for changed lines or
`nil' to disable cleanup."
  '(choice (const nil) (const all) (const trailing) (const changed))
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
  "List of search tool executable names. Spacemacs uses the first installed
tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'."
  '(set (const "rg") (const "ag") (const "pt") (const "ack") (const "grep"))
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-startup-lists '((recents  . 5)
                                             (projects . 7))
  "Association list of items to show in the startup buffer of the form
`(list-type . list-size)`. If nil it is disabled.

Possible values for list-type are:
`recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
List sizes may be nil, in which case
`spacemacs--buffer-startup-lists-length' takes effect.
In the `recents-by-project' case, the list size should be a `cons' cell whose
`car' is the maximum number of projects to show, and whose `cdr' is the maximum
number of recent files to show in each project."
  '(choice (alist :key-type (choice (const recents)
                                    (const recents-by-project)
                                    (const bookmarks)
                                    (const projects)
                                    (const agenda)
                                    (const todos))
                  :value-type (choice integer
                                      (const nil)
                                      ;; for `recents-by-project':
                                      (cons integer integer)))
           (const nil))
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-startup-buffer-responsive t
  "True if the home buffer should respond to resize events."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-show-startup-list-numbers t
  "Show numbers before the startup list lines."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-startup-buffer-multi-digit-delay 0.4
  "The minimum delay in seconds between number key presses."
  'number
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-excluded-packages '()
  "A list of packages that will not be installed and loaded."
  '(repeat symbol)
  'spacemacs-dotspacemacs-layers)

(spacemacs|defc dotspacemacs-frozen-packages '()
  "A list of packages that cannot be updated."
  '(repeat symbol)
  'spacemacs-dotspacemacs-layers)

(spacemacs|defc dotspacemacs-pretty-docs nil
  "Run `spacemacs/prettify-org-buffer' when
visiting README.org files of Spacemacs."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-new-empty-buffer-major-mode nil
  "Set the major mode for a new empty buffer."
  'symbol
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-use-clean-aindent-mode t
  "Correct indentation for simple modes.

If non nil activate `clean-aindent-mode' which tries to correct
virtual indentation of simple modes. This can interfere with mode specific
indent handling like has been reported for `go-mode'.
If it does deactivate it here. (default t)"
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-use-SPC-as-y nil
  "Accept SPC as y for prompts if non nil. (default nil)"
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-swap-number-row nil
  "Shift number row for easier symbol access.

If non-nil shift your number row to match the entered keyboard layout
(only in insert mode). Currently the keyboard layouts
(qwerty-us qwertz-de qwerty-ca-fr) are supported.
New layouts can be added in the `spacemacs-editing' layer.
(default nil)"
  '(choice (const qwerty-us) (const qwertz-de) (const qwerty-ca-fr) (const nil))
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-home-shorten-agenda-source nil
  "If nil the home buffer shows the full path of agenda items
and todos. If non nil only the file name is shown."
  'boolean
  'spacemacs-dotspacemacs-init)

(defvar dotspacemacs--pretty-ignore-subdirs
  '(".cache/junk")
  "Subdirectories of `spacemacs-start-directory' to ignore when
prettifying Org files.")

(spacemacs|defc dotspacemacs-scratch-buffer-persistent nil
  "If non-nil, *scratch* buffer will be persistent. Things you write down in
   *scratch* buffer will be saved automatically."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-scratch-buffer-unkillable nil
  "If non-nil, `kill-buffer' on *scratch* buffer
will bury it instead of killing."
  'boolean
  'spacemacs-dotspacemacs-init)

(spacemacs|defc dotspacemacs-byte-compile nil
  "If non-nil, byte-compile some of Spacemacs files."
  'boolean
  'spacemacs-dotspacemacs-init)

(defun dotspacemacs//prettify-spacemacs-docs ()
  "Run `spacemacs/prettify-org-buffer' if `buffer-file-name'
looks like Spacemacs documentation."
  (when (and dotspacemacs-pretty-docs
             spacemacs-start-directory
             buffer-file-name)
    (let ((start-dir (expand-file-name spacemacs-start-directory))
          (buf-path (expand-file-name buffer-file-name)))
      (when (and (string-prefix-p start-dir buf-path)
                 (not (--any? (string-prefix-p (expand-file-name it start-dir) buf-path)
                              dotspacemacs--pretty-ignore-subdirs)))
        (spacemacs/prettify-org-buffer)))))

;; only for backward compatibility
(defalias 'dotspacemacs-mode 'emacs-lisp-mode)

(defmacro dotspacemacs|call-func (func &optional msg)
  "Call the function from the dotfile only if it is bound.
If MSG is not nil then display a message in `*Messages*'. Errors
are caught and signaled to user in spacemacs buffer."
  `(progn
     (when ,msg (spacemacs-buffer/message ,msg))
     (when (fboundp ',func)
       (condition-case-unless-debug err
           (,func)
         (error
          (configuration-layer//increment-error-count)
          (spacemacs-buffer/append (format "Error in %s: %s\n"
                                           ',(symbol-name func)
                                           (error-message-string err))
                                   t))))))

(defun dotspacemacs/call-user-env ()
  "Call the function `dotspacemacs/user-env'."
  (interactive)
  (dotspacemacs|call-func dotspacemacs/user-env "Calling dotfile user env..."))

(defun dotspacemacs/go-to-user-env ()
  "Go to the `dotspacemacs/user-env' function."
  (interactive)
  (find-function 'dotspacemacs/user-env))

(defun dotspacemacs//check-layers-changed ()
  "Check if the value of `dotspacemacs-configuration-layers'
changed, and issue a warning if it did."
  (unless (eq dotspacemacs-configuration-layers
              dotspacemacs--configuration-layers-saved)
    (spacemacs-buffer/warning (concat
                               "`dotspacemacs-configuration-layers' was "
                               "changed outside of `dotspacemacs/layers'."))))
(add-hook 'spacemacs-post-user-config-hook
          'dotspacemacs//check-layers-changed)

(defun dotspacemacs//read-editing-style-config (config)
  "Read editing style CONFIG: apply variables and return the editing style.
CONFIG can be the symbol of an editing style or a list where the car is
the symbol of an editing style and the cdr is a list of keyword arguments like
  `:variables'."
  (cond
   ((symbolp config) config)
   ((listp config)
    (let ((variables (spacemacs/mplist-get-values config :variables)))
      (while variables
        (let ((var (pop variables)))
          (if (consp variables)
              (condition-case-unless-debug err
                  (set-default var (eval (pop variables)))
                ('error
                 (spacemacs-buffer/append
                  (format (concat "\nAn error occurred while reading the "
                                  "editing style variable %s "
                                  "(error: %s). Be sure to quote the value "
                                  "if needed.\n")
                          var err))))
            (spacemacs-buffer/warning "Missing value for variable %s !" var)))))
    (car config))))

(defun dotspacemacs/add-layer (layer-name)
  "Add LAYER_NAME to dotfile and reload the it.
Returns non nil if the layer has been effectively inserted."
  (unless (configuration-layer/layer-used-p layer-name)
    (with-current-buffer (find-file-noselect (dotspacemacs/location))
      (goto-char (point-min))
      (let ((insert-point
             (re-search-forward
              "[^`]dotspacemacs-configuration-layers\\s-*\n?[^(]*\\((\\)")))
        (insert (format "%S" layer-name))
        (unless (equal (point) (point-at-eol))
          (insert "\n"))
        (indent-region insert-point (min (point-max)
                                         (+ insert-point 2
                                            (length (symbol-name layer-name)))))
        (save-buffer)))
    (load-file (dotspacemacs/location))
    t))

(defun dotspacemacs//profile-user-config (f &rest args)
  "Compute time taken by the `dotspacemacs/user-config' function."
  (let ((stime (current-time)))
    (apply f args)
    (setq dotspacemacs--user-config-elapsed-time
          (float-time (time-subtract (current-time) stime)))))

(defun dotspacemacs/sync-configuration-layers (&optional arg)
  "Synchronize declared layers in dotfile with spacemacs.

Called with `C-u' skips `dotspacemacs/user-config'.
Called with `C-u C-u' skips `dotspacemacs/user-config' _and_ preliminary tests."
  (interactive "P")
  (when (file-exists-p dotspacemacs-filepath)
    (with-current-buffer (find-file-noselect dotspacemacs-filepath)
      (let ((dotspacemacs-loading-progress-bar nil))
        (setq spacemacs-loading-string "")
        (save-buffer)
        (let ((tests-ok (or (equal arg '(16)) (dotspacemacs/test-dotfile t))))
          (if tests-ok
              (progn
                (load-file buffer-file-name)
                (dotspacemacs|call-func dotspacemacs/init
                                        "Calling dotfile init...")
                (dotspacemacs|call-func dotspacemacs/user-init
                                        "Calling dotfile user init...")
                (setq dotspacemacs-editing-style
                      (dotspacemacs//read-editing-style-config
                       dotspacemacs-editing-style))
                (dotspacemacs/call-user-env)
                ;; try to force a redump when reloading the configuration
                (let ((spacemacs-force-dump t))
                  (configuration-layer/load))
                (if (member arg '((4) (16)))
                    (message (concat "Done (`dotspacemacs/user-config' "
                                     "function has been skipped)."))
                  (dotspacemacs|call-func dotspacemacs/user-config
                                          "Calling dotfile user config...")
                  (run-hooks 'spacemacs-post-user-config-hook)
                  (message "Done.")))
            (switch-to-buffer-other-window dotspacemacs-test-results-buffer)
            (spacemacs-buffer/warning "Some tests failed, check `%s' buffer"
                                      dotspacemacs-test-results-buffer))))))
  (when (configuration-layer/package-used-p 'spaceline)
    (spacemacs//restore-buffers-powerline)))

(eval-and-compile
  (defun dotspacemacs/get-variable-string-list ()
    "Return a list of all the dotspacemacs variables as strings."
    (all-completions "dotspacemacs" obarray
                     (lambda (x)
                       (and (boundp x)
                            ;; avoid private variables to show up
                            (not (string-match-p "--" (symbol-name x)))))))

  (defun dotspacemacs/get-variable-list ()
    "Return a list of all dotspacemacs variable symbols."
    (mapcar 'intern (dotspacemacs/get-variable-string-list))))

(defmacro dotspacemacs|symbol-value (symbol)
  "Return the value of SYMBOL corresponding to a dotspacemacs variable.
If SYMBOL value is `display-graphic-p' then return the result of
`(display-graphic-p)', otherwise return the value of the symbol."
  `(if (eq 'display-graphic-p ,symbol) (display-graphic-p) ,symbol))

(defun dotspacemacs/location ()
  "Return the absolute path to the spacemacs dotfile."
  dotspacemacs-filepath)

(defun dotspacemacs/copy-template ()
  "Copy `.spacemacs.template' in home directory. Ask for confirmation
before copying the file if the destination already exists."
  (interactive)
  (let* ((copy? (if (file-exists-p dotspacemacs-filepath)
                    (y-or-n-p
                     (format "%s already exists. Do you want to overwrite it ? "
                             dotspacemacs-filepath)) t)))
    (when copy?
      (copy-file (concat dotspacemacs-template-directory ".spacemacs.template")
                 dotspacemacs-filepath t)
      (message "%s has been installed." dotspacemacs-filepath))))

(defun dotspacemacs//ido-completing-read (prompt candidates)
  "Call `ido-completing-read' with a CANDIDATES alist where the key is
a display strng and the value is the actual value to return."
  (let ((ido-max-window-height (1+ (length candidates))))
    (cadr (assoc (ido-completing-read prompt (mapcar 'car candidates))
                 candidates))))

(defun dotspacemacs/maybe-install-dotfile ()
  "Install the dotfile if it does not exist."
  (unless (file-exists-p dotspacemacs-filepath)
    (spacemacs-buffer/set-mode-line "Dotfile wizard installer" t)
    (when (dotspacemacs/install 'with-wizard)
      (configuration-layer/load))))

(defun dotspacemacs/install (arg)
  "Install the dotfile, return non nil if the dotfile has been installed.

If ARG is non nil then ask questions to the user before installing the dotfile."
  (interactive "P")
  ;; preferences is an alist where the key is the text to replace by
  ;; the value in the dotfile
  (let ((preferences
         (when arg
           ;; editing style
           `(("dotspacemacs-editing-style 'vim"
              ,(format
                "dotspacemacs-editing-style '%S"
                (dotspacemacs//ido-completing-read
                 "What is your preferred editing style? "
                 '(("Among the stars aboard the Evil flagship (vim)"
                    vim)
                   ("On the planet Emacs in the Holy control tower (emacs)"
                    emacs)))))
             ("dotspacemacs-distribution 'spacemacs"
              ,(format
                "dotspacemacs-distribution '%S"
                (dotspacemacs//ido-completing-read
                 "What distribution of spacemacs would you like to start with? "
                 `(("The standard distribution, recommended (spacemacs)"
                    spacemacs)
                   (,(concat "A minimalist distribution that you can build on "
                             "(spacemacs-base)")
                    spacemacs-base)))))))))
    (with-current-buffer (find-file-noselect
                          (concat dotspacemacs-template-directory
                                  ".spacemacs.template"))
      (dolist (p preferences)
        (goto-char (point-min))
        (re-search-forward (car p))
        (replace-match (cadr p)))
      (let ((install
             (if (file-exists-p dotspacemacs-filepath)
                 (y-or-n-p
                  (format "%s already exists. Do you want to overwrite it ? "
                          dotspacemacs-filepath)) t)))
        (when install
          (write-file dotspacemacs-filepath)
          (message "%s has been installed." dotspacemacs-filepath)
          t))))
  (dotspacemacs/load-file)
  ;; force new wizard values to be applied
  (dotspacemacs/init))

(defun dotspacemacs/load-file ()
  "Load ~/.spacemacs if it exists."
  (let ((dotspacemacs (dotspacemacs/location)))
    (if (file-exists-p dotspacemacs)
        (unless (with-demoted-errors "Error loading .spacemacs: %S"
                  (load dotspacemacs))
          (dotspacemacs/safe-load))))
  (advice-add 'dotspacemacs/layers :after
              'spacemacs-customization//validate-dotspacemacs-layers-vars)
  (advice-add 'dotspacemacs/init :after
              'spacemacs-customization//validate-dotspacemacs-init-vars)
  (advice-add 'dotspacemacs/user-config :around
              'dotspacemacs//profile-user-config))

(defun spacemacs/title-prepare (title-format)
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
  ;; save-match-data to work around Emacs bug, see
  ;; https://github.com/syl20bnr/spacemacs/issues/9700
  (save-match-data
    ;; disable buffer-list-update-hook to work around recursive invocations
    ;; caused by the temp-buffer used by `format-spec' below, see
    ;; https://github.com/syl20bnr/spacemacs/issues/12387
    (let* ((buffer-list-update-hook nil)
           (project-name
            (when (string-match-p "%t" title-format)
              (if (boundp 'spacemacs--buffer-project-name)
                  spacemacs--buffer-project-name
                (setq-local spacemacs--buffer-project-name
                            (if (fboundp 'projectile-project-name)
                                (projectile-project-name)
                              "-")))))
           (abbreviated-file-name
            (when (string-match-p "%a" title-format)
              (if (boundp 'spacemacs--buffer-abbreviated-filename)
                  spacemacs--buffer-abbreviated-filename
                (setq-local spacemacs--buffer-abbreviated-filename
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
                ?- "%-")))

      (format-spec title-format fs))))

(defun dotspacemacs/safe-load ()
  "Error recovery from malformed .spacemacs.
Loads default .spacemacs template and suspends pruning of orphan packages.
Informs users of error and prompts for default editing style for use during
error recovery."
  (load (concat dotspacemacs-template-directory
                ".spacemacs.template"))
  (define-advice dotspacemacs/layers (:after (&rest _) error-recover-preserve-packages)
    (setq-default dotspacemacs-install-packages 'used-but-keep-unused)
    (advice-remove 'dotspacemacs/layers #'dotspacemacs/layers@error-recover-preserve-packages))
  (define-advice dotspacemacs/init (:after (&rest _) error-recover-prompt-for-style)
    (setq-default dotspacemacs-editing-style
                  (intern
                   (ido-completing-read
                    (format
                     (concat
                      "Spacemacs encountered an error while "
                      "loading your `%s' file.\n"
                      "Pick your editing style for recovery "
                      "(use left and right arrows): ")
                     dotspacemacs-filepath)
                    '(("vim" vim)
                      ("emacs" emacs)
                      ("hybrid" hybrid))
                    nil t nil nil 'vim)))
    (advice-remove 'dotspacemacs/init #'dotspacemacs/init@error-recover-prompt-for-style)))

(defun dotspacemacs//test-dotspacemacs/layers ()
  "Tests for `dotspacemacs/layers'"
  (insert
   (format (concat "\n* Testing settings in dotspacemacs/layers "
                   "[[file:%s::dotspacemacs/layers][Show in File]]\n")
           dotspacemacs-filepath))
  ;; protect global values of these variables
  (let (dotspacemacs-additional-packages
        dotspacemacs-configuration-layer-path
        dotspacemacs-configuration-layers
        dotspacemacs-excluded-packages
        dotspacemacs-install-packages
        (passed-tests 0)
        (total-tests 0))
    (load dotspacemacs-filepath)
    (dotspacemacs/layers)
    (spacemacs//test-list 'stringp
                          'dotspacemacs-configuration-layer-path
                          "is a string" "path")
    (spacemacs//test-list 'file-directory-p
                          'dotspacemacs-configuration-layer-path
                          "exists in filesystem" "path")
    (setq dotspacemacs-configuration-layers
          (mapcar (lambda (l) (if (listp l) (car l) l))
                  (remove nil dotspacemacs-configuration-layers)))
    (spacemacs//test-list 'configuration-layer/get-layer-path
                          'dotspacemacs-configuration-layers
                          "can be found" "layer")
    (insert (format
             (concat "** RESULTS: "
                     "[[file:%s::dotspacemacs/layers][dotspacemacs/layers]] "
                     "passed %s out of %s tests\n")
             dotspacemacs-filepath passed-tests total-tests))
    (equal passed-tests total-tests)))

(defmacro dotspacemacs||let-init-test (&rest body)
  "Macro to protect dotspacemacs variables"
  `(let ((fpath dotspacemacs-filepath)
         ,@(mapcar (lambda (symbol)
                     `(,symbol ,(let ((v (symbol-value symbol)))
                                  (if (or (symbolp v) (listp v))
                                      `',v v))))
                   (dotspacemacs/get-variable-list))
         (passed-tests 0) (total-tests 0))
     (setq dotspacemacs-filepath fpath)
     (load dotspacemacs-filepath)
     ,@body))

(defun dotspacemacs//test-dotspacemacs/init ()
  "Tests for `dotspacemacs/init'"
  (insert
   (format (concat "\n* Testing settings in dotspacemacs/init "
                   "[[file:%s::dotspacemacs/init][Show in File]]\n")
           dotspacemacs-filepath))
  (dotspacemacs||let-init-test
   (dotspacemacs/init)
   (spacemacs//test-var
    (lambda (x)
      (or (member x '(vim
                      emacs
                      hybrid))
          (and (listp x)
               (member (car x) '(vim emacs hybrid))
               (spacemacs/mplist-get-values x :variables))))
    'dotspacemacs-editing-style
    "is \'vim, \'emacs or \'hybrid or and list with `:variables' keyword")
   (spacemacs//test-var
    (lambda (x)
      (let ((themes '(spacemacs
                      all-the-icons
                      custom
                      doom
                      vim-powerline
                      vanilla)))
        (or (member x themes)
            (and (listp x)
                 (memq (car x) themes)
                 ;; TODO define a function to remove several properties at once
                 (null (spacemacs/mplist-remove
                        (spacemacs/mplist-remove (cdr x) :separator)
                        :separator-scale))))))
    'dotspacemacs-mode-line-theme
    (concat
     "is \'spacemacs, \'all-the-icons, \'custom, \'vim-powerline or 'vanilla "
     "or a list with `car' one of the previous values and properties one of "
     "the following: `:separator' or `:separator-scale'"))
   (spacemacs//test-var
    (lambda (x) (member x '(original cache nil)))
    'dotspacemacs-auto-save-file-location (concat "is one of \'original, "
                                                  "\'cache or nil"))
   (spacemacs//test-var
    (lambda (x) (member x '(all any current nil)))
    'dotspacemacs-highlight-delimiters
    "is one of \'all, \'any, \'current or nil")
   (spacemacs//test-list
    (lambda (x)
      (let ((el (or (car-safe x) x)))
        (member el '(recents recents-by-project bookmarks projects todos agenda))))
    'dotspacemacs-startup-lists (concat "includes \'recents, 'recents-by-project, "
                                        "\'bookmarks, \'todos, "
                                        "\'agenda or \'projects"))
   (spacemacs//test-list
    (lambda (x)
      (let ((el (or (car-safe x) x))
            (list-size (cdr-safe x)))
        (if (eq el 'recents-by-project)
            (and (consp list-size)
                 (numberp (car list-size))
                 (numberp (cdr list-size)))
          (or (null list-size) (numberp list-size)))))
    'dotspacemacs-startup-lists (concat "list size is a number, unless "
                                        "list type is recents-by-project "
                                        "when it is a pair of numbers"))
   (spacemacs//test-var 'stringp 'dotspacemacs-leader-key "is a string")
   (spacemacs//test-var 'stringp 'dotspacemacs-emacs-leader-key "is a string")
   (spacemacs//test-var
    (lambda (x) (or (null x) (stringp x)))
    'dotspacemacs-major-mode-leader-key "is a string or nil")
   (spacemacs//test-var
    (lambda (x) (or (null x) (stringp x)))
    'dotspacemacs-major-mode-emacs-leader-key "is a string or nil")
   (spacemacs//test-var 'stringp 'dotspacemacs-emacs-command-key "is a string")
   (insert (format
            (concat "** RESULTS: "
                    "[[file:%s::dotspacemacs/init][dotspacemacs/init]] "
                    "passed %s out of %s tests\n")
            dotspacemacs-filepath passed-tests total-tests))
   (equal passed-tests total-tests)))

(defun dotspacemacs/test-dotfile (&optional hide-buffer)
  "Test settings in dotfile for correctness.
Return non-nil if all the tests passed."
  (interactive)
  (configuration-layer/discover-layers 'refresh-index)
  (let ((min-version "0.0"))
    ;; dotspacemacs-version not implemented yet
    ;; (if (version< dotspacemacs-version min-version)
    (if nil
        (error (format (concat "error: dotspacemacs/test-dotfile requires "
                               "dotspacemacs-version %s")
                       min-version))
      (with-current-buffer (get-buffer-create dotspacemacs-test-results-buffer)
        (unless hide-buffer
          (switch-to-buffer-other-window dotspacemacs-test-results-buffer))
        (org-mode)
        (org-indent-mode)
        (view-mode)
        (when (bound-and-true-p flyspell-mode)
          (flyspell-mode -1))
        (let (buffer-read-only)
          (erase-buffer)
          (insert (format "* Running tests on [[file:%s][%s]] (v%s)\n"
                          dotspacemacs-filepath dotspacemacs-filepath "0.0"))
          ;; dotspacemacs-version not implemented yet
          ;; (insert (format "* Running tests on %s (v%s)\n"
          ;;                 dotspacemacs-filepath dotspacemacs-version))
          (prog1
              ;; execute all tests no matter what
              (cl-reduce (lambda (x y)
                           (and (funcall y) x))
                         '(dotspacemacs//test-dotspacemacs/layers
                           dotspacemacs//test-dotspacemacs/init)
                         :initial-value t)
            (goto-char (point-min))))))))

(define-advice en/disable-command (:around (orig-f &rest args) write-to-dotspacemacs-instead)
  "Attempt to modify `dotspacemacs/user-config' rather than ~/.emacs.d/init.el."
  (let ((orig-f-called))
    (condition-case-unless-debug e
        (let* ((location (find-function-noselect 'dotspacemacs/user-config 'lisp-only))
               (buffer (car location))
               (start (cdr location))
               (user-init-file (buffer-file-name buffer)))
          (with-current-buffer buffer
            (save-excursion
              (save-restriction
                ;; Set `user-init-file' and narrow the buffer visiting that
                ;; file, to trick en/disable-command into writing inside the
                ;; body of `dotspacemacs/user-config' instead of
                ;; ~/.emacs.d/init.el.
                (goto-char start)
                (forward-sexp)
                (backward-char)
                (narrow-to-region start (point))
                (setq orig-f-called t)
                (apply orig-f args)))))
      (error
       ;; If the error happened before we managed to call the advised function,
       ;; just allow the original function to run and modify ~/.emacs.d/init.el,
       ;; which is better than failing completely.
       (unless orig-f-called
         (apply orig-f args))))))

(provide 'core-dotspacemacs)
