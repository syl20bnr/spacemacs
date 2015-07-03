;;; core-dotspacemacs.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defconst dotspacemacs-template-directory
  (expand-file-name (concat spacemacs-core-directory "templates/"))
  "Templates directory.")

(defconst dotspacemacs-filepath
  (concat user-home-directory ".spacemacs")
  "Filepath to the installed dotfile.")

(defvar dotspacemacs-verbose-loading nil
  "If non nil output loading progess in `*Messages*' buffer.")

(defvar dotspacemacs-configuration-layer-path '()
  "List of additional paths where to look for configuration layers.
Paths must have a trailing slash (ie. `~/.mycontribs/')")

(defvar dotspacemacs-additional-packages '()
  "List of additional packages that will be installed wihout being
wrapped in a layer. If you need some configuration for these
packages then consider to create a layer, you can also put the
configuration in `dotspacemacs/config'.")

(defvar dotspacemacs-editing-style 'vim
  "Either `vim' or `emacs'. Evil is always enabled but if the variable
is `emacs' then the `holy-mode' is enabled at startup.")

(defvar dotspacemacs-startup-banner 'official
   "Specify the startup banner. Default value is `official', it displays
the official spacemacs logo. An integer value is the index of text
banner, `random' chooses a random text banner in `core/banners'
directory. A string value must be a path to a .PNG file.
If the value is nil then no banner is displayed.")

(defvar dotspacemacs-configuration-layers '()
  "List of configuration layers to load. If it is the symbol `all' instead
of a list then all discovered layers will be installed.")

(defvar dotspacemacs-themes '(solarized-light
                              solarized-dark
                              leuven
                              monokai
                              zenburn)
  "List of themes, the first of the list is loaded when spacemacs starts.
Press <SPC> T n to cycle to the next theme in the list (works great
with 2 themes variants, one dark and one light")

(defvar dotspacemacs-colorize-cursor-according-to-state t
  "If non nil the cursor color matches the state color.")

(defvar dotspacemacs-leader-key "SPC"
  "The leader key.")

(defvar dotspacemacs-emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'")

(defvar dotspacemacs-major-mode-leader-key ","
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`. Set it to `nil` to disable it.")

(defvar dotspacemacs-major-mode-emacs-leader-key "C-M-m"
  "Major mode leader key accessible in `emacs state' and `insert state'")

(defvar dotspacemacs-default-font '("Source Code Pro"
                                    :size 13
                                    :weight normal
                                    :width normal
                                    :powerline-scale 1.1)
  "Default font. `powerline-scale' allows to quickly tweak the mode-line
size to make separators look not too crappy.")

(defvar dotspacemacs-command-key ":"
  "The key used for Evil commands (ex-commands) and Emacs commands (M-x).
By default the command key is `:' so ex-commands are executed like in Vim
with `:' and Emacs commands are executed with `<leader> :'.")

(defvar dotspacemacs-use-ido nil
  "If non nil then `ido' replaces `helm' for some commands. For now only
`find-files' (SPC f f) is replaced.")

(defvar dotspacemacs-auto-save-file-location 'cache
  "Location where to auto-save files. Possible values are `original' to
auto-save the file in-place, `cache' to auto-save the file to another
file stored in the cache directory and `nil' to disable auto-saving.
Default value is `cache'.")

(defvar dotspacemacs-enable-paste-micro-state t
  "If non nil the paste micro-state is enabled. While enabled pressing `p`
several times cycle between the kill ring content.'")

(defvar dotspacemacs-guide-key-delay 0.4
  "Guide-key delay in seconds.")

(defvar dotspacemacs-loading-progress-bar t
  "If non nil a progress bar is displayed when spacemacs is loading. This
may increase the boot time on some systems and emacs builds, set it to nil
to boost the loading time.")

(defvar dotspacemacs-fullscreen-at-startup nil
  "If non nil the frame is fullscreen when Emacs starts up (Emacs 24.4+ only).")

(defvar dotspacemacs-fullscreen-use-non-native nil
  "If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen. Use
to disable fullscreen animations in OSX.")

(defvar dotspacemacs-maximized-at-startup nil
  "If non nil the frame is maximized when Emacs starts up (Emacs 24.4+ only).
Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.")

(defvar dotspacemacs-active-transparency 90
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's active or selected. Transparency
can be toggled through `toggle-transparency'.")

(defvar dotspacemacs-inactive-transparency 90
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's inactive or deselected. Transparency
can be toggled through `toggle-transparency'.")

(defvar dotspacemacs-mode-line-unicode-symbols t
  "If non nil unicode symbols are displayed in the mode-line (eg. for lighters)")

(defvar dotspacemacs-smooth-scrolling t
  "If non nil smooth scrolling (native-scrolling) is enabled. Smooth scrolling
overrides the default behavior of Emacs which recenters the point when
it reaches the top or bottom of the screen.")

(defvar dotspacemacs-persistent-server nil
  "If non nil advises quit functions to keep server open when quitting.")

(defvar dotspacemacs-smartparens-strict-mode nil
  "If non-nil smartparens-strict-mode will be enabled in programming modes.")

(defvar dotspacemacs-highlight-delimiters 'all
  "Select a scope to highlight delimiters. Possible value is `all', `current'
or `nil'. Default is `all'")

(defvar dotspacemacs-delete-orphan-packages t
  "If non-nil spacemacs will delete any orphan packages, i.e. packages that are
declared in a layer which is not a member of
 `dotspacemacs-configuration-layers'")

(defvar dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
  "List of search tool executable names. Spacemacs uses the first installed
tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.")

(defvar dotspacemacs-default-package-repository 'melpa-stable
  "The default package repository used if no explicit repository has been
specified with an installed package.
NOT USED FOR NOW :-)")

(defvar dotspacemacs-startup-lists '(recents projects)
  "List of items to show in the startup buffer. If nil it is disabled.
Possible values are: `recents' `bookmarks' `projects'.")

(defvar dotspacemacs-excluded-packages '()
  "A list of packages and/or extensions that will not be install and loaded.")

;; only for backward compatibility
(defalias 'dotspacemacs-mode 'emacs-lisp-mode)

(defun dotspacemacs/sync-configuration-layers (&optional arg)
  "Synchronize declared layers in dotfile with spacemacs.

If ARG is non nil then `dotspacemacs/config' is skipped."
  (interactive "P")
  (when (file-exists-p dotspacemacs-filepath)
    (with-current-buffer (find-file-noselect dotspacemacs-filepath)
      (let ((dotspacemacs-loading-progress-bar nil))
        (setq spacemacs-loading-string "")
        (save-buffer)
        (load-file buffer-file-name)
        (dotspacemacs|call-func dotspacemacs/init "Calling dotfile init...")
        (configuration-layer/sync)
        (if arg
            (message "Done (`dotspacemacs/config' function has been skipped).")
          (dotspacemacs|call-func dotspacemacs/config
                                  "Calling dotfile config...")
          (message "Done."))
        (when (configuration-layer/package-usedp 'powerline)
          (spacemacs//restore-powerline (current-buffer)))))))

(defun dotspacemacs/location ()
  "Return the absolute path to the spacemacs dotfile."
  dotspacemacs-filepath)

(defun dotspacemacs/copy-template ()
  "Copy `.spacemacs.template' in home directory. Ask for confirmation
before copying the file if the destination already exists."
  (interactive)
  (let* ((copy? (if (file-exists-p dotspacemacs-filepath)
                    (y-or-n-p
                     (format "%s already exists. Do you want to overwite it ? "
                             dotspacemacs-filepath)) t)))
    (when copy?
      (copy-file (concat dotspacemacs-template-directory
                         ".spacemacs.template") dotspacemacs-filepath t)
      (message "%s has been installed." dotspacemacs-filepath))))

(defun dotspacemacs//ido-completing-read (prompt candidates)
  "Call `ido-completing-read' with a CANDIDATES alist where the key is
a display strng and the value is the actual value to return."
  (let ((ido-max-window-height (1+ (length candidates))))
    (cadr (assoc (ido-completing-read prompt (mapcar 'car candidates))
                 candidates))))

(defun dotspacemacs/install (arg)
  "Install the dotfile, return non nil if the doftile has been installed.

If ARG is non nil then Ask questions to the user before installing the dotfile."
  (interactive "P")
  ;; preferences is an alist where the key is the text to replace by
  ;; the value in the dotfile
  (let ((preferences
         (when arg
           ;; editing style
           `(("dotspacemacs-editing-style 'vim"
              ,(format "dotspacemacs-editing-style '%S"
                       (dotspacemacs//ido-completing-read
                        "What is your preferred style? "
                        '(("Among the stars aboard the Evil flagship (vim)"
                           vim)
                          ("On the planet Emacs in the Holy control tower (emacs)"
                           emacs)))))))))
    (with-current-buffer (find-file-noselect
                       (concat dotspacemacs-template-directory
                               ".spacemacs.template"))
      (dolist (p preferences)
        (beginning-of-buffer)
        (re-search-forward (car p))
        (replace-match (cadr p)))
      (let ((install
             (if (file-exists-p dotspacemacs-filepath)
                 (y-or-n-p
                  (format "%s already exists. Do you want to overwite it ? "
                          dotspacemacs-filepath)) t)))
        (when install
          (write-file dotspacemacs-filepath)
          (message "%s has been installed." dotspacemacs-filepath)
          t)))))

(defun dotspacemacs//install-and-replace (&optional values)
  "Install the dotfile and replace its content according to VALUES.

VALUES is an alist where the key is the text to replace and value is the new
value."
  )

(defun dotspacemacs/load-file ()
  "Load ~/.spacemacs if it exists."
  (let ((dotspacemacs (dotspacemacs/location)))
    (if (file-exists-p dotspacemacs) (load dotspacemacs))))

(defmacro dotspacemacs|call-func (func &optional msg)
  "Call the function from the dotfile only if it is bound.
If MSG is not nil then display a message in `*Messages'."
  `(progn
     (when ,msg (spacemacs-buffer/message ,msg))
     (if (fboundp ',func) (,func))))

(provide 'core-dotspacemacs)
