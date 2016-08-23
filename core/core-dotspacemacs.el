;;; core-dotspacemacs.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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

(defconst dotspacemacs-test-results-buffer "*dotfile-test-results*"
  "Name of the buffer to display dotfile test results.")

(defconst dotspacemacs-directory
  (let* ((env (getenv "SPACEMACSDIR"))
         (env-dir (when env (expand-file-name (concat env "/"))))
         (no-env-dir-default (expand-file-name
                              (concat user-home-directory
                                      ".spacemacs.d/"))))
    (cond
     ((and env (file-exists-p env-dir))
      env-dir)
     ((file-exists-p no-env-dir-default)
      no-env-dir-default)
     (t
      nil)))
  "Optional spacemacs directory, which defaults to
~/.spacemacs.d. This setting can be overridden using the
SPACEMACSDIR environment variable. If neither of these
directories exist, this variable will be nil.")

(defconst dotspacemacs-filepath
  (let* ((default (concat user-home-directory ".spacemacs"))
         (spacemacs-dir-init (when dotspacemacs-directory
                                 (concat dotspacemacs-directory
                                         "init.el"))))
    (if (and (not (file-exists-p default))
             dotspacemacs-directory
             (file-exists-p spacemacs-dir-init))
        spacemacs-dir-init
      default))
  "Filepath to the installed dotfile. If ~/.spacemacs exists,
then this is used. If ~/.spacemacs does not exist, then check
for init.el in dotspacemacs-directory and use this if it
exists. Otherwise, fallback to ~/.spacemacs")

(defvar dotspacemacs-distribution 'spacemacs
  "Base distribution to use. This is a layer contained in the directory
`+distribution'. For now available distributions are `spacemacs-base'
or `spacemacs'.")

(defvar dotspacemacs-elpa-https t
  "If non nil ELPA repositories are contacted via HTTPS whenever it's
possible. Set it to nil if you have no way to use HTTPS in your
environment, otherwise it is strongly recommended to let it set to t.")

(defvar dotspacemacs-elpa-timeout 5
  "Maximum allowed time in seconds to contact an ELPA repository.")

(defvar dotspacemacs-configuration-layer-path '()
  "List of additional paths where to look for configuration layers.
Paths must have a trailing slash (ie. `~/.mycontribs/')")

(defvar dotspacemacs-additional-packages '()
  "List of additional packages that will be installed wihout being
wrapped in a layer. If you need some configuration for these
packages then consider to create a layer, you can also put the
configuration in `dotspacemacs/user-config'.")

(defvar dotspacemacs-editing-style 'vim
  "One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
uses emacs key bindings for vim's insert mode, but otherwise leaves evil
unchanged.")

(defvar dotspacemacs-startup-banner 'official
   "Specify the startup banner. Default value is `official', it displays
the official spacemacs logo. An integer value is the index of text
banner, `random' chooses a random text banner in `core/banners'
directory. A string value must be a path to a .PNG file.
If the value is nil then no banner is displayed.")

(defvar dotspacemacs-scratch-mode 'text-mode
  "Default major mode of the scratch buffer.")

(defvar dotspacemacs-check-for-update t
  "If non nil then spacemacs will check for updates at startup
when the current branch is not `develop'")

(defvar dotspacemacs-configuration-layers '(emacs-lisp)
  "List of configuration layers to load. If it is the symbol `all' instead
of a list then all discovered layers will be installed.")

(defvar dotspacemacs-themes '(spacemacs-dark
                              spacemacs-light
                              solarized-dark
                              solarized-light
                              leuven)
  "List of themes, the first of the list is loaded when spacemacs starts.
Press <SPC> T n to cycle to the next theme in the list (works great
with 2 themes variants, one dark and one light")

(defvar dotspacemacs-colorize-cursor-according-to-state t
  "If non nil the cursor color matches the state color in GUI Emacs.")

(defvar dotspacemacs-leader-key "SPC"
  "The leader key.")

(defvar dotspacemacs-emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'")

(defvar dotspacemacs-major-mode-leader-key ","
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`. Set it to `nil` to disable it.")

(defvar dotspacemacs-major-mode-emacs-leader-key "C-M-m"
  "Major mode leader key accessible in `emacs state' and `insert state'")

(defvar dotspacemacs-distinguish-gui-tab nil
  "If non nil, distinguish C-i and tab in the GUI version of
emacs.")

;; (defvar dotspacemacs-distinguish-gui-ret nil
;;   "If non nil, distinguish C-m and return in the GUI version of
;; emacs.")

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

(defvaralias 'dotspacemacs-remap-Y-to-y$ 'evil-want-Y-yank-to-eol
  "If non nil `Y' is remapped to `y$'.")

(defvar dotspacemacs-default-layout-name "Default"
  " Name of the default layout.")

(defvar dotspacemacs-display-default-layout nil
  "If non nil the default layout name is displayed in the mode-line.")

(defvar dotspacemacs-auto-resume-layouts nil
  "If non nil then the last auto saved layouts are resume automatically upon
start.")

(defvar dotspacemacs-max-rollback-slots 5
  "Maximum number of rollback slots to keep in the cache.")

(defvar dotspacemacs-use-ido nil
  "If non nil then `ido' replaces `helm' for some commands. For now only
`find-files' (SPC f f) is replaced.")

(defvar dotspacemacs-helm-resize nil
  "If non nil, `helm' will try to minimize the space it uses.")

(defvar dotspacemacs-helm-no-header nil
  "if non nil, the helm header is hidden when there is only one source.")

(defvar dotspacemacs-helm-position 'bottom
  "Position in which to show the `helm' mini-buffer.")

(defvar dotspacemacs-auto-save-file-location 'cache
  "Location where to auto-save files. Possible values are `original' to
auto-save the file in-place, `cache' to auto-save the file to another
file stored in the cache directory and `nil' to disable auto-saving.
Default value is `cache'.")

(defvar dotspacemacs-enable-paste-micro-state t
  "If non nil the paste micro-state is enabled. While enabled pressing `p`
several times cycle between the kill ring content.'")

(defvar dotspacemacs-which-key-delay 0.4
  "Delay in seconds starting from the last keystroke after which
the which-key buffer will be shown if you have not completed a
key sequence. Setting this variable is equivalent to setting
`which-key-idle-delay'.")

(defvar dotspacemacs-which-key-position 'bottom
  "Location of the which-key popup buffer. Possible choices are bottom,
right, and right-then-bottom. The last one will display on the
right if possible and fallback to bottom if not.")

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

(defvar dotspacemacs-line-numbers nil
  "If non nil line numbers are turned on in all `prog-mode' and `text-mode'
derivatives. If set to `relative', also turns on relative line numbers.")

(defvar dotspacemacs-persistent-server nil
  "If non nil advises quit functions to keep server open when quitting.")

(defvar dotspacemacs-smartparens-strict-mode nil
  "If non-nil smartparens-strict-mode will be enabled in programming modes.")

(defvar dotspacemacs-highlight-delimiters 'all
  "Select a scope to highlight delimiters. Possible values are `any',
`current', `all' or `nil'. Default is `all' (highlight any scope and
 emphasis the current one.")

(defvar dotspacemacs-whitespace-cleanup nil
  "delete whitespace while saving buffer. possible values are `all'
to aggressively delete empty lines and long sequences of whitespace, `trailing'
to delete only the whitespace at end of lines, `changed' to delete only
whitespace for changed lines or `nil' to disable cleanup.")

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

(defvar dotspacemacs-startup-recent-list-size 5
  "Number of recent files to show in the startup buffer. Ignored if
`dotspacemacs-startup-lists' doesn't include `recents'.")

(defvar dotspacemacs-excluded-packages '()
  "A list of packages and/or extensions that will not be install and loaded.")

;; only for backward compatibility
(defalias 'dotspacemacs-mode 'emacs-lisp-mode)

(defmacro dotspacemacs|call-func (func &optional msg)
  "Call the function from the dotfile only if it is bound.
If MSG is not nil then display a message in `*Messages'. Errors
are caught and signalled to user in spacemacs buffer."
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

(defun dotspacemacs/sync-configuration-layers (&optional arg)
  "Synchronize declared layers in dotfile with spacemacs.

Called with `C-u' skips `dotspacemacs/user-config'.
Called with `C-u C-u' skips `dotspacemacs/user-config' _and_ preleminary tests."
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
                (configuration-layer/sync)
                (if (member arg '((4) (16)))
                    (message (concat "Done (`dotspacemacs/user-config' function has "
                                     "been skipped)."))
                  ;; TODO remove support for dotspacemacs/config in 0.106
                  (if (fboundp 'dotspacemacs/user-config)
                      (dotspacemacs|call-func dotspacemacs/user-config
                                              "Calling dotfile user config...")
                    (spacemacs-buffer/warning (concat "`dotspacemacs/config' is deprecated, "
                                                      "please rename your function to "
                                                      "`dotspacemacs/user-config'"))
                    (dotspacemacs|call-func dotspacemacs/config
                                            "Calling dotfile user config..."))
                  (message "Done."))
                (when (configuration-layer/package-usedp 'spaceline)
                  (spacemacs//restore-powerline (current-buffer))))
            (switch-to-buffer-other-window dotspacemacs-test-results-buffer)
            (spacemacs-buffer/warning "Some tests failed, check `%s' buffer"
                                      dotspacemacs-test-results-buffer))))))
  (when (configuration-layer/package-usedp 'spaceline)
    (spacemacs//set-powerline-for-startup-buffers)))

(defun dotspacemacs/get-variable-string-list ()
  "Return a list of all the dotspacemacs variables as strings."
  (all-completions "" obarray
                   (lambda (x)
                     (and (boundp x)
                          (not (keywordp x))
                          (string-prefix-p "dotspacemacs"
                                           (symbol-name x))))))

(defun dotspacemacs/get-variable-list ()
  "Return a list of all dotspacemacs variable symbols."
  (mapcar 'intern (dotspacemacs/get-variable-string-list)))

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
              ,(format
                "dotspacemacs-editing-style '%S"
                (dotspacemacs//ido-completing-read
                 "What is your preferred style? "
                 '(("Among the stars aboard the Evil flagship (vim)"
                    vim)
                   ("On the planet Emacs in the Holy control tower (emacs)"
                    emacs)))))
             ("dotspacemacs-distribution 'spacemacs"
              ,(format
                "dotspacemacs-distribution '%S"
                (dotspacemacs//ido-completing-read
                 "What distribution of spacemacs would you like to start with? "
                 '(("The standard distribution, recommended. (spacemacs)"
                    spacemacs)
                   ("A minimalist distribution that you can build on. (spacemacs-base)"
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
    (if (file-exists-p dotspacemacs)
        (unless (with-demoted-errors "Error loading .spacemacs: %S" (load dotspacemacs))
          (dotspacemacs/safe-load)))))

(defun dotspacemacs/safe-load ()
  "Error recovery from malformed .spacemacs.
Loads default .spacemacs template and suspends pruning of orphan packages.
Informs users of error and prompts for default editing style for use during
error recovery."
  (load (concat dotspacemacs-template-directory
                ".spacemacs.template"))
  (defadvice dotspacemacs/layers
      (after error-recover-preserve-packages activate)
    (progn
      (setq-default dotspacemacs-delete-orphan-packages nil)
      (ad-disable-advice 'dotspacemacs/layers 'after
                         'error-recover-preserve-packages)
      (ad-activate 'dotspacemacs/layers)))
  (defadvice dotspacemacs/init
      (after error-recover-prompt-for-style activate)
    (progn
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
      (ad-disable-advice 'dotspacemacs/init 'after
                         'error-recover-prompt-for-style)
      (ad-activate 'dotspacemacs/init))))

(defun dotspacemacs//test-dotspacemacs/layers ()
  "Tests for `dotspacemacs/layers'"
  (insert
   (format (concat "\n* Testing settings in dotspacemacs/layers "
                   "[[file:%s::dotspacemacs/layers][Show in File]]\n")
           dotspacemacs-filepath))
  ;; protect global values of these variables
  (let (dotspacemacs-configuration-layer-path dotspacemacs-configuration-layers
        dotspacemacs-additional-packages dotspacemacs-excluded-packages
        dotspacemacs-delete-orphan-packages
        (passed-tests 0) (total-tests 0))
    (load dotspacemacs-filepath)
    (dotspacemacs/layers)
    (spacemacs//test-list
     'stringp 'dotspacemacs-configuration-layer-path
     "is a string" "path")
    (spacemacs//test-list
     'file-directory-p 'dotspacemacs-configuration-layer-path
     "exists in filesystem" "path")
    (setq dotspacemacs-configuration-layers
          (mapcar (lambda (l) (if (listp l) (car l) l))
                  dotspacemacs-configuration-layers))
    (spacemacs//test-list
     'configuration-layer/get-layer-path
     'dotspacemacs-configuration-layers  "can be found" "layer")
    (insert (format
             (concat "** RESULTS: "
                     "[[file:%s::dotspacemacs/layers][dotspacemacs/layers]] "
                     "passed %s out of %s tests\n")
             dotspacemacs-filepath passed-tests total-tests))
    (equal passed-tests total-tests)))

(defmacro dotspacemacs||let-init-test (&rest body)
  "Macro to protect dotspacemacs variables"
  `(let ((fpath dotspacemacs-filepath)
         ,@(dotspacemacs/get-variable-list)
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
    (lambda (x) (member x '(vim emacs hybrid)))
    'dotspacemacs-editing-style "is \'vim, \'emacs or \'hybrid")
   (spacemacs//test-var
    (lambda (x) (member x '(original cache nil)))
    'dotspacemacs-auto-save-file-location (concat "is one of \'original, "
                                                  "\'cache or nil"))
   (spacemacs//test-var
    (lambda (x) (member x '(all any current nil)))
    'dotspacemacs-highlight-delimiters "is one of \'all, \'any, \'current or nil")
   (spacemacs//test-list
    (lambda (x) (member x '(recents bookmarks projects)))
    'dotspacemacs-startup-lists (concat "includes only \'recents, "
                                        "\'bookmarks or \'projects"))
   (spacemacs//test-var 'stringp 'dotspacemacs-leader-key "is a string")
   (spacemacs//test-var 'stringp 'dotspacemacs-emacs-leader-key "is a string")
   (spacemacs//test-var
    (lambda (x) (or (null x) (stringp x)))
    'dotspacemacs-major-mode-leader-key "is a string or nil")
   (spacemacs//test-var
    (lambda (x) (or (null x) (stringp x)))
    'dotspacemacs-major-mode-emacs-leader-key "is a string or nil")
   (spacemacs//test-var 'stringp 'dotspacemacs-command-key "is a string")
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
  (setq configuration-layer-paths (configuration-layer//discover-layers))
  (let ((min-version "0.0"))
    ;; dotspacemacs-version not implemented yet
    ;; (if (version< dotspacemacs-version min-version)
    (if nil
        (error (format (concat "error: dotspacemacs/test-dotfile requires "
                               "dotspacemacs-version %s") min-version))
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
          ;; (insert (format "* Running tests on %s (v%s)\n" dotspacemacs-filepath dotspacemacs-version))
          (prog1
              ;; execute all tests no matter what
              (cl-reduce (lambda (x y)
                        (and (funcall y) x))
                      '(dotspacemacs//test-dotspacemacs/layers
                        dotspacemacs//test-dotspacemacs/init)
                      :initial-value t)
            (goto-char (point-min))))))))

(provide 'core-dotspacemacs)
