;;; helm-core.el --- Development files for Helm  -*- lexical-binding: t -*-

;; Copyright (C) 2022 ~ 2023  Thierry Volpiatto

;; Author: Thierry Volpiatto <thievol@posteo.net>
;; URL: https://emacs-helm.github.io/helm/
;; Version: 3.9.7
;; Package-Requires: ((emacs "25.1") (async "1.9.7"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Contains the main code for Helm.
;; As a package helm-core provides the files helm-core.el, helm-lib.el,
;; helm-source.el and helm-multi-match.el.

;;; Code:

(require 'cl-lib)
(require 'async)
(require 'helm-lib)
(require 'helm-multi-match)
(require 'helm-source)

;; Ensure async-bytecomp is used even with helm-core package.
(declare-function async-bytecomp-package-mode "ext:async-bytecomp.el")
(when (require 'async-bytecomp nil t)
  (and (fboundp 'async-bytecomp-package-mode)
       (async-bytecomp-package-mode 1)))

;; Setup completion styles for helm-mode
(helm--setup-completion-styles-alist)

(declare-function helm-comp-read "helm-mode.el")
(declare-function custom-unlispify-tag-name "cus-edit.el")
(declare-function helm-quit-and-find-file "helm-utils.el")
(declare-function linum-mode "linum.el")
(declare-function minibuffer-depth-setup "mb-depth.el")

(defvar helm-marked-buffer-name)
(defvar display-buffer-function)
(defvar minibuffer-follows-selected-frame)
(defvar minibuffer-depth-indicate-mode)


;;; Internal Variables
;;
;;
(defvar helm-source-filter nil
  "A list of source names to be displayed.
Other sources won't appear in the search results.
If nil, no filtering is done.
Don't set this directly, use `helm-set-source-filter' during a
Helm session to modify it.")
(defvar helm-saved-action nil
  "Saved value of the currently selected action by key.")
(defvar helm-saved-current-source nil
  "Value of the current source when the action list is shown.")
(defvar helm-in-persistent-action nil
  "Flag whether in persistent-action or not.")
(defvar helm-last-buffer nil
  "`helm-buffer' of a previous Helm session.")
(defvar helm-saved-selection nil
  "Value of the currently selected object when the action list is shown.")
(defvar helm-sources nil
  "[INTERNAL] Value of current sources in use, a list of alists.
The list of sources (symbols or alists) is normalized to alists
in `helm-initialize'.")
(defvar helm-buffer-file-name nil
  "Variable `buffer-file-name' when Helm is invoked.")
(defvar helm-candidate-cache (make-hash-table :test 'equal)
  "Holds the available candidate within a single Helm invocation.")
(defvar helm--candidate-buffer-alist nil)
(defvar helm-input ""
  "The input typed in the candidates panel.")
(defvar helm-input-local nil
  "Internal, store locally `helm-pattern' value for later use in `helm-resume'.")
(defvar helm--source-name nil)
(defvar helm-current-source nil)
(defvar helm-issued-errors nil)
(defvar helm--last-log-file nil
  "The name of the log file of the last Helm session.")
(defvar helm--local-variables nil)
(defvar helm-split-window-state nil)
(defvar helm--window-side-state nil)
(defvar helm-selection-point nil
  "The value of point at selection.")
(defvar helm-alive-p nil)
(defvar helm-visible-mark-overlays nil)
(defvar helm-update-blacklist-regexps '("^" "^ *" "$" "!" " " "\\b"
                                        "\\<" "\\>" "\\_<" "\\_>" ".*"
                                        "??" "?*" "*?" "?"))
(defvar helm--force-updating-p nil
  "[INTERNAL] Don't use this in your programs.")
(defvar helm-exit-status 0
  "Flag to inform if Helm did exit or quit.
0 means Helm did exit when executing an action.
1 means Helm did quit with \\[keyboard-quit]
Knowing this exit-status could help restore a window config when
Helm aborts in some special circumstances.  See
`helm-exit-minibuffer' and `helm-keyboard-quit'.")
(defvar helm-minibuffer-confirm-state nil)
(defvar helm--quit nil)
(defvar helm-buffers nil
  "Helm buffers listed in order of most recently used.")
(defvar helm-current-position nil
  "Cons of (point . window-start)  when Helm is invoked.
`helm-current-buffer' uses this to restore position after
`helm-keyboard-quit'")
(defvar helm-last-frame-or-window-configuration nil
  "Used to store window or frame configuration at Helm start.")
(defvar helm-onewindow-p nil)
(defvar helm-types nil)
(defvar helm--mode-line-string-real nil) ; The string to display in mode-line.
(defvar helm-persistent-action-display-window nil)
(defvar helm-marked-candidates nil
  "Marked candidates.  List of (source . real) pair.")
(defvar helm--mode-line-display-prefarg nil)
(defvar helm--temp-follow-flag nil
  "[INTERNAL] A simple flag to notify persistent action we are following.")
(defvar helm--reading-passwd-or-string nil)
(defvar helm--in-update nil)
(defvar helm--in-fuzzy nil)
(defvar helm-maybe-use-default-as-input nil
  "Flag to notify the use of use-default-as-input.
Use only in let-bindings.
Use :default arg of `helm' as input to update display.
Note that if also :input is specified as `helm' arg, it will take
precedence on :default.")
(defvar helm--temp-hooks nil
  "Store temporary hooks added by `with-helm-temp-hook'.")
(defvar helm--prompt nil)
(defvar helm--file-completion-sources
  '("Find Files" "Read File Name")
  "Sources that use the *find-files mechanism can be added here.
Sources generated by `helm-mode' don't need to be added here
because they are automatically added.

You should not modify this yourself unless you know what you are
doing.")
(defvar helm--completing-file-name nil
  "Non nil when `helm-read-file-name' is running.
Used for `helm-file-completion-source-p'.")
;; Same as `ffap-url-regexp' but keep it here to ensure `ffap-url-regexp' is not nil.
(defvar helm--url-regexp "\\`\\(news\\(post\\)?:\\|mailto:\\|file:\\|\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://\\)")
(defvar helm--ignore-errors nil
  "Flag to prevent Helm popping up errors in candidates functions.
Should be set in candidates functions if needed, and will be
restored at end of session.")
(defvar helm--action-prompt "Select action: ")
(defvar helm--cycle-resume-iterator nil)
(defvar helm--buffer-in-new-frame-p nil)
(defvar helm-initial-frame nil
  "[INTERNAL] The selected frame before starting Helm.
Helm use this internally to know in which frame it started, don't
modify this yourself.")
(defvar helm-popup-frame nil
  "The frame where Helm is displayed.

This is only used when Helm is using
`helm-display-buffer-in-own-frame' as `helm-display-function' and
`helm-display-buffer-reuse-frame' is non nil.")
(defvar helm--nested nil)
(defconst helm--frame-default-attributes
  '(width height tool-bar-lines left top
          title undecorated vertical-scroll-bars
          visibility fullscreen menu-bar-lines undecorated
          alpha foreground-color background-color)
  "Frame parameters to save in `helm--last-frame-parameters'.")
(defvar helm--last-frame-parameters nil
  "Frame parameters to save for later resuming.
Local to `helm-buffer'.")
(defvar helm--executing-helm-action nil
  "Non nil when action is triggering a new helm-session.
This may be let bounded in other places to notify the display
function to reuse the same frame parameters as the previous Helm
session just like resume would do.")
(defvar helm--current-buffer-narrowed nil)
(defvar helm--suspend-update-interactive-flag nil)
(defvar helm-persistent-action-window-buffer nil
  "[INTERNAL] Store the buffer where helm is started.
It is generally `helm-current-buffer', but when this one is displayed
in a dedicated buffer, helm can't start in this window and use another
window handling a buffer, it is this one we store.")
(defvar helm--tramp-archive-maybe-loaded nil)
(defvar helm--original-dedicated-windows-alist nil
  "[INTERNAL] Store all dedicated windows with their dedicated state on startup")

;;; Multi keys
;;
;;
;;;###autoload
(defun helm-define-multi-key (keymap key functions &optional delay)
  "In KEYMAP, define key sequence KEY for function list FUNCTIONS.
Each function runs sequentially for each KEY press.
If DELAY is specified, switch back to initial function of FUNCTIONS list
after DELAY seconds.
The functions in FUNCTIONS list take no args.
E.g.
    (defun foo ()
      (interactive)
      (message \"Run foo\"))
    (defun bar ()
      (interactive)
      (message \"Run bar\"))
    (defun baz ()
      (interactive)
      (message \"Run baz\"))

\(helm-define-multi-key global-map (kbd \"<f5> q\") \\='(foo bar baz) 2)

Each time \"<f5> q\" is pressed, the next function is executed.
Waiting more than 2 seconds between key presses switches back to
executing the first function on the next hit."
  (define-key keymap key (helm-make-multi-command functions delay)))

;;;###autoload
(defmacro helm-multi-key-defun (name docstring funs &optional delay)
  "Define NAME as a multi-key command running FUNS.
After DELAY seconds, the FUNS list is reinitialized.
See `helm-define-multi-key'."
  (declare (indent 2) (doc-string 2))
  (setq docstring (if docstring (concat docstring "\n\n")
                    "This is a helm-ish multi-key command."))
  `(defalias (quote ,name) (helm-make-multi-command ,funs ,delay) ,docstring))

(defun helm-make-multi-command (functions &optional delay)
  "Return an anonymous multi-key command running FUNCTIONS.
Run each function in the FUNCTIONS list in turn when called within
DELAY seconds."
  (declare (indent 1))
  (let ((funs functions)
        (iter (list nil)) ; ref-cell[1].
        (timeout delay))
    (lambda ()
      (interactive)
      (helm-run-multi-key-command funs iter timeout))))

(defun helm-run-multi-key-command (functions iterator delay)
  (let ((fn (lambda ()
              (cl-loop for count from 1 to (length functions)
                       collect count)))
        next)
    ;; By passing a list containing a single 'nil' element [1] as ITERATOR we
    ;; avoid using a global var.
    (unless (and (car iterator)
                 ;; Reset iterator when another key is pressed.
                 (eq this-command real-last-command))
      (setcar iterator (helm-iter-circular (funcall fn))))
    (setq next (helm-iter-next (car iterator)))
    (and next (car iterator)
         (call-interactively (nth (1- next) functions)))
    (when delay (run-with-idle-timer
                 delay nil (lambda ()
                             (setcar iterator nil))))))

(helm-multi-key-defun helm-toggle-resplit-and-swap-windows
    "Multi key command to re-split and swap Helm window.
First call runs `helm-toggle-resplit-window',
and second call within 1s runs `helm-swap-windows'."
  '(helm-toggle-resplit-window helm-swap-windows) 1)
(put 'helm-toggle-resplit-and-swap-windows 'helm-only t)

(defun helm-command-with-subkeys (map subkey command
                                  &optional other-subkeys prompt exit-fn delay)
  "Build a command that run COMMAND when SUBKEY is read.

The command runs a loop reading keys and exit when user stops typing after DELAY
seconds.  After this DELAY EXIT-FN run if specified.

Arg OTHER-SUBKEYS should be an alist composed of (command . short-key) where
command is another command than COMMAND bound to short-key.

A PROMPT can be used to describe bindings of COMMAND and OTHER-SUBKEYS.
 
Return an anonymous interactive command to use with
`helm-define-key-with-subkeys'."
  (lambda ()
    (interactive)
    (let (timer)
      (call-interactively command)
      (unless (or defining-kbd-macro executing-kbd-macro) 
        (unwind-protect
             (progn
               (when delay
                 (setq timer (run-with-idle-timer
                              delay nil (lambda () (keyboard-quit)))))
               (while (let ((input (read-key prompt)) other kb com)
                        (setq last-command-event input)
                        (cond
                          ((eq input subkey)
                           (call-interactively command)
                           (setq last-command command)
                           t)
                          ((setq other (assoc input other-subkeys))
                           (call-interactively (cdr other))
                           (setq last-command (cdr other))
                           t)
                          (t
                           (setq kb (vector last-command-event))
                           (setq com (lookup-key map kb))
                           (if (commandp com)
                               (call-interactively com)
                             (setq unread-command-events
                                   (nconc (mapcar #'identity kb)
                                          unread-command-events)))
                           nil)))))
          (when timer (cancel-timer timer))
          (and exit-fn (funcall exit-fn)))))))

;;;###autoload
(defun helm-define-key-with-subkeys (map key subkey command
                                         &optional other-subkeys
                                           prompt exit-fn delay
                                           docstring)
  "Define in MAP a KEY and SUBKEY to COMMAND.

This allows typing KEY to call COMMAND the first time and
type only SUBKEY on subsequent calls.

Arg MAP is the keymap to use, SUBKEY is the initial short
key binding to call COMMAND.

Arg OTHER-SUBKEYS is an alist specifying other short key bindings
to use once started, e.g.:

    (helm-define-key-with-subkeys global-map
       (kbd \"C-x v n\") ?n \\='git-gutter:next-hunk
       \\='((?p . git-gutter:previous-hunk)))

In this example, `C-x v n' will run `git-gutter:next-hunk'
subsequent \"n\" will run this command again and subsequent \"p\"
will run `git-gutter:previous-hunk'.

If specified PROMPT can be displayed in minibuffer to describe
SUBKEY and OTHER-SUBKEYS.  Arg EXIT-FN specifies a function to run
on exit.

For any other key pressed, run their assigned command as defined
in MAP and then exit the loop running EXIT-FN, if specified.

If DELAY an integer is specified exit after DELAY seconds.

NOTE: SUBKEY and OTHER-SUBKEYS bindings support only char syntax
and vectors, so don't use strings to define them.  While defining
or executing a kbd macro no SUBKEY or OTHER-SUBKEYS are provided,
i.e. the loop is not entered after running COMMAND."
  (declare (indent 1))
  (let ((fn (helm-command-with-subkeys
             map subkey command other-subkeys prompt exit-fn delay))
        (com (intern (format "helm-%s-with-subkeys"
                             (symbol-name command)))))
    (defalias com fn
      (or docstring
          ;; When no DOCSTRING, generate a basic one specifying
          ;; COMMAND, SUBKEY and OTHER-SUBKEYS.
          (concat
           (format "Run `%s' and bound it to `%s' for subsequent calls."
                   command (if (numberp subkey) (single-key-description subkey) subkey))
           (if other-subkeys
               (helm-basic-docstring-from-alist other-subkeys)
             ""))))
    (define-key map key com)))

(defun helm-basic-docstring-from-alist (alist)
  (let* ((len (length alist))
         (osk (cl-loop for (k . v) in alist
                       for count from 1
                       for sep = (cond ((and (= count len) (> len 1))
                                        " and ")
                                       ((> count 1) ",")
                                       (t ""))
                       for key = (if (numberp k) (single-key-description k) k)
                       concat (format "%s`%s'" sep key) into ks
                       concat (format "%s`%s'" sep v) into kv
                       finally return (list ks kv))))
    (format "\nBound as well %s to %s%s."
            (car osk) (if (> len 1) "respectively " "") (cadr osk))))

;;; Keymap
;;
;;
(defvar helm-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "<down>")     #'helm-next-line)
    (define-key map (kbd "<up>")       #'helm-previous-line)
    (define-key map (kbd "C-n")        #'helm-next-line)
    (define-key map (kbd "C-p")        #'helm-previous-line)
    (define-key map (kbd "<C-down>")   #'helm-follow-action-forward)
    (define-key map (kbd "<C-up>")     #'helm-follow-action-backward)
    (define-key map (kbd "<prior>")    #'helm-previous-page)
    (define-key map (kbd "<next>")     #'helm-next-page)
    (define-key map (kbd "M-v")        #'helm-scroll-up)
    (define-key map (kbd "C-v")        #'helm-scroll-down)
    (define-key map (kbd "M-<")        #'helm-beginning-of-buffer)
    (define-key map (kbd "M->")        #'helm-end-of-buffer)
    (define-key map (kbd "C-g")        #'helm-keyboard-quit)
    (define-key map (kbd "<RET>")      #'helm-maybe-exit-minibuffer)
    (define-key map (kbd "C-i")        #'helm-select-action)
    (define-key map (kbd "C-j")        #'helm-execute-persistent-action)
    (define-key map (kbd "C-o")        #'helm-next-source)
    (define-key map (kbd "M-o")        #'helm-previous-source)
    (define-key map (kbd "<right>")    #'helm-next-source)
    (define-key map (kbd "<left>")     #'helm-previous-source)
    (define-key map (kbd "C-l")        #'helm-recenter-top-bottom-other-window)
    (define-key map (kbd "M-C-l")      #'helm-reposition-window-other-window)
    (define-key map (kbd "C-M-v")      #'helm-scroll-other-window)
    (define-key map (kbd "M-<next>")   #'helm-scroll-other-window)
    (define-key map (kbd "C-M-y")      #'helm-scroll-other-window-down)
    (define-key map (kbd "C-M-S-v")    #'helm-scroll-other-window-down)
    (define-key map (kbd "M-<prior>")  #'helm-scroll-other-window-down)
    (define-key map (kbd "<C-M-down>") #'helm-scroll-other-window)
    (define-key map (kbd "<C-M-up>")   #'helm-scroll-other-window-down)
    (define-key map (kbd "C-@")        #'helm-toggle-visible-mark)
    (define-key map (kbd "C-SPC")      #'helm-toggle-visible-mark-forward)
    (define-key map (kbd "M-SPC")      #'helm-toggle-visible-mark-backward)
    (define-key map (kbd "M-[")        nil)
    (define-key map (kbd "M-(")        #'helm-prev-visible-mark)
    (define-key map (kbd "M-)")        #'helm-next-visible-mark)
    (define-key map (kbd "C-k")        #'helm-delete-minibuffer-contents)
    (define-key map (kbd "DEL")        #'helm-delete-char-backward)
    (define-key map (kbd "C-x C-f")    #'helm-quit-and-find-file)
    (define-key map (kbd "M-m")        #'helm-toggle-all-marks)
    (define-key map (kbd "M-a")        #'helm-mark-all)
    (define-key map (kbd "M-U")        #'helm-unmark-all)
    (define-key map (kbd "C-M-a")      #'helm-show-all-candidates-in-source)
    (define-key map (kbd "C-M-e")      #'helm-display-all-sources)
    (define-key map (kbd "C-s")        #'undefined)
    (define-key map (kbd "M-s")        #'undefined)
    (define-key map (kbd "C-r")        #'undefined)
    (define-key map (kbd "C-M-r")      #'undefined)
    (define-key map (kbd "C-M-s")      #'undefined)
    (define-key map (kbd "C-}")        #'helm-narrow-window)
    (define-key map (kbd "C-{")        #'helm-enlarge-window)
    (define-key map (kbd "C-c -")      #'helm-swap-windows)
    (define-key map (kbd "C-c _")      #'helm-toggle-full-frame)
    (define-key map (kbd "C-z")        #'helm-toggle-full-frame)
    (define-key map (kbd "C-c %")      #'helm-exchange-minibuffer-and-header-line)
    (define-key map (kbd "C-c C-y")    #'helm-yank-selection)
    (define-key map (kbd "C-c C-k")    #'helm-kill-selection-and-quit)
    (define-key map (kbd "C-c C-i")    #'helm-insert-or-copy)
    (define-key map (kbd "C-c C-f")    #'helm-follow-mode)
    (define-key map (kbd "C-c C-u")    #'helm-refresh)
    (define-key map (kbd "C-c >")      #'helm-toggle-truncate-line)
    (define-key map (kbd "C-c l")      #'helm-display-line-numbers-mode)
    (define-key map (kbd "M-p")        #'previous-history-element)
    (define-key map (kbd "M-n")        #'next-history-element)
    ;; Unbind `previous-matching-history-element' which is non sense for helm.
    (define-key map (kbd "M-r")        #'undefined)
    (define-key map (kbd "C-!")        #'helm-toggle-suspend-update)
    (define-key map (kbd "C-x b")      #'helm-resume-previous-session-after-quit)
    (define-key map (kbd "C-x C-b")    #'helm-resume-list-buffers-after-quit)
    (helm-define-key-with-subkeys map (kbd "C-c n") ?n #'helm-run-cycle-resume)
    ;; Disable `file-cache-minibuffer-complete'.
    (define-key map (kbd "<C-tab>")    #'undefined)
    ;; Multi keys
    (define-key map (kbd "C-t")        #'helm-toggle-resplit-and-swap-windows)
    ;; Debugging command
    (define-key map (kbd "C-h C-d")    #'helm-enable-or-switch-to-debug)
    (define-key map (kbd "C-h c")      #'helm-customize-group)
    (define-key map (kbd "C-h d")      #'helm-debug-output)
    ;; Allow to eval keymap without errors.
    (define-key map [f1] nil)
    (define-key map (kbd "C-h C-h")    #'undefined)
    (define-key map (kbd "C-h h")      #'undefined)
    (helm-define-key-with-subkeys map
      (kbd "C-w") ?\C-w #'helm-yank-text-at-point
      '((?\C-_ . helm-undo-yank-text-at-point)))
    ;; Use `describe-mode' key in `global-map'.
    (dolist (k (where-is-internal #'describe-mode global-map))
      (define-key map k #'helm-help))
    ;; Bind all actions from f1 to f12, `helm-select-nth-action'
    ;; counts from 0, i.e. (helm-select-nth-action 0) = action 1.
    (dotimes (n 12)
      (define-key map (kbd (format "<f%s>" (1+ n)))
        (lambda ()
          (interactive)
          (helm-select-nth-action n))))
    map)
  "Keymap for helm.")

(defun helm-customize-group-1 (group)
  (require 'cus-edit)
  (let ((name (format "*Customize Group: %s*"
                      (custom-unlispify-tag-name group))))
    (if (buffer-live-p (get-buffer name))
        (switch-to-buffer name)
      (custom-buffer-create
       (list (list group 'custom-group))
       name
       (concat " for group "
               (custom-unlispify-tag-name group))))))

(defun helm-customize-group ()
  "Jump to customization group of current source.

Default to Helm group when group is not defined in source."
  (interactive)
  (let ((source (or (helm-get-current-source)
                    (helm-comp-read
                       "Customize variables for: "
                       (cl-loop for src in (with-helm-buffer helm-sources)
                                collect `(,(assoc-default 'name src) .
                                          ,src))
                       :allow-nest t
                       :exec-when-only-one t))))
    (helm-run-after-exit 'helm-customize-group-1 (helm-get-attr 'group source))))
(put 'helm-customize-group 'helm-only t)

(defun helm--action-at-nth-set-fn-1 (value &optional negative)
  (dotimes (n 9)
    (let ((key (format value (1+ n)))
          (fn (lambda ()
                (interactive)
                (helm-execute-selection-action-at-nth
                 (if negative (- (1+ n)) (1+ n))))))
      (define-key helm-map (kbd key) nil)
      (define-key helm-map (kbd key) fn))))

(defun helm--action-at-nth-set-fn- (var val)
  (set var val)
  (helm--action-at-nth-set-fn-1 val 'negative))

(defun helm--action-at-nth-set-fn+ (var val)
  (set var val)
  (helm--action-at-nth-set-fn-1 val))

(defcustom helm-action-at-nth-negative-prefix-key "C-x %d"
  "The prefix key to execute default action on nth <-n> candidate.

This is a format spec where %d will be replaced by the candidate
number.

NOTE: `setq' have no effect until you restart Emacs, use
customize for immediate effect."
  :group 'helm
  :type 'string
  :set #'helm--action-at-nth-set-fn-)

(defcustom helm-action-at-nth-positive-prefix-key "C-c %d"
  "The prefix key to execute default action on nth <+n> candidate.

This is a format spec where %d will be replaced by the candidate
number.

NOTE: `setq' have no effect until you restart Emacs, use
customize for immediate effect."
  :group 'helm
  :type 'string
  :set #'helm--action-at-nth-set-fn+)


(defgroup helm nil
  "Open Helm."
  :prefix "helm-" :group 'convenience)

;; Easy access to customize
;;;###autoload
(defun helm-configuration ()
  "Customize Helm."
  (interactive)
  (customize-group "helm"))

(defcustom helm-completion-window-scroll-margin 5
  "`scroll-margin' to use for Helm completion window.
Set to 0 to disable.
NOTE: This has no effect when `helm-display-source-at-screen-top'
id is non-nil."
  :group 'helm
  :type  'integer)

(defcustom helm-left-margin-width 0
  "`left-margin-width' value for the `helm-buffer'."
  :group 'helm
  :type 'integer)

(defcustom helm-display-source-at-screen-top t
  "Display candidates at the top of screen.
This happens with `helm-next-source' and `helm-previous-source'.
NOTE: When non-nil (default), disable
`helm-completion-window-scroll-margin'."
  :group 'helm
  :type 'boolean)

(defcustom helm-candidate-number-limit 50
  "Global limit for number of candidates displayed.
When the pattern is empty, the number of candidates shown will be
as set here instead of the entire list, which may be hundreds or
thousands.  Since narrowing and filtering rapidly reduces
available candidates, having a small list will keep the interface
responsive.

Set this value to nil for no limit."
  :group 'helm
  :type '(choice (const :tag "Disabled" nil) integer))

(defcustom helm-input-idle-delay 0.01
  "Idle time before updating, specified in seconds."
  :group 'helm
  :type 'float)

(defcustom helm-exit-idle-delay 0
  "Idle time before exiting minibuffer while Helm is updating.
Has no affect when helm-buffer is up to date (i.e. exit without
delay in this condition)."
  :group 'helm
  :type 'float)

(defvaralias 'helm-samewindow 'helm-full-frame)
(make-obsolete-variable 'helm-samewindow 'helm-full-frame "1.4.8.1")
(defcustom helm-full-frame nil
  "Use current window for showing candidates.
If t, then Helm does not pop-up a new window."
  :group 'helm
  :type 'boolean)

(defcustom helm-candidate-separator
  (if (fontp (char-displayable-p (read "#x2015")))
      "――――――――――――――――――――――――――――――――――――――"
    "--------------------------------------")
  "Candidates separator of `multiline' source."
  :group 'helm
  :type 'string)

(defcustom helm-save-configuration-functions
  '(set-window-configuration . current-window-configuration)
  "Functions used to restore or save configurations for frames and windows.
Specified as a pair of functions, where car is the restore
function and cdr is the save function.

To save and restore frame configuration, set this variable to
\\='(set-frame-configuration . current-frame-configuration)

NOTE: This may not work properly with own-frame minibuffer
settings.  Older versions saves/restores frame configuration, but
the default has changed now to avoid flickering."
  :group 'helm
  :type 'sexp)

(defcustom helm-display-function 'helm-default-display-buffer
  "Function used to display `helm-buffer'.

Local value in `helm-buffer' will take precedence on this default
value.  Commands that are in `helm-commands-using-frame' will have
`helm-buffer' displayed in frame, `helm-display-function' being
ignored.
If no local value is found and current command is not one of
`helm-commands-using-frame' use this default value.
The function in charge of deciding which value use is
`helm-resolve-display-function'.

To set it locally to `helm-buffer' in Helm sources use
`helm-set-local-variable' in init function or use
:display-function slot in `helm' call."
  :group 'helm
  :type 'symbol)

(defcustom helm-case-fold-search 'smart
  "Adds \\='smart' option to `case-fold-search'.
Smart option ignores case for searches as long as there are no
upper case characters in the pattern.

Use nil or t to turn off smart behavior and use
`case-fold-search' behavior.

Default is smart.

NOTE: Case fold search has no effect when searching asynchronous
sources, which relies on customized features implemented directly
into their execution process. See helm-grep.el for an example."
  :group 'helm
  :type '(choice (const :tag "Ignore case" t)
                 (const :tag "Respect case" nil)
                 (other :tag "Smart" smart)))

(defcustom helm-file-name-case-fold-search
  (if (memq system-type
            '(cygwin windows-nt ms-dos darwin))
      t
    helm-case-fold-search)
  "Local setting of `helm-case-fold-search' for reading filenames.

See `helm-case-fold-search' for more info."
  :group 'helm
  :type 'symbol)

(defcustom helm-reuse-last-window-split-state nil
  "Use the same state of window split, vertical or horizontal.
`helm-toggle-resplit-window' for the next helm session will use
the same window scheme as the previous session unless
`helm-split-window-default-side' is \\='same or \\='other."
  :group 'helm
  :type 'boolean)

(defcustom helm-split-width-threshold nil
  "The value of `split-width-threshold' for helm windows.
This affect the behavior of `helm-split-window-default-fn'.
When the value is an integer, `split-window-sensibly' is used inconditionally
and all the helm variables that affect window splitting are ignored."
  :group 'helm
  :type '(choice
           (const :tag "Maybe use `split-window-sensibly'" nil)
           (integer :tag "Inconditionally use `split-window-sensibly'")))

(defcustom helm-split-window-preferred-function 'helm-split-window-default-fn
  "Default function used for splitting window."
  :group 'helm
  :type 'function)

(defcustom helm-split-window-default-side 'below
  "The default side to display `helm-buffer'.
Must be one acceptable arg for `split-window' SIDE,
that is `below', `above', `left' or `right'.

Other acceptable values are `same' which always displays
`helm-buffer' in current window and `other' that displays
`helm-buffer' below if only one window or in
`other-window-for-scrolling' when available.

A nil value has same effect as `below'. If `helm-full-frame' is
non-nil, it take precedence over this setting.

See also `helm-split-window-inside-p' and
`helm-always-two-windows' that take precedence over this.

NOTE: this has no effect if
`helm-split-window-preferred-function' is not
`helm-split-window-default-fn' unless this new function can
handle this."
  :group 'helm
  :type 'symbol)

(defcustom helm-split-window-other-side-when-one-window 'below
  "Place for `helm-window' when `helm-split-window-default-side' is \\='other.

The default side to display `helm-buffer' when (1)
`helm-split-window-default-side' is \\='other and (2)
the current frame only has one window. Possible values
are acceptable args for `split-window' SIDE, that is `below',
`above', `left' or `right'.

If `helm-full-frame' is non-nil, it takes precedence over this
setting.

See also `helm-split-window-inside-p' and `helm-always-two-windows' that
takes precedence over this.

NOTE: this has no effect if
`helm-split-window-preferred-function' is not
`helm-split-window-default-fn' unless this new function can
handle this."
  :group 'helm
  :type 'symbol)

(defcustom helm-display-buffer-default-height nil
  "Initial height of `helm-buffer', specified as an integer or a function.

The function should take one arg and be responsible for re-sizing
the window; function's return value is ignored.  Note that this
has no effect when the split is vertical.  See `display-buffer'
for more info."
  :group 'helm
  :type '(choice integer function))

(defcustom helm-display-buffer-default-width nil
  "Initial width of `helm-buffer', specified as an integer or a function.

The function should take one arg and be responsible for re-sizing
the window; function's return value is ignored.  Note that this
have no effect when the split is horizontal.  See `display-buffer'
for more info."
  :group 'helm
  :type '(choice integer function))

(defvaralias 'helm-split-window-in-side-p 'helm-split-window-inside-p)
(make-obsolete-variable 'helm-split-window-in-side-p 'helm-split-window-inside-p "2.8.6")
(defcustom helm-split-window-inside-p nil
  "Force split inside selected window when non-nil.
See also `helm-split-window-default-side'.

NOTE: this has no effect if
`helm-split-window-preferred-function' is not
`helm-split-window-default-fn' unless this new function can
handle this."
  :group 'helm
  :type 'boolean)

(defcustom helm-always-two-windows t
  "When non-nil Helm uses two windows in this frame.

I.e. `helm-buffer' in one window and `helm-current-buffer'
in the other.

Note: this has no effect when `helm-split-window-inside-p' is
non-nil, or when `helm-split-window-default-side' is set to
\\='same.

When `helm-autoresize-mode' is enabled, setting this to nil
will have no effect.

Also when non-nil it overrides the effect of
`helm-split-window-default-side' set to `other'."
  :group 'helm
  :type 'boolean)

(defcustom helm-display-buffer-width 72
  "Frame width when displaying helm-buffer in own frame."
  :group 'helm
  :type 'integer)

(defcustom helm-display-buffer-height 20
  "Frame height when displaying helm-buffer in own frame."
  :group 'helm
  :type 'integer)

(defcustom helm-default-display-buffer-functions nil
  "Action functions to pass to `display-buffer'.
See (info \"(elisp) Buffer Display Action Functions\").

It may override others helm window related variables settings like
`helm-always-two-windows', `helm-split-window-inside-p' etc..."
  :group 'helm
  :type '(repeat symbol))

(defcustom helm-default-display-buffer-alist nil
  "Additional alist to pass to `display-buffer' action.
See (info \"(elisp) Action Alists for Buffer Display\").

It may override others helm window related variables settings like
`helm-always-two-windows', `helm-split-window-inside-p' etc...

Note that window-height and window-width have to be configured in
`helm-display-buffer-height' and `helm-display-buffer-width'."
  :group 'helm
  :type '(alist :key-type symbol :value-type sexp))

(defcustom helm-sources-using-default-as-input '(helm-source-imenu
                                                 helm-source-imenu-all
                                                 helm-source-info-elisp
                                                 helm-source-etags-select
                                                 helm-source-man-pages
                                                 helm-source-occur
                                                 helm-source-moccur
                                                 helm-source-grep-ag
                                                 helm-source-grep-git
                                                 helm-source-grep)
  "List of Helm sources that need to use `helm-maybe-use-default-as-input'.
When a source is a member of this list, default `thing-at-point'
will be used as input."
  :group 'helm
  :type '(repeat (choice symbol)))

(defcustom helm-delete-minibuffer-contents-from-point t
  "When non-nil, `helm-delete-minibuffer-contents' deletes region from `point'.
Otherwise it deletes `minibuffer-contents'.
See documentation for `helm-delete-minibuffer-contents'."
  :group 'helm
  :type 'boolean)

(defcustom helm-follow-mode-persistent nil
  "When non-nil, save last state of `helm-follow-mode' for the next Emacs sessions.

Each time you turn on or off `helm-follow-mode', the current
source name will be stored or removed from
`helm-source-names-using-follow'.

Note that this may be disabled in some places where it is unsafe
to use because persistent action is changing according to
context."
  :group 'helm
  :type 'boolean)

(defcustom helm-source-names-using-follow nil
  "A list of source names to have follow enabled.
This list of source names will be used only
when `helm-follow-mode-persistent' is non-nil.

You don't have to customize this yourself unless you really want
and know what you are doing, instead just set
`helm-follow-mode-persistent' to non-nil and as soon as you turn
on or off `helm-follow-mode' (C-c C-f) in a source, Helm will
save or remove source name in this variable."
  :group 'helm
  :type '(repeat (choice string)))

(defcustom helm-prevent-escaping-from-minibuffer t
  "Prevent escaping from minibuffer with `other-window' during the Helm session."
  :group 'helm
  :type 'boolean)

(defcustom helm-allow-mouse t
  "Allow mouse usage during the Helm session when non-nil.

Note that this also allows moving out of minibuffer when clicking
outside of `helm-buffer', so it is up to you to get back to Helm
by clicking back in `helm-buffer' or minibuffer."
  :group 'helm
  :type 'boolean)

(defcustom helm-move-to-line-cycle-in-source t
  "Cycle to the beginning or end of the list after reaching the bottom or top.
This applies when using `helm-next/previous-line'."
  :group 'helm
  :type 'boolean)

(defcustom helm-fuzzy-match-fn 'helm-fuzzy-match
  "The function for fuzzy matching in `helm-source-sync' based sources."
  :group 'helm
  :type 'function)

(defcustom helm-fuzzy-search-fn 'helm-fuzzy-search
  "The function for fuzzy matching in `helm-source-in-buffer' based sources."
  :group 'helm
  :type 'function)

(defcustom helm-fuzzy-sort-fn 'helm-fuzzy-matching-default-sort-fn
  "The sort transformer function used in fuzzy matching."
  :group 'helm
  :type 'function)

(defcustom helm-fuzzy-matching-highlight-fn #'helm-fuzzy-default-highlight-match
  "The function to highlight fuzzy matches.
The function must have the same signature as
`helm-fuzzy-default-highlight-match' which is the default."
  :group 'helm
  :type 'function)

(defcustom helm-autoresize-max-height 40
  "Specify maximum height and defaults to percent of Helm window's frame height.

See `fit-window-to-buffer' for more infos."
  :group 'helm
  :type 'integer)

(defcustom helm-autoresize-min-height 10
  "Specify minimum height and defaults to percent of Helm window's frame height.

If nil, `window-min-height' is used.
See `fit-window-to-buffer' for details."
  :group 'helm
  :type 'integer)

(defcustom helm-input-method-verbose-flag nil
  "The default value for `input-method-verbose-flag' used in Helm minibuffer.
It is nil by default, which does not turn off input method. Helm
updates and exits without interruption -- necessary for complex
methods.

If set to any other value as per `input-method-verbose-flag',
then use `C-\\' to disable the `current-input-method' to exit or
update Helm."
  :group 'helm
  :type '(radio :tag "A flag to control extra guidance for input methods in helm."
                (const :tag "Never provide guidance" nil)
                (const :tag "Always provide guidance" t)
                (const :tag "Provide guidance only for complex methods" complex-only)))

(defcustom helm-display-header-line t
  "Display header-line when non nil.
It has to be non nil when you want to display minibuffer contents in there with
`helm-echo-input-in-header-line'."
  :group 'helm
  :type 'boolean)

(defcustom helm-inherit-input-method t
  "Inherit `current-input-method' from `current-buffer' when non-nil.
The default is to enable this by default and then toggle
`toggle-input-method'."
  :group 'helm
  :type 'boolean)

(defcustom helm-echo-input-in-header-line nil
  "Send current input to header-line when non-nil.
Note that `helm-display-header-line' has to be non nil as well for this to take
effect."
  :group 'helm
  :type 'boolean)

(defcustom helm-header-line-space-before-prompt 'left-fringe
  "Specify the space before prompt in header-line.

This will be used when `helm-echo-input-in-header-line' is
non-nil.

Value can be one of the symbols \\='left-fringe or \\='left-margin or
an integer specifying the number of spaces before prompt.  Note
that on input longer that `window-width' the continuation string
will be shown on left side of window without taking care of
this."
  :group 'helm
  :type '(choice
          (symbol
           (const :tag "Fringe" left-fringe)
           (const :tag "Margin" left-margin))
          integer))

(defcustom helm-tramp-connection-min-time-diff 5
  "Value of `tramp-connection-min-time-diff' for Helm remote processes.
If set to zero Helm remote processes are not delayed.

Setting this to a value less than 5 or disabling it with a zero
value is risky, however on Emacs versions starting at 24.5 it
seems it is now possible to disable it.

Anyway at any time in Helm you can suspend your processes while
typing by hitting \\<helm-map> `\\[helm-toggle-suspend-update]'.

Only async sources than use a sentinel calling
`helm-process-deferred-sentinel-hook' are affected by this."
  :type 'integer
  :group 'helm)

(defcustom helm-show-action-window-other-window 'left
  "Show action buffer beside `helm-buffer' when non-nil.

If nil don't split and replace helm-buffer by the action buffer
in same window.
Possible value are left, right, below and above."
  :group 'helm
  :type '(choice
          (const :tag "Split at left" left)
          (const :tag "Split at right" right)
          (const :tag "Split below" below)
          (const :tag "Split above" above)
          (const :tag "Don't split" nil)))

(defcustom helm-cycle-resume-delay 1.0
  "Delay used before resuming in `helm-run-cycle-resume'."
  :type 'float
  :group 'helm)

(defcustom helm-display-buffer-reuse-frame nil
  "When non nil Helm frame is not deleted and reused in next sessions.

This was used to workaround a bug in Emacs where frames where
popping up slowly, now that the bug have been fixed upstream
\(emacs-27) probably you don't want to use this any more.  On
emacs-26 set `x-wait-for-event-timeout' to nil to have your
frames popping up fast."
  :group 'helm
  :type 'boolean)

(defcustom helm-commands-using-frame nil
  "A list of commands where `helm-buffer' is displayed in a frame."
  :group 'helm
  :type '(repeat symbol))

(defcustom helm-actions-inherit-frame-settings t
  "Actions inherit Helm frame settings of initial command when non nil."
  :group 'helm
  :type 'boolean)

(defcustom helm-use-undecorated-frame-option t
  "Display Helm frame undecorated when non nil.

This option has no effect with Emacs versions lower than 26."
  :group 'helm
  :type 'boolean)

(defcustom helm-frame-background-color nil
  "Background color for Helm frames, a string.
Fallback to default face background when nil."
  :group 'helm
  :type 'string)

(defcustom helm-frame-foreground-color nil
  "Foreground color for Helm frames, a string.
Fallback to default face foreground when nil"
  :group 'helm
  :type 'string)

(defcustom helm-frame-alpha nil
  "Alpha parameter for Helm frames, an integer.
Fallback to 100 when nil."
  :group 'helm
  :type 'integer)

(defcustom helm-use-frame-when-more-than-two-windows nil
  "Display Helm buffer in frame when more than two windows."
  :group 'helm
  :type 'boolean)

(defvaralias 'helm-use-frame-when-dedicated-window
    'helm-use-frame-when-no-suitable-window)

(defcustom helm-use-frame-when-no-suitable-window nil
  "Display Helm buffer in frame when Helm is started from a dedicated window."
  :group 'helm
  :type 'boolean)
(make-obsolete-variable 'helm-use-frame-when-dedicated-window
                        'helm-use-frame-when-no-suitable-window
                        "3.8.1")

(defcustom helm-default-prompt-display-function
  #'helm-set-default-prompt-display
  "The function to use to set face of fake cursor in header-line."
  :group 'helm
  :type 'function)

(defcustom helm-truncate-lines nil
  "The value of `truncate-lines' when Helm starts.
You can toggle later `truncate-lines' with
\\<helm-map>\\[helm-toggle-truncate-line]."
  :group 'helm
  :type 'boolean)

(defcustom helm-visible-mark-prefix "*"
  "Prefix used in margin for marked candidates.
Set this to an empty string if you don't want prefix in margin when marking."
  :group 'helm
  :type 'string)

;;; Faces
;;
;;
(defgroup helm-faces nil
  "Customize the appearance of Helm."
  :prefix "helm-"
  :group 'faces
  :group 'helm)

(defface helm-source-header
  `((((background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#22083397778B"
     :foreground "white"
     :weight bold :height 1.3 :family "Sans Serif")
    (((background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#abd7f0"
     :foreground "black"
     :weight bold :height 1.3 :family "Sans Serif"))
  "Face for source header in the Helm buffer."
  :group 'helm-faces)

(defface helm-visible-mark
  `((((min-colors 88) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "green1"
     :foreground "black")
    (((background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "green"
     :foreground "black")
    (((background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#d1f5ea")
    (((min-colors 88))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "green1")
    (t ,@(and (>= emacs-major-version 27) '(:extend t))
       :background "green"))
  "Face for visible mark."
  :group 'helm-faces)

(defface helm-header
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :inherit header-line))
  "Face for header lines in the Helm buffer."
  :group 'helm-faces)

(defface helm-candidate-number
  `((((background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "Yellow" :foreground "black")
    (((background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#faffb5" :foreground "black"))
  "Face for candidate number in mode-line."
  :group 'helm-faces)

(defface helm-candidate-number-suspended
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
     :inherit helm-candidate-number :inverse-video t))
  "Face for candidate number in mode-line when Helm is suspended."
  :group 'helm-faces)

(defface helm-selection
  `((((background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "ForestGreen"
     :distant-foreground "black")
    (((background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#b5ffd1"
     :distant-foreground "black"))
  "Face for currently selected item in the Helm buffer."
  :group 'helm-faces)

(defface helm-separator
  `((((background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "red")
    (((background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "#ffbfb5"))
  "Face for multiline source separator."
  :group 'helm-faces)

(defface helm-action
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :underline t))
  "Face for action lines in the Helm action buffer."
  :group 'helm-faces)

(defface helm-prefarg
  `((((background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "green")
    (((background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "red"))
  "Face for showing prefix arg in mode-line."
  :group 'helm-faces)

(defface helm-match
  `((((background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "#b00000")
    (((background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "gold1"))
  "Face used to highlight matches."
  :group 'helm-faces)

(defface helm-header-line-left-margin
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "black" :background "yellow"))
  "Face used to highlight helm-header sign in left-margin.
This face is used only when using `helm-echo-input-in-header-line' and pattern
is wider then screen."
  :group 'helm-faces)

(defface helm-minibuffer-prompt
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :inherit minibuffer-prompt))
  "Face used for the minibuffer/headline prompt (such as Pattern:) in Helm."
  :group 'helm-faces)

(defface helm-eob-line
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :inherit default))
  "Face for empty line at end of sources in the Helm buffer.
Allow specifying the height of this line."
  :group 'helm-faces)

(defface helm-mark-prefix
  `((t :inherit default))
  "Face for string `helm-visible-mark-prefix'."
  :group 'helm-faces)

;;; Variables.
;;
;;
(defvar helm-selection-overlay nil
  "Overlay used to highlight the currently selected item.")

(defvar helm-async-processes nil
  "List of information about asynchronous processes managed by Helm.")

(defvar helm-before-initialize-hook nil
  "Runs before Helm initialization.
This hook runs before init functions in `helm-sources', which is
before creation of `helm-buffer'.  Set local variables for
`helm-buffer' that need a value from `current-buffer' with
`helm-set-local-variable'.")

(defvar helm-after-initialize-hook nil
  "Runs after Helm initialization.
This hook runs after `helm-buffer' is created but not from
`helm-buffer'.  The hook needs to specify in which buffer to
run.")

(defvaralias 'helm-update-hook 'helm-after-update-hook)
(make-obsolete-variable 'helm-update-hook 'helm-after-update-hook "1.9.9")

(defvar helm-after-update-hook nil
  "Runs after updating the Helm buffer with the new input pattern.")

(defvar helm-before-update-hook nil
  "Runs before updating the Helm buffer with the new input pattern.")

(defvar helm-cleanup-hook nil
  "Runs after exiting the minibuffer and before performing an
action.

This hook runs even if Helm exits the minibuffer abnormally (e.g.
via `helm-keyboard-quit').")

(defvar helm-select-action-hook nil
  "Runs when opening the action buffer.")

(defvar helm-before-action-hook nil
  "Runs before executing action.
Unlike `helm-cleanup-hook', this hook runs before Helm closes the
minibuffer and also before performing an action.")

(defvar helm-after-action-hook nil
  "Runs after executing action.")

(defvar helm-exit-minibuffer-hook nil
  "Runs just before exiting the minibuffer.

This hook runs when Helm exits the minibuffer normally (e.g., via
candidate selection), but does NOT run if Helm exits the
minibuffer abnormally (e.g. via `helm-keyboard-quit').")

(defvar helm-after-persistent-action-hook nil
  "Runs after executing persistent action.")

(defvar helm-move-selection-before-hook nil
  "Runs before moving selection in `helm-buffer'.")

(defvar helm-move-selection-after-hook nil
  "Runs after moving selection in `helm-buffer'.")

(defvar helm-after-preselection-hook nil
  "Runs after pre-selection in `helm-buffer'.")

(defvar helm-window-configuration-hook nil
  "Runs when switching to and from the action buffer.
Should run also at end of `helm-display-function'.")

(defvar helm-execute-action-at-once-if-one nil
  "When non-nil execute the default action and then exit if only one candidate.
If symbol \\='current-source is given as value exit if only one
candidate in current source.  This variable accepts a function
with no args that should returns a boolean value or \\='current-source.")

(defvar helm-quit-if-no-candidate nil
  "When non-nil, quit if there are no candidates.
This variable accepts a function.")

(defvar helm-debug-function #'helm-default-debug-function
  "A Function that returns a list of values to print in `helm-debug-output' buffer.")

(defvar helm-debug-output-buffer "*Helm Debug*")

(defvar helm-debug-buffer "*Debug Helm Log*")

(defvar helm-debug nil
  "If non-nil, write log message to `helm-debug-buffer'.
Default is nil, which disables writing log messages because the
size of `helm-debug-buffer' grows quickly.")

(defvar helm-mode-line-string "\
\\<helm-map>\
\\[helm-help]:Help \
\\[helm-select-action]:Act \
\\[helm-maybe-exit-minibuffer]/\
f1..f12:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend \
\\[helm-customize-group]:Conf"
  "Help string displayed by Helm in the mode-line.
It is either a string or a list of two string arguments where the
first string is the name and the second string is displayed in
the mode-line. When nil, it defaults to `mode-line-format'.")

(defvar helm-minibuffer-set-up-hook '(helm-hide-minibuffer-maybe)
  "Hook that runs at minibuffer initialization.
A hook useful for modifying minibuffer settings in Helm.

Uses `helm-hide-minibuffer-maybe' by default which hide minibuffer contents with
header-line contents when `helm-echo-input-in-header-line' is non nil.")

(defvar helm-help-message
  "* Helm Generic Help
** Basics

To navigate in this Help buffer see [[Helm help][here]].

Helm narrows down the list of candidates as you type a filter
pattern.  See [[Matching in Helm][Matching in Helm]].

Helm accepts multiple space-separated patterns, each pattern can
be negated with \"!\".

Helm also supports fuzzy matching in some places when specified,
you will find several variables to enable fuzzy matching in
diverse [[Helm sources][sources]], see [[https://github.com/emacs-helm/helm/wiki/Fuzzy-matching][fuzzy-matching]] in helm-wiki for more infos.

Helm generally uses familiar Emacs keys to navigate the list.
Here follow some of the less obvious bindings:

- `\\<helm-map>\\[helm-maybe-exit-minibuffer]' selects the
candidate from the list, executes the default action upon exiting
the Helm session.

- `\\<helm-map>\\[helm-execute-persistent-action]' executes the
default action but without exiting the Helm session.  Not all
sources support this.

- `\\<helm-map>\\[helm-select-action]' displays a list of actions
available on current candidate or all marked candidates.  The
default binding <tab> is ordinarily used for completion, but that
would be redundant since Helm completes upon every character
entered in the prompt.  See [[https://github.com/emacs-helm/helm/wiki#helm-completion-vs-emacs-completion][Helm wiki]].

Note: In addition to the default actions list, additional actions
appear depending on the type of the selected candidate(s).  They
are called filtered actions.

** Helm sources

Helm uses what's called sources to provide different kinds of
completions.  Each Helm session can handle one or more source.  A
source is an alist object which is build from various classes,
see [[Writing your own Helm sources][here]] and [[https://github.com/emacs-helm/helm/wiki/Developing#creating-a-source][Helm wiki]] for more infos.

*** Configure sources

You will find in Helm sources already built and bound to a
variable called generally `helm-source-<something>'.  In this case
it is an alist and you can change the attributes (keys) values
using `helm-set-attr' function in your configuration.  Of course
you have to ensure before calling `helm-set-attr' that the file
containing source is loaded, e.g. with `with-eval-after-load'.  Of
course you can also completely redefine the source but this is
generally not elegant as it duplicate for its most part code
already defined in Helm.

You will find also sources that are not built and even not bound
to any variables because they are rebuilded at each start of a
Helm session.  In this case you can add a defmethod called
`helm-setup-user-source' to your config:

#+begin_src elisp

    (cl-defmethod helm-setup-user-source ((source helm-moccur-class))
      (setf (slot-value source 'follow) -1))

#+end_src

See
[[https://github.com/emacs-helm/helm/wiki/FAQ#why-is-a-customizable-helm-source-nil][here]]
for more infos, and for more complex examples of configuration
[[https://github.com/thierryvolpiatto/emacs-tv-config/blob/master/init-helm.el#L340][here]].

** Modify keybindings in Helm

Helm main keymap is `helm-map', all keys bound in this map apply
to all Helm sources.  However, most sources have their own keymap,
with each binding overriding its counterpart in `helm-map', you
can see all bindings in effect in the [[Commands][Commands]]
section (available only if the source has its own keymap and
documentation of course).

** Matching in Helm

All that you write in minibuffer is interpreted as a regexp or
multiple regexps if separated by a space.  This is true for most
sources unless the developer of the source has disabled it or
have choosen to use fuzzy matching.  Even if a source has fuzzy
matching enabled, Helm will switch to multi match as soon as it
detects a space in the pattern.  It may also switch to multi match
as well if pattern starts with a \"^\" beginning of line sign.  In
those cases each pattern separated with space should be a regexp
and not a fuzzy pattern.  When using multi match patterns, each
pattern starting with \"!\" is interpreted as a negation i.e.
match everything but this.

*** Completion-styles

UPDATE: At version 3.8.0 Helm default is now to NOT use
`completion-styles' i.e. now `helm-completion-style' default to
'helm and no more to 'emacs.

If you want to use `completion-styles' in Helm customize
`helm-completion-style' to 'emacs.

Helm generally fetches its candidates with the :candidates
function up to `helm-candidate-number-limit' and then applies
match functions to these candidates according to `helm-pattern'.
But Helm allows matching candidates directly from the :candidates
function using its own `completion-styles'.
Helm provides 'helm completion style but also 'helm-flex
completion style for Emacs<27 that don't have 'flex completion
style, otherwise (emacs-27) 'flex completion style is used to
provide fuzzy aka flex completion.

When using helm-fuzzy as `helm-completion-style' helm use its own
fuzzy implementation which have nothing to do with emacs `flex'
style.

Any Helm sources can use `completion-styles'
by using :match-dynamic slot and building their :candidates
function with `helm-dynamic-completion'.

Example:

#+begin_src elisp

    (helm :sources (helm-build-sync-source \"test\"
                     :candidates (helm-dynamic-completion
                                  '(foo bar baz foab)
                                  'symbolp)
                     :match-dynamic t)
          :buffer \"*helm test*\")

#+end_src

By default Helm sets up `completion-styles' and always adds 'helm
to it.  However the flex completion styles are not added.  This is
up to the user if she wants to have such completion to enable
this.
As specified above use 'flex for emacs-27 and 'helm-flex for
emacs-26. Anyway, 'helm-flex is not provided in
`completion-styles-alist' if 'flex is present.

Finally Helm provides two user variables to control
`completion-styles' usage: `helm-completion-style' and
`helm-completion-styles-alist'.  Both variables are customizable.
The former allows retrieving previous Helm behavior if needed, by
setting it to `helm' or `helm-fuzzy', default being `emacs' which
allows dynamic completion and usage of `completion-styles'.  The
second allows setting `helm-completion-style' per mode and also
specifying `completion-styles' per mode (see its docstring).  Note
that these two variables take effect only in helm-mode i.e. in
all that uses `completion-read' or `completion-in-region', IOW all
helmized commands.  File completion in `read-file-name' family
doesn't obey completion-styles and has its own file completion
implementation. Native Helm commands using `completion-styles'
doesn't obey `helm-completion-style' and
`helm-completion-styles-alist' (e.g., helm-M-x).

Also for a better control of styles in native Helm sources (not
helmized by helm-mode) using :match-dynamic,
`helm-dynamic-completion' provides a STYLES argument that allows
specifying explicitely styles for this source.

NOTE: Some old completion styles are not working fine with Helm
and are disabled by default in
`helm-blacklist-completion-styles'.  They are anyway not useful in
Helm because 'helm style supersedes these styles.

** Helm mode

`helm-mode' toggles Helm completion in native Emacs functions, so
when you turn `helm-mode' on, commands like `switch-to-buffer'
will use Helm completion instead of the usual Emacs completion
buffer.

*** What gets or does not get \"helmized\" when `helm-mode' is enabled?

Helm provides generic completion on all Emacs functions using
`completing-read', `completion-in-region' and their derivatives,
e.g. `read-file-name'.  Helm exposes a user variable to control
which function to use for a specific Emacs command:
`helm-completing-read-handlers-alist'.  If the function for a
specific command is nil, it turns off Helm completion.  See the
variable documentation for more infos.

*** Helm functions vs helmized Emacs functions

While there are Helm functions that perform the same completion
as other helmized Emacs functions, e.g. `switch-to-buffer' and
`helm-buffers-list', the native Helm functions like
`helm-buffers-list' can receive new features that allow marking
candidates, have several actions, etc.  Whereas the helmized Emacs
functions only have Helm completion, Emacs can provide no more
than one action for this function.  This is the intended behavior.

Generally you are better off using the native Helm command than
the helmized Emacs equivalent.

*** Completion behavior with Helm and completion-at-point

Helm is NOT completing dynamically.  That means that when you are
completing some text at point, completion is done against this
text and subsequent characters you add AFTER this text.  This
allows you to use matching methods provided by Helm, that is multi
matching or fuzzy matching (see [[Matching in Helm][Matching in
Helm]]).

Completion is not done dynamically (against `helm-pattern')
because backend functions (i.e. `completion-at-point-functions')
are not aware of Helm matching methods.

By behaving like this, the benefit is that you can fully use Helm
matching methods but you can't start a full completion against a
prefix different than the initial text you have at point.  Helm
warns you against this by colorizing the initial input and sends
a user-error message when trying to delete backward text beyond
this limit at first hit on DEL.  A second hit on DEL within a
short delay (1s) quits Helm and delete-backward char in
current-buffer.

** Helm help

\\[helm-documentation]: Show all Helm documentations concatenated
in one org file.

From a Helm session, just hit \\<helm-map>\\[helm-help] to have
the documentation for the current source followed by the global
Helm documentation.

While in the help buffer, most of the Emacs regular key bindings
are available; the most important ones are shown in minibuffer.
However, due to implementation restrictions, no regular Emacs
keymap is used (it runs in a loop when reading the help buffer).
Only simple bindings are available and they are defined in
`helm-help-hkmap', which is a simple alist of (key . function).
You can define or redefine bindings in help with
`helm-help-define-key' or by adding/removing entries directly in
`helm-help-hkmap'.
See `helm-help-hkmap' for restrictions on bindings and functions.

The documentation of default bindings are:

| Key       | Alternative keys | Command             |
|-----------+------------------+---------------------|
| C-v       | Space next       | Scroll up           |
| M-v       | b prior          | Scroll down         |
| C-s       |                  | Isearch forward     |
| C-r       |                  | Isearch backward    |
| C-a       |                  | Beginning of line   |
| C-e       |                  | End of line         |
| C-f       | right            | Forward char        |
| C-b       | left             | Backward char       |
| C-n       | down             | Next line           |
| C-p       | up               | Previous line       |
| M-a       |                  | Backward sentence   |
| M-e       |                  | Forward sentence    |
| M-f       |                  | Forward word        |
| M-b       |                  | Backward word       |
| M->       |                  | End of buffer       |
| M-<       |                  | Beginning of buffer |
| C-<SPACE> |                  | Toggle mark         |
| C-M-SPACE |                  | Mark sexp           |
| RET       |                  | Follow org link     |
| C-%       |                  | Push org mark       |
| C-&       |                  | Goto org mark-ring  |
| TAB       |                  | Org cycle           |
| M-<TAB>   |                  | Toggle visibility   |
| M-w       |                  | Copy region         |
| q         |                  | Quit                |

** Customize Helm

Helm provides a lot of user variables for extensive customization.
From any Helm session, type \\<helm-map>\\[helm-customize-group]
to jump to the current source `custom' group.  Helm also has a
special group for faces you can access via `M-x customize-group
RET helm-faces'.

Note: Some sources may not have their group set and default to
the `helm' group.

** Display Helm in windows and frames

You can display the Helm completion buffer in many different
window configurations, see the custom interface to discover the
different windows configurations available (See [[Customize Helm][Customize Helm]] to jump to custom interface).
When using Emacs in a graphic display (i.e. not in a terminal) you can as
well display your Helm buffers in separated frames globally for
all Helm commands or separately for specific Helm commands.
See `helm-display-function' and `helm-commands-using-frame'.
See also [[https://github.com/emacs-helm/helm/wiki/frame][helm wiki]] for more infos.

There is a variable to allow reusing frame instead of deleting
and creating a new one at each session, see `helm-display-buffer-reuse-frame'.
Normally you don't have to use this, it have been made to workaround
slow frame popup in Emacs-26, to workaround this slowness in Emacs-26 use instead

#+begin_src elisp 
    (when (= emacs-major-version 26)
      (setq x-wait-for-event-timeout nil))
#+end_src

WARNING:
There is a package called Posframe and also one called Helm-posframe,
you DO NOT need these packages to display helm buffers in frames.
Thus Posframe package use child frames which have no minibuffers
and are by the way not compatible with Helm.

** Helm's basic operations and default key bindings

| Key| Command|
|----+--------|
|                                                                                                                  |
| \\[helm-previous-line]| Previous line                                                                            |
| \\[helm-next-line]| Next line                                                                                    |
| \\[helm-scroll-up]| Scroll up                                                                                    |
| \\[helm-scroll-down]| Scroll down                                                                                |
| \\[helm-scroll-other-window]| Scroll up other-window                                                             |
| \\[helm-scroll-other-window-down]| Scroll down other-window                                                      |
| \\[helm-maybe-exit-minibuffer]| Execute first (default) action / Select [1]                                      |
| \\[helm-beginning-of-buffer]| Goto beginning of buffer                                                           |
| \\[helm-end-of-buffer]| Goto end of buffer                                                                       |
| \\[helm-select-action]| Show actions menu                                                                        |
| \\[helm-previous-source]| Previous source                                                                        |
| \\[helm-next-source]| Next source                                                                                |
| \\[helm-delete-minibuffer-contents]| Delete pattern (with prefix arg delete from point to end or all [2])        |
| \\[helm-execute-persistent-action]| Persistent action (Execute and keep Helm session)                            |
|\\[helm-toggle-resplit-and-swap-windows]|Rotate or swap windows.                                                  |
|\\[helm-exchange-minibuffer-and-header-line]|Exchange minibuffer and header-line.                                 |
|\\[helm-quit-and-find-file]|Drop into `helm-find-files'.                                                          |
|\\[helm-kill-selection-and-quit]|Kill display value of candidate and quit (with prefix arg, kill the real value). |
|\\[helm-yank-selection]|Yank current selection into pattern.                                                      |
|\\[helm-insert-or-copy]|Insert or copy marked candidates (C-u) .                                                  |
|\\[helm-follow-mode]|Toggle automatic execution of persistent action.                                             |
|\\[helm-follow-action-forward]|Run persistent action then select next line.                                       |
|\\[helm-follow-action-backward]|Run persistent action then select previous line.                                  |
|\\[helm-refresh]|Recalculate and redisplay candidates.                                                            |
|\\[helm-toggle-suspend-update]|Toggle candidate updates.                                                          |

\[1] Behavior may change depending context in some source e.g. `helm-find-files'.

\[2] Delete from point to end or all depending on the value of
`helm-delete-minibuffer-contents-from-point'.

NOTE: Any of these bindings are from `helm-map' and may be
overriten by the map specific to the current source in use (each
source can have its own keymap).

** The actions menu

You can display the action menu in the same window
as helm candidates (default) or in a side window according to
`helm-show-action-window-other-window' value.

When the action menu popup, the helm prompt is used to narrow
down this menu, no more candidates.

When `helm-allow-mouse' is non nil, you can use as well
mouse-3 (right click) in the candidate zone to select actions
with the mouse once your candidate is selected.

** Action transformers

You may be surprized to see your actions list changing depending
on the context.  This happen when a source has an action
transformer function which checks the current selected candidate
and adds specific actions for this candidate.

** Shortcuts for n-th first actions

f1-f12: Execute n-th action where n is 1 to 12.

** Shortcuts for executing the default action on the n-th candidate

Helm does not display line numbers by default, with Emacs-26+ you
can enable it permanently in all helm buffers with:

    (add-hook 'helm-after-initialize-hook 'helm-init-relative-display-line-numbers)

You can also toggle line numbers with
\\<helm-map>\\[helm-display-line-numbers-mode] in current Helm
buffer.

Of course when enabling `global-display-line-numbers-mode' Helm
buffers will have line numbers as well. \(Don't forget to
customize `display-line-numbers-type' to relative).

In Emacs versions < to 26 you will have to use
[[https://github.com/coldnew/linum-relative][linum-relative]]
package and `helm-linum-relative-mode'.

Then when line numbers are enabled with one of the methods above
the following keys are available([1]):

C-x <n>: Execute default action on the n-th candidate before
currently selected candidate.

C-c <n>: Execute default action on the n-th candidate after
current selected candidate.

\"n\" is limited to 1-9.  For larger jumps use other navigation
keys.

\[1] Note that the key bindings are always available even if line
numbers are not displayed.  They are just useless in this case.

** Mouse control in Helm

A basic support for the mouse is provided when the user sets
`helm-allow-mouse' to non-nil.

- mouse-1 selects the candidate.
- mouse-2 executes the default action on selected candidate.
- mouse-3 pops up the action menu.

Note: When mouse control is enabled in Helm, it also lets you
click around and lose the minibuffer focus: you'll have to click
on the Helm buffer or the minibuffer to retrieve control of your
Helm session.

** Marked candidates

You can mark candidates to execute an action on all of them
instead of the current selected candidate only.  (See bindings
below.) Most Helm actions operate on marked candidates unless
candidate-marking is explicitely forbidden for a specific source.

- To mark/unmark a candidate, use
\\[helm-toggle-visible-mark].  (See bindings below.) With a
numeric prefix arg mark ARG candidates forward, if ARG is
negative mark ARG candidates backward.

- To mark all visible unmarked candidates at once in current
source use \\[helm-mark-all].  With a prefix argument, mark all
candidates in all sources.

- To unmark all visible marked candidates at once use
  \\[helm-unmark-all].

- To mark/unmark all candidates at once use
\\[helm-toggle-all-marks].  With a prefix argument, mark/unmark
all candidates in all sources.

Note: When multiple candidates are selected across different
sources, only the candidates of the current source will be used
when executing most actions (as different sources can have
different actions).  Some actions support multi-source marking
however.

** Follow candidates

When `helm-follow-mode' is on (\\<helm-map>\\[helm-follow-mode]
to toggle it), moving up and down Helm session or updating the
list of candidates will automatically execute the
persistent-action as specified for the current source.

If `helm-follow-mode-persistent' is non-nil, the state of the
mode will be restored for the following Helm sessions.

If you just want to follow candidates occasionally without
enabling `helm-follow-mode', you can use
\\<helm-map>\\[helm-follow-action-forward] or
\\[helm-follow-action-backward] instead.  Conversely, when
`helm-follow-mode' is enabled, those commands go to previous/next
line without executing the persistent action.

** Special yes, no or yes for all answers

You may be prompted in the minibuffer to answer by [y,n,!,q] in
some places for confirmation.

- y  mean yes
- no mean no
- !  mean yes for all
- q  mean quit or abort current operation.

When using ! you will not be prompted for the same thing in
current operation any more, e.g. file deletion, file copy etc...

** Moving in `helm-buffer'

You can move in `helm-buffer' with the usual commands used in
Emacs: \(\\<helm-map>\\[helm-next-line],
\\<helm-map>\\[helm-previous-line], etc.  See above basic
commands.  When `helm-buffer' contains more than one source,
change source with \\<helm-map>\\[helm-next-source] and
\\[helm-previous-source].

Note: When reaching the end of a source,
\\<helm-map>\\[helm-next-line] will *not* go to the next source
when variable `helm-move-to-line-cycle-in-source' is non-nil, so
you will have to use \\<helm-map>\\[helm-next-source] and
\\[helm-previous-source].

** Resume previous session from current Helm session

You can use `C-c n' (`helm-run-cycle-resume') to cycle in
resumables sources.  `C-c n' is a special key set with
`helm-define-key-with-subkeys' which, after pressing it, allows
you to keep cycling with further `n'.

Tip: You can bound the same key in `global-map' to
     `helm-cycle-resume' with `helm-define-key-with-subkeys' to
     let you transparently cycle sessions, Helm fired up or not.
     You can also bind the cycling commands to single key
     presses (e.g., `S-<f1>') this time with a simple
     `define-key'.  (Note that `S-<f1>' is not available in
     terminals.)

Note: `helm-define-key-with-subkeys' is available only once Helm
is loaded.

You can also use
\\<helm-map>\\[helm-resume-previous-session-after-quit] to resume
the previous session, or
\\<helm-map>\\[helm-resume-list-buffers-after-quit] to have
completion on all resumable buffers.

** Global commands

*** Resume Helm session from outside Helm

\\<global-map>\\[helm-resume] revives the last Helm session.
Binding a key to this command will greatly improve Helm
interactivity, e.g. when quitting Helm accidentally.

You can call \\<global-map>\\[helm-resume] with a prefix argument
to choose \(with completion!) which session you'd like to resume.
You can also cycle in these sources with `helm-cycle-resume' (see
above).

** Debugging Helm

Helm exposes the special variable `helm-debug': setting it to
non-nil will enable Helm logging in a special outline-mode
buffer.  Helm resets the variable to nil at the end of each
session.

For convenience, \\<helm-map>\\[helm-enable-or-switch-to-debug]
allows you to turn on debugging for this session only.  To avoid
accumulating log entries while you are typing patterns, you can
use \\<helm-map>\\[helm-toggle-suspend-update] to turn off
updating.  When you are ready turn it on again to resume logging.

Once you exit your Helm session you can access the debug buffer
with `helm-debug-open-last-log'.

Note: Be aware that Helm log buffers grow really fast, so use
`helm-debug' only when needed.

** Writing your own Helm sources

Writing simple sources for your own usage is easy.  When calling
the `helm' function, the sources are added the :sources slot
which can be a symbol or a list of sources.  Sources can be built
with different EIEIO classes depending on what you want to do.  To
simplify this, several `helm-build-*' macros are provided.  Below
there are simple examples to start with.

We will not go further here, see
[[https://github.com/emacs-helm/helm/wiki/Developing][Helm wiki]]
and the source code for more information and more complex
examples.

#+begin_src elisp

    ;; Candidates are stored in a list.
    (helm :sources (helm-build-sync-source \"test\"
                     ;; A function can be used as well
                     ;; to provide candidates.
                     :candidates '(\"foo\" \"bar\" \"baz\"))
          :buffer \"*helm test*\")

    ;; Candidates are stored in a buffer.
    ;; Generally faster but doesn't allow a dynamic updating
    ;; of the candidates list i.e the list is fixed on start.
    (helm :sources (helm-build-in-buffer-source \"test\"
                     :data '(\"foo\" \"bar\" \"baz\"))
          :buffer \"*helm test*\")

#+end_src

** Helm Map
\\{helm-map}"
  "Message string containing detailed help for `helm'.
It also accepts function or variable symbol.")

(defvar helm-autoresize-mode) ;; Undefined in `helm-default-display-buffer'.

(defvar helm-async-outer-limit-hook nil
  "Run in async sources when process output is out of `candidate-number-limit'.
Should be set locally to `helm-buffer' with `helm-set-local-variable'.")

(defvar helm-quit-hook nil
  "A hook that runs when quitting Helm.")

(defvar helm-resume-after-hook nil
  "A hook that runs after resuming a Helm session.
The hook should takes one arg SOURCES.")

(defvar helm-help-buffer-name "*Helm Help*"
  "The name of helm help buffer.")

;; See bug#2503.
(defvar helm-process-output-split-string-separator "\n"
  "Separator to use when splitting helm async output.")

(defvar helm-last-query ""
  "The value of `helm-pattern' is stored here exit or quit.")

;; Utility: logging
(defun helm-log (from format-string &rest args)
  "Log message if `helm-debug' is non-nil.
Messages are written to the `helm-debug-buffer' buffer.

FROM is the place from where it is called.
Argument FORMAT-STRING is a string to use with `format'.
Use optional arguments ARGS like in `format'."
  (when helm-debug
    (with-current-buffer (get-buffer-create helm-debug-buffer)
      (outline-mode)
      (buffer-disable-undo)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (let ((tm (current-time)))
                  (format (concat (if (string-match "Start session" format-string)
                                      "* " "** ")
                                  "%s.%06d (%s)\n %s\n")
                          (format-time-string "%H:%M:%S" tm)
                          (nth 2 tm)
                          from
                          (apply #'format (cons format-string args)))))))))

(defun helm-log-run-hook (from hook)
  "Run HOOK like `run-hooks' and log these actions to Helm log buffer.
FROM is the place from where it is called."
  (helm-log from "Executing %s with value = %S" hook (symbol-value hook))
  (helm-log from "Executing %s with global value = %S" hook (default-value hook))
  (run-hooks hook)
  (helm-log from "executed %s" hook))

(defun helm-log-error (from &rest args)
  "Accumulate error messages into `helm-issued-errors'.
FROM is the place from where it is called.
ARGS are args given to `format'.
E.g. (helm-log-error \"Error %s: %s\" (car err) (cdr err))."
  (apply 'helm-log from (concat "ERROR: " (car args)) (cdr args))
  (let ((msg (apply 'format args)))
    (unless (member msg helm-issued-errors)
      (cl-pushnew msg helm-issued-errors :test 'equal))))

;;;###autoload
(defun helm-debug-open-last-log ()
  "Open Helm log file or buffer of last Helm session."
  (interactive)
  (if helm--last-log-file
      (progn
        (find-file helm--last-log-file)
        (outline-mode) (view-mode 1) (visual-line-mode 1))
    (switch-to-buffer helm-debug-buffer)
    (view-mode 1) (visual-line-mode 1)))

(defun helm-print-error-messages ()
  "Print error messages in `helm-issued-errors'."
  (and helm-issued-errors
       (message "Helm issued errors: %s"
                (mapconcat 'identity (reverse helm-issued-errors) "\n"))))


;; Test tools
(defmacro with-helm-time-after-update (&rest body)
  (helm-with-gensyms (start-time time-elapsed)
    `(let ((,start-time (float-time)) ,time-elapsed)
       (add-hook 'helm-after-update-hook
                 (lambda ()
                   (setq ,time-elapsed (- (float-time) ,start-time))
                   (keyboard-quit)))
       (unwind-protect ,@body
         (remove-hook 'helm-after-update-hook
                      (lambda ()
                        (setq  ,time-elapsed (- (float-time) ,start-time))
                        (keyboard-quit))))
       ,time-elapsed)))


;; Helm API
(defmacro with-helm-default-directory (directory &rest body)
  (declare (indent 1) (debug t))
  `(let ((default-directory (or (and ,directory
                                     (file-name-as-directory ,directory))
                                default-directory)))
     ,@body))

(defun helm-default-directory ()
  "Return the local value of `default-directory' in `helm-buffer'."
  (buffer-local-value 'default-directory (get-buffer helm-buffer)))

(defmacro with-helm-temp-hook (hook &rest body)
  "Execute temporarily BODY as a function for HOOK."
  (declare (indent 1) (debug t))
  `(letrec ((helm--hook (lambda ()
                          (unwind-protect
                               (progn ,@body)
                            (remove-hook ,hook helm--hook)))))
     (push (cons helm--hook ,hook) helm--temp-hooks)
     (add-hook ,hook helm--hook)))

(defmacro with-helm-after-update-hook (&rest body)
  "Execute BODY at end of `helm-update'."
  (declare (indent 0) (debug t))
  `(with-helm-temp-hook 'helm-after-update-hook ,@body))

(defmacro with-helm-alive-p (&rest body)
  "Return error when BODY run outside Helm context."
  (declare (indent 0) (debug t))
  `(progn
     (if helm-alive-p
         (progn ,@body)
       (error "Running helm command outside of context"))))

(defmacro with-helm-in-frame (&rest body)
  "Execute Helm function in BODY displaying `helm-buffer' in separate frame."
  (declare (debug t) (indent 0))
  `(progn
     (helm-set-local-variable
      'helm-display-function 'helm-display-buffer-in-own-frame)
     ,@body))

(defmacro helm-make-command-from-action (symbol doc action &rest body)
  "Make a command SYMBOL from ACTION with docstring DOC.
The command SYMBOL will quit helm before execute.
Argument ACTION should be an existing helm action.
BODY form will run before calling action.

Example:

    (helm-make-command-from-action foo-command
        \"Docstring\"
      \\='foo-action
      (run body))

will produce a command like this:

    (defun foo-command ()
      \"docstring\"
      (interactive)
      (with-helm-alive-p
        (run body)
        (helm-exit-and-execute-action \\='foo-action)))

And automatically put the symbol \\='helm-only on SYMBOL."
  (declare (indent defun) (debug t))
  `(progn
     (defun ,symbol ()
       ,doc
       (interactive)
       (with-helm-alive-p
         (progn ,@body)
         (helm-exit-and-execute-action ,action)))
     (put ',symbol 'helm-only t)))

(defmacro helm-make-persistent-command-from-action (symbol doc psymbol action)
  "Make a persistent command SYMBOL bound to PSYMBOL from ACTION."
  (declare (indent defun) (debug t))
  `(progn
     (defun ,symbol ()
       ,doc
       (interactive)
       (with-helm-alive-p
         (helm-set-attr ,psymbol (cons ,action 'never-split))
         (helm-execute-persistent-action ,psymbol)))
     (put ',symbol 'helm-only t)))


;;; helm-attributes
;;
(defun helm-get-attr (attribute-name &optional source compute)
  "Get the value of ATTRIBUTE-NAME of SRC.

If SRC is omitted, use current source.

If COMPUTE is non-`nil' compute value of ATTRIBUTE-NAME with
`helm-interpret-value'.  COMPUTE can have also \\='ignorefn as value,
in this case `helm-interpret-value' will return a function as
value unchanged, but will eval a symbol which is bound.

You can use `setf' to modify value of ATTRIBUTE-NAME unless
COMPUTE is specified, if attribute ATTRIBUTE-NAME is not found in
SOURCE `setf' will create new attribute ATTRIBUTE-NAME with
specified value.  You can also use `helm-set-attr' to modify
ATTRIBUTE-NAME."
  (declare (gv-setter
            (lambda (val)
              `(let* ((src (or ,source (helm-get-current-source)))
                      (attr (assq ,attribute-name src)))
                 (cl-assert (null ,compute) nil
                            "`setf' can't set the computed value of attribute `%s'"
                            ,attribute-name)
                 (if attr
                     (setcdr attr ,val)
                   (and (setcdr src (cons (cons ,attribute-name ,val)
                                          (cdr src)))
                        ,val))))))
  (let ((src (or source (helm-get-current-source))))
    (helm-aif (assq attribute-name src)
        (if compute
            (helm-interpret-value (cdr it) src compute)
          (cdr it)))))

(defalias 'helm-attr 'helm-get-attr)
(make-obsolete 'helm-attr 'helm-get-attr "3.7.0")

(cl-defun helm-set-attr (attribute-name value
                                       &optional
                                       (src (helm-get-current-source)))
  "Set the value of ATTRIBUTE-NAME of source SRC to VALUE.

If ATTRIBUTE-NAME doesn't exists in source it is created with
value VALUE.  If SRC is omitted, use current source.  If operation
succeed, return value, otherwise nil.

Using this function is same as using `setf' on `helm-get-attr'."
  (setf (helm-get-attr attribute-name src) value))

(defalias 'helm-attrset 'helm-set-attr)
(make-obsolete 'helm-attrset 'helm-set-attr "3.7.0")

(defun helm-add-action-to-source (name fn source &optional index)
  "Add new action NAME linked to function FN to SOURCE.
Function FN should be a valid function that takes one arg i.e.
candidate, argument NAME is a string that will appear in action
menu and SOURCE should be an existing helm source already loaded.
If INDEX is specified, action is added to the action list at INDEX,
otherwise added at end.
This allows users to add specific actions to an existing source
without modifying source code."
  (let ((actions    (helm-get-attr 'action source 'ignorefn))
        (new-action (list (cons name fn))))
    (when (functionp actions)
      (setq actions (list (cons "Default action" actions))))
    (helm-set-attr 'action
                  (if index
                      (helm-append-at-nth actions new-action index)
                    (append actions new-action))
                  source)))

(defun helm-delete-action-from-source (action-or-name source)
  "Delete ACTION-OR-NAME from SOURCE.
ACTION-OR-NAME can either be the name of action or the symbol
function associated to name."
  (let* ((actions    (helm-get-attr 'action source 'ignorefn))
         (del-action (if (symbolp action-or-name)
                         (rassoc action-or-name actions)
                       (assoc action-or-name actions))))
    (helm-set-attr 'action (delete del-action actions) source)))

(cl-defun helm-add-action-to-source-if (name fn source predicate
                                             &optional (index 4) test-only)
  "Add new action NAME linked to function FN to SOURCE.
Action NAME will be available when the current candidate matches
PREDICATE.
This function adds an entry in the `action-transformer' attribute
of SOURCE (or creates one if not found).
Function PREDICATE must take one candidate as arg.
Function FN should be a valid function that takes one arg i.e.
candidate, argument NAME is a string that will appear in action
menu and SOURCE should be an existing Helm source already loaded.
If INDEX is specified, action is added in action list at INDEX.
Value of INDEX should be always >=1, default to 4.  This allows
user to add a specific `action-transformer' to an existing source
without modifying source code.

E.g.

Add the action \"Byte compile file async\" linked to function
`async-byte-compile-file' to source `helm-source-find-files' only
when predicate helm-ff-candidates-lisp-p returns non-nil:

\(helm-add-action-to-source-if \"Byte compile file async\"
                              \\='async-byte-compile-file
                              helm-source-find-files
                              \\='helm-ff-candidates-lisp-p)."
  (let* ((actions     (helm-get-attr 'action source 'ignorefn))
         (action-transformers (helm-get-attr 'action-transformer source))
         (new-action  (list (cons name fn)))
         (transformer (lambda (actions _candidate)
                        (let ((candidate (car (helm-marked-candidates))))
                          (cond ((funcall predicate candidate)
                                 (helm-append-at-nth
                                  actions new-action index))
                                (t actions))))))
    (when (functionp actions)
      (helm-set-attr 'action (list (cons "Default action" actions)) source))
    (when (or (symbolp action-transformers) (functionp action-transformers))
      (setq action-transformers (list action-transformers)))
    (if test-only                       ; debug
        (delq nil (append (list transformer) action-transformers))
      (helm-set-attr 'action-transformer
                    (helm-fast-remove-dups
                     (delq nil (append (list transformer) action-transformers))
                     :test 'equal)
                    source))))


;;; Source filter
;;
(defun helm-set-source-filter (sources)
  "Set the value of `helm-source-filter' to SOURCES and update.

This function sets a filter for Helm sources and it may be called
while Helm is running.  It can be used to toggle displaying of
sources dynamically.  For example, additional keys can be bound
into `helm-map' to display only the file-related results if there
are too many matches from other sources and you're after files
only:

Shift+F shows only file results from some sources:

\(define-key helm-map \"F\" \\='helm-my-show-files-only)

\(defun helm-my-show-files-only ()
  (interactive)
  (helm-set-source-filter \\='(\"File Name History\"
                                  \"Files from Current Directory\")))

Shift+A shows all results:

\(define-key helm-map \"A\" \\='helm-my-show-all)

\(defun helm-my-show-all ()
  (interactive)
  (helm-set-source-filter nil))

The -my- part is added to avoid collisions with
existing Helm function names."
  (with-helm-buffer
    (let ((cur-disp-sel (helm-get-selection nil t)))
      (set (make-local-variable 'helm-source-filter)
           (helm--normalize-filter-sources sources))
      (helm-log "helm-set-source-filter" "helm-source-filter = %S" helm-source-filter)
      ;; Use force-update to run init/update functions.
      (helm-update (and (stringp cur-disp-sel)
                        (regexp-quote cur-disp-sel))))))

(defun helm--normalize-filter-sources (sources)
  (cl-loop for s in sources collect
           (cl-typecase s
             (symbol (assoc-default 'name (symbol-value s)))
             (list   (assoc-default 'name s))
             (string s))))

(defun helm-set-sources (sources &optional no-init no-update)
  "Set SOURCES during `helm' invocation.
If NO-INIT is non-nil, skip executing init functions of SOURCES.
If NO-UPDATE is non-nil, skip executing `helm-update'."
  (with-current-buffer helm-buffer
    (setq helm-sources (helm-get-sources sources))
    (helm-log "helm-set-sources" "helm-sources = %S" helm-sources))
  (unless no-init (helm-compute-attr-in-sources 'init))
  (unless no-update (helm-update)))

(defun helm-show-all-candidates-in-source (arg)
  "Toggle all or only candidate-number-limit cands in current source.
With a numeric prefix arg show only the ARG number of candidates.
The prefix arg has no effect when toggling to only
candidate-number-limit."
  (interactive "p")
  (with-helm-alive-p
    (with-helm-buffer
      (if helm-source-filter
          (progn
            (setq-local helm-candidate-number-limit
                        (default-value 'helm-candidate-number-limit))
            (helm-set-source-filter nil))
        (with-helm-default-directory (helm-default-directory)
          (setq-local helm-candidate-number-limit (and (> arg 1) arg))
          (helm-set-source-filter
           (list (helm-get-current-source))))))))
(put 'helm-show-all-candidates-in-source 'helm-only t)

(defun helm-display-all-sources ()
  "Display all sources previously hidden by `helm-set-source-filter'."
  (interactive)
  (with-helm-alive-p
    (helm-set-source-filter nil)))
(put 'helm-display-all-sources 'helm-only t)


;;; Source infos fns.
;;
(defun helm-get-selection (&optional buffer force-display-part source)
  "Return the currently selected candidate from BUFFER.

If BUFFER is nil or unspecified, use `helm-buffer' as default value.

If FORCE-DISPLAY-PART is non-nil, return the display part of candidate.

If FORCE-DISPLAY-PART value is `withprop' the display part of
candidate is returned with its properties.

When FORCE-DISPLAY-PART is nil the real part of candidate is returned.

SOURCE default to current-source when unspecified but it is better to
specify SOURCE when it is already available to avoid to call
`helm-get-current-source' uselessly.

Note that FORCE-DISPLAY-PART when specified takes precedence over
`display-to-real' attribute, that's mean don't use FORCE-DISPLAY-PART
when you want the `display-to-real' function(s) to be applied."
  (with-current-buffer (or buffer helm-buffer)
    (unless (or (helm-empty-buffer-p (current-buffer))
                (helm-pos-header-line-p))
      (let* ((beg     (overlay-start helm-selection-overlay))
             (end     (overlay-end helm-selection-overlay))
             (disp-fn (if (eq force-display-part 'withprop)
                          'buffer-substring
                        'buffer-substring-no-properties))
             ;; If there is no selection at point, the
             ;; overlay is at its initial pos, (point-min)
             ;; (point-min), that's mean the helm-buffer
             ;; is not empty but have no selection yet,
             ;; this happen with grep sentinel sending an
             ;; error message in helm-buffer when no matches.
             (disp (unless (= beg end) (funcall disp-fn beg (1- end))))
             (src  (or source (helm-get-current-source)))
             (selection (helm-acond (force-display-part disp)
                                    ;; helm-realvalue always takes precedence
                                    ;; over display-to-real.
                                    ((get-text-property beg 'helm-realvalue) it)
                                    ((assoc-default 'display-to-real src)
                                     (helm-apply-functions-from-source source it disp))
                                    (t disp))))
        (unless (equal selection "")
          (helm-log "helm-get-selection" "selection = %S" selection)
          selection)))))

(defun helm-get-actions-from-current-source (&optional source)
  "Return the associated action for the selected candidate.
It is a function symbol (sole action) or list
of (action-display . function)."
  (unless (helm-empty-buffer-p (helm-buffer-get))
    (let* ((src                (or source (helm-get-current-source)))
           (marked             (helm-marked-candidates))
           (action-transformer (helm-get-attr 'action-transformer src))
           (actions            (helm-get-attr 'action src 'ignorefn)))
      (if action-transformer
          (helm-apply-functions-from-source
           src action-transformer actions
           ;; When there is marked candidates assume the set of
           ;; candidates user selected contains candidates of the same
           ;; type so that the actions added by transformer fit with
           ;; all marked (previously we were looping on each marked
           ;; but it is too costly for the benefit it brings).
           (car marked))
        actions))))

(defun helm-get-current-source ()
  "Return the source for the current selection.
Return nil when `helm-buffer' is empty."
  (or helm-current-source
      (with-helm-buffer
        (or (get-text-property (point) 'helm-cur-source)
            (progn
              ;; This is needed to not loose selection.
              (goto-char (overlay-start helm-selection-overlay))
              (let ((header-pos (or (helm-get-previous-header-pos)
                                    (helm-get-next-header-pos))))
                ;; Return nil when no--candidates.
                (when header-pos
                  (cl-loop with source-name = (save-excursion
                                                (goto-char header-pos)
                                                (helm-current-line-contents))
                           for source in helm-sources thereis
                           (and (equal (assoc-default 'name source) source-name)
                                source)))))))))

(defun helm-run-after-exit (function &rest args)
  "Execute FUNCTION with ARGS after exiting Helm.
The action is to call FUNCTION with arguments ARGS.
Unlike `helm-exit-and-execute-action', this can be used
to call non-actions functions with any ARGS or no ARGS at all.

Use this on commands invoked from key bindings, but not on action
functions invoked as action from the action menu, i.e. functions
called with RET."
  (helm-kill-async-processes)
  (helm-log "helm-run-after-exit" "function = %S" function)
  (helm-log "helm-run-after-exit" "args = %S" args)
  (helm-exit-and-execute-action
   (lambda (_candidate)
     (apply function args))))

(defun helm-exit-and-execute-action (action)
  "Exit current Helm session and execute ACTION.
Argument ACTION is a function called with one arg (candidate) and
part of the actions of current source.

Use this on commands invoked from key bindings, but not
on action functions invoked as action from the action menu,
i.e. functions called with RET."
  ;; If ACTION is not an action available in source 'action attribute,
  ;; return an error.  This allow to remove unneeded actions from
  ;; source that inherit actions from type, note that ACTION have to
  ;; be bound to a symbol and not to be an anonymous action
  ;; i.e. lambda or byte-code.
  (helm-log "helm-exit-and-execute-action" "Start executing action")
  (let ((actions (helm-get-actions-from-current-source)))
    (when actions
      (cl-assert (or (eq action actions)
                     ;; Compiled lambdas
                     (byte-code-function-p action)
                     ;; Natively compiled (libgccjit)
                     (helm-subr-native-elisp-p action)
                     ;; Lambdas
                     (and (listp action) (functionp action))
                     ;; One of current actions.
                     (rassq action actions))
                 nil "No such action `%s' for this source" action)))
  (setq helm-saved-action action)
  (setq helm-saved-selection (or (helm-get-selection) ""))
  (setq helm--executing-helm-action t)
  ;; When toggling minibuffer and header-line, we want next action
  ;; inherit this setting.
  (helm-set-local-variable 'helm-echo-input-in-header-line
                           (with-helm-buffer helm-echo-input-in-header-line))
  ;; Ensure next action use same display function as initial helm-buffer when
  ;; helm-actions-inherit-frame-settings is non nil.
  (when (and helm-actions-inherit-frame-settings
             helm--buffer-in-new-frame-p)
    (helm-set-local-variable 'helm-display-function
                             (with-helm-buffer helm-display-function)
                             'helm--last-frame-parameters
                             (with-helm-buffer
                               (helm--get-frame-parameters)))
    ;; The helm-buffer keeps `helm-display-function' and
    ;; `helm--get-frame-parameters' values during 0.5 seconds, just
    ;; the time to execute the possible helm action with those values.
    ;; If no helm based action run within 0.5 seconds, the next helm
    ;; session will have to resolve again those variable values.
    (run-with-idle-timer 0.5 nil
                         (lambda () (helm-set-local-variable 'helm-display-function nil
                                                             'helm--last-frame-parameters nil))))
  (helm-exit-minibuffer))

(defun helm--get-frame-parameters (&optional frame)
  (cl-loop with params = (frame-parameters frame)
           for p in helm--frame-default-attributes
           when (assq p params) collect it))

(defalias 'helm-run-after-quit 'helm-run-after-exit)
(make-obsolete 'helm-run-after-quit 'helm-run-after-exit "1.7.7")
(defalias 'helm-quit-and-execute-action 'helm-exit-and-execute-action)
(make-obsolete 'helm-quit-and-execute-action 'helm-exit-and-execute-action "1.7.7")

(defun helm-interpret-value (value &optional source compute)
  "Interpret VALUE as variable, function or literal and return it.
If VALUE is a function, call it with no arguments and return the value
unless COMPUTE value is \\='ignorefn.
If SOURCE compute VALUE for this source.
If VALUE is a variable, return the value.
If VALUE is a symbol, but it is not a function or a variable, cause an error.
Otherwise, return VALUE itself."
  (cond ((and source (functionp value) (not (eq compute 'ignorefn)))
         (helm-apply-functions-from-source source value))
        ((and (functionp value) (not (eq compute 'ignorefn)))
         (funcall value))
        ((and (symbolp value) (boundp value))
         (symbol-value value))
        ((and (symbolp value) (not (functionp value)))
         (error
          "helm-interpret-value: Symbol must be a function or a variable"))
        (t
         value)))

(defun helm-set-local-variable (&rest args)
  "Bind each pair in ARGS locally to `helm-buffer'.

Use this to set local vars before calling helm.

When used from an init or update function
\(i.e. when `helm-force-update' is running) the variables are set
using `make-local-variable' within the `helm-buffer'.

Usage: helm-set-local-variable ([VAR VALUE]...)
Just like `setq' except that the vars are not set sequentially.
IOW Don't use VALUE of previous VAR to set the VALUE of next VAR.

\(fn VAR VALUE ...)"
  (if helm--force-updating-p
      (with-helm-buffer
        (cl-loop for i on args by #'cddr
                 do (set (make-local-variable (car i)) (cadr i))))
    (setq helm--local-variables
          (append (cl-loop for i on args by #'cddr
                           collect (cons (car i) (cadr i)))
                  helm--local-variables))))

(defun helm--set-local-variables-internal ()
  (cl-loop for (var . val) in helm--local-variables
           ;; If `helm-set-local-variable' is called twice or more
           ;; on same variable use the last value entered which is
           ;; the first on stack e.g.
           ;; (helm-set-local-variable 'helm-foo 1)
           ;; (helm-set-local-variable 'helm-foo 2)
           ;; helm--local-variables =>
           ;; '((helm-foo . 2) (helm-foo. 1))
           ;; (helm-foo . 2) is retained and (helm-foo . 1)
           ;; ignored.
           unless (memq var computed)
           do (set (make-local-variable var) val)
           collect var into computed
           finally (setq helm--local-variables nil)))


;; API helper
(cl-defun helm-empty-buffer-p (&optional (buffer helm-buffer))
  "Check if BUFFER have candidates.
Default value for BUFFER is `helm-buffer'."
  (zerop (buffer-size (and buffer (get-buffer buffer)))))

(defun helm-empty-source-p ()
  "Check if current source contains candidates.
This could happen when for example the last element of a source
was deleted and the candidates list not updated."
  (when (helm-window)
    (with-helm-window
      (or (helm-empty-buffer-p)
          (and (helm-end-of-source-p)
               (eq (pos-bol) (pos-eol))
               (or
                (save-excursion
                  (forward-line -1)
                  (helm-pos-header-line-p))
                (bobp)))))))


;; Tools
;;
(defun helm-apply-functions-from-source (source functions &rest args)
  "From SOURCE apply FUNCTIONS on ARGS.

This function is used to process filter functions.  When filter is
a `filtered-candidate-transformer', we pass to ARGS
candidates+source whereas when the filter is
`candidate-transformer' we pass to ARGS candidates only.
This function is also used to process functions called with no
args, e.g. init functions.  In this case it is called without
ARGS.
See `helm-process-filtered-candidate-transformer'
    `helm-compute-attr-in-sources'
    `helm-process-candidate-transformer'.

Arg FUNCTIONS is either a symbol or a list of functions, each
function being applied on ARGS and called on the result of the
precedent function.  Return the result of last function call."
  (let ((helm--source-name (assoc-default 'name source))
        (helm-current-source source)
        (funs (if (functionp functions) (list functions) functions)))
    (cl-loop with result
             for fn in funs
             ;; In filter functions, ARGS is a list of one or two elements where
             ;; the first element is the list of candidates and the second
             ;; a list containing the source.
             do (setq result (apply fn args))
             when (and args (cdr funs))
             ;; When more than one fn, set the candidates list to what returns
             ;; this fn to compute the modified candidates with the next fn
             ;; and so on.
             do (setcar args result)
             finally return result)))

(defalias 'helm-funcall-with-source 'helm-apply-functions-from-source)
(make-obsolete 'helm-funcall-with-source 'helm-apply-functions-from-source "2.9.7")

(defun helm-compute-attr-in-sources (attr &optional sources)
  "Call the associated function(s) to ATTR for each source if any."
  (let ((sources (or (helm-get-sources sources)
                     ;; Fix error no buffer named *helm... by checking
                     ;; if helm-buffer exists.
                     (and (buffer-live-p (get-buffer (helm-buffer-get)))
                          ;; `helm-sources' are local to helm-buffer.
                          (with-helm-buffer helm-sources)))))
    (when sources
      (dolist (source sources)
        (helm-aif (assoc-default attr source)
            (helm-apply-functions-from-source source it))))))

(defalias 'helm-funcall-foreach 'helm-compute-attr-in-sources)
(make-obsolete 'helm-funcall-foreach 'helm-compute-attr-in-sources "2.9.7")

(defun helm-normalize-sources (sources)
  "If SOURCES is only one source, make a list of one element."
  (if (or (and sources (symbolp sources))
          (and (listp sources) (assq 'name sources)))
      (list sources)
    sources))

(defun helm-get-candidate-number (&optional in-current-source)
  "Return candidates number in `helm-buffer'.
If IN-CURRENT-SOURCE is provided return the number of candidates
of current source only."
  (with-helm-buffer
    (if (or (helm-empty-buffer-p)
            (helm-empty-source-p))
        0
      (save-excursion
        (helm-aif (and in-current-source (helm-get-previous-header-pos))
            (goto-char it)
          (goto-char (point-min)))
        (forward-line 1)
        (if (helm-pos-multiline-p)
            (cl-loop with count-multi = 1
                     while (and (not (if in-current-source
                                         (save-excursion
                                           (forward-line 2)
                                           (or (helm-pos-header-line-p) (eobp)))
                                       (eobp)))
                                (search-forward helm-candidate-separator nil t))
                     do (cl-incf count-multi)
                     finally return count-multi)
          (cl-loop with ln = 0
                   while (not (if in-current-source
                                  (or (helm-pos-header-line-p) (eobp))
                                (eobp)))
                   ;; Don't count empty lines maybe added by popup (bug#1370).
                   unless (or (eq (pos-bol) (pos-eol))
                              (helm-pos-header-line-p))
                   do (cl-incf ln)
                   do (forward-line 1) finally return ln))))))

;; Entry point
;; `:allow-nest' is not in this list because it is treated before.
(defconst helm-argument-keys
  '(:sources :input :prompt :resume
             :preselect :buffer :keymap :default :history))

;;;###autoload
(defun helm (&rest plist)
  "Main function to execute helm sources.

PLIST is a list like

\(:key1 val1 :key2 val2 ...)

 or

\(&optional sources input prompt resume preselect
            buffer keymap default history allow-nest).

** Keywords

Keywords supported:

- :sources
- :input
- :prompt
- :resume
- :preselect
- :buffer
- :keymap
- :default
- :history
- :allow-nest

Extra LOCAL-VARS keywords are supported, see the \"** Other
keywords\" section below.

Basic keywords are the following:

*** :sources

One of the following:

- List of sources
- Symbol whose value is a list of sources
- Alist representing a Helm source.
  - In this case the source has no name and is referenced in
    `helm-sources' as a whole alist.

*** :input

Initial input of minibuffer (temporary value of `helm-pattern')

*** :prompt

Minibuffer prompt. Default value is `helm--prompt'.

*** :resume

If t, allow resumption of the previous session of this Helm
command, skipping initialization.

If \\='noresume, this instance of `helm' cannot be resumed.

*** :preselect

Initially selected candidate (string or regexp).

*** :buffer

Buffer name for this Helm session. `helm-buffer' will take this value.

*** :keymap

\[Obsolete]

Keymap used at the start of this Helm session.

It is overridden by keymaps specified in sources, and is kept
only for backward compatibility.

Keymaps should be specified in sources using the :keymap slot
instead. See `helm-source'.

This keymap is not restored by `helm-resume'.

*** :default

Default value inserted into the minibuffer \ with
\\<minibuffer-local-map>\\[next-history-element].

It can be a string or a list of strings, in this case
\\<minibuffer-local-map>\\[next-history-element] cycles through
the list items, starting with the first.

If nil, `thing-at-point' is used.

If `helm-maybe-use-default-as-input' is non-nil, display is
updated using this value if this value matches, otherwise it is
ignored. If :input is specified, it takes precedence on :default.

*** :history

Minibuffer input, by default, is pushed to `minibuffer-history'.

When an argument HISTORY is provided, input is pushed to
HISTORY. HISTORY should be a valid symbol.

*** :allow-nest

Allow running this Helm command in a running Helm session.

** Other keywords

Other keywords are interpreted as local variables of this Helm
session. The `helm-' prefix can be omitted. For example,

\(helm :sources \\='helm-source-buffers-list
       :buffer \"*helm buffers*\"
       :candidate-number-limit 10)

Starts a Helm session with the variable
`helm-candidate-number-limit' set to 10.

** Backward compatibility

For backward compatibility, positional parameters are
supported:

\(helm sources input prompt resume preselect
       buffer keymap default history allow-nest)

However, the use of non-keyword args is deprecated.

\(fn &key SOURCES INPUT PROMPT RESUME PRESELECT BUFFER KEYMAP DEFAULT HISTORY ALLOW-NEST OTHER-LOCAL-VARS)"
  (let ((fn (cond ((or (and helm-alive-p (plist-get plist :allow-nest))
                       (and helm-alive-p (memq 'allow-nest plist)))
                   #'helm--nest)
                  ((keywordp (car plist))
                   #'helm)
                  (t #'helm-internal))))
    (if (and helm-alive-p (eq fn #'helm))
        (if (helm--alive-p)
            ;; A helm session is normally running.
            (error "Error: Trying to run helm within a running helm session")
          ;; A helm session is already running and user jump somewhere else
          ;; without deactivating it.
          (with-helm-buffer
            (prog1
                (message "Aborting an helm session running in background")
              ;; `helm-alive-p' will be reset in unwind-protect forms.
              (helm-keyboard-quit))))
      (if (keywordp (car plist))
          ;; Parse `plist' and move not regular `helm-argument-keys'
          ;; to `helm--local-variables', then calling helm on itself
          ;; with normal arguments (the non--arguments-keys removed)
          ;; will end up in [1].
          (progn
            (setq helm--local-variables
                  (append helm--local-variables
                          ;; Vars passed by keyword on helm call
                          ;; take precedence on same vars
                          ;; that may have been passed before helm call.
                          (helm-parse-keys plist)))
            (apply fn (mapcar (lambda (key) (plist-get plist key))
                              helm-argument-keys)))
        (apply fn plist))))) ; [1] fn == helm-internal.

(defun helm--alive-p ()
  "[INTERNAL] Check if `helm' is alive.
An Helm session is considered alive if `helm-alive-p' value is
non-nil, the `helm-buffer' is visible, and cursor is in the
minibuffer."
  (and helm-alive-p
       (get-buffer-window (helm-buffer-get) 'visible)
       (minibuffer-window-active-p (minibuffer-window))
       (minibufferp (current-buffer))))

(defun helm-parse-keys (keys)
  "Parse the KEYS arguments of `helm'.
Return only those keys not in `helm-argument-keys', prefix them
with \"helm\", and then convert them to an alist.  This allows
adding arguments that are not part of `helm-argument-keys', but
are valid helm variables nevertheless.  For example,
:candidate-number-limit is bound to `helm-candidate-number-limit'
in the source.

  (helm-parse-keys \\='(:sources ((name . \"test\")
                               (candidates . (a b c)))
                     :buffer \"toto\"
                     :candidate-number-limit 4))
  ==> ((helm-candidate-number-limit . 4))."

  (cl-loop for (key value) on keys by #'cddr
           for symname = (substring (symbol-name key) 1)
           for sym = (intern (if (string-match "^helm-" symname)
                                 symname
                               (concat "helm-" symname)))
           unless (memq key helm-argument-keys)
           collect (cons sym value)))

(defun helm--maybe-load-tramp-archive ()
  ;; Should fix bug#2393 and bug#2394.  `while-no-input-ignore-events'
  ;; is also let-bounded in `helm--maybe-use-while-no-input'.
  (let ((while-no-input-ignore-events
         (and (boundp 'while-no-input-ignore-events)
              (cons 'dbus-event while-no-input-ignore-events))))
    (unless helm--tramp-archive-maybe-loaded
      ;; This for Emacs-27 not requiring tramp-archive.
      (and (boundp 'tramp-archive-enabled)
           (require 'tramp-archive nil t))
      (setq helm--tramp-archive-maybe-loaded t))))

;;; Entry point helper
(defun helm-internal (&optional
                      sources input
                      prompt resume
                      preselect buffer
                      keymap default history)
  "The internal Helm function called by `helm'.
For SOURCES INPUT PROMPT RESUME PRESELECT BUFFER KEYMAP DEFAULT and
HISTORY args see `helm'."
  (cl-assert (or (stringp input)
                 (null input))
             nil "Error in %S buffer: Initial input should be a string or nil"
             buffer)
  ;; Set all windows NON dedicated to avoid headaches with PA and
  ;; helm-window (bug#2443)
  (cl-loop for win in (window-list nil 1)
           for state = (window-dedicated-p win)
           when state
           do (progn (set-window-dedicated-p win nil)
                     (push `(,win . ,state) helm--original-dedicated-windows-alist)))
  (unless helm--nested (setq helm-initial-frame (selected-frame)))
  ;; Launch tramp-archive with dbus-event in `while-no-input-ignore-events'.
  (helm--maybe-load-tramp-archive)
  ;; Activate the advices.
  ;; Advices will be available only in >=emacs-24.4, but
  ;; allow compiling without errors on lower emacs.
  (when (fboundp 'advice-add)
    (advice-add 'tramp-read-passwd :around #'helm--suspend-read-passwd)
    (advice-add 'ange-ftp-get-passwd :around #'helm--suspend-read-passwd)
    (advice-add 'epa-passphrase-callback-function
                :around #'helm--suspend-read-passwd)
    ;; Ensure linum-mode is disabled in Helm buffers to preserve
    ;; performances (Bug#1894).
    (advice-add 'linum-on :override #'helm--advice-linum-on '((depth . 100))))
  (helm-log "helm-internal" (concat "[Start session] " (make-string 41 ?+)))
  (helm-log "helm-internal" "prompt = %S" prompt)
  (helm-log "helm-internal" "preselect = %S" preselect)
  (helm-log "helm-internal" "buffer = %S" buffer)
  (helm-log "helm-internal" "keymap = %S" keymap)
  (helm-log "helm-internal" "default = %S" default)
  (helm-log "helm-internal" "history = %S" history)
  (setq helm--prompt (or prompt "pattern: "))
  (let ((non-essential t)
        ;; Prevent mouse jumping to the upper-right
        ;; hand corner of the frame (bug#1538).
        mouse-autoselect-window
        focus-follows-mouse
        mode-line-in-non-selected-windows
        minibuffer-completion-confirm
        (ori--minibuffer-follows-selected-frame
         (and (boundp 'minibuffer-follows-selected-frame)
              (default-toplevel-value 'minibuffer-follows-selected-frame)))
        (input-method-verbose-flag helm-input-method-verbose-flag)
        (helm-maybe-use-default-as-input
         (and (null input)
              (or helm-maybe-use-default-as-input ; it is let-bounded so use it.
                  (cl-loop for s in (helm-normalize-sources sources)
                           thereis (memq s helm-sources-using-default-as-input))))))
    (unwind-protect
        (condition-case-unless-debug _v
            (let ( ;; `helm--source-name' is non-`nil'
                  ;; when `helm' is invoked by action, reset it.
                  helm--source-name
                  helm-current-source
                  helm-in-persistent-action
                  helm--quit
                  (helm-buffer (or buffer helm-buffer)))
              (helm-initialize
               resume input default sources)
              ;; This allows giving the focus to a nested helm session which use
              ;; a frame, like completion in
              ;; `helm-eval-expression'. Unfortunately
              ;; `minibuffer-follows-selected-frame' is available only in
              ;; emacs-28+ (bug#2536).
              ;; When non-nil (the default) the current active
              ;; minibuffer is used in new frame, which is not what we
              ;; want in helm when starting from an active minibuffer,
              ;; either a helm minibuffer or something line M-:. 
              (and ori--minibuffer-follows-selected-frame
                   (setq minibuffer-follows-selected-frame
                         (unless (or helm--nested
                                     ;; Allow keeping initial minibuffer visible
                                     ;; e.g. completion-at-point from  M-:.
                                     (minibufferp helm-current-buffer))
                           t)))
              ;; We don't display helm-buffer here to avoid popping
              ;; up a window or a frame when exiting immediately when
              ;; only one candidate (this avoid having the helm frame
              ;; flashing), lets first compute candidates and if more
              ;; than one display helm-buffer (this is done later in
              ;; helm-read-from-minibuffer).
              (unless helm-execute-action-at-once-if-one
                (helm-display-buffer helm-buffer resume)
                (select-window (helm-window))
                (when (and resume helm-visible-mark-overlays)
                  (set-window-margins (selected-window)
                                      (+ (string-width helm-visible-mark-prefix)
                                         helm-left-margin-width))))
              ;; We are now in helm-buffer.
              (unless helm-allow-mouse
                (helm--remap-mouse-mode 1)) ; Disable mouse bindings.
              (add-hook 'post-command-hook 'helm--maybe-update-keymap)
              ;; Add also to update hook otherwise keymap is not updated
              ;; until a key is hitted (Bug#1670).
              (add-hook 'helm-after-update-hook 'helm--maybe-update-keymap)
              (add-hook 'post-command-hook 'helm--update-header-line)
              (helm-log "helm-internal" "show prompt")
              (unwind-protect
                  (helm-read-from-minibuffer
                   prompt input preselect
                   resume keymap default history)
                (helm-cleanup))
              (prog1
                  (unless helm--quit (helm-execute-selection-action))
                (helm-log "helm-internal" (concat "[End session] " (make-string 41 ?-)))))
          (quit
           (helm-restore-position-on-quit)
           (helm-log-run-hook "helm-internal" 'helm-quit-hook)
           (helm-log "helm-internal" (concat "[End session (quit)] " (make-string 34 ?-)))
           nil))
      (when (fboundp 'advice-remove)
        (advice-remove 'tramp-read-passwd #'helm--suspend-read-passwd)
        (advice-remove 'ange-ftp-get-passwd #'helm--suspend-read-passwd)
        (advice-remove 'epa-passphrase-callback-function #'helm--suspend-read-passwd)
        (advice-remove 'linum-on #'helm--advice-linum-on))
      (helm-log "helm-internal" "helm-alive-p = %S" (setq helm-alive-p nil))
      (helm--remap-mouse-mode -1)       ; Reenable mouse bindings.
      (setq helm-alive-p nil)
      (and ori--minibuffer-follows-selected-frame
           (set-default-toplevel-value 'minibuffer-follows-selected-frame
                                       ori--minibuffer-follows-selected-frame))
      ;; Prevent error "No buffer named *helm*" triggered by
      ;; `helm-set-local-variable'.
      (setq helm--force-updating-p nil)
      (setq helm--buffer-in-new-frame-p nil)
      ;; Reset helm-pattern so that lambda's using it
      ;; before running helm will not start with its old value.
      (setq helm-pattern "")
      (setq helm--ignore-errors nil
            helm-debug nil))))

(defun helm--advice-linum-on ()
  (unless (or (minibufferp)
              (string-match "\\`\\*helm" (buffer-name))
              (and (daemonp) (null (frame-parameter nil 'client))))
    (linum-mode 1)))

;;; Helm resume
;;
;;
(defun helm-resume (arg)
  "Resume a previous Helm session.
Call with a prefix arg to choose among existing Helm
buffers (sessions).  When calling from Lisp, specify a
`buffer-name' as a string with ARG."
  (interactive "P")
  (let (buffer
        cur-dir
        narrow-pos
        (helm-full-frame (default-value 'helm-full-frame))
        sources)
    (if arg
        (if (and (stringp arg) (bufferp (get-buffer arg)))
            (setq buffer arg)
          (setq buffer (helm-resume-select-buffer)))
      (setq buffer helm-last-buffer))
    (cl-assert buffer nil
               "helm-resume: No helm buffers found to resume")
    (setq sources (buffer-local-value
                   'helm-sources (get-buffer buffer)))
    ;; Reset `cursor-type' to nil as it have been set to t
    ;; when quitting previous session.
    (with-current-buffer buffer (setq cursor-type nil))
    (setq helm-full-frame (buffer-local-value
                           'helm-full-frame (get-buffer buffer)))
    (setq cur-dir (buffer-local-value
                   'default-directory (get-buffer buffer)))
    (setq helm-saved-selection nil
          helm-saved-action nil)
    (unless (buffer-live-p helm-current-buffer)
      ;; `helm-current-buffer' may have been killed.
      (setq helm-current-buffer (current-buffer)))
    (helm-aif (with-current-buffer buffer
                helm--current-buffer-narrowed)
        (progn
          (set-buffer (car it))
          (setq narrow-pos (cdr it))))
    ;; This happen when calling C-x b within helm.
    (helm-aif (get-buffer-window helm-marked-buffer-name 'visible)
        (progn (delete-window it) (kill-buffer helm-marked-buffer-name)))
    (save-restriction
      (when narrow-pos (apply #'narrow-to-region narrow-pos))
      ;; Restart with same `default-directory' value this session
      ;; was initially started with.
      (with-helm-default-directory cur-dir
        (unwind-protect
            (helm
             :sources sources
             :input (buffer-local-value 'helm-input-local (get-buffer buffer))
             :prompt (buffer-local-value 'helm--prompt (get-buffer buffer))
             :resume t
             :buffer buffer)
          (run-hook-with-args 'helm-resume-after-hook sources))))))

(defun helm-resume-previous-session-after-quit ()
  "Resume previous Helm session within a running Helm."
  (interactive)
  (with-helm-alive-p
    (let ((arg (if (null (member helm-buffer helm-buffers)) 0 1)))
      (if (> (length helm-buffers) arg)
          (helm-run-after-exit (lambda () (helm-resume (nth arg helm-buffers))))
        (message "No previous helm sessions available for resuming!")))))
(put 'helm-resume-previous-session-after-quit 'helm-only t)

(defun helm-resume-list-buffers-after-quit ()
  "List Helm buffers that can be resumed within a running Helm."
  (interactive)
  (with-helm-alive-p
    (if (> (length helm-buffers) 0)
        (helm-run-after-exit (lambda () (helm-resume t)))
      (message "No previous helm sessions available for resuming!"))))
(put 'helm-resume-list-buffers-after-quit 'helm-only t)

(defun helm-resume-p (resume)
  "Whether current Helm session is resumed or not."
  (eq resume t))

(defun helm-resume-select-buffer ()
  "Select an `helm-buffer' in `helm-buffers' list to resume a helm session.
Return nil if no `helm-buffer' found."
  (when helm-buffers
    (or (helm :sources (helm-build-sync-source "Resume helm buffer"
                         :candidates helm-buffers)
              :resume 'noresume
              :buffer "*helm resume*")
        (keyboard-quit))))

;;;###autoload
(defun helm-cycle-resume ()
  "Cycle in `helm-buffers' list and resume when waiting more than 1.2s."
  (interactive)
  (cl-assert (and helm-buffers helm-last-buffer)
             nil "No helm buffers to resume")
  ;; Setup a new iterator only on first hit on
  ;; `helm-run-cycle-resume', subsequents hits should reuse same
  ;; iterator.
  (unless (and (eq last-command 'helm-cycle-resume)
               helm--cycle-resume-iterator)
    (setq helm--cycle-resume-iterator
          (helm-iter-sub-next-circular
           helm-buffers helm-last-buffer :test 'equal)))
  (helm--resume-or-iter))

(defun helm--resume-or-iter (&optional from-helm)
  (message "Resuming helm buffer `%s'" helm-last-buffer)
  (if (sit-for helm-cycle-resume-delay)
      ;; Delay expire, run helm-resume.
      (if from-helm
          (helm-run-after-exit (lambda () (helm-resume helm-last-buffer)))
        (helm-resume helm-last-buffer))
    ;; key pressed before delay, cycle.
    (unless from-helm ; cycling to next item already done.
      (message "Resuming helm buffer `%s'"
               (setq helm-last-buffer
                     (helm-iter-next helm--cycle-resume-iterator))))))

(defun helm-run-cycle-resume ()
  "Same as `helm-cycle-resume' but intended to be called only from Helm."
  (interactive)
  (when (cdr helm-buffers) ; only one session registered.
    ;; Setup a new iterator only on first hit on
    ;; `helm-run-cycle-resume', subsequents hits should reuse same
    ;; iterator.
    (unless (and (eq last-command 'helm-run-cycle-resume)
                 helm--cycle-resume-iterator)
      (setq helm--cycle-resume-iterator
            (helm-iter-sub-next-circular
             helm-buffers helm-last-buffer :test 'equal)))
    ;; start at next buffer as we already are at `helm-last-buffer'.
    (setq helm-last-buffer
          (helm-iter-next helm--cycle-resume-iterator))
    (helm--resume-or-iter 'from-helm)))
(put 'helm-run-cycle-resume 'helm-only t)


;;; Nested sessions
;;
;;
(defun helm--nest (&rest same-as-helm)
  "[INTERNAL] Allow calling `helm' within a running Helm session.

Arguments SAME-AS-HELM are the same as `helm'.

Don't use this directly, use instead `helm' with the keyword
:allow-nest.

\(fn &key SOURCES INPUT PROMPT RESUME PRESELECT BUFFER KEYMAP DEFAULT HISTORY OTHER-LOCAL-VARS)"
  (with-helm-window
    (let ((orig-helm-current-buffer helm-current-buffer)
          (orig-helm-buffer helm-buffer)
          (orig-helm--prompt helm--prompt)
          (orig-helm-sources helm-sources)
          (orig-helm--in-fuzzy helm--in-fuzzy)
          (orig-helm--display-frame helm--buffer-in-new-frame-p)
          (orig-helm-last-frame-or-window-configuration
           helm-last-frame-or-window-configuration)
          (orig-one-window-p helm-onewindow-p)
          (helm--nested (if helm--buffer-in-new-frame-p 'share t)))
      ;; FIXME Using helm-full-frame here allow showing the new
      ;; helm-buffer in the same window as old helm-buffer, why?
      (helm-set-local-variable 'helm-full-frame t)
      (unwind-protect
          (let (helm-current-position
                helm-current-buffer
                helm-pattern
                (helm-buffer (or (cl-getf same-as-helm :buffer)
                                 (nth 5 same-as-helm)
                                 "*Helm*"))
                (enable-recursive-minibuffers t))
            (setq helm-sources nil)
            (apply #'helm same-as-helm))
        (with-current-buffer orig-helm-buffer
          (setq helm-sources orig-helm-sources)
          (setq helm--nested nil)
          (setq helm--buffer-in-new-frame-p orig-helm--display-frame)
          (setq helm-alive-p t) ; Nested session set this to nil on exit.
          (setq helm-buffer orig-helm-buffer)
          (setq helm-full-frame nil)
          (setq helm--prompt orig-helm--prompt)
          (setq helm--in-fuzzy orig-helm--in-fuzzy)
          (helm-initialize-overlays helm-buffer)
          (unless (helm-empty-buffer-p) (helm-mark-current-line t))
          (setq helm-last-frame-or-window-configuration
                orig-helm-last-frame-or-window-configuration)
          (setq cursor-type nil)
          (setq helm-current-buffer orig-helm-current-buffer)
          (setq helm-onewindow-p orig-one-window-p)
          ;; Be sure advices, hooks, and local modes keep running.
          (advice-add 'tramp-read-passwd
                      :around #'helm--suspend-read-passwd)
          (advice-add 'ange-ftp-get-passwd
                      :around #'helm--suspend-read-passwd)
          (advice-add 'epa-passphrase-callback-function
                      :around #'helm--suspend-read-passwd)
          (unless helm-allow-mouse
            (helm--remap-mouse-mode 1))
          (unless (cl-loop for h in post-command-hook
                           thereis (memq h '(helm--maybe-update-keymap
                                             helm--update-header-line)))
            (add-hook 'post-command-hook 'helm--maybe-update-keymap)
            (add-hook 'post-command-hook 'helm--update-header-line))
          (helm-display-mode-line (helm-get-current-source)))))))


;;; Windows and frames
;;
;;

(defun helm-frame-or-window-configuration (save-or-restore)
  "Save or restore last frame or window configuration.
Argument SAVE-OR-RESTORE is either save or restore of window or
frame configuration as per `helm-save-configuration-functions'."
  (helm-log "helm-frame-or-window-configuration" "helm-save-configuration-functions = %S"
            helm-save-configuration-functions)
  (let ((window-persistent-parameters (append '((no-other-window . t))
                                              window-persistent-parameters)))
    (cl-case save-or-restore
      (save    (setq helm-last-frame-or-window-configuration
                     (funcall (cdr helm-save-configuration-functions))))
      (restore (funcall (car helm-save-configuration-functions)
                        helm-last-frame-or-window-configuration)
               ;; Restore dedicated windows (bug#2443).
               (when helm--original-dedicated-windows-alist
                 (cl-loop for (win . state) in helm--original-dedicated-windows-alist
                          when (window-live-p win)
                          do (set-window-dedicated-p win state))
                 (setq helm--original-dedicated-windows-alist nil))
               ;; Restore frame focus.
               ;; This is needed for minibuffer own-frame config
               ;; when recursive minibuffers are in use.
               ;; e.g M-: + helm-minibuffer-history.
               (cl-letf ((frame (if (minibufferp helm-current-buffer)
                                    (selected-frame)
                                  (last-nonminibuffer-frame)))
                         ;; This is a workaround, because the i3 window
                         ;; manager developers are refusing to fix their
                         ;; broken timestamp and event handling.
                         ;;
                         ;; We basically just disable the part of
                         ;; select-frame-set-input-focus that would call
                         ;; XSetInputFocus in Xlib (x-focus-frame), that
                         ;; resets a timestamp in the xserver which the i3
                         ;; developers fail to notice.
                         ;;
                         ;; Since they don't know about the new timestamp,
                         ;; their keyboard handling can break after a helm
                         ;; user quits emacs, as reported in bug#1641.
                         ;;
                         ;; Fortunately for us, we really don't need this
                         ;; XSetInputFocus call, since we already have focus
                         ;; for Emacs, the user is just using helm!  We call
                         ;; select-frame-set-input-focus for the other
                         ;; side-effects, not for x-focus-frame.
                         ((symbol-function 'x-focus-frame) #'ignore))
                 (select-frame-set-input-focus frame))))))

(defun helm-split-window-default-fn (window)
  "Default function to split windows before displaying `helm-buffer'.

It is used as default value for
`helm-split-window-preferred-function' which is then the
let-bounded value of `split-window-preferred-function' in
`helm-display-buffer'.  When `helm-display-function' which default
to `helm-default-display-buffer' is called from
`helm-display-buffer' the value of
`split-window-preferred-function' will be used by
`display-buffer'."
  (let* ((split-width-threshold (and (integerp helm-split-width-threshold)
                                     helm-split-width-threshold))
         (win (if (and (fboundp 'window-in-direction)
                       ;; Don't try to split when starting in a minibuffer
                       ;; e.g M-: and try to use helm-show-kill-ring.
                       (not (minibufferp helm-current-buffer))
                       (null helm-split-width-threshold))
                  (if (or (one-window-p t)
                          helm-split-window-inside-p)
                      (split-window
                       (selected-window) nil
                       (if (eq helm-split-window-default-side 'other)
                           helm-split-window-other-side-when-one-window
                         helm-split-window-default-side))
                    ;; If more than one window reuse one of them.
                    (cl-case helm-split-window-default-side
                      (left  (or (helm-window-in-direction 'left)
                                 (helm-window-in-direction 'above)
                                 (selected-window)))
                      (above (or (helm-window-in-direction 'above)
                                 (helm-window-in-direction 'left)
                                 (selected-window)))
                      (right (or (helm-window-in-direction 'right)
                                 (helm-window-in-direction 'below)
                                 (selected-window)))
                      (below (or (helm-window-in-direction 'below)
                                 (helm-window-in-direction 'right)
                                 (selected-window)))
                      (same  (selected-window))
                      (other (or (helm-other-window-for-scrolling)
                                 (selected-window)))
                      (t     (or (window-next-sibling) (selected-window)))))
                (split-window-sensibly window))))
    (setq helm-persistent-action-window-buffer (window-buffer win))
    win))

(defun helm-window-in-direction (direction)
  "Same as `window-in-direction' but check if window is dedicated.
Return nil when window is dedicated."
  (helm-aif (window-in-direction direction)
      (and (not (window-dedicated-p it)) it)))

(defun helm-other-window-for-scrolling ()
  "Same as `other-window-for-scrolling' but check if window is dedicated.
Returns nil when window is dedicated."
  (helm-aif (other-window-for-scrolling)
      (and (not (window-dedicated-p it)) it)))

(defun helm-resolve-display-function (com)
  "Decide which display function to use according to `helm-commands-using-frame'.

The `helm-display-function' buffer local value takes precedence
on `helm-commands-using-frame'.
If `helm-initial-frame' has no minibuffer, use
`helm-display-buffer-in-own-frame' function.
Fallback to global value of `helm-display-function' when no local
value found and current command is not in
`helm-commands-using-frame'."
  (let ((win (get-buffer-window helm-current-buffer)))
    (or (with-helm-buffer helm-display-function)
        (and (or (memq com helm-commands-using-frame)
                 (and helm-use-frame-when-no-suitable-window
                      (or (window-dedicated-p win)
                          (window-parameter win 'window-side)))
                 (and helm-use-frame-when-more-than-two-windows
                      (null helm--nested)
                      (> (length (window-list)) 2))
                 ;; Frame parameter is unreliable for minibuffer on emacs-26.
                 (null (member helm-initial-frame (minibuffer-frame-list))))
             #'helm-display-buffer-in-own-frame)
        (default-value 'helm-display-function))))

(defun helm-display-buffer (buffer &optional resume)
  "Display BUFFER.

The function used to display `helm-buffer' by calling
`helm-display-function' which splits window with
`helm-split-window-preferred-function'."
  (let ((split-window-preferred-function
         helm-split-window-preferred-function)
        (helm-split-window-default-side
         (if (and (not helm-full-frame)
                  helm-reuse-last-window-split-state)
             (cond ((eq helm-split-window-default-side 'same) 'same)
                   ((eq helm-split-window-default-side 'other) 'other)
                   (helm--window-side-state)
                   (t helm-split-window-default-side))
           helm-split-window-default-side))
        (disp-fn (with-current-buffer buffer
                   (helm-resolve-display-function
                    (if helm-actions-inherit-frame-settings
                        (helm-this-command) this-command)))))
    (prog1
        (funcall disp-fn buffer (or (helm-resume-p resume)
                                    (and helm-actions-inherit-frame-settings
                                         helm--executing-helm-action)))
      (with-helm-buffer (setq-local helm-display-function disp-fn))
      (setq helm-onewindow-p (one-window-p t))
      ;; Don't allow other-window and friends switching out of minibuffer.
      (when helm-prevent-escaping-from-minibuffer
        (helm-prevent-switching-other-window)))))

(cl-defun helm-prevent-switching-other-window (&key (enabled t))
  "Allow setting `no-other-window' parameter for all windows.
Arg ENABLE is the value of `no-other-window' window property."
  (walk-windows
   (lambda (w)
     (unless (window-dedicated-p w)
       (set-window-parameter w 'no-other-window enabled)))
   0))

(defun helm-default-display-buffer (buffer &optional _resume)
  "Default function to display `helm-buffer' BUFFER.

It is the default value of `helm-display-function'.
It uses `switch-to-buffer' or `display-buffer' depending on the
value of `helm-full-frame' or `helm-split-window-default-side'."
  (let (pop-up-frames
        (curwin (get-buffer-window helm-current-buffer)))
    (if (or (buffer-local-value 'helm-full-frame (get-buffer buffer))
            (and (eq helm-split-window-default-side 'same)
                 (one-window-p t)))
        (progn (and (not (minibufferp helm-current-buffer))
                    ;; side-windows can't be the only window in frame,
                    ;; emacs refuse to delete other windows when
                    ;; current is a side-window [1].
                    (not (window-parameter curwin 'window-side))
                    (delete-other-windows))
               (switch-to-buffer buffer))
      (when (and (or helm-always-two-windows helm-autoresize-mode)
                 (not (eq helm-split-window-default-side 'same))
                 (not (minibufferp helm-current-buffer))
                 (not helm-split-window-inside-p)
                 ;; Same comment as in [1].
                 (not (window-parameter curwin 'window-side)))
        (delete-other-windows))
      (display-buffer
       buffer `(,helm-default-display-buffer-functions
                . ,(append helm-default-display-buffer-alist
                           `((window-height . ,helm-display-buffer-default-height)
                             (window-width  . ,helm-display-buffer-default-width)))))
      (helm-log-run-hook "helm-default-display-buffer" 'helm-window-configuration-hook))))

;; Shut up byte-compiler in emacs-26
(defvar tab-bar-mode)
;; No warnings in Emacs built --without-x
(defvar x-display-name)

(defun helm-display-buffer-in-own-frame (buffer &optional resume)
  "Display Helm buffer BUFFER in a separate frame.

Function suitable for `helm-display-function',
`helm-completion-in-region-display-function' and/or
`helm-show-completion-default-display-function'.

See `helm-display-buffer-height' and `helm-display-buffer-width'
to configure frame size.

Note that this feature is available only with emacs-25+.
Note also it is not working properly in helm nested session with emacs
version < emacs-28."
  (cl-assert (and (fboundp 'window-absolute-pixel-edges)
                  (fboundp 'frame-geometry))
             nil "Helm buffer in own frame is only available starting at emacs-25+")
  (if (not (display-graphic-p))
      ;; Fallback to default when frames are not usable.
      (helm-default-display-buffer buffer)
    (setq helm--buffer-in-new-frame-p t)
    (let* ((pos (window-absolute-pixel-position))
           (half-screen-size (/ (display-pixel-height x-display-name) 2))
           (frame-info (frame-geometry))
           (prmt-size (length helm--prompt))
           (line-height (frame-char-height))
           tab-bar-mode
           (new-frame-alist
             (if resume
                 (buffer-local-value 'helm--last-frame-parameters
                                     (get-buffer buffer))
               `((width . ,helm-display-buffer-width)
                 (height . ,helm-display-buffer-height)
                 (tool-bar-lines . 0)
                 (left . ,(- (car pos)
                             (* (frame-char-width)
                                (if (< (- (point) (pos-bol)) prmt-size)
                                    (- (point) (pos-bol))
                                  prmt-size))))
                 ;; Try to put frame at the best possible place.
                 ;; Frame should be below point if enough
                 ;; place, otherwise above point and
                 ;; current line should not be hidden
                 ;; by helm frame.
                 (top . ,(if (> (cdr pos) half-screen-size)
                             ;; Above point
                             (- (cdr pos)
                                ;; add 2 lines to make sure there is always a gap
                                (* (+ helm-display-buffer-height 2) line-height)
                                ;; account for title bar height too
                                (cddr (assq 'title-bar-size frame-info)))
                           ;; Below point
                           (+ (cdr pos) line-height)))
                 (title . "Helm")
                 (undecorated . ,helm-use-undecorated-frame-option)
                 (background-color . ,(or helm-frame-background-color
                                          (face-attribute 'default :background)))
                 (foreground-color . ,(or helm-frame-foreground-color
                                          (face-attribute 'default :foreground)))
                 (alpha . ,(or helm-frame-alpha 100))
                 (font . ,(assoc-default 'font (frame-parameters)))
                 (vertical-scroll-bars . nil)
                 (menu-bar-lines . 0)
                 (fullscreen . nil)
                 (visibility . ,(null helm-display-buffer-reuse-frame))
                 (minibuffer . t))))
           display-buffer-alist)
      ;; Display minibuffer above or below only in initial session,
      ;; not on a session triggered by action, this way if user have
      ;; toggled minibuffer and header-line manually she keeps this
      ;; setting in next action.
      (unless (or helm--executing-helm-action resume)
        ;; Add the hook inconditionally, if
        ;; helm-echo-input-in-header-line is nil helm-hide-minibuffer-maybe
        ;; will have anyway no effect so no need to remove the hook.
        (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
        (with-helm-buffer
          (setq-local helm-echo-input-in-header-line
                      (not (> (cdr pos) half-screen-size)))))
      (helm-display-buffer-popup-frame buffer new-frame-alist)
      ;; When frame size have been modified manually by user restore
      ;; it to default value unless resuming or not using
      ;; `helm-display-buffer-reuse-frame'.
      ;; This have to be done AFTER raising the frame otherwise
      ;; minibuffer visibility is lost until next session.
      (unless (or resume (not helm-display-buffer-reuse-frame))
        (set-frame-size helm-popup-frame
                        helm-display-buffer-width
                        helm-display-buffer-height)))
    (helm-log-run-hook "helm-display-buffer-in-own-frame" 'helm-window-configuration-hook)))

(defun helm-display-buffer-popup-frame (buffer frame-alist)
  (if helm-display-buffer-reuse-frame
      (let* ((x (cdr (assoc 'left frame-alist)))
             (y (cdr (assoc 'top frame-alist)))
             (width (cdr (assoc 'width frame-alist)))
             (height (cdr (assoc 'height frame-alist))))
        (unless (and helm-popup-frame
                     (frame-live-p helm-popup-frame))
          (setq helm-popup-frame (make-frame frame-alist)))
        (select-frame helm-popup-frame)
        (set-frame-position helm-popup-frame x y)
        (set-frame-width helm-popup-frame width)
        (set-frame-height helm-popup-frame height)
        (switch-to-buffer buffer)
        (select-frame-set-input-focus helm-popup-frame t))
    ;; If user have changed `helm-display-buffer-reuse-frame' to nil
    ;; maybe kill the frame.
    (when (and helm-popup-frame
               (frame-live-p helm-popup-frame))
      (delete-frame helm-popup-frame))
    (display-buffer
     buffer `(display-buffer-pop-up-frame
              . ((pop-up-frame-parameters . ,frame-alist))))))

;; Ensure to quit helm when user delete helm frame manually.
;; If user deletes another frame keep session running.
(defun helm--delete-frame-function (frame)
  (when (and helm-alive-p
             ;; FRAME is handling helm-buffer
             (get-buffer-window helm-buffer frame))
    (helm-keyboard-quit)))
(add-hook 'delete-frame-functions 'helm--delete-frame-function)

;;; Initialize
;;
(defun helm-get-sources (sources)
  "Transform each element of SOURCES in alist.
Return the resulting list."
  (when sources
    (mapcar (lambda (source)
              (if (listp source)
                  source (symbol-value source)))
            (helm-normalize-sources sources))))

(defun helm-initialize (resume input default sources)
  "Start initialization of Helm session.
For RESUME INPUT DEFAULT and SOURCES see `helm'."
  (helm-log "helm-initialize" "start initialization: resume=%S input=%S"
            resume input)
  (helm-frame-or-window-configuration 'save)
  (let ((sources-list (helm-get-sources sources)))
    (setq helm--in-fuzzy
          (cl-loop for s in sources-list
                   for matchfns = (helm-match-functions s)
                   for searchfns = (helm-search-functions s)
                   when (or (memq 'helm-fuzzy-match matchfns)
                            (memq 'helm-fuzzy-search searchfns))
                   return t))
    (helm-log "helm-initialize" "sources-list = %S" sources-list)
    (helm-set-local-variable 'helm-sources sources-list)
    ;; Once `helm-buffer' is created `helm-sources' will be a local
    ;; variable which value is a list of alists.
    (helm-current-position 'save)
    (if (helm-resume-p resume)
        (helm-initialize-overlays (helm-buffer-get))
      (helm-initial-setup input default sources-list))
    (setq helm-alive-p t)
    (unless (eq resume 'noresume)
      (helm--push-and-remove-dups helm-buffer 'helm-buffers)
      (setq helm-last-buffer helm-buffer))
    ;; If a `resume' attribute is present `helm-compute-attr-in-sources'
    ;; will run its function.
    (when (helm-resume-p resume)
      (helm-compute-attr-in-sources 'resume))
    (helm-log "helm-initialize" "end initialization")))

(defun helm-current-position (save-or-restore)
  "Save or restore current position in `helm-current-buffer'.
Argument SAVE-OR-RESTORE is either save or restore."
  (cl-case save-or-restore
    (save
     (helm-log "helm-current-position" "Save position at %S" (cons (point) (window-start)))
     (setq helm-current-position (cons (point) (window-start))))
    (restore
     ;; Maybe `helm-current-buffer' have been deleted
     ;; during helm session so check if it is here
     ;; otherwise position in underlying buffer will be lost.
     (when (get-buffer-window helm-current-buffer 'visible)
       (helm-log "helm-current-position" "Restore position at  %S in buffer %s"
                 helm-current-position
                 (buffer-name (current-buffer)))
       (goto-char (car helm-current-position))
       ;; Fix this position with the NOFORCE arg of `set-window-start'
       ;; otherwise, if there is some other buffer than `helm-current-buffer'
       ;; one, position will be lost.
       (set-window-start (selected-window) (cdr helm-current-position) t)))))

(defun helm-initialize-overlays (buffer)
  "Initialize Helm overlays in BUFFER."
  (helm-log "helm-initialize-overlays" "overlay setup")
  (if helm-selection-overlay
      ;; make sure the overlay belongs to the helm buffer if
      ;; it's newly created
      (move-overlay helm-selection-overlay (point-min) (point-min)
                    (get-buffer buffer))

    (setq helm-selection-overlay
          (make-overlay (point-min) (point-min) (get-buffer buffer)))
    (overlay-put helm-selection-overlay 'face 'helm-selection)
    (overlay-put helm-selection-overlay 'priority 1)))

(defun helm-initial-setup (input default sources)
  "Initialize Helm settings and set up the Helm buffer."
  ;; Run global hook.
  (helm-log-run-hook "helm-initial-setup" 'helm-before-initialize-hook)
  ;; Run local source hook.
  (helm--run-init-hooks 'before-init-hook sources)
  ;; For initialization of helm locals vars that need
  ;; a value from current buffer, it is here.
  (helm-set-local-variable 'current-input-method current-input-method)
  (setq helm-current-prefix-arg nil
        helm-saved-action nil
        helm-saved-selection nil
        helm-suspend-update-flag nil
        ;; Ensure this is called BEFORE selecting helm-window.
        helm-current-buffer (helm--current-buffer)
        helm-buffer-file-name buffer-file-name
        helm-issued-errors nil
        helm-saved-current-source nil
        helm--suspend-update-interactive-flag nil)
  (when (and (with-helm-current-buffer
               (and (buffer-narrowed-p)
                    (use-region-p)))
             (not helm--nested))
    (helm-set-local-variable 'helm--current-buffer-narrowed
                             (list (current-buffer)
                                   (region-beginning) (region-end))))
  (unless (and (or helm-split-window-state
                   helm--window-side-state)
               helm-reuse-last-window-split-state)
    ;; `helm-split-window-state' should be the contrary of what we currently
    ;; have to allow toggling windows with C-t.  This was influencing the
    ;; behavior of `helm-show-action-window-other-window' but we have now
    ;; removed this limitation, the action buffer beeing displayed 'below' when
    ;; helm-window is too narrow (vertical split). See bug#2635.
    (setq helm-split-window-state
          (if (or (null helm-split-window-default-side) ; same as below.
                  (memq helm-split-window-default-side '(below above))
                  (null helm-split-width-threshold)
                  (and (integerp helm-split-width-threshold)
                       (>= helm-split-width-threshold (+ (frame-width) 4))))
              'vertical 'horizontal))
    (setq helm--window-side-state
          (or helm-split-window-default-side 'below)))
  ;; Some sources like helm-mu are using input to init their
  ;; candidates in init function, so setup initial helm-pattern here.
  ;; See bug#2530 and https://github.com/emacs-helm/helm-mu/issues/54.
  ;; Input should have precedence on default.
  (cond (input
         (setq helm-input   input
               helm-pattern input))
        ((and default helm-maybe-use-default-as-input)
         (setq helm-pattern (if (listp default)
                                (car default)
                              default)
               ;; Even if helm-pattern is set we want the
               ;; prompt to be empty when using default as input, why
               ;; helm-input is initialized to "".
               helm-input ""))
        (helm-maybe-use-default-as-input
         (setq helm-pattern (or (with-helm-current-buffer
                                  (thing-at-point 'symbol))
                                "")
               helm-input ""))
        (t
         (setq helm-pattern ""
               helm-input   "")))
  (helm--fuzzy-match-maybe-set-pattern)
  ;; Call the init function for sources where appropriate
  (helm-compute-attr-in-sources 'init sources)
  (clrhash helm-candidate-cache)
  (helm-create-helm-buffer)
  (helm-clear-visible-mark)
  ;; Run global hook.
  (helm-log-run-hook "helm-initial-setup" 'helm-after-initialize-hook)
  ;; Run local source hook.
  (helm--run-init-hooks 'after-init-hook sources))

(defun helm--run-init-hooks (hook sources)
  "Run after and before init hooks local to source.
See :after-init-hook and :before-init-hook in `helm-source'."
  ;; We handle here incorrect values of hooks to not break packages using such
  ;; values i.e. lambda's or lists not bound to a symbol.  In the future we may
  ;; use `helm-log-run-hook' directly which allow using add-hook, remove-hook
  ;; etc...
  (cl-loop for s in sources
           for hv = (assoc-default hook s)
           when hv
           do (pcase hv
                ((and (pred (functionp))
                      (guard (not (symbolp hv)))) 
                 (funcall hv))
                ((and hook (pred (listp)))
                 (dolist (h hook) (funcall h)))
                (_ (helm-log-run-hook "helm--run-init-hooks" hv)))))

(defun helm-restore-position-on-quit ()
  "Restore position in `helm-current-buffer' when quitting."
  (helm-current-position 'restore))

(defun helm--push-and-remove-dups (elm sym)
  "Move ELM of SYM value on top and set SYM to this new value."
  (set sym (cons elm (delete elm (symbol-value sym)))))

(defun helm--current-buffer ()
  "[INTERNAL] Return `current-buffer' BEFORE `helm-buffer' is initialized.
Note that it returns the minibuffer in use after Helm has started
and is intended for `helm-initial-setup'.  To get the buffer where
Helm was started, use `helm-current-buffer' instead."
  (if (minibuffer-window-active-p (minibuffer-window))
      ;; If minibuffer is active be sure to use it's buffer
      ;; as `helm-current-buffer', this allow to use helm
      ;; from an already active minibuffer (M-: etc...)
      (window-buffer (active-minibuffer-window))
    ;; Fix Bug#456
    ;; Use this instead of `current-buffer' to ensure
    ;; helm session started in helm-mode from a completing-read
    ;; Use really the buffer where we started and not the one
    ;; where the completing-read is wrapped. i.e
    ;; (with-current-buffer SOME-OTHER-BUFFER (completing-read [...])
    (window-buffer (with-selected-window (minibuffer-window)
                     (minibuffer-selected-window)))))

(define-derived-mode helm-major-mode
  fundamental-mode "Hmm"
  "[INTERNAL] Provide major-mode name in Helm buffers.
Unuseful when used outside Helm, don't use it.")
(put 'helm-major-mode 'mode-class 'special)
(put 'helm-major-mode 'helm-only t)

(defun helm-create-helm-buffer ()
  "Create and setup `helm-buffer'."
  (let ((root-dir default-directory)
        (inhibit-read-only t))
    (with-current-buffer (get-buffer-create helm-buffer)
      (helm-log "helm-create-helm-buffer" "Enabling major-mode %S" major-mode)
      (helm-log "helm-create-helm-buffer" "kill local variables: %S" (buffer-local-variables))
      (kill-all-local-variables)
      (helm-major-mode)
      (set (make-local-variable 'buffer-read-only) nil)
      (buffer-disable-undo)
      (erase-buffer)
      ;; Use this instead of setting helm-map local ensure we have all
      ;; our keys when helm loose minibuffer focus.  And the map is
      ;; made local as well AFAIU.
      (use-local-map helm-map)
      (set (make-local-variable 'helm-source-filter) nil)
      (make-local-variable 'helm-sources)
      (set (make-local-variable 'helm-display-function) nil)
      (set (make-local-variable 'helm-selection-point) nil)
      (set (make-local-variable 'scroll-margin)
           (if helm-display-source-at-screen-top
               0 helm-completion-window-scroll-margin))
      (set (make-local-variable 'default-directory) root-dir)
      (set (make-local-variable 'helm-marked-candidates) nil)
      (set (make-local-variable 'helm--prompt) helm--prompt)
      (helm-initialize-persistent-action)
      (helm-log "helm-create-helm-buffer" "helm-display-function = %S" helm-display-function)
      (helm-log "helm-create-helm-buffer" "helm--local-variables = %S" helm--local-variables)
      (helm--set-local-variables-internal)
      (setq truncate-lines helm-truncate-lines) ; already local.
      (setq left-margin-width helm-left-margin-width)
      (setq cursor-type nil))
    (helm-initialize-overlays helm-buffer)
    (get-buffer helm-buffer)))

(define-minor-mode helm--minor-mode
  "[INTERNAL] Enable keymap in Helm minibuffer.
Since this mode has no effect when run outside of Helm context,
please don't use it outside of Helm.

\\{helm-map}"
  :group 'helm
  :keymap (and helm-alive-p helm-map)
  (unless helm-alive-p (setq helm--minor-mode nil)))
(put 'helm--minor-mode 'helm-only t)

(defun helm--reset-default-pattern ()
  (setq helm-pattern "")
  (setq helm-maybe-use-default-as-input nil))

(defun helm-read-from-minibuffer (prompt
                                input preselect resume
                                keymap default history)
  "Read pattern with prompt PROMPT and initial input INPUT.
For PRESELECT RESUME KEYMAP DEFAULT HISTORY, see `helm'."
  (with-helm-buffer
    (if (and (helm-resume-p resume)
             ;; When no source, helm-buffer is empty
             ;; or contain non--candidate lines (e.g grep exit status)
             (helm-get-current-source))
        (helm-mark-current-line t)
      (helm-update preselect))
    (let* ((src        (helm-get-current-source))
           (src-keymap (assoc-default 'keymap src))
           (hist       (or (and history (symbolp history) history)
                           ;; Needed for resuming.
                           (assoc-default 'history src)))
           (timer nil)
           blink-matching-paren
           (resize-mini-windows (and (null helm-echo-input-in-header-line)
                                     resize-mini-windows))
           (first-src (car helm-sources))
           (source-process-p (or (assq 'candidates-process src)
                                 (assq 'candidates-process first-src)))
           ;; As we are using `helm-keyboard-quit' for `C-g' we have
           ;; to prevent emacs command loop redefining `C-g' during
           ;; helm-session. This happen only on async source with
           ;; large output after a certain delay. The effect is that
           ;; the minibuffer is exited but the helm async process
           ;; continue running, and because minibuffer is lost `C-g'
           ;; have no more effect. By binding `inhibit-quit' here we
           ;; prevent this and allow `C-g' (the helm one aka
           ;; `helm-keyboard-quit') to quit immediately.
           (inhibit-quit source-process-p))
      (helm-log "helm-read-from-minibuffer" "helm-get-candidate-number => %S"
                (helm-get-candidate-number))
      (helm-log "helm-read-from-minibuffer" "helm-execute-action-at-once-if-one = %S"
                helm-execute-action-at-once-if-one)
      (helm-log "helm-read-from-minibuffer"
                "helm-quit-if-no-candidate = %S" helm-quit-if-no-candidate)
      (when (and src (helm-resume-p resume))
        (helm-display-mode-line src)
        (setq helm-pattern input))
      ;; Reset `helm-pattern' and update
      ;; display if no result found with precedent value of `helm-pattern'
      ;; unless `helm-quit-if-no-candidate' is non-`nil', in this case
      ;; Don't force update with an empty pattern.
      ;; Reset also `helm-maybe-use-default-as-input' as this checking
      ;; happen only on startup.
      (when helm-maybe-use-default-as-input
        ;; Store value of `default' temporarily here waiting next update
        ;; to allow actions like helm-moccur-action matching pattern
        ;; at the place it jump to.
        (setq helm-input helm-pattern)
        (if source-process-p
            ;; Reset pattern to next update.
            (with-helm-after-update-hook
              (helm--reset-default-pattern))
          ;; Reset pattern right now.
          (helm--reset-default-pattern))
        ;; Ensure force-update when no candidates
        ;; when we start with an empty pattern.
        (and (helm-empty-buffer-p)
             (null helm-quit-if-no-candidate)
             (helm-force-update preselect)))
      ;; Handle `helm-execute-action-at-once-if-one' and
      ;; `helm-quit-if-no-candidate' now.
      (cond ((and (if (functionp helm-execute-action-at-once-if-one)
                      (funcall helm-execute-action-at-once-if-one)
                    helm-execute-action-at-once-if-one)
                  (= (helm-get-candidate-number
                      (eq helm-execute-action-at-once-if-one 'current-source))
                     1))
             (ignore))              ; Don't enter the minibuffer loop.
            ((and helm-quit-if-no-candidate
                  (= (helm-get-candidate-number) 0))
             (setq helm--quit t)
             (and (functionp helm-quit-if-no-candidate)
                  (funcall helm-quit-if-no-candidate)))
            (t              ; Enter now minibuffer and wait for input.
             (let ((tap (or default
                            (with-helm-current-buffer
                              (thing-at-point 'symbol)))))
               (when helm-execute-action-at-once-if-one
                 (helm-display-buffer helm-buffer resume)
                 (select-window (helm-window)))
               (unwind-protect
                   (minibuffer-with-setup-hook
                       (lambda ()
                         ;; Start minor-mode with global value of helm-map.
                         (helm--minor-mode 1)
                         ;; Now override the global value of `helm-map' with
                         ;; the local one which is in this order:
                         ;; - The keymap of current source.
                         ;; - The value passed in KEYMAP
                         ;; - Or fallback to the global value of helm-map.
                         (helm--maybe-update-keymap
                          (or src-keymap keymap helm-map))
                         (helm-log-run-hook "helm-read-from-minibuffer"
                                            'helm-minibuffer-set-up-hook)
                         (setq timer
                               (run-with-idle-timer
                                (max (with-helm-buffer helm-input-idle-delay)
                                     0.001)
                                'repeat
                                (lambda ()
                                  ;; Stop updating in persistent action
                                  ;; or when `helm-suspend-update-flag'
                                  ;; is non-`nil'.
                                  (unless (or helm-in-persistent-action
                                              helm-suspend-update-flag)
                                    (save-selected-window
                                      (helm-check-minibuffer-input)
                                      (helm-print-error-messages))))))
                         ;; minibuffer has already been filled here.
                         (helm--update-header-line))
                     (read-from-minibuffer (propertize (or prompt "pattern: ")
                                                       'face 'helm-minibuffer-prompt)
                                           input helm-map
                                           nil hist tap
                                           helm-inherit-input-method))
                 (when timer (cancel-timer timer) (setq timer nil)))))))))

(defun helm-toggle-suspend-update ()
  "Enable or disable display update in helm.
This can be useful for example for quietly writing a complex
regexp without Helm constantly updating."
  (interactive)
  (helm-suspend-update (not helm-suspend-update-flag) t)
  (setq helm--suspend-update-interactive-flag
        (not helm--suspend-update-interactive-flag)))
(put 'helm-toggle-suspend-update 'helm-only t)

(defun helm-suspend-update (arg &optional verbose)
  "Enable or disable display update in helm.
If ARG is 1 or non nil suspend update, if it is -1 or nil reenable
updating.  When VERBOSE is specified display a message."
  (with-helm-buffer
    (when (setq helm-suspend-update-flag
                (helm-acase arg
                  (1 t)
                  (-1 nil)
                  (t it)))
      (helm-kill-async-processes)
      (setq helm-pattern ""))
    (when verbose
      (message (if helm-suspend-update-flag
                   "Helm update suspended!"
                 "Helm update re-enabled!")))
    (helm-aif (helm-get-current-source)
        (helm-display-mode-line it t))))

(defun helm-delete-backward-no-update (arg)
  "Disable update and delete ARG chars backward.
Update is reenabled when idle 1s."
  (interactive "p")
  (with-helm-alive-p
    (unless helm--suspend-update-interactive-flag
      (helm-suspend-update 1))
    (delete-char (- arg))
    (run-with-idle-timer
     1 nil
     (lambda ()
       (unless helm--suspend-update-interactive-flag
         (helm-suspend-update -1)
         (helm-check-minibuffer-input)
         (helm-force-update))))))
(put 'helm-delete-backward-no-update 'helm-only t)

(defun helm-delete-char-backward (arg)
  "Delete char backward and update when reaching prompt."
  (interactive "p")
  (condition-case _err
      (delete-char (- arg))
    (buffer-read-only
     (progn
       (helm-update)
       (helm-reset-yank-point)))))
(put 'helm-delete-char-backward 'helm-only t)

(defun helm--suspend-read-passwd (old--fn &rest args)
  "Suspend Helm while reading password.
This is used to advice `tramp-read-passwd', `ange-ftp-get-passwd'
and `epa-passphrase-callback-function'."
  ;; Suspend update when prompting for a tramp password.
  (setq helm-suspend-update-flag t)
  (setq overriding-terminal-local-map nil)
  (setq helm--reading-passwd-or-string t)
  (unwind-protect
      ;; No need to suspend timer in emacs-24.4
      ;; it is fixed upstream.
      (apply old--fn args)
    (setq helm--reading-passwd-or-string nil)
    (setq helm-suspend-update-flag nil)))

(defun helm--maybe-update-keymap (&optional map)
  "Handle different keymaps in multiples sources.

Overrides `helm-map' with the local map of current source.  If no
map is found in current source, does nothing (keeps previous
map)."
  (with-helm-buffer
    (helm-aif (or map (assoc-default 'keymap (helm-get-current-source)))
        ;; We used a timer in the past to leave
        ;; enough time to helm to setup its keymap
        ;; when changing source from a recursive minibuffer.
        ;; e.g C-x C-f M-y C-g
        ;; => *find-files have now the bindings of *kill-ring.
        ;; It is no more true now we are using `minor-mode-overriding-map-alist'
        ;; and `helm--minor-mode' thus it fix Bug#1076 for emacs-24.3
        ;; where concurrent timers are not supported.
        ;; i.e update keymap+check input.
        (with-current-buffer (window-buffer (minibuffer-window))
          (setq minor-mode-overriding-map-alist `((helm--minor-mode . ,it)))))))

;;; Prevent loosing focus when using mouse.
;;
(defvar helm--remap-mouse-mode-map
  (let ((map (make-sparse-keymap)))
    (cl-loop for k in '([mouse-1] [mouse-2] [mouse-3]
                        [down-mouse-1] [down-mouse-2] [down-mouse-3]
                        [drag-mouse-1] [drag-mouse-2] [drag-mouse-3]
                        [double-mouse-1] [double-mouse-2] [double-mouse-3]
                        [triple-mouse-1] [triple-mouse-2] [triple-mouse-3])
             do (define-key map k 'ignore))
    map))

(define-minor-mode helm--remap-mouse-mode
  "[INTERNAL] Prevent escaping helm minibuffer with mouse clicks.
Do nothing when used outside of helm context.

WARNING: Do not use this mode yourself, it is internal to Helm."
  :group 'helm
  :global t
  :keymap helm--remap-mouse-mode-map
  (unless helm-alive-p
    (setq helm--remap-mouse-mode-map nil)))
(put 'helm--remap-mouse-mode 'helm-only t)

;; Clean up

(defun helm-cleanup ()
  "Clean up the mess when Helm exit or quit."
  (helm-log "helm-cleanup" "start cleanup")
  (with-selected-window
      ;; When exiting with `helm-execute-action-at-once-if-one',
      ;; `helm-window' may not be created and we endup with an error
      ;; e.g. in eshell completion when only one candidate to complete
      ;; so fallback to selected-window in such cases.
      (or (get-buffer-window helm-buffer)
          (selected-window))
    (let ((frame (selected-frame)))
      (setq cursor-type (default-value 'cursor-type))
      ;; Ensure restoring default-value of mode-line to allow user
      ;; using the mouse when helm is inactive (Bug#1517,Bug#2377).
      (setq mode-line-format (default-value 'mode-line-format))
      (remove-hook 'post-command-hook 'helm--maybe-update-keymap)
      (remove-hook 'post-command-hook 'helm--update-header-line)
      ;; Be sure we call cleanup functions from helm-buffer.
      (helm-compute-attr-in-sources 'cleanup)
      ;; Delete or make invisible helm frame.
      (if (and helm--buffer-in-new-frame-p
               ;; a helm session running in a frame that runs a nested
               ;; session share the same frame for both sessions so
               ;; don't delete the common frame.
               ;; i.e. helm--nested == t   => delete
               ;;      helm--nested == nil => delete
               ;;      helm--nested == share => don't delete
               (not (eq helm--nested 'share)))
          (progn
            (setq-local helm--last-frame-parameters
                        (helm--get-frame-parameters))
            (bury-buffer)
            (if helm-display-buffer-reuse-frame
                (make-frame-invisible frame) (delete-frame frame)))
        ;; bury-buffer from this window [1].
        ;; Do it at end to make sure buffer is still current.
        (bury-buffer))))
  (helm-kill-async-processes)
  ;; Remove the temporary hooks added
  ;; by `with-helm-temp-hook' that
  ;; may not have been consumed.
  (when helm--temp-hooks
    (cl-loop for (fn . hook) in helm--temp-hooks
             do (remove-hook hook fn))
    (setq helm--temp-hooks nil))
  ;; When running helm from a dedicated frame
  ;; with no minibuffer, helm will run in the main frame
  ;; which have a minibuffer, so be sure to disable
  ;; the `no-other-window' prop there.
  (helm-prevent-switching-other-window :enabled nil)
  (helm-log-run-hook "helm-cleanup" 'helm-cleanup-hook)
  (helm-frame-or-window-configuration 'restore)
  ;; [1] now bury-buffer from underlying windows otherwise,
  ;; if this window is killed the underlying buffer will
  ;; be a helm buffer.
  (replace-buffer-in-windows helm-buffer)
  (setq helm-alive-p nil)
  ;; Prevent error "No buffer named *helm*" triggered by
  ;; `helm-set-local-variable'.
  (setq helm--force-updating-p nil)
  (setq helm--buffer-in-new-frame-p nil)
  ;; No need to reinitialize helm-pattern here now it is done only
  ;; once in init function bug#2530.
  (setq helm-last-query helm-pattern)
  ;; This is needed in some cases where last input
  ;; is yielded infinitely in minibuffer after helm session.
  (helm-clean-up-minibuffer))

(defun helm-clean-up-minibuffer ()
  "Remove contents of minibuffer."
  (let ((miniwin (minibuffer-window)))
    ;; Clean only current minibuffer used by helm.
    ;; i.e The precedent one is active.
    (unless (minibuffer-window-active-p miniwin)
      (with-current-buffer (window-buffer miniwin)
        (delete-minibuffer-contents)))))


;;; Input handling
;;
;;
(defun helm-check-minibuffer-input ()
  "Check minibuffer content."
  (with-selected-window (or (active-minibuffer-window)
                            (minibuffer-window))
    (helm-check-new-input (minibuffer-contents))))

(defun helm-check-new-input (input)
  "Check INPUT string and update the helm buffer if necessary."
  (unless (equal input helm-pattern)
    (setq helm-pattern input)
    (unless (helm-action-window)
      (setq helm-input helm-pattern))
    (helm-log "helm-check-new-input" "helm-pattern = %S" helm-pattern)
    (helm-log "helm-check-new-input" "helm-input = %S" helm-input)
    (helm-log-run-hook "helm-check-new-input" 'helm-before-update-hook)
    (setq helm--in-update t)
    (helm-update)))

(defun helm--reset-update-flag ()
  (run-with-idle-timer
   helm-exit-idle-delay nil
   (lambda () (setq helm--in-update nil))))

;; (add-hook 'helm-after-update-hook #'helm--reset-update-flag)


;; All candidates

(defun helm-get-candidates (source)
  "Retrieve and return the list of candidates from SOURCE."
  (let* ((candidate-fn (assoc-default 'candidates source))
         (candidate-proc (assoc-default 'candidates-process source))
         ;; See comment in helm-get-cached-candidates (Bug#2113).
         (inhibit-quit candidate-proc)
         cfn-error
         (notify-error
          (lambda (&optional e)
            (error
             "In `%s' source: `%s' %s %s"
             (assoc-default 'name source)
             (or candidate-fn candidate-proc)
             (if e "\n" "must be a list, a symbol bound to a list, or a function returning a list")
             (if e (prin1-to-string e) ""))))
         (candidates (condition-case-unless-debug err
                         ;; Process candidates-(process) function
                         ;; It may return a process or a list of candidates.
                         (if candidate-proc
                             ;; Calling `helm-interpret-value' with no
                             ;; SOURCE arg force the use of `funcall'
                             ;; and not `helm-apply-functions-from-source'.
                             (helm-interpret-value candidate-proc)
                           (helm-interpret-value candidate-fn source))
                       (error (helm-log "helm-get-candidates"
                                        "Error: %S" (setq cfn-error err))
                              nil))))
    (cond ((and (processp candidates) (not candidate-proc))
           (warn "Candidates function `%s' should be called in a `candidates-process' attribute"
                 candidate-fn))
          ((and candidate-proc (not (processp candidates)))
           (error "Candidates function `%s' should run a process" candidate-proc)))
    (cond ((processp candidates)
           ;; Candidates will be filtered later in process filter.
           candidates)
          ;; An error occured in candidates function.
          (cfn-error (unless helm--ignore-errors
                       (funcall notify-error cfn-error)))
          ;; Candidates function returns no candidates.
          ((or (null candidates)
               ;; Can happen when the output of a process
               ;; is empty, and the candidates function call
               ;; something like (split-string (buffer-string) "\n")
               ;; which result in a list of one empty string (Bug#938).
               ;; e.g (completing-read "test: " '(""))
               (equal candidates '("")))
           nil)
          ((listp candidates)
           ;; Transform candidates with `candidate-transformer' functions or
           ;; `real-to-display' functions if those are found,
           ;; otherwise return candidates unmodified.
           ;; `filtered-candidate-transformer' is NOT called here.
           (helm-transform-candidates candidates source))
          (t (funcall notify-error)))))

(defun helm-get-cached-candidates (source)
  "Return the cached value of candidates for SOURCE.
Cache the candidates if there is no cached value yet."
  (let* ((name (assoc-default 'name source))
         (candidate-cache (gethash name helm-candidate-cache))
         ;; Bind inhibit-quit to ensure function terminate in case of
         ;; quit from `helm-while-no-input' and processes are added to
         ;; helm-async-processes for further deletion (Bug#2113).
         ;; FIXME: Is this still needed now `helm-while-no-input'
         ;; handles quit-flag?
         (inhibit-quit (assoc-default 'candidates-process source)))
    (helm-aif candidate-cache
        (prog1 it (helm-log "helm-get-cached-candidates"
                            "Use cached candidates"))
      (helm-log "helm-get-cached-candidates"
                "No cached candidates, calculate candidates")
      (let ((candidates (helm-get-candidates source)))
        (cond ((processp candidates)
               (push (cons candidates
                           (append source
                                   (list (cons 'item-count 0)
                                         (cons 'incomplete-line ""))))
                     helm-async-processes)
               (set-process-filter candidates 'helm-output-filter)
               (setq candidates nil))
              ((not (assq 'volatile source))
               (puthash name candidates helm-candidate-cache)))
        candidates))))


;;; Candidate transformers

(defun helm-process-candidate-transformer (candidates source)
  "Execute `candidate-transformer' function(s) on CANDIDATES in SOURCE."
  (helm-aif (assoc-default 'candidate-transformer source)
      (helm-apply-functions-from-source source it candidates)
    candidates))

(defun helm-process-filtered-candidate-transformer (candidates source)
  "Execute `filtered-candidate-transformer' function(s) on CANDIDATES in SOURCE."
  (helm-aif (assoc-default 'filtered-candidate-transformer source)
      (helm-apply-functions-from-source source it candidates source)
    candidates))

(defmacro helm--maybe-process-filter-one-by-one-candidate (candidate source)
  "Execute `filter-one-by-one' function(s) on real value of CANDIDATE in SOURCE."
  `(helm-aif (assoc-default 'filter-one-by-one ,source)
       (let ((real (if (consp ,candidate)
                       (cdr ,candidate)
                     ,candidate)))
         (if (and (listp it)
                  (not (functionp it))) ;; Don't treat lambda's as list.
             (cl-loop for f in it
                      do (setq ,candidate (funcall f real))
                      finally return ,candidate)
           (setq ,candidate (funcall it real))))
     ,candidate))

(defun helm--initialize-one-by-one-candidates (candidates source)
  "Process CANDIDATES with the `filter-one-by-one' function in SOURCE.
Return CANDIDATES unchanged when pattern is not empty."
  (helm-aif (and (string= helm-pattern "")
                 (assoc-default 'filter-one-by-one source))
      (cl-loop for cand in candidates collect
               (helm--maybe-process-filter-one-by-one-candidate cand source))
    candidates))

(defun helm-process-filtered-candidate-transformer-maybe
    (candidates source process-p)
  "Execute `filtered-candidate-transformer' function(s) on CANDIDATES in SOURCE.
When PROCESS-P is non-nil execute
`filtered-candidate-transformer' functions if some, otherwise
return CANDIDATES."
  (if process-p
      ;; When no filter return CANDIDATES unmodified.
      (helm-process-filtered-candidate-transformer candidates source)
    candidates))

(defun helm-process-real-to-display (candidates source)
  "Execute real-to-display function on all CANDIDATES of SOURCE."
  (helm-aif (assoc-default 'real-to-display source)
      (setq candidates (helm-apply-functions-from-source
                        source 'mapcar
                        (lambda (cand)
                          (if (consp cand)
                              ;; override DISPLAY from candidate-transformer
                              (cons (funcall it (cdr cand)) (cdr cand))
                            (cons (funcall it cand) cand)))
                        candidates))
    candidates))

(defun helm-transform-candidates (candidates source &optional process-p)
  "Transform CANDIDATES from SOURCE according to candidate transformers.

When PROCESS-P is non-nil executes the
`filtered-candidate-transformer' functions, otherwise processes
`candidate-transformer' functions only,
`filtered-candidate-transformer' functions being processed later,
after the candidates have been narrowed by
`helm-candidate-number-limit', see `helm-compute-matches'.  When
`real-to-display' attribute is present, execute its functions on all
maybe filtered CANDIDATES."
  (helm-process-real-to-display
   (helm-process-filtered-candidate-transformer-maybe
    (helm-process-candidate-transformer
     candidates source)
    source process-p)
   source))


;; Narrowing candidates
(defun helm-candidate-number-limit (source)
  "Apply candidate-number-limit attribute value.
This overrides `helm-candidate-number-limit' variable.

E.g.:
If (candidate-number-limit) is in SOURCE, show all candidates in SOURCE.
If (candidate-number-limit . 123) is in SOURCE limit candidate to 123."
  (helm-aif (assq 'candidate-number-limit source)
      ;; When assoc value is nil use by default 99999999 otherwise use
      ;; the assoc value, when it is a symbol interpret its value (bug#1831).
      (or (helm-aand (cdr it) (helm-interpret-value it)) 99999999)
    (or helm-candidate-number-limit 99999999)))

(defun helm-candidate-get-display (candidate)
  "Get searched display part from CANDIDATE.
CANDIDATE is either a string, a symbol, or a (DISPLAY . REAL)
cons cell."
  (cond ((car-safe candidate))
        ((symbolp candidate)
         (symbol-name candidate))
        ((numberp candidate)
         (number-to-string candidate))
        (t candidate)))

(defun helm-process-pattern-transformer (pattern source)
  "Execute pattern-transformer attribute function(s) on PATTERN in SOURCE."
  (helm-aif (assoc-default 'pattern-transformer source)
      (helm-apply-functions-from-source source it pattern)
    pattern))

(defun helm-default-match-function (candidate)
  "Check if `helm-pattern' match CANDIDATE.
Default function to match candidates according to `helm-pattern'."
  (string-match helm-pattern candidate))


;;; Fuzzy matching
;;
;;
(defvar helm--fuzzy-regexp-cache (make-hash-table :test 'eq))
(defun helm--fuzzy-match-maybe-set-pattern ()
  ;; Computing helm-pattern with helm--mapconcat-pattern
  ;; is costly, so cache it once time for all and reuse it
  ;; until pattern change.
  (when helm--in-fuzzy
    (let ((fun (if (string-match "\\`\\^" helm-pattern)
                   #'identity
                 #'helm--mapconcat-pattern)))
      (clrhash helm--fuzzy-regexp-cache)
      ;; FIXME: Splitted part are not handled here,
      ;; I must compute them in `helm-search-match-part'
      ;; when negation and in-buffer are used.
      (if (string-match "\\`!" helm-pattern)
          (puthash 'helm-pattern
                   (if (> (length helm-pattern) 1)
                       (list (regexp-quote (substring helm-pattern 1 2))
                             (funcall fun (substring helm-pattern 1)))
                     '("" ""))
                   helm--fuzzy-regexp-cache)
        (puthash 'helm-pattern
                 (if (> (length helm-pattern) 0)
                     (list (regexp-quote (substring helm-pattern 0 1))
                           (funcall fun helm-pattern))
                   '("" ""))
                 helm--fuzzy-regexp-cache)))))

(defun helm-fuzzy-match (candidate)
  "Check if `helm-pattern' fuzzy matches CANDIDATE.
This function is used with sources built with `helm-source-sync'."
  (unless (string-match " " helm-pattern)
    ;; When pattern have one or more spaces, let
    ;; multi-match doing the job with no fuzzy matching.[1]
    (let ((regexp (cadr (gethash 'helm-pattern helm--fuzzy-regexp-cache))))
      (if (string-match "\\`!" helm-pattern)
          (not (string-match regexp candidate))
        (string-match regexp candidate)))))

(defun helm-fuzzy-search (pattern)
  "Same as `helm-fuzzy-match' but for sources built with
`helm-source-in-buffer'."
  (unless (string-match " " helm-pattern)
    ;; Same as in `helm-fuzzy-match' ref[1].
    (let* ((regexps (gethash 'helm-pattern helm--fuzzy-regexp-cache))
           (partial-regexp (car regexps))
           (regexp (cadr regexps)))
      (if (string-match "\\`!" pattern)
          ;; Don't try to search here, just return
          ;; the position of line and go ahead,
          ;; letting `helm-search-match-part' checking if
          ;; pattern match against this line.
          (prog1 (list (pos-bol) (pos-eol))
            (forward-line 1))
        ;; We could use here directly `re-search-forward'
        ;; on the regexp produced by `helm--mapconcat-pattern',
        ;; but it is very slow because emacs have to do an incredible
        ;; amount of loops to match e.g "[^f]*f[^o]*o..." in the whole buffer,
        ;; more the regexp is long more the amount of loops grow.
        ;; (Probably leading to a max-lisp-eval-depth error if both
        ;; regexp and buffer are too big)
        ;; So just search the first bit of pattern e.g "[^f]*f", and
        ;; then search the corresponding line with the whole regexp,
        ;; which increase dramatically the speed of the search.
        (cl-loop while (re-search-forward partial-regexp nil t)
                 for bol = (pos-bol)
                 for eol = (pos-eol)
                 if (progn (goto-char bol)
                           (re-search-forward regexp eol t))
                 do (goto-char eol) and return t
                 else do (goto-char eol)
                 finally return nil)))))

(defvar helm-fuzzy-default-score-fn #'helm-fuzzy-flex-style-score)
(defun helm-score-candidate-for-pattern (candidate pattern)
  "Assign score to CANDIDATE according to PATTERN."
  ;; Unknown candidates always go on top.
  (if (helm-candidate-prefixed-p candidate)
      200.00
    (funcall helm-fuzzy-default-score-fn candidate pattern)))

;; The flex scoring needs a regexp whereas the fuzzy scoring works
;; directly with helm-pattern, so cache the needed regexp for flex
;; scoring to not (re)compute it at each candidate.  We could reuse
;; the regexp cached in `helm--fuzzy-regexp-cache' but it is not
;; exactly the same as the one needed for flex and also it is always
;; computed against the whole helm-pattern which is not usable for
;; e.g. file completion.
(defvar helm--fuzzy-flex-regexp-cache (make-hash-table :test 'equal))
(defun helm-fuzzy-flex-style-score (candidate pattern)
  "Give a score to CANDIDATE according to PATTERN.
A regexp is generated from PATTERN to calculate score.
Score is calculated with the emacs-27 flex algorithm using
`helm-flex--style-score'."
  (let ((regexp (helm-aif (gethash pattern helm--fuzzy-flex-regexp-cache)
                    it
                  (clrhash helm--fuzzy-flex-regexp-cache)
                  (puthash pattern (helm--fuzzy-flex-pattern-to-regexp pattern)
                           helm--fuzzy-flex-regexp-cache))))
    (helm-flex--style-score candidate regexp t)))

(defun helm--fuzzy-flex-pattern-to-regexp (pattern)
  "Return a regexp from PATTERN compatible with emacs-27 flex algorithm."
  (completion-pcm--pattern->regex
   (helm-completion--flex-transform-pattern (list pattern)) 'group))

(defun helm-flex-add-score-as-prop (candidates regexp)
  (cl-loop with case-fold-search = (helm-set-case-fold-search)
           for cand in candidates
           collect (helm-flex--style-score cand regexp)))

(defun helm-completion--flex-transform-pattern (pattern)
  ;; "fob" => '(prefix "f" any "o" any "b" any point)
  (cl-loop for p in pattern
           if (stringp p) nconc
           (cl-loop for str across p
                    nconc (list (string str) 'any))
           else nconc (list p)))

(defun helm-fuzzy-helm-style-score (candidate pattern)
  "Give a score to CANDIDATE according to PATTERN.
Score is calculated for contiguous matches found with PATTERN.
Score is 100 (maximum) if PATTERN is fully matched in CANDIDATE.
One point bonus is added to score when PATTERN prefix matches
CANDIDATE.  Contiguous matches get a coefficient of 2."
  (let* ((cand (if (stringp candidate)
                   candidate (helm-stringify candidate)))
         (pat-lookup (helm--collect-pairs-in-string pattern))
         (str-lookup (helm--collect-pairs-in-string cand))
         (inter (cl-nintersection pat-lookup str-lookup :test 'equal))
         ;; Prefix
         (bonus (cond ((or (equal (car pat-lookup) (car str-lookup))
                           (equal (caar pat-lookup) (caar str-lookup)))
                       2)
                      ((and (null pat-lookup) ; length = 1
                            (string= pattern (substring cand 0 1)))
                       150)
                      (t 0)))
         ;; Exact match e.g. foo -> foo == 200
         (bonus1 (and (string= cand pattern) 200))
         ;; Partial match e.g. foo -> aafooaa == 100
         ;;               or   foo -> fooaaa
         (bonus2 (and (or (string-match
                           (concat "\\`" (regexp-quote pattern))
                           cand)
                          (string-match
                           (concat "\\<" (regexp-quote pattern) "\\>")
                           cand))
                      100)))
    (+ bonus
       (or bonus1 bonus2
           ;; Give a coefficient of 2 for contiguous matches.
           ;; That's mean that "wiaaaki" will not take precedence
           ;; on "aaawiki" when matching on "wiki" even if "wiaaaki"
           ;; starts by "wi".
           (* (length inter) 2)))))

(defun helm-fuzzy-matching-default-sort-fn-1 (candidates &optional use-real basename preserve-tie-order)
  "The transformer for sorting candidates in fuzzy matching.
It sorts on the display part by default.

It sorts CANDIDATES by their scores as calculated by
`helm-score-candidate-for-pattern'.  Set USE-REAL to non-nil to
sort on the real part.  If BASENAME is non-nil assume we are
completing filenames and sort on basename of candidates.  If
PRESERVE-TIE-ORDER is nil, ties in scores are sorted by length of
the candidates."
  (if (string= helm-pattern "")
      candidates
    (let ((table-scr (make-hash-table :test 'equal)))
      (sort candidates
            (lambda (s1 s2)
              ;; Score and measure the length on real or display part of candidate
              ;; according to `use-real'.
              (let* ((real-or-disp-fn (if use-real #'cdr #'car))
                     (cand1 (cond ((and basename (consp s1))
                                   (helm-basename (funcall real-or-disp-fn s1)))
                                  ((consp s1) (funcall real-or-disp-fn s1))
                                  (basename (helm-basename s1))
                                  (t s1)))
                     (cand2 (cond ((and basename (consp s2))
                                   (helm-basename (funcall real-or-disp-fn s2)))
                                  ((consp s2) (funcall real-or-disp-fn s2))
                                  (basename (helm-basename s2))
                                  (t s2)))
                     (data1 (or (gethash cand1 table-scr)
                                (puthash cand1
                                         (list (helm-score-candidate-for-pattern
                                                cand1 helm-pattern)
                                               (length (helm-stringify cand1)))
                                         table-scr)))
                     (data2 (or (gethash cand2 table-scr)
                                (puthash cand2
                                         (list (helm-score-candidate-for-pattern
                                                cand2 helm-pattern)
                                               (length (helm-stringify cand2)))
                                         table-scr)))
                     (len1 (cadr data1))
                     (len2 (cadr data2))
                     (scr1 (car data1))
                     (scr2 (car data2)))
                (cond ((= scr1 scr2)
                       ;; Comparison by length should not be called here most of
                       ;; the time because we use now flex scoring which does
                       ;; such test, however we still use helm fuzzy scoring
                       ;; with preserve-tie-order, so keep testing length here
                       ;; for it.
                       (unless preserve-tie-order
                         (< len1 len2)))
                      ((> scr1 scr2)))))))))

(defun helm-fuzzy-matching-default-sort-fn (candidates _source)
  "Default `filtered-candidate-transformer' to sort in fuzzy matching."
  (helm-fuzzy-matching-default-sort-fn-1 candidates))

(defun helm-fuzzy-matching-sort-fn-preserve-ties-order (candidates _source)
  "Same as `helm-fuzzy-matching-default-sort-fn' but preserving order of ties.
The default function, `helm-fuzzy-matching-default-sort-fn',
sorts ties by length, shortest first.  This function may be more
useful when the order of the candidates is meaningful, e.g. with
`recentf-list'."
  ;; Flex scoring is already taking in account length of strings so this
  ;; function have no effect when flex scoring is in use, force the usage of
  ;; helm fuzzy scoring to ensure no testing against length is done.
  (let ((helm-fuzzy-default-score-fn #'helm-fuzzy-helm-style-score))
    (helm-fuzzy-matching-default-sort-fn-1 candidates nil nil t)))

(defun helm--maybe-get-migemo-pattern (pattern &optional diacritics)
  (or (and helm-migemo-mode
           (assoc-default pattern helm-mm--previous-migemo-info))
      (if diacritics
          (char-fold-to-regexp pattern)
        pattern)))

(defun helm-fuzzy-default-highlight-match-1 (candidate &optional pattern diacritics file-comp)
  (let* ((pair    (and (consp candidate) candidate))
         (display (helm-stringify (if pair (car pair) candidate)))
         (real    (cdr pair))
         (host    (and file-comp (get-text-property
                                  (max 0 (1- (length display))) 'host display)))
         (regex   (helm--maybe-get-migemo-pattern pattern diacritics))
         (mpart   (get-text-property 0 'match-part display))
         (mp      (cond ((and mpart (string= display mpart)) nil)
                        (mpart)
                        ;; FIXME: This may be wrong when match-on-real
                        ;; is nil, so we should flag match-on-real on
                        ;; top and use it.
                        (file-comp (file-name-nondirectory
                                    (or host (and (stringp real) real) display)))))
         (count   0)
         beg-str end-str)
    ;; Happens when matching empty lines (^$), in this case there is nothing to
    ;; highlight. 
    (if (string= mpart "")
        candidate
      (when host (setq pattern (cadr (split-string pattern ":"))))
      ;; Extract all parts of display keeping original properties.
      (when (and mp (ignore-errors
                      ;; Avoid error when candidate is a huge line.
                      (string-match (regexp-quote mp) display)))
        (setq beg-str (substring display 0 (match-beginning 0))
              end-str (substring display (match-end 0) (length display))
              mp (substring display (match-beginning 0) (match-end 0))))
      (with-temp-buffer
        ;; Insert the whole display part and remove non--match-part
        ;; to keep their original face properties.
        (insert (propertize (or mp display) 'read-only nil)) ; Fix (bug#1176)
        (goto-char (point-min))
        (condition-case nil
            (progn
              ;; Try first matching against whole pattern.
              (unless (string= pattern "")
                (while (re-search-forward regex nil t)
                  (cl-incf count)
                  (helm-add-face-text-properties
                   (match-beginning 0) (match-end 0) 'helm-match)))
              ;; If no matches start matching against multiples or fuzzy matches.
              (when (zerop count)
                (cl-loop with multi-match = (string-match-p " " pattern)
                         with patterns = (if multi-match
                                             (cl-loop for pat in (helm-mm-split-pattern
                                                                  pattern)
                                                      collect
                                                      (helm--maybe-get-migemo-pattern
                                                       pat diacritics))
                                           (split-string pattern "" t))
                         for p in patterns
                         ;; Multi matches (regexps patterns).
                         if multi-match do
                         (progn
                           (while (re-search-forward p nil t)
                             (helm-add-face-text-properties
                              (match-beginning 0) (match-end 0)
                              'helm-match))
                           (goto-char (point-min)))
                         ;; Fuzzy matches (literal patterns).
                         else do
                         (when (search-forward p nil t)
                           (helm-add-face-text-properties
                            (match-beginning 0) (match-end 0)
                            'helm-match)))))
          (invalid-regexp nil))
        ;; Now replace the original match-part with the part
        ;; with face properties added.
        (setq display (if mp (concat beg-str (buffer-string) end-str) (buffer-string))))
      (if real (cons display real) display))))

(cl-defun helm-fuzzy-default-highlight-match (candidate
                                              &optional (pattern helm-pattern) diacritics file-comp)
  "The default function to highlight matches in fuzzy matching.
Highlight elements in CANDIDATE matching PATTERN according
to the matching method in use.  When DIACRITICS is specified, ignore
diacritics, see `char-fold-to-regexp' for more infos."
  (if (string= pattern "")
      ;; Empty pattern, do nothing.  This is needed when this function
      ;; is used outside of helm-fuzzy-highlight-matches like in *buffers-list. 
      candidate
    ;; Else start highlighting.
    (helm-fuzzy-default-highlight-match-1 candidate pattern diacritics file-comp)))

(defun helm-fuzzy-highlight-matches (candidates source)
  "Highlight matches in CANDIDATES for SOURCE.
The filtered-candidate-transformer function to highlight fuzzy matches.
See `helm-fuzzy-default-highlight-match'."
  (cl-assert helm-fuzzy-matching-highlight-fn nil "Wrong type argument functionp: nil")
  (cl-loop with diac = (helm-get-attr 'diacritics source)
           with file-comp-p = (and (not (helm-action-window))
                                   (or minibuffer-completing-file-name
                                       (helm-get-attr 'completing-file-name source)))
           ;; helm-pattern may have been modified (fuzzy) so ensure to
           ;; use helm-input which is the raw pattern.
           with pattern = (if file-comp-p
                              (file-name-nondirectory helm-input)
                            helm-pattern)
           when (string= pattern "") return candidates
           for c in candidates
           collect (funcall helm-fuzzy-matching-highlight-fn c pattern diac file-comp-p)))


;;; helm-flex style
;;
;; Provide the emacs-27 flex style for emacs<27.
;; Reuse the flex scoring algorithm of flex style in emacs-27.
(defun helm-flex--style-score (str regexp &optional score)
  "Score STR candidate according to REGEXP.

REGEXP should be generated from a pattern which is a list like
\\='(point \"f\" any \"o\" any \"b\" any) for \"fob\" as pattern.
Such pattern may be build with
`helm-completion--flex-transform-pattern' function, and the regexp
with `completion-pcm--pattern->regex'.  For commodity,
`helm--fuzzy-flex-pattern-to-regexp' is used to build such regexp. 

Function extracted from `completion-pcm--hilit-commonality' in
emacs-27 to provide such scoring in emacs<27."
  ;; Don't modify the string itself.
  (setq str (copy-sequence str))
  (if (string-match regexp str)
      (let* ((md (match-data))
             (start (pop md))
             (len (length str))
             (score-numerator 0)
             (score-denominator 0)
             (last-b 0)
             (update-score
              (lambda (a b)
                "Update score variables given match range (A B)."
                (setq score-numerator (+ score-numerator (- b a)))
                (unless (or (= a last-b)
                            (zerop last-b)
                            (= a (length str)))
                  (setq score-denominator (+ score-denominator
                                             1
                                             (expt (- a last-b 1)
                                                   (/ 1.0 3)))))
                (setq last-b b)))
             result)
        (funcall update-score start start)
        (setq md (cdr md))
        (while md
          (funcall update-score start (pop md))
          (setq start (pop md)))
        (funcall update-score len len)
        (unless (zerop (length str))
          (setq result (/ score-numerator (* len (1+ score-denominator)) 1.0))
          (put-text-property 0 1 'completion-score result str))
        (if (and score result) result str))
    (put-text-property 0 1 'completion-score 0.0 str)
    (if score 0.0 str)))


;;; Matching candidates
;;
;;
(defun helm-match-functions (source)
  (let ((matchfns (or (assoc-default 'match source)
                      (assoc-default 'match-strict source)
                      #'helm-default-match-function)))
    (if (and (listp matchfns) (not (functionp matchfns)))
        matchfns (list matchfns))))

(defun helm-search-functions (source)
  (let ((searchfns (assoc-default 'search source)))
    (if (and (listp searchfns) (not (functionp searchfns)))
        searchfns (list searchfns))))

(defun helm-match-from-candidates (cands matchfns match-part-fn limit source)
  (when cands ; nil in async sources.
    (condition-case-unless-debug err
        (cl-loop with hash = (make-hash-table :test 'equal)
                 with allow-dups = (assq 'allow-dups source)
                 with case-fold-search = (helm-set-case-fold-search)
                 with count = 0
                 for iter from 1
                 for fn in matchfns
                 when (< count limit) nconc
                 (cl-loop for c in cands
                          for dup = (gethash c hash)
                          for disp = (helm-candidate-get-display c)
                          while (< count limit)
                          for target = (if (helm-get-attr 'match-on-real source)
                                           ;; Let's fails on error in
                                           ;; case next block returns nil.
                                           (or (cdr-safe c)
                                               (get-text-property 0 'helm-realvalue disp))
                                         disp)
                          for prop-part = (get-text-property 0 'match-part target)
                          for part = (and match-part-fn
                                          (or prop-part
                                              (funcall match-part-fn target)))
                          ;; When allowing dups check if DUP
                          ;; have been already found in previous loop
                          ;; by comparing its value with ITER.
                          when (and (or (and allow-dups dup (= dup iter))
                                        (null dup))
                                    (condition-case nil
                                        (funcall fn (or part target))
                                      (invalid-regexp nil)))
                          do
                          (progn
                            ;; Give as value the iteration number of
                            ;; inner loop to be able to check if
                            ;; the duplicate have not been found in previous loop.
                            (puthash c iter hash)
                            (helm--maybe-process-filter-one-by-one-candidate c source)
                            (cl-incf count))
                          ;; Filter out nil candidates maybe returned by
                          ;; `helm--maybe-process-filter-one-by-one-candidate'.
                          and when c collect
                          (if (and part (not prop-part))
                              (if (consp c)
                                  (cons (propertize target 'match-part part) (cdr c))
                                (propertize c 'match-part part))
                            c)))
      (error (unless (eq (car err) 'invalid-regexp) ; Always ignore regexps errors.
               (helm-log-error "helm-match-from-candidates"
                               "helm-match-from-candidates in source `%s': %s %s"
                               (assoc-default 'name source) (car err) (cdr err)))
             nil))))

(defun helm-compute-matches (source)
  "Start computing candidates in SOURCE."
  (save-current-buffer
    (let ((matchfns (helm-match-functions source))
          (matchpartfn (assoc-default 'match-part source))
          (helm--source-name (assoc-default 'name source))
          (helm-current-source source)
          (limit (helm-candidate-number-limit source))
          (helm-pattern (helm-process-pattern-transformer
                         helm-pattern source)))
      (helm--fuzzy-match-maybe-set-pattern)
      ;; If source have a `filtered-candidate-transformer' attr
      ;; Filter candidates with this func, otherwise just compute
      ;; candidates.
      ;; NOTE that this next block of code is returning nil on async sources,
      ;; the candidates being processed directly in `helm-output-filter'
      ;; process-filter.
      (helm-process-filtered-candidate-transformer
       ;; When using in-buffer method or helm-pattern is empty or
       ;; using dynamic completion always compute all candidates.
       (if (or (equal helm-pattern "")
               (assq 'match-dynamic source)
               (helm--candidates-in-buffer-p source))
           ;; Compute all candidates up to LIMIT.
           ;; one-by-one are computed here only for sources that
           ;; display a list of  candidates even with an empty
           ;; pattern.
           (helm--initialize-one-by-one-candidates
            (helm-take (helm-get-cached-candidates source) limit)
            source)
         ;; Compute candidates according to pattern with their match
         ;; fns.
         ;; one-by-one filtered candidates are computed during the
         ;; execution of next loop in `helm-match-from-candidates'.
         (helm-match-from-candidates
          (helm-get-cached-candidates source) matchfns matchpartfn limit source))
       source))))

(defun helm--candidates-in-buffer-p (source)
  (assq 'search source))

(defun helm-render-source (source matches)
  "Display MATCHES from SOURCE according to its settings."
  (helm-log "helm-render-source" "Source = %S" (remove (assq 'keymap source) source))
  (when matches
    (helm-insert-header-from-source source)
    (cl-loop with separate = nil
             with start = (point)
             with singleline = (null (assq 'multiline source))
             for m in matches
             for count from 1
             if singleline
             do (helm-insert-match m 'insert count source)
             else
             do (progn
                  (if separate
                      (helm-insert-candidate-separator)
                    (setq separate t))
                  (helm-insert-match m 'insert count source))
             finally (and (null singleline)
                          (put-text-property start (point)
                                             'helm-multiline t)))))

(defmacro helm-while-no-input (&rest body)
  "Same as `while-no-input' but returns either BODY or nil.
Unlike `while-no-input' this macro ensure to not returns `t'."
  (declare (debug t) (indent 0))
  (let ((catch-sym (make-symbol "input")))
    `(with-local-quit
       (catch ',catch-sym
         (let ((throw-on-input ',catch-sym)
               val)
           (setq val (progn ,@body))
           ;; See comments in `while-no-input' about resetting
           ;; quit-flag.
           (cond ((eq quit-flag throw-on-input)
                  (setq quit-flag nil))
                 (quit-flag nil)
                 (t val)))))))

(defmacro helm--maybe-use-while-no-input (&rest body)
  "Wrap BODY in `helm-while-no-input' unless initializing a remote connection."
  `(progn
     (if (and (file-remote-p helm-pattern)
              (not (file-remote-p helm-pattern nil t)))
         ;; Tramp will ask for passwd, don't use `helm-while-no-input'.
         ,@body
       (helm-log "helm--maybe-use-while-no-input"
                 "Using here `helm-while-no-input'")
       ;; Emacs bug <https://debbugs.gnu.org/47205>, unexpected
       ;; dbus-event is triggered on dbus init.
       ;; Ignoring the dbus-event work on emacs28+; for emacs27 or older
       ;; version, require tramp-archive can workaround the issue.
       (let ((while-no-input-ignore-events
              (and (boundp 'while-no-input-ignore-events)
                   (cons 'dbus-event while-no-input-ignore-events))))
         (helm-while-no-input ,@body)))))

(defun helm--collect-matches (src-list)
  "Return a list of matches for each source in SRC-LIST.

The resulting value is a list of lists, e.g. ((a b c) (c d) (e
f)) or (nil nil nil) for three sources when no matches found,
however this function can be interrupted by new input and in this
case returns a plain nil i.e. not (nil), in this case
`helm-update' is not rendering the source, keeping previous
candidates in display."
  (let ((matches (helm--maybe-use-while-no-input
                  (cl-loop for src in src-list
                           collect (helm-compute-matches src)))))
    (unless (eq matches t) matches)))


;;; Case fold search
;;
;;
(cl-defun helm-set-case-fold-search (&optional (pattern helm-pattern))
  "Used to set the value of `case-fold-search' in Helm.
Return t or nil depending on the value of `helm-case-fold-search'
and `helm-pattern'."
  (if helm-alive-p
      (let ((helm-case-fold-search
             (helm-aif (assq 'case-fold-search (helm-get-current-source))
                 (cdr it)
               helm-case-fold-search))
            ;; Only parse basename for filenames
            ;; to avoid setting case sensitivity
            ;; when expanded directories contains upcase
            ;; characters.
            (bn-or-pattern (if (string-match "[~/]*" pattern)
                               (helm-basename pattern)
                             pattern)))
        (helm-set-case-fold-search-1 bn-or-pattern))
    case-fold-search))

(defun helm-set-case-fold-search-1 (pattern)
  (cl-case helm-case-fold-search
    (smart (let ((case-fold-search nil))
             (if (string-match "[[:upper:]]" pattern) nil t)))
    (t helm-case-fold-search)))


;;; Helm update
;;
(defun helm-update (&optional preselect source candidates)
  "Update candidates list in `helm-buffer' based on `helm-pattern'.
Argument PRESELECT is a string or regexp used to move selection
to a particular place after finishing update.
When SOURCE is provided update mode-line for this source, otherwise
the current source will be used.
Argument CANDIDATES when provided is used to redisplay these candidates
without recomputing them, it should be a list of lists."
  (helm-log "helm-update" "Start updating")
  (helm-kill-async-processes)
  ;; When persistent action have been called
  ;; we have two windows even with `helm-full-frame'.
  ;; So go back to one window when updating if `helm-full-frame'
  ;; is non-`nil'.
  (when (with-helm-buffer
          (and helm-onewindow-p
               ;; We are not displaying helm-buffer in a frame and
               ;; helm-window is already displayed.
               (not helm--buffer-in-new-frame-p)
               (helm-window)
               (not (helm-action-window))))
    (with-helm-window (delete-other-windows)))
  (with-current-buffer (helm-buffer-get)
    (set (make-local-variable 'helm-input-local) helm-pattern)
    (unwind-protect
        (let (sources matches)
          ;; Collect sources ready to be updated.
          (setq sources
                (cl-loop for src in helm-sources
                         when (helm-update-source-p src)
                         collect src))
          ;; When no sources to update erase buffer
          ;; to avoid duplication of header and candidates
          ;; when next chunk of update will arrive,
          ;; otherwise the buffer is erased AFTER [1] the results
          ;; are computed.
          (unless sources (erase-buffer))
          ;; Compute matches without rendering the sources.
          ;; This prevent the helm-buffer flickering when constantly
          ;; updating.
          (helm-log "helm-update" "Matches: %S"
                    (setq matches (or candidates (helm--collect-matches sources))))
          ;; If computing matches finished and is not interrupted
          ;; erase the helm-buffer and render results (Fix #1157).
          (when matches ;; nil only when interrupted by while-no-input.
            (erase-buffer)             ; [1]
            (cl-loop for src in sources
                     for mtc in matches
                     do (helm-render-source src mtc))
            ;; Move to first line only when there is matches
            ;; to avoid cursor moving upside down (Bug#1703).
            (helm--update-move-first-line)))
      ;; When there is only one async source, update mode-line and run
      ;; `helm-after-update-hook' in `helm-output-filter--post-process',
      ;; when there is more than one source, update mode-line and run
      ;; `helm-after-update-hook' now even if an async source is
      ;; present and running in BG.
      (let ((src (or source (helm-get-current-source))))
        (unless (assq 'candidates-process src)
          (helm-display-mode-line src 'force)
          (helm-log-run-hook "helm-update" 'helm-after-update-hook)))
      (when preselect
        (helm-log "helm-update" "Update preselect candidate %s" preselect)
        (if (helm-window)
            (with-helm-window (helm-preselect preselect source))
          (helm-preselect preselect source)))
      (setq helm--force-updating-p nil)
      (helm--reset-update-flag))
    (helm-log "helm-update" "end update")))

(defun helm-update-source-p (source)
  "Whether SOURCE needs updating or not."
  (let ((len (string-width
              (if (assq 'multimatch source)
                  ;; Don't count spaces entered when using
                  ;; multi-match.
                  (replace-regexp-in-string " " "" helm-pattern)
                helm-pattern))))
    (and (or (not helm-source-filter)
             (member (assoc-default 'name source) helm-source-filter))
         (>= len
             (helm-aif (assq 'requires-pattern source) (or (cdr it) 1) 0))
         ;; Entering repeatedly these strings (*, ?) takes 100% CPU
         ;; and hang emacs on MacOs preventing deleting backward those
         ;; characters (Bug#1802). Update: it seems it is no more true,
         ;; thus this affect bug#2423, so let's remove this for now.
         ;; (not (string-match-p "\\`[*]+\\'" helm-pattern))
         ;; These incomplete regexps hang helm forever
         ;; so defer update. Maybe replace spaces quoted when using
         ;; multi-match.
         (not (member (replace-regexp-in-string "\\s\\ " " " helm-pattern)
                      helm-update-blacklist-regexps)))))

(defun helm--update-move-first-line ()
  "Goto first line of `helm-buffer'."
  (goto-char (point-min))
  (if (helm-window)
      (helm-move-selection-common :where 'line
                                  :direction 'next
                                  :follow t)
    (forward-line 1)
    (helm-mark-current-line)
    (helm-follow-execute-persistent-action-maybe)))

(cl-defun helm-force-update (&optional preselect (recenter t))
  "Force recalculation and update of candidates.

Unlike `helm-update', this function re-evaluates `init' and
`update' attributes when present; also `helm-candidate-cache' is
not reinitialized, meaning candidates are not recomputed unless
pattern has changed.

Selection is preserved to current candidate if it still exists after
update or moved to PRESELECT, if specified.
The helm-window is re-centered at the end when RECENTER is t which
is the default.  RECENTER can be also a number in this case it is
passed as argument to `recenter'."
  (with-helm-buffer
    (let* ((source    (helm-get-current-source))
           (selection (helm-aif (helm-get-selection nil t source)
                          (regexp-quote it))))
      (setq helm--force-updating-p t)
      (mapc 'helm-force-update--reinit helm-sources)
      (helm-update (or preselect selection) source)
      (when (and (helm-window) recenter)
        (with-helm-window
          (recenter (and (numberp recenter) recenter)))))))

(defun helm-refresh ()
  "Force recalculation and update of candidates."
  (interactive)
  (with-helm-alive-p
    (helm-force-update)))
(put 'helm-refresh 'helm-only t)

(defun helm-force-update--reinit (source)
  "Reinit SOURCE by calling its update and init functions."
  ;; When using a specific buffer as cache, don't kill it.
  (helm-aif (and (null (bufferp (assoc-default
                                 (helm-get-attr 'name source)
                                 helm--candidate-buffer-alist)))
                 (helm-apply-functions-from-source
                  source 'helm-candidate-buffer))
      (kill-buffer it))
  (dolist (attr '(update init))
    (helm-aif (assoc-default attr source)
        (helm-apply-functions-from-source source it)))
  (helm-remove-candidate-cache source))

(defun helm-redisplay-buffer ()
  "Redisplay candidates in `helm-buffer'.

Candidates are not recomputed, only redisplayed after modifying
the whole list of candidates in each source with functions found
in `redisplay' attribute of current source.  Note that candidates
are redisplayed with their display part with all properties
included only.  This function is used in async sources to
transform the whole list of candidates from the sentinel
functions (i.e. when all candidates have been computed) because
other filters like `candidate-transformer' are modifying only
each chunk of candidates from `process-filter' as they come in
and not the whole list.  Use this for e.g. sorting the whole list
of async candidates once computed.

Note: To ensure redisplay is done in async sources after Helm
reached `candidate-number-limit' you will have also to redisplay
your candidates from `helm-async-outer-limit-hook'."
  (with-helm-buffer
    (let ((get-cands (lambda (source)
                       (let ((fns (assoc-default 'redisplay source))
                             candidates
                             helm-move-to-line-cycle-in-source
                             helm-allow-mouse)
                         (helm-goto-source source)
                         (helm-next-line)
                         (helm-awhile (condition-case-unless-debug nil
                                          (and (not (helm-pos-header-line-p))
                                               (helm-get-selection
                                                nil 'withprop source))
                                        (error nil))
                           (push it candidates)
                           (when (save-excursion
                                   (forward-line 1) (helm-end-of-source-p t))
                             (cl-return nil))
                           (helm-next-line))
                         (helm-apply-functions-from-source
                          source fns (nreverse candidates)))))
          (get-sources (lambda ()
                         (let (sources helm-move-to-line-cycle-in-source)
                           (helm-awhile (helm-get-current-source)
                             (push it sources)
                             (when (save-excursion
                                     (helm-move--end-of-source)
                                     (forward-line 1) (eobp))
                               (cl-return nil))
                             (helm-next-source))
                           (nreverse sources)))))
      (goto-char (point-min))
      (helm-update nil (helm-get-current-source)
                   (cl-loop with sources = (funcall get-sources)
                            for s in helm-sources
                            for name =  (assoc-default 'name s) collect
                            (when (cl-loop for src in sources thereis
                                           (string= name
                                                    (assoc-default 'name src)))
                              (funcall get-cands s)))))))

(defun helm-remove-candidate-cache (source)
  "Remove SOURCE from `helm-candidate-cache'."
  (remhash (assoc-default 'name source) helm-candidate-cache))

(defvar helm-drag-mouse-1-fn 'ignore)
(defun helm-insert-match (match insert-function &optional num source)
  "Insert MATCH into `helm-buffer' with INSERT-FUNCTION.
If MATCH is a cons cell then insert the car as display with the
cdr stored as real value in a `helm-realvalue' text property.
Args NUM and SOURCE are also stored as text property when
specified as respectively `helm-cand-num' and `helm-cur-source'."
  (let ((start     (pos-bol (point)))
        (dispvalue (helm-candidate-get-display match))
        (realvalue (cdr-safe match))
        (map       (when helm-allow-mouse (make-sparse-keymap)))
        (inhibit-read-only t)
        end)
    (when (and (stringp dispvalue)
               (not (zerop (length dispvalue))))
      (funcall insert-function dispvalue)
      (setq end (pos-eol))
      ;; Some strings may handle another keymap prop.
      (remove-text-properties start end '(keymap nil))
      (put-text-property start end 'read-only nil)
      ;; Some sources with candidates-in-buffer have already added
      ;; 'helm-realvalue property when creating candidate buffer.
      (unless (get-text-property start 'helm-realvalue)
        (and realvalue
             (put-text-property start end
                                'helm-realvalue realvalue)))
      (when map
        (define-key map [drag-mouse-1] 'ignore)
        (define-key map [mouse-1] 'helm-mouse-select-candidate)
        (define-key map [mouse-2] 'ignore)
        (define-key map [mouse-3] 'helm-menu-select-action)
        (add-text-properties
         start end
         `(mouse-face highlight
                      keymap ,map
                      help-echo ,(pcase (get-text-property start 'help-echo)
                                   ((and it (pred stringp))
                                    (concat it "\nmouse-1: select candidate\nmouse-3: menu actions"))
                                   (_ "mouse-1: select candidate\nmouse-3: menu actions")))))
      (when num
        (put-text-property start end 'helm-cand-num num))
      (when source
        (put-text-property start end 'helm-cur-source source))
      (funcall insert-function "\n"))))

(defun helm--mouse-reset-selection-help-echo ()
  (let* ((inhibit-read-only t)
         (start (overlay-start helm-selection-overlay))
         (end   (overlay-end helm-selection-overlay))
         (help-echo (get-text-property start 'help-echo)))
    (when (and (stringp help-echo)
               (string-match "mouse-2: execute action" help-echo))
      (put-text-property
       start end
       'help-echo (replace-match "mouse-1: select candidate"
                                 t t help-echo)))))

(defun helm--bind-mouse-for-selection (pos)
  (let ((inhibit-read-only t)
        (map (get-text-property pos 'keymap)))
    (when map
      (define-key map [drag-mouse-1] helm-drag-mouse-1-fn)
      (define-key map [mouse-2] 'helm-maybe-exit-minibuffer)
      (put-text-property
       helm-selection-point
       (overlay-end helm-selection-overlay)
       'help-echo (helm-aif (get-text-property pos 'help-echo)
                      (if (and (stringp it)
                               (string-match "mouse-1: select candidate" it))
                          (replace-match "mouse-2: execute action" t t it)
                        "mouse-2: execute action\nmouse-3: menu actions")
                    "mouse-2: execute action\nmouse-3: menu actions")))))

(defun helm-mouse-select-candidate (event)
  (interactive "e")
  (let* ((window (posn-window (event-end event)))
         (pos    (posn-point (event-end event))))
    (unwind-protect
        (with-current-buffer (window-buffer window)
          (if (and (helm-action-window)
                   (eql window (get-buffer-window helm-buffer)))
              (user-error "selection in helm-window not available while selecting action")
            (helm--mouse-reset-selection-help-echo)
            (goto-char pos)
            (when (helm-pos-multiline-p)
              (goto-char (or (helm-get-previous-candidate-separator-pos)
                             (helm-get-previous-header-pos)))
              (forward-line 1)))
          (helm-mark-current-line)
          (helm-follow-execute-persistent-action-maybe))
      (select-window (minibuffer-window))
      (set-buffer (window-buffer window)))))
(put 'helm-mouse-select-candidate 'helm-only t)

(defun helm-insert-header-from-source (source)
  "Insert SOURCE name in `helm-buffer' header.
Maybe insert, by overlay, additional info after the source name
if SOURCE has header-name attribute."
  (let ((name (assoc-default 'name source)))
    (helm-insert-header
     name
     (helm-aif (assoc-default 'header-name source)
         (helm-apply-functions-from-source source it name)))))

(defun helm-insert-header (name &optional display-string)
  "Insert header of source NAME into the helm buffer.
If DISPLAY-STRING is non-nil and a string value then display this
additional info after the source name by overlay."
  (unless (bobp)
    (let ((start (point)))
      (insert (propertize "\n" 'face 'helm-eob-line))
      (put-text-property start (point) 'helm-header-separator t)))
  (let ((start (point)))
    (insert name)
    (put-text-property (pos-bol)
                       (pos-eol) 'helm-header t)
    (when display-string
      (overlay-put (make-overlay (pos-bol) (pos-eol))
                   'display display-string))
    (insert "\n")
    (add-text-properties start (point) '(face helm-source-header
                                              ;; Disable line numbers on
                                              ;; source headers.
                                              display-line-numbers-disable t))))

(defun helm-insert-candidate-separator ()
  "Insert separator of candidates into the Helm buffer."
  (insert (propertize helm-candidate-separator 'face 'helm-separator))
  (put-text-property (pos-bol)
                     (pos-eol) 'helm-candidate-separator t)
  (insert "\n"))

(defun helm-init-relative-display-line-numbers ()
  "Enable `display-line-numbers' for Helm buffers.
This is intended to be added to `helm-after-initialize-hook'.
This will work only in Emacs-26+, i.e. Emacs versions that have
`display-line-numbers-mode'."
  (when (boundp 'display-line-numbers)
    (with-helm-buffer
      (setq display-line-numbers 'relative))))

(define-minor-mode helm-display-line-numbers-mode
  "Toggle display of line numbers in current Helm buffer."
  :group 'helm
  (with-helm-alive-p
    (cl-assert (boundp 'display-line-numbers) nil
               "`display-line-numbers' not available")
    (if helm-display-line-numbers-mode
        (with-helm-buffer
          (setq display-line-numbers 'relative))
      (with-helm-buffer
        (setq display-line-numbers nil)))))
(put 'helm-display-line-numbers-mode 'helm-only t)



;;; Async process
;;
(defun helm-output-filter (process output-string)
  "The `process-filter' function for Helm async sources."
  (with-local-quit
    (helm-output-filter-1 (assoc process helm-async-processes) output-string)))

(defun helm-output-filter-1 (process-assoc output-string)
  (helm-log "helm-output-filter-1" "output-string = %S" output-string)
  (with-current-buffer helm-buffer
    (let ((source (cdr process-assoc)))
      (save-excursion
        (helm-aif (assoc-default 'insertion-marker source)
            (goto-char it)
          (goto-char (point-max))
          (helm-insert-header-from-source source)
          (setcdr process-assoc
                  (append source `((insertion-marker . ,(point-marker))))))
        (helm-output-filter--process-source
         (car process-assoc) output-string source
         (helm-candidate-number-limit source))))
    (helm-output-filter--post-process)))

(defun helm-output-filter--process-source (process output-string source limit)
  (cl-dolist (candidate (helm-transform-candidates
                         (helm-output-filter--collect-candidates
                          (split-string output-string
                                        helm-process-output-split-string-separator)
                          (assq 'incomplete-line source))
                         source t))
    (setq candidate
          (helm--maybe-process-filter-one-by-one-candidate candidate source))
    (if (assq 'multiline source)
        (let ((start (point)))
          (helm-insert-candidate-separator)
          (helm-insert-match candidate 'insert-before-markers
                             (1+ (cdr (assq 'item-count source)))
                             source)
          (put-text-property start (point) 'helm-multiline t))
      (helm-insert-match candidate 'insert-before-markers
                         (1+ (cdr (assq 'item-count source)))
                         source))
    (cl-incf (cdr (assq 'item-count source)))
    (when (>= (assoc-default 'item-count source) limit)
      (helm-kill-async-process process)
      (helm-log-run-hook "helm-output-filter--process-source"
                         'helm-async-outer-limit-hook)
      (cl-return))))

(defun helm-output-filter--collect-candidates (lines incomplete-line-info)
  "Collect LINES maybe completing the truncated first and last lines."
  ;; The output of process may come in chunks of any size, so the last
  ;; line of LINES could be truncated, this truncated line is stored
  ;; in INCOMPLETE-LINE-INFO to be concatenated with the first
  ;; incomplete line of the next arriving chunk. INCOMPLETE-LINE-INFO
  ;; is an attribute of source; it is created with an empty string
  ;; when the source is computed => (incomplete-line . "")
  (helm-log "helm-output-filter--collect-candidates"
            "incomplete-line-info = %S" (cdr incomplete-line-info))
  (butlast
   (cl-loop for line in lines
            ;; On start `incomplete-line-info' value is empty string.
            for newline = (helm-aif (cdr incomplete-line-info)
                              (prog1
                                  (concat it line)
                                (setcdr incomplete-line-info nil))
                            line)
            collect newline
            ;; Store last incomplete line (last chunk truncated) until
            ;; new output arrives. Previously storing 'line' in
            ;; incomplete-line-info assumed output was truncated in
            ;; only two chunks. But output could be large and
            ;; truncated in more than two chunks. Therefore store
            ;; 'newline' to contain the previous chunks (Bug#1187).
            finally do (setcdr incomplete-line-info newline))))

(defun helm-output-filter--post-process ()
  (helm-aif (get-buffer-window helm-buffer 'visible)
      (with-selected-window it
        (helm-skip-noncandidate-line 'next)
        (helm-mark-current-line nil 'nomouse)
        ;; FIXME Don't hardcode follow delay.
        (helm-follow-execute-persistent-action-maybe 0.5)
        (helm-display-mode-line (helm-get-current-source))
        (helm-log-run-hook "helm-output-filter--post-process"
                           'helm-after-update-hook)
        (helm--reset-update-flag))))

(defun helm-process-deferred-sentinel-hook (process event file)
  "Defer remote processes in sentinels.
Meant to be called at the beginning of a sentinel process
function."
  (when (and (not (zerop helm-tramp-connection-min-time-diff))
             (string= event "finished\n")
             (or (file-remote-p file)
                 ;; `helm-suspend-update-flag'
                 ;; is non-`nil' here only during a
                 ;; running process, this will never be called
                 ;; when user set it explicitly with `C-!'.
                 helm-suspend-update-flag))
    (setq helm-suspend-update-flag t)
    ;; Kill the process but don't delete entry in
    ;; `helm-async-processes'.
    (helm-kill-async-process process)
    ;; When tramp opens the same connection twice in less than 5
    ;; seconds, it throws 'suppress, which calls the real-handler on
    ;; the main "Emacs". To avoid this [1] helm waits for 5 seconds
    ;; before updates yet allows user input during this delay. [1] In
    ;; recent Emacs versions, this has been fixed so tramp returns nil
    ;; in such conditions. Note: `tramp-connection-min-time-diff' cannot
    ;; have values less than 5 seconds otherwise the process dies.
    (run-at-time helm-tramp-connection-min-time-diff
                 nil (lambda ()
                       (when helm-alive-p ; Don't run timer fn after quit.
                         (setq helm-suspend-update-flag nil)
                         (helm-check-minibuffer-input))))))

(defun helm-kill-async-processes ()
  "Kill all asynchronous processes registered in `helm-async-processes'."
  (while helm-async-processes
    (helm-kill-async-process (caar helm-async-processes))
    (setq helm-async-processes (cdr helm-async-processes))))

(defun helm-kill-async-process (process)
  "Stop output from `helm-output-filter' and kill associated PROCESS."
  (set-process-filter process nil)
  (delete-process process))


;;; Actions
;;
(defun helm-execute-selection-action ()
  "Execute current action."
  (helm-log-run-hook "helm-execute-selection-action"
                     'helm-before-action-hook)
  ;; Position can be change when `helm-current-buffer'
  ;; is split, so jump to this position before executing action.
  (helm-current-position 'restore)
  (unwind-protect
      (prog1 (helm-execute-selection-action-1)
        (helm-log-run-hook "helm-execute-selection-action"
                           'helm-after-action-hook))
    (setq helm--executing-helm-action nil)))

(defun helm-execute-selection-action-1 (&optional
                                        selection action
                                        preserve-saved-action)
  "Execute ACTION on current SELECTION.
If PRESERVE-SAVED-ACTION is non-nil, then save the action."
  (helm-log "helm-execute-selection-action-1" "executing action")
  (setq action (helm-get-default-action
                (or action
                    helm-saved-action
                    (if (get-buffer helm-action-buffer)
                        (helm-get-selection helm-action-buffer)
                      (helm-get-actions-from-current-source)))))
  (helm-aif (and (not helm-in-persistent-action)
                 (get-buffer helm-action-buffer))
      (kill-buffer it))
  (let ((source (or helm-saved-current-source
                    (helm-get-current-source)))
        non-essential)
    (setq selection (helm-coerce-selection
                     (or selection
                         helm-saved-selection
                         (helm-get-selection nil nil source)
                         (and (assq 'accept-empty source) ""))
                     source))
    (unless preserve-saved-action (setq helm-saved-action nil))
    (when (and selection action) (funcall action selection))))

(defun helm-coerce-selection (selection source)
  "Apply coerce attribute function to SELECTION in SOURCE.
Coerce source with coerce function."
  (helm-aif (assoc-default 'coerce source)
      (helm-apply-functions-from-source source it selection)
    selection))

(defun helm-get-default-action (action)
  "Get the first ACTION value of action list in source."
  (if (and (listp action) (not (functionp action)))
      (cdar action)
    action))

(defun helm--show-action-window-other-window-p ()
  "Decide if window layout is suitable for showing action buffer.
Note that the return value is meaningful only at some point in the code,
i.e. before displaying action menu."
  (when helm-show-action-window-other-window
    ;; We were previously checking helm-split-window-state (eq vertical) to
    ;; decide to show action window, we now show it inconditionally in such case
    ;; but 'below'.
    (if (< (window-width (helm-window))
           (or split-width-threshold 160))
        'below
      helm-show-action-window-other-window)))

(defun helm-select-action ()
  "Select an action for the currently selected candidate.
If action buffer is selected, back to the Helm buffer."
  (interactive)
  (with-helm-alive-p
    (let ((src (helm-get-current-source)))
      (helm-log-run-hook "helm-select-action" 'helm-select-action-hook)
      (setq helm-saved-selection (helm-get-selection nil nil src))
      (with-selected-frame (with-helm-window (selected-frame))
        (prog1
            (helm-acond ((get-buffer-window helm-action-buffer 'visible)
                         (let ((delta (window-total-height it)))
                           (set-window-buffer it helm-buffer)
                           (helm--set-action-prompt 'restore)
                           ;; If `helm-show-action-window-other-window' is non nil
                           ;; we should have now two windows displaying
                           ;; helm-buffer, delete the one that was handling
                           ;; previously action buffer. 
                           (when (helm--show-action-window-other-window-p)
                             (delete-window it))
                           ;; Resize window on horizontal split, though for some
                           ;; reasons only 'above' needs to be resized. 
                           (when (memq helm-show-action-window-other-window '(below above))
                             (window-resize (get-buffer-window helm-buffer) delta))
                           (kill-buffer helm-action-buffer)
                           (setq helm-saved-selection nil)
                           (helm-set-pattern helm-input 'noupdate)
                           ;; Maybe hide minibuffer if helm was showing
                           ;; minibuffer in header-line and we are just toggling
                           ;; menu [1].
                           (helm-hide-minibuffer-maybe)))
                        (helm-saved-selection
                         (setq helm-saved-current-source src)
                         (let ((actions (helm-get-actions-from-current-source src))
                               helm-onewindow-p)
                           (if (functionp actions)
                               (message "Sole action: %s"
                                        (if (or (consp actions)
                                                (byte-code-function-p actions)
                                                (helm-subr-native-elisp-p actions))
                                            "Anonymous" actions))
                             (helm-show-action-buffer actions)
                             ;; Be sure the minibuffer is entirely deleted (bug#907).
                             (helm--delete-minibuffer-contents-from "")
                             ;; Unhide minibuffer to make visible action prompt [1].
                             (with-selected-window (minibuffer-window)
                               (remove-overlays) (setq cursor-type t))
                             (helm--set-action-prompt)
                             (helm-check-minibuffer-input))))
                        (t (message "No Actions available")))
          (helm-display-mode-line (helm-get-current-source))
          (run-hooks 'helm-window-configuration-hook))))))
(put 'helm-select-action 'helm-only t)

(defun helm-menu-select-action (_event)
  "Popup action menu from mouse-3."
  (interactive "e")
  (if (get-buffer-window helm-action-buffer 'visible)
      (helm-select-action)
    (let ((src (helm-get-current-source)))
      (helm-aif (helm-get-actions-from-current-source src)
          (progn
            (setq helm-saved-current-source src)
            (if (functionp it)
                (message "Sole action: %s"
                         (if (or (consp it)
                                 (byte-code-function-p it)
                                 (helm-subr-native-elisp-p it))
                             "Anonymous" it))
              (setq helm-saved-action
                    (x-popup-menu
                     t (list "Available Actions"
                             (cons "" it))))
              (helm-maybe-exit-minibuffer))
            (message "No Actions available"))))))
(put 'helm-menu-select-action 'helm-only t)

(defun helm--set-action-prompt (&optional restore)
  (with-selected-window (minibuffer-window)
    (let ((inhibit-read-only t)
          (props '(face minibuffer-prompt
                        field t
                        read-only t
                        rear-nonsticky t
                        front-sticky t))
          (prt (if restore helm--prompt helm--action-prompt)))
      (erase-buffer)
      (insert (apply #'propertize prt props))
      ;; Restore minibuffer depth indicator if the mode is enabled.
      (when minibuffer-depth-indicate-mode
        (minibuffer-depth-setup)))))

(defun helm-show-action-buffer (actions)
  (with-current-buffer (get-buffer-create helm-action-buffer)
    (erase-buffer)
    (buffer-disable-undo)
    (setq cursor-type nil)
    ;; Maybe display action buffer 'below' if window isn't large enough
    ;; (bug#2635).
    (set-window-buffer (helm-aif (helm--show-action-window-other-window-p)
                           (split-window (get-buffer-window helm-buffer) nil it)
                         (get-buffer-window helm-buffer))
                       helm-action-buffer)
    (set (make-local-variable 'helm-sources)
         (list
          (helm-build-sync-source "Actions"
            :volatile t
            :nomark t
            :persistent-action #'ignore
            :persistent-help "DoNothing"
            :keymap 'helm-map
            :candidates actions
            :mode-line '("Action(s)" "\\<helm-map>\\[helm-select-action]:BackToCands RET/f1/f2/fn:NthAct")
            :candidate-transformer
            (lambda (candidates)
              (cl-loop for (i . j) in candidates
                       for count from 1
                       collect
                       (cons (concat (cond ((> count 12)
                                            "      ")
                                           ((< count 10)
                                            (format "[f%s]  " count))
                                           (t (format "[f%s] " count)))
                                     (propertize i 'face 'helm-action))
                             j)))
            :candidate-number-limit nil)))
    (set (make-local-variable 'helm-source-filter) nil)
    (set (make-local-variable 'helm-selection-overlay) nil)
    (helm-initialize-overlays helm-action-buffer)))


;; Selection of candidates

(defun helm-display-source-at-screen-top-maybe (unit)
  "Display source at the top of screen when UNIT value is \\='source.
Return nil for any other value of UNIT."
  (when (and helm-display-source-at-screen-top (eq unit 'source))
    (set-window-start (selected-window)
                      (save-excursion (forward-line -1) (point)))))

(defun helm-skip-noncandidate-line (direction)
  "Skip source header or candidates separator when going in DIRECTION.
DIRECTION is either \\='next or \\='previous.
Same as `helm-skip-header-and-separator-line' but ensure point is
moved to the right place when at bob or eob."
  (helm-skip-header-and-separator-line direction)
  (and (bobp) (forward-line 1))     ; Skip first header.
  (and (eobp) (forward-line -1)))   ; Avoid last empty line.

(defun helm-skip-header-and-separator-line (direction)
  "Skip source header or candidate separator when going to next/previous line.
DIRECTION is either \\='next or \\='previous."
  (let ((fn (cl-ecase direction
              (next 'eobp)
              (previous 'bobp))))
    (while (and (not (funcall fn))
                (or (helm-pos-header-line-p)
                    (helm-pos-candidate-separator-p)))
      (forward-line (if (and (eq direction 'previous)
                             (not (eq (pos-bol) (point-min))))
                        -1 1)))))

(defun helm-display-mode-line (source &optional force)
  "Set up mode-line and header-line for `helm-buffer'.

SOURCE is a Helm source object.

Optional argument FORCE forces redisplay of the Helm buffer's
mode and header lines."
  (set (make-local-variable 'helm-mode-line-string)
       (helm-interpret-value (or (and (listp source) ; Check if source is empty.
                                      (assoc-default 'mode-line source))
                                 (default-value 'helm-mode-line-string))
                             source))
  (let ((follow (and (or (helm-follow-mode-p source)
                         (and helm-follow-mode-persistent
                              (member (assoc-default 'name source)
                                      helm-source-names-using-follow)))
                     " (HF)"))
        (marked (and helm-marked-candidates
                     (cl-loop with cur-name = (assoc-default 'name source)
                              for c in helm-marked-candidates
                              for name = (assoc-default 'name (car c))
                              when (string= name cur-name)
                              collect c))))
    ;; Setup mode-line.
    (if helm-mode-line-string
        (setq mode-line-format
              `(:propertize
                (" " mode-line-buffer-identification " "
                 (:eval (format "L%-3d" (helm-candidate-number-at-point)))
                 ,follow
                 " "
                 (:eval ,(and marked
                              (propertize
                               (format "M%d" (length marked))
                               'face 'helm-visible-mark)))
                 (:eval (when ,helm--mode-line-display-prefarg
                          (let ((arg (prefix-numeric-value
                                      (or prefix-arg current-prefix-arg))))
                            (unless (= arg 1)
                              (propertize (format " [prefarg:%s]" arg)
                                          'face 'helm-prefarg)))))
                 " "
                 (:eval (with-helm-buffer
                          (helm-show-candidate-number
                           (car-safe helm-mode-line-string))))
                 " " helm--mode-line-string-real " "
                 (:eval (make-string (window-width) ? )))
                keymap (keymap (mode-line keymap
                                          (mouse-1 . ignore)
                                          (down-mouse-1 . ignore)
                                          (drag-mouse-1 . ignore)
                                          (mouse-2 . ignore)
                                          (down-mouse-2 . ignore)
                                          (drag-mouse-2 . ignore)
                                          (mouse-3 . ignore)
                                          (down-mouse-3 . ignore)
                                          (drag-mouse-3 . ignore))))
              helm--mode-line-string-real
              (substitute-command-keys (if (listp helm-mode-line-string)
                                           (cadr helm-mode-line-string)
                                         helm-mode-line-string)))
      (setq mode-line-format (default-value 'mode-line-format)))
    ;; Setup header-line.
    (cond (helm-echo-input-in-header-line
           (setq force t)
           (helm--set-header-line))
          (helm-display-header-line
           (let ((hlstr (helm-interpret-value
                         (and (listp source)
                              (assoc-default 'header-line source))
                         source))
                 (endstr (make-string (window-width) ? )))
             (setq header-line-format
                   (propertize (concat " " hlstr endstr)
                               'face 'helm-header))))))
  (when force (force-mode-line-update)))

(defun helm--set-header-line (&optional update)
  (with-selected-window (minibuffer-window)
    (when helm-display-header-line
      ;; Prevent cursor movement over the overlay displaying
      ;; persistent-help in minibuffer (Bug#2108).
      (setq-local disable-point-adjustment t))
    (let* ((beg  (save-excursion (vertical-motion 0 (helm-window)) (point)))
           (end  (save-excursion (end-of-visual-line) (point)))
           ;; The visual line where the cursor is.
           (cont (buffer-substring beg end))
           (pref (propertize
                  " "
                  'display (if (string-match-p (regexp-opt `(,helm--prompt
                                                             ,helm--action-prompt))
                                               cont)
                               `(space :width ,helm-header-line-space-before-prompt)
                             (propertize
                              "->"
                              'face 'helm-header-line-left-margin))))
           (pos  (- (point) beg)))
      ;; Increment pos each time we find a "%" up to current-pos (bug#1648).
      (cl-loop for c across (buffer-substring-no-properties beg (point))
               when (eql c ?%) do (cl-incf pos))
      ;; Increment pos when cursor is on a "%" to make it visible in header-line
      ;; i.e "%%|" and not "%|%" (bug#1649).
      (when (eql (char-after) ?%) (setq pos (1+ pos)))
      (setq cont (replace-regexp-in-string "%" "%%" cont))
      (with-helm-buffer
        (setq header-line-format (concat pref cont " "))
        (funcall helm-default-prompt-display-function pos)
        (when update (force-mode-line-update))))))

(defun helm-set-default-prompt-display (pos)
  (put-text-property
   ;; Increment pos to handle the space before prompt (i.e `pref').
   (+ 1 pos) (+ 2 pos)
   'face
   ;; Don't just use cursor face, this can hide the current character.
   (list :inverse-video t
         :foreground (face-background 'cursor)
         :background (face-background 'default))
   header-line-format))

(defun helm-exchange-minibuffer-and-header-line ()
  "Display minibuffer in header-line and vice versa for current Helm session.

This is a toggle command."
  (interactive)
  (with-helm-window
    (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
    (setq-local helm-echo-input-in-header-line
                (not helm-echo-input-in-header-line))
    (with-selected-window (minibuffer-window)
      (if (with-helm-buffer helm-echo-input-in-header-line)
          (helm-hide-minibuffer-maybe)
        (remove-overlays)
        (setq cursor-type t)))
    (helm-display-mode-line (helm-get-current-source) t)))
(put 'helm-exchange-minibuffer-and-header-line 'helm-only t)

(defun helm--update-header-line ()
  ;; This should be used in `post-command-hook',
  ;; nowhere else.
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (helm--set-header-line t)))

(defun helm-hide-minibuffer-maybe ()
  "Hide minibuffer contents in a Helm session.
This function should normally go to `helm-minibuffer-set-up-hook'.
It has no effect if `helm-echo-input-in-header-line' is nil."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (helm-aif (and helm-display-header-line
                     (helm-get-attr 'persistent-help))
          (progn
            (overlay-put ov 'display
                         (truncate-string-to-width
                          (substitute-command-keys
                           (concat "\\<helm-map>\\[helm-execute-persistent-action]: "
                                   (format "%s (keeping session)" it)))
                          (- (window-width) 1)))
            (overlay-put ov 'face 'helm-header))
        (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                `(:background ,bg-color :foreground ,bg-color))))

      (setq cursor-type nil))))

(defun helm-show-candidate-number (&optional name)
  "Used to display candidate number in mode-line.
You can specify NAME of candidates e.g. \"Buffers\" otherwise it
is \"Candidate(s)\" by default."
  (when helm-alive-p
    (unless (helm-empty-source-p)
      ;; Build a fixed width string when candidate-number < 1000
      (let* ((cand-name (or name "Candidate(s)"))
             (width (length (format "[999 %s]" cand-name))))
        (propertize
         (format (concat "%-" (number-to-string width) "s")
                 (format "[%s %s]"
                         (helm-get-candidate-number 'in-current-source)
                         cand-name))
         'face (if helm-suspend-update-flag
                   'helm-candidate-number-suspended
                 'helm-candidate-number))))))

(cl-defun helm-move-selection-common (&key where direction (follow t))
  "Move the selection marker to a new position.
Position is determined by WHERE and DIRECTION.
Key arg WHERE can be one of:
 - line
 - page
 - edge
 - source
Key arg DIRECTION can be one of:
 - previous
 - next
 - A source or a source name when used with :WHERE \\='source."
  (let ((move-func (cl-case where
                     (line (cl-ecase direction
                             (previous 'helm-move--previous-line-fn)
                             (next 'helm-move--next-line-fn)))
                     (page (cl-ecase direction
                             (previous 'helm-move--previous-page-fn)
                             (next 'helm-move--next-page-fn)))
                     (edge (cl-ecase direction
                             (previous 'helm-move--beginning-of-buffer-fn)
                             (next 'helm-move--end-of-buffer-fn)))
                     (source (cl-case direction
                               (previous 'helm-move--previous-source-fn)
                               (next 'helm-move--next-source-fn)
                               (t (lambda () ; A source is passed as DIRECTION arg.
                                    (helm-move--goto-source-fn direction)))))))
        source)
    (unless (or (helm-empty-buffer-p (helm-buffer-get))
                (not (helm-window)))
      (with-helm-window
        (when helm-allow-mouse
          (helm--mouse-reset-selection-help-echo))
        (helm-log-run-hook "helm-move-selection-common"
                           'helm-move-selection-before-hook)
        (funcall move-func)
        (and (memq direction '(next previous))
             (helm-skip-noncandidate-line direction))
        (when (helm-pos-multiline-p)
          (helm-move--beginning-of-multiline-candidate))
        (helm-display-source-at-screen-top-maybe where)
        (helm-mark-current-line)
        (when follow
          (helm-follow-execute-persistent-action-maybe))
        (helm-display-mode-line (setq source (helm-get-current-source)))
        (helm-log-run-hook "helm-move-selection-common"
                           'helm-move-selection-after-hook)
        (helm--set-minibuffer-completion-confirm source)))))

(defun helm-move--beginning-of-multiline-candidate ()
  (let ((header-pos (helm-get-previous-header-pos))
        (separator-pos (helm-get-previous-candidate-separator-pos)))
    (when header-pos
      (goto-char (if (or (null separator-pos)
                         (< separator-pos header-pos))
                     header-pos
                   separator-pos))
      (forward-line 1))))

(defun helm-move--previous-multi-line-fn ()
  (forward-line -1)
  (unless (helm-pos-header-line-p)
    (helm-skip-header-and-separator-line 'previous)
    (helm-move--beginning-of-multiline-candidate)))

(defun helm-move--previous-line-fn ()
  (if (not (helm-pos-multiline-p))
      (forward-line -1)
    (helm-move--previous-multi-line-fn))
  (when (and helm-move-to-line-cycle-in-source
             (helm-pos-header-line-p))
    (forward-line 1)
    (helm-move--end-of-source)
    ;; We are at end of helm-buffer
    ;; check if last candidate is a multiline candidate
    ;; and jump to it
    (when (and (eobp)
               (save-excursion (forward-line -1) (helm-pos-multiline-p)))
      (helm-move--previous-multi-line-fn))))

(defun helm-move--next-multi-line-fn ()
  (let ((header-pos (helm-get-next-header-pos))
        (separator-pos (helm-get-next-candidate-separator-pos)))
    (cond ((and separator-pos
                (or (null header-pos) (< separator-pos header-pos)))
           (goto-char separator-pos))
          (header-pos
           (goto-char header-pos)))))

(defun helm-move--next-line-fn ()
  (if (not (helm-pos-multiline-p))
      (forward-line 1)
    (helm-move--next-multi-line-fn))
  (when (and helm-move-to-line-cycle-in-source
             (or (save-excursion (and (helm-pos-multiline-p)
                                      (goto-char (overlay-end
                                                  helm-selection-overlay))
                                      (helm-end-of-source-p t)))
                 (helm-end-of-source-p t)))
    (helm-move--beginning-of-source)
    (helm-display-source-at-screen-top-maybe 'source)))

(defun helm-move--previous-page-fn ()
  (condition-case nil
      (scroll-down helm-scroll-amount)
    (beginning-of-buffer (goto-char (point-min)))))

(defun helm-move--next-page-fn ()
  (condition-case nil
      (scroll-up helm-scroll-amount)
    (end-of-buffer (goto-char (point-max)))))

(defun helm-move--beginning-of-buffer-fn ()
  (goto-char (point-min)))

(defun helm-move--end-of-buffer-fn ()
  (goto-char (point-max)))

(defun helm-move--end-of-source ()
  (helm-aif (helm-get-next-header-pos)
      (progn (goto-char it) (forward-line -2))
    (goto-char (point-max))))

(defun helm-move--beginning-of-source ()
  (helm-aif (helm-get-previous-header-pos)
      (progn (goto-char it)
             (forward-line 1))
    (goto-char (point-min))))

(defun helm-move--previous-source-fn ()
  (forward-line -1)
  (if (bobp)
      (goto-char (point-max))
    (helm-skip-header-and-separator-line 'previous))
  (goto-char (helm-get-previous-header-pos))
  (forward-line 1))

(defun helm-move--next-source-fn ()
  (goto-char (or (and (not (save-excursion
                             (forward-line 1) (eobp)))
                      ;; Empty source at eob are just
                      ;; not displayed unless they are dummy.
                      ;; Bug#1117.
                      (helm-get-next-header-pos))
                 (point-min))))

(defun helm-move--goto-source-fn (source-or-name)
  (goto-char (point-min))
  (let ((name (if (stringp source-or-name)
                  source-or-name
                (assoc-default 'name source-or-name))))
    (if (or (null name) (string= name ""))
        (forward-line 1)
      (condition-case err
          (while (not (string= name (helm-current-line-contents)))
            (goto-char (helm-get-next-header-pos)))
        (error (helm-log "helm-move--goto-source-fn" "%S" err))))))

(defun helm-candidate-number-at-point ()
  (if helm-alive-p
      (with-helm-buffer
        (or (get-text-property (point) 'helm-cand-num) 1))
    (or (get-text-property (point) 'helm-cand-num) 1)))

(defun helm--next-or-previous-line (direction &optional arg)
  ;; Be sure to not use this in non--interactives calls.
  (let ((helm-move-to-line-cycle-in-source
         (and helm-move-to-line-cycle-in-source arg)))
    (if (and arg (> arg 1))
        (cl-loop with pos = (helm-candidate-number-at-point)
                 with cand-num = (helm-get-candidate-number t)
                 with iter = (min arg (if (eq direction 'next)
                                          (- cand-num pos)
                                        (min arg (1- pos))))
                 for count from 1
                 while (<= count iter)
                 do
                 (helm-move-selection-common :where 'line :direction direction))
      (helm-move-selection-common :where 'line :direction direction))))

(defun helm-previous-line (&optional arg)
  "Move selection to the ARG previous line(s).
Same behavior as `helm-next-line' when called with a numeric
prefix arg."
  (interactive "p")
  (with-helm-alive-p
    (helm--next-or-previous-line 'previous arg)))
(put 'helm-previous-line 'helm-only t)

(defun helm-next-line (&optional arg)
  "Move selection to the next ARG line(s).
When numeric prefix arg is > than the number of candidates, then
move to the last candidate of current source (i.e. don't move to
next source)."
  (interactive "p")
  (with-helm-alive-p
    (helm--next-or-previous-line 'next arg)))
(put 'helm-next-line 'helm-only t)

(defun helm-scroll-up ()
  "Scroll up helm-buffer by `helm-scroll-amount' lines."
  (interactive)
  (with-helm-alive-p
    (helm-move-selection-common :where 'page :direction 'previous)))
(put 'helm-scroll-up 'helm-only t)

(defun helm-previous-page ()
  "Move selection back with a pageful."
  (interactive)
  (with-helm-alive-p
    (let (helm-scroll-amount)
      (helm-move-selection-common :where 'page :direction 'previous))))
(put 'helm-previous-page 'helm-only t)

(defun helm-scroll-down ()
  "Scroll down helm-buffer by `helm-scroll-amount' lines."
  (interactive)
  (with-helm-alive-p
    (helm-move-selection-common :where 'page :direction 'next)))
(put 'helm-scroll-down 'helm-only t)

(defun helm-next-page ()
  "Move selection forward with a pageful."
  (interactive)
  (with-helm-alive-p
    (let (helm-scroll-amount)
      (helm-move-selection-common :where 'page :direction 'next))))
(put 'helm-next-page 'helm-only t)

(defun helm-beginning-of-buffer ()
  "Move selection at the top."
  (interactive)
  (with-helm-alive-p
    (helm-move-selection-common :where 'edge :direction 'previous)))
(put 'helm-beginning-of-buffer 'helm-only t)

(defun helm-end-of-buffer ()
  "Move selection at the bottom."
  (interactive)
  (with-helm-alive-p
    (helm-move-selection-common :where 'edge :direction 'next)))
(put 'helm-end-of-buffer 'helm-only t)

(defun helm-previous-source ()
  "Move selection to the previous source."
  (interactive)
  (with-helm-alive-p
    (helm-move-selection-common :where 'source :direction 'previous)))
(put 'helm-previous-source 'helm-only t)

(defun helm-next-source ()
  "Move selection to the next source."
  (interactive)
  (with-helm-alive-p
    (helm-move-selection-common :where 'source :direction 'next)))
(put 'helm-next-source 'helm-only t)

(defun helm-goto-source (&optional source-or-name)
  "Move the selection to the source named SOURCE-OR-NAME.

If SOURCE-OR-NAME is empty string or nil go to the first
candidate of first source."
  (helm-move-selection-common :where 'source :direction source-or-name))

(defvar helm-follow-action-white-list-commands
  '(helm-ff-decrease-image-size-persistent
    helm-ff-increase-image-size-persistent
    helm-ff-rotate-left-persistent
    helm-ff-rotate-right-persistent)
  "Allow `helm-follow-action-forward/backward' switching to next file
when one of these commands is the `last-command'.

For example when browsing files with `C-<down>` and rotate the current file,
hitting `C-<down>` again will not switch to next file but kill its buffer.")

(defun helm--follow-action (arg)
  (let ((helm--temp-follow-flag t) ; Needed in HFF.
        (in-follow-mode (helm-follow-mode-p)))
    ;; When follow-mode is already enabled, just go to next or
    ;; previous line.
    (when (or (eq last-command 'helm-follow-action-forward)
              (eq last-command 'helm-follow-action-backward)
              (eq last-command 'helm-execute-persistent-action)
              (memq last-command helm-follow-action-white-list-commands)
              in-follow-mode)
      (if (> arg 0)
          (helm-move-selection-common :where 'line
                                      :direction 'next
                                      :follow nil)
        (helm-move-selection-common :where 'line
                                    :direction 'previous
                                    :follow nil)))
    (unless in-follow-mode
      (helm-execute-persistent-action))))

(defun helm-follow-action-forward ()
  "Go to next line and execute persistent action."
  (interactive)
  (with-helm-alive-p (helm--follow-action 1)))
(put 'helm-follow-action-forward 'helm-only t)

(defun helm-follow-action-backward ()
  "Go to previous line and execute persistent action."
  (interactive)
  (with-helm-alive-p (helm--follow-action -1)))
(put 'helm-follow-action-backward 'helm-only t)

(defun helm-mark-current-line (&optional resumep nomouse)
  "Move `helm-selection-overlay' to current line.
When RESUMEP is non nil move overlay to `helm-selection-point'.
When NOMOUSE is specified do not set mouse bindings.

Note that selection is unrelated to visible marks used for
marking candidates."
  (with-helm-buffer
    (when resumep
      (goto-char helm-selection-point))
    (move-overlay
     helm-selection-overlay (pos-bol)
     (if (helm-pos-multiline-p)
         (let ((header-pos (helm-get-next-header-pos))
               (separator-pos (helm-get-next-candidate-separator-pos)))
           (or (and (null header-pos) separator-pos)
               (and header-pos separator-pos
                    (< separator-pos header-pos)
                    separator-pos)
               header-pos
               (point-max)))
       (1+ (pos-eol))))
    (setq helm-selection-point (overlay-start helm-selection-overlay))
    (when (and helm-allow-mouse (null nomouse))
      (helm--bind-mouse-for-selection helm-selection-point))))

(defun helm-confirm-and-exit-minibuffer ()
  "Maybe ask for confirmation when exiting helm.
It is similar to `minibuffer-complete-and-exit' adapted to Helm.
If `minibuffer-completion-confirm' value is \\='confirm, send
minibuffer confirm message and exit on next hit.  If
`minibuffer-completion-confirm' value is t, don't exit and send
message \\='no match'."
  (interactive)
  (with-helm-alive-p
    (if (and (helm--updating-p)
             (null helm--reading-passwd-or-string))
        (progn (message "[Display not ready]")
               (sit-for 0.5) (message nil)
               (helm-update))
      (let* ((src (helm-get-current-source))
             (empty-buffer-p (with-current-buffer helm-buffer
                               (eq (point-min) (point-max))))
             (unknown (and (not empty-buffer-p)
                           ;; Now such candidates have a helm-new-file or an
                           ;; unknown text property (we were testing if string
                           ;; match [?] previously).
                           (helm-candidate-prefixed-p
                            (helm-get-selection nil 'withprop src)))))
        (cond ((and (or empty-buffer-p unknown)
                    (memq minibuffer-completion-confirm
                          '(confirm confirm-after-completion)))
               (setq helm-minibuffer-confirm-state
                     'confirm)
               (setq minibuffer-completion-confirm nil)
               (minibuffer-message " [confirm]"))
              ;; When require-match is strict (i.e. `t'), buffer
              ;; should be either empty or in read-file-name have an
              ;; unknown candidate ([+] prefix), if it's not the case
              ;; fix it in helm-mode but not here.
              ;; When `minibuffer-completion-confirm' is set to 'noexit or
              ;; 'exit, that's mean MUST-MATCH is a function and we use its
              ;; return value to set `minibuffer-completion-confirm', this is
              ;; done in `helm--set-minibuffer-completion-confirm'.
              ((or (eq minibuffer-completion-confirm 'noexit)
                   (and (or empty-buffer-p unknown)
                        (eq minibuffer-completion-confirm t)))
               (minibuffer-message " [No match]"))
              (empty-buffer-p
               ;; This is used when helm-buffer is totally empty,
               ;; i.e. the [+] have not been added because must-match
               ;; is used from outside helm-comp-read i.e. from a helm
               ;; source built with :must-match.
               (setq helm-saved-selection helm-pattern
                     helm-saved-action (helm-get-default-action
                                        (assoc-default
                                         'action
                                         (car (with-helm-buffer helm-sources))))
                     helm-minibuffer-confirm-state nil)
               (helm-exit-minibuffer))
              (t
               (setq helm-minibuffer-confirm-state nil)
               (helm-exit-minibuffer)))))))
(put 'helm-confirm-and-exit-minibuffer 'helm-only t)

(defun helm-confirm-and-exit-hook ()
  "Restore `minibuffer-completion-confirm' when helm update."
  (unless (or (eq minibuffer-completion-confirm t)
              (not helm-minibuffer-confirm-state))
    (setq minibuffer-completion-confirm
          helm-minibuffer-confirm-state)))
(add-hook 'helm-after-update-hook 'helm-confirm-and-exit-hook)

(defun helm--set-minibuffer-completion-confirm (src)
  (with-helm-buffer
    (setq minibuffer-completion-confirm
          (pcase (helm-get-attr 'must-match src)
            ((and (pred functionp) fun
                  (let sel (helm-get-selection nil nil src)))
             (if (funcall fun sel) 'exit 'noexit))
            (val val)))))

(defun helm-read-string (prompt &optional initial-input history
                                default-value inherit-input-method)
  "Same as `read-string' but for reading string from a helm session."
  (let ((helm--reading-passwd-or-string t))
    (read-string
     prompt initial-input history default-value inherit-input-method)))

(defun helm--updating-p ()
  ;; helm timer is between two cycles.
  ;; IOW `helm-check-minibuffer-input' haven't yet compared input
  ;; and `helm-pattern'.
  (or (not (equal (minibuffer-contents) helm-pattern))
      ;; `helm-check-minibuffer-input' have launched `helm-update'.
      helm--in-update))

(defun helm-maybe-exit-minibuffer ()
  (interactive)
  (with-helm-alive-p
    (if (and (helm--updating-p)
             (null helm--reading-passwd-or-string))
        (progn
          (message "[Display not ready]")
          (sit-for 0.5) (message nil)
          (helm-update))
      (helm-exit-minibuffer))))
(put 'helm-maybe-exit-minibuffer 'helm-only t)

(defun helm-exit-minibuffer ()
  "Select the current candidate by exiting the minibuffer."
  (unless helm-current-prefix-arg
    (setq helm-current-prefix-arg current-prefix-arg))
  (setq helm-exit-status 0)
  (helm-log-run-hook "helm-exit-minibuffer"
                     'helm-exit-minibuffer-hook)
  (exit-minibuffer))

(defun helm-keyboard-quit ()
  "Quit minibuffer in helm.
If action buffer is displayed, kill it."
  (interactive)
  (with-helm-alive-p
    (when (get-buffer-window helm-action-buffer 'visible)
      (kill-buffer helm-action-buffer))
    (setq helm-exit-status 1)
    (abort-recursive-edit)))
(put 'helm-keyboard-quit 'helm-only t)

(defun helm-get-next-header-pos ()
  "Return the position of the next header from point."
  (next-single-property-change (point) 'helm-header))

(defun helm-get-previous-header-pos ()
  "Return the position of the previous header from point."
  (previous-single-property-change (point) 'helm-header))

(defun helm-pos-multiline-p ()
  "Return non-`nil' if the current position is in the multiline source region."
  (get-text-property (point) 'helm-multiline))

(defun helm-get-next-candidate-separator-pos ()
  "Return the position of the next candidate separator from point."
  (let ((hp (helm-get-next-header-pos)))
    (helm-aif (next-single-property-change (point) 'helm-candidate-separator)
        (or
         ;; Be sure we don't catch
         ;; the separator of next source.
         (and hp (< it hp) it)
         ;; The separator found is in next source
         ;; we are at last cand, so use the header pos.
         (and hp (< hp it) hp)
         ;; A single source, just try next separator.
         it))))

(defun helm-get-previous-candidate-separator-pos ()
  "Return the position of the previous candidate separator from point."
  (previous-single-property-change (point) 'helm-candidate-separator))

(defun helm-pos-header-line-p ()
  "Return t if the current line is a header line."
  (or (get-text-property (pos-bol) 'helm-header)
      (get-text-property (pos-bol) 'helm-header-separator)))

(defun helm-pos-candidate-separator-p ()
  "Return t if the current line is a candidate separator."
  (get-text-property (pos-bol) 'helm-candidate-separator))


;;; Debugging
;;
;;
(defun helm-debug-output ()
  "Show all Helm locals variables and output of `helm-debug-function'."
  (interactive)
  (with-helm-alive-p
    (helm-help-internal helm-debug-output-buffer 'helm-debug-output-function)))
(put 'helm-debug-output 'helm-only t)

(defun helm-default-debug-function ()
  "Collect sources of helm current session without their keymap.
This is the default function for `helm-debug-function'."
  (cl-loop for source in (with-helm-buffer helm-sources)
           collect (remove (assq 'keymap source) source)))

(defun helm-debug-output-function ()
  (let ((local-vars (buffer-local-variables (get-buffer helm-buffer)))
        (count 1))
    (insert (format "* Helm debug from `%s' buffer\n\n" helm-buffer))
    (insert "** Local variables\n\n#+begin_src elisp\n"
            (pp-to-string (remove (assq 'helm-sources local-vars) local-vars))
            "\n#+end_src\n")
    (dolist-with-progress-reporter (v (helm-interpret-value helm-debug-function))
        "Calculating all helm-related values..."
      (insert (format "** Value%s\n" count)
              "#+begin_src elisp\n" (pp-to-string v) "\n#+end_src\n")
      (cl-incf count))))

(defun helm-enable-or-switch-to-debug ()
  "First hit enable helm debugging, second hit switch to debug buffer."
  (interactive)
  (with-helm-alive-p
    (if helm-debug
        (helm-run-after-exit
         #'helm-debug-open-last-log)
      (setq helm-debug t)
      (with-helm-buffer (setq truncate-lines nil))
      (message "Debugging enabled"))))
(put 'helm-enable-or-switch-to-debug 'helm-only t)


;; Misc

(defun helm-preselect (candidate-or-regexp &optional source)
  "Move selection to CANDIDATE-OR-REGEXP on Helm start.

CANDIDATE-OR-REGEXP can be a:

- String
- Cons cell of two strings
- Nullary function, which moves to a candidate

When CANDIDATE-OR-REGEXP is a cons cell, tries moving to first
element of the cons cell, then the second, and so on.  This allows
selection of duplicate candidates after the first.

Optional argument SOURCE is a Helm source object."
  (with-helm-buffer
    (when candidate-or-regexp
      (if source
          (helm-goto-source source)
        (goto-char (point-min))
        (forward-line 1))
      (if (functionp candidate-or-regexp)
          (funcall candidate-or-regexp)
        (let ((start (point)) mp)
          (helm-awhile (if (consp candidate-or-regexp)
                           (and (re-search-forward (car candidate-or-regexp) nil t)
                                (re-search-forward (cdr candidate-or-regexp) nil t))
                         (re-search-forward candidate-or-regexp nil t))
            ;; If search fall on an header line continue loop
            ;; until it match or fail (Bug#1509).
            (unless (helm-pos-header-line-p) (cl-return (setq mp it))))
          (goto-char (or mp start)))))
    (forward-line 0) ; Avoid scrolling right on long lines.
    (when (helm-pos-multiline-p)
      (helm-move--beginning-of-multiline-candidate))
    (when (helm-pos-header-line-p) (forward-line 1))
    (when helm-allow-mouse
      (helm--mouse-reset-selection-help-echo))
    (helm-mark-current-line)
    (helm-display-mode-line (or source (helm-get-current-source)))
    (helm-log-run-hook "helm-preselect" 'helm-after-preselection-hook)))

(defun helm-delete-current-selection ()
  "Delete the currently selected item."
  (with-helm-window
    (cond ((helm-pos-multiline-p)
           (helm-aif (helm-get-next-candidate-separator-pos)
               (delete-region (pos-bol)
                              (1+ (progn (goto-char it) (pos-eol))))
             ;; last candidate
             (goto-char (helm-get-previous-candidate-separator-pos))
             (delete-region (pos-bol) (point-max)))
           (when (helm-end-of-source-p)
             (goto-char (or (helm-get-previous-candidate-separator-pos)
                            (point-min)))
             (forward-line 1)))
          (t
           (delete-region (pos-bol) (1+ (pos-eol)))
           (when (helm-end-of-source-p t)
             (let ((headp (save-excursion
                            (forward-line -1)
                            (not (helm-pos-header-line-p)))))
               (and headp (forward-line -1))))))
    (unless (helm-end-of-source-p t)
      (helm-mark-current-line))))

(defun helm-end-of-source-1 (n at-point)
  (save-excursion
    (if (and (helm-pos-multiline-p) (null at-point))
        (null (helm-get-next-candidate-separator-pos))
      (forward-line (if at-point 0 n))
      (or (eq (pos-bol) (pos-eol))
          (helm-pos-header-line-p)
          (if (< n 0) (bobp) (eobp))))))

(defun helm-end-of-source-p (&optional at-point)
  "Return non-nil if we are at EOB or end of source."
  (helm-end-of-source-1 1 at-point))

(defun helm-beginning-of-source-p (&optional at-point)
  "Return non-nil if we are at BOB or beginning of source."
  (helm-end-of-source-1 -1 at-point))

(defun helm--edit-current-selection-internal (func)
  (with-helm-window
    (forward-line 0)
    (let ((realvalue (get-text-property (point) 'helm-realvalue))
          (multiline (get-text-property (point) 'helm-multiline)))
      (funcall func)
      (forward-line 0)
      (and realvalue
           (put-text-property (point) (pos-eol)
                              'helm-realvalue realvalue))
      (and multiline
           (put-text-property (point)
                              (or (helm-get-next-candidate-separator-pos)
                                  (point-max))
                              'helm-multiline multiline))
      (helm-mark-current-line))))

(defmacro helm-edit-current-selection (&rest forms)
  "Evaluate FORMS at current selection in the helm buffer.
Used generally to modify current selection."
  (declare (indent 0) (debug t))
  `(helm--edit-current-selection-internal
    (lambda () ,@forms)))

(defun helm--delete-minibuffer-contents-from (from-str)
  ;; Giving an empty string value to FROM-STR delete all.
  (let ((input (minibuffer-contents)))
    (helm-reset-yank-point)
    (if (> (length input) 0)
        ;; minibuffer is not empty, delete contents from end
        ;; of FROM-STR and update.
        (helm-set-pattern from-str)
      ;; minibuffer is already empty, force update.
      (helm-force-update))))

(defun helm-delete-minibuffer-contents (&optional arg)
  "Delete minibuffer contents.
When `helm-delete-minibuffer-contents-from-point' is non-nil,
delete minibuffer contents from point instead of deleting all.
With a prefix arg reverse this behaviour.  When at the end of
minibuffer, delete all."
  (interactive "P")
  (with-helm-alive-p
    (let ((str (if helm-delete-minibuffer-contents-from-point
                   (if (or arg (eobp))
                       "" (helm-minibuffer-completion-contents))
                 (if (and arg (not (eobp)))
                     (helm-minibuffer-completion-contents) ""))))
      (helm--delete-minibuffer-contents-from str))))
(put 'helm-delete-minibuffer-contents 'no-helm-mx t)


;;; helm-source-in-buffer.
;;
(defun helm-candidates-in-buffer (&optional source)
  "The top level function used to store candidates with `helm-source-in-buffer'.

Candidates are stored in a buffer generated internally by
`helm-candidate-buffer' function.  Each candidate must be placed
in one line.

The buffer is created and fed in the init attribute function of
Helm.

E.g.:

     (helm-build-in-buffer-source \"test\"
       :init (lambda ()
               (helm-init-candidates-in-buffer
                   \\='global \\='(foo foa fob bar baz))))

A shortcut can be used to simplify:

     (helm-build-in-buffer-source \"test\"
       :data \\='(foo foa fob bar baz))

By default, Helm makes candidates by evaluating the candidates
function, then narrows them by `string-match' for each candidate.

But this is slow for large number of candidates.  The new way is
to store all candidates in a buffer and then narrow with
`re-search-forward'.  Search function is customizable by search
attribute.  The important point is that buffer processing is MUCH
FASTER than string list processing and is the Emacs way.

The init function writes all candidates to a newly-created
candidate buffer.  The candidates buffer is created or specified
by `helm-candidate-buffer'.  Candidates are stored in a line.

The candidates function narrows all candidates, IOW creates a
subset of candidates dynamically.

Class `helm-source-in-buffer' is implemented with three attributes:

    (candidates . helm-candidates-in-buffer)
    (volatile)
    (match identity)

The volatile attribute is needed because
`helm-candidates-in-buffer' creates candidates dynamically and
need to be called every time `helm-pattern' changes.

Because `helm-candidates-in-buffer' plays the role of `match'
attribute function, specifying `(match identity)' makes the
source slightly faster.

However if source contains `match-part' attribute, match is
computed only on part of candidate returned by the call of
function provided by this attribute.  The function should have one
arg, candidate, and return only a specific part of candidate.

To customize `helm-candidates-in-buffer' behaviour, use `search',
`get-line' and `match-part' attributes."
  (let ((src (or source (helm-get-current-source))))
    (helm-candidates-in-buffer-1
     (helm-candidate-buffer)
     helm-pattern
     (or (assoc-default 'get-line src)
         #'buffer-substring-no-properties)
     (or (assoc-default 'search src)
         '(helm-candidates-in-buffer-search-default-fn))
     ;; When candidate-transformer is specified in source ALL candidates should
     ;; be computed with the candidate-transformer function (in contrast with
     ;; filtered-candidate-transformer).  This to be consistent with what sync
     ;; sources do. The car of the cons is used for initial fetching of
     ;; candidates whereas the cdr is used after when searching (in this case
     ;; the candidate number limit is used).
     (if (helm-get-attr 'candidate-transformer src)
         (cons 99999999 (helm-candidate-number-limit src))
       (helm-candidate-number-limit src))
     (helm-get-attr 'match-part)
     src)))

(defun helm-candidates-in-buffer-search-default-fn (pattern)
  "Search PATTERN with `re-search-forward' with bound and noerror args."
  (condition-case _err
      (re-search-forward pattern nil t)
    (invalid-regexp nil)))

(defun helm-candidates-in-buffer-1 (buffer pattern get-line-fn
                                           search-fns limit
                                           match-part-fn source)
  "Return the list of candidates inserted in BUFFER matching PATTERN."
  ;; buffer == nil when candidates buffer does not exist.
  (when buffer
    (with-current-buffer buffer
      (let ((inhibit-point-motion-hooks t)
            (start-point (1- (point-min))))
        (goto-char start-point)
        (if (string= pattern "")
            (helm-initial-candidates-from-candidate-buffer
             get-line-fn (if (consp limit) (car limit) limit))
          (helm-search-from-candidate-buffer
           pattern get-line-fn search-fns
           (if (consp limit) (cdr limit) limit)
           start-point match-part-fn source))))))


(defun helm-search-from-candidate-buffer (pattern get-line-fn search-fns
                                                  limit start-point match-part-fn source)
  (let ((inhibit-read-only t)
        (diacritics (assoc-default 'diacritics source)))
    (helm--search-from-candidate-buffer-1
     (lambda ()
       (cl-loop with hash = (make-hash-table :test 'equal)
                with allow-dups = (assq 'allow-dups source)
                with case-fold-search = (helm-set-case-fold-search)
                with count = 0
                for iter from 1
                for searcher in search-fns
                do (progn
                     (goto-char start-point)
                     ;; The character at start-point is a newline,
                     ;; if pattern match it that's mean we are
                     ;; searching for newline in buffer, in this
                     ;; case skip this false line.
                     ;; See comment >>>[1] in
                     ;; `helm--search-from-candidate-buffer-1'.
                     (and (condition-case nil
                              (looking-at pattern)
                            (invalid-regexp nil))
                          (forward-line 1)))
                nconc
                (cl-loop with pos-lst
                         ;; POS-LST is used as a flag to decide if we
                         ;; run `helm-search-match-part' even if
                         ;; MATCH-PART isn't specified on source. This
                         ;; happen when fuzzy matching or using a
                         ;; negation (!) in one of the patterns, in
                         ;; these case the searcher returns a list
                         ;; '(BEG END) instead of an integer like
                         ;; `re-search-forward'.
                         while (and (setq pos-lst (funcall searcher pattern))
                                    (not (eobp))
                                    (< count limit))
                         for cand = (apply get-line-fn
                                           (if (and pos-lst (listp pos-lst))
                                               pos-lst
                                             (list (pos-bol) (pos-eol))))
                         when (and match-part-fn
                                   (not (get-text-property 0 'match-part cand)))
                         do (setq cand
                                  (propertize cand 'match-part (funcall match-part-fn cand)))
                         for dup = (gethash cand hash)
                         when (and (or (and allow-dups dup (= dup iter))
                                       (null dup))
                                   (or
                                    ;; Always collect when cand is matched
                                    ;; by searcher funcs and match-part attr
                                    ;; is not present.
                                    (and (not match-part-fn)
                                         (not (consp pos-lst)))
                                    ;; If match-part attr is present, or if SEARCHER fn
                                    ;; returns a cons cell, collect PATTERN only if it
                                    ;; match the part of CAND specified by
                                    ;; the match-part func.
                                    (helm-search-match-part cand pattern diacritics)))
                         do (progn
                              (puthash cand iter hash)
                              (helm--maybe-process-filter-one-by-one-candidate cand source)
                              (cl-incf count))
                         and collect cand))))))

(defun helm-search-match-part (candidate pattern diacritics)
  "Match PATTERN only on match-part property value of CANDIDATE.

Because `helm-search-match-part' may be called even if
unspecified in source (negation or fuzzy), the part to match
falls back to the whole candidate even if match-part hasn't been
computed by match-part-fn and stored in the match-part property."
  (let ((part (or (get-text-property 0 'match-part candidate)
                  candidate))
        (fuzzy-regexp (cadr (gethash 'helm-pattern helm--fuzzy-regexp-cache)))
        (matchfn (cond (helm-migemo-mode 'helm-mm-migemo-string-match)
                       (diacritics 'helm-mm-diacritics-string-match)
                       (t 'string-match))))
    (condition-case _err
        (if (string-match " " pattern)
            (cl-loop for i in (helm-mm-split-pattern pattern) always
                     (if (string-match "\\`!" i)
                         (not (funcall matchfn (substring i 1) part))
                       (funcall matchfn i part)))
          (if (string-match "\\`!" pattern)
              (if helm--in-fuzzy
                  ;; Fuzzy regexp have already been
                  ;; computed with substring 1.
                  (not (string-match fuzzy-regexp part))
                (not (funcall matchfn (substring pattern 1) part)))
            (funcall matchfn (if helm--in-fuzzy fuzzy-regexp pattern) part)))
      (invalid-regexp nil))))

(defun helm-initial-candidates-from-candidate-buffer (get-line-fn limit)
  (cl-loop repeat limit
           until (eobp)
           for line = (funcall get-line-fn (pos-bol) (pos-eol))
           when line collect line
           do (forward-line 1)))

(defun helm--search-from-candidate-buffer-1 (search-fn)
  ;; We are adding a newline at bob and at eol
  ;; and removing these newlines afterward.
  ;; This is a bad hack that should be removed.
  ;; To avoid matching the empty line at first line
  ;; when searching with e.g occur and "^$" just
  ;; forward-line before searching (See >>>[1] above).
  (goto-char (point-min))
  (insert "\n")
  (goto-char (point-max))
  (insert "\n")
  (unwind-protect
      (funcall search-fn)
    (goto-char (point-min))
    (delete-char 1)
    (goto-char (1- (point-max)))
    (delete-char 1)
    (set-buffer-modified-p nil)))

(defun helm-candidate-buffer (&optional buffer-spec)
  "Register and return a buffer storing candidates of current source.

This is used to initialize a buffer for storing candidates for a
candidates-in-buffer source, candidates will be searched in this
buffer and displayed in `helm-buffer'.  This should be used only
in init functions, don't relay on this in other places unless you
know what you are doing.

This function is still in public API only for backward
compatibility, you should use instead
`helm-init-candidates-in-buffer' for initializing your sources.

Internally, this function is called without argument and returns
the buffer corresponding to current source i.e.
`helm--source-name' which is available in only some places.

Acceptable values of BUFFER-SPEC:

- global (a symbol)
  Create a new global candidates buffer,
  named \" *helm candidates:SOURCE*\".
  This is used by `helm-init-candidates-in-buffer' and it is
  the most common usage of BUFFER-SPEC.
  The buffer will be killed and recreated at each new
  helm-session.

- local (a symbol)
  Create a new local candidates buffer,
  named \" *helm candidates:SOURCE*HELM-CURRENT-BUFFER\".
  You may want to use this when you want to have a different
  buffer each time source is used from a different
  `helm-current-buffer'.
  The buffer is erased and refilled at each new session but not
  killed. You probably don't want to use this value for
  BUFFER-SPEC.

- nil (omit)
  Only return the candidates buffer of current source if found.

- A buffer
  Register a buffer as a candidates buffer.
  The buffer needs to exists, it is not created.
  This allow you to use the buffer as a cache, it is faster
  because the buffer is already drawn, but be careful when using
  this as you may mangle your buffer depending on what you write
  in your init(s) function, IOW don't modify the contents of the
  buffer in init(s) function but in a transformer.
  The buffer is not erased nor deleted.
  Generally it is safer to use a copy of buffer inserted
  in a global or local buffer.

If for some reasons a global buffer and a local buffer exist and
are belonging to the same source, the local buffer takes
precedence on the global one and is used instead.

When forcing update only the global and local buffers are killed
before running again the init function."
  (let ((global-bname (format " *helm candidates:%s*"
                              helm--source-name))
        (local-bname (format " *helm candidates:%s*%s"
                             helm--source-name
                             (buffer-name helm-current-buffer))))
    (when buffer-spec
      ;; Register buffer in `helm--candidate-buffer-alist'.
      ;; This is used only to retrieve buffer associated to current source
      ;; when using named buffer as value of BUFFER-SPEC.
      (setq helm--candidate-buffer-alist
            (cons (cons helm--source-name buffer-spec)
                  (delete (assoc helm--source-name
                                 helm--candidate-buffer-alist)
                          helm--candidate-buffer-alist)))
      ;; When using global or local as value of BUFFER-SPEC
      ;; create the buffer global-bname or local-bname, otherwise
      ;; reuse the buffer named BUFFER-SPEC.
      (unless (bufferp buffer-spec)
        ;; Global buffers are killed and recreated.
        (and (eq buffer-spec 'global)
             (buffer-live-p (get-buffer global-bname))
             (kill-buffer global-bname))
        ;; Create global or local buffer.
        ;; Local buffer, once created are reused and a new one
        ;; is created when `helm-current-buffer' change across sessions.
        (with-current-buffer (get-buffer-create
                              (helm-acase buffer-spec
                                (global global-bname)
                                (local  local-bname)
                                (t (and (stringp buffer-spec)
                                        buffer-spec))))
          ;; We need a buffer not read-only to perhaps insert later
          ;; text coming from read-only buffers (Bug#1176).
          (set (make-local-variable 'buffer-read-only) nil)
          ;; Undo is automatically disabled in buffer names starting
          ;; with a space, so no need to disable it.
          (erase-buffer)
          (font-lock-mode -1))))
    ;; Finally return the candidates buffer.
    (helm-acond ((get-buffer local-bname))
                ((get-buffer global-bname))
                ((assoc-default helm--source-name helm--candidate-buffer-alist)
                 (and (or (stringp it) (bufferp it))
                      (buffer-live-p (get-buffer it))
                      it)))))

(defvar helm-candidate-buffer-longest-len 0
  "May store the longest length of candidates in a in-buffer source.
It is a local variable set from `helm-init-candidates-in-buffer' in
`helm-candidate-buffer'.
Allow getting the longest length of initial candidates in transformers
without looping again through the whole list.
It is useful to align extra informations after candidates in `helm-buffer'.")

(defsubst helm-in-buffer-get-longest-candidate ()
  "Return the longest candidate recorded in `helm-candidate-buffer'."
  (helm-aif (helm-candidate-buffer)
      (buffer-local-value
       'helm-candidate-buffer-longest-len
       (get-buffer it))
    0))

(defun helm-make-separator (cand &optional longest)
  "Create a separator to align candidates.
Longest candidate should have been calculated at initialization
of `helm-source-in-buffer' by `helm-init-candidates-in-buffer' , otherwise
LONGEST can be used to specify longest candidate."
  (let ((lgst (or longest (helm-in-buffer-get-longest-candidate)))
        (len  (length cand)))
    (make-string (1+ (if (>= lgst len)
                         (- lgst len)
                       0))
                 ? )))

(defun helm-init-candidates-in-buffer (buffer-spec data &optional force-longest)
  "Register BUFFER-SPEC with DATA for a helm candidates-in-buffer session.

Arg BUFFER-SPEC can be a `buffer-name' (stringp), a buffer-spec
object (bufferp), or a symbol, either \\='local or \\='global which is
passed to `helm-candidate-buffer'.
The most common usage of BUFFER-SPEC is \\='global.

Arg DATA can be either a list or a plain string.
Returns the resulting buffer.

Use this in your init function to register a buffer for a
`helm-source-in-buffer' session and feed it with DATA.  You
probably don't want to bother with this and use the :data slot
when initializing a source with `helm-source-in-buffer' class.

When inserting DATA in `helm-candidate-buffer', if DATA is a list the longest
candidate will be recorded in `helm-candidate-buffer-longest-len' local
variable. If DATA is a string, it is inserted directly in
`helm-candidate-buffer' and `helm-candidate-buffer-longest-len' is not computed
unless FORCE-LONGEST is non nil."
  (declare (indent 1))
  (let ((caching (and (or (stringp buffer-spec)
                          (bufferp buffer-spec))
                      (buffer-live-p (get-buffer buffer-spec))))
        (buf (helm-candidate-buffer buffer-spec)))
    (unless caching
      (with-current-buffer buf
        (erase-buffer)
        (cond ((listp data)
               (insert (mapconcat (lambda (i)
                                    (let ((cand (cond ((symbolp i) (symbol-name i))
                                                      ((numberp i) (number-to-string i))
                                                      ((consp i) (propertize
                                                                  (car i)
                                                                  'helm-realvalue (cdr i)))
                                                      (t i))))
                                      (setq-local helm-candidate-buffer-longest-len
                                                  (max helm-candidate-buffer-longest-len
                                                       (length cand)))
                                      cand))
                                  data "\n")))
              ((stringp data)
               (insert data)
               (when force-longest
                 (setq-local helm-candidate-buffer-longest-len
                             (helm--get-longest-len-in-buffer))))))
      buf)))

(defun helm--get-longest-len-in-buffer ()
  "Return length of the longest line in buffer." 
  (save-excursion
    (goto-char (point-min))
    (let ((max 0)
          len)
      (while (not (eobp))
        (setq len (- (pos-eol) (pos-bol)))
        (when (> len max)
          (setq max len))
        (forward-line 1))
      max)))


;;; Resplit helm window
;;
;;
(defun helm-toggle-resplit-window ()
  "Toggle resplit helm window, vertically or horizontally."
  (interactive)
  (with-helm-alive-p
    (if (and (= (length (window-list nil 1)) 2)
             (not (window-dedicated-p
                   (get-buffer-window helm-current-buffer))))
        (progn
          (when helm-prevent-escaping-from-minibuffer
            (helm-prevent-switching-other-window :enabled nil))
          (unwind-protect
              (with-helm-window
                (cond ((or helm-full-frame (one-window-p t))
                       (user-error "Attempt to resplit a single window"))
                      ((helm-action-window)
                       (user-error "Can't resplit while selecting actions"))
                      (t
                       (let ((before-height (window-height)))
                         (delete-window)
                         (set-window-buffer
                          (select-window
                           (if (= (window-height) before-height) ; initial split was horizontal.
                               ;; Split window vertically with `helm-buffer' placed
                               ;; on the good side according to actual value of
                               ;; `helm-split-window-default-side'.
                               (prog1
                                   (cond ((or (eq helm-split-window-default-side 'above)
                                              (eq helm-split-window-default-side 'left))
                                          (split-window
                                           (selected-window) nil 'above))
                                         (t (split-window-vertically)))
                                 (setq helm-split-window-state 'vertical))
                             ;; Split window vertically, same comment as above.
                             (setq helm-split-window-state 'horizontal)
                             (cond ((or (eq helm-split-window-default-side 'left)
                                        (eq helm-split-window-default-side 'above))
                                    (split-window (selected-window) nil 'left))
                                   (t (split-window-horizontally)))))
                          helm-buffer))))
                (setq helm--window-side-state (helm--get-window-side-state)))
            (when helm-prevent-escaping-from-minibuffer
              (helm-prevent-switching-other-window :enabled t))))
      (error "current window configuration not suitable for splitting"))))
(put 'helm-toggle-resplit-window 'helm-only t)

;; Utility: Resize helm window.
(defun helm-enlarge-window-1 (n)
  "Enlarge or narrow helm window.
If N is positive enlarge, if negative narrow."
  (unless helm-full-frame
    (let ((horizontal-p (eq helm-split-window-state 'horizontal)))
      (with-helm-window
        (enlarge-window n horizontal-p)))))

(defun helm-narrow-window ()
  "Narrow helm window."
  (interactive)
  (with-helm-alive-p
    (helm-enlarge-window-1 -1)))
(put 'helm-narrow-window 'helm-only t)

(defun helm-enlarge-window ()
  "Enlarge helm window."
  (interactive)
  (with-helm-alive-p
    (helm-enlarge-window-1 1)))
(put 'helm-enlarge-window 'helm-only t)

(defun helm-toggle-full-frame (&optional arg)
  "Toggle `helm-buffer' full-frame view."
  (interactive "p")
  (cl-assert (null (helm-action-window))
             nil "Unable to toggle full frame from action window")
  (when arg ; Called interactively
    (cl-assert (null helm--buffer-in-new-frame-p)
               nil "Can't toggle full frame when using helm own frame"))
  (if (or helm-onewindow-p
          (buffer-local-value 'helm-full-frame (get-buffer helm-buffer)))
      (with-helm-window
        (setq-local helm-full-frame nil)
        (setq helm-onewindow-p nil)
        (let ((split-window-preferred-function
               helm-split-window-preferred-function))
          (switch-to-buffer helm-current-buffer)
          (helm-display-buffer helm-buffer)
          (select-window (minibuffer-window))))
    (with-helm-window
      (delete-other-windows)
      (setq-local helm-full-frame t)
      (setq helm-onewindow-p t))))
(put 'helm-toggle-full-frame 'helm-only t)

(defun helm-swap-windows ()
  "Swap window holding `helm-buffer' with other window."
  (interactive)
  (with-helm-alive-p
    (if (= (length (window-list nil 1)) 2)
        (cond ((and helm-full-frame (one-window-p t))
               (user-error "Can't swap windows in a single window"))
              ((helm-action-window)
               (user-error "Can't resplit while selecting actions"))
              (t
               (let* ((w1          (helm-window))
                      (split-state (eq helm-split-window-state 'horizontal))
                      (w1size      (window-total-size w1 split-state))
                      (b1          (window-buffer w1)) ; helm-buffer
                      (s1          (window-start w1))
                      (cur-frame   (window-frame w1))
                      (w2          (with-selected-window (helm-window)
                                     ;; Don't try to display helm-buffer
                                     ;; in a dedicated window.
                                     (get-window-with-predicate
                                      (lambda (w) (not (window-dedicated-p w)))
                                      1 cur-frame)))
                      (w2size      (window-total-size w2 split-state))
                      (b2          (window-buffer w2)) ; probably helm-current-buffer
                      (s2          (window-start w2))
                      resize)
                 (with-selected-frame (window-frame w1)
                   (helm-replace-buffer-in-window w1 b1 b2)
                   (helm-replace-buffer-in-window w2 b2 b1)
                   (setq resize
                         (cond ( ;; helm-window is smaller than other window.
                                (< w1size w2size)
                                (- (- (max w2size w1size)
                                      (min w2size w1size))))
                               ( ;; helm-window is larger than other window.
                                (> w1size w2size)
                                (- (max w2size w1size)
                                   (min w2size w1size)))
                               ( ;; windows have probably same size.
                                t nil)))
                   ;; Maybe resize the window holding helm-buffer.
                   (and resize (window-resize w2 resize split-state))
                   (set-window-start w1 s2 t)
                   (set-window-start w2 s1 t))
                 (setq helm--window-side-state (helm--get-window-side-state)))))
      (error "current window configuration not suitable for splitting"))))
(put 'helm-swap-windows 'helm-only t)

(defun helm--get-window-side-state ()
  "Return the position of `helm-window' from `helm-current-buffer'.
Possible values are \\='left \\='right \\='below or \\='above."
  (let ((side-list '(left right below above)))
    (cl-loop for side in side-list
             thereis (and (equal (helm-window)
                                 (window-in-direction
                                  side (get-buffer-window helm-current-buffer t)
                                  t))
                          side))))

(defun helm-replace-buffer-in-window (window buffer1 buffer2)
  "Replace BUFFER1 by BUFFER2 in WINDOW registering BUFFER1."
  (when (get-buffer-window buffer1)
    (unrecord-window-buffer window buffer1)
    (set-window-buffer window buffer2)))

;; Utility: select another action by key
(defun helm-select-nth-action (n)
  "Select the N nth action for the currently selected candidate."
  (let ((src (helm-get-current-source)))
    (setq helm-saved-selection (helm-get-selection nil nil src))
    (unless helm-saved-selection
      (error "Nothing is selected"))
    (setq helm-saved-action
          (helm-get-nth-action
           n
           (if (get-buffer-window helm-action-buffer 'visible)
               (assoc-default 'candidates src)
             (helm-get-actions-from-current-source src))))
    (helm-maybe-exit-minibuffer)))

(defun helm-get-nth-action (n action)
  (cond ((and (zerop n) (functionp action))
         action)
        ((listp action)
         (or (cdr (elt action n))
             (error "No such action")))
        ((and (functionp action) (< 0 n))
         (error "Sole action"))
        (t
         (error "Error in `helm-select-nth-action'"))))

(defun helm-execute-selection-action-at-nth (linum)
  "Execute default action on candidate at LINUM lines from selection."
  (let ((prefarg current-prefix-arg))
    (if (>= linum 0)
        (helm-next-line linum)
      (helm-previous-line (lognot (1- linum))))
    (setq current-prefix-arg prefarg)
    (helm-exit-minibuffer)))

;;; Persistent Action
;;
(defun helm-initialize-persistent-action ()
  (set (make-local-variable 'helm-persistent-action-display-window) nil))

(cl-defun helm-execute-persistent-action (&optional attr split)
  "Perform the associated action ATTR without quitting helm.

Arg ATTR default will be `persistent-action' or
`persistent-action-if' if unspecified depending on what's found
in source, but it can be anything else.
In this case you have to add this new attribute to your source.
See `persistent-action' and `persistent-action-if' slot
documentation in `helm-source'.

When `helm-full-frame' is non-nil, and `helm-buffer' is displayed
in only one window, the helm window is split to display
`helm-select-persistent-action-window' in other window to
maintain visibility.  The argument SPLIT can be used to force
splitting inconditionally, it is unused actually."
  (interactive)
  (with-helm-alive-p
    (let ((source (helm-get-current-source)))
      (unless attr
        (setq attr (or (car (assq 'persistent-action source))
                       (car (assq 'persistent-action-if source)))))
      (helm-log "helm-execute-persistent-action" "executing persistent-action")
      (let* ((selection (and source (helm-get-selection nil nil source)))
             (attr-val (if (eq attr 'persistent-action-if)
                           (funcall (assoc-default attr source) selection)
                         (assoc-default attr source)))
             ;; If attr value is a cons, use its car as persistent function.
             (fn       (if (and (consp attr-val)
                                ;; maybe a lambda.
                                (not (functionp attr-val)))
                           (car attr-val) attr-val))
             ;; And its cdr to decide if helm window should be splitted.
             (no-split (and (consp attr-val)
                            (not (functionp attr-val))
                            (cdr attr-val)))
             ;; Is next-window (from helm-window) a suitable window for PA?
             (no-suitable-win
              (helm-aand (not helm--buffer-in-new-frame-p)
                         (get-buffer-window helm-current-buffer)
                         (or (window-dedicated-p it)
                             (window-parameter it 'window-side))))
             (cursor-in-echo-area t)
             mode-line-in-non-selected-windows)
        (progn
          (when (and helm-onewindow-p (null no-split)
                     (null helm--buffer-in-new-frame-p))
            (helm-toggle-full-frame))
          (when (eq fn 'ignore)
            (cl-return-from helm-execute-persistent-action nil))
          (when source
            (with-helm-window
              (save-selected-window
                ;; FIXME: Simplify SPLIT behavior, it is a mess actually. 
                (if no-split
                    (helm-select-persistent-action-window :split 'never)
                  (helm-select-persistent-action-window
                   :split (or split helm-onewindow-p no-suitable-win)))
                (helm-log "helm-execute-persistent-action"
                          "current-buffer = %S" (current-buffer))
                (let ((helm-in-persistent-action t)
                      (display-buffer-alist '((".*" (display-buffer-same-window))))
                      display-buffer-function pop-up-windows pop-up-frames
                      special-display-regexps special-display-buffer-names)
                  (helm-execute-selection-action-1
                   selection (or fn (helm-get-actions-from-current-source source)) t)
                  (unless (helm-action-window)
                    (helm-log-run-hook "helm-execute-persistent-action"
                                       'helm-after-persistent-action-hook)))
                ;; A typical case is when a persistent action delete
                ;; the buffer already displayed in
                ;; `helm-persistent-action-display-window' and `helm-full-frame'
                ;; is enabled, we end up with the `helm-buffer'
                ;; displayed in two windows.
                (when (and helm-onewindow-p
                           (> (length (window-list)) 1)
                           (equal (buffer-name
                                   (window-buffer
                                    helm-persistent-action-display-window))
                                  (helm-buffer-get)))
                  (delete-other-windows))))))))))
(put 'helm-execute-persistent-action 'helm-only t)

(cl-defun helm-persistent-action-display-window (&key split)
  "Return the window that will be used for persistent action.
If SPLIT is t window is split in persistent action, if it has the
special symbol `never' don't split, if it is nil don't split either.
The symbol `never' is kept for backward compatibility."
  (with-helm-window
    (setq helm-persistent-action-display-window
          (cond ((and (window-live-p helm-persistent-action-display-window)
                      (not (member helm-persistent-action-display-window
                                   (get-buffer-window-list helm-buffer))))
                 helm-persistent-action-display-window)
                ((and helm--buffer-in-new-frame-p helm-initial-frame)
                 (with-selected-frame helm-initial-frame
                   (let ((win (selected-window)))
                     (if (or (window-dedicated-p win)
                             (window-parameter win 'window-side))
                         (next-window win 1)
                       win))))
                ((and split (not (eq split 'never)))
                 (split-window))
                ((get-buffer-window helm-current-buffer))
                (t (previous-window (selected-window) 1))))))

(cl-defun helm-select-persistent-action-window (&key split)
  "Select the window that will be used for persistent action.
See `helm-persistent-action-display-window' for how to use SPLIT."
  (select-window (get-buffer-window (helm-buffer-get)))
  (prog1
      (select-window
       (setq minibuffer-scroll-window
             (helm-persistent-action-display-window :split split)))
    (helm-log "helm-select-persistent-action-window"
              "Selected window is %S" minibuffer-scroll-window)))

;;; Scrolling - recentering
;;
;;
(defun helm-other-window-base (command &optional arg)
  (let ((minibuffer-scroll-window
         (helm-persistent-action-display-window)))
    (funcall command (or arg helm-scroll-amount))))

(defun helm-scroll-other-window (&optional arg)
  "Scroll other window upward ARG many lines.
When arg is not provided scroll `helm-scroll-amount' lines.
See `scroll-other-window'."
  (interactive "P")
  (with-helm-alive-p (helm-other-window-base 'scroll-other-window arg)))
(put 'helm-scroll-other-window 'helm-only t)

(defun helm-scroll-other-window-down (&optional arg)
  "Scroll other window downward ARG many lines.
When arg is not provided scroll `helm-scroll-amount' lines.
See `scroll-other-window-down'."
  (interactive "P")
  (with-helm-alive-p (helm-other-window-base 'scroll-other-window-down arg)))
(put 'helm-scroll-other-window-down 'helm-only t)

(defun helm-recenter-top-bottom-other-window (&optional arg)
  "Run `recenter-top-bottom' in other window.
Meaning of prefix ARG is the same as in `recenter-top-bottom'."
  (interactive "P")
  (with-helm-alive-p
    (with-helm-window
      (with-selected-window (helm-persistent-action-display-window)
        (recenter-top-bottom arg)))))
(put 'helm-recenter-top-bottom-other-window 'helm-only t)

(defun helm-reposition-window-other-window (&optional arg)
  "Run `reposition-window' in other window.
Meaning of prefix ARG is the same as in `reposition-window'."
  (interactive "P")
  (with-helm-alive-p
    (with-helm-window
      (with-selected-window (helm-persistent-action-display-window)
        (reposition-window arg)))))
(put 'helm-reposition-window-other-window 'helm-only t)


;; Utility: Visible Mark

(defun helm-clear-visible-mark ()
  (with-current-buffer (helm-buffer-get)
    (mapc 'delete-overlay helm-visible-mark-overlays)
    (set (make-local-variable 'helm-visible-mark-overlays) nil)))

(defun helm-this-visible-mark ()
  (cl-loop for o in (overlays-at (point))
           when (overlay-get o 'visible-mark)
           return o))

(defun helm-delete-visible-mark (overlay)
  (let ((src (helm-get-current-source)))
    (setq helm-marked-candidates
          (remove
           (cons src (helm-get-selection nil nil src))
           helm-marked-candidates))
    (delete-overlay overlay)
    (setq helm-visible-mark-overlays
          (delq overlay helm-visible-mark-overlays))))

(defun helm-make-visible-mark (&optional src selection)
  (let* ((source (or src  (helm-get-current-source)))
         (sel    (or selection (helm-get-selection
                                nil (helm-get-attr 'marked-with-props source)
                                source)))
         (selection-end (if (helm-pos-multiline-p)
                            ;; Stays within source
                            (or (helm-get-next-candidate-separator-pos)
                                (helm-get-next-header-pos)
                                (point-max))
                          ;; Not multiline
                          (1+ (pos-eol))))
         (o (make-overlay (pos-bol) selection-end)))
    (overlay-put o 'priority 0)
    (overlay-put o 'face   'helm-visible-mark)
    (overlay-put o 'source source)
    (overlay-put o 'string (buffer-substring (overlay-start o) (overlay-end o)))
    (overlay-put o 'real sel)
    (overlay-put o 'before-string (propertize " " 'display
                                              `((margin left-margin)
                                                ,(propertize
                                                  helm-visible-mark-prefix
                                                  'face 'helm-mark-prefix))))
    (overlay-put o 'visible-mark t)
    (overlay-put o 'evaporate t)
    (cl-pushnew o helm-visible-mark-overlays)
    (push (cons source sel) helm-marked-candidates)))

(defun helm-toggle-visible-mark (arg)
  "Toggle Helm visible mark at point ARG times.
If ARG is negative toggle backward."
  (interactive "p")
  (with-helm-alive-p
    (with-helm-window
      (let ((nomark (assq 'nomark (helm-get-current-source)))
            (next-fns (if (< arg 0)
                          '(helm-beginning-of-source-p . helm-previous-line)
                        '(helm-end-of-source-p . helm-next-line))))
        (if nomark
            (message "Marking not allowed in this source")
          (cl-loop with n = (if (< arg 0) (* arg -1) arg)
                   repeat n do
                   (progn
                     (helm-aif (helm-this-visible-mark)
                         (helm-delete-visible-mark it)
                       (helm-make-visible-mark))
                     (if (funcall (car next-fns))
                         (progn
                           (helm-display-mode-line (helm-get-current-source))
                           (cl-return nil))
                       (funcall (cdr next-fns)))))
          (set-window-margins (selected-window)
                              (if helm-visible-mark-overlays
                                  (+ (string-width helm-visible-mark-prefix)
                                     helm-left-margin-width)
                                helm-left-margin-width)))))))
(put 'helm-toggle-visible-mark 'helm-only t)

(defun helm-toggle-visible-mark-forward ()
  (interactive)
  (helm-toggle-visible-mark 1))

(defun helm-toggle-visible-mark-backward ()
  (interactive)
  (helm-toggle-visible-mark -1))

(defun helm-file-completion-source-p (&optional source)
  "Return non-nil if current source is a file completion source."
  (or helm--completing-file-name ; helm-read-file-name
      (let ((cur-source (cdr (assq 'name
                                   (or source (helm-get-current-source))))))
        (cl-loop for i in helm--file-completion-sources
                 thereis (string= cur-source i)))))

(defun helm-mark-all (&optional all)
  "Mark all visible unmarked candidates in current source.

With a prefix arg mark all visible unmarked candidates in all
sources."
  (interactive "P")
  (with-helm-alive-p
    (with-helm-window ; Using `with-helm-buffer' for some unknow
                      ; reasons infloop.
      (set-window-margins (selected-window)
                          (+ (string-width helm-visible-mark-prefix)
                             helm-left-margin-width))
      (if (null all)
          (helm-mark-all-1 t)
        (let ((pos (point)))
          (goto-char (point-min))
          (helm-awhile (helm-get-next-header-pos)
            (goto-char it)
            (forward-line 1)
            (helm-mark-current-line)
            (helm-mark-all-1))
          ;; `save-excursion' seems confused if used in addition of
          ;; the one used in `helm-mark-all-1', so save POS and back
          ;; to it when loop is finished.
          (goto-char pos)
          (helm-mark-current-line)
          (helm-display-mode-line (helm-get-current-source) t))))))
(put 'helm-mark-all 'helm-only t)

(defun helm-mark-all-1 (&optional ensure-beg-of-source)
  "Mark all visible unmarked candidates in current source.
Need to be wrapped in `with-helm-window'.
Arg ENSURE-BEG-OF-SOURCE ensure we are at beginning of source
when starting to mark candidates, if handled elsewhere before
starting it is not needed."
  (let* ((src        (helm-get-current-source))
         (follow     (if (helm-follow-mode-p src) 1 -1))
         (nomark     (assq 'nomark src))
         (src-name   (assoc-default 'name src))
         (filecomp-p (or (helm-file-completion-source-p src)
                         (string= src-name "Files from Current Directory"))))
    ;; Note that `cl-letf' prevents edebug working properly.
    (cl-letf (((symbol-function 'message) #'ignore))
      (helm-follow-mode -1)
      (unwind-protect
          (if nomark
              (user-error "Marking not allowed in this source")
            (save-excursion
              (when ensure-beg-of-source
                (goto-char (helm-get-previous-header-pos))
                (forward-line 1))
              (let* ((next-head (helm-get-next-header-pos))
                     (end       (and next-head
                                     (save-excursion
                                       (goto-char next-head)
                                       (forward-line -1)
                                       (point))))
                     (maxpoint  (or end (point-max))))
                (while (< (point) maxpoint)
                  (helm-mark-current-line)
                  (let* ((prefix (or (get-text-property (pos-bol) 'helm-new-file)
                                     (get-text-property (pos-bol) 'unknown)))
                         (cand   (helm-get-selection
                                  nil (helm-get-attr 'marked-with-props src)
                                  src))
                         (bn     (and filecomp-p (helm-basename cand))))
                    ;; Don't mark possibles directories ending with . or ..
                    ;; autosave files/links and non--existent files.
                    (unless
                        (or (helm-this-visible-mark)
                            ;; Non existing files in HFF and
                            ;; RFN. Display may be an image. See
                            ;; https://github.com/yyoncho/helm-treemacs-icons/issues/5
                            ;; and also Bug#2296.
                            prefix
                            (and filecomp-p
                                 (or
                                  ;; autosave files
                                  (string-match-p "\\`[.]?#.*#?\\'" bn)
                                  ;; dot files
                                  (member bn '("." "..")))))
                      (helm-make-visible-mark src cand)))
                  (when (helm-pos-multiline-p)
                    (goto-char
                     (or (helm-get-next-candidate-separator-pos)
                         (point-max))))
                  (forward-line 1))))
            (helm-mark-current-line))
        (helm-follow-mode follow)))))

(defun helm-unmark-all ()
  "Unmark all candidates in all sources of current helm session."
  (interactive)
  (with-helm-alive-p
    (with-helm-window
      (save-excursion
        (helm-clear-visible-mark))
      (setq helm-marked-candidates nil)
      (helm-mark-current-line)
      (helm-display-mode-line (helm-get-current-source))
      (set-window-margins (selected-window) helm-left-margin-width))))
(put 'helm-unmark-all 'helm-only t)

(defun helm-toggle-all-marks (&optional all)
  "Toggle all marks.

Mark all visible candidates of current source or unmark all
candidates visible or invisible in all sources of current Helm
session.

With a prefix argument mark all candidates in all sources."
  (interactive "P")
  (with-helm-alive-p
    (let ((marked (helm-marked-candidates)))
      (if (and (>= (length marked) 1)
               (with-helm-window helm-visible-mark-overlays))
          (helm-unmark-all)
        (helm-mark-all all)))))
(put 'helm-toggle-all-marks 'helm-only t)

(defun helm--compute-marked (real source &optional wildcard)
  (let* ((coerced (helm-coerce-selection real source))
         (wilds   (and wildcard
                       (condition-case nil
                           (helm-file-expand-wildcards
                            coerced t)
                         (error nil)))))
    ;; Avoid returning a not expanded wildcard fname.
    ;; e.g assuming "/tmp" doesn't contain "*.el"
    ;; return nil when coerced is "/tmp/*.el".
    (unless (or wilds (null wildcard)
                (string-match-p helm--url-regexp coerced)
                (file-exists-p coerced)
                (and (stringp coerced)
                     (null (string-match-p "[[*?]" coerced))))
      (setq coerced nil))
    (or wilds (and coerced (list coerced)))))

(cl-defun helm-marked-candidates (&key with-wildcard all-sources)
  "Return marked candidates of current source, if any.

Otherwise return one element list consisting of the current
selection.  When key WITH-WILDCARD is specified, expand it.  When
ALL-SOURCES key value is non-nil returns marked candidates of all
sources."
  (with-current-buffer helm-buffer
    (let* ((current-src (helm-get-current-source))
           (candidates
            (cl-loop for (source . real) in (reverse helm-marked-candidates)
                     for use-wc = (and with-wildcard
                                       (string-match-p "\\*" real)
                                       (null (file-exists-p real)))
                     when (or all-sources
                              (equal (assq 'name source)
                                     (assq 'name current-src)))
                     append (helm--compute-marked real source use-wc)))
           sel)
      (unless candidates
        (setq sel (helm-get-selection
                   nil (helm-get-attr 'marked-with-props
                                  current-src)
                   current-src))
        (setq candidates
              (helm--compute-marked
               sel current-src
               (and with-wildcard (null (file-exists-p sel))))))
      (helm-log "helm-marked-candidates"
                "Marked candidates = %S" candidates)
      candidates)))

(defun helm--remove-marked-and-update-mode-line (elm)
  (with-helm-buffer
    (setq helm-marked-candidates
          (delete (rassoc elm helm-marked-candidates)
                  helm-marked-candidates))
    (helm-display-mode-line (helm-get-current-source))))

(defun helm-current-source-name= (name)
  (save-excursion
    (goto-char (helm-get-previous-header-pos))
    (equal name (helm-current-line-contents))))

(defun helm-revive-visible-mark ()
  "Restore marked candidates when helm updates display."
  (with-current-buffer helm-buffer
    (save-excursion
      (dolist (o helm-visible-mark-overlays)
        (let* ((source (overlay-get o 'source))
               (ov-src-name (assoc-default 'name source))
               (ov-str (overlay-get o 'string))
               (ov-real (overlay-get o 'real))
               (ov-ml-str (helm-aif (helm-get-attr 'multiline source)
                              (if (numberp it)
                                  ;; Assume display have been computed
                                  ;; against real e.g. kill-ring.
                                  (helm--multiline-get-truncated-candidate
                                   ov-real it)
                                ov-str)
                            ov-str))
               beg end)
          ;; Move point to end of source header line.
          (goto-char (point-min))
          (search-forward ov-src-name nil t)
          (while (and (search-forward ov-ml-str nil t)
                      (cl-loop for ov in (overlays-at (pos-bol 0))
                               never (overlay-get ov 'visible-mark))
                      (helm-current-source-name= ov-src-name))
            (setq beg (match-beginning 0)
                  end (if (string= ov-ml-str ov-str)
                          (match-end 0) (1+ (match-end 0))))
            ;; Calculate real value of candidate.
            ;; It can be nil if candidate have only a display value.
            (let ((real (get-text-property (pos-bol 0) 'helm-realvalue)))
              (if real
                  ;; Check if real value of current candidate is the same
                  ;; than the one stored in overlay.
                  ;; This is needed when some cands have same display names.
                  ;; Using equal allow testing any type of value for real cand.
                  ;; bug#706.
                  (and (equal ov-real real)
                       (move-overlay o beg end))
                (and (equal ov-str (buffer-substring beg end))
                     (move-overlay o beg end))))))))))
(add-hook 'helm-after-update-hook 'helm-revive-visible-mark)

(defun helm-next-point-in-list (curpos points &optional prev)
  (cond
   ;; rule out special cases.
   ((null points) curpos)
   ((and prev (<= curpos (car points)))
    (nth (1- (length points)) points))
   ((< (car (last points)) curpos)
    (if prev (car (last points)) (nth 0 points)))
   ((and (not prev) (>= curpos (car (last points))))
    (nth 0 points))
   (t
    (nth (if prev
             (cl-loop for pt in points
                      for i from 0
                      if (<= curpos pt) return (1- i))
           (cl-loop for pt in points
                    for i from 0
                    if (< curpos pt) return i))
         points))))

(defun helm-next-visible-mark (&optional prev)
  "Move next Helm visible mark.
If PREV is non-nil move to precedent."
  (interactive)
  (with-helm-alive-p
    (with-helm-window
      (ignore-errors
        (goto-char (helm-next-point-in-list
                    (point)
                    (sort (mapcar 'overlay-start helm-visible-mark-overlays) '<)
                    prev)))
      (helm-mark-current-line))))
(put 'helm-next-visible-mark 'helm-only t)

(defun helm-prev-visible-mark ()
  "Move previous helm visible mark."
  (interactive)
  (with-helm-alive-p
    (helm-next-visible-mark t)))
(put 'helm-prev-visible-mark 'helm-only t)

;;; Utility: Selection Paste
;;
(defun helm-yank-selection (arg)
  "Set minibuffer contents to current display selection.
With a prefix arg set to real value of current selection."
  (interactive "P")
  (with-helm-alive-p
    (let ((str (format "%s" (helm-get-selection nil (not arg)))))
      (kill-new str)
      (helm-set-pattern str))))
(put 'helm-yank-selection 'helm-only t)

(defun helm-kill-selection-and-quit (arg)
  "Store display value of current selection to kill ring.
With a prefix arg use real value of current selection.
Display value is shown in `helm-buffer' and real value is used to
perform actions."
  (interactive "P")
  (with-helm-alive-p
    (helm-run-after-exit
     (lambda (sel)
       (kill-new sel)
       ;; Return nil to force `helm-mode--keyboard-quit'
       ;; in `helm-comp-read' otherwise the value "Saved to kill-ring: foo"
       ;; is used as exit value for `helm-comp-read'.
       (prog1 nil (message "Saved to kill-ring: %s" sel) (sit-for 1)))
     (format "%s" (helm-get-selection nil (not arg))))))
(put 'helm-kill-selection-and-quit 'helm-only t)

(defun helm-insert-or-copy (&optional arg)
  "Insert selection or marked candidates in current buffer.

With a prefix arg copy marked candidates to kill-ring.
The real value of each candidate is used."
  (interactive "P")
  (with-helm-alive-p
    (helm-run-after-exit
     (lambda (cands)
       (with-helm-current-buffer
         (let ((sels (mapconcat (lambda (c)
                                  (format "%s" c))
                                cands "\n")))
         (if arg (kill-new sels) (insert sels)))))
     (helm-marked-candidates))))
(put 'helm-insert-or-copy 'helm-only t)


;;; Follow-mode: Automatic execution of persistent-action
;;
;;
(defvar helm-follow-input-idle-delay nil
  "`helm-follow-mode' will execute its persistent action after this delay.
Note that if the `follow-delay' attr is present in source, it
will take precedence over this.")

(defun helm-follow-mode (&optional arg)
  "Execute persistent action every time the cursor is moved.

This mode is source local, i.e. It applies on current source only.
\\<helm-map>
This mode can be enabled or disabled interactively at anytime during
a helm session with \\[helm-follow-mode].

When enabling interactively `helm-follow-mode' in a source, you
can keep it enabled for next Emacs sessions by setting
`helm-follow-mode-persistent' to a non-nil value.

When `helm-follow-mode' is called with a prefix arg and
`helm-follow-mode-persistent' is non-nil `helm-follow-mode' will
be persistent only for this Emacs session, but not for the next
Emacs sessions, i.e. the current source will not be saved to
`helm-source-names-using-follow'.

A prefix arg with `helm-follow-mode' already enabled will have no
effect.

Note that you can use instead of this mode the commands
`helm-follow-action-forward' and `helm-follow-action-backward' at
anytime in all Helm sessions.

They are bound by default to \\[helm-follow-action-forward] and
\\[helm-follow-action-backward]."
  (interactive (list (helm-aif (and current-prefix-arg
                                    (prefix-numeric-value current-prefix-arg))
                         (unless (helm-follow-mode-p) it))))
  (with-helm-alive-p
    (with-current-buffer helm-buffer
      (let* ((src      (helm-get-current-source))
             (name     (assoc-default 'name src))
             (fol-attr (assq 'follow src))
             (enabled  (or (helm-follow-mode-p src)
                           (and helm-follow-mode-persistent
                                (member (assoc-default 'name src)
                                        helm-source-names-using-follow)))))
        (if src
            (progn
              (if (eq (cdr fol-attr) 'never)
                  (message "helm-follow-mode not allowed in this source")
                ;; Make follow attr persistent for this emacs session.
                (helm-follow-mode-set-source
                 (if (or enabled (and (numberp arg) (< arg 0))) -1 1)
                 src)
                ;; When arg is nil assume the call is interactive.
                ;; However if user call helm-follow-mode with a prefix arg,
                ;; the call will be considered non--interactive and
                ;; src-name will NOT be saved to helm-source-names-using-follow.
                ;; When called from lisp (non--interactive) src-name
                ;; will never be saved.
                (when (and helm-follow-mode-persistent (null arg))
                  (if (null enabled)
                      (unless (member name helm-source-names-using-follow)
                        (push name helm-source-names-using-follow)
                        (customize-save-variable 'helm-source-names-using-follow
                                                 helm-source-names-using-follow))
                    (when (member name helm-source-names-using-follow)
                      (setq helm-source-names-using-follow
                            (delete name helm-source-names-using-follow))
                      (customize-save-variable 'helm-source-names-using-follow
                                               helm-source-names-using-follow))))
                (message "helm-follow-mode is %s"
                         (if (helm-follow-mode-p src)
                             "enabled" "disabled"))
                (helm-display-mode-line src t)))
          (message "Not enough candidates for helm-follow-mode"))))))
(put 'helm-follow-mode 'helm-only t)

(defun helm-follow-execute-persistent-action-maybe (&optional delay)
  "Execute persistent action in mode `helm-follow-mode'.

This happen after: DELAY or the \\='follow-attr value of current
source or `helm-follow-input-idle-delay' or
`helm-input-idle-delay' secs."
  (let* ((src (helm-get-current-source))
         (at (or delay
                 (assoc-default 'follow-delay src)
                 helm-follow-input-idle-delay
                 (or (and helm-input-idle-delay
                          (max helm-input-idle-delay 0.01))
                     0.01)))
         (suspend (and helm--in-update
                       ;; Specific to helm-find-files.
                       (assoc-default 'suspend-follow-in-update src))))
    (when (and (not suspend)
               (not (get-buffer-window helm-action-buffer 'visible))
               (not (helm-pos-header-line-p))
               (or (helm-follow-mode-p src)
                   (and helm-follow-mode-persistent
                        (member (assoc-default 'name src)
                                helm-source-names-using-follow)))
               (null (eq (assoc-default 'follow src) 'never))
               (helm-get-selection nil nil src))
      (helm-follow-mode-set-source 1 src)
      (run-with-idle-timer at nil (lambda ()
                                    (when helm-alive-p
                                      (helm-execute-persistent-action)))))))

(defun helm-follow-mode-p (&optional source)
  (with-helm-buffer
    (eq (helm-get-attr 'follow (or source (helm-get-current-source))) 1)))

(defun helm-follow-mode-set-source (value &optional source)
  (with-helm-buffer
    (helm-set-attr 'follow value (or source (helm-get-current-source)))))

;;; Auto-resize mode
;;
(defun helm--autoresize-hook (&optional max-height min-height)
  (when (helm-window)
    (with-helm-window
      (fit-window-to-buffer nil
                            (/ (* (frame-height)
                                  (or max-height helm-autoresize-max-height))
                               100)
                            (/ (* (frame-height)
                                  (or min-height helm-autoresize-min-height))
                               100)))))

(define-minor-mode helm-autoresize-mode
  "Auto resize helm window when enabled.
Helm window is re-sized according to `helm-autoresize-max-height'
and `helm-autoresize-min-height'.  Note that when this mode is
enabled, Helm behaves as if `helm-always-two-windows' is enabled.

See `fit-window-to-buffer' for more infos."
  :group 'helm
  :global t
  (if helm-autoresize-mode
      (progn (add-hook 'helm-after-update-hook 'helm--autoresize-hook)
             (add-hook 'helm-window-configuration-hook 'helm--autoresize-hook))
    (remove-hook 'helm-after-update-hook 'helm--autoresize-hook)
    (remove-hook 'helm-window-configuration-hook 'helm--autoresize-hook)))

(defun helm-help ()
  "Generate Helm's help according to `help-message' attribute.

If `helm-buffer' is empty, provide completions on `helm-sources'
to choose its local documentation.
If source doesn't have any `help-message' attribute, a generic
message explaining this is added instead.
The global `helm-help-message' is always added after this local
help."
  (interactive)
  (require 'helm-mode) ; for helm-comp-read.
  (with-helm-alive-p
    (let ((source (or (helm-get-current-source)
                      (helm-comp-read
                       "Help for: "
                       (cl-loop for src in (with-helm-buffer helm-sources)
                                collect `(,(assoc-default 'name src) .
                                          ,src))
                       :allow-nest t
                       :exec-when-only-one t))))
      (save-selected-window
        (helm-help-internal
         helm-help-buffer-name
         (lambda ()
           (helm-aif (assoc-default 'help-message source)
               (insert (substitute-command-keys
                        (helm-interpret-value it)))
             (insert "* No specific help for this source available."))
           (insert "\n\n"
                   (substitute-command-keys
                    (helm-interpret-value helm-help-message)))))))))
(put 'helm-help 'helm-only t)

(defun helm-toggle-truncate-line ()
  "Toggle `truncate-lines' value in `helm-buffer'"
  (interactive)
  (with-helm-alive-p
    (with-helm-buffer
      (setq truncate-lines (not truncate-lines))
      (when (helm-get-previous-header-pos)
        (helm-update (regexp-quote (helm-get-selection nil t))))
      (message "%sisplaying continuation lines"
               (if truncate-lines "Not D" "D")))))
(put 'helm-toggle-truncate-line 'helm-only t)

;;;###autoload
(defun helm-other-buffer (sources buffer)
  "Simplified Helm interface with other `helm-buffer'.
Call `helm' only with SOURCES and BUFFER as args."
  (helm :sources sources :buffer buffer))


(provide 'helm-core)
;;; helm-core.el ends here
