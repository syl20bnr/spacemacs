;;; funcs.el --- Spacemacs Layer functions File
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

;; add emacs binary helper functions
(defun emacsbin-path()
  (interactive)
  (concat exec-directory (if (system-is-mswindows) "bin/") "emacs"))

(defun emacs()
  (interactive)
  (call-process (emacsbin-path) nil 0 nil)
  (message "Started 'emacs' - it will be ready soon ..."))

(defun emacs-debug-init()
  (interactive)
  (call-process (emacsbin-path) nil 0 nil "--debug-init")
  (message "Started 'emacs --debug-init' - it will be ready soon ..."))

(defun emacs-reload()
  (interactive)
  (load-file user-init-file)
  (message ".emacs reloaded successfully"))

(defun emacs-Q() (interactive)
  (call-process (emacsbin-path) nil 0 nil "-Q")
  (message "Started 'emacs -Q' - it will be ready soon ..."))

;; from https://github.com/cofi/dotfiles/blob/master/emacs.d/config/cofi-util.el#L38
(defun add-to-hooks (fun hooks)
  "Add function to hooks"
  (dolist (hook hooks)
    (add-hook hook fun)))
(defun add-all-to-hook (hook &rest funs)
  "Add functions to hook."
  (add-to-hook hook funs))
(defun add-to-hook (hook funs)
  "Add list of functions to hook."
  (dolist (fun funs)
    (add-hook hook fun)))

(defun echo (msg &rest args)
  "Display MSG in echo-area without logging it in *Messages* buffer."
  (interactive)
  (let ((message-log-max nil))
    (apply 'message msg args)))

(defun system-is-mac ()
  (string-equal system-type "darwin"))
(defun system-is-linux ()
  (string-equal system-type "gnu/linux"))
(defun system-is-mswindows ()
  (string-equal system-type "windows-nt"))

(defvar spacemacs/prefix-command-string "group:"
  "Prefix string for prefix commands.")

(defun spacemacs/jump-in-buffer ()
  (interactive)
  (cond
   ((eq major-mode 'org-mode)
    (call-interactively 'helm-org-in-buffer-headings))
   (t
    (call-interactively 'helm-semantic-or-imenu))))

(defun spacemacs/declare-prefix (prefix name)
  "Declare a prefix PREFIX. PREFIX is a string describing
a key sequence. NAME is a symbol name used as the prefix command."
  (let ((command (intern (concat spacemacs/prefix-command-string name))))
    ;; define the prefix command only if it does not already exist
    (unless (lookup-key evil-leader--default-map prefix)
      (define-prefix-command command)
      (evil-leader/set-key prefix command))))

;; Waiting to fix the issue with guide-key before reactivating/updating this
;; function
;; (defun spacemacs/declare-prefix-for-mode (mode prefix name)
;;   "Declare a prefix PREFIX. MODE is the mode in which this prefix command should
;; be added. PREFIX is a string describing a key sequence. NAME is a symbol name
;; used as the prefix command."
;;   (let ((command (intern (concat spacemacs/prefix-command-string name))))
;;     (define-prefix-command command)
;;     (evil-leader/set-key-for-mode mode prefix command)))

(defun spacemacs/activate-major-mode-leader ()
  "Bind major mode key map to `dotspacemacs-major-mode-leader-key'."
  (setq mode-map (cdr (assoc major-mode evil-leader--mode-maps)))
  (when mode-map
    (setq major-mode-map (lookup-key mode-map (kbd "m")))
    (mapc (lambda (s)
            (eval `(define-key
                     ,(intern (format "evil-%S-state-local-map" s))
                     ,(kbd dotspacemacs-major-mode-leader-key)
                     major-mode-map)))
          '(normal motion))
    (mapc (lambda (s)
            (eval `(define-key
                     ,(intern (format "evil-%S-state-local-map" s))
                     ,(kbd dotspacemacs-major-mode-emacs-leader-key)
                     major-mode-map)))
          '(emacs insert normal motion visual))))

(defun spacemacs/split-and-new-line ()
  "Split a quoted string or s-expression and insert a new line with
auto-indent."
  (interactive)
  (sp-split-sexp 1)
  (sp-newline))

(defun spacemacs/push-mark-and-goto-beginning-of-line ()
  "Push a mark at current location and go to the beginnign of the line."
  (interactive)
  (push-mark (point))
  (evil-beginning-of-line))

(defun spacemacs/push-mark-and-goto-end-of-line ()
  "Push a mark at current location and go to the end of the line."
  (interactive)
  (push-mark (point))
  (evil-end-of-line))

;; insert one or several line below without changing current evil state
(defun evil-insert-line-below (count)
  "Insert one of several lines below the current point's line without changing
the current state and point position."
  (interactive "p")
  (save-excursion
    (evil-save-state (evil-open-below count))))

;; insert one or several line above without changing current evil state
(defun evil-insert-line-above (count)
  "Insert one of several lines above the current point's line without changing
the current state and point position."
  (interactive "p")
  (save-excursion
    (evil-save-state (evil-open-above count))))

(defun evil-goto-next-line-and-indent (&optional count)
  (interactive "p")
  (let ((counter (or count 1)))
    (while (> counter 0)
      (join-line 1)
      (sp-newline)
      (setq counter (1- counter)))))

;; from Prelude
;; TODO: dispatch these in the layers
(defvar spacemacs-indent-sensitive-modes
  '(coffee-mode
    python-mode
    slim-mode
    haml-mode
    yaml-mode
    makefile-mode
    makefile-gmake-mode
    makefile-imake-mode
    makefile-bsdmake-mode)
  "Modes for which auto-indenting is suppressed.")

(defcustom spacemacs-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'spacemacs)


(defun spacemacs/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode spacemacs-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (evil-indent (point-min) (point-max))
          (message "Indented buffer.")))
      (whitespace-cleanup))))

;; linum gutter helpers
(defvar *linum-mdown-line* nil
  "Define persistent variable for linum selection")

(defun spacemacs/line-at-click ()
  "Determine the visual line at click"
  (save-excursion
    (let ((click-y (cddr (mouse-position)))
          (debug-on-error t)
          (line-move-visual t))
      (goto-char (window-start))
      (next-line (1- click-y))
      (1+ (line-number-at-pos))
      )))

(defun spacemacs/md-select-linum (event)
  "Set point as *linum-mdown-line*"
  (interactive "e")
  (mouse-select-window event)
  (goto-line (spacemacs/line-at-click))
  (set-mark (point))
  (setq *linum-mdown-line*
        (line-number-at-pos)))

(defun spacemacs/mu-select-linum ()
  "Select code block between point and *linum-mdown-line*"
  (interactive)
  (when *linum-mdown-line*
    (let (mu-line)
      (setq mu-line (spacemacs/line-at-click))
      (goto-line (max *linum-mdown-line* mu-line))
      (set-mark (line-end-position))
      (goto-line (min *linum-mdown-line* mu-line))
      (setq *linum-mdown*
            nil))))

(defun spacemacs/select-current-block ()
  "Select the current block of text between blank lines."
  (interactive)
  (let (p1 p2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "move")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq p1 (point)))
        (setq p1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "move")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq p2 (point)))
        (setq p2 (point))))
    (set-mark p1)))

;; eval lisp helpers
(defun spacemacs/eval-region ()
  (interactive)
  (eval-region (region-beginning) (region-end))
  (evil-normal-state))

;; idea from http://www.reddit.com/r/emacs/comments/312ge1/i_created_this_function_because_i_was_tired_of/
(defun spacemacs/eval-current-form ()
  "Looks for the current def* or set* command then evaluates, unlike `eval-defun', does not go to topmost function"
  (interactive)
  (save-excursion
    (search-backward-regexp "(def\\|(set")
    (forward-list)
    (call-interactively 'eval-last-sexp)))

;; from magnars
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; from https://gist.github.com/3402786
(defun toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc'_ register-alist))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(defun toggle-maximize-centered-buffer ()
  "Maximize buffer and center it on the screen"
  (interactive)
  (if (= 1 (length (window-list)))
      (progn  (bzg-big-fringe-mode 0)
              (jump-to-register '_))
    (progn
      (set-register '_ (list (current-window-configuration)))
      (delete-other-windows)
      (bzg-big-fringe-mode 1))))

(defun toggle-transparency ()
  "Toggle between transparent or opaque display."
  (interactive)
  ;; Define alpha if it's nil
  (if (eq (frame-parameter (selected-frame) 'alpha) nil)
      (set-frame-parameter (selected-frame) 'alpha '(100 100)))
  ;; Do the actual toggle
  (if (/= (cadr (frame-parameter (selected-frame) 'alpha)) 100)
      (set-frame-parameter (selected-frame) 'alpha '(100 100))
    (set-frame-parameter (selected-frame) 'alpha
                         (list dotspacemacs-active-transparency
                               dotspacemacs-inactive-transparency))))

(defun toggle-triple-double-column-mode ()
  " Toggle between triple columns and double columns mode quickly. "
  (interactive)
  (if (= 3 (length (window-list)))
      (progn (delete-window (window-next-sibling))
             (golden-ratio-mode 1))
    (let ((num-windows (length (window-list))))
      (progn
        (golden-ratio-mode 0)
        (dotimes (i (max 0 (- num-windows 3)))
          (delete-window (window-next-sibling)))
        (dotimes (i (- 3 (length (window-list))))
          (progn (split-window-right)
                 (balance-windows)))))))

(defun layout-triple-columns ()
  " Set the layout to triple columns. "
  (interactive)
  (golden-ratio-mode 0)
  (delete-other-windows)
  (dotimes (i 2) (split-window-right))
  (balance-windows))

(defun layout-double-columns ()
  " Set the layout to double columns. "
  (interactive)
  (golden-ratio-mode 1)
  (delete-other-windows)
  (split-window-right))

;; from magnars modified by ffevotte for dedicated windows support
(defun rotate-windows (count)
  "Rotate your windows.
Dedicated windows are left untouched. Giving a negative prefix
argument takes the kindows rotate backwards."
  (interactive "p")
  (let* ((non-dedicated-windows (remove-if 'window-dedicated-p (window-list)))
         (num-windows (length non-dedicated-windows))
         (i 0)
         (step (+ num-windows count)))
    (cond ((not (> num-windows 1))
           (message "You can't rotate a single window!"))
          (t
           (dotimes (counter (- num-windows 1))
             (let* ((next-i (% (+ step i) num-windows))

                    (w1 (elt non-dedicated-windows i))
                    (w2 (elt non-dedicated-windows next-i))

                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))

                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1 b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i next-i)))))))

(defun rotate-windows-backward (count)
  "Rotate your windows backward."
  (interactive "p")
  (rotate-windows (* -1 count)))

(defun spacemacs/useless-buffer-p (buffer)
  "Determines if a buffer is useful."
  (let ((buf-paren-major-mode (get (with-current-buffer buffer
                                     major-mode)
                                   'derived-mode-parent))
        (buf-name (buffer-name buffer)))
    ;; first find if useful buffer exists, if so returns nil and don't check for
    ;; useless buffers. If no useful buffer is found, check for useless buffers.
    (unless (cl-loop for regexp in spacemacs-useful-buffers-regexp do
                     (when (or (eq buf-paren-major-mode 'comint-mode)
                               (string-match regexp buf-name))
                       (return t)))
      (cl-loop for regexp in spacemacs-useless-buffers-regexp do
               (when (string-match regexp buf-name)
                 (return t))))))

(defun spacemacs/next-useful-buffer ()
  "Switch to the next buffer and avoid special buffers."
  (interactive)
  (let ((start-buffer (current-buffer)))
    (next-buffer)
    (while (and (spacemacs/useless-buffer-p (current-buffer))
                (not (eq (current-buffer) start-buffer)))
      (next-buffer))))

(defun spacemacs/previous-useful-buffer ()
  "Switch to the previous buffer and avoid special buffers."
  (interactive)
  (let ((start-buffer (current-buffer)))
    (previous-buffer)
    (while (and (spacemacs/useless-buffer-p (current-buffer))
                (not (eq (current-buffer) start-buffer)))
      (previous-buffer))))

;; from magnars
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

;; from magnars
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; from magnars
(defun find-or-create-file-at-point ()
  "Guesses what parts of the buffer under point is a file name and opens it."
  (interactive)
  (find-file (file-name-at-point)))

;; from magnars
(defun find-or-create-file-at-point-other-window ()
  "Guesses what parts of the buffer under point is a file name and opens it."
  (interactive)
  (find-file-other-window (file-name-at-point)))

;; from magnars
(defun file-name-at-point ()
  (save-excursion
    (let* ((file-name-regexp "[./a-zA-Z0-9\-_~]")
           (start (progn
                    (while (looking-back file-name-regexp)
                      (forward-char -1))
                    (point)))
           (end (progn
                  (while (looking-at file-name-regexp)
                    (forward-char 1))
                  (point))))
      (buffer-substring start end))))

;; from magnars
(defun touch-buffer-file ()
  (interactive)
  (insert " ")
  (backward-delete-char 1)
  (save-buffer))

;; from magnars
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; found at http://emacswiki.org/emacs/KillingBuffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (let (name (buffer-name))
    (when (yes-or-no-p (format "Killing all buffers except \"%s\" ? " buffer-file-name))
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
      (message "Buffers deleted!"))))

;; evenly split windows horizontally
(defun evenly-split-window-right ()
  "Evenly split frame horizontally."
  (interactive)
  (split-window-right)
  (balance-windows))
;; evenly split windows vertically
(defun evenly-split-window-below ()
  "Evenly split frame vertically."
  (interactive)
  (split-window-below)
  (balance-windows))

;; from http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun toggle-current-window-dedication ()
  "Toggle dedication state of a window."
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun show-and-copy-buffer-filename ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

;; adapted from bozhidar
;; http://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun find-user-init-file ()
  "Edit the `user-init-file', in the current window."
  (interactive)
  (find-file-existing user-init-file))

(defun find-dotfile ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (find-file-existing (dotspacemacs/location)))

(defun ediff-dotfile-and-template ()
  "ediff the current `dotfile' with the template"
  (interactive)
  (ediff-files (dotspacemacs/location)
               (concat dotspacemacs-template-directory ".spacemacs.template")))

(defun find-spacemacs-file ()
  (interactive)
  "Edit the `file' in the spacemacs base directory, in the current window."
  (ido-find-file-in-dir spacemacs-directory))

(defun find-contrib-file ()
  (interactive)
  "Edit the `file' in the spacemacs base directory, in the current window."
  (ido-find-file-in-dir configuration-layer-contrib-directory))

;; From http://xugx2007.blogspot.ca/2007/06/benjamin-rutts-emacs-c-development-tips.html
(setq compilation-finish-function
      (lambda (buf str)

        (if (or (string-match "exited abnormally" str)
                (string-match "FAILED" (buffer-string)))

            ;; there were errors
            (message "There were errors. SPC-e-n to visit.")
          (unless (or (string-match "Grep finished" (buffer-string))
                      (string-match "Ag finished" (buffer-string))
                      (string-match "nosetests" (buffer-name)))

            ;; no errors
            (message "compilation ok.")))))

;; from https://gist.github.com/timcharper/493269
(defun split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun ido-invoke-in-other-window ()
  "signals ido mode to switch to (or create) another window after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'other)
  (ido-exit-minibuffer))

(defun ido-invoke-in-horizontal-split ()
  "signals ido mode to split horizontally and switch after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'horizontal)
  (ido-exit-minibuffer))

(defun ido-invoke-in-vertical-split ()
  "signals ido mode to split vertically and switch after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'vertical)
  (ido-exit-minibuffer))

(defun ido-invoke-in-new-frame ()
  "signals ido mode to create a new frame after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'frame)
  (ido-exit-minibuffer))

(defadvice ido-read-internal (around ido-read-internal-with-minibuffer-other-window activate)
  (let* (ido-exit-minibuffer-target-window
         (this-buffer (current-buffer))
         (result ad-do-it))
    (cond
     ((equal ido-exit-minibuffer-target-window 'other)
      (if (= 1 (count-windows))
          (split-window-horizontally-and-switch)
        (other-window 1)))
     ((equal ido-exit-minibuffer-target-window 'horizontal)
      (split-window-horizontally-and-switch))

     ((equal ido-exit-minibuffer-target-window 'vertical)
      (split-window-vertically-and-switch))
     ((equal ido-exit-minibuffer-target-window 'frame)
      (make-frame)))
    (switch-to-buffer this-buffer) ;; why? Some ido commands, such as textmate.el's textmate-goto-symbol don't switch the current buffer
    result))

(defun set-google-translate-languages (source target)
  "Set source language for google translate.
For instance pass En as source for english."
  (interactive "sEnter source language (ie. En): \nsEnter target language (ie. En): "
               source target)
  (message (format "Set google translate source language to %s and target to %s"
                   source target))
  (setq google-translate-default-source-language source)
  (setq google-translate-default-target-language target))

;; from http://www.emacswiki.org/emacs/WordCount
(defun count-words-analysis (start end)
  "Count how many times each word is used in the region.
 Punctuation is ignored."
  (interactive "r")
  (let (words)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\w+" end t)
        (let* ((word (intern (match-string 0)))
               (cell (assq word words)))
          (if cell
              (setcdr cell (1+ (cdr cell)))
            (setq words (cons (cons word 1) words))))))
    (when (interactive-p)
      (message "%S" words))
    words))

(defun  set-attributes-from-alist (face attr)
  "Apply an alist of attributes in the form ((:PROP . VALUE)) to face."
  (while (car attr)
    (set-face-attribute face nil (caar attr) (cdar attr))
    (setq attr (cdr attr))))

(defun new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

(defun spacemacs/home ()
  "Go to home Spacemacs buffer"
  (interactive)
  (switch-to-buffer "*spacemacs*")
  )

(defun spacemacs/insert-line-above-no-indent (count)
  (interactive "p")
  (save-excursion
    (evil-previous-line)
    (evil-move-end-of-line)
    (while (> count 0)
      (insert "\n")
      (setq count (1- count)))))

(defun spacemacs/insert-line-below-no-indent (count)
  "Insert a new line below with no identation."
  (interactive "p")
  (save-excursion
    (evil-move-end-of-line)
    (while (> count 0)
      (insert "\n")
      (setq count (1- count)))))

;; from https://github.com/gempesaw/dotemacs/blob/emacs/dg-defun.el
(defun kill-matching-buffers-rudely (regexp &optional internal-too)
  "Kill buffers whose name matches the specified REGEXP. This
function, unlike the built-in `kill-matching-buffers` does so
WITHOUT ASKING. The optional second argument indicates whether to
kill internal buffers too."
  (interactive "sKill buffers matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (kill-buffer buffer)))))

;; advise to prevent server from closing

(defvar spacemacs-really-kill-emacs nil
  "prevent window manager close from closing instance.")

(defun spacemacs-persistent-server-running-p ()
  "Requires spacemacs-really-kill-emacs to be toggled and
dotspacemacs-persistent-server to be t"
  (and (fboundp 'server-running-p)
       (server-running-p)
       dotspacemacs-persistent-server))

(defadvice kill-emacs (around spacemacs-really-exit activate)
  "Only kill emacs if a prefix is set"
  (if (and (not spacemacs-really-kill-emacs)
           (spacemacs-persistent-server-running-p))
      (spacemacs/frame-killer)
    ad-do-it))

(defadvice save-buffers-kill-emacs (around spacemacs-really-exit activate)
  "Only kill emacs if a prefix is set"
  (if (or spacemacs-really-kill-emacs (not dotspacemacs-persistent-server))
      ad-do-it
    (spacemacs/frame-killer)))

(defun spacemacs/save-buffers-kill-emacs ()
  "Save all changed buffers and exit Spacemacs"
  (interactive)
  (setq spacemacs-really-kill-emacs t)
  (save-buffers-kill-emacs))

(defun spacemacs/kill-emacs ()
  "Lose all changes and exit Spacemacs"
  (interactive)
  (setq spacemacs-really-kill-emacs t)
  (kill-emacs))

(defun spacemacs/prompt-kill-emacs ()
  "Prompt to save changed buffers and exit Spacemacs"
  (interactive)
  (setq spacemacs-really-kill-emacs t)
  (save-some-buffers)
  (kill-emacs))

(defun spacemacs/frame-killer ()
  "Kill server buffer and hide the main Emacs window"
  (interactive)
  (server-kill-buffer)
  (make-frame-invisible nil 1))

;; A small minor mode to use a big fringe
;; from http://bzg.fr/emacs-strip-tease.html
(defvar bzg-big-fringe-mode nil)
(define-minor-mode bzg-big-fringe-mode
  "Minor mode to use big fringe in the current buffer."
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
           (* 100 (frame-char-width)))
        2))))

(defun fill-char-to-column (char column)
  " Fill the line with CHAR up to the given COLUMN"
  (interactive "cFill with char: \nnUp to column: "
               char column)

)

(defun spacemacs/toggle-frame-fullscreen ()
  "Respect the `dotspacemacs-fullscreen-use-non-native' variable when
toggling fullscreen."
  (interactive)
  (if dotspacemacs-fullscreen-use-non-native
      (toggle-frame-fullscreen-non-native)
    (toggle-frame-fullscreen)))

(defun toggle-fullscreen ()
  "Toggle full screen on X11 and Carbon"
  (interactive)
  (cond
   ((eq window-system 'x)
    (set-frame-parameter nil 'fullscreen
                         (when (not (frame-parameter nil 'fullscreen))
                           'fullboth)))
   ((eq window-system 'mac)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullscreen)))
   ))

(defun toggle-frame-fullscreen-non-native ()
  "Toggle full screen non-natively. Uses the `fullboth' frame paramerter
   rather than `fullscreen'. Useful to fullscreen on OSX w/o animations."
  (interactive)
  (modify-frame-parameters
   nil
   `((maximized
      . ,(unless (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
	   (frame-parameter nil 'fullscreen)))
     (fullscreen
      . ,(if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
	     (if (eq (frame-parameter nil 'maximized) 'maximized)
		 'maximized)
	   'fullboth)))))

(defmacro spacemacs|diminish (mode unicode &optional ascii)
  "Diminish MODE name in mode line to UNICODE or ASCII depending on the value
`dotspacemacs-mode-line-unicode-symbols'.

If ASCII si not provided then UNICODE is used instead."
  (let ((dim (if dotspacemacs-mode-line-unicode-symbols
                 unicode
               (if ascii ascii unicode))))
    `(eval-after-load 'diminish '(diminish ',mode ,dim))))

(defmacro spacemacs|hide-lighter (mode)
  "Diminish MODE name in mode line to LIGHTER."
  `(eval-after-load 'diminish '(diminish ',mode)))

;; taken from Prelude: https://github.com/bbatsov/prelude
(defmacro spacemacs|advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command
                      (,class ,(intern (format "%S-%s" command advice-name))
                              activate)
                    ,@body))
               commands)))

(defun disable-electric-indent-mode ()
  (if (fboundp 'electric-indent-local-mode)
      ;; for 24.4
      (electric-indent-local-mode -1)
    ;; for 24.3
    (add-hook 'electric-indent-functions
              (lambda () 'no-indent) nil 'local)))

(defun spacemacs/safe-revert-buffer ()
  "Prompt before reverting the file."
  (interactive)
  (revert-buffer nil nil))

(defun spacemacs/safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
      (erase-buffer)))

(defun spacemacs/ert-run-tests-buffer ()
  "Run all the tests in the current buffer."
  (interactive)
  (save-buffer)
  (load-file (buffer-file-name))
  (ert t))

(defun spacemacs/alternate-buffer ()
  "Switch back and forth between current and last buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(defun spacemacs/highlight-TODO-words ()
  "Highlight keywords for  "
  (interactive)
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

(defun current-line ()
  "Return the line at point as a string."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun spacemacs/open-in-external-app ()
  "Open current file in external application."
  (interactive)
  (let ((file-path (if (eq major-mode 'dired-mode)
                       (dired-get-file-for-visit)
                     (buffer-file-name))))
    (if file-path
        (cond
         ((system-is-mswindows) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
         ((system-is-mac) (shell-command (format "open \"%s\"" file-path)))
         ((system-is-linux) (let ((process-connection-type nil))
                              (start-process "" nil "xdg-open" file-path))))
      (message "No file associated to this buffer."))))

(defun spacemacs/next-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (if (and (boundp 'flycheck-mode)
           (symbol-value flycheck-mode))
      (call-interactively 'flycheck-next-error)
    (call-interactively 'next-error)))

(defun spacemacs/previous-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (if (and (boundp 'flycheck-mode)
           (symbol-value flycheck-mode))
      (call-interactively 'flycheck-previous-error)
    (call-interactively 'previous-error)))

(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;; http://stackoverflow.com/a/10216338/4869
(defun copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))


(defun copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;; indent on paste
;; from Prelude: https://github.com/bbatsov/prelude
(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) spacemacs-yank-indent-threshold)
      (indent-region beg end nil)))

;; hide mode line
;; from http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)
(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(spacemacs|advise-commands
 "indent" (yank yank-pop evil-paste-before evil-paste-after) after
 "If current mode is not one of spacemacs-indent-sensitive-modes
 indent yanked text (with universal arg don't indent)."
 (if (and (not (equal '(4) (ad-get-arg 0)))
          (not (member major-mode spacemacs-indent-sensitive-modes))
          (or (derived-mode-p 'prog-mode)
              (member major-mode spacemacs-indent-sensitive-modes)))
     (let ((transient-mark-mode nil))
       (yank-advised-indent-function (region-beginning) (region-end)))))

;; modified function from http://emacswiki.org/emacs/AlignCommands
(defun align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let ((complete-regexp (if after
                             (concat regexp "\\([ \t]*\\)")
                           (concat "\\([ \t]*\\)" regexp)))
        (group (if justify-right -1 1)))
    (align-regexp start end complete-regexp group 1 t)))

;; Modified answer from http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)"
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defmacro create-align-repeat-x (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (align-repeat start end ,regexp ,justify-right after)))))

(create-align-repeat-x "comma" "," nil t)
(create-align-repeat-x "semicolon" ";" nil t)
(create-align-repeat-x "colon" ":" nil t)
(create-align-repeat-x "equal" "=")
(create-align-repeat-x "math-oper" "[+\\-*/]")
(create-align-repeat-x "ampersand" "&")
(create-align-repeat-x "bar" "|")
(create-align-repeat-x "left-paren" "(")
(create-align-repeat-x "right-paren" ")" t)
