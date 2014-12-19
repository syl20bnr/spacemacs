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

(defun spacemacs/declare-prefix (prefix name)
  "Declare a prefix PREFIX. PREFIX is a string describing
a key sequence. NAME is a symbol name used as the prefix command."
  (let ((command (intern (concat spacemacs/prefix-command-string name))))
    (define-prefix-command command)
    (evil-leader/set-key prefix command)))

;; Waiting to fix the issue with guide-key before reactivating/updating this
;; function
;; (defun spacemacs/declare-prefix-for-mode (mode prefix name)
;;   "Declare a prefix PREFIX. MODE is the mode in which this prefix command should
;; be added. PREFIX is a string describing a key sequence. NAME is a symbol name
;; used as the prefix command."
;;   (let ((command (intern (concat spacemacs/prefix-command-string name))))
;;     (define-prefix-command command)
;;     (evil-leader/set-key-for-mode mode prefix command)))

(defun spacemacs/activate-evil-leader-for-maps (map-list)
  "Remove the evil-leader binding from all the maps in MAP-LIST."
  (mapc (lambda (x)
          (eval `(define-key ,x (kbd evil-leader/leader)
                   evil-leader--default-map)))
        map-list))

(defun spacemacs/activate-evil-leader-for-map (map)
  "Remove the evil-leader binding from the passed MAP."
  (spacemacs/activate-evil-leader-for-maps `(,map)))

(defun spacemacs/activate-major-mode-leader ()
  "Bind major mode key map to `dotspacemacs-major-mode-leader-key'."
  (setq mode-map (cdr (assoc major-mode evil-leader--mode-maps)))
  (when mode-map
    (setq major-mode-map (lookup-key mode-map (kbd "m")))
    (define-key evil-normal-state-local-map
      (kbd dotspacemacs-major-mode-leader-key) major-mode-map)
    (define-key evil-motion-state-local-map
      (kbd dotspacemacs-major-mode-leader-key) major-mode-map)))

(defmacro spacemacs|evilify (map &rest body)
  "Add `hjkl' navigation, search and visual state to MAP and set additional
bindings contained in BODY."
  `(evil-add-hjkl-bindings ,map 'emacs
    "/" 'evil-search-forward
    "n" ',(lookup-key evil-motion-state-map "n")
    "N" ',(lookup-key evil-motion-state-map "N")
    "v" 'evil-visual-char
    "V" 'evil-visual-line
    (kbd "C-v") 'evil-visual-block
    ,@body))

;; From http://stackoverflow.com/a/18796138
;; Cycle through this set of themes
(defvar spacemacs-themes '(solarized-light
                           solarized-dark
                           leuven
                           monokai
                           zenburn)
  "Themes officially supported by spacemacs.")
(defvar spacemacs-cur-theme (pop spacemacs-themes)
  "Current spacemacs theme.")

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

(defun spacemacs/cycle-spacemacs-theme ()
  "Cycle through themes defined in spacemacs-themes."
  (interactive)
  (when  spacemacs-cur-theme
    (disable-theme  spacemacs-cur-theme)
    (setq spacemacs-themes (append spacemacs-themes
                                   (list spacemacs-cur-theme))))
  (setq  spacemacs-cur-theme (pop spacemacs-themes))
  (message "Loading theme %s..." spacemacs-cur-theme)
  (load-theme spacemacs-cur-theme t))

(defadvice load-theme (after spacemacs/load-theme-adv activate)
  "Perform post load processing."
  (let ((theme (ad-get-arg 0)))
    (setq spacemacs-cur-theme theme)
    (spacemacs/post-theme-init theme)))

(defun spacemacs/post-theme-init (theme)
  " Some processing that needs to be done when the current theme has been
changed to THEME."
  (interactive)
      ;; Define a face for each state
  (if (fboundp 'spacemacs/set-state-faces)
      (spacemacs/set-state-faces))
  (if (fboundp 'spacemacs/set-flycheck-mode-line-faces)
      (spacemacs/set-flycheck-mode-line-faces))
  (if (fboundp 'powerline-reset)
      (powerline-reset)))

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
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (set-register '_ (list (current-window-configuration)))
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

(defun find-spacemacs-file ()
  (interactive)
  "Edit the `file' in the spacemacs base directory, in the current window."
  (ido-find-file-in-dir spacemacs-directory))

(defun find-contrib-file ()
  (interactive)
  "Edit the `file' in the spacemacs base directory, in the current window."
  (ido-find-file-in-dir config-system-contrib-directory))

;; From http://xugx2007.blogspot.ca/2007/06/benjamin-rutts-emacs-c-development-tips.html
(setq compilation-finish-function
   (lambda (buf str)

     (if (or (string-match "exited abnormally" str)
            (string-match "FAILED" (buffer-string)))

         ;;there were errors
         (message "There were errors. SPC-e-n to visit.")
       (unless (or (string-match "Grep finished" (buffer-string))
                  (string-match "Ag finished" (buffer-string))
                  (string-match "nosetests" (buffer-name)))

         ;;no errors, make the compilation window go away in 0.5 seconds
         (delete-windows-on buf)
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


;; from https://gist.github.com/cofi/3013327
(defun cofi/helm-flyspell-correct ()
    "Use helm for flyspell correction.
Adapted from `flyspell-correct-word-before-point'."
    (interactive)
    ;; use the correct dictionary
    (flyspell-accept-buffer-local-defs)
    (let ((cursor-location (point))
          (word (flyspell-get-word))
          (opoint (point)))
      (if (consp word)
          (let ((start (car (cdr word)))
                (end (car (cdr (cdr word))))
                (word (car word))
                poss ispell-filter)
            ;; now check spelling of word.
            (ispell-send-string "%\n")	;put in verbose mode
            (ispell-send-string (concat "^" word "\n"))
            ;; wait until ispell has processed word
            (while (progn
                     (accept-process-output ispell-process)
                     (not (string= "" (car ispell-filter)))))
            ;; Remove leading empty element
            (setq ispell-filter (cdr ispell-filter))
            ;; ispell process should return something after word is sent.
            ;; Tag word as valid (i.e., skip) otherwise
            (or ispell-filter
               (setq ispell-filter '(*)))
            (if (consp ispell-filter)
                (setq poss (ispell-parse-output (car ispell-filter))))
            (cond
             ((or (eq poss t) (stringp poss))
              ;; don't correct word
              t)
             ((null poss)
              ;; ispell error
              (error "Ispell: error in Ispell process"))
             (t
              ;; The word is incorrect, we have to propose a replacement.
              (flyspell-do-correct (helm-comp-read "Correction: "
                                                   (append
                                                    (third poss)
                                                    '(("Save word"        . save)
                                                      ("Accept (session)" . session)
                                                      ("Accept (buffer)"  . buffer)))
                                                   :name (format "%s [%s]" word (or ispell-local-dictionary
                                                                                   ispell-dictionary
                                                                                   "Default"))
                                                   :must-match t
                                                   :alistp t)

                                   poss word cursor-location start end opoint)))
            (ispell-pdict-save t)))))

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

(defadvice kill-emacs (around spacemacs-really-exit activate)
  "Only kill emacs if a prefix is set"
  (if (or spacemacs-really-kill-emacs (not dotspacemacs-persistent-server))
      ad-do-it)
  (spacemacs/frame-killer))

(defadvice save-buffers-kill-emacs (around spacemacs-really-exit activate)
  "Only kill emacs if a prefix is set"
  (if (or spacemacs-really-kill-emacs (not dotspacemacs-persistent-server))
      ad-do-it)
  (spacemacs/frame-killer))

(defun spacemacs/save-buffers-kill-emacs ()
  (interactive)
  (setq spacemacs-really-kill-emacs t)
  (save-buffers-kill-emacs))

(defun spacemacs/kill-emacs ()
  (interactive)
  (setq spacemacs-really-kill-emacs t)
  (kill-emacs))

(defun spacemacs/frame-killer ()
  "Exit server buffers and hide the main Emacs window"
  (interactive)
  (server-edit)
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

;;; begin scale font micro-state

(defun spacemacs/scale-font-size-overlay-map ()
  "Set a temporary overlay map to easily change the font size."
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "+") 'spacemacs/scale-up-font)
     (define-key map (kbd "-") 'spacemacs/scale-down-font)
     (define-key map (kbd "=") 'spacemacs/reset-font-size)
     map) t))

(defun spacemacs/font-scaling-micro-state-doc ()
  "Display a short documentation in the mini buffer."
  (echo "Scale Font micro-state:
  + to scale up
  - to scale down
  = to reset
Press any other key to exit."))

(defun spacemacs/scale-up-or-down-font-size (direction)
  "Scale the font. If DIRECTION is positive or zero the font is scaled up,
otherwise it is scaled down."
  (interactive)
  (let ((scale 0.5))
    (if (eq direction 0)
        (text-scale-set 0)
      (if (< direction 0)
          (text-scale-decrease scale)
        (text-scale-increase scale))))
  (spacemacs/scale-font-size-overlay-map)
  (spacemacs/font-scaling-micro-state-doc))

(defun spacemacs/scale-up-font ()
  "Scale up the font."
  (interactive)
  (spacemacs/scale-up-or-down-font-size 1))

(defun spacemacs/scale-down-font ()
  "Scale up the font."
  (interactive)
  (spacemacs/scale-up-or-down-font-size -1))

(defun spacemacs/reset-font-size ()
  "Reset the font size."
  (interactive)
  (spacemacs/scale-up-or-down-font-size 0))

;;; end scale font micro-state

;;; begin resize window micro-state

(defun spacemacs//resize-window-micro-state-doc ()
  (echo (format
         "[%sx%s] Resize window: (H/L) shrink/enlarge horizontally, (J/K) shrink/enlarge vertically"
         (window-total-width) (window-total-height))))

(defun spacemacs/resize-window-overlay-map ()
  "Set a temporary overlay map to easily resize a window."
  (interactive)
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "H") 'spacemacs/shrink-window-horizontally)
     (define-key map (kbd "J") 'spacemacs/shrink-window)
     (define-key map (kbd "K") 'spacemacs/enlarge-window)
     (define-key map (kbd "L") 'spacemacs/enlarge-window-horizontally)
     map) t)
  (spacemacs//resize-window-micro-state-doc))

(defun spacemacs/shrink-window-horizontally (delta)
  "Wrap `spacemacs/shrink-window-horizontally'."
  (interactive "p")
  (shrink-window delta t)
  (spacemacs/resize-window-overlay-map))

(defun spacemacs/shrink-window (delta)
  "Wrap `spacemacs/shrink-window'."
  (interactive "p")
  (shrink-window delta)
  (spacemacs/resize-window-overlay-map))

(defun spacemacs/enlarge-window (delta)
  "Wrap `spacemacs/enlarge-window'."
  (interactive "p")
  (enlarge-window delta)
  (spacemacs/resize-window-overlay-map))

(defun spacemacs/enlarge-window-horizontally (delta)
  "Wrap `spacemacs/enlarge-window-horizontally'."
  (interactive "p")
  (enlarge-window delta t)
  (spacemacs/resize-window-overlay-map))

;;; end resize window micro-state

(defmacro spacemacs|diminish (mode lighter)
  "Diminish MODE name in mode line to LIGHTER."
  `(when (display-graphic-p)
     (eval-after-load 'diminish
       '(diminish ',mode ,lighter))))

(defmacro spacemacs|hide-lighter (mode)
  "Diminish MODE name in mode line to LIGHTER."
  `(eval-after-load 'diminish '(diminish ',mode)))

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

(defun spacemacs/last-buffer ()
  "Switch back and forth between current and last buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(defun spacemacs/highlight-TODO-words ()
  "Highlight keywords for  "
  (interactive)
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))
