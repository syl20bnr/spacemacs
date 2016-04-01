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

(defun system-is-mac ()
  (interactive)
  (string-equal system-type "darwin"))
(defun system-is-linux ()
  (interactive)
  (string-equal system-type "gnu/linux"))

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
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
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
(defun camdez/show-buffer-file-name ()
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

;; From http://stackoverflow.com/a/18796138
;; Cycle through this set of themes
(setq spacemacs-themes '(solarized-light
                         solarized-dark
                         monokai
                         zenburn))
(defvar spacemacs-cur-theme (pop spacemacs-themes))
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
  (setq spacemacs-cur-theme (ad-get-arg 0))
  (spacemacs/post-theme-init spacemacs-cur-theme))

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

(defun spacemacs/set-font (font size &optional options)
  (let* ((fontstr (if options
                       (format "%s-%s:%s" font size options)
                     (format "%s-%s" font size))))
    (message (format "Set default font: %s" fontstr))
    (add-to-list 'default-frame-alist (cons 'font fontstr))
    (set-default-font fontstr)))

(defun spacemacs/scale-font-size (direction)
  "Set a temporary overlay map to easily change the font size.
 DIRECTION is any number where:
 - a positive number increase the size of the font
 - a negative number decrease the size of the font
 - 0 reset the font size to its default value"
  (interactive)
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "+") 
       '(lambda () (interactive)
          (spacemacs/scale-up-or-down-font-size 1)))
     (define-key map (kbd "-")
       '(lambda () (interactive)
          (spacemacs/scale-up-or-down-font-size -1)))
     (define-key map (kbd "=")
       '(lambda () (interactive) (spacemacs/reset-font-size)))
     map) t)
  (if (eq direction 0)
      (spacemacs/reset-font-size)
    (spacemacs/scale-up-or-down-font-size direction)))

(defun spacemacs/font-scaling-micro-state-doc ()
  "Display a short documentation in the mini buffer."
  (message "Scale Font micro-state:
  + to scale up
  - to scale down
  = to reset
Press any other key to exit."))

(defun spacemacs/scale-up-or-down-font-size (direction)
  "Scale the font. If DIRECTION is positive or zero the font is scaled up,
otherwise it is scaled down."
  (interactive)
  (let ((scale 0.5))
    (if (< direction 0)
        (text-scale-decrease scale)
      (text-scale-increase scale))
    (spacemacs/font-scaling-micro-state-doc)))

(defun spacemacs/reset-font-size ()
  "Reset the font size (apply a scale of 0)."
  (interactive)
  (text-scale-set 0)
  (spacemacs/font-scaling-micro-state-doc))

(defmacro spacemacs//diminish (mode lighter)
  "Diminish MODE name in mode line to LIGHTER."
  `(when (display-graphic-p)
     (eval-after-load 'diminish
       '(diminish ',mode ,lighter))))

(defmacro spacemacs//hide-lighter (mode)
  "Diminish MODE name in mode line to LIGHTER."
  `(eval-after-load 'diminish '(diminish ',mode)))
