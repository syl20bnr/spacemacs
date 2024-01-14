;;; helm-command.el --- Helm execute-exended-command. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2023 Thierry Volpiatto 

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

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-help)
(require 'helm-mode)
(require 'helm-elisp)


(defvar helm-M-x-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-comp-read-map)
    (define-key map (kbd "C-u") nil)
    (define-key map (kbd "C-u") #'helm-M-x-universal-argument)
    (define-key map (kbd "C-]") #'helm-M-x-toggle-short-doc)
    map))


(defgroup helm-command nil
  "Emacs command related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-M-x-always-save-history nil
  "`helm-M-x' save command in `extended-command-history' even when it fails."
  :type  'boolean)

(defcustom helm-M-x-reverse-history nil
  "The history source of `helm-M-x' appear in second position when non-nil."
  :type 'boolean)

(defcustom helm-M-x-fuzzy-match t
  "Helm-M-x fuzzy matching when non nil."
  :type 'boolean)

(defcustom helm-M-x-show-short-doc nil
  "Show short docstring of command when non nil.
This value can be toggled with
\\<helm-M-x-map>\\[helm-M-x-toggle-short-doc] while in helm-M-x session."
  :type 'boolean)


;;; Faces
;;
;;
(defgroup helm-command-faces nil
  "Customize the appearance of helm-command."
  :prefix "helm-"
  :group 'helm-command
  :group 'helm-faces)

(defface helm-M-x-key
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "orange" :box (:line-width -1)))
  "Face used in helm-M-x to show keybinding."
  :group 'helm-command-faces)

(defface helm-command-active-mode
  '((t :inherit font-lock-builtin-face))
  "Face used by `helm-M-x' for activated modes."
  :group 'helm-command-faces)

(defface helm-M-x-short-doc
    '((t :box (:line-width -1) :foreground "DimGray"))
    "Face used by `helm-M-x' for short docstring."
  :group 'helm-command-faces)

(defvar helm-M-x-input-history nil)
(defvar helm-M-x-prefix-argument nil
  "Prefix argument before calling `helm-M-x'.")
(defvar helm-M-x--timer nil)
(defvar helm-M-x--unwind-forms-done nil)

(defun helm-M-x-get-major-mode-command-alist (mode-map)
  "Return alist of MODE-MAP."
  (when mode-map
    (cl-loop for key being the key-seqs of mode-map using (key-bindings com)
             for str-key  = (key-description key)
             for ismenu   = (string-match "<menu-bar>" str-key)
             unless ismenu collect (cons str-key com))))

(defun helm-get-mode-map-from-mode (mode)
  "Guess the mode-map name according to MODE.
Some modes don't use conventional mode-map name so we need to
guess mode-map name. E.g. `python-mode' ==> py-mode-map.
Return nil if no mode-map found."
  (cl-loop ;; Start with a conventional mode-map name.
        with mode-map    = (intern-soft (format "%s-map" mode))
        with mode-string = (symbol-name mode)
        with mode-name   = (replace-regexp-in-string "-mode" "" mode-string)
        while (not mode-map)
        for count downfrom (length mode-name)
        ;; Return when no result after parsing entire string.
        when (eq count 0) return nil
        for sub-name = (substring mode-name 0 count)
        do (setq mode-map (intern-soft (format "%s-map" (concat sub-name "-mode"))))
        finally return mode-map))

(defun helm-M-x-current-mode-map-alist ()
  "Return mode-map alist of current `major-mode'."
  (let ((map-sym (helm-get-mode-map-from-mode major-mode)))
    (when (and map-sym (boundp map-sym))
      (helm-M-x-get-major-mode-command-alist (symbol-value map-sym)))))

(defun helm-M-x-toggle-short-doc ()
  "Toggle short doc display in helm-M-x."
  (interactive)
  (setq helm-M-x-show-short-doc (not helm-M-x-show-short-doc))
  (helm-force-update (concat "^" (helm-get-selection)) (helm-get-current-source)))
(put 'helm-M-x-toggle-short-doc 'no-helm-mx t)

(defun helm-M-x-transformer-1 (candidates &optional sort ignore-props)
  "Transformer function to show bindings in emacs commands.
Show global bindings and local bindings according to current
`major-mode'.
If SORT is non nil sort list with `helm-generic-sort-fn'.
Note that SORT should not be used when fuzzy matching because
fuzzy matching is running its own sort function with a different
algorithm."
  (with-helm-current-buffer
    (cl-loop with max-len = (when helm-M-x-show-short-doc
                              (helm-in-buffer-get-longest-candidate))
             with local-map = (helm-M-x-current-mode-map-alist)
             for cand in candidates
             for local-key  = (car (rassq cand local-map))
             for key        = (substitute-command-keys (format "\\[%s]" cand))
             for sym        = (intern (if (consp cand) (car cand) cand))
             for doc = (when max-len
                         (helm-get-first-line-documentation (intern-soft cand)))   
             for disp       = (if (or (eq sym major-mode)
                                      (and (memq sym minor-mode-list)
                                           (boundp sym)
                                           (buffer-local-value sym helm-current-buffer)))
                                  (propertize cand 'face 'helm-command-active-mode)
                                cand)
             unless (and (null ignore-props) (or (get sym 'helm-only) (get sym 'no-helm-mx)))
             collect
             (cons (cond ((and (string-match "^M-x" key) local-key)
                          (propertize (format "%s%s%s %s"
                                              disp
                                              (if doc (make-string (+ 1 (- max-len (length cand))) ? ) "")
                                              (if doc (propertize doc 'face 'helm-M-x-short-doc) "")
                                              (propertize
                                               " " 'display
                                               (propertize local-key 'face 'helm-M-x-key)))
                                      'match-part disp))
                         ((and (string-match "^M-x" key) (not (string= key "M-x")))
                          (propertize (format "%s%s%s"
                                              disp
                                              (if doc (make-string (+ 1 (- max-len (length cand))) ? ) "")
                                              (if doc (propertize doc 'face 'helm-M-x-short-doc) ""))
                                      'match-part disp))
                         (t (propertize (format "%s%s%s %s"
                                                disp
                                                (if doc (make-string (+ 1 (- max-len (length cand))) ? ) "")
                                                (if doc (propertize doc 'face 'helm-M-x-short-doc) "")
                                                (propertize
                                                 " " 'display
                                                 (propertize key 'face 'helm-M-x-key)))
                                        'match-part disp)))
                   cand)
             into ls
             finally return
             (if sort (sort ls #'helm-generic-sort-fn) ls))))

(defun helm-M-x-transformer (candidates _source)
  "Transformer function for `helm-M-x' candidates."
  ;; Generic sort function is handling helm-flex.
  (helm-M-x-transformer-1 candidates (null helm--in-fuzzy)))

(defun helm-M-x-transformer-no-sort (candidates _source)
  "Transformer function for `helm-M-x' candidates."
  (helm-M-x-transformer-1 candidates))

(defun helm-M-x-transformer-no-sort-no-props (candidates _source)
  "Transformer function for `helm-M-x' candidates."
  (helm-M-x-transformer-1 candidates nil t))

(defun helm-M-x--notify-prefix-arg ()
  ;; Notify a prefix-arg set AFTER calling M-x.
  (when prefix-arg
    (with-helm-window
      (helm-display-mode-line (helm-get-current-source) 'force))))

(defun helm-cmd--get-current-function-name ()
  (save-excursion
    (beginning-of-defun)
    (cadr (split-string (buffer-substring-no-properties
                         (pos-bol) (pos-eol))))))

(defun helm-cmd--get-preconfigured-commands (&optional dir)
  (let* ((helm-dir (or dir (helm-basedir (locate-library "helm"))))
         (helm-autoload-file (expand-file-name "helm-autoloads.el" helm-dir))
         results)
    (when (file-exists-p helm-autoload-file)
      (with-temp-buffer
        (insert-file-contents helm-autoload-file)
        (while (re-search-forward "Preconfigured" nil t)
          (push (substring (helm-cmd--get-current-function-name) 1) results))))
    results))

(defun helm-M-x-universal-argument ()
  "Same as `universal-argument' but for `helm-M-x'."
  (interactive)
  (if helm-M-x-prefix-argument
      (progn (setq helm-M-x-prefix-argument nil)
             (let ((inhibit-read-only t))
               (with-selected-window (minibuffer-window)
                 (save-excursion
                   (goto-char (point-min))
                   (delete-char (- (minibuffer-prompt-width) (length "M-x "))))))
             (message "Initial prefix arg disabled"))
    (setq prefix-arg (list 4))
    (universal-argument--mode)))
(put 'helm-M-x-universal-argument 'helm-only t)

(defun helm-M-x-persistent-action (candidate)
  (helm-elisp--persistent-help
   candidate 'helm-describe-function))

(defun helm-M-x--move-selection-after-hook ()
  (setq current-prefix-arg nil))

(defun helm-M-x--before-action-hook ()
  (remove-hook 'helm-move-selection-after-hook
               #'helm-M-x--move-selection-after-hook))

(defclass helm-M-x-class (helm-source-in-buffer helm-type-command)
  ((requires-pattern :initform 0)
   (must-match :initform t)
   (filtered-candidate-transformer :initform 'helm-M-x-transformer-no-sort)
   (persistent-help :initform "Describe this command")
   (help-message :initform 'helm-M-x-help-message)
   (nomark :initform t)
   (cleanup :initform #'helm-M-x--unwind-forms)
   (keymap :initform 'helm-M-x-map)
   (resume :initform 'helm-M-x-resume-fn)))

(defun helm-M-x-resume-fn ()
  (when (and helm-M-x--timer (timerp helm-M-x--timer))
    (cancel-timer helm-M-x--timer)
    (setq helm-M-x--timer nil))
  (setq helm-M-x--timer (run-at-time 1 0.1 #'helm-M-x--notify-prefix-arg))
  (setq helm--mode-line-display-prefarg t)
  ;; Prevent displaying a wrong prefix arg when helm-resume is called
  ;; from prefix arg.
  (setq current-prefix-arg nil))

(defun helm-M-x-read-extended-command (collection &optional predicate history)
  "Read or execute action on command name in COLLECTION or HISTORY.

Helm completion is not provided when executing or defining kbd macros.

Arg COLLECTION should be an `obarray'.
Arg PREDICATE is a function that default to `commandp'.
Arg HISTORY default to `extended-command-history'."
  (setq helm--mode-line-display-prefarg t)
  (let* ((pred (or predicate #'commandp))
         (helm-fuzzy-sort-fn (lambda (candidates _source)
                               ;; Sort on real candidate otherwise
                               ;; "symbol (<binding>)" is used when sorting.
                               (helm-fuzzy-matching-default-sort-fn-1 candidates t)))
         (sources `(,(helm-make-source "Emacs Commands history" 'helm-M-x-class
                       :data (lambda ()
                               (helm-comp-read-get-candidates
                                ;; History should be quoted to
                                ;; force `helm-comp-read-get-candidates'
                                ;; to use predicate against
                                ;; symbol and not string.
                                (or history 'extended-command-history)
                                ;; Ensure using empty string to
                                ;; not defeat helm matching fns [1]
                                pred nil nil ""))
                       :fuzzy-match helm-M-x-fuzzy-match)
                    ,(helm-make-source "Emacs Commands" 'helm-M-x-class
                       :data (lambda ()
                               (helm-comp-read-get-candidates
                                ;; [1] Same comment as above.
                                collection pred nil nil ""))
                       :fuzzy-match helm-M-x-fuzzy-match)))
         (prompt (concat (cond
                          ((eq helm-M-x-prefix-argument '-) "- ")
                          ((and (consp helm-M-x-prefix-argument)
                                (eq (car helm-M-x-prefix-argument) 4))
                           "C-u ")
                          ((and (consp helm-M-x-prefix-argument)
                                (integerp (car helm-M-x-prefix-argument)))
                           (format "%d " (car helm-M-x-prefix-argument)))
                          ((integerp helm-M-x-prefix-argument)
                           (format "%d " helm-M-x-prefix-argument)))
                         "M-x ")))
    (setq helm-M-x--timer (run-at-time 1 0.1 #'helm-M-x--notify-prefix-arg))
    ;; Fix Bug#2250, add `helm-move-selection-after-hook' which
    ;; reset prefix arg to nil only for this helm session.
    (add-hook 'helm-move-selection-after-hook
              #'helm-M-x--move-selection-after-hook)
    (add-hook 'helm-before-action-hook
              #'helm-M-x--before-action-hook)
    (when (and sources helm-M-x-reverse-history)
      (setq sources (nreverse sources)))
    (unwind-protect
        (progn
          (setq current-prefix-arg nil)
          (helm :sources sources
                :prompt prompt
                :buffer "*helm M-x*"
                :history 'helm-M-x-input-history
                :truncate-lines t))
      (helm-M-x--unwind-forms))))

;; When running a command involving again helm from helm-M-x, the
;; unwind-protect UNWINDS forms are executed only once this helm
;; command exit leaving the helm-M-x timer running and other variables
;; and hooks not unset, so the timer is now in a global var and all
;; the forms that should normally run in unwind-protect are running as
;; well as soon as helm-M-x-execute-command is called.
(defun helm-M-x--unwind-forms (&optional done)
  ;; helm-M-x--unwind-forms-done is non nil when it have been called
  ;; once from helm-M-x-execute-command.
  (unless helm-M-x--unwind-forms-done
    (when (timerp helm-M-x--timer)
      (cancel-timer helm-M-x--timer)
      (setq helm-M-x--timer nil))
    (setq helm--mode-line-display-prefarg nil
          helm-fuzzy-sort-fn (default-toplevel-value 'helm-fuzzy-sort-fn))
    ;; Be sure to remove it here as well in case of quit.
    (remove-hook 'helm-move-selection-after-hook
                 #'helm-M-x--move-selection-after-hook)
    (remove-hook 'helm-before-action-hook
                 #'helm-M-x--before-action-hook))
  ;; Reset helm-M-x--unwind-forms-done to nil when DONE is
  ;; unspecified.
  (setq helm-M-x--unwind-forms-done done))

(defun helm-M-x-execute-command (command)
  "Execute COMMAND as an editor command.
COMMAND must be a symbol that satisfies the `commandp' predicate.
Save COMMAND to `extended-command-history'."
  (helm-M-x--unwind-forms t)
  (when command
    ;; Avoid having `this-command' set to *exit-minibuffer.
    (setq this-command command
          ;; Handle C-x z (repeat) Bug#322
          real-this-command command)
    ;; If helm-M-x is called with regular emacs completion (kmacro)
    ;; use the value of arg otherwise use helm-current-prefix-arg.
    (let ((prefix-arg (or helm-current-prefix-arg helm-M-x-prefix-argument))
          (command-name (symbol-name command)))
      (condition-case-unless-debug err
          (progn
            (command-execute command 'record)
            (add-to-history 'extended-command-history command-name))
        (error
         (when helm-M-x-always-save-history
           (add-to-history 'extended-command-history command-name))
         (signal (car err) (cdr err)))))))

(defun helm-M-x--vanilla-M-x ()
  (helm-M-x-execute-command
   (intern-soft
    (if helm-mode
        (unwind-protect
            (progn
              (helm-mode -1)
              (read-extended-command))
          (helm-mode 1))
      (read-extended-command)))))

;;;###autoload
(defun helm-M-x (_arg)
  "Preconfigured `helm' for Emacs commands.
It is `helm' replacement of regular `M-x'
`execute-extended-command'.

Unlike regular `M-x' Emacs vanilla `execute-extended-command'
command, the prefix args if needed, can be passed AFTER starting
`helm-M-x'.  When a prefix arg is passed BEFORE starting
`helm-M-x', the first `C-u' while in `helm-M-x' session will
disable it.

You can get help on each command by persistent action."
  (interactive
   (progn
     (setq helm-M-x-prefix-argument current-prefix-arg)
     (list current-prefix-arg)))
  (if (or defining-kbd-macro executing-kbd-macro)
      (helm-M-x--vanilla-M-x)
  (helm-M-x-read-extended-command obarray)))
(put 'helm-M-x 'interactive-only 'command-execute)

(provide 'helm-command)

;;; helm-command.el ends here
