;;; funcs.el --- Helm Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;;; Code:

(require 'core-funcs)
(require 'dash)
(require 'dired)

;;;; Helper functions

(defun spacemacs//helm-cleanup ()
  "Cleanup some helm related states when quitting."
  ;; deactivate helm transient state if active when closing the helm buffer
  (ignore-errors
    (spacemacs/helm-navigation-transient-state/nil)))

(defun spacemacs//helm-prepare-display ()
  "Prepare necessary settings to make Helm display properly."
  (setq spacemacs-display-buffer-alist display-buffer-alist)
  ;; the only buffer to display is Helm, nothing else we must set this
  ;; otherwise Helm cannot reuse its own windows for copyinng/deleting
  ;; etc... because of existing popwin buffers in the alist
  (setq display-buffer-alist nil)
  (setq spacemacs--helm-popwin-mode popwin-mode)
  (when popwin-mode
    (popwin-mode -1)))

(defun spacemacs//helm-restore-display ()
  "Not documented."
  ;; we must enable popwin-mode first then restore `display-buffer-alist'
  ;; Otherwise, popwin keeps adding up its own buffers to
  ;; `display-buffer-alist' and could slow down Emacs as the list grows
  (when spacemacs--helm-popwin-mode (popwin-mode))
  (setq display-buffer-alist spacemacs-display-buffer-alist))

;;;; REPLs Integration

(defun helm-available-repls ()
  "Show all the repls available."
  (interactive)
  (let ((helm-available-repls
         `((name . "HELM available REPLs")
           (candidates . ,(mapcar #'car spacemacs-repl-list))
           (action . (lambda (candidate)
                       (let ((repl (cdr (assoc candidate spacemacs-repl-list))))
                         (require (car repl))
                         (call-interactively (cdr repl))))))))
    (helm :sources '(helm-available-repls)
          :buffer "*helm repls*")))

;;;; Search Tools Integration

(defun spacemacs//helm-search-dispatcher (scope &optional tool default-input-p)
  "Call an appropriate search function.
- SCOPE The scope of the search:
  - \"buffer\" Search within all opened buffers.
  - \"dir\" Search within current directory.
  - \"project\" Search within current project.
- TOOL A string specifies the search tool to use. It must be found on \"PATH\"
  When nil, use the first search tool in `dotspacemacs-search-tools' found on
  \"PATH\".
- DEFAULT-INPUT-P When non-nil, use current region or symbol as default input.

In addition, when called with prefix argument, it has special behaviour that
depends on SCOPE:
- For \"buffer\", prompt for a buffer to search.
- For \"dir\", prompt for a directory to search.
- For \"project\", prompt for a project to the search."
  (require 'helm-files)
  (require 'helm-mode)
  (require 'projectile)
  (cl-letf
      (;; whether called with C-u
       (prefix-p current-prefix-arg)
       ;; when TOOL is non-nil, verify it exist
       ;; otherwise it's smart-search, and use the first one found
       (search-tool
        (if-let ((tool-found
                  (seq-find 'executable-find
                            (if (stringp tool)
                                (list tool)
                              dotspacemacs-search-tools))))
            tool-found
          (user-error (if (stringp tool)
                          "Can't find %s in your PATH"
                        "Can't find any search tool in your PATH")
                      tool)))
       ((symbol-function 'current-project-root)
        (lambda ()
          (if-let ((root (projectile-project-root)))
              root
            (user-error (if default-directory
                            (projectile-prepend-project-name
                             "is not in a project")
                          "You're not in a project")
                        default-directory))))
       ((symbol-function 'ask-dir-name)
        (lambda ()
          (helm-read-file-name
           "Directory name: "
           :name "Directory to search"
           :marked-candidates t
           :test 'file-directory-p
           :preselect
           (when-let ((preselection (or (dired-get-filename nil t)
                                        (buffer-file-name))))
             (if helm-ff-transformer-show-only-basename
                 (helm-basename preselection)
               preselection)))))
       ((symbol-function 'ask-project-dir)
        (lambda ()
          (if (projectile-project-p)
              (let
                  ((project
                    (helm
                     :sources
                     (helm-build-sync-source "Projectile projects"
                       :candidates
                       (lambda ()
                         (with-helm-current-buffer projectile-known-projects))
                       :fuzzy-match helm-projectile-fuzzy-match
                       :mode-line helm-read-file-name-mode-line-string)
                     :buffer (concat "*helm-projectile: "
                                     (projectile-project-name) "*")
                     :prompt (projectile-prepend-project-name
                              "Project to search: "))))
                (if (file-exists-p project)
                    project
                  (projectile-remove-known-project project)
                  (user-error "Selected project doesn't exist: %s")))
            (user-error "You're not in a project"))))
       ((symbol-function 'helm-grep-wrapper)
        (lambda (targets default-input-p)
          (let ((current-prefix-arg nil))
            (when default-input-p
              (cl-letf*
                  (((symbol-function 'this-fn) (symbol-function 'helm-do-grep-1))
                   ((symbol-function 'helm-do-grep-1)
                    (lambda (thing)
                      (let ((res (if (region-active-p)
                                     (buffer-substring-no-properties
                                      (region-beginning) (region-end))
                                   (thing-at-point 'symbol t))))
                        (this-fn thing nil nil nil nil
                                 (when res (rxt-quote-pcre res)))))))))
            (helm-do-grep-1 targets))))
       ((symbol-function 'helm-ag-wrapper)
        (lambda (func default-input-p args)
          (let ((current-prefix-arg nil))
            (when default-input-p
              (cl-letf* ((helm-ag-insert-at-point 'symbol)
                         ((symbol-function 'this-fn)
                          (symbol-function 'thing-at-point))
                         ((symbol-function 'thing-at-point)
                          (lambda (thing)
                            (let ((res (if (region-active-p)
                                           (buffer-substring-no-properties
                                            (region-beginning) (region-end))
                                         (this-fn thing))))
                              (when res (rxt-quote-pcre res))))))))
            (funcall func args)))))
    (pcase search-tool
      ("grep"
       (require 'helm-grep)
       (let
           ((targets
             (pcase scope
               ("buffer" (if prefix-p
                             (list (buffer-file-name))
                             (seq-filter #'identity
                                         (mapcar #'buffer-file-name
                                                 (buffer-list)))))
               ("dir" (seq-filter #'file-regular-p
                                  (directory-files (if prefix-p
                                                       (ask-dir-name)
                                                     default-directory)
                                                   t)))
               ("project" (seq-filter #'file-regular-p
                                      (directory-files (if prefix-p
                                                           (ask-project-dir)
                                                         (current-project-root))
                                                       t))))))
         (helm-grep-wrapper targets default-input-p)))
      ((pred (lambda (x) (member x '("rg" "ag" "pt" "ack" "grep"))))
       (require 'helm-ag)
       (pcase-let (;; setup appropriate base command and exit status
                   (helm-ag-base-command
                    (pcase search-tool
                      ("rg"
                       (concat "rg --smart-case --no-heading --color=never --line-number"
                               (when-let
                                   ((maxcol spacemacs-helm-rg-max-column-number)
                                    (_ (> maxcol 0)))
                                 (format " --max-columns=%d" maxcol))))
                      ((and "ag" (pred (lambda (_) (memq system-type '(ms-dos windows-nt)))))
                       "ag --vimgrep")
                      (val
                       (format "%s --nocolor --nogroup" (if (string= val "pt")
                                                            "pt -e"
                                                          val)))))
                   (helm-ag-success-exit-status
                    (when (string= search-tool "rg")
                      (list 0 2)))
                   ;; function to call and additional args
                   (`(,func ,args)
                    (pcase scope
                      ("buffer" (list (if prefix-p
                                          #'helm-do-ag-this-file
                                          #'helm-do-ag-buffers)
                                      nil))
                      ("dir" (list #'helm-do-ag
                                   (if prefix-p
                                       (ask-dir-name)
                                     default-directory)))
                      ("project" (list #'helm-do-ag
                                       (if prefix-p
                                           (ask-project-dir)
                                         (current-project-root)))))))
         (helm-ag-wrapper func default-input-p args)))
      (_ (user-error "Not a valid search tool %s" search-tool)))))

(byte-compile 'spacemacs//helm-search-dispatcher)

(defmacro spacemacs||helm-search-command (spec tool default-input-p)
  "Template for generic spacemacs helm search commands.
- SPEC A list of strings in the form of \"(SCOPE . DOC)\"
  - SCOPE The scope of the search.
  - DOC Extra documentation appended to the generic docstring.
- TOOL When nil, use the first search tool in variable
  `dotspacemacs-search-tools' whose excutable is found on \"PATH\".
  When non-nil, use the specified search tool if it's found on \"PATH\".
- DEFAULT-INPUT-P When non-nil, use current region or symbol as default input."
  (pcase-let*
      ((`(,scope ,doc) spec)
       (func-name
        (intern
         (format
          "spacemacs/helm-%s-%s%s"
          (or tool "smart-search")
          scope
          (if default-input-p "-with-input" ""))))
       (docstring
        (format
         "Search in %s with %s%s.\nWhen prefixed with universal argument, %s."
         scope
         (or tool
             "the first search tool in variable \`dotspacemacs-search-tools\' \
that is found in PATH")
         (if default-input-p " using a default input" "")
         doc)))
    `(;;;###autoload
      defun ,func-name ()
      ,docstring
      (interactive)
      (spacemacs//helm-search-dispatcher ,scope ,tool ,default-input-p))))

(--each
    (-table-flat
     'list
     '(spacemacs||helm-search-command)
     '(("buffer" "search in a specified buffers") ; spec, i.e. (scope doc)
       ("dir" "search in a specified directory")
       ("project" "search in a specified project"))
     '("rg" "ag" "pt" "ack" "grep" nil)           ; tool
     '(t nil))                                    ; default-input-p
  (eval it))

;;;; helm-projectile integration

(defun spacemacs//helm-project-smart-search-in-dir (dir)
  "Action to call `spacemacs/helm-smart-search-project' on DIR."
  (interactive)
  (let ((default-directory dir))
    (spacemacs/helm-smart-search-project)))

(defun spacemacs/helm-projectile-grep ()
  "Replace `helm-projectile-grep' to use `spacemacs/helm-smart-search-project'."
  (interactive)
  (require 'helm)
  (helm-exit-and-execute-action 'spacemacs//helm-project-smart-search-in-dir))

;;;; Helm Last Buffer

(defun spacemacs/resume-last-search-buffer ()
  "Open last \"helm-ag\" or \"helm-grep\" buffer."
  (interactive)
  (require 'helm)
  (cond ((get-buffer "*helm ag results*")
         (switch-to-buffer-other-window "*helm ag results*"))
        ((get-buffer "*helm-ag*")
         (helm-resume "*helm-ag*"))
        ((get-buffer "*hgrep*")
         (switch-to-buffer-other-window "*hgrep*"))
        (t
         (message "no previous search buffer found"))))

;;;; Helm Find Files

(defun spacemacs/helm-find-files (arg)
  "Custom spacemacs implementation for calling `helm-find-files-1'.
This removes the automatic guessing of the initial value based on thing at
point.

ARG An existing file."
  (interactive "P")
  ;; fixes #10882 and #11270
  (require 'helm-files)
  (let* ((hist (and arg
                    helm-ff-history
                    (helm-find-files-history arg)))
         (default-input hist)
         (input (cond ((and (eq major-mode 'dired-mode) default-input)
                       (file-name-directory default-input))
                      ((and (not (string= default-input ""))
                            default-input))
                      (t (expand-file-name (helm-current-directory))))))
    (set-text-properties 0 (length input) nil input)
    (helm-find-files-1 input)))

(byte-compile 'spacemacs/helm-find-files)

(defalias 'spacemacs/helm-find-files-with-guess 'helm-find-files)

;;;; Key-bindings

(defmacro spacemacs||set-helm-key (keys func)
  "Define a key bindings for FUNC using KEYS.
Ensure that helm is required before calling FUNC."
  (let ((func-name (intern (format "lazy-helm/%s" (symbol-name func)))))
    `(progn
       (defun ,func-name ()
         ,(format "Wrapper to ensure that `helm' is loaded before calling %s."
                  (symbol-name func))
         (interactive)
         (call-interactively ',func))
       (spacemacs/set-leader-keys ,keys ',func-name))))

;;;; Helm Actions

(defun spacemacs//helm-find-files-edit (candidate)
  "Open a dired buffer and immediately switch to editable mode.
CANDIDATE File to be opened."
  (dired (file-name-directory candidate))
  (dired-goto-file candidate)
  (dired-toggle-read-only))

(defun spacemacs/helm-find-files-edit ()
  "Exit helm, open a dired buffer and immediately switch to editable mode."
  (interactive)
  (require 'helm)
  (helm-exit-and-execute-action 'spacemacs//helm-find-files-edit))

(defun spacemacs/helm-jump-in-buffer ()
  "Jump in buffer using `imenu' facilities and helm."
  (interactive)
  (call-interactively
   (cond
    ((eq major-mode 'org-mode)
     (require 'helm-org)
     'helm-org-in-buffer-headings)
    (t
     (require 'helm-semantic)
     'helm-semantic-or-imenu))))

(defun spacemacs//helm-open-buffers-in-windows (buffers)
  "Open buffers across already-opened windows.
This function allows a different default action, on marking multiple
candidate buffers/files for helm. By default, helm either opens all
files/buffers in the same window, or creates splits.

This function instead opens the buffers (or files) across different already-open
windows. The first selected buffer is opened in the current window, the next is
opened in the window with higher number, etc. This will make a loop around, so
with 4 windows, and window 2 active, opening 4 buffers will open them in windows
2 3 4 1. If more buffers are opened than windows available, the remainder are
not set to any window (but in the case of files, they are still opened
to buffers).

BUFFERS Buffers to be opened."
  (require 'winum)
  (let ((num-buffers (length buffers))
        (num-windows (length (winum--window-list)))
        (cur-win (or (winum-get-number) (winum-get-number (other-window 1))))
        (num-buffers-placed 0))
    (cl-loop for buffer in buffers do
             (when (>= num-buffers-placed num-windows) (cl-return))
             (set-window-buffer (winum-get-window-by-number cur-win) buffer)
             (setq cur-win (+ 1 (mod cur-win num-windows)))
             (cl-incf num-buffers-placed))))

(defun spacemacs/helm-find-buffers-windows ()
  "Action to open the window with the specified buffer in `helm-buffers-list'."
  (interactive)
  (require 'helm)
  (unless (helm-marked-candidates) (user-error "Not in `helm-buffers-list'"))
  (helm-exit-and-execute-action
   (lambda (candidate)
     (spacemacs//helm-open-buffers-in-windows (helm-marked-candidates)))))

(defun spacemacs/helm-find-files-windows ()
  "Action to open the window with the specified file in a `helm' buffer."
  (interactive)
  (require 'helm)
  (unless (helm-marked-candidates) (user-error "Not in `helm' buffer"))
  (helm-exit-and-execute-action
   (lambda (candidate)
     (let* ((files (helm-marked-candidates))
            (buffers (mapcar 'find-file-noselect files)))
       (spacemacs//helm-open-buffers-in-windows buffers)))))

;;;; Generalized Next-Error Interface

(defun spacemacs//gne-init-helm-ag ()
  "Generalized `next-error' interface with `helm-ag' integration."
  (require 'helm-ag)
  (with-current-buffer "*helm ag results*"
    (setq spacemacs--gne-min-line 5
          spacemacs--gne-max-line (save-excursion
                                    (goto-char (point-max))
                                    (forward-line -1)
                                    (line-number-at-pos))
          spacemacs--gne-line-func
          (lambda (c)
            (helm-ag--find-file-action
             c 'find-file helm-ag--search-this-file-p))
          next-error-function 'spacemacs/gne-next)))

(defun spacemacs//gne-init-helm-grep ()
  "Generalized `next-error' interface with `helm-grep' integration."
  (require 'helm-grep)
  (with-current-buffer "*hgrep*"
    (setq spacemacs--gne-min-line 5
          spacemacs--gne-max-line
          (save-excursion
            (goto-char (point-max))
            (forward-line -1)
            (line-number-at-pos))
          spacemacs--gne-line-func 'helm-grep-action
          next-error-function 'spacemacs/gne-next)))

;;;; Helm Themes

(defun spacemacs/helm-themes ()
  "Theme selection with helm interface.
Wrapper of `helm-themes' that removes limit on number of candidates."
  (interactive)
  (require 'helm-themes)
  (let (helm-candidate-number-limit)
    (helm-themes)))

;;;; Helm Buffers List

(defun spacemacs/helm-buffers-list-unfiltered ()
  "Helm buffers without filtering."
  (interactive)
  (require 'helm-buffers)
  (let ((helm-boring-buffer-regexp-list nil))
    (call-interactively #'helm-buffers-list)))

;;;; Helm Command Search

(defun spacemacs/helm-M-x-fuzzy-matching ()
  "Helm \\[helm-M-x-fuzzy-matching] with fuzzy matching enabled."
  (interactive)
  (require 'helm-command)
  (let ((completion-styles completion-styles))
    (add-to-list 'completion-styles
                 `,(if (version< emacs-version "27") 'helm-flex 'flex) t)
    (call-interactively 'helm-M-x)))

;;; funcs.el ends here
