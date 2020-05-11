;;; funcs.el --- Helm Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3



(defvar spacemacs--helm-popwin-mode nil
  "Temp variable to store `popwin-mode''s value.")

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
  ;; we must enable popwin-mode first then restore `display-buffer-alist'
  ;; Otherwise, popwin keeps adding up its own buffers to
  ;; `display-buffer-alist' and could slow down Emacs as the list grows
  (when spacemacs--helm-popwin-mode
    (popwin-mode))
  (setq display-buffer-alist spacemacs-display-buffer-alist))


;; REPLs integration

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




;; Search tools integration

(defun spacemacs//helm-do-ag-region-or-symbol (func &optional dir)
  "Search with `ag' with a default input."
  (require 'helm-ag)
  (cl-letf* (((symbol-value 'helm-ag-insert-at-point) 'symbol)
             ;; make thing-at-point choosing the active region first
             ((symbol-function 'this-fn) (symbol-function 'thing-at-point))
             ((symbol-function 'thing-at-point)
              (lambda (thing)
                (let ((res (if (region-active-p)
                               (buffer-substring-no-properties
                                (region-beginning) (region-end))
                             (this-fn thing))))
                  (when res (rxt-quote-pcre res))))))
    (funcall func dir)))

(defun spacemacs//helm-do-search-find-tool (base tools default-inputp)
  "Create a cond form given a TOOLS string list and evaluate it."
  (eval
   `(cond
     ,@(mapcar
        (lambda (x)
          `((executable-find ,x)
            ',(let ((func
                     (intern
                      (format (if default-inputp
                                  "spacemacs/%s-%s-region-or-symbol"
                                "spacemacs/%s-%s")
                              base x))))
                (if (fboundp func)
                    func
                  (intern (format "%s-%s"  base x))))))
        tools)
     (t 'helm-do-grep))))

;; Search in current file ----------------------------------------------

(defun spacemacs/helm-file-do-ag (&optional _)
  "Wrapper to execute `helm-ag-this-file.'"
  (interactive)
  (helm-do-ag-this-file))

(defun spacemacs/helm-file-do-ag-region-or-symbol ()
  "Search in current file with `ag' using a default input."
  (interactive)
  (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-file-do-ag))

(defun spacemacs/helm-file-smart-do-search (&optional default-inputp)
  "Search in current file using `dotspacemacs-search-tools'.
 Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
 are used as default input."
  (interactive)
  (call-interactively
   (spacemacs//helm-do-search-find-tool "helm-file-do"
                                        dotspacemacs-search-tools
                                        default-inputp)))

(defun spacemacs/helm-file-smart-do-search-region-or-symbol ()
  "Search in current file using `dotspacemacs-search-tools' with
 default input.
 Search for a search tool in the order provided by `dotspacemacs-search-tools'."
  (interactive)
  (spacemacs/helm-file-smart-do-search t))

;; Search in files -----------------------------------------------------

(defun spacemacs/helm-files-do-ag (&optional dir)
  "Search in files with `ag' using a default input."
  (interactive)
  (helm-do-ag dir))

(defun spacemacs/helm-files-do-ag-region-or-symbol ()
  "Search in files with `ag' using a default input."
  (interactive)
  (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-ag))

(defun spacemacs/helm-files-do-ack (&optional dir)
  "Search in files with `ack'."
  (interactive)
  (let ((helm-ag-base-command "ack --nocolor --nogroup"))
    (helm-do-ag dir)))

(defun spacemacs/helm-files-do-ack-region-or-symbol ()
  "Search in files with `ack' using a default input."
  (interactive)
  (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-ack))

(defun spacemacs/helm-files-do-pt (&optional dir)
  "Search in files with `pt'."
  (interactive)
  (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
    (helm-do-ag dir)))

(defun spacemacs/helm-files-do-pt-region-or-symbol ()
  "Search in files with `pt' using a default input."
  (interactive)
  (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-pt))

(defun spacemacs/helm-files-do-rg (&optional dir)
  "Search in files with `rg'."
  (interactive)
  ;; --line-number forces line numbers (disabled by default on windows)
  ;; no --vimgrep because it adds column numbers that wgrep can't handle
  ;; see https://github.com/syl20bnr/spacemacs/pull/8065
  (let* ((root-helm-ag-base-command "rg --smart-case --no-heading --color=never --line-number")
         (helm-ag-base-command (if spacemacs-helm-rg-max-column-number
                                   (concat root-helm-ag-base-command " --max-columns=" (number-to-string spacemacs-helm-rg-max-column-number))
                                 root-helm-ag-base-command)))
    (helm-do-ag dir)))

(defun spacemacs/helm-files-do-rg-region-or-symbol ()
  "Search in files with `rg' using a default input."
  (interactive)
  (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-rg))

(defun spacemacs/helm-files-smart-do-search (&optional default-inputp)
  "Search in files using `dotspacemacs-search-tools'.
 Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
 are used as default input."
  (interactive)
  (call-interactively
   (spacemacs//helm-do-search-find-tool "helm-files-do"
                                        dotspacemacs-search-tools
                                        default-inputp)))

(defun spacemacs/helm-files-smart-do-search-region-or-symbol ()
  "Search in files using `dotspacemacs-search-tools' with default input.
 Search for a search tool in the order provided by `dotspacemacs-search-tools'."
  (interactive)
  (spacemacs/helm-files-smart-do-search t))

;; Search in current dir -----------------------------------------------

(defun spacemacs/helm-dir-do-ag ()
  "Search in current directory with `ag'."
  (interactive)
  (spacemacs/helm-files-do-ag default-directory))

(defun spacemacs/helm-dir-do-ag-region-or-symbol ()
  "Search in current directory with `ag' with a default input."
  (interactive)
  (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-ag default-directory))

(defun spacemacs/helm-dir-do-ack ()
  "Search in current directory with `ack'."
  (interactive)
  (spacemacs/helm-files-do-ack default-directory))

(defun spacemacs/helm-dir-do-grep ()
  "Search in current directory with `grep'."
  (interactive)
  (spacemacs//helm-do-grep-region-or-symbol (list default-directory) nil))

(defun spacemacs/helm-dir-do-ack-region-or-symbol ()
  "Search in current directory with `ack' with a default input."
  (interactive)
  (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-ack default-directory))

(defun spacemacs/helm-dir-do-pt ()
  "Search in current directory with `pt'."
  (interactive)
  (spacemacs/helm-files-do-pt default-directory))

(defun spacemacs/helm-dir-do-pt-region-or-symbol ()
  "Search in current directory with `pt' with a default input."
  (interactive)
  (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-pt default-directory))

(defun spacemacs/helm-dir-do-rg ()
  "Search in current directory with `rg'."
  (interactive)
  (spacemacs/helm-files-do-rg default-directory))

(defun spacemacs/helm-dir-do-rg-region-or-symbol ()
  "Search in current directory with `rg' with a default input."
  (interactive)
  (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-rg default-directory))

(defun spacemacs/helm-dir-smart-do-search (&optional default-inputp)
  "Search in current directory using `dotspacemacs-search-tools'.
 Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
 are used as default input."
  (interactive)
  (call-interactively
   (spacemacs//helm-do-search-find-tool "helm-dir-do"
                                        dotspacemacs-search-tools
                                        default-inputp)))

(defun spacemacs/helm-dir-smart-do-search-region-or-symbol ()
  "Search in current directory using `dotspacemacs-search-tools'.
 with default input.
 Search for a search tool in the order provided by `dotspacemacs-search-tools'."
  (interactive)
  (spacemacs/helm-dir-smart-do-search t))

;; Search in buffers ---------------------------------------------------

(defun spacemacs/helm-buffers-do-ag (&optional _)
  "Wrapper to execute `helm-ag-buffers.'"
  (interactive)
  (helm-do-ag-buffers))

(defun spacemacs/helm-buffers-do-ag-region-or-symbol ()
  "Search in opened buffers with `ag' with a default input."
  (interactive)
  (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-ag))

(defun spacemacs/helm-buffers-do-ack (&optional _)
  "Search in opened buffers with `ack'."
  (interactive)
  (let ((helm-ag-base-command "ack --nocolor --nogroup"))
    (helm-do-ag-buffers)))

(defun spacemacs/helm-buffers-do-ack-region-or-symbol ()
  "Search in opened buffers with `ack' with a default input."
  (interactive)
  (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-ack))

(defun spacemacs/helm-buffers-do-pt (&optional _)
  "Search in opened buffers with `pt'."
  (interactive)
  (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
    (helm-do-ag-buffers)))

(defun spacemacs/helm-buffers-do-pt-region-or-symbol ()
  "Search in opened buffers with `pt' using a default input."
  (interactive)
  (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-pt))

(defun spacemacs/helm-buffers-do-rg (&optional _)
  "Search in opened buffers with `rg'."
  (interactive)
  ;; --line-number forces line numbers (disabled by default on windows)
  ;; no --vimgrep because it adds column numbers that wgrep can't handle
  ;; see https://github.com/syl20bnr/spacemacs/pull/8065
  (let ((helm-ag-base-command "rg --smart-case --no-heading --color=never --line-number --max-columns=150"))
    (helm-do-ag-buffers)))

(defun spacemacs/helm-buffers-do-rg-region-or-symbol ()
  "Search in opened buffers with `rg' using a default input."
  (interactive)
  (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-rg))

(defun spacemacs/helm-buffers-smart-do-search (&optional default-inputp)
  "Search in opened buffers using `dotspacemacs-search-tools'.
 Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
 are used as default input."
  (interactive)
  (call-interactively
   (spacemacs//helm-do-search-find-tool "helm-buffers-do"
                                        dotspacemacs-search-tools
                                        default-inputp)))

(defun spacemacs/helm-buffers-smart-do-search-region-or-symbol ()
  "Search in opened buffers using `dotspacemacs-search-tools' with
 default input.
 Search for a search tool in the order provided by `dotspacemacs-search-tools'."
  (interactive)
  (spacemacs/helm-buffers-smart-do-search t))

;; Search in project ---------------------------------------------------

(defun spacemacs/helm-project-smart-do-search-in-dir (dir)
  (interactive)
  (let ((default-directory dir))
    (spacemacs/helm-project-smart-do-search)))

(defun spacemacs/helm-projectile-grep ()
  "Replace `helm-projectile-grep' to actually use `ag', `pt' etc.."
  (interactive)
  (helm-exit-and-execute-action
   'spacemacs/helm-project-smart-do-search-in-dir))

(defun spacemacs/helm-project-do-ag ()
  "Search in current project with `ag'."
  (interactive)
  (let ((dir (projectile-project-root)))
    (if dir
        (helm-do-ag dir)
      (message "error: Not in a project."))))

(defun spacemacs/helm-project-do-ag-region-or-symbol ()
  "Search in current project with `ag' using a default input."
  (interactive)
  (let ((dir (projectile-project-root)))
    (if dir
        (spacemacs//helm-do-ag-region-or-symbol 'helm-do-ag dir)
      (message "error: Not in a project."))))

(defun spacemacs/helm-project-do-ack ()
  "Search in current project with `ack'."
  (interactive)
  (let ((dir (projectile-project-root)))
    (if dir
        (spacemacs/helm-files-do-ack dir)
      (message "error: Not in a project."))))

(defun spacemacs/helm-project-do-ack-region-or-symbol ()
  "Search in current project with `ack' using a default input."
  (interactive)
  (let ((dir (projectile-project-root)))
    (if dir
        (spacemacs//helm-do-ag-region-or-symbol
         'spacemacs/helm-files-do-ack dir)
      (message "error: Not in a project."))))

(defun spacemacs/helm-project-do-pt ()
  "Search in current project with `pt'."
  (interactive)
  (let ((dir (projectile-project-root)))
    (if dir
        (spacemacs/helm-files-do-pt dir)
      (message "error: Not in a project."))))

(defun spacemacs/helm-project-do-pt-region-or-symbol ()
  "Search in current project with `pt' using a default input."
  (interactive)
  (let ((dir (projectile-project-root)))
    (if dir
        (spacemacs//helm-do-ag-region-or-symbol
         'spacemacs/helm-files-do-pt dir)
      (message "error: Not in a project."))))

(defun spacemacs/helm-project-do-rg ()
  "Search in current project with `rg'."
  (interactive)
  (let ((dir (projectile-project-root)))
    (if dir
        (spacemacs/helm-files-do-rg dir)
      (message "error: Not in a project."))))

(defun spacemacs/helm-project-do-rg-region-or-symbol ()
  "Search in current project with `rg' using a default input."
  (interactive)
  (let ((dir (projectile-project-root)))
    (if dir
        (spacemacs//helm-do-ag-region-or-symbol
         'spacemacs/helm-files-do-rg dir)
      (message "error: Not in a project."))))

(defun spacemacs/helm-project-smart-do-search (&optional default-inputp)
  "Search in current project using `dotspacemacs-search-tools'.
 Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
 are used as default input."
  (interactive)
  (let ((projectile-require-project-root nil))
    (call-interactively
     (spacemacs//helm-do-search-find-tool "helm-project-do"
                                          dotspacemacs-search-tools
                                          default-inputp))))

(defun spacemacs/helm-project-smart-do-search-region-or-symbol ()
  "Search in current project using `dotspacemacs-search-tools' with
 default input.
 Search for a search tool in the order provided by `dotspacemacs-search-tools'."
  (interactive)
  (spacemacs/helm-project-smart-do-search t))

;; grep

(defun spacemacs//helm-do-grep-region-or-symbol
    (&optional targs use-region-or-symbol-p)
  "Version of `helm-do-grep' with a default input."
  (interactive)
  (require 'helm)
  (cl-letf*
      (((symbol-function 'this-fn) (symbol-function 'helm-do-grep-1))
       ((symbol-function 'helm-do-grep-1)
        (lambda (targets &optional recurse zgrep exts
                         default-input region-or-symbol-p)
          (let* ((new-input (when region-or-symbol-p
                              (if (region-active-p)
                                  (buffer-substring-no-properties
                                   (region-beginning) (region-end))
                                (thing-at-point 'symbol t))))
                 (quoted-input (when new-input
                                 (rxt-quote-pcre new-input))))
            (this-fn targets recurse zgrep exts
                     default-input quoted-input))))
       (preselection (or (dired-get-filename nil t)
                         (buffer-file-name (current-buffer))))
       (targets   (if targs
                      targs
                    (helm-read-file-name
                     "Search in file(s): "
                     :marked-candidates t
                     :preselect (when preselection
                                  (if helm-ff-transformer-show-only-basename
                                      (helm-basename preselection)
                                    preselection))))))
    (helm-do-grep-1 targets nil nil nil nil use-region-or-symbol-p)))

(defun spacemacs/helm-file-do-grep ()
  "Search in current file with `grep' using a default input."
  (interactive)
  (spacemacs//helm-do-grep-region-or-symbol
   (list (buffer-file-name (current-buffer))) nil))

(defun spacemacs/helm-file-do-grep-region-or-symbol ()
  "Search in current file with `grep' using a default input."
  (interactive)
  (spacemacs//helm-do-grep-region-or-symbol
   (list (buffer-file-name (current-buffer))) t))

(defun spacemacs/helm-files-do-grep ()
  "Search in files with `grep'."
  (interactive)
  (spacemacs//helm-do-grep-region-or-symbol nil nil))

(defun spacemacs/helm-files-do-grep-region-or-symbol ()
  "Search in files with `grep' using a default input."
  (interactive)
  (spacemacs//helm-do-grep-region-or-symbol nil t))

(defun spacemacs/helm-buffers-do-grep ()
  "Search in opened buffers with `grep'."
  (interactive)
  (let ((buffers (cl-loop for buffer in (buffer-list)
                          when (buffer-file-name buffer)
                          collect (buffer-file-name buffer))))
    (spacemacs//helm-do-grep-region-or-symbol buffers nil)))

(defun spacemacs/helm-buffers-do-grep-region-or-symbol ()
  "Search in opened buffers with `grep' with a default input."
  (interactive)
  (let ((buffers (cl-loop for buffer in (buffer-list)
                          when (buffer-file-name buffer)
                          collect (buffer-file-name buffer))))
    (spacemacs//helm-do-grep-region-or-symbol buffers t)))

(defun spacemacs/resume-last-search-buffer ()
  "open last helm-ag or hgrep buffer."
  (interactive)
  (cond ((get-buffer "*helm ag results*")
         (switch-to-buffer-other-window "*helm ag results*"))
        ((get-buffer "*helm-ag*")
         (helm-resume "*helm-ag*"))
        ((get-buffer "*hgrep*")
         (switch-to-buffer-other-window "*hgrep*"))
        (t
         (message "No previous search buffer found"))))

(defun spacemacs/helm-find-files (arg)
  "Custom spacemacs implementation for calling helm-find-files-1.
Removes the automatic guessing of the initial value based on thing at point. "
  (interactive "P")
  ;; fixes #10882 and #11270
  (require 'helm-files)
  (let* ((hist (and arg helm-ff-history (helm-find-files-history)))
         (default-input hist)
         (input (cond ((and (eq major-mode 'dired-mode) default-input)
                       (file-name-directory default-input))
                      ((and (not (string= default-input ""))
                            default-input))
                      (t (expand-file-name (helm-current-directory))))))
    (set-text-properties 0 (length input) nil input)
    (helm-find-files-1 input)))

 ;; Key bindings

(defmacro spacemacs||set-helm-key (keys func)
  "Define a key bindings for FUNC using KEYS.
Ensure that helm is required before calling FUNC."
  (let ((func-name (intern (format "lazy-helm/%s" (symbol-name func)))))
    `(progn
       (defun ,func-name ()
         ,(format "Wrapper to ensure that `helm' is loaded before calling %s."
                  (symbol-name func))
         (interactive)
         (require 'helm)
         (call-interactively ',func))
       (spacemacs/set-leader-keys ,keys ',func-name))))

 ;; Find files tweaks

(defun spacemacs//helm-find-files-edit (candidate)
  "Opens a dired buffer and immediately switches to editable mode."
  (dired (file-name-directory candidate))
  (dired-goto-file candidate)
  (dired-toggle-read-only))

(defun spacemacs/helm-find-files-edit ()
  "Exits helm, opens a dired buffer and immediately switches to editable mode."
  (interactive)
  (helm-exit-and-execute-action 'spacemacs//helm-find-files-edit))

(defun spacemacs/helm-jump-in-buffer ()
  "Jump in buffer using `imenu' facilities and helm."
  (interactive)
  (call-interactively
   (cond
    ((eq major-mode 'org-mode) 'helm-org-in-buffer-headings)
    (t 'helm-semantic-or-imenu))))

(defun spacemacs//helm-open-buffers-in-windows (buffers)
  "This function allows a different default action, on marking multiple
candidate buffers/files for helm. By default, helm either opens all
files/buffers in the same window, or creates splits. This function instead
opens the buffers (or files) across different already-open windows. The first
selected buffer is opened in the current window, the next is opened in the
window with higher number, etc. This will make a loop around, so with 4
windows, and window 2 active, opening 4 buffers will open them in windows
2 3 4 1. If more buffers are opened than windows available, the remainder are
not set to any window (but in the case of files, they are still opened
to buffers)."
  (let ((num-buffers (length buffers))
        (num-windows (length (winum--window-list)))
        (cur-win (or (winum-get-number) (winum-get-number (other-window 1))))
        (num-buffers-placed 0))
    (cl-loop for buffer in buffers do
             (when (>= num-buffers-placed num-windows) (cl-return))
             (set-window-buffer (winum-get-window-by-number cur-win) buffer)
             (setq cur-win (+ 1 (mod cur-win num-windows)))
             (incf num-buffers-placed))))

(defun spacemacs/helm-find-buffers-windows ()
  (interactive)
  (helm-exit-and-execute-action
   (lambda (candidate)
     (spacemacs//helm-open-buffers-in-windows (helm-marked-candidates)))))

(defun spacemacs/helm-find-files-windows ()
  (interactive)
  (helm-exit-and-execute-action
   (lambda (candidate)
     (let* ((files (helm-marked-candidates))
            (buffers (mapcar 'find-file-noselect files)))
       (spacemacs//helm-open-buffers-in-windows buffers)))))


;; Generalized next-error interface

(defun spacemacs//gne-init-helm-ag (&rest args)
  (with-current-buffer "*helm ag results*"
    (setq spacemacs--gne-min-line 5
          spacemacs--gne-max-line (save-excursion
                                    (goto-char (point-max))
                                    (previous-line)
                                    (line-number-at-pos))
          spacemacs--gne-line-func
          (lambda (c)
            (helm-ag--find-file-action
             c 'find-file helm-ag--search-this-file-p))
          next-error-function 'spacemacs/gne-next)))

(defun spacemacs//gne-init-helm-grep (&rest args)
  (with-current-buffer "*hgrep*"
    (setq spacemacs--gne-min-line 5
          spacemacs--gne-max-line
          (save-excursion
            (goto-char (point-max))
            (previous-line)
            (line-number-at-pos))
          spacemacs--gne-line-func 'helm-grep-action
          next-error-function 'spacemacs/gne-next)))


;; theme

(defun spacemacs/helm-themes ()
  "Remove limit on number of candidates on `helm-themes'"
  (interactive)
  (let (helm-candidate-number-limit)
    (helm-themes)))

;; Buffers ---------------------------------------------------------------------

(defun spacemacs/helm-buffers-list-unfiltered ()
  "Helm buffers without filtering."
  (interactive)
  (let ((helm-boring-buffer-regexp-list nil))
    (call-interactively #'helm-buffers-list)))

;; Command search ---------------------------------------------------------------------

(defun spacemacs/helm-M-x-fuzzy-matching ()
  "Helm M-x with fuzzy matching enabled"
  (interactive)
  (let ((completion-styles completion-styles))
    (add-to-list 'completion-styles `,(if (version< emacs-version "27") 'helm-flex 'flex) t)
    (command-execute 'helm-M-x)))
