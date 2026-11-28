;;; funcs.el --- Helm Layer functions File for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
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
              (lambda (thing &optional no-properties)
                (let ((res (if (region-active-p)
                               (buffer-substring-no-properties
                                (region-beginning) (region-end))
                             (this-fn thing no-properties))))
                  (when res (rxt-quote-pcre res))))))
    (funcall func dir)))

(defun spacemacs//helm-do-search-find-tool (base tools default-inputp)
  "Create a cond form given a TOOLS string list and evaluate it."
  (eval
   `(cond
     ,@(mapcar
        (lambda (x)
          `((executable-find ,x t)
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

;; Search in current buffer using helm-occur (replaces helm-swoop) -------------

(defun spacemacs/helm-occur ()
  "Search lines in the current buffer matching a regexp with `helm-occur'.

This is the maintained replacement of the obsolete `helm-swoop'.  It
uses Helm to collect the lines matching a regexp, the same kind of
search performed by the Emacs builtin `occur'."
  (interactive)
  (require 'helm-occur)
  (helm-occur))

(defun spacemacs/helm-occur-region-or-symbol ()
  "Search the current buffer with `helm-occur' using the region or symbol."
  (interactive)
  (spacemacs/helm-occur))

(defun spacemacs/helm-multi-occur ()
  "Search lines in multiple buffers with `helm-multi-occur'.

Prompts for the buffers to search, then runs helm's builtin
`helm-multi-occur-1' across the selected buffers.  This is the
maintained replacement for the obsolete `helm-multi-swoop'."
  (interactive)
  (require 'helm-occur)
  (require 'helm-buffers)
  (let ((buf-names (helm-comp-read
                    "Buffers: "
                    (helm-skip-boring-buffers
                     (helm-buffer-list-1 nil) nil)
                    :name "Occur in buffer(s)"
                    :marked-candidates t
                    :must-match t
                    :buffer "*helm occur buffer list*")))
    (helm-multi-occur-1 (mapcar 'get-buffer buf-names))))

(defun spacemacs/helm-multi-occur-all ()
  "Search lines in all open buffers with `helm-multi-occur'.

Searches all non-boring current buffers (excluding helm buffers,
minibuffers, hidden windows, etc.).  This is the maintained
replacement for the obsolete `helm-multi-swoop-all'."
  (interactive)
  (require 'helm-occur)
  (require 'helm-buffers)
  (let* ((names (helm-skip-boring-buffers
                 (helm-buffer-list-1 nil) nil))
         (buffers (mapcar 'get-buffer names)))
    (helm-multi-occur-1 buffers)))

(defun spacemacs//helm-occur-normalize-buffers (mbufs)
  "Return a clean list of live buffer objects from MBUFS.

MBUFS may contain buffer objects, buffer names, or nils.  Return the
list restricted to live buffer objects, dropping everything else."
  (delq nil (mapcar (lambda (b)
                      (let ((bb (if (stringp b) (get-buffer b) b)))
                        (and (bufferp bb) (buffer-live-p bb) bb)))
                    mbufs)))

(defun spacemacs/helm-occur-edit ()
  "Edit the `helm-occur' results in an editable `*Occur*' buffer.

This replaces the obsolete helm-swoop edit workflow.  It exits the
helm session, runs the Emacs builtin `occur' command with the current
search pattern over every buffer that was searched (a single buffer for
`helm-occur', several buffers for `helm-multi-occur') to create a
standard `*Occur*' buffer, and enables `occur-edit-mode' so that edits
to the occurrences are written back to their originating buffer.

Use \\[occur-cease-edit] or the Spacemacs leader binding (`,c' / `,,')
to return to plain Occur mode."
  (interactive)
  (require 'helm-occur)
  (require 'replace)
  ;; Capture the target buffers and the current pattern while the helm
  ;; session is still alive.  funcs.el is loaded with dynamic binding
  ;; (lexical-binding is nil) so a closure cannot capture these `let*'
  ;; variables; we therefore pass them as ARGS through
  ;; `helm-run-after-exit', which are evaluated before exiting helm and
  ;; handed to the helper function after the session has quit.
  ;;
  ;; Every moccur source (both the single-buffer `helm-occur' and the
  ;; multi-buffer `helm-multi-occur' sessions) carries the
  ;; `moccur-buffers' attribute holding the full list of buffers that
  ;; were searched.  We collect all of them so the editable `*Occur*'
  ;; buffer contains occurrences from every searched buffer, exactly like
  ;; `helm-multi-swoop-edit' does, instead of only the source under
  ;; point (the old `buffer-name' approach dropped other buffers' matches).
  (let* ((mbufs (helm-get-attr 'moccur-buffers))
         (bufs (if (and mbufs (listp mbufs))
                   (spacemacs//helm-occur-normalize-buffers mbufs)
                 ;; Fall back to the source's single buffer.
                 (let ((bn (helm-get-attr 'buffer-name)))
                   (and bn (list (get-buffer bn))))))
         (pattern (and (boundp 'helm-input)
                       (substring-no-properties helm-input))))
    (helm-run-after-exit
     #'spacemacs//helm-occur-edit-finish bufs pattern)))

(defun spacemacs//helm-occur-edit-finish (bufs pattern)
  "Create an editable `*Occur*' buffer for BUFS with PATTERN.

Called after exiting the helm session by `spacemacs/helm-occur-edit'.
Runs the builtin `occur' on BUFS (a list of one or more buffers) then
enables `occur-edit-mode' so edits are written back to the originating
buffer of each occurrence."
  (let ((bufs (spacemacs//helm-occur-normalize-buffers bufs)))
    (if (or (null pattern) (string-empty-p pattern) (null bufs))
        (message "helm-occur: no active occur search to edit")
      ;; When several buffers were searched, run the builtin
      ;; `multi-occur'; otherwise `occur'.  Both produce a `*Occur*'
      ;; buffer whose lines carry `occur-target' markers that let
      ;; `occur-edit-mode' write each edit back to the correct buffer.
      (if (cdr bufs)
          (multi-occur bufs pattern)
        (with-current-buffer (car bufs)
          (occur pattern)))
      (let ((obuf (get-buffer "*Occur*")))
        (when obuf
          (switch-to-buffer obuf)
          (occur-edit-mode)
          ;; Place point on the first occurrence so the user can start
          ;; editing right away.
          (goto-char (point-min))
          (when (re-search-forward "^[[:space:]]*[0-9]+:" nil t)
            (goto-char (match-end 0))))))))

;; Bind a `C-c C-e' operation on `helm-occur-map' that opens an editable
;; `*Occur*' buffer (the Emacs builtin `occur' + `occur-edit-mode').  This is
;; the maintained replacement for the removed obsolete helm-swoop edit
;; workflow.  The binding is set up with a top-level `with-eval-after-load'
;; so it is reliably installed when the helm layer is set up, regardless of
;; whether the helm package's `:config' block has run yet.
(with-eval-after-load 'helm-occur
  (define-key helm-occur-map (kbd "C-c C-e") 'spacemacs/helm-occur-edit))

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

(defun spacemacs/helm-buffers-do-rg (&optional _)
  "Search in opened buffers with `rg'."
  (interactive)
  ;; --line-number forces line numbers (disabled by default on windows)
  ;; no --vimgrep because it adds column numbers that wgrep can't handle
  ;; see https://github.com/syl20bnr/spacemacs/pull/8065
  (let ((helm-ag-base-command "rg --smart-case --no-heading --color=never --line-number --max-columns=150")
        (helm-ag-success-exit-status '(0 2)))
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
  "Replace `helm-projectile-grep' to actually use `ag', `rg' etc.."
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
  (let* ((hist (and arg helm-ff-history (helm-find-files-history nil)))
         (default-input hist)
         (input (cond ((and (eq major-mode 'dired-mode) default-input)
                       (file-name-directory default-input))
                      ((and (not (string= default-input ""))
                            default-input))
                      (t (expand-file-name (helm-current-directory))))))
    (set-text-properties 0 (length input) nil input)
    (helm-find-files-1 input)))

(defun spacemacs/helm-git-grep ()
  "Search for a pattern in the Git repository of the current buffer."
  (interactive)
  (helm-grep-git-1 "" t nil ""))

(defun spacemacs/helm-git-grep-at-point ()
  "Search for the symbol at point in the Git repository of the current buffer."
  (interactive)
  (helm-grep-git-1 "" t))

 ;; Key bindings

(defmacro spacemacs||set-helm-key (keys func)
  "Define a key bindings for FUNC using KEYS.
Ensure that helm is required before calling FUNC."
  (let* ((actual-func (if (consp func) (cdr func) func))
         (func-name (intern (format "lazy-helm/%s" (symbol-name actual-func))))
         (func-param (if (consp func) `(,(car func) . ,func-name) func-name)))
    `(progn
       (defun ,func-name ()
         ,(format "Wrapper to ensure that `helm' is loaded before calling %s."
                  (symbol-name actual-func))
         (interactive)
         (require 'helm)
         (command-execute ',actual-func))
       (spacemacs/set-leader-keys ,keys ',func-param))))
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
             (cl-incf num-buffers-placed))))

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
(defun spacemacs//helm-themes-load (theme)
  "Disable the `custom-enabled-themes'first then load the named THEME."
  (mapc 'disable-theme custom-enabled-themes)
  (if (string= theme "default")
      t
    (load-theme (intern theme) t)))

(defun spacemacs//helm-themes-candidates ()
  "Return list of available themes with `default' on the head."
  (cons 'default (custom-available-themes)))

(defun spacemacs/helm-themes ()
  "List the theme candidates without number limit."
  (interactive)
  (let* (helm-candidate-number-limit
         (get-theme (lambda () (or (car-safe custom-enabled-themes) 'default)))
         (orig-theme (funcall get-theme)))
    (unwind-protect
        (unless (helm :prompt (format "pattern (current theme: %s): " orig-theme)
                      :preselect (format "%s$" orig-theme)
                      :sources (helm-build-sync-source "Selection Theme"
                                 :candidates 'spacemacs//helm-themes-candidates
                                 :action 'spacemacs//helm-themes-load
                                 :persistent-action 'spacemacs//helm-themes-load)
                      :buffer "*helm-themes*")
          (unless (eq orig-theme (funcall get-theme))
            (spacemacs//helm-themes-load (symbol-name orig-theme)))))))

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
    (add-to-list 'completion-styles 'flex t)
    (call-interactively 'helm-M-x)))
