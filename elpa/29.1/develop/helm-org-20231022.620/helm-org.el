;;; helm-org.el --- Helm for org headlines and keywords completion -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2019 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; Author:      Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; URL: https://github.com/emacs-helm/helm-org
;; Package-Requires: ((helm "3.3") (emacs "24.4"))
;; Version: 1.0

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


;;; Commentary:
;; 
;; Helm for org headlines and keywords completion

;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'helm-utils)
(require 'org)

(defvar helm-completing-read-handlers-alist)

;; Internals
(defvar helm-org--headers-cache nil)
(defvar helm-org--buffer-tick nil)
(defvar helm-org--force-refresh nil
  "[INTERNAL] Force refreshing caches when non nil.")

;; Menu
;;;###autoload
(progn
  (require 'helm-easymenu)
  (easy-menu-add-item
   nil '("Tools" "Helm")
   '("Org"
     ["Org headlines in org agenda files" helm-org-agenda-files-headings t]
     ["Org headlines in buffer" helm-org-in-buffer-headings t])
   "Elpa"))


;; Load org-with-point-at macro when compiling
(eval-when-compile
  (require 'org-macs))

(declare-function org-agenda-switch-to "org-agenda.el")

(defgroup helm-org nil
  "Org related functions for helm."
  :group 'helm)

(defcustom helm-org-headings-fontify nil
  "Fontify org buffers before parsing them.
This reflect fontification in `helm-buffer' when non--nil.
NOTE: This will be slow on large org buffers."
  :group 'helm-org
  :type 'boolean
  :set (lambda (var value)
         (set var value)
         (setq helm-org--force-refresh t)))

(defcustom helm-org-format-outline-path nil
  "Show all org level as path."
  :group 'helm-org
  :type 'boolean
  :set (lambda (var value)
         (set var value)
         (setq helm-org--force-refresh t)))

(defcustom helm-org-headings-min-depth 1
  "Minimum depth of org headings to start with."
  :group 'helm-org
  :type 'integer
  :set (lambda (var value)
         (set var value)
         (setq helm-org--force-refresh t)))

(defcustom helm-org-headings-max-depth 8
  "Go down to this maximum depth of org headings."
  :group 'helm-org
  :type 'integer
  :set (lambda (var value)
         (set var value)
         (setq helm-org--force-refresh t)))

(defcustom helm-org-headings-actions
  '(("Go to heading" . helm-org-goto-marker)
    ("Open in indirect buffer `C-c i'" . helm-org--open-heading-in-indirect-buffer)
    ("Refile heading(s) (marked-to-selected|current-to-selected) `C-c w`" . helm-org--refile-heading-to)
    ("Insert link to this heading `C-c l`" . helm-org-insert-link-to-heading-at-marker))
  "Default actions alist for `helm-source-org-headings-for-files'."
  :group 'helm-org
  :type '(alist :key-type string :value-type function))

(defcustom helm-org-truncate-lines t
  "Truncate org-header-lines when non-nil."
  :type 'boolean
  :group 'helm-org)

(defcustom helm-org-ignore-autosaves nil
  "Ignore autosave files when starting `helm-org-agenda-files-headings'."
  :type 'boolean
  :group 'helm-org)

(defcustom helm-org-completion-styles '(helm)
  "A list of styles suitable for `completion-styles'."
  :group 'helm-org
  :type '(repeat symbol))


;;; Help
;;
(defvar helm-org-headings-help-message
  "* Helm Org headings

** Tips

*** Matching and completion-styles

In addition of multi matching like in all other helm commands, helm-org obey `completion-styles'
which allow having flex aka fuzzy matching, see [[Completion-styles][Completion-styles]].

*** Refiling

You can refile one or more headings at a time.

To refile one heading, move the point to the entry you want to refile and run
\\[helm-org-in-buffer-headings].  Then select the heading you want to refile to
and press \\<helm-org-headings-map>\\[helm-org-run-refile-heading-to] or select the refile action from the actions menu.

To refile multiple headings, run \\[helm-org-in-buffer-headings] and mark the
headings you want to refile.  Then select the heading you want to refile to
\(without marking it) and press \\<helm-org-headings-map>\\[helm-org-run-refile-heading-to] or select the refile action from the
actions menu.

*** Tags completion

Tags completion use `completing-read-multiple', perhaps have a
look at its docstring.

**** Single tag

From an org heading hit C-c C-c which provide a
\"Tags\" prompt, then hit TAB and RET if you want to enter an
existing tag or write a new tag in prompt.  At this point you end
up with an entry in your prompt, if you enter RET, the entry is
added as tag in your org header.

**** Multiple tags

If you want to add more tag to your org header, add a separator[1] after
your tag and write a new tag or hit TAB to find another existing
tag, and so on until you have all the tags you want
e.g \"foo,bar,baz\" then press RET to finally add the tags to your
org header.
Note: [1] A separator can be a comma, a colon i.e. [,:] or a space.

** Commands
\\<helm-org-headings-map>
|Keys|Description
|-----------+----------|
|\\[helm-org-run-open-heading-in-indirect-buffer]|Open heading in indirect buffer.
|\\[helm-org-run-refile-heading-to]|Refile current or marked headings to selection.
|\\[helm-org-run-insert-link-to-heading-at-marker]|Insert link at point to selection."
  )

;;; Org capture templates
;;
;;
(defvar org-capture-templates)
(defun helm-source-org-capture-templates ()
  "Build source for org capture templates."
  (helm-build-sync-source "Org Capture Templates:"
    :candidates (cl-loop for template in org-capture-templates
                         collect (cons (nth 1 template) (nth 0 template)))
    :action '(("Do capture" . (lambda (template-shortcut)
                                (org-capture nil template-shortcut))))))

;;; Org headings
;;
;;
(defun helm-org-goto-marker (marker)
  "Go to MARKER in org buffer."
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker))
  (org-show-context)
  (re-search-backward "^\\*+ " nil t)
  (org-show-entry)
  (org-show-children))

(defun helm-org--open-heading-in-indirect-buffer (marker)
  "Open org heading at MARKER in indirect buffer."
  (helm-org-goto-marker marker)
  (org-tree-to-indirect-buffer)

  ;; Put the non-indirect buffer at the bottom of the prev-buffers
  ;; list so it won't be selected when the indirect buffer is killed
  (set-window-prev-buffers nil (append (cdr (window-prev-buffers))
                                       (car (window-prev-buffers)))))

(defun helm-org-run-open-heading-in-indirect-buffer ()
  "Open selected Org heading in an indirect buffer."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-org--open-heading-in-indirect-buffer)))
(put 'helm-org-run-open-heading-in-indirect-buffer 'helm-only t)

(defvar helm-org-headings-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c i") 'helm-org-run-open-heading-in-indirect-buffer)
    (define-key map (kbd "C-c w") 'helm-org-run-refile-heading-to)
    (define-key map (kbd "C-c l") 'helm-org-run-insert-link-to-heading-at-marker)
    map)
  "Keymap for `helm-source-org-headings-for-files'.")

(defun helm-org-build-sources (filenames &optional parents force-refresh)
  (unwind-protect
      (cl-loop for file in filenames
               for name = (if (bufferp file)
                              (buffer-name file)
                            (helm-basename file))
               collect
               (helm-build-sync-source (format "Org headings (%s)" name)
                 :candidates (helm-dynamic-completion
                              (helm-org--get-candidates-in-file
                               file
                               helm-org-headings-fontify
                               t
                               parents (or force-refresh
                                           helm-org--force-refresh))
                              'stringp
                              nil '(metadata (display-sort-function
                                              .
                                              (lambda (candidates)
                                                (sort candidates
                                                      #'helm-generic-sort-fn))))
                              nil helm-org-completion-styles)
                 :match-dynamic t
                 :find-file-target (lambda (source)
                                     (let ((marker (helm-get-selection nil nil source)))
                                       (buffer-file-name (marker-buffer marker))))
                 :filtered-candidate-transformer
                 #'helm-org-indent-headings
                 :action 'helm-org-headings-actions
                 :help-message 'helm-org-headings-help-message
                 :keymap helm-org-headings-map
                 :group 'helm-org))
    (setq helm-org--force-refresh nil)))

(defun helm-org--get-candidates-in-file (filename &optional fontify nofname parents force-refresh)
  "Get candidates for org FILENAME.
Fontify each heading when FONTIFY is specified.
Don't show filename when NOFNAME.
Get PARENTS as well when specified."
  (with-current-buffer (pcase filename
                         ((pred bufferp) filename)
                         ((pred stringp) (find-file-noselect filename t)))
    (let ((tick (buffer-chars-modified-tick)))
      (if (and helm-org--buffer-tick
               (= tick helm-org--buffer-tick)
               (null force-refresh))
          helm-org--headers-cache
        (message "Refreshing cache in `%s'..." (buffer-name))
        (set (make-local-variable 'helm-org--buffer-tick) tick)
        (prog1
            (set (make-local-variable 'helm-org--headers-cache)
                 (let ((match-fn (if fontify
                                     #'match-string
                                   #'match-string-no-properties))
                       (search-fn (lambda ()
                                    (re-search-forward
                                     org-complex-heading-regexp nil t)))
                       (file (unless (or (bufferp filename) nofname)
                               (concat (helm-basename filename) ":"))))
                   (when parents
                     (add-function :around (var search-fn)
                                   (lambda (old-fn &rest args)
                                     (when (org-up-heading-safe)
                                       (apply old-fn args)))))
                   (save-excursion
                     (save-restriction
                       (unless (and (bufferp filename)
                                    (buffer-base-buffer filename))
                         ;; Only widen direct buffers, not indirect ones.
                         (widen))
                       (unless parents (goto-char (point-min)))
                       ;; clear cache for new version of org-get-outline-path
                       (and (boundp 'org-outline-path-cache)
                            (setq org-outline-path-cache nil))
                       (cl-loop with width = (window-width (helm-window))
                                while (funcall search-fn)
                                for beg = (point-at-bol)
                                for end = (point-at-eol)
                                when (and fontify
                                          (null (text-property-any
                                                 beg end 'fontified t)))
                                do (jit-lock-fontify-now beg end)
                                for level = (length (match-string-no-properties 1))
                                for heading = (funcall match-fn 4)
                                if (and (>= level helm-org-headings-min-depth)
                                        (<= level helm-org-headings-max-depth))
                                collect (propertize
                                         (if helm-org-format-outline-path
                                             (org-format-outline-path
                                              ;; org-get-outline-path changed in signature and behaviour since org's
                                              ;; commit 105a4466971. Let's fall-back to the new version in case
                                              ;; of wrong-number-of-arguments error.
                                              (condition-case nil
                                                  (append (apply #'org-get-outline-path
                                                                 (unless parents
                                                                   (list t level heading)))
                                                          (list heading))
                                                (wrong-number-of-arguments
                                                 (org-get-outline-path t t)))
                                              width file)
                                           (if file
                                               (concat file (funcall match-fn  0))
                                             (funcall match-fn  0)))
                                         'helm-real-display heading
                                         'helm-realvalue (point-marker)))))))
          (message "Refreshing cache in `%s' done" (buffer-name)))))))

(defun helm-org-indent-headings (candidates _source)
  "Indent headings and hide leading stars displayed in the helm buffer.
If `org-startup-indented' and `org-hide-leading-stars' are nil, do
nothing to CANDIDATES."
  (cl-loop for disp in candidates collect
           (helm-org-indent-headings-1 disp)))

(defun helm-org-indent-headings-1 (candidate)
  (if helm-org-headings-fontify
      (if (string-match "^\\(\\**\\)\\(\\* \\)\\(.*\n?\\)" candidate)
          (replace-match "\\1\\2\\3" t nil candidate)
        candidate)
    (if (string-match "^\\(\\**\\)\\(\\* \\)\\(.*\n?\\)" candidate)
        (let ((foreground (org-find-invisible-foreground)))
          (with-helm-current-buffer
            (cond
             ;; org-startup-indented is t, and org-hide-leading-stars is t
             ;; Or: #+STARTUP: indent hidestars
             ((and org-startup-indented org-hide-leading-stars)
              (with-helm-buffer
                (require 'org-indent)
                (org-indent-mode 1)
                (replace-match
                 (format "%s\\2\\3"
                         (propertize (replace-match "\\1" t nil candidate)
                                     'face `(:foreground ,foreground)))
                 t nil candidate)))
             ;; org-startup-indented is nil, org-hide-leading-stars is t
             ;; Or: #+STARTUP: noindent hidestars
             ((and (not org-startup-indented) org-hide-leading-stars)
              (with-helm-buffer
                (replace-match
                 (format "%s\\2\\3"
                         (propertize (replace-match "\\1" t nil candidate)
                                     'face `(:foreground ,foreground)))
                 t nil candidate)))
             ;; org-startup-indented is nil, and org-hide-leading-stars is nil
             ;; Or: #+STARTUP: noindent showstars
             (t
              (with-helm-buffer
                (replace-match "\\1\\2\\3" t nil candidate))))))
      candidate)))

(defun helm-org-insert-link-to-heading-at-marker (marker)
  "Insert link to heading at MARKER position."
  (with-current-buffer (marker-buffer marker)
    (let ((heading-name (save-excursion (goto-char (marker-position marker))
                                        (nth 4 (org-heading-components))))
          (file-name (buffer-file-name)))
      (with-helm-current-buffer
        (org-insert-link
         file-name (concat "file:" file-name "::*" heading-name))))))

(defun helm-org-run-insert-link-to-heading-at-marker ()
  "Run interactively `helm-org-insert-link-to-heading-at-marker'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action
     'helm-org-insert-link-to-heading-at-marker)))

(defun helm-org--refile-heading-to (marker)
  "Refile headings to heading at MARKER.
If multiple candidates are marked in the Helm session, they will
all be refiled.  If no headings are marked, the selected heading
will be refiled."
  (let* ((victims (with-helm-buffer (helm-marked-candidates)))
         (buffer (marker-buffer marker))
         (filename (buffer-file-name buffer))
         ;; get the heading we refile to so org doesn't
         ;; output 'Refile to "nil" in file ...'
         (heading (with-current-buffer buffer
                    (org-with-point-at marker
                      (org-get-heading :no-tags :no-todo :no-priority :no-comment))))
         (rfloc (list heading filename nil marker)))
    (when (and (= 1 (length victims))
               (equal (helm-get-selection) (car victims)))
      ;; No candidates are marked; we are refiling the entry at point
      ;; to the selected heading
      (setq victims (list (point))))
    ;; Probably best to check that everything returned a value
    (when (and victims buffer filename rfloc)
      (cl-loop for victim in victims
               do (org-with-point-at victim
                    (org-refile nil nil rfloc))))))

(defun helm-org-in-buffer-preselect ()
  "Return the current or closest visible heading as a regexp string."
  (save-excursion
    (cond ((org-at-heading-p) (forward-line 0))
	  ((org-before-first-heading-p)
	   (outline-next-visible-heading 1))
	  (t (outline-previous-visible-heading 1)))
    (regexp-quote (buffer-substring-no-properties (point)
						  (point-at-eol)))))

(defun helm-org-run-refile-heading-to ()
  "Helm org refile heading action."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-org--refile-heading-to)))
(put 'helm-org-run-refile-heading-to 'helm-only t)

;;;###autoload
(defun helm-org-agenda-files-headings (&optional arg)
  "Preconfigured helm for org files headings."
  (interactive "P")
  (let ((autosaves (cl-loop for f in (org-agenda-files)
                            when (file-exists-p
                                  (expand-file-name
                                   (concat "#" (helm-basename f) "#")
                                   (helm-basedir f)))
                            collect (helm-basename f)))
        (files (org-agenda-files)))
    (when (or (null autosaves)
              helm-org-ignore-autosaves
              (y-or-n-p (format "%s have auto save data, continue? "
                                (mapconcat #'identity autosaves ", "))))
      (helm :sources (helm-org-build-sources files nil arg)
            :truncate-lines helm-org-truncate-lines
            :buffer "*helm org headings*"))))

;;;###autoload
(defun helm-org-in-buffer-headings (&optional arg)
  "Preconfigured helm for org buffer headings."
  (interactive "P")
  (let ((files (list (current-buffer))))
    (helm :sources (helm-org-build-sources files nil arg)
          :preselect (helm-org-in-buffer-preselect)
          :truncate-lines helm-org-truncate-lines
          :buffer "*helm org inbuffer*")))

;;;###autoload
(defun helm-org-parent-headings (&optional arg)
  "Preconfigured helm for org headings that are parents of the current heading."
  (interactive "P")
  ;; Use a large max-depth to ensure all parents are displayed.
  (let ((helm-org-headings-min-depth 1)
        (helm-org-headings-max-depth  50)
        (files (list (current-buffer))))
    (helm :sources (helm-org-build-sources files t arg)
          :truncate-lines helm-org-truncate-lines
          :buffer "*helm org parent headings*")))

;;;###autoload
(defun helm-org-capture-templates ()
  "Preconfigured helm for org templates."
  (interactive)
  (helm :sources (helm-source-org-capture-templates)
        :truncate-lines helm-org-truncate-lines
        :buffer "*helm org capture templates*"))

;;; Org tag completion

;; Based on code from Anders Johansson posted on 3 Mar 2016 at
;; <https://groups.google.com/d/msg/emacs-helm/tA6cn6TUdRY/G1S3TIdzBwAJ>

(defvar crm-separator)

;;;###autoload
(defun helm-org-completing-read-tags (prompt collection pred req initial
                                      hist def inherit-input-method _name _buffer)
  "Completing read function for Org tags.

This function is used as a `completing-read' function in
`helm-completing-read-handlers-alist' by `org-set-tags' and
`org-capture'.

NOTE: Org tag completion will work only if you disable org fast tag
selection, see (info \"(org) setting tags\")."
  (if (not (string= "Tags: " prompt))
      ;; Not a tags prompt.  Use normal completion by calling
      ;; `org-icompleting-read' again without this function in
      ;; `helm-completing-read-handlers-alist'
      (let ((helm-completing-read-handlers-alist
             (rassq-delete-all
              'helm-org-completing-read-tags
              (copy-alist helm-completing-read-handlers-alist))))
        (org-icompleting-read
         prompt collection pred req initial hist def inherit-input-method))
    ;; Tags prompt
    (let* ((curr (and (stringp initial)
                      (not (string= initial ""))
                      (org-split-string initial ":")))
           (table   (delete curr
                            (org-uniquify
                             (mapcar #'car org-last-tags-completion-table))))
           (crm-separator ":\\|,\\|\\s-"))
      (cl-letf (((symbol-function 'crm-complete-word)
                 'self-insert-command))
        (mapconcat #'identity
                   (completing-read-multiple
                    prompt table pred nil initial hist def)
                   ":")))))

(provide 'helm-org)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-org.el ends here
