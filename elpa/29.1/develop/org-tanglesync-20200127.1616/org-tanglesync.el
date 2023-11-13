;;; org-tanglesync.el --- Syncing org src blocks with tangled external files -*- lexical-binding: t; -*-

;; Copright (C) 2019 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-tanglesync.el
;; Keywords: outlines
;; Package-Requires: ((emacs "24.4"))
;; Version: 1.1

;;; Commentary:

;; Pulling external file changes to a tangled org-babel src block
;; is surprisingly not a well-implemented feature.  This addresses that.
;;
;; Any block that has :tangle <fname> will compare the block with
;; that external <fname>.  When a diff is detected, 1 of 5 actions
;; can occur:
;;
;;   1. External - <fname> contents will override the block contents
;;   2. Internal - block will keep the block contents
;;   3. Prompt - The user will be prompted to pull external changes
;;   4. Diff - A diff of the <fname> and block contents will be produced
;;   5. Custom - A user-defined function will be called instead.
;;
;; These 5 options can be set as the default action by changing the
;; `org-tanglesync-default-diff-action` custom parameter.  Otherwise
;; individual block actions can be set in the org src block header
;; e.g. `:diff external` for pulling external changes without
;; prompt into a specific block.
;;
;; This package also provides a hook when the org src block is
;; being edited (e.g. via `C-c '`) which asks the user if they
;; want to pull external changes if a difference is detected.
;; The user can bypass this and always pull by setting the
;; `org-tanglesync-skip-user-check` custom parameter.
;;
;; Since v1.1, a "watch" mode has been added that does not require you
;; to be within the org buffer to sync changes to the tangled blocks,
;; but can now instead modify directly from the external tangled buffer
;; and the changes will be synced back to the org file in the background.
;; The `org-tanglesync-watch-files` needs only to be set a list of org
;; files with tangled blocks, and `org-tanglesync-watch-mode` needs to
;; enabled globally.


;;; Code:
(require 'org)
(require 'diff)
(require 'cl-lib)
(require 'subr-x)

(defgroup org-tanglesync nil
  "Super group for syncing tangled files, either from the config file, or from the external buffers."
  :group 'emacs)

;; ---- Mode methods ----
(defcustom org-tanglesync-default-diff-action :prompt
  "Which default action to perform when a diff is detected between an internal block and the external file it is tangled to.  This is overridden by the ':diff <action>' header on the block."
  :type 'symbol
  :options '(:external  ;; always overwrites with external
             :internal  ;; always keep the internal block
             :prompt    ;; prompts the user to overwrite
             :diff      ;; performs a diff between two buffers
             :custom)   ;; performs a user action between buffers
  :group 'org-tanglesync)

(defcustom org-tanglesync-perform-custom-diff-hook nil
  "A user passed function for action on the internal and external buffer.
Only takes effect when :custom is set"
  :type 'hook
  :group 'org-tanglesync)

(defcustom org-tanglesync-skip-user-check nil
  "Pull changes from external file if different when launching org src code mode."
  :type 'logical
  :group 'org-tanglesync)

(defvar org-tanglesync-minor-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c M-i") 'org-tanglesync-process-buffer-interactive)
    m)
  "Keymap for function `org-tanglesync-minor-mode'.")

(define-minor-mode org-tanglesync-mode
  "Mode for syncing tangled org babel headers to their external files."
  nil
  " tanglesync"
  org-tanglesync-minor-mode-map
  (if org-tanglesync-mode
      (progn
        (add-hook 'org-src-mode-hook #'org-tanglesync-user-edit-buffer)
        (message "Use C-c M-i to interactively process the buffer."))
    (remove-hook 'org-src-mode-hook #'org-tanglesync-user-edit-buffer)))

(defun org-tanglesync-get-blockbody-buffer (block-info)
  "Extract the body of the code block from BLOCK-INFO to compare with the external file later."
  (let ((buff (get-buffer-create "*block-info*")))
    (with-current-buffer buff
      (erase-buffer)
      (insert (car (cdr block-info)))
      (insert "\n")
    buff)))

(defun org-tanglesync-get-header-property (keyw block-info)
  "Extract specific keyword KEYW property from header given BLOCK-INFO."
  (let* ((heads (car (cdr (cdr block-info))))
         (kvalue (assoc keyw heads)))
    (cdr kvalue)))


(defun org-tanglesync-get-diffaction (block-info)
  "Extract the diff action if present in BLOCK-INFO otherwise use the default `diff-action`."
  (let ((act (org-tanglesync-get-header-property :diff block-info)))
    (when act
      (intern (concat ":" act)))))

(defun org-tanglesync-get-tangledfile (block-info)
  "Extract tangled info from BLOCK-INFO and return either nil or the file."
  (let* ((tfile (org-tanglesync-get-header-property :tangle block-info)))
    (unless (or (string-equal tfile "no")
                (string-equal tfile "yes"))
      tfile)))


(defun org-tanglesync-get-filedata-buffer (file)
  "Pull the latest content from external FILE into a temp buffer."
  (let ((buff (get-buffer-create "*filedata*")))
    (with-current-buffer buff
      (erase-buffer)
      (insert-file-contents file))
    buff))

(defun org-tanglesync-get-diffline (diff-buffer)
  "Extract the final status line from the DIFF-BUFFER."
  (with-current-buffer diff-buffer
    (save-excursion)
    (let ((point1 nil) (point2 nil)
          (marker "Diff finished"))
      (search-forward marker)
      (setq point1 (- (point) (length marker)))
      (goto-char (line-end-position))
      (setq point2 (point))
      (string-trim (buffer-substring-no-properties point1 point2)))))

(defun org-tanglesync-has-diff (internal external)
  "Perform a diff between the INTERNAL and EXTERNAL and determine if a difference is present."
  (let ((bname (get-buffer-create "*Diff-file-against-block*"))
        (found-diff nil))
    (diff-no-select internal external nil t bname)
    (let ((lastline (org-tanglesync-get-diffline bname)))
      (setq found-diff (not (string-match "no differences" lastline))))
    (set-buffer-modified-p nil)
    (kill-buffer bname)
    found-diff))

(defun org-tanglesync-perform-nothing (internal external org-buffer)
  "Keep the INTERNAL block in the ORG-BUFFER by ignoring EXTERNAL change."
  (ignore internal external org-buffer))

(defun org-tanglesync-perform-custom (internal external org-buffer)
  "Call the function `org-tanglesync-perform-custom-diff-hook` defined by the user with parameters INTERNAL EXTERNAL and ORG-BUFFER."
  (when org-tanglesync-perform-custom-diff-hook
    (funcall org-tanglesync-perform-custom-diff-hook internal external org-buffer)))

(defun org-tanglesync-perform-diff (internal external org-buffer)
  "Call diff on INTERNAL and EXTERNAL, ignoring ORG-BUFFER."
  (diff internal external)
  (ignore org-buffer)
  (when (y-or-n-p "Halt execution, and resolve this diff? ")
    (signal 'quit nil)))

(defun org-tanglesync-auto-format-block ()
  "Format an org src block with the correct indentation, no questions asked.  Assumes org buffer is current."
  (let ((tmp-suc org-tanglesync-skip-user-check))
    (setq org-tanglesync-skip-user-check t)
    ;; current buffer is lost in line below, needs to be restored by parent function.
    (org-edit-src-code)
    (org-edit-src-exit)
    (setq org-tanglesync-skip-user-check tmp-suc)))

(defun org-tanglesync-perform-overwrite (internal external org-buffer &optional pos)
  "Overwrites the current code block INTERNAL with EXTERNAL change in the ORG-BUFFER at optional position POS."
  (ignore internal)
  (let ((cut-beg nil) (cut-end nil)
        (currentbuffer (current-buffer)))
    (with-current-buffer org-buffer
      (when pos
        (goto-char pos))
      ;;(goto-char org-src--beg-marker) only works from within edit-buffer
      (org-babel-goto-src-block-head)
      (search-forward "\n")
      (setq cut-beg (point))
      (search-forward "#+END_SRC")
      (goto-char (- (line-beginning-position) 1))
      (setq cut-end (point))
      ;; cut out the old text
      (delete-region cut-beg cut-end)
      ;; insert the new text
      (goto-char cut-beg)
      (insert-buffer-substring external)
      ;; Perform the auto indent without prompt
      ;; -- this function steals buffer
      (org-tanglesync-auto-format-block)
      (switch-to-buffer currentbuffer))
  (message "Block updated from external")))


(defcustom org-tanglesync-highlight-color '(:background "lightblue")
  "Colour to highlight the header line being tested."
  :type 'list
  :group 'org-tanglesync)

(defun org-tanglesync-perform-userask-overwrite (internal external org-buffer)
  "Asks user whether to overwrite the EXTERNAL file change with the INTERNAL src block into the ORG-BUFFER."
  (let* ((lbeg (line-beginning-position))
        (lend (line-end-position))
        (overlay-highlight (make-overlay lbeg lend)))
    (overlay-put overlay-highlight 'face org-tanglesync-highlight-color)
    (overlay-put overlay-highlight 'line-highlight-overlay-marker t))
  (recenter)
  (when (y-or-n-p "Block has changed externally.  Pull changes? ")
    (org-tanglesync-perform-overwrite internal external org-buffer))
  (remove-overlays (line-beginning-position) (line-end-position)))


(defun org-tanglesync-resolve-action (dont-ask-user block-action)
  "Resolves the action to operate on a block, taking into preferences given by the BLOCK-ACTION header and the DONT-ASK-USER parameter, returning an action."
  (let ((do-action org-tanglesync-default-diff-action))
    (cl-flet ((method-do (a b c) (ignore a b c)))
      ;; default action is overridden by block action
      (when block-action
        (setq do-action block-action))
      (cond
       (dont-ask-user (fset 'method-do 'org-tanglesync-perform-overwrite))
       ((eq do-action :external) (fset 'method-do 'org-tanglesync-perform-overwrite))
       ((eq do-action :internal) (fset 'method-do 'org-tanglesync-perform-nothing))
       ((eq do-action :custom) (fset 'method-do 'org-tanglesync-perform-custom))
       ((eq do-action :diff) (fset 'method-do 'org-tanglesync-perform-diff))
       ((eq do-action :prompt) (fset 'method-do 'org-tanglesync-perform-userask-overwrite)))
      'method-do)))

(defun org-tanglesync-perform-action (internal external org-buffer method-do)
  "Perform the previously resolved action METHOD-DO on the INTERNAL and EXTERNAL change of the org src block within the ORG-BUFFER."
  (funcall method-do internal external org-buffer)
  (kill-buffer internal)
  (kill-buffer external)
  (ignore method-do))

(defun org-tanglesync-process-current-block (dont-ask-user)
  "Process the org src block under cursor, and notify user on each change unless DONT-ASK-USER is set.  A marker to the block is returned if modified, otherwise nil."
  (org-babel-goto-src-block-head)
  (let* ((org-buffer (current-buffer))
         (block-info (org-babel-get-src-block-info))
         (buffer-internal (org-tanglesync-get-blockbody-buffer block-info))
         (tfile (org-tanglesync-get-tangledfile block-info))
         (no-user-action dont-ask-user)
         (block-marker nil))
    (when tfile
      (if (file-exists-p tfile)
          (let ((buffer-external (org-tanglesync-get-filedata-buffer tfile)))
            (when (org-tanglesync-has-diff buffer-internal buffer-external)
              (org-reveal t)
              (let* ((block-action (org-tanglesync-get-diffaction block-info))
                     (res-action (org-tanglesync-resolve-action no-user-action block-action)))
                (org-tanglesync-perform-action buffer-internal buffer-external org-buffer res-action)
                (setq block-marker (point)))))
        ;; Otherwise prompt to copy internal block to file
        (when (y-or-n-p (format "%s does not yet exist, export this block to file? " tfile))
          (with-current-buffer buffer-internal
            (write-file tfile)
            (setq block-marker (point)))))) ;; copy internal buffer to external
    block-marker))

(defun org-tanglesync-process-buffer (dont-ask-user)
  "Process all org src blocks within the current buffer, prompting the user for action unless the DONT-ASK-USER parameter is set.  All headers and subheaders are collapsed except those containing newly-modified src blocks."
  (let ((modified-lines nil)
        (tmp-mark (point))
        (tmp-start (window-start)))
    ;; mark pos
    (condition-case mess
        ;; Protected form
        (while (org-babel-next-src-block)
          (org-overview)
          (unless dont-ask-user
            (recenter))
          (let ((modded-line (org-tanglesync-process-current-block dont-ask-user)))
            (when modded-line
              (cl-pushnew modded-line modified-lines))))
      ;; Out of bounds
      (error ;; type of error
       (let ((emess (error-message-string mess)))
         (if (not(string-match "No further code blocks" emess))
             ;; Propagate error
             (error mess)
           ;; Otherwise Process Buffers
           (message "%d blocks updated" (length modified-lines))
           (org-overview)
           ;; Reverse, then Pop and expand the buffers
           (when modified-lines
             (with-current-buffer (current-buffer)
               (set-buffer-modified-p t))
             (setq modified-lines (reverse modified-lines))
             (while modified-lines
               (let ((lmark (car modified-lines)))
                 (setq modified-lines (cdr modified-lines))
                 (goto-char lmark)
                 (org-reveal t))))
             ;; restore initial
             (goto-char tmp-mark)
             (set-window-start (selected-window) tmp-start)))))))

;;;###autoload
(defun org-tanglesync-process-buffer-interactive ()
  "Interactively processes each org src block in a buffer."
  (interactive)
  (org-tanglesync-process-buffer nil))

;;;###autoload
(defun org-tanglesync-process-buffer-automatic ()
  "Process each org src block in a buffer without prompt."
  (interactive)
  (org-tanglesync-process-buffer t))

(defun org-tanglesync-user-edit-buffer ()
  "A hook to the org-src-code mode.  If there is an external change, prompt the user unless the `org-tanglesync-skip-user-check` custom parameter is set."
  (let* ((edit-buffer (current-buffer))
         (org-buffer (org-src-source-buffer))
         (org-position org-src--beg-marker))
    (with-current-buffer org-buffer
      (when (bound-and-true-p org-tanglesync-mode)
        (goto-char org-position)
        (let* ((tangle-fname (org-tanglesync-get-tangledfile
                              (org-babel-get-src-block-info))))
          (when tangle-fname
            (if (file-exists-p tangle-fname)
                (let* ((file-buffer (org-tanglesync-get-filedata-buffer tangle-fname))
                       (hasdiff (org-tanglesync-has-diff edit-buffer file-buffer)))
                  (when hasdiff
                      (let ((pullchanges
                             (cond (org-tanglesync-skip-user-check t)
                                   ((y-or-n-p "Change detected, load external? ") t)
                                   (t nil))))
                        (when pullchanges
                          (with-current-buffer edit-buffer
                            (erase-buffer)
                            (insert-buffer-substring file-buffer)))))
                  (kill-buffer file-buffer))
              ;; File does not exist, prompt user
              (message "File '%s' does not yet exist, please export it after editing." tangle-fname))))))))


;; ---- Watch methods ----
(define-minor-mode org-tanglesync-watch-mode
  "Allow org-tanglesync to watch other buffers and check to see if they need syncing back
to the original conf file."
  nil
  " o-ts-watch"
  nil
  (if org-tanglesync-watch-mode
      (progn (message "Watching buffers")
             (add-hook 'after-save-hook #'org-tanglesync-watch-save nil t))
    (remove-hook 'after-save-hook #'org-tanglesync-watch-save t)))

(defcustom org-tanglesync-watch-files nil
  "A list of pathnames to config files.
Each pathname contains a list of tangled files (though hopefully without
overlaps!) The tangled file names given in these files are watched when
`org-tanglesync-watch-mode` is enabled, and changes are synced back to
the org file on save."
  :type 'listp
  :group 'org-tanglesync)

(defun org-tanglesync-watch-get-tanglesync-fnames (org-filename)
  "Get tangled file names from an ORG-FILENAME."
  (let ((buff (find-file-noselect org-filename))
        (foundtfiles nil))
    (with-current-buffer buff
      (condition-case mess
          (progn
            (goto-char 0)
            (while (org-babel-next-src-block)
              (org-babel-goto-src-block-head)
              (let* ((block-info (org-babel-get-src-block-info))
                     (tfile (org-tanglesync-get-tangledfile block-info))
                     ;; the :nowatch in the header stops it from being watched
                     (nowatch (org-tanglesync-get-header-property :nowatch block-info)))
                (when (and tfile (file-exists-p tfile))
                  (unless nowatch
                    (push (expand-file-name tfile) foundtfiles))))))
        ;; Out of bounds
        (error ;; type of error
         (let ((emess (error-message-string mess)))
           (if (not(string-match "No further code blocks" emess))
               ;; Propagate error
               (error mess)
             ;; Otherwise return
             foundtfiles)))))))

(defun org-tanglesync-watch-make-watchlist (watchlist)
  "Generate an association list of conf files given by the WATCHLIST to their tangled files."
  (let ((tangle-map nil))
    (dolist (element watchlist tangle-map)
      (push `(,element ,(org-tanglesync-watch-get-tanglesync-fnames element)) tangle-map))
    tangle-map))

(defvar org-tanglesync-confmap
  (org-tanglesync-watch-make-watchlist org-tanglesync-watch-files)
  "An alist that points a config file to all containing tangled filenames.
Uses `org-tanglesync-watch-files` to generate.")

(defun org-tanglesync-watch-get-conf-source (tfile confmap)
  "Find the conf file for a given TFILE tangled file in the CONFMAP.  Assumes unique, so return first match."
  (let ((foundcfile nil))
    (while (and confmap (not foundcfile))
      (let* ((elemmap (car confmap))
             (cfile (car elemmap))
             (tfilellist (car (cdr elemmap))))
        (if (member tfile tfilellist)
            (setq foundcfile cfile)
          (setq confmap (cdr confmap)))))
    foundcfile))

(defun org-tanglesync-watch-get-tfile-pos (tfile cfile)
  "Get cursor position of TFILE in the CFILE."
  (let ((buff (find-file-noselect cfile))
        (blockpos nil))
    (with-current-buffer buff
      (condition-case mess
          (progn
            (goto-char 0)
            (while (and (not blockpos) (org-babel-next-src-block))
              (org-babel-goto-src-block-head)
              (let* ((block-info (org-babel-get-src-block-info))
                     (block-tfile (org-tanglesync-get-tangledfile block-info)))
                (when (and block-tfile
                           (string= (expand-file-name block-tfile)
                                    (expand-file-name tfile)))
                  ;; Found the block for the specific tfile
                  ;; - We don't store the positions for each tfile when we
                  ;;   first build the file map, because positions are likely
                  ;;   to change often.
                  (setq blockpos (point)))))
            blockpos)
        ;; Out of bounds
        (error ;; type of error
         (let ((emess (error-message-string mess)))
           (if (not(string-match "No further code blocks" emess))
               ;; Propagate error
               (error mess)
             ;; Otherwise return
             blockpos)))))))

(defun org-tanglesync-watch-sync-tfile-to-conf (tfile confmap)
  "Sync the TFILE tangle file back to the CONFMAP config file."
  (let ((cfile (org-tanglesync-get-conf-source tfile confmap)))
    (if cfile ;; Perform sync
        (org-tanglesync-watch-perform-sync tfile cfile)
      (message "Could not find config file for %s." tfile))))

(defun org-tanglesync-watch-perform-sync (tfile cfile content-buffer)
  "Take CONTENT-BUFFER from TFILE and place them into the CFILE config."
  (let ((cbuff (find-file-noselect cfile))
         (tpos (org-tanglesync-watch-get-tfile-pos tfile cfile)))
    (org-tanglesync-perform-overwrite t content-buffer cbuff tpos)
    (message "Synced %s to %s" tfile cfile)))

(defun org-tanglesync-watch-save ()
  "A hook to update current buffer contents in the source org file.
Takes the current contents of the saved file and sync them back to
the source org file they are originally tangled to."
  (when org-tanglesync-watch-files
    (let* ((tfile buffer-file-name)
           (cfile (org-tanglesync-watch-get-conf-source
                   tfile
                   org-tanglesync-confmap)))
      (when cfile
        (org-tanglesync-watch-perform-sync
         tfile cfile (current-buffer))))))

(provide 'org-tanglesync)
;;; org-tanglesync.el ends here
