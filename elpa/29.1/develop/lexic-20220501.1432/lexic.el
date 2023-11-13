;;; lexic.el --- A major mode to find out more about words -*- lexical-binding: t; -*-

;; Copyright 2006~2008 pluskid,
;;           2011~2012 gucong
;;           2020~2021 tecosaur

;; Author: pluskid <pluskid@gmail.com>,
;;         gucong <gucong43216@gmail.com>,
;;         TEC <tec@tecosaur.com>
;;
;; Maintainer: TEC <tec@tecosaur.com>
;; Version: 0.0.1
;; Homepage: https://github.com/tecosaur/lexic
;; Package-Requires: ((emacs "26.3"))

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This provides a major mode to view the output of dictionary tools,
;; and utilities that perform searches and nicely format the results.
;;
;; Currently tied to sdcv, but this is intended to be changed in the future.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;   (require 'lexic-mode)
;;   (global-set-key (kbd "C-c d") 'lexic-search)

;;; Changelog:

;; 2020/07/28
;;     * New variable: `lexic-dictionary-specs', allows for
;;       - Dictionary display names
;;       - Custom dictionary entry formatters
;;       - Dictionary sorting
;;     * Update outline function calls to replace depreciated names.'
;;     * Tweak lexic-mode
;;       - Remove font-locking
;;       - Change `outline-regexp' to ZERO WIDTH SPACE
;;       - Add `outline-heading-end-regexp', a PUNCTUATION SPACE
;;       - Expand the mode map, to bind
;;         - Two modes of entry navigation
;;         - History navigation
;;         - TAB for toggling an entry
;;     * Expand popup window
;;     * Add linear history navigation
;;     * Revise behaviour of `lexic-next-entry' and `lexic-previous-entry'
;;     * New function: `lexic-get-outline-path' which gives the structural path
;;       to the current position in buffer, e.g. dict → word v. t. → 1. (Chem.)
;;     * Remove (now unused) custom face vars, could do with adding some
;;       new face vars in the future
;;     * Change the default of `lexic-program-path' to be an absolute path
;;     * New functions: `lexic-format-result', `lexic-failed-p',
;;       and `lexic-format-failure' to handle the upgraded entry processing
;;     * New functions: `lexic-format-webster', `lexic-format-online-etym',
;;       `lexic-format-element', and `lexic-format-soule' to format
;;       the dictionaries recognised by default in `lexic-dictionary-specs'.
;;       - with helper functions `lexic-format-webster-diacritics', and
;;         `lexic-format-expand-abbreviations' for nicer content.

;; 2012/01/02
;;     * New variable: `lexic-word-processor'
;;     * Breaking change:
;;       for `lexic-dictionary-alist', non-list (non-nil) value now means full dictionary list
;;     * Rewrite `lexic-search' for both interactive and non-interactive use
;;     * `lexic-dictionary-list' is left for customization use only
;;     * Better highlighting.
;;
;; 2011/06/30
;;     * New feature: parse output for failed lookup
;;     * Keymap modification
;;
;; 2008/06/11
;;     * lexic-mode v 0.1 init (with background process)

;;; Code:

(require 'outline)
(require 'visual-fill-column nil t)
(require 'cl-lib)
(require 'subr-x)

(declare-function spell-fu-mode "spell-fu")

;;;;##################################################################
;;;;  User Options, Variables
;;;;##################################################################

(defvar lexic-buffer-name "*lexic*"
  "The name of the buffer of lexic.")
(defvar lexic-dictionary-list t
  "A list of dictionaries to use.
Each entry is a string denoting the name of a dictionary, which
is then passed to lexic through the '-u' command line option.
Any non-list value means using all the dictionaries.")
(defvar lexic-dictionary-alist nil
  "An alist of dictionaries, used to interactively form the dictionary list.
It has the form:
   ((\"full\" . t)
    (\"group1\" \"dict1\" \"dict2\" ...)
    (\"group2\" \"dict2\" \"dict3\"))
Any cons cell here means using all dictionaries.")

(defvar lexic-program-path (executable-find "sdcv")
  "The path of lexic program.")

(defvar lexic-dictionary-path nil
  "The path to the dictionaries.")

(defvar lexic-word-processor nil
  "This is the function that take a word (stirng)
and return a word or a list of words for lookup by `lexic-search'.
All lookup result(s) will finally be concatenated together.

nil value means do nothing with the original word.

The following is an example.  This function takes the original word and
compare whether simplified and traditional form of the word are the same.
If not, look up both of the words.

      (lambda (word)
        (let ((sim (chinese-conv word \"simplified\"))
              (tra (chinese-conv word \"traditional\")))
          (if (not (string= sim tra))
              (list sim tra)
            word)))
")


(defvar lexic-current-dictionary-list nil
  "A list of dictionaries to use in searches.
Either entries from `lexic-dictionary-alist', or any non-list value,
which will cause all avalible dictionaries to be used.")

(defvar lexic-wait-timeout 2
  "The max time (in seconds) to wait for the lexic process to produce some output.")
(defvar lexic-wait-interval 0.1
  "The interval (in seconds) to sleep each time to wait for lexic's output.")

(defconst lexic-process-name " %lexic-mode-process%")
(defconst lexic-process-buffer-name " *lexic-mode-process*")

(defvar lexic-word-prompts '("Enter word or phrase: ")
  "A list of prompts that lexic use to prompt for word.")

(defvar lexic-choice-prompts '("Your choice[-1 to abort]: ")
  "A list of prompts that lexic use to prompt for a choice of multiple candidates.")

(defvar lexic-result-patterns '("^Found [0-9]+ items, similar to [*?/|]*\\(.+?\\)[*?]*\\.")
  "A list of patterns to extract result word of lexic.
Special characters are stripped.")

(defvar lexic--search-history nil)
(defvar lexic--search-history-position -1)

(defvar lexic-expand-abbreviations t
  "Whether or not to try to expand abbreviations, where they are expected.")


;;; ==================================================================
;;; Frontend, search word and display lexic buffer

;;;###autoload
(defun lexic-search (word &optional dict-list-name dict-list interactive-p no-history-p)
  "Search WORD through the command line tool lexic.
The result will be displayed in buffer named with
`lexic-buffer-name' with `lexic-mode' if called interactively.

When provided with DICT-LIST-NAME, query `lexic-dictionary-alist'
to get the new dictionary list before search.
Alternatively, dictionary list can be specified directly
by DICT-LIST.  Any non-list value of it means using all dictionaries.

When called interactively, prompt for the word.
Prefix argument have the following meaning:
If `lexic-dictionary-alist' is defined,
use prefix argument to select a new DICT-LIST-NAME.
Otherwise, prefix argument means using all dictionaries.

When INTERACTIVE-P is non-nil, a buffer displaying the result(s) is shown.
Otherwise, the result is returned as a string.

When NO-HISTORY-P is non-nil, the search is not added to the session history.

Word may contain some special characters:
    *       match zero or more characters
    ?       match zero or one character
    /       used at the beginning, for fuzzy search
    |       used at the beginning, for data search
    \       escape the character right after

TODO decouple the tool from the general method."
  (interactive
   (let* ((dict-list-name
           (and current-prefix-arg lexic-dictionary-alist
                (completing-read "Select dictionary list: "
                                 lexic-dictionary-alist nil t)))
          (dict-list
           (and current-prefix-arg (not lexic-dictionary-alist)))
          (guess (or (and transient-mark-mode mark-active
                          (buffer-substring-no-properties
                           (region-beginning) (region-end)))
                     (current-word nil t)
                     "lexical"))
          (word (read-string (format "Search dict (default: %s): " guess)
                             nil nil guess)))
     (list word dict-list-name dict-list t)))
  ;; init current dictionary list
  (unless lexic-current-dictionary-list
    (setq lexic-current-dictionary-list lexic-dictionary-list))
  ;; dict-list-name to dict-list
  (when (and (not dict-list) dict-list-name)
    (if (not lexic-dictionary-alist)
        (error "`lexic-dictionary-alist' not defined"))
    (setq dict-list
          (cdr (assoc dict-list-name lexic-dictionary-alist))))
  ;; prepare new dictionary list
  (when (and dict-list (not (equal lexic-current-dictionary-list dict-list)))
    (setq lexic-current-dictionary-list dict-list)
    ;; kill lexic process
    (and (get-process lexic-process-name)
         (kill-process (get-process lexic-process-name)))
    (while (get-process lexic-process-name)
      (sleep-for 0.01)))
  (let ((result
         (mapconcat
          (lambda (w) (lexic-do-lookup w))
          (if lexic-word-processor
              (let ((processed (funcall lexic-word-processor word)))
                (if (listp processed) processed (list processed)))
            (list word))
          "")))
    (unless (or no-history-p (string= word
                                      (nth lexic--search-history-position
                                           lexic--search-history)))
      (setq lexic--search-history
            (append (cl-subseq lexic--search-history
                               0 (1+ lexic--search-history-position))
                    (list word))
            lexic--search-history-position (1- (length lexic--search-history))))
    (if (not interactive-p)
        result
      (with-current-buffer (get-buffer-create lexic-buffer-name)
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert result))
      (lexic-goto-lexic)
      (lexic-mode)
      (lexic-mode-reinit)
      (let* ((window (get-buffer-window (lexic-get-buffer)))
             (height (window-height window))
             (min-height (pcase (count-lines (point-min) (point-max))
                           ((pred (> 50)) 12)
                           ((pred (> 100)) 16)
                           (_ 20))))
        (when (> min-height height)
          (window-resize window (- 12 height)))))))

;;;###autoload
(defun lexic-search-word-at-point ()
  "Perform `lexic-search' on the word at or near point."
  (interactive)
  (lexic-search
   (downcase
    (or (and transient-mark-mode mark-active
             (buffer-substring-no-properties
              (region-beginning) (region-end)))
        (current-word nil t)
        "lexical"))
   nil nil t))

;;;###autoload
(defun lexic-list-dictionary ()
  "Show available dictionaries."
  (interactive)
  (let (resize-mini-windows)
    (shell-command (concat lexic-program-path " -l") lexic-buffer-name)))

(defun lexic-generate-dictionary-argument ()
  "Generate the appropriate stcv dictionary argument.
Using `lexic-current-dictionary-list' and `lexic-dictionary-path'."
  (append
   (and lexic-dictionary-path (list "--data-dir" (expand-file-name lexic-dictionary-path)))
   (and (listp lexic-current-dictionary-list)
        (mapcan (lambda (dict)
                  (list "-u" dict))
                lexic-current-dictionary-list))))

(defun lexic-search-history-backwards ()
  "Show the previous word searched."
  (interactive)
  (if (> lexic--search-history-position 0)
      (lexic-search (nth (setq lexic--search-history-position
                               (1- lexic--search-history-position))
                         lexic--search-history)
                    nil nil t t)
    (message "At start of search history.")))

(defun lexic-search-history-forwards ()
  "Show the next word searched."
  (interactive)
  (if (> (length lexic--search-history) lexic--search-history-position)
      (lexic-search (nth (setq lexic--search-history-position
                               (1+ lexic--search-history-position))
                         lexic--search-history)
                    nil nil t t)
    (message "At end of search history.")))

;;; ==================================================================
;;; utilities to switch from and to lexic buffer
(defvar lexic-previous-window-conf nil
  "Window configuration before switching to lexic buffer.")
(defun lexic-goto-lexic ()
  "Switch to lexic buffer in other window."
  (interactive)
  (unless (eq (current-buffer)
              (lexic-get-buffer))
    (setq lexic-previous-window-conf (current-window-configuration)))
  (let* ((buffer (lexic-get-buffer))
         (window (get-buffer-window buffer)))
    (if (null window)
        (switch-to-buffer-other-window buffer)
      (select-window window))))

(defun lexic-return-from-lexic ()
  "Bury lexic buffer and restore the previous window configuration."
  (interactive)
  (kill-process (get-process lexic-process-name))
  (if (window-configuration-p lexic-previous-window-conf)
      (progn
        (set-window-configuration lexic-previous-window-conf)
        (setq lexic-previous-window-conf nil)
        (bury-buffer (lexic-get-buffer)))
    (bury-buffer)))

(defun lexic-get-buffer ()
  "Get the lexic buffer. Create one if there's none."
  (let ((buffer (get-buffer-create lexic-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'lexic-mode)
        (lexic-mode)))
    buffer))

;;; ==================================================================

(defvar lexic-mode-map
  (let ((map (copy-keymap special-mode-map)))
    (define-key map "q" 'lexic-return-from-lexic)
    (define-key map (kbd "RET") 'lexic-search-word-at-point)
    (define-key map "a" 'outline-show-all)
    (define-key map "h" 'outline-hide-body)
    (define-key map "o" 'lexic-toggle-entry)
    (define-key map (kbd "TAB") 'lexic-toggle-entry)
    (define-key map "n" 'lexic-next-entry)
    (define-key map "N" (lambda () (interactive) (lexic-next-entry t)))
    (define-key map "p" 'lexic-previous-entry)
    (define-key map "P" (lambda () (interactive) (lexic-previous-entry t)))
    (define-key map "b" 'lexic-search-history-backwards)
    (define-key map "f" 'lexic-search-history-forwards)
    (set-keymap-parent map special-mode-map)
    map)
  "Keymap for `lexic-mode'.")


(define-derived-mode lexic-mode fundamental-mode "lexic"
  "Major mode to look up word through lexic.
\\{lexic-mode-map}
Turning on lexic mode runs the normal hook `lexic-mode-hook'.

This mode locally removes any `spell-fu-mode' or `flyspell-mode' entries in
`text-mode-hook', but won't catch any other spell-checking initialisation.
Consider resolving any edge cases with an addition to `lexic-mode-hook'."
  (setq buffer-read-only t)
  (add-hook 'kill-buffer-hook
            (lambda ()
              (let ((proc (get-process lexic-process-name)))
                (when (process-live-p proc)
                  (kill-process proc))))
            nil t)
  (setq-local outline-regexp "\u200B+")
  (setq-local outline-heading-end-regexp "\u2008")
  (when (featurep 'visual-fill-column)
    (setq-local visual-fill-column-center-text t)
    (visual-fill-column-mode 1)))

(defun lexic-mode-reinit ()
  "Re-initialize buffer.
Hide all entrys but the first one and goto
the beginning of the buffer."
  (ignore-errors
    (setq buffer-read-only nil)
    (lexic-parse-failed)
    (setq buffer-read-only t)

    (let* ((window (get-buffer-window (lexic-get-buffer)))
           (win-height (window-height window))
           (content-height (count-lines (point-min) (point-max))))
      (when (> 0.5 (/ (float win-height) content-height))
        (outline-hide-sublevels 3)))

    (goto-char (point-min))
    (search-forward "\u200B\u200B")
    (left-char 1)))

(defun lexic-parse-failed ()
  "Determine if the search failed, and if so parse the failure."
  (goto-char (point-min))
  (let (save-word)
    (while (re-search-forward "^[0-9]+).*-->\\(.*\\)$" nil t)
      (let ((cur-word (match-string-no-properties 1)))
        (unless (string= save-word cur-word)
          (setq save-word cur-word)
          (re-search-backward "^\\(.\\)" nil t)
          (insert (format "\n==>%s\n" save-word)))))))

(defun lexic-expand-entry ()
  "Show the children of the current entry, or subtree if there are none."
  (outline-show-children)
  (when ; no children
      (<= 0 (- (save-excursion (outline-next-heading) (point))
               (save-excursion (outline-end-of-subtree) (point))))
    (outline-show-subtree)))

(defun lexic-next-entry (&optional linear)
  "Move to the next entry, targeting the same level unless LINEAR is set."
  (interactive)
  (when (< 1 (lexic-outline-level))
    (outline-hide-subtree))
  (if linear
      (outline-next-heading)
    (condition-case nil
        (outline-forward-same-level 1)
      (error
       (condition-case nil
           (progn
             (outline-up-heading 1 t)
             (outline-forward-same-level 1))
         (error (progn (outline-next-heading)
                       (lexic-expand-entry)))))))
  (lexic-expand-entry)
  (recenter-top-bottom 1)
  (message "%s" (lexic-get-outline-path)))

(defun lexic-previous-entry (&optional linear)
  "Move to the previous entry, targeting the same level unless LINEAR is set."
  (interactive)
  (outline-hide-subtree)
  (if (= 2 (line-number-at-pos))
      (recenter-top-bottom -1)
    (if linear
        (outline-previous-heading)
      (condition-case nil
          (outline-backward-same-level 1)
        (error
         (condition-case nil
             (outline-up-heading 1 t)
           (error (outline-previous-heading))))))
    (lexic-expand-entry)
    (recenter-top-bottom 2))
  (message "%s" (lexic-get-outline-path)))

(defun lexic-toggle-entry ()
  "Toggle the folding of the lexic entry point currently lies in."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (not (save-excursion
               (outline-end-of-heading)
               (outline-invisible-p (line-end-position))))
        (outline-hide-subtree)
      (outline-show-subtree))))

;;; ==================================================================
;;; Support for lexic process in background
(defun lexic-do-lookup (word &optional raw-p)
  "Send the WORD to the lexic process and return the result.
Optional argument RAW-P signals whether the result should be formatted or not."
  (let ((process (lexic-get-process)))
    (process-send-string process (concat word "\n"))
    (with-current-buffer (process-buffer process)
      (let ((i 0) result done)
        (while (and (not done)
                    (< i lexic-wait-timeout))
          (when (lexic-match-tail lexic-word-prompts)
            (setq result (buffer-substring-no-properties (point-min)
                                                         (point-max)))
            (setq done t))
          (when (lexic-match-tail lexic-choice-prompts)
            (process-send-string process "-1\n"))
          (unless done
            (sleep-for lexic-wait-interval)
            (setq i (+ i lexic-wait-interval))))
        (unless (< i lexic-wait-timeout)
          ;; timeout
          (kill-process process)
          (error "ERROR: timeout waiting for lexic"))
        (erase-buffer)
        (if raw-p result
          (lexic-format-result result))))))

(defun lexic-oneshot-lookup (word &optional raw-p args)
  "Use a oneshot stcv process just to look up WORD, with ARGS.
Optional argument RAW-P signals whether the result should be formatted or not."
  (let ((result (with-temp-buffer
                  (apply #'call-process lexic-program-path nil t nil
                         (append '("-n") args (list word)))
                  (buffer-string))))
    (if raw-p result
      (lexic-format-result result))))

(defun lexic-get-process ()
  "Get or create the lexic process."
  (let ((process (get-process lexic-process-name)))
    (unless process
      (with-current-buffer (get-buffer-create
                            lexic-process-buffer-name)
        (erase-buffer)
        (setq process (apply #'start-process
                             lexic-process-name
                             lexic-process-buffer-name
                             lexic-program-path
                             (lexic-generate-dictionary-argument)))
        (set-process-query-on-exit-flag process nil)
        ;; kill the initial prompt
        (let ((i 0))
          (message "starting lexic...")
          (while (and (not (lexic-match-tail lexic-word-prompts))
                      (< i lexic-wait-timeout))
            (sit-for lexic-wait-interval t)
            (setq i (+ i lexic-wait-interval)))
          (unless (< i lexic-wait-timeout)
            ;; timeout
            (kill-process process)
            (error "ERROR: timeout waiting for lexic"))
          (erase-buffer))
        (message "")))
    process))

(defun lexic-buffer-tail (length)
  "Get a substring of length LENGTH at the end of current buffer."
  (let ((beg (- (point-max) length))
        (end (point-max)))
    (if (< beg (point-min))
        (setq beg (point-min)))
    (buffer-substring-no-properties beg end)))

(defun lexic-match-tail (prompts)
  "Look for a sdcv prompt from PROMPTS in the tail of the current buffer.
Remove it and return t if found. Return nil otherwise."
  (let ((done nil)
        (prompt nil))
    (while (and (not done)
                prompts)
      (setq prompt (car prompts))
      (setq prompts (cdr prompts))
      (when (string-equal prompt
                          (lexic-buffer-tail (length prompt)))
        (delete-region (- (point-max) (length prompt))
                       (point-max))
        (setq done t)))
    done))

;;;;##################################################################
;;;;  Output Processing
;;;;##################################################################

(defun lexic-format-result (result)
  "For a RESULT from lexic, test for failure and format accordingly.
Entries are sorted by their :priority in `lexic-dictionary-specs' then formatted
by `lexic-format-result' in successful case, `cases-format-failure' otherwise."
  (cond
   ((string-match-p "^Nothing similar to" result)
    (lexic-consider-no-results))
   ((lexic-failed-p result)
    (lexic-format-failure result))
   (t
    (let* ((entries
            (sort (lexic-parse-results result)
                  (lambda (a b)
                    (< (or (lexic-dictionary-spec (plist-get a :dict) :priority) 1)
                       (or (lexic-dictionary-spec (plist-get b :dict) :priority) 1)))))
           (word (save-match-data
                   (string-match "\\`Found.* similar to \\(\\w+\\)\\." result)
                   (downcase (match-string 1 result)))))
      (concat
       "\u200B"
       (propertize (capitalize word) 'face 'outline-1)
       "\u2008"
       (apply #'concat
              (mapcar (lambda (e)
                        (lexic-format-entry
                         e word))
                      entries)))))))

(defun lexic-consider-no-results ()
  "No results have been found. What should we tell the user?"
  (let ((dicts? (not (string-match-p "\\`Dictionary's name +Word count[\n ]+\\'"
                                     (shell-command-to-string (concat lexic-program-path " -l"))))))
    (if dicts?
        (user-error "Couldn't find anything similar to your search, sorry :(")
      (user-error "No results found, but you don't seem to have any dictionaries installed! Try %s"
                  (propertize "M-x lexic-dictionary-help" 'face 'font-lock-keyword-face)))))

(defun lexic-parse-results (result)
  "Loop through every entry in RESULT and parse each one.
Returns a list of plists with keys :word, :dict, and :info."
  (let (entries latest-match last-match dict word)
    (with-temp-buffer
      (insert result)
      (goto-char (point-min))
      (while
          (setq latest-match (re-search-forward
                              "-->\\([^\n]+\\)\n-->\\(.+\\)\n\n" nil t))
        (when last-match
          (forward-line -3)
          (setq entries
                (append entries
                        `((:word ,word
                           :dict ,dict
                           :info ,(buffer-substring last-match (point))))))
          (forward-line 3))
        (setq last-match latest-match)
        (setq dict (match-string 1))
        (setq word (match-string 2)))
      (when last-match
        (setq entries
              (append entries
                      `((:word ,word
                         :dict ,dict
                         :info ,(buffer-substring last-match (point-max))))))))))

(defun lexic-failed-p (results)
  "Whether the RESULTS match the hardcoded failure pattern."
  (if (string-match-p "Found [0-9]+ items, similar to [^.]+\\.\n0)" results) t nil))

(defun lexic-format-failure (results)
  "When lexic failed to match the word, format the suggestions in RESULTS."
  (let (suggestions last-match)
    (while (setq last-match
                 (string-match "^[0-9]+)\\(.*\\)-->\\([A-Za-z]+\\)"
                               results
                               (when last-match (1+ last-match))))
      (let ((dict (match-string 1 results))
            (word (match-string 2 results)))
        (if (assoc dict suggestions)
            (setcdr (assoc dict suggestions)
                    (list (append (cadr (assoc dict suggestions)) (list word))))
          (setq suggestions (append suggestions `((,dict . ((,word)))))))))
    (concat
     (propertize
      (replace-regexp-in-string
       "items" "entries"
       (substring results 0 (string-match "\n" results)))
      'face 'warning)
     "\n"
     (mapconcat (lambda (dict-suggestions)
                  (format "\u200B\u200B%s\n\u200B\u200B\u200B%s"
                          (propertize (or
                                       (lexic-dictionary-spec (car dict-suggestions) :short)
                                       (car dict-suggestions))
                                      'face 'outline-3)
                          (propertize
                           (mapconcat #'identity (cadr dict-suggestions) "\n\u200B\u200B\u200B")
                           'face 'font-lock-keyword-face)))
                (sort suggestions
                      (lambda (a b)
                        (< (or (lexic-dictionary-spec (car a) :priority) 1)
                           (or (lexic-dictionary-spec (car b) :priority) 1))))
                "\n"))))

(defun lexic-format-entry (entry &optional expected-word)
  "Format a given ENTRY, a plist with :word :dict and :info.
If the DICT has a :short value in `lexic-dictionary-specs' that is used as
the display name. Likewise if present, :formatter is used to generate the
entry. EXPECTED-WORD is the word expected in ENTRY."
  (let ((dict (plist-get entry :dict)))
    (concat
     "\n\u200B\u200B"
     (propertize (or (lexic-dictionary-spec dict :short)
                     dict) 'face 'outline-3)
     "\n\u2008\n"
     (if-let* ((formatter (lexic-dictionary-spec dict :formatter)))
         (let ((case-fold-search nil))
           (string-trim (funcall formatter entry expected-word)))
       (plist-get entry :info))
     "\n")))

(defun lexic-get-outline-path ()
  "Return a string giving the structural path to the current position."
  (let ((outline-path "")
        (last-pos 0)
        outline-level-current substring level-regexp)
    (save-excursion
      (outline-back-to-heading)
      (setq outline-level-current (lexic-outline-level))
      (while (/= (point) last-pos)
        (setq outline-level-current (lexic-outline-level))
        (setq substring
              (buffer-substring
               (point)
               (save-excursion (search-forward "\u2008") (point))))
        (setq level-regexp
              (pcase outline-level-current
                (1 "^\\([^\n]+\\)")
                (2 "^\\([^ \n]+\\)")
                (3 "^\u200B\u200B*\\([^,]+\\(?:, [ &.;a-z]+\\)?\\)")
                (4 "\\([0-9]+\\.\\(  ?([^)]+)\\)?\\( \\w+\\)\\{0,4\\}\\)")
                (5 "\\(([a-z])\\(  ?([^)]+)\\)?\\( \\w+\\)\\{0,4\\}\\)")
                (_ "^\u200B\u200B*\\([^ ]+\\)")))
        (setq outline-path
              (concat
               (propertize " → " 'face 'bold)
               (save-match-data
                 (string-match level-regexp substring)
                 (match-string 1 substring))
               outline-path))
        (setq last-pos (point))
        (ignore-errors
          (outline-up-heading 1)))
      (substring outline-path 2))))

(defun lexic-outline-level ()
  "It seems that while (outline-level) should work, it has issues."
  (- (save-excursion (outline-back-to-heading)
                     (search-forward-regexp "\u200B+"))
     (point)))

(defvar lexic-dictionary-specs
  '(("Webster's Revised Unabridged Dictionary (1913)"
     :formatter lexic-format-webster
     :priority 1)
    ("Elements database"
     :short "Element"
     :formatter lexic-format-element
     :priority 2)
    ("Hitchcock's Bible Names Dictionary"
     :short "Hitcchcock's Bible Names"
     :priority 3)
    ("Online Etymology Dictionary"
     :short "Etymology"
     :formatter lexic-format-online-etym
     :priority 4)
    ("Soule's Dictionary of English Synonyms"
     :short "Synonyms"
     :formatter lexic-format-soule
     :priority 5))
  "List of dictionary specifications.
In each entry the car is the name according to lexic, and the cdr is
a plist whith the following options:
  :short - a (usually) shorter display name for the dictionary
  :formatter - a function with signature (ENTRY WORD) that returns a string
  :priority - sort priority, defaults to 1")

(defun lexic-dictionary-spec (dict spec)
  "Helper function to get a :SPEC of a given DICT."
  (plist-get (cdr (assoc dict lexic-dictionary-specs)) spec))

(defun lexic-format-webster (entry &optional _expected-word)
  "Make a Webster's dictionary ENTRY for WORD look nice.
Designed for Webster's Revised Unabridged Dictionary (1913),as found at
http://download.huzheng.org/dict.org/stardict-dictd-web1913-2.4.2.tar.bz2.

This should also work nicely with GCIDE."
  (thread-last (plist-get entry :info)
       (lexic-format-webster-diacritics)
       (replace-regexp-in-string ; entry dividors
        (format "\n\n\\(%s\\)" (plist-get entry :word))
        "\n                     ━━━━━━━━━ ■ ━━━━━━━━━\n\n\\1")
       (replace-regexp-in-string ; entry headline
        (rx line-start
            (group-n 1 ; word
                     (any "A-Z")
                     (+ (any "a-z")))
            (optional " \\" ; word2
                      (group-n 2 (+ (not (any "\\"))))
                      "\\")
            (optional " (" ; pronounciation
                      (group-n 3 (+ (not (any ")"))))
                      ")")
            ", "
            (group-n 4 ; part of speech
                     (+ (any "A-Z" "a-z" ".;&" " ")))
            (optional "[" ; etymology / alternative forms
                      (group-n 5
                               (+ (or (+ (not (any "][")))
                                      (and "[" (+ (not (any "]["))) "]"))))
                      "]")
            (optional ; definitely etymology
             (+ (any "\n" " ")) "["
             (group-n 6
                      (+ (or (+ (not (any "][")))
                             (and "[" (+ (not (any "]["))) "]"))))
             "]")
            (optional " (" ; category
                      (group-n 7 (+ (not (any ")"))))
                      ")"))
        (lambda (match)
          (let* ((word2 (match-string 2 match))
                 (pronounciation (match-string 3 match))
                 (part-of-speech (lexic-format-expand-abbreviations
                                  (replace-regexp-in-string " \\'" ""
                                                            (match-string 4 match))))
                 (alternative-forms (when (match-string 6 match)
                                      (lexic-format-expand-abbreviations (match-string 5 match))))
                 (etymology (lexic-format-expand-abbreviations (match-string (if alternative-forms 6 5) match)))
                 (category (lexic-format-expand-abbreviations (match-string 7 match)))
                 (last-newline (lambda (text) (- (length text)
                                            (or (save-match-data
                                                  (string-match "\n[^\n]*\\'" text)) 0)))))
            (concat
             "\u200B\u200B\u200B"
             (propertize word2
                         'face 'bold)
             (when pronounciation
               (propertize (format " %s" pronounciation)
                           'face 'font-lock-type-face))
             ", "
             (propertize part-of-speech
                         'face '(bold font-lock-keyword-face))
             (when alternative-forms
               (setq alternative-forms
                     (lexic-format-reflow-text
                      (format " [%s]" alternative-forms)
                      80 10
                      (+ 3 (if pronounciation 1 0)
                         (funcall last-newline
                                  (concat word2 pronounciation part-of-speech)))
                      "   "))
               (propertize alternative-forms
                           'face 'diff-context))
             (when etymology
               (setq etymology
                     (lexic-format-reflow-text
                      (format " [%s]" etymology)
                      80 10
                      (+ 3 (if pronounciation 1 0)
                         (funcall last-newline
                                  (concat word2 pronounciation part-of-speech alternative-forms)))
                      "   "))
               (propertize etymology
                           'face 'font-lock-comment-face))
             (when category
               (propertize (format " (%s)" category)
                           'face 'font-lock-constant-face))
             "\u2008"))))
       (replace-regexp-in-string ; categorised terms
        "{\\([^}]+?\\)}\\(.?\\) (\\([^)]+?\\))"
        (lambda (match)
          (let ((term (match-string 1 match))
                (punct (match-string 2 match))
                (category (match-string 3 match)))
            (concat
             (propertize term 'face 'font-lock-keyword-face)
             punct
             (propertize (format " (%s)"
                                 (if lexic-expand-abbreviations
                                     (lexic-format-expand-abbreviations category)
                                   category))
                         'face 'font-lock-constant-face)))))
       (replace-regexp-in-string ; other terms
        "{\\([^}]+?\\)}"
        (lambda (match)
          (let ((term (match-string 1 match)))
            (concat
             (propertize term 'face 'font-lock-keyword-face)))))
       (replace-regexp-in-string ; quotations
        "^\n            +\\(\\w[[:ascii:]]+?\\)\\(\n? *--[A-Za-z0-9. ]+\n? *[A-Za-z0-9. ]*\\)"
        (lambda (match)
          (let ((body (match-string 1 match))
                (author (match-string 2 match)))
            (concat
             "\n           "
             (propertize (format "❝%s❞" body)
                         'face 'font-lock-doc-face)
             author "\n"))))
       (replace-regexp-in-string ; attributions
        " --\\([A-Z][A-Za-z. ]+\n? *[A-Za-z0-9. ]*\\)"
        (lambda (match)
          (propertize (concat " ──" (match-string 1 match))
                      'face '(italic font-lock-type-face))))
       (replace-regexp-in-string ; inline quotations (1)
        "``" "“")
       (replace-regexp-in-string ; inline quotations (1)
        "''" "”")
       (replace-regexp-in-string ; em dash approximation
        " -- " " ─── ")
       (replace-regexp-in-string ; lists
        "   \\(?:\\([0-9]+\\.\\)\\|\\(   ([a-z])\\)\\) \\(?: ?(\\([^)]+\\)) \\)?\\(.*\\)"
        (lambda (match)
          (let ((number (match-string 1 match))
                (letter (match-string 2 match))
                (category (match-string 3 match))
                (rest-of-line (match-string 4 match)))
            (concat
             (when letter "\u200B")
             "\u200B\u200B\u200B\u200B   "
             (when number
               (propertize number 'face '(bold font-lock-string-face)))
             (when letter
               (propertize letter 'face 'font-lock-string-face))
             (when category
               (propertize (format " (%s)"
                                   (if lexic-expand-abbreviations
                                       (lexic-format-expand-abbreviations category)
                                     category))
                           'face 'font-lock-constant-face))
             " "
             rest-of-line
             "\u2008"))))
       (replace-regexp-in-string ; note
        "  Note: "
        (concat "     "
                (propertize " " 'display '(space . (:width 0.55)))
                (propertize "☞" 'face 'font-lock-function-name-face)
                " "))
       (replace-regexp-in-string ; subheadings
        "  \\(\\w+\\): "
        (lambda (match)
          (propertize  (concat "  "(match-string 1 match) ": ")
                       'face 'bold)))))

(defun lexic-format-expand-abbreviations (content &optional force)
  "Expand certain standard abbreviations in CONTENT when `lexic-expand-abbreviations' or FORCE are non-nil."
  (when content
    (when (or lexic-expand-abbreviations force)
      (let ((abbreviations
             '(; A
               ("adj"                     "adjective")
               ("a"                       "adjective")
               ("abbrev"                  "abbreviated")
               ("abl"                     "ablative")
               ("Abp"                     "Archbishop")
               ("acc"                     "Acoustics")
               ("act"                     "active")
               ("adv"                     "adverb")
               ("Agric"                   "Agriculture")
               ("Alban"                   "Albanian")
               ("Alg"                     "Algebra")
               ("Am"                      "America")
               ("Amer"                    "American")
               ("Am"                      "Amos")
               ("Am\\. Cyc"               "Appleton's American Cyclopedia")
               ("Anal. Geom"              "Analytical Geometry")
               ("Anat"                    "Anatomy")
               ("Anc"                     "Ancient")
               ("Angl\\. Ch"              "Anglican Church")
               ("aor"                     "aorist")
               ("Ar"                      "Arabic")
               ("Arch"                    "Architecture")
               ("Arch\\. Pub\\. Soc"      "Architectural Pub. Society")
               ("Arith"                   "Arithmetic")
               ("Arm\\., Armor"           "Armorican")
               ("AS"                      "Anglo-Saxon")
               ("Astrol"                  "Astrology")
               ("Astron"                  "Astronomy")
               ("aug"                     "augmentative")
               ;; B
               ("Bank"                    "Banking")
               ("Beau\\. & Fl"            "Beaumont & Fletcher")
               ("B\\. & Fl"               "Beaumont & Fletcher")
               ("Bib\\. Sacra"            "Bibliotheca Sacra")
               ("Bib"                     "Biblical")
               ("Bibliog"                 "Bibliography")
               ("Biol"                    "Biology")
               ("Bisc"                    "Biscayan")
               ("B\\. Jon"                "Ben Jonson")
               ("Bk\\. of Com\\. Prayer " "Book of Common Prayer")
               ("Blackw\\. Mag"           "Blackwood's Magazine")
               ("Bohem"                   "Bohemian")
               ("Bot"                     "Botany")
               ("Bp"                      "Bishop")
               ("Brande & C"              "Brande & Cox")
               ("Braz"                    "Brazilian")
               ("Brit\\. Critic"          "British Critic")
               ("Brit\\. Quar\\. Rev"     "British Quarterly Review")
               ("Burl"                    "Burlesque")
               ;; C
               ("C"                       "Centigrade")
               ("Cant"                    "Canticles")
               ("Carp"                    "Carpentry")
               ("Catal"                   "Catalan")
               ("Cath\\. Dict"            "Catholic Dictionary")
               ("Celt"                    "Celtic")
               ("cf"                      "confer")
               ("Cf"                      "Confer")
               ("Ch"                      "Church")
               ("Chald"                   "Chaldee")
               ("Chem"                    "Chemistry")
               ("Ch\\. Hist"              "Church History")
               ("Chron"                   "Chronology, Chronicles")
               ("Civ"                     "Civil")
               ("Class"                   "Classical")
               ("Class\\. Myth"           "Classical Mythology")
               ("Col"                     "Colossians")
               ("colloq\\., coll"         "colloquial, colloquially")
               ("Com"                     "Commerce, Common")
               ("comp"                    "compound, compounded, composition")
               ("compar"                  "comparative")
               ("conj"                    "conjunction")
               ("Con\\. Sect"             "Conic Sections")
               ("contr"                   "contracted, contraction")
               ("Copt"                    "Coptic")
               ("Corn"                    "Cornish")
               ("corrupt"                 "corrupted, corruption")
               ("Cotgr"                   "Cotgrave")
               ("Cyc\\. Med"              "Cyclopedia of Practical Medicine")
               ("Crim\\. Law"             "Criminal Law")
               ("Crystallog"              "Crystallography")
               ("Cyc"                     "Cyclopedia")
               ;; D
               ("D"                       "Dutch (sometimes Daniel)")
               ("Dan"                     "Danish")
               ("dat"                     "dative")
               ("def"                     "definitions")
               ("Deut"                    "Deuteronomy")
               ("Dial"                    "Dialectic")
               ("dim"                     "diminutive")
               ("Diosc"                   "dioscorides")
               ("Disp"                    "Dispensatory")
               ("Disus"                   "Disused")
               ("Dom\\. Econ"             "Domestic Economy")
               ("Dublin Univ\\. Mag"      "Dublin University Magazine")
               ("Dyn"                     "Dynamics")
               ;; E
               ("E"                       "English")
               ("Eccl"                    "Ecclesiastical, Ecclesiastes")
               ("Eccl\\. Hist"            "Ecclesiastical History")
               ("Ecclus"                  "Ecclesiasticus")
               ("Eclec\\. Rev"            "Eclectic Review")
               ("Ed\\. Rev"               "Edinburgh Review")
               ;; ("e\\. g"                  "exempli gratia (for example)")
               ("Egypt"                   "Egyptian")
               ("Elect"                   "Electricity")
               ("Elec"                    "Electrical")
               ("emph"                    "emphatic")
               ("Encyc\\. Amer"           "Encyclopedia Americana")
               ("Encyc\\. Crit"           "Encyclopedia Britannica")
               ("Encyc\\. Dict"           "Hunter's Encyclopedic Dictionary")
               ("Encyc"                   "Encyclopedia")
               ("Eng\\. Cyc"              "English Cyclopedia")
               ("Eng"                     "English")
               ("Engin"                   "Engineering")
               ("Eol"                     "Eolic")
               ("Eph\\., Ephes"           "Ephesians")
               ("equiv"                   "equivalent")
               ("Esd"                     "Esdras")
               ("esp"                     "especially")
               ("Etch\\. & Eng"           "Etching & Engraving")
               ("Ethnol"                  "Ethnology")
               ("etym\\., etymol"         "etymology")
               ("Ex\\., Exod"             "Exodus")
               ("Ezek"                    "Ezekiel")
               ;; F
               ("F"                       "French")
               ("f"                       "feminine")
               ("fem"                     "feminine")
               ("Fahr"                    "Fahrenheit")
               ("Far"                     "Farriery")
               ("Feud"                    "Feudal")
               ("Fig"                     "Figurative, figuratively")
               ("Fin"                     "Finnish")
               ("For\\. Quart\\. Rev"     "Foreign Quarterly Review")
               ("Fort"                    "Fortification")
               ("Fr"                      "French")
               ("fr"                      "from")
               ("freq"                    "frequentative")
               ("Fries"                   "Friesic")
               ("fut"                     "future")
               ;; G
               ("G"                       "German")
               ("Gael"                    "Gaelic")
               ("Gal"                     "Galen")
               ("Gal"                     "Galatians")
               ("Galv"                    "Galvanism")
               ("gen"                     "generally, genitive")
               ("Geneal"                  "Genealogy")
               ("Gent\\. Mag"             "Gentleman's Magazine")
               ("Geog"                    "Geography")
               ("Geol"                    "Geology")
               ("Geom"                    "Geometry")
               ("Ger"                     "Germanic or German")
               ("Gk"                      "Greek")
               ("Goth"                    "Gothic")
               ("Gov\\. of Tongue"        "Government of the Tongue")
               ("Gr"                      "Greek")
               ("Gram"                    "Grammar")
               ("Gris"                    "Grisons")
               ("Gun"                     "Gunnery")
               ;; H
               ("H"                       "High")
               ("Hab"                     "Habakkuk")
               ("Hag"                     "Haggai")
               ("Ham\\. Nav\\. Encyc"     "Hamersly's Naval Encyclopedia")
               ("Heb"                     "Hebrew")
               ("Her"                     "Heraldry")
               ("Hind"                    "Hindostanee")
               ("Hipp"                    "Hippocrates")
               ("Hist"                    "History")
               ("Horol"                   "Horology")
               ("Hort"                    "Horticulture")
               ("Hung"                    "Hungarian")
               ("Hydraul"                 "Hydraulics")
               ("Hydros"                  "Hydrostatics")
               ("hypoth"                  "hypothetical")
               ;; I
               ("Icel"                    "Icelandic")
               ;; ("i\\. e"                  "id est (that is)")
               ("Illust"                  "Illustration, Illustrated")
               ("imp"                     "imperfect")
               ("Imp\\. Dict"             "Imperial Dictionary")
               ("incho"                   "inchoative")
               ("ind"                     "indicative")
               ("indef"                   "indefinite")
               ("inf"                     "infinitive")
               ("intens"                  "intensive")
               ("interj"                  "interjection")
               ("Internat\\. Cyc"         "International Cyclopeia")
               ("Ion"                     "Ionic")
               ("i\\. q"                  "idem quod")
               ("Ir"                      "Irish")
               ("Is"                      "Isaiah")
               ("Isa"                     "Isaiah")
               ("It"                      "Italian")
               ;; J
               ("Jap"                     "Japanese")
               ("Jas"                     "James")
               ("Jav"                     "Javanese")
               ("Jer"                     "Jeremiah")
               ("Join"                    "Joinery")
               ("Josh"                    "Joshua")
               ("Judg"                    "Judges")
               ;; K
               ("K"                       "Kings")
               ;; L
               ("L"                       "Latin")
               ("Lam"                     "Lamentations")
               ("Lapp"                    "Lappish")
               ("Lat"                     "Latin")
               ("LD"                      "Low Dutch")
               ("Lett"                    "Lettish")
               ("Lev"                     "Leviticus")
               ("LG"                      "Low German")
               ("LGr"                     "Low Greek")
               ("Linn"                    "Linnæus")
               ("Lit"                     "Literally")
               ("lit"                     "literally")
               ("Lit"                     "Literature")
               ("Lith"                    "Lithuanian")
               ("LL"                      "Late Latin")
               ;; M
               ("M"                       "Middle")
               ("m"                       "masculine")
               ("masc"                    "masculine")
               ("Maced"                   "Macedonian")
               ("Mach"                    "Machinery")
               ("Mad"                     "Madam")
               ("Mag"                     "Magazine")
               ("Mal"                     "Malachi")
               ("Malay"                   "Malayan")
               ("Man"                     "Manège")
               ("Manuf"                   "Manufacturing")
               ("Mar"                     "Maritime")
               ("Math"                    "Mathematics")
               ("Matt"                    "Matthew")
               ("ME"                      "Middle English")
               ("Mech"                    "Mechanic")
               ("Med"                     "Medicine")
               ("Metal"                   "Metallurgy")
               ("Metaph"                  "Metaphysics")
               ("Meteor"                  "Meteorolgy")
               ("mgr"                     "milligrams")
               ("MHG"                     "Middle High German")
               ("Micros"                  "Microscopy")
               ("Mil"                     "Military")
               ("Min"                     "Mineralogy")
               ("Mir\\. for Mag"          "Mirror for Magistrates")
               ("MLG"                     "Middle Low German")
               ("Moham"                   "Mohammedan")
               ("Mozley & W"              "Mozley & Whiteley")
               ("Mus"                     "Music")
               ("Myst"                    "Mysteries")
               ("Myth"                    "Mythology")
               ;; N
               ("Nat\\. Hist"             "Natural History")
               ("Nat\\. ord"              "Natural order")
               ("Naut"                    "Nautical")
               ("Nav"                     "Navy")
               ("Navig"                   "Navigation")
               ("N\\. Brit\\. Rev"        "North British Review")
               ("Neh"                     "Nehemiah")
               ("neut"                    "neuter")
               ("New Am\\. Cyc"           "New American Cyclopedia")
               ("New Month\\. Mag"        "New Monthly Magazine")
               ("NF"                      "New French")
               ("NGr"                     "Mew Greek")
               ("NHeb"                    "New Hebrew")
               ("NL"                      "New Latin")
               ("nom"                     "nominative")
               ("Norm\\. F"               "Norman French")
               ("North Am\\. Rev"         "North American Review")
               ("Norw"                    "Norwegian")
               ("Num"                     "Numbers")
               ("Numis"                   "Numismatics")
               ("N"                       "New")
               ;; O
               ("O"                       "Old")
               ("Ob"                      "Obadiah")
               ("obs"                     "obsolete")
               ("Obsoles"                 "Obsolescent")
               ("OCelt"                   "Old Celtic")
               ("OD"                      "Old Dutch")
               ("ODan"                    "Old Danish")
               ("OE"                      "Old English")
               ("OF"                      "Old French")
               ("OFelm"                   "Old Flemish")
               ("OFris"                   "Old Frisian")
               ("OFries"                  "Old Friesic")
               ("OGael"                   "Old Gaelic")
               ("OGr"                     "Old Greek")
               ("OHG"                     "Old High German")
               ("OIcel"                   "Old Icelandic")
               ("OIt"                     "Old Italian")
               ("OL"                      "Old Latin")
               ("OIr"                     "Old Irish")
               ("ON"                      "Old Norse")
               ("OLG"                     "Old Low German")
               ("OPer"                    "Old Persian")
               ("OPg"                     "Old Portuguese")
               ("OPol"                    "Old Polish")
               ("Opt"                     "Optics")
               ("orig"                    "original")
               ("Ornith"                  "Ornithology")
               ("OS"                      "Old Saxon")
               ("OSlav"                   "Old Slavic")
               ("OSp"                     "Old Spanish")
               ("Oxf\\. Gloss"            "Oxford Glossary of Architecture")
               ;; P
               ("p\\.[\n ]*a"             "participial adjective")
               ("Paint"                   "Painting")
               ("Paleon"                  "Paleontology")
               ("pass"                    "passive")
               ("Pathol"                  "Pathology")
               ("P\\. Cyc"                "Penny Cyclopedia")
               ("Per"                     "Persian")
               ("perh"                    "perhaps")
               ("pers"                    "person")
               ("Persp"                   "Perspective")
               ("Pert"                    "Pertaining")
               ("Peruv"                   "Peruvian")
               ("Pet"                     "Peter")
               ("Pg"                      "Portuguese")
               ("Pharm"                   "Pharmacy, Pharmacopœia")
               ("Phil"                    "Phillipians")
               ("Philem"                  "Philemon")
               ("Philol"                  "Philology")
               ("Philos"                  "Philosophy")
               ("Phon"                    "Phonetics")
               ("Photog"                  "Photography")
               ("Photom"                  "Photometry")
               ("Phren"                   "Phrenology")
               ("Phys"                    "Physics")
               ("Phys\\. Sci"             "Physical Science")
               ("Physiol"                 "Physiology")
               ("pl"                      "plural")
               ("Poet"                    "Poetry, Poetical")
               ("Pol"                     "Polish")
               ("Pol\\. Econ"             "Political Economy")
               ("Polit\\. Econ"           "Political Economy")
               ("Pop\\. Sci\\. Monthly"   "Polular Science Monthly")
               ("Poss"                    "Possessive")
               ("pp"                      "pages")
               ("P\\. Plowman"            "Piers Plowman")
               ("p\\.[\n ]*p"             "past participle")
               ("p\\.[\n ]*pr"            "present participle")
               ("p\\.[\n ]*ple"           "present participle")
               ("Pr"                      "Provençal")
               ("Pref"                    "Preface")
               ("pref"                    "prefix")
               ("prep"                    "preposition")
               ("pres"                    "present")
               ("pret"                    "preterit")
               ("prin"                    "principally")
               ("Print"                   "Printing")
               ("priv"                    "privative")
               ("prob"                    "probably")
               ("pron"                    "pronoun")
               ("prop"                    "properly")
               ("Pros"                    "Prosody")
               ("prov"                    "provincial")
               ("Prov"                    "Provincial")
               ("Prov"                    "Proverbs")
               ("Ps\\., Psa"              "Psalms")
               ("Pyro\\.-elect"           "Pyro-electricity")
               ("p"                       "participle")
               ;; Q
               ("Quart\\. Rev"            "Quarterly Review")
               ("q\\. v"                  "quod vide (which see)")
               ;; R
               ("R\\. C"                  "Roman Catholic")
               ("R\\. C\\. Ch"            "Roman Catholic Church")
               ("Rep\\. Sec\\. of War"    "Report of Secretary of War")
               ("Rev"                     "Revelation")
               ("Rev"                     "Review")
               ("Rev\\. Ver"              "Revised Version (of the Bible)")
               ("Rhet"                    "Rhetoric")
               ("R\\. of Brunne"          "Robert of Brunne")
               ("R\\. of Gl"              "Robert of Gloucester")
               ("Rom"                     "Roman, Romans")
               ("Rom\\. Cath"             "Roman Catholic")
               ("Rom\\. of R"             "Romaunt of the Rose")
               ("Rpts"                    "reports")
               ("Russ"                    "Russian")
               ("R"                       "Rare")
               ;; S
               ("Sam"                     "Samaritan")
               ("Sam"                     "Samuel")
               ("Sat\\. Rev"              "Saturday Review")
               ("Sax"                     "Saxon")
               ("sc"                      "scilicet (being understood)")
               ("Scand"                   "Scandinavian")
               ("Sci"                     "Science")
               ("Sci\\. Am"               "Scientific American")
               ("Scot"                    "Scotland, Scottish")
               ("Script"                  "Scripture, Scriptural")
               ("Sculp"                   "Sculpture")
               ("Serb"                    "Serbian")
               ("Serv"                    "Servian")
               ("Shak"                    "Shakespeare")
               ("sing"                    "singular")
               ("Skr"                     "Sanskrit")
               ("Slav"                    "Slavonic")
               ("Sp"                      "Spanish")
               ("Specif"                  "Specifically")
               ("Stat"                    "Statuary")
               ("subj"                    "subjunctive")
               ("superl"                  "superlative")
               ("Surg"                    "Surgery")
               ("Surv"                    "Surveying")
               ("Sw"                      "Swedish")
               ("Syd\\. Soc\\. Lex"       "Sydenham Society Lexicon")
               ("Syn"                     "Synonyms")
               ("Synop"                   "Synopsis")
               ("Syr"                     "Syriac")
               ;; T
               ("Tart"                    "Tartaric")
               ("Teleg"                   "Telegraphy")
               ("term"                    "termination")
               ("Test"                    "Testament")
               ("Theol"                   "Theology")
               ("Thes"                    "Thessalonians")
               ("Tim"                     "Timothy")
               ("Todd & B"                "Todd & Bowman")
               ("Trans"                   "Translation")
               ("Treas"                   "Treasury")
               ("Trig"                    "Trigonometry")
               ("Turk"                    "Turkish")
               ("Typog"                   "Typography")
               ;; U
               ("Univ"                    "University")
               ("Up"                      "Upper")
               ("U\\. ?S"                 "United States")
               ("U\\. ?S\\. Disp"         "United States Dispensatory")
               ("U\\. ?S\\. Pharm"        "United States Pharmacopœia")
               ("U\\. ?S\\. Int\\. Rev\\. Statutes" "United States Internal Revenue Statutes")
               ("usu"                     "usually")
               ;; V
               ("v\\.[\n ]*i"             "intransitive verb")
               ("v\\.[\n ]*t"             "transitive verb")
               ("var"                     "variety")
               ("vb\\.[\n ]*n"            "verbal noun")
               ("Veter"                   "Veterinary")
               ("Vitr"                    "Vitruvius")
               ;; W
               ("W"                       "Welsh")
               ("Wall"                    "Wallachian")
               ("Westm\\. Cat"            "Westminster Catechism")
               ("Westm\\. Rev"            "Westminster Review")
               ;; Z
               ("Zech"                    "Zechariah")
               ("Zeph"                    "Zephaniah")
               ("Zoöl"                    "Zoölogy")
               ;; Reordered for correctness
               ("n"                       "noun")
               ("v"                       "verb"))))
        (dolist (abbrev abbreviations)
          (setq content
                (replace-regexp-in-string
                 (concat "\\b" (car abbrev) "\\.")
                 (cadr abbrev)
                 content t)))))
    content))

(defun lexic-format-webster-diacritics (pronunciation)
  "Replace ascii pronounciation symbols in PRONUNCIATION with unicode equivalents."
  (let ((diacritics
         '(("[,C]" "Ç")
           ("\"u" "ü") ; uum
           ("'e" "é") ; eacute
           ("\\^a" "â") ; acir
           ("\"a" "ä") ; aum
           ("`a" "à") ; agrave
           ("\\*a" "å") ; aring
           ("\\*u" "ů") ; uring
           (",c" "ç") ; ccedil
           ("cced" "ç")
           ("\\^e" "ê") ; ecir
           ("\"e" "ë") ; eum
           ("`e" "è") ; egrave
           ("\"i" "ï") ; ium
           ("\\^i" "î") ; icir
           ("`i" "ì") ; igrave
           ("\"A" "Ä") ; Aum
           ("\\*A" "Å") ; Aring
           ("'E" "È") ; Eacute
           ("ae" "æ") ; ae
           ("AE" "Æ") ; AE
           ("\\^o" "ô") ; ocir
           ("\"o" "ö") ; oum
           ("`o" "ò") ; ograve
           ("'o" "ó") ; oacute
           ("Oacute" "Ó")
           ("\\^u" "û") ; ucir
           ("`u" "ù") ; ugrave
           ("'u" "ú") ; uacute
           ("\"y" "ÿ") ; yum
           ("\"O" "Ö") ; Oum
           ("\"U" "Ü")
           ("pound" "£")
           ("'a" "á") ; aacute
           ("'i" "í") ; iacute
           ("frac23" "⅔")
           ("frac13" "⅓")
           ("frac12" "½")
           ("frac14" "¼")
           ("\\?" "�")  ; Place-holder for unknown or illegible character.
           ("hand" "☞")  ; pointing hand (printer's u"fist") ; hand ; and
           ("\\.\\.\\." "…")
           ("\\*\\*\\*\\*\\*\\*\\*\\*" "✶")
           ("sect" "§") ; sect
           ("=a" "ā") ; amac
           ("ng" "ṉ")  ; u"n sub-macron" ; nsm
           ("sharp" "♯") ; sharp
           ("flat" "♭") ; flat
           ("th" "th") ; th
           ("=i" "ī") ; imac
           ("imac" "ī") ; imac
           ("=e" "ē") ; emac
           ("\\.d" "ḍ")  ; Sanskrit/Tamil d dot
           ("\\.n" "ṇ")  ; Sanskrit/Tamil n dot ; nsdot
           ("\\.t" "ṭ")  ; Sanskrit/Tamil t dot ; tsdot
           ("a\\^" "ă") ; acr
           ("e\\^" "ĕ") ; ecr
           ("i\\^" "ĭ") ; icr
           ("o\\^" "ŏ") ; ocr
           ("!o" "ǒ")
           ("OE" "Œ") ; OE
           ("oe" "œ") ; oe
           ("=O" "Ō") ; Omac
           ("=o" "ō") ; omac
           ("=u" "ū") ; umac
           ("ocar" "ǒ") ; o hacek
           ("=ae" "ǣ") ; aemac
           ("u\\^" "ŭ") ; ucr
           ("a\\^" "ă")
           ("=y" "ȳ") ; ymac
           ("asl" "a")  ; FIXME: a u"semilong" (has a macron above with a short
           ("-e" "e")  ; FIXME: e u"semilong" ; esl
           ("-i" "i")  ; FIXME: i u"semilong" ; isl
           ("-o" "o")  ; FIXME: o u"semilong" ; osl
           ("-u" "u")  ; FIXME: u u"semilong" ; usl
           ("-n" "ṉ") ; nsmac
           ("\\.a" "ȧ")  ; a with dot above ; adot
           ("\\.c" "ċ")  ; cdot
           ("\\.h" "ḥ")  ; hsdot
           ("\\.m" "ṃ")  ; msdot
           ("\\.u" "ụ")  ; usdot
           ("\\.z" "ẓ")  ; zsdot
           ("Eth" "Ð") ; EDH
           ("eth" "ð") ; edh
           ("thorn" "þ") ; thorn
           ("~a" "ã") ; atil
           ("~e" "ẽ") ; etil
           ("itil" "ĩ")
           ("otil" "õ")
           ("util" "ũ")
           ("~n" "ñ") ; ntil
           ("Atil" "Ã")
           ("Etil" "Ẽ")
           ("Itil" "Ĩ")
           ("Otil" "Õ")
           ("Util" "Ũ")
           ("~N" "Ñ") ; Ntil
           ("\\.n" "ṅ") ; ndot
           ("\\.r" "ṛ") ; rsdot
           ("yogh" "ȝ") ; yogh
           ("deg" "°")
           ("middot" "•")
           ("root" "√")
           ;; Greek letters
           ("alpha" "α")
           ("beta" "β")
           ("gamma" "γ")
           ("delta" "δ")
           ("epsilon" "ε")
           ("zeta" "ζ")
           ("eta" "η")
           ("theta" "θ")
           ("iota" "ι")
           ("approx" "κ") ; ap
           ("lambda" "λ")
           ("mu" "μ")
           ("nu" "ν")
           ("xi" "ξ")
           ("omicron" "ο")
           ("pi" "π")
           ("rho" "ρ")
           ("sigma" "σ")
           ("sigmat" "ς")
           ("tau" "τ")
           ("upsilon" "υ")
           ("phi" "φ")
           ("chi" "χ")
           ("psi" "ψ")
           ("omega" "ω")
           ("digamma" "ϝ")
           ("ALPHA" "Α")
           ("BETA" "Β")
           ("Gamma" "Γ") ; GAMMA
           ("Delta" "Δ") ; DELTA
           ("EPSILON" "Ε")
           ("ZETA" "Ζ")
           ("ETA" "Η")
           ("Theta" "Θ") ; THETA
           ("IOTA" "Ι")
           ("KAPPA" "Κ")
           ("Lambda" "Λ") ; LAMBDA
           ("MU" "Μ")
           ("NU" "Ν")
           ("XI" "Ξ")
           ("Omicron" "Ο") ; OMICRON
           ("Pi" "Π") ; PI
           ("RHO" "Ρ")
           ("Sigma" "Σ") ; SIGMA
           ("Tau" "Τ") ; TAU
           ("Upsilon" "Υ") ; UPSILON
           ("PHI" "Φ")
           ("Chi" "Χ") ; CHI
           ("PSI" "Ψ")
           ("Omega" "Ω") ; OMEGA
           ;; FIXME: Vowels with a double dot below. There`s nothing suitable in the Unicode
           ("add" "a")
           ("udd" "u")
           ("ADD" "A")
           ("UDD" "U")
           ;; Quotes
           ("dagger" "†")
           ("dag" "†")
           ("u\\^" "§") ; par
           ("and" "and")
           ("or" "or")
           ("sec" "˝")
           ("[,C]" "Ç")
           ("\"u" "ü") ; uum
           ("'e" "é") ; eacute
           ("\\^a" "â") ; acir
           ("\"a" "ä") ; aum
           ("`a" "à") ; agrave
           ("\\*a" "å") ; aring
           ("\\*u" "ů") ; uring
           (",c" "ç") ; ccedil
           ("cced" "ç")
           ("\\^e" "ê") ; ecir
           ("\"e" "ë") ; eum
           ("`e" "è") ; egrave
           ("\"i" "ï") ; ium
           ("\\^i" "î") ; icir
           ("`i" "ì") ; igrave
           ("\"A" "Ä") ; Aum
           ("\\*A" "Å") ; Aring
           ("'E" "È") ; Eacute
           ("ae" "æ") ; ae
           ("AE" "Æ") ; AE
           ("\\^o" "ô") ; ocir
           ("\"o" "ö") ; oum
           ("`o" "ò") ; ograve
           ("'o" "ó") ; oacute
           ("Oacute" "Ó")
           ("\\^u" "û") ; ucir
           ("`u" "ù") ; ugrave
           ("'u" "ú") ; uacute
           ("\"y" "ÿ") ; yum
           ("\"O" "Ö") ; Oum
           ("\"U" "Ü")
           ("pound" "£")
           ("'a" "á") ; aacute
           ("'i" "í") ; iacute
           ("frac23" "⅔")
           ("frac13" "⅓")
           ("frac12" "½")
           ("frac14" "¼")
           ;; ("\\?" "�")  ; Place-holder for unknown or illegible character.
           ("hand" "☞")  ; pointing hand (printer's u"fist") ; hand ; and
           ("sect" "§") ; sect
           ("=a" "ā") ; amac
           ("ng" "ṉ")  ; u"n sub-macron" ; nsm
           ("sharp" "♯") ; sharp
           ("flat" "♭") ; flat
           ("th" "th") ; th
           ("=i" "ī") ; imac
           ("=e" "ē") ; emac
           ("\\.d" "ḍ")  ; Sanskrit/Tamil d dot
           ("\\.n" "ṇ")  ; Sanskrit/Tamil n dot ; nsdot
           ("\\.t" "ṭ")  ; Sanskrit/Tamil t dot ; tsdot
           ("a\\^" "ă") ; acr
           ("e\\^" "ĕ") ; ecr
           ("i\\^" "ĭ") ; icr
           ("o\\^" "ŏ") ; ocr
           ("!o" "ǒ")
           ("OE" "Œ") ; OE
           ("oe" "œ") ; oe
           ("=O" "Ō") ; Omac
           ("=o" "ō") ; omac
           ("=u" "ū") ; umac
           ("ocar" "ǒ") ; o hacek
           ("=ae" "ǣ") ; aemac
           ("u\\^" "ŭ") ; ucr
           ("a\\^" "ă")
           ("=y" "ȳ") ; ymac
           ("asl" "a")  ; FIXME: a u"semilong" (has a macron above with a short
           ("-e" "e")  ; FIXME: e u"semilong" ; esl
           ("-i" "i")  ; FIXME: i u"semilong" ; isl
           ("-o" "o")  ; FIXME: o u"semilong" ; osl
           ("-u" "u")  ; FIXME: u u"semilong" ; usl
           ("-n" "ṉ") ; nsmac
           ("\\.a" "ȧ")  ; a with dot above ; adot
           ("\\.c" "ċ")  ; cdot
           ("\\.h" "ḥ")  ; hsdot
           ("\\.m" "ṃ")  ; msdot
           ("\\.u" "ụ")  ; usdot
           ("\\.z" "ẓ")  ; zsdot
           ("Eth" "Ð") ; EDH
           ("eth" "ð") ; edh
           ("thorn" "þ") ; thorn
           ("~a" "ã") ; atil
           ("~e" "ẽ") ; etil
           ("itil" "ĩ")
           ("otil" "õ")
           ("util" "ũ")
           ("~n" "ñ") ; ntil
           ("Atil" "Ã")
           ("Etil" "Ẽ")
           ("Itil" "Ĩ")
           ("Otil" "Õ")
           ("Util" "Ũ")
           ("~N" "Ñ") ; Ntil
           ("\\.n" "ṅ") ; ndot
           ("\\.r" "ṛ") ; rsdot
           ("yogh" "ȝ") ; yogh
           ("deg" "°")
           ("middot" "•")
           ("root" "√")
           ;; Greek letters
           ("alpha" "α")
           ("beta" "β")
           ("gamma" "γ")
           ("delta" "δ")
           ("epsilon" "ε")
           ("zeta" "ζ")
           ("eta" "η")
           ("theta" "θ")
           ("iota" "ι")
           ("approx" "κ") ; ap
           ("lambda" "λ")
           ("mu" "μ")
           ("nu" "ν")
           ("xi" "ξ")
           ("omicron" "ο")
           ("pi" "π")
           ("rho" "ρ")
           ("sigma" "σ")
           ("sigmat" "ς")
           ("tau" "τ")
           ("upsilon" "υ")
           ("phi" "φ")
           ("chi" "χ")
           ("psi" "ψ")
           ("omega" "ω")
           ("digamma" "ϝ")
           ("ALPHA" "Α")
           ("BETA" "Β")
           ("Gamma" "Γ") ; GAMMA
           ("Delta" "Δ") ; DELTA
           ("EPSILON" "Ε")
           ("ZETA" "Ζ")
           ("ETA" "Η")
           ("Theta" "Θ") ; THETA
           ("IOTA" "Ι")
           ("KAPPA" "Κ")
           ("Lambda" "Λ") ; LAMBDA
           ("MU" "Μ")
           ("NU" "Ν")
           ("XI" "Ξ")
           ("Omicron" "Ο") ; OMICRON
           ("Pi" "Π") ; PI
           ("RHO" "Ρ")
           ("Sigma" "Σ") ; SIGMA
           ("Tau" "Τ") ; TAU
           ("Upsilon" "Υ") ; UPSILON
           ("PHI" "Φ")
           ("Chi" "Χ") ; CHI
           ("PSI" "Ψ")
           ("Omega" "Ω") ; OMEGA
           ;; FIXME: Vowels with a double dot below. There`s nothing suitable in the Unicode
           ("add" "a")
           ("udd" "u")
           ("ADD" "A")
           ("UDD" "U")
           ;; Quotes
           ("dagger" "†")
           ("dag" "†")
           ("u\\^" "§") ; par
           ("and" "and")
           ("or" "or")
           ("times" "×")
           ("sec" "˝"))))
    (setq pronunciation
          (replace-regexp-in-string
           "\\.\\.\\." "…"
           (replace-regexp-in-string
            "\\*\\*\\*\\*\\*\\*\\*\\*" "✶"
            pronunciation)))
    (dolist (dcrt diacritics)
      (setq pronunciation (replace-regexp-in-string
                           (concat "\\[" (car dcrt) "\\]")
                           (cadr dcrt)
                           pronunciation t)))
    pronunciation))

(defun lexic-format-reflow-text (text max-width &optional min-width initial-colunm indent sepregex)
  "Add newlines as required to ensure that TEXT never exceeds MAX-WIDTH columns.

Can also set a MIN-WIDTH for new lines of text created by a line break,
an INITIAL-COLUNM that the text starts at, and an INDENT string to be inserted
after every line break.

When regex SEPREGEX is provided, it is used to detect word separators.
It is \"[ ,.]\" by default."
  (let* ((initial-col (or initial-colunm 0))
         (min-width (or min-width 1))
         (indent (or indent ""))
         (sepregex (or sepregex "[ ,.]"))
         (line-regex (format "\\(\\`.\\{%d,%d\\}\\(?:%s\\(?:.\\{1,%d\\}\\'\\)?\\|\\'\\)\\|.\\{%d\\}\\)\\(.*\\)"
                             min-width (- max-width (length indent))
                             sepregex min-width (- max-width (length indent))))
         reflowed-text)
    (setq text (replace-regexp-in-string "[[:space:]]+" " " text))
    (setq text
          (if (> initial-col max-width)
              (replace-regexp-in-string "\\` " "" text)
            (replace-regexp-in-string
             (format "\\`.\\{%d,%d\\}%s\\(?:.\\{1,%d\\}\\'\\)?\\|\\`"
                     (min min-width (- max-width initial-col)) (- max-width initial-col)
                     sepregex min-width)
             (lambda (match)
               (setq reflowed-text match)
               "")
             text)))
    (while (not (string-empty-p text))
      (setq text
            (if (<= (length text) max-width)
                (progn (setq reflowed-text (concat reflowed-text
                                                   (unless (string-empty-p reflowed-text)
                                                     (concat "\n" indent))
                                                   text)) "")
              (replace-regexp-in-string
               line-regex
               (lambda (match)
                 (setq reflowed-text (concat reflowed-text "\n" indent (match-string 1 match)))
                 (match-string 2 match))
               text))))
    reflowed-text))

(defun lexic-format-online-etym (entry &optional _expected-word)
  "Make an html ENTRY look nice.
Designed for an export of Douglas Harper's Online Etymology Dictionary,
collected using https://framagit.org/tuxor1337/dictmaster."
  (thread-last
      (string-join
       (delq nil
             (mapcar
              (lambda (e)
                (when (string= (plist-get e :dict)
                               (plist-get entry :dict))
                  (plist-get e :info)))
              (lexic-parse-results
               (lexic-oneshot-lookup
                (replace-regexp-in-string " ?(.*)" " (*)" (plist-get entry :word)) ; lexic accepts a glob
                t (list "-0" "-u" (plist-get entry :dict)))))))
    (replace-regexp-in-string
     "\\(?:\\`\\|\n\n\\)<b>\\(.+?\\) (\\(.+?\\)\\([0-9]+\\)?)</b> ?"
     (lambda (match)
       (let ((word (match-string 1 match))
             (pos (lexic-format-expand-abbreviations (match-string 2 match)))
             (index (match-string 3 match)))
         (concat "\n\n\u200B\u200B\u200B"
                 (propertize word 'face 'bold)
                 " "
                 (propertize pos 'face '(bold font-lock-keyword-face))
                 (when index
                   (propertize (concat " " index) 'face '(italic font-lock-doc-face)))
                 "\u2008\n\n"))))
    (replace-regexp-in-string
     "<i>\\(.*?\\)</i>"
     (lambda (match) (propertize (match-string 1 match) 'face 'italic)))
    (replace-regexp-in-string
     "<b>\\(.*?\\)</b>"
     (lambda (match) (propertize (match-string 1 match) 'face 'bold)))
    (replace-regexp-in-string
     "<strong>\\(.*?\\)</strong>"
     (lambda (match) (propertize (match-string 1 match) 'face 'font-lock-constant-face)))
    (replace-regexp-in-string
     "<a href=\".*?\">\\(.*?\\)</a>"
     (lambda (match) (propertize (match-string 1 match) 'face 'font-lock-keyword-face)))
    (replace-regexp-in-string
     "<span style=\".*?\">\\(.*?\\)</span>"
     (lambda (match) (propertize (match-string 1 match) 'face 'font-lock-doc-face)))
    (replace-regexp-in-string
     "[0-9]\\{4\\}s?\\|[0-9]+c\\."
     (lambda (match) (propertize match 'face 'font-lock-string-face)))
    (replace-regexp-in-string
     "<span>\\(.*?\\)</span>\\( (.+?)\\)?"
     (lambda (match)
       (let ((linked (match-string 1 match))
             (pos (lexic-format-expand-abbreviations (match-string 2 match))))
         (concat
          (propertize linked 'face 'font-lock-keyword-face)
          (when pos (propertize (replace-regexp-in-string "\\([0-9]+\\))" " \\1)" pos)
                                'face '(bold diff-context)))))))
    (replace-regexp-in-string
     "<blockquote>\\(.+?\\) ?\\[\\(.+\\)\\]</blockquote>"
     (lambda (match)
       (concat "❝"
               (propertize
                (lexic-format-reflow-text
                 (match-string 1 match) 80 5 1 " ")
                'face 'diff-context)
               "❞\n"
               (propertize (concat " ──"
                                   (lexic-format-reflow-text (match-string 2 match)
                                                             75 5 3 "   "))
                           'face '(italic font-lock-type-face)))))
    (replace-regexp-in-string "<br/>\n?<br/>" "\n")
    (replace-regexp-in-string
     "<p>\\(.*?\\)</p>"
     (lambda (match)
       (concat
        (lexic-format-reflow-text (match-string 1 match)
                                  80 5)
        "\n")))
    (replace-regexp-in-string "</?p>" "") ; any straggling pars
    (replace-regexp-in-string
     "^.\\{86,\\}"
     (lambda (match)
       (lexic-format-reflow-text match 80 5)))))

(defun lexic-format-element (entry &optional _expected-word)
  "Make an ENTRY for an element Look nice.
Based on http://download.huzheng.org/dict.org/stardict-dictd_www.dict.org_elements-2.4.2.tar.bz2."
  (replace-regexp-in-string
   "^\\([a-z]+\\)
Symbol: \\([A-Za-z]+\\)
Atomic number: \\([0-9]+\\)
Atomic weight: \\((?[0-9.]+)?\\)"
   (lambda (match)
     (let ((element (match-string 1 match))
           (symbol (match-string 2 match))
           (number (match-string 3 match))
           (weight (match-string 4 match)))
       (format
        "┌────────────────┐
│ %-3s %10s │
│ %s %11s │
└────────────────┘
"
        (propertize number 'face 'font-lock-function-name-face)
        (propertize weight 'face 'font-lock-comment-face)
        (propertize symbol 'face '(bold font-lock-keyword-face))
        (propertize element 'face 'font-lock-string-face))))
   (plist-get entry :info)))

(defun lexic-format-soule (entry &optional _expected-word)
  "Format an ENTRY for WORD in Soule's Dictionary of English Synonyms.
Designed using http://download.huzheng.org/bigdict/stardict-Soule_s_Dictionary_of_English_Synonyms-2.4.2.tar.bz2."
  (thread-last (plist-get entry :info)
       (replace-regexp-in-string ; categories
        "^\\([IVX]+\\. \\)?\\([a-z.;& ]+\\)"
        (lambda (match)
          (concat
           "\u200B\u200B\u200B"
           (when case-fold-search
             (propertize
              'face '(bold font-lock-constant-face)))
           (propertize (lexic-format-expand-abbreviations (match-string 2 match))
                       'face '(bold font-lock-keyword-face))
           "\u2008")))
       (replace-regexp-in-string
        "^\\([0-9]+\\.\\) \n\\([^,.]*,?\\)"
        (lambda (match)
          (concat
           "\u200B\u200B\u200B\u200B"
           (propertize (match-string 1 match)
                       'face '(bold font-lock-string-face))
           " "
           (match-string 2 match)
           "\u2008")))
       (replace-regexp-in-string
        "^\\(.\\{81\\}\\)"
        (lambda (match)
          (lexic-format-reflow-text match 80 1 0 "   " "[, ]")))
       (replace-regexp-in-string
        ","
        (propertize "," 'face 'font-lock-type-face))))

;;;###autoload
(defun lexic-dictionary-help ()
  "Show the Lexic help page."
  (interactive)
  (let ((dict-help-buf (get-buffer-create "*lexic-dict-help*")))
    (with-current-buffer dict-help-buf
      (setq buffer-read-only t)
      (with-silent-modifications
        (erase-buffer)
        (insert "#+title: Lexic Dictionary Help
#+author: TEC

* sdcv

First you want to make sure you have the *stardict* CLI tool =sdcv=.
It should be available in [[https://repology.org/project/sdcv/versions][many package repositories]] under that name.

Not that by itself =sdcv= does not come with any dictionaries, you'll need to install those yourself.

* Downloading dictionaries

You can find quite a few dictionaries on http://download.huzheng.org/dict.org/
and http://download.huzheng.org/bigdict/. By default, Lexic will provide nice
formatting for some of these. Namely:

| Dictionary             | Recognised ~bookname~                            |
|------------------------+------------------------------------------------|
| [[http://download.huzheng.org/dict.org/stardict-dictd-web1913-2.4.2.tar.bz2.][Webster 1913]]        | =Webster's Revised Unabridged Dictionary (1913)= |
| [[http://download.huzheng.org/dict.org/stardict-dictd_www.dict.org_elements-2.4.2.tar.bz2][Elements]]             | =Elements database=                              |
| Hitchcock's Bible Name | =Hitchcock's Bible Names Dictionary=             |
| Online Etymology       | =Online Etymology Dictionary=                    |
| [[http://download.huzheng.org/bigdict/stardict-Soule_s_Dictionary_of_English_Synonyms-2.4.2.tar.bz2][Soule's Synonyms]]     | =Soule's Dictionary of English Synonyms=         |

The stardict form of the /Online Etymology Dictionary/ can be created by running
[[https://framagit.org/tuxor1337/dictmaster][dictmaster]]. Unfortunately I do not know of any nice hosted result of this.

* Installing dictionaries

By default =sdcv= will look for dictionaries in =$HOME/.stardict/dic=, (do take note
of the =dic= part of the path) but this can be changed by setting environment
variable =STARDICT_DATA_DIR=.

I recommend creating a folder for each dictionary in =STARDICT_DATA_DIR=, for
example:

#+begin_example
$STARDICT_DATA_DIR
├── elements
│   └── ...
├── etymology
│   └── ...
├── hitchcock
│   └── ...
├── synonyms
│   ├── SoulesSynonyms.dict.dz
│   ├── SoulesSynonyms.idx
│   └── SoulesSynonyms.ifo
└── webster
    └── ...
#+end_example

A particular dictionary is composed of a few files:
+ =DICT.dict.dz=
+ =DICT.idx=
+ =DICT.ifo=

For our purposes, we only care about the =.ifo= (info) file. It should look
something like this.
#+begin_example
StarDict's dict ifo file
version=2.4.2
wordcount=160161
idxfilesize=3024035
bookname=Webster's Revised Unabridged Dictionary (1913)
description=Connoisseur's reference to American English
date=2007.8.28
sametypesequence=m
#+end_example

We are particularly interested in the ~bookname=~ line, as if you have one of the
[[Downloading dictionaries][recognised dictionaries]] in order to get the nice formatting you need to set the
~bookname~ to the recognised value.

* Using Lexic

Once you have =sdcv= and some dictionaries installed, just go ahead and try =M-x
lexic-search= 🙂."))
      (goto-char (point-min))
      (org-mode)
      (set-transient-map special-mode-map t))
    (switch-to-buffer-other-window dict-help-buf)))

(provide 'lexic)
;;; lexic.el ends here
