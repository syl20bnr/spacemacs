;;; biblio-core.el --- A framework for looking up and displaying bibliographic entries -*- lexical-binding: t -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>
;; Version: 0.3
;; Package-Requires: ((emacs "24.3") (let-alist "1.0.4") (seq "1.11") (dash "2.12.1"))
;; Keywords: bib, tex, convenience, hypermedia
;; URL: https://github.com/cpitclaudel/biblio.el

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; A framework for browsing bibliographic search results.  This is the core
;; package; for user interfaces, see any of `biblio-crossref', `biblio-dblp', `biblio-doi',
;; `biblio-arxiv', `biblio-hal' and `biblio-dissemin', which are part of the `biblio' package.

;;; Code:

(require 'bibtex)
(require 'browse-url)
(require 'hl-line)
(require 'ido)
(require 'json)
(require 'url-queue)

(require 'dash)
(require 'let-alist)
(require 'seq)

(defvar-local biblio--target-buffer nil
  "Buffer into which BibTeX entries should be inserted.
This variable is local to each search results buffer.")

(defvar-local biblio--search-terms nil
  "Keywords that led to a page of bibliographic search results.")

(defvar-local biblio--backend nil
  "Backend that produced a page of bibliographic search results.")

(defgroup biblio nil
  "A browser for bibliographic information."
  :group 'communication)

(defgroup biblio-core nil
  "Core of the biblio package."
  :group 'biblio)

(defgroup biblio-faces nil
  "Faces of the biblio package."
  :group 'biblio)

(defcustom biblio-synchronous nil
  "Whether bibliographic queries should be synchronous."
  :group 'biblio-core
  :type 'boolean)

(defcustom biblio-authors-limit 10
  "Maximum number of authors to display per paper."
  :group 'biblio-core
  :type 'integer)

;;; Compatibility

(defun biblio-alist-get (key alist)
  "Copy of Emacs 25's `alist-get', minus default.
Get the value associated to KEY in ALIST, or nil."
  (cdr (assq key alist)))

(defun biblio--plist-to-alist (plist)
  "Copy of Emacs 25's `json--plist-to-alist'.
Return an alist of the property-value pairs in PLIST."
  (let (res)
    (while plist
      (let ((prop (pop plist))
            (val (pop plist)))
        (push (cons prop val) res)))
    (nreverse res)))

;;; Utilities

(defconst biblio--bibtex-entry-format
  (list 'opts-or-alts 'numerical-fields 'page-dashes 'whitespace
        'inherit-booktitle 'realign 'last-comma 'delimiters
        'unify-case 'braces 'strings 'sort-fields)
  "Format to use in `biblio-format-bibtex'.
See `bibtex-entry-format' for details; this list is all
transformations, except errors for missing fields.
Also see `biblio-cleanup-bibtex-function'.")

(defun biblio--cleanup-bibtex-1 (dialect autokey)
  "Cleanup BibTeX entry starting at point.
DIALECT is `BibTeX' or `biblatex'.  AUTOKEY: see `biblio-format-bibtex'."
  (let ((bibtex-entry-format biblio--bibtex-entry-format)
        (bibtex-align-at-equal-sign t)
        (bibtex-autokey-edit-before-use nil))
    ;; Use biblatex to allow for e.g. @Online
    ;; Use BibTeX to allow for e.g. @TechReport
    (bibtex-set-dialect dialect t)
    (bibtex-clean-entry autokey)))

(defun biblio--cleanup-bibtex (autokey)
  "Default value of `biblio-cleanup-bibtex-function'.
AUTOKEY: See `biblio-format-bibtex'."
  (save-excursion
    (when (search-forward "@data{" nil t)
      (replace-match "@misc{")))
  (ignore-errors ;; See https://github.com/crosscite/citeproc-doi-server/issues/12
    (condition-case _
        (biblio--cleanup-bibtex-1 'biblatex autokey)
      (error (biblio--cleanup-bibtex-1 'BibTeX autokey)))))

(defcustom biblio-cleanup-bibtex-function
  #'biblio--cleanup-bibtex
  "Function to clean up BibTeX entries.
This function is called in a `bibtex-mode' buffer containing an
unprocessed, potentially invalid BibTeX (or BibLaTeX) entry, and
should clean it up in place.  It should take a single argument,
AUTOKEY, indicating whether the entry needs a new key."
  :group 'biblio
  :type 'function)

(defun biblio-format-bibtex (bibtex &optional autokey)
  "Format BIBTEX entry.
With non-nil AUTOKEY, automatically generate a key for BIBTEX."
  (with-temp-buffer
    (bibtex-mode)
    (save-excursion
      (insert (biblio-strip bibtex)))
    (if (fboundp 'font-lock-ensure) (font-lock-ensure)
      (with-no-warnings (font-lock-fontify-buffer)))
    (when (functionp biblio-cleanup-bibtex-function)
      (funcall biblio-cleanup-bibtex-function autokey))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun biblio--beginning-of-response-body ()
  "Move point to beginning of response body."
  (goto-char (point-min))
  (unless (re-search-forward "^\n" nil t)
    (error "Invalid response from server: %S" (buffer-string))))

(defun biblio-response-as-utf-8 ()
  "Extract body of response."
  (set-buffer-multibyte t)
  (decode-coding-region (point) (point-max) 'utf-8 t))

(defun biblio-decode-url-buffer (coding)
  "Decode URL buffer with CODING."
  (set-buffer-multibyte t) ;; URL buffer is unibyte
  (decode-coding-region (point-min) (point-max) coding))

(defun biblio--event-error-code (event)
  "Extract HTTP error code from EVENT, if any."
  (pcase event
    (`(:error . (error ,source ,details))
     (cons source details))))

(eval-and-compile
  (define-error 'biblio--url-error "URL retrieval error."))

(defun biblio--throw-on-unexpected-errors (errors allowed-errors)
  "Throw an url-error for any error in ERRORS not in ALLOWED-ERRORS."
  (dolist (err errors)
    (cond ((eq (car err) 'url-queue-timeout)
           (signal 'biblio--url-error 'timeout))
          ((not (member err allowed-errors))
           (signal 'biblio--url-error err)))))

(defun biblio--extract-errors (events)
  "Extract errors from EVENTS."
  (delq nil (mapcar #'biblio--event-error-code (biblio--plist-to-alist events))))

(defun biblio-generic-url-callback (callback &optional cleanup-function &rest allowed-errors)
  "Make an `url'-ready callback from CALLBACK.
CALLBACK is called with no arguments; the buffer containing the
server's response is current at the time of the call, and killed
after the call returns.  Call CLEANUP-FUNCTION before checking
for errors.  If the request returns one of the errors in
ALLOWED-ERRORS, CALLBACK is instead called with one argument, the
list of allowed errors that occurred instead of a buffer.  If the
request returns another error, an exception is raised."
  (lambda (events)
    (let ((target-buffer (current-buffer)))
      (unwind-protect
          (progn
            (funcall (or cleanup-function #'ignore))
            (condition-case err
                (-if-let* ((errors (biblio--extract-errors events)))
                    (progn
                      (biblio--throw-on-unexpected-errors errors allowed-errors)
                      (funcall callback errors))
                  (biblio--beginning-of-response-body)
                  (delete-region (point-min) (point))
                  (funcall callback))
              (error (message "Error while processing request: %S" err))))
        (kill-buffer target-buffer)))))

(defun biblio-url-retrieve (url callback)
  "Wrapper around `url-queue-retrieve'.
URL and CALLBACK; see `url-queue-retrieve'"
  (message "Fetching %s" url)
  (if biblio-synchronous
      (with-current-buffer (url-retrieve-synchronously url)
        (funcall callback nil))
    (setq url-queue-timeout 5)
    (url-queue-retrieve url callback)))

(defun biblio-strip (str)
  "Remove spaces surrounding STR."
  (when str
    (->> str
         (replace-regexp-in-string "[ \t\n\r]+\\'" "")
         (replace-regexp-in-string "\\`[ \t\n\r]+" ""))))

(defun biblio-cleanup-doi (doi)
  "Cleanup DOI string."
  (biblio-strip (replace-regexp-in-string "https?://\\(dx\\.\\)?doi\\.org/" "" doi)))

(defun biblio-remove-empty (strs)
  "Remove empty sequences from STRS."
  (seq-remove #'seq-empty-p strs))

(defun biblio-join-1 (sep strs)
  "Join non-empty elements of STRS with SEP."
  (declare (indent 1))
  (let ((strs (biblio-remove-empty strs)))
    (mapconcat #'identity strs sep)))

(defun biblio-join (sep &rest strs)
  "Join non-empty elements of STRS with SEP."
  (declare (indent 1))
  (biblio-join-1 sep strs))

(defmacro biblio--with-text-property (prop value &rest body)
  "Set PROP to VALUE on text inserted by BODY."
  (declare (indent 2)
           (debug t))
  (let ((beg-var (make-symbol "beg")))
    `(let ((,beg-var (point)))
       ,@body
       (put-text-property ,beg-var (point) ,prop ,value))))

(defmacro biblio-with-fontification (face &rest body)
  "Apply FACE to text inserted by BODY."
  (declare (indent 1)
           (debug t))
  (let ((beg-var (make-symbol "beg")))
    `(let ((,beg-var (point)))
       ,@body
       (font-lock-append-text-property ,beg-var (point) 'face ,face))))

;;; Help with major mode

(defsubst biblio--as-list (x)
  "Make X a list, if it isn't."
  (if (consp x) x (list x)))

(defun biblio--map-keymap (func map)
  "Call `map-keymap' on FUNC and MAP, and collect the results."
  (let ((out))
    (map-keymap (lambda (&rest args) (push (apply func args) out)) map)
    out))

(defun biblio--flatten-map (keymap &optional prefix)
  "Flatten KEYMAP, prefixing its keys with PREFIX.
This should really be in Emacs core (in Elisp), instead of being
implemented in C (at least for sparse keymaps).  Don't run this on
non-sparse keymaps."
  (nreverse
   (cond
    ((keymapp keymap)
     (seq-map (lambda (key-value)
                "Add PREFIX to key in KEY-VALUE."
                (cons (append prefix (biblio--as-list (car key-value)))
                      (cdr key-value)))
              (delq nil
                    (apply
                     #'seq-concatenate
                     'list (biblio--map-keymap
                            (lambda (k v)
                              "Return a list of bindings in V, prefixed by K."
                              (biblio--flatten-map v (biblio--as-list k)))
                            keymap)))))
    ;; FIXME This breaks if keymap is a symbol whose function cell is a keymap
    ((symbolp keymap)
     (list (cons prefix keymap))))))

(defun biblio--group-alist (alist)
  "Return a copy of ALIST whose keys are lists of keys, grouped by value.
That is, if two key map to `eq' values, they are grouped."
  (let ((map (make-hash-table :test 'eq))
        (new-alist nil))
    (pcase-dolist (`(,key . ,value) alist)
      (puthash value (cons key (gethash value map)) map))
    (pcase-dolist (`(,_ . ,value) alist)
      (-when-let* ((keys (gethash value map)))
        (push (cons (nreverse keys) value) new-alist)
        (puthash value nil map)))
    (nreverse new-alist)))

(defun biblio--quote (str)
  "Quote STR and call `substitute-command-keys' on it."
  (if str (substitute-command-keys (concat "`" str "'")) ""))

(defun biblio--quote-keys (keys)
  "Quote and concatenate keybindings in KEYS."
  (mapconcat (lambda (keyseq)
               (biblio--quote (ignore-errors (help-key-description keyseq nil))))
             keys ", "))

(defun biblio--brief-docs (command)
  "Return first line of documentation of COMMAND."
  (let ((docs (or (ignore-errors (documentation command t)) "")))
    (string-match "\\(.*\\)$" docs)
    (match-string-no-properties 1 docs)))

(defun biblio--help-with-major-mode-1 (keyseqs-command)
  "Print help on KEYSEQS-COMMAND to standard output."
  (insert (biblio--quote-keys (car keyseqs-command)) " ")
  (insert (propertize "\t" 'display '(space :align-to 10)))
  (insert-text-button (format "%S" (cdr keyseqs-command)))
  (insert "\n")
  (biblio-with-fontification '(font-lock-comment-face (:height 0.95))
    (insert (format "  %s\n" (biblio--brief-docs (cdr keyseqs-command)))))
  (biblio-with-fontification '(:height 0.3)
    (insert "\n")))

(defun biblio--help-with-major-mode ()
  "Display help with current major mode."
  (let ((buf (format "*%S help*" major-mode)))
    (with-help-window buf
      (princ (format "Help with %s\n\n" (biblio--quote (symbol-name major-mode))))
      (let ((bindings (nreverse
                       (biblio--group-alist
                        (biblio--flatten-map
                         (current-local-map))))))
        (with-current-buffer buf
          (seq-do #'biblio--help-with-major-mode-1 bindings))))
    buf))

;;; Interaction

(defconst biblio--search-result-marker-regexp "^> "
  "Indicator of a search result.")

(defun biblio--selection-move (move-fn search-fn)
  "Move using MOVE-FN, then call SEARCH-FN and go to first match."
  (let ((target (point)))
    (save-excursion
      (funcall move-fn)
      (when (funcall search-fn biblio--search-result-marker-regexp nil t)
        (setq target (match-end 0))))
    (goto-char target)))

(defun biblio-get-url (metadata)
  "Compute a url from METADATA.
Uses .url, and .doi as a fallback."
  (let-alist metadata
    (if .url .url
      (when .doi
        (concat "https://doi.org/" (url-encode-url .doi))))))

(defun biblio--selection-browse ()
  "Open the web page of the current entry in a web browser."
  (interactive)
  (-if-let* ((url (biblio-get-url (biblio--selection-metadata-at-point))))
      (browse-url url)
    (user-error "This record does not contain a URL")))

(defun biblio--selection-browse-direct ()
  "Open the full text of the current entry in a web browser."
  (interactive)
  (-if-let* ((url (biblio-alist-get 'direct-url (biblio--selection-metadata-at-point))))
      (browse-url url)
    (user-error "This record does not contain a direct URL (try arXiv or HAL)")))

(defun biblio--selection-next ()
  "Move to next search result."
  (interactive)
  (biblio--selection-move #'end-of-line #'re-search-forward))

(defun biblio--selection-first ()
  "Move to first search result."
  (goto-char (point-min))
  (biblio--selection-move #'ignore #'re-search-forward))

(defun biblio--selection-previous ()
  "Move to previous search result."
  (interactive)
  (biblio--selection-move #'beginning-of-line #'re-search-backward))

(defun biblio--selection-copy-callback (bibtex entry)
  "Add BIBTEX (from ENTRY) to kill ring."
  (kill-new bibtex)
  (message "Killed bibtex entry for %S."
           (biblio--prepare-title (biblio-alist-get 'title entry))))

(defun biblio--selection-copy ()
  "Copy BibTeX of current entry at point."
  (interactive)
  (biblio--selection-forward-bibtex #'biblio--selection-copy-callback))

(defun biblio--selection-copy-quit ()
  "Copy BibTeX of current entry at point and close results."
  (interactive)
  (biblio--selection-forward-bibtex #'biblio--selection-copy-callback t))

(defun biblio--target-window ()
  "Get the window of the source buffer."
  (get-buffer-window biblio--target-buffer))

(defun biblio--selection-insert-callback (bibtex entry)
  "Add BIBTEX (from ENTRY) to kill ring."
  (let ((target-buffer biblio--target-buffer))
    (with-selected-window (or (biblio--target-window) (selected-window))
      (with-current-buffer target-buffer
        (insert bibtex "\n\n"))))
  (message "Inserted bibtex entry for %S."
           (biblio--prepare-title (biblio-alist-get 'title entry))))

(defun biblio--selection-insert ()
  "Insert BibTeX of current entry into source buffer."
  (interactive)
  (biblio--selection-forward-bibtex #'biblio--selection-insert-callback))

(defun biblio--selection-insert-quit ()
  "Insert BibTeX of current entry into source buffer and close results."
  (interactive)
  (biblio--selection-forward-bibtex #'biblio--selection-insert-callback t))

(defun biblio--selection-metadata-at-point ()
  "Return the metadata of the entry at point."
  (or (get-text-property (point) 'biblio-metadata)
      (user-error "No entry at point")))

(defcustom biblio-bibtex-use-autokey nil
  "Whether to generate new BibTeX keys for inserted entries."
  :type '(choice (const :tag "Keep original BibTeX keys" nil)
                 (const :tag "Generate new BibTeX keys" t))
  :group 'biblio
  :package-version '(biblio . "0.2.1"))

(defun biblio--selection-forward-bibtex (forward-to &optional quit)
  "Retrieve BibTeX for entry at point and pass it to FORWARD-TO.
If QUIT is set, also kill the results buffer."
  (let* ((metadata (biblio--selection-metadata-at-point))
         (results-buffer (current-buffer)))
    (progn
      (funcall (biblio-alist-get 'backend metadata)
               'forward-bibtex metadata
               (lambda (bibtex)
                 (with-current-buffer results-buffer
                   (funcall
                    forward-to
                    (biblio-format-bibtex bibtex biblio-bibtex-use-autokey)
                    metadata))))
      (when quit (quit-window)))))

(defun biblio--selection-change-buffer (buffer-name)
  "Change buffer in which BibTeX results will be inserted.
BUFFER-NAME is the name of the new target buffer."
  (interactive (list (read-buffer "Buffer to insert entries into: ")))
  (let ((buffer (get-buffer buffer-name)))
    (if (buffer-local-value 'buffer-read-only buffer)
        (user-error "%s is read-only" (buffer-name buffer))
      (setq biblio--target-buffer buffer))))

(defvar biblio-selection-mode-actions-alist nil
  "An alist of extensions for `biblio-selection-mode'.
Each element should be in the for (LABEL . FUNCTION); FUNCTION
will be called with the metadata of the current item.")

(defun biblio--completing-read-function ()
  "Return ido, unless user picked another completion package."
  (if (and (eq completing-read-function #'completing-read-default)
           (not (catch 'advised ;; https://github.com/cpitclaudel/biblio.el/issues/55
                  (advice-mapc (lambda (&rest _args) (throw 'advised t))
                               'completing-read-default))))
      #'ido-completing-read
    completing-read-function))

(defun biblio-completing-read (prompt collection &optional predicate require-match
                                      initial-input hist def inherit-input-method)
  "Complete using `biblio-completing-read-function'.
PROMPT, COLLECTION, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
HIST, DEF, INHERIT-INPUT-METHOD: see `completing-read'."
  (let ((completing-read-function (biblio--completing-read-function)))
    (completing-read prompt collection predicate require-match
                     initial-input hist def inherit-input-method)))

(defun biblio-completing-read-alist (prompt collection &optional predicate require-match
                                            initial-input hist def inherit-input-method)
  "Same as `biblio-completing-read', when COLLECTION in an alist.
Complete with the `car's, and return the `cdr' of the result.
PROMPT, COLLECTION, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
HIST, DEF, INHERIT-INPUT-METHOD: see `completing-read'."
  (let ((choices (mapcar #'car collection)))
    (cdr (assoc (biblio-completing-read
                 prompt choices predicate require-match
                 initial-input hist def inherit-input-method)
                collection))))

(defun biblio--read-selection-extended-action ()
  "Read an action from `biblio-selection-mode-actions-alist'."
  (biblio-completing-read-alist
   "Action: " biblio-selection-mode-actions-alist nil t))

(defun biblio--selection-extended-action (action)
  "Run an ACTION with metadata of current entry.
Interactively, query for ACTION from
`biblio-selection-mode-actions-alist'."
  (interactive (list (biblio--read-selection-extended-action)))
  (let* ((metadata (biblio--selection-metadata-at-point)))
    (funcall action metadata)))

(defun biblio--selection-help ()
  "Show help on local keymap."
  (interactive)
  (biblio--help-with-major-mode))

(defvar biblio-selection-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") #'biblio--selection-previous)
    (define-key map (kbd "C-p") #'biblio--selection-previous)
    (define-key map (kbd "p") #'biblio--selection-previous)
    (define-key map (kbd "<down>") #'biblio--selection-next)
    (define-key map (kbd "C-n") #'biblio--selection-next)
    (define-key map (kbd "n") #'biblio--selection-next)
    (define-key map (kbd "RET") #'biblio--selection-browse)
    (define-key map (kbd "<C-return>") #'biblio--selection-browse-direct)
    (define-key map (kbd "C-RET") #'biblio--selection-browse-direct)
    (define-key map (kbd "M-w") #'biblio--selection-copy)
    (define-key map (kbd "c") #'biblio--selection-copy)
    (define-key map (kbd "C-w") #'biblio--selection-copy-quit)
    (define-key map (kbd "C") #'biblio--selection-copy-quit)
    (define-key map (kbd "i") #'biblio--selection-insert)
    (define-key map (kbd "C-y") #'biblio--selection-insert-quit)
    (define-key map (kbd "I") #'biblio--selection-insert-quit)
    (define-key map (kbd "b") #'biblio--selection-change-buffer)
    (define-key map (kbd "x") #'biblio--selection-extended-action)
    (define-key map (kbd "?") #'biblio--selection-help)
    (define-key map (kbd "h") #'biblio--selection-help)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keybindings for Bibliographic search results.")

(defconst biblio--selection-mode-name-base "Bibliographic search results")

(defun biblio--selection-mode-name ()
  "Compute a modeline string for `biblio-selection-mode'."
  (concat biblio--selection-mode-name-base
          (if (bufferp biblio--target-buffer)
              (format " (→ %s)"
                      (buffer-name biblio--target-buffer))
            "")))

(defface biblio-highlight-extend-face `((t (:inherit highlight
						     ,@(and (>= emacs-major-version 27) '(:extend t)))))
  "Face used for highlighting lines."
  :group 'biblio-faces)

(define-derived-mode biblio-selection-mode fundamental-mode biblio--selection-mode-name-base
  "Browse bibliographic search results.
\\{biblio-selection-mode-map}"
  (setq-local hl-line-face 'biblio-highlight-extend-face)
  (hl-line-mode 1)
  (visual-line-mode)
  (setq-local truncate-lines nil)
  (setq-local cursor-type nil)
  (setq-local buffer-read-only t)
  (setq-local mode-name '(:eval (biblio--selection-mode-name)))
  (setq-local
   header-line-format
   `(:eval
     (concat
      (ignore-errors
        (propertize " " 'display '(space :align-to 0) 'face 'fringe))
      (substitute-command-keys
       (biblio-join "   "
         "\\[biblio--selection-help]: Help"
         "\\[biblio--selection-insert],\\[biblio--selection-insert-quit]: Insert BibTex"
         "\\[biblio--selection-copy],\\[biblio--selection-copy-quit]: Copy BibTeX"
         "\\[biblio--selection-extended-action]: Extended action"
         "\\[biblio--selection-browse]: Open in browser"
         "\\[biblio--selection-change-buffer]: Change buffer"))))))

;;; Printing search results

(defun biblio-parenthesize (str)
  "Add parentheses to STR, if not empty."
  (if (seq-empty-p str) ""
    (concat "(" str ")")))

(defun biblio-insert-with-prefix (prefix &rest strs)
  "Like INSERT with PREFIX and STRS, but set `wrap-prefix'.
That is, the inserted text gets a `wrap-prefix' made of enough
white space to align with the end of PREFIX."
  (declare (indent 1))
  (biblio--with-text-property 'wrap-prefix (make-string (length prefix) ?\s)
    (apply #'insert prefix strs)))

(defface biblio-detail-header-face
  '((t :slant normal))
  "Face used for headers of details in `biblio-selection-mode'."
  :group 'biblio-faces)

(defun biblio--insert-detail (prefix items newline)
  "Insert PREFIX followed by ITEMS, if ITEMS has non-empty entries.
If ITEMS is a list or vector, join its entries with “, ”.  If
NEWLINE is non-nil, add a newline before the main text."
  (when (or (vectorp items) (listp items))
    (setq items (biblio-join-1 ", " items)))
  (unless (seq-empty-p items)
    (when newline (insert "\n"))
    (let ((fontified (propertize prefix 'face 'biblio-detail-header-face)))
      (biblio-insert-with-prefix fontified items))))

(defun biblio--nonempty-string-p (str)
  "Return STR if STR is non-empty."
  (unless (seq-empty-p str)
    str))

(defun biblio--cleanup-field (text)
  "Cleanup TEXT for presentation to the user."
  (when text (biblio-strip (replace-regexp-in-string "[ \r\n\t]+" " " text))))

(defun biblio--prepare-authors (authors)
  "Cleanup and join list of AUTHORS."
  (let* ((authors (biblio-remove-empty (seq-map #'biblio-strip authors)))
         (num-authors (length authors)))
    ;; Only truncate when significantly above limit
    (when (> num-authors (+ 2 biblio-authors-limit))
      (let* ((last (nthcdr biblio-authors-limit authors)))
        (setcar last (format "… (%d more)" (- num-authors biblio-authors-limit)))
        (setcdr last nil)))
    (if authors (biblio-join-1 ", " authors)
      "(no authors)")))

(defun biblio--prepare-title (title &optional year)
  "Cleanup TITLE and add YEAR for presentation to the user."
  (concat (or (biblio--nonempty-string-p (biblio--cleanup-field title))
              "(no title)")
          (if year (format " [%s]" year) "")))

(defun biblio--browse-url (button)
  "Open web browser on page pointed to by BUTTON."
  (browse-url (button-get button 'target)))

(defun biblio-make-url-button (url &optional label)
  "Make a text button pointing to URL.
With non-nil LABEL, use that instead of URL to label the button."
  (unless (seq-empty-p url)
    (with-temp-buffer
      (insert-text-button (or label url)
                          'target url
                          'follow-link t
                          'action #'biblio--browse-url)
      (buffer-string))))

(defun biblio--references-redundant-p (references url)
  "Check whether REFERENCES are all containted in URL.

This is commonly the case with DOIs, which don't need to be
displayed if they are already in the `dx.doi.org' url."
  (and (stringp url)
       (seq-every-p (lambda (ref) (string-match-p (regexp-quote ref) url))
                    references)))

(defun biblio-insert-result (item &optional no-sep)
  "Print a (prepared) bibliographic search result ITEM.
With NO-SEP, do not add space after the record.

This command expects ITEM to be a single alist, in the following format:

  ((title . \"Title of entry\")
   (authors . (\"Author 1\" \"Author 2\" …))
   (container . \"Where this was published (which journal, conference, …)\")
   (type . \"Type of document (journal paper, proceedings, report, …)\")
   (category . \"Category of this document (aka primary topic)\")
   (publisher . \"Publisher of this document\")
   (references . \"Identifier(s) of this document (DOI, DBLP id, Handle, …)\")
   (open-access-status . \"Open access status of this document\")
   (url . \"Relevant URL\")
   (year . \"Publication year as a string, if available\")
   (direct-url . \"Direct URL of paper (typically PDF)\"))

Each of `container', `type', `category', `publisher',
`references', and `open-access-status' may be a list; in that
case, entries of the list are displayed comma-separated.  All
entries are optional.

`crossref--extract-interesting-fields' and `dblp--extract-interesting-fields'
provide examples of how to build such a result."
  (biblio--with-text-property 'biblio-metadata item
    (let-alist item
      (biblio-with-fontification 'font-lock-function-name-face
        (biblio-insert-with-prefix "> " (biblio--prepare-title .title .year)))
      (insert "\n")
      (biblio-with-fontification 'font-lock-doc-face
        (biblio-insert-with-prefix "  " (biblio--prepare-authors .authors)))
      (biblio-with-fontification 'font-lock-comment-face
        (biblio--insert-detail "  In: " .container t)
        (biblio--insert-detail "  Type: " .type t)
        (biblio--insert-detail "  Category: " .category t)
        (biblio--insert-detail "  Publisher: " .publisher t)
        ;; (-when-let* ((year (and (numberp .year) (number-to-string .year))))
        ;;   (if .publisher
        ;;       (insert (format " (%s)" year))
        ;;     (biblio--insert-detail "  Publication date: " year t)))
        (let ((references (remq nil .references)))
          (unless (biblio--references-redundant-p references .url)
            (biblio--insert-detail "  References: " references t)))
        (biblio--insert-detail "  Open Access: " .open-access-status t)
        (biblio--insert-detail "  URL: " (list (biblio-make-url-button .url)
                                         (biblio-make-url-button .direct-url))
                         t))
      (unless no-sep
        (insert "\n\n")))))

(defface biblio-results-header-face
  '((t :height 1.5 :weight bold :inherit font-lock-preprocessor-face))
  "Face used for general search results header in `biblio-selection-mode'."
  :group 'biblio-faces)

(defun biblio--search-results-header (&optional loading-p)
  "Compute a header for the current `selection-mode' buffer.
With LOADING-P, mention that results are being loaded."
  (format "%s search results for %s%s"
          (funcall biblio--backend 'name)
          (biblio--quote biblio--search-terms)
          (if loading-p " (loading…)" "")))

(defun biblio--make-results-buffer (target-buffer search-terms backend)
  "Set up the results buffer for TARGET-BUFFER, SEARCH-TERMS and BACKEND."
  (with-current-buffer (get-buffer-create
                        (format "*%s search*" (funcall backend 'name)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (biblio-selection-mode)
      (setq biblio--target-buffer target-buffer)
      (setq biblio--search-terms search-terms)
      (setq biblio--backend backend)
      (biblio--insert-header (biblio--search-results-header t))
      (setq buffer-read-only t)
      (current-buffer))))

(defun biblio--insert-header (header)
  "Prettify and insert HEADER in current buffer."
  (when header
    (biblio--with-text-property 'line-spacing 0.5
      (biblio--with-text-property 'line-height 1.75
        (biblio-with-fontification 'biblio-results-header-face
          (insert header "\n"))))))

(defun biblio-insert-results (items &optional header)
  "Populate current buffer with ITEMS and HEADER, then display it."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (biblio--insert-header header)
    (seq-do #'biblio-insert-result items))
  (pop-to-buffer (current-buffer))
  (biblio--selection-first)
  (hl-line-highlight))

(defun biblio--tag-backend (backend items)
  "Add (backend . BACKEND) to each alist in ITEMS."
  (seq-map (lambda (i) (cons `(backend . ,backend) i)) items))

(defun biblio--callback (results-buffer backend)
  "Generate a search results callback for RESULTS-BUFFER.
Results are parsed with (BACKEND 'parse-buffer)."
  (biblio-generic-url-callback
   (lambda () ;; no allowed errors, so no arguments
     "Parse results of bibliographic search."
     (let ((results (biblio--tag-backend backend (funcall backend 'parse-buffer))))
       (with-current-buffer results-buffer
         (biblio-insert-results results (biblio--search-results-header)))
       (message "Tip: learn to browse results with `h'")))))

;;; Searching

(defvar biblio--search-history nil)

(defvar biblio-backends nil
  "List of biblio backends.
This list is generally populated through `biblio-init-hook',
which is called by `biblio-collect-backends'.


Each backend is a function that take a variable number of
arguments.  The first argument is a command; the rest are
arguments to this specific command.  The command is one of the
following:

`name': (no arguments) The name of the backend, displayed when picking a
backend from a list.

`prompt': (no arguments) The string used when querying the user for a search
term to feed this backend.

`url': (one argument, QUERY) Create a URL to query the backend's API.

`parse-buffer': (no arguments) Parse the contents of the current
buffer and return a list of results.  At the time of the call,
the current buffer contains the results of querying a url
returned by (THIS-BACKEND `url' QUERY).  The format of individual
results is described in the docstring of `biblio-insert-result').

`forward-bibtex': (two arguments, METADATA and FORWARD-TO)
Produce a BibTeX record from METADATA (one of the elements of the
list produced by `parse-buffer') and call FORWARD-TO on it.

For examples of backends, see one of `biblio-crossref-backend',
`biblio-dblp-backend', `biblio-arxiv-backend', etc.


To register your backend automatically, you may want to add a
`register' command:

`register': Add the current backend to `biblio-backends'.
Something like (add-to-list \\='biblio-backends \\='THIS-BACKEND).

Then it's enough to add your backend to `biblio-init-hook':

;;;###autoload
\(add-hook \\='biblio-init-hook \\='YOUR-BACKEND-HERE).")

(defvar biblio-init-hook nil
  "Hook run before every search.
Each function is called with one argument, `register'.  This
makes it possible to register backends by adding them directly to
this hook, and making them react to `register' by adding
themselves to biblio-backends.")

(defun biblio-collect-backends ()
  "Populate `biblio-backends' and return that."
  (run-hook-with-args 'biblio-init-hook 'register)
  biblio-backends)

(defun biblio--named-backends ()
  "Collect an alist of (NAME . BACKEND)."
  (seq-map (lambda (b) (cons (funcall b 'name) b)) (biblio-collect-backends)))

(defun biblio--read-backend ()
  "Run `biblio-init-hook', then read a backend from `biblio-backend'."
  (biblio-completing-read-alist "Backend: " (biblio--named-backends) nil t))

(defun biblio--read-query (backend)
  "Interactively read a query.
Get prompt string from BACKEND."
  (let* ((prompt (funcall backend 'prompt)))
    (read-string prompt nil 'biblio--search-history)))

(defun biblio--lookup-1 (backend query)
  "Just like `biblio-lookup' on BACKEND and QUERY, but never prompt."
  (let ((results-buffer (biblio--make-results-buffer (current-buffer) query backend)))
    (biblio-url-retrieve
     (funcall backend 'url query)
     (biblio--callback results-buffer backend))
    results-buffer))

;;;###autoload
(defun biblio-lookup (&optional backend query)
  "Perform a search using BACKEND, and QUERY.
Prompt for any missing or nil arguments.  BACKEND should be a
function obeying the interface described in the docstring of
`biblio-backends'.  Returns the buffer in which results will be
inserted."
  (interactive)
  (unless backend (setq backend (biblio--read-backend)))
  (unless query (setq query (biblio--read-query backend)))
  (biblio--lookup-1 backend query))

(defun biblio-kill-buffers ()
  "Kill all `biblio-selection-mode' buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (eq (buffer-local-value 'major-mode buf)
                   'biblio-selection-mode))
      (kill-buffer buf))))

;; Local Variables:
;; nameless-current-name: "biblio"
;; checkdoc-arguments-in-order-flag: nil
;; End:

(provide 'biblio-core)
;;; biblio-core.el ends here
