;;; pdf-isearch.el --- Isearch in pdf buffers. -*- lexical-binding: t -*-

;; Copyright (C) 2013, 2014  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
;; Keywords: files, multimedia

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
;;; Todo:
;;
;; * Add the possibility to limit the search to a range of pages.

(require 'cl-lib)
(require 'pdf-util)
(require 'pdf-info)
(require 'pdf-misc)
(require 'pdf-view)
(require 'pdf-cache)
(require 'let-alist)

;;; Code:



;; * ================================================================== *
;; * Customizations
;; * ================================================================== *

(defgroup pdf-isearch nil
  "Isearch in pdf buffers."
  :group 'pdf-tools)

(defface pdf-isearch-match
  '((((background dark)) (:inherit isearch))
    (((background light)) (:inherit isearch)))
  "Face used to determine the colors of the current match."
  :group 'pdf-isearch
  :group 'pdf-tools-faces)

(defface pdf-isearch-lazy
  '((((background dark)) (:inherit lazy-highlight))
    (((background light)) (:inherit lazy-highlight)))
  "Face used to determine the colors of non-current matches."
  :group 'pdf-isearch
  :group 'pdf-tools-faces)

(defface pdf-isearch-batch
  '((((background dark)) (:inherit match))
    (((background light)) (:inherit match)))
  "Face used to determine the colors in `pdf-isearch-batch-mode'."
  :group 'pdf-isearch
  :group 'pdf-tools-faces)

(defcustom pdf-isearch-hyphenation-character "-­"
  "Characters used as hyphens when word searching."
  :group 'pdf-isearch
  :type 'string)

(defvar pdf-isearch-search-fun-function nil
  "Search function used when searching.

Like `isearch-search-fun-function', though it should return a
function \(FN STRING &optional PAGES\), which in turn should
return a result like `pdf-info-search-regexp'.")


;; * ================================================================== *
;; * Internal Variables
;; * ================================================================== *

(defvar-local pdf-isearch-current-page nil
  "The page that is currently searched.")

(defvar-local pdf-isearch-current-match nil
  "A list ((LEFT TOP RIGHT BOT) ...) of the current match or nil.

A match may contain more than one edges-element, e.g. when regexp
searching across multiple lines.")

(defvar-local pdf-isearch-current-matches nil
  "A list of matches of the last search.")

(defvar-local pdf-isearch-current-parameter nil
  "A list of search parameter \(search-string regex-p case-fold word-search\).")


;; * ================================================================== *
;; * Modes
;; * ================================================================== *

(declare-function pdf-occur "pdf-occur.el")
(declare-function pdf-sync-backward-search "pdf-sync.el")

(defvar pdf-isearch-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap [remap occur] 'pdf-occur)
    kmap)
  "Keymap used in `pdf-isearch-minor-mode'.")

(defvar pdf-isearch-active-mode-map
  (let ((kmap (make-sparse-keymap)))
    (set-keymap-parent kmap isearch-mode-map)
    (define-key kmap (kbd "C-d") 'pdf-view-dark-minor-mode)
    (define-key kmap (kbd "C-b") 'pdf-isearch-batch-mode)
    (define-key kmap (kbd "M-s o") 'pdf-isearch-occur)
    (define-key kmap (kbd "M-s s") 'pdf-isearch-sync-backward)
    kmap)
  "Keymap used in `pdf-isearch-active-mode'.

This keymap is used, when isearching in PDF buffers.  Its parent
keymap is `isearch-mode-map'.")

(put 'image-scroll-up 'isearch-scroll t)
(put 'image-scroll-down 'isearch-scroll t)

(define-minor-mode pdf-isearch-active-mode
  "This mode is enabled when isearch is active in a PDF file."
  :group 'pdf-isearch
  (cond
   (pdf-isearch-active-mode
    (set (make-local-variable 'isearch-mode-map)
         pdf-isearch-active-mode-map)
    (setq overriding-terminal-local-map
          isearch-mode-map))
   (t
    ;;(setq overriding-terminal-local-map nil) ?
    (kill-local-variable 'isearch-mode-map))))

;;;###autoload
(define-minor-mode pdf-isearch-minor-mode
  "Isearch mode for PDF buffer.

When this mode is enabled \\[isearch-forward], among other keys,
starts an incremental search in this PDF document.  Since this mode
uses external programs to highlight found matches via
image-processing, proceeding to the next match may be slow.

Therefore two isearch behaviours have been defined: Normal isearch and
batch mode.  The later one is a minor mode
\(`pdf-isearch-batch-mode'\), which when activated inhibits isearch
from stopping at and highlighting every single match, but rather
display them batch-wise.  Here a batch means a number of matches
currently visible in the selected window.

The kind of highlighting is determined by three faces
`pdf-isearch-match' \(for the current match\), `pdf-isearch-lazy'
\(for all other matches\) and `pdf-isearch-batch' \(when in batch
mode\), which see.

Colors may also be influenced by the minor-mode
`pdf-view-dark-minor-mode'.  If this is minor mode enabled, each face's
dark colors, are used (see e.g. `frame-background-mode'), instead
of the light ones.

\\{pdf-isearch-minor-mode-map}
While in `isearch-mode' the following keys are available. Note
that not every isearch command work as expected.

\\{pdf-isearch-active-mode-map}"
  :group 'pdf-isearch
  (pdf-util-assert-pdf-buffer)
  (cond
   (pdf-isearch-minor-mode
    (when (boundp 'character-fold-search)
      (setq-local character-fold-search nil))
    (set (make-local-variable 'isearch-search-fun-function)
         (lambda nil 'pdf-isearch-search-function))
    (set (make-local-variable 'isearch-push-state-function)
         'pdf-isearch-push-state-function)
    (set (make-local-variable 'isearch-wrap-function)
         'pdf-isearch-wrap-function)
    (set (make-local-variable 'isearch-lazy-highlight) nil)
    ;; Make our commands work in isearch-mode.
    (set (make-local-variable 'isearch-allow-scroll) t)
    (set (make-local-variable 'search-exit-option)
         ;; This maybe edit or t, but edit would suppress our cmds
         ;; in isearch-other-meta-char.
         (not (not search-exit-option)))
    ;; FIXME: Die Variable imagemagick-render-type entweder an anderer
    ;; Stelle global setzen oder nur irgendwo auf den
    ;; Performancegewinn hinweisen.
    (when (and (boundp 'imagemagick-render-type)
               (= 0 imagemagick-render-type))
      ;; This enormously speeds up rendering.
      (setq imagemagick-render-type 1))
    (add-hook 'isearch-mode-hook 'pdf-isearch-mode-initialize nil t)
    (add-hook 'isearch-mode-end-hook 'pdf-isearch-mode-cleanup nil t)
    (add-hook 'isearch-update-post-hook 'pdf-isearch-update nil t))
   (t
    (when (boundp 'character-fold-search)
      (kill-local-variable 'character-fold-search))
    (kill-local-variable 'search-exit-option)
    (kill-local-variable 'isearch-allow-scroll)
    (kill-local-variable 'isearch-search-fun-function)
    (kill-local-variable 'isearch-push-state-function)
    (kill-local-variable 'isearch-wrap-function)
    (kill-local-variable 'isearch-lazy-highlight)
    (remove-hook 'isearch-update-post-hook 'pdf-isearch-update t)
    (remove-hook 'isearch-mode-hook 'pdf-isearch-mode-initialize t)
    (remove-hook 'isearch-mode-end-hook 'pdf-isearch-mode-cleanup t))))

(define-minor-mode pdf-isearch-batch-mode
  "Isearch PDF documents batch-wise.

If this mode is enabled, isearching does not stop at every match,
but rather moves to the next one not currently visible.  This
behaviour is much faster than ordinary isearch, since far less
different images have to be displayed."
  :group 'pdf-isearch
  (when isearch-mode
    (pdf-isearch-redisplay)
    (pdf-isearch-message
     (if pdf-isearch-batch-mode "batch mode" "isearch mode"))))



;; * ================================================================== *
;; * Isearch interface
;; * ================================================================== *

(defvar pdf-isearch-filter-matches-function nil
  "A function for filtering isearch matches.

The function receives one argument: a list of matches, each
being a list of edges. It should return a subset of this list.
Edge coordinates are in image-space.")

(defvar pdf-isearch-narrow-to-page nil
  "Non-nil, if the search should be limited to the current page.")

(defun pdf-isearch-search-function (string &rest _)
  "Search for STRING in the current PDF buffer.

This is a Isearch interface function."
  (when (> (length string) 0)
    (let ((same-search-p (pdf-isearch-same-search-p))
          (oldpage pdf-isearch-current-page)
          (matches (pdf-isearch-search-page string))
          next-match)
      ;; matches is a list of list of edges ((x0 y1 x1 y2) ...),
      ;; sorted top to bottom ,left to right. Coordinates are in image
      ;; space.
      (unless isearch-forward
        (setq matches (reverse matches)))
      (when pdf-isearch-filter-matches-function
        (setq matches (funcall pdf-isearch-filter-matches-function matches)))
      ;; Where to go next ?
      (setq pdf-isearch-current-page (pdf-view-current-page)
            pdf-isearch-current-matches matches
            next-match
            (pdf-isearch-next-match
             oldpage pdf-isearch-current-page
             pdf-isearch-current-match matches
             same-search-p
             isearch-forward)
            pdf-isearch-current-parameter
            (list string isearch-regexp
                  isearch-case-fold-search isearch-word))
      (cond
       (next-match
        (setq pdf-isearch-current-match next-match)
        (pdf-isearch-hl-matches next-match matches)
        (pdf-isearch-focus-match next-match)
        ;; Don't get off track.
        (when (or (and (bobp) (not isearch-forward))
                  (and (eobp) isearch-forward))
          (goto-char (1+ (/ (buffer-size) 2))))
        ;; Signal success to isearch.
        (if isearch-forward
            (re-search-forward ".")
          (re-search-backward ".")))
       ((and (not pdf-isearch-narrow-to-page)
             (not (pdf-isearch-empty-match-p matches)))
        (let ((next-page (pdf-isearch-find-next-matching-page
                          string pdf-isearch-current-page t)))
          (when next-page
            (pdf-view-goto-page next-page)
            (pdf-isearch-search-function string))))))))

(defun pdf-isearch-push-state-function ()
  "Push the current search state.

This is a Isearch interface function."
  (let ((hscroll (window-hscroll))
        (vscroll (window-vscroll))
        (parms pdf-isearch-current-parameter)
        (matches pdf-isearch-current-matches)
        (match pdf-isearch-current-match)
        (page pdf-isearch-current-page))
    (lambda (_state)
      (setq pdf-isearch-current-parameter parms
            pdf-isearch-current-matches matches
            pdf-isearch-current-match match
            pdf-isearch-current-page page)

      (pdf-view-goto-page pdf-isearch-current-page)
      (when pdf-isearch-current-match
        (pdf-isearch-hl-matches
         pdf-isearch-current-match
         pdf-isearch-current-matches))
      (image-set-window-hscroll hscroll)
      (image-set-window-vscroll vscroll))))

(defun pdf-isearch-wrap-function ()
  "Go to first or last page.

This is a Isearch interface function."
  (let ((page (if isearch-forward
                  1
                (pdf-cache-number-of-pages))))
    (unless (or pdf-isearch-narrow-to-page
                (= page (pdf-view-current-page)))
      (pdf-view-goto-page page)
      (let ((next-screen-context-lines 0))
        (if (= page 1)
            (image-scroll-down)
          (image-scroll-up)))))
  (setq pdf-isearch-current-match nil))

(defun pdf-isearch-mode-cleanup ()
  "Cleanup after exiting Isearch.

This is a Isearch interface function."
  (pdf-isearch-active-mode -1)
  (pdf-view-redisplay))

(defun pdf-isearch-mode-initialize ()
  "Initialize isearching.

This is a Isearch interface function."
  (pdf-isearch-active-mode 1)
  (setq pdf-isearch-current-page (pdf-view-current-page)
        pdf-isearch-current-match nil
        pdf-isearch-current-matches nil
        pdf-isearch-current-parameter nil)
  (goto-char (1+ (/ (buffer-size) 2))))

(defun pdf-isearch-same-search-p (&optional ignore-search-string-p)
  "Return non-nil, if search parameter have not changed.

Parameter inspected are `isearch-string' (unless
IGNORE-SEARCH-STRING-P is t) and `isearch-case-fold-search'.  If
there was no previous search, this function returns t."
  (or (null pdf-isearch-current-parameter)
      (let ((parameter (list isearch-string
                             isearch-regexp
                             isearch-case-fold-search
                             isearch-word)))
        (if ignore-search-string-p
            (equal (cdr pdf-isearch-current-parameter)
                   (cdr parameter))
          (equal pdf-isearch-current-parameter
                 parameter)))))

(defun pdf-isearch-next-match (last-page this-page last-match
                                         all-matches continued-p
                                         forward-p)
  "Determine the next match."
  (funcall (if pdf-isearch-batch-mode
               'pdf-isearch-next-match-batch
             'pdf-isearch-next-match-isearch)
           last-page this-page last-match
           all-matches continued-p forward-p))

(defun pdf-isearch-focus-match (current-match)
  "Make the CURRENT-MATCH visible in the window."
  (funcall (if pdf-isearch-batch-mode
               'pdf-isearch-focus-match-batch
             'pdf-isearch-focus-match-isearch)
           current-match))

(defun pdf-isearch-redisplay ()
  "Redisplay the current highlighting."
  (pdf-isearch-hl-matches pdf-isearch-current-match
                          pdf-isearch-current-matches))

(defun pdf-isearch-update ()
  "Update search and redisplay, if necessary."
  (unless (pdf-isearch-same-search-p t)
    (setq pdf-isearch-current-parameter
          (list isearch-string isearch-regexp
                isearch-case-fold-search isearch-word)
          pdf-isearch-current-matches
          (pdf-isearch-search-page isearch-string))
    (pdf-isearch-redisplay)))

(defun pdf-isearch-message (fmt &rest args)
  "Like `message', but Isearch friendly."
  (unless args (setq args (list fmt) fmt "%s"))
  (let ((msg (apply 'format fmt args)))
    (if (cl-some (lambda (buf)
                   (buffer-local-value 'isearch-mode buf))
                 (mapcar 'window-buffer (window-list)))
        (let ((isearch-message-suffix-add
               (format " [%s]" msg)))
          (isearch-message)
          (sit-for 1))
      (message "%s" msg))))

(defun pdf-isearch-empty-match-p (matches)
  (and matches
       (cl-every
        (lambda (match)
          (cl-every (lambda (edges)
                      (cl-every 'zerop edges))
                    match))
        matches)))

(defun pdf-isearch-occur ()
  "Run `occur' using the last search string or regexp."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search)
        (regexp
         (cond
          ((functionp isearch-word)
           (funcall isearch-word isearch-string))
          (isearch-word (pdf-isearch-word-search-regexp
                         isearch-string nil
                         pdf-isearch-hyphenation-character))
          (isearch-regexp isearch-string))))
    (save-selected-window
      (pdf-occur (or regexp isearch-string) regexp))
    (isearch-message)))

(defun pdf-isearch-sync-backward ()
  "Visit the source of the beginning of the current match."
  (interactive)
  (pdf-util-assert-pdf-window)
  (unless pdf-isearch-current-match
    (user-error "No current or recent match"))
  (when isearch-mode
    (isearch-exit))
  (cl-destructuring-bind (left top _right _bot)
      (car pdf-isearch-current-match)
    (pdf-sync-backward-search left top)))


;; * ================================================================== *
;; * Interface to epdfinfo
;; * ================================================================== *

(defun pdf-isearch-search-page (string &optional page)
  "Search STRING on PAGE in the current window.

Returns a list of edges (LEFT TOP RIGHT BOTTOM) in PDF
coordinates, sorted top to bottom, then left to right."

  (unless page (setq page (pdf-view-current-page)))
  (mapcar (lambda (match)
            (let-alist match
              (pdf-util-scale-relative-to-pixel .edges 'round)))
          (let ((case-fold-search isearch-case-fold-search))
            (funcall (pdf-isearch-search-fun)
                     string page))))

(defun pdf-isearch-search-fun ()
  (funcall (or pdf-isearch-search-fun-function
               'pdf-isearch-search-fun-default)))

(defun pdf-isearch-search-fun-default ()
  "Return default functions to use for the search."
  (cond
   ((eq isearch-word t)
    (lambda (string &optional pages)
      ;; Use lax versions to not fail at the end of the word while
      ;; the user adds and removes characters in the search string
      ;; (or when using nonincremental word isearch)
      (let ((lax (not (or isearch-nonincremental
                          (null (car isearch-cmds))
                          (eq (length isearch-string)
                              (length (isearch--state-string
                                       (car isearch-cmds))))))))
        (pdf-info-search-regexp
         (pdf-isearch-word-search-regexp
          string lax pdf-isearch-hyphenation-character)
         pages 'invalid-regexp))))
   (isearch-regexp
    (lambda (string &optional pages)
      (pdf-info-search-regexp string pages 'invalid-regexp)))
   (t
    'pdf-info-search-string)))


(defun pdf-isearch-word-search-regexp (string &optional lax hyphenization-chars)
  "Return a PCRE which matches words, ignoring punctuation."
  (let ((hyphenization-regexp
         (and hyphenization-chars
              (format "(?:[%s]\\n)?"
                      (replace-regexp-in-string
                       "[]^\\\\-]" "\\\\\\&"
                       hyphenization-chars t)))))
    (cond
     ((equal string "") "")
     ((string-match-p "\\`\\W+\\'" string) "\\W+")
     (t (concat
         (if (string-match-p "\\`\\W" string) "\\W+"
           (unless lax "\\b"))
         (mapconcat (lambda (word)
                      (if hyphenization-regexp
                          (mapconcat
                           (lambda (ch)
                             (pdf-util-pcre-quote (string ch)))
                           (append word nil)
                           hyphenization-regexp)
                        (pdf-util-pcre-quote word)))
                    (split-string string "\\W+" t) "\\W+")
         (if (string-match-p "\\W\\'" string) "\\W+"
           (unless lax "\\b")))))))

(defun pdf-isearch-find-next-matching-page (string page &optional interactive-p)
  "Find STRING after or before page PAGE, according to FORWARD-P.

If INTERACTIVE-P is non-nil, give some progress feedback.
Returns the page number where STRING was found, or nil if there
is no such page."
  ;; Do a exponentially expanding search.
  (let* ((incr 1)
         (pages (if isearch-forward
                    (cons (1+ page)
                          (1+ page))
                  (cons (1- page)
                        (1- page))))
         (fn (pdf-isearch-search-fun))
         matched-page
         reporter)

    (while (and (null matched-page)
                (or (and isearch-forward
                         (<= (car pages)
                             (pdf-cache-number-of-pages)))
                    (and (not isearch-forward)
                         (>= (cdr pages) 1))))
      (let* ((case-fold-search isearch-case-fold-search)
             (matches (funcall fn string pages)))
        (setq matched-page
              (alist-get 'page (if isearch-forward
                                   (car matches)
                                 (car (last matches))))))
      (setq incr (* incr 2))
      (cond (isearch-forward
             (setcar pages (1+ (cdr pages)))
             (setcdr pages (min (pdf-cache-number-of-pages)
                                (+ (cdr pages) incr))))
            (t
             (setcdr pages (1- (car pages)))
             (setcar pages (max 1 (- (car pages)
                                     incr)))))
      (when interactive-p
        (when (and (not reporter)
                   (= incr 8)) ;;Don't bother right away.
          (setq reporter
                (apply
                    'make-progress-reporter "Searching"
                    (if isearch-forward
                        (list (car pages) (pdf-cache-number-of-pages) nil 0)
                      (list 1 (cdr pages) nil 0)))))
        (when reporter
          (progress-reporter-update
           reporter (if isearch-forward
                        (- (cdr pages) page)
                      (- page (car pages)))))))
    matched-page))



;; * ================================================================== *
;; * Isearch Behavior
;; * ================================================================== *

(defun pdf-isearch-next-match-isearch (last-page this-page last-match
                                                 matches same-search-p
                                                 forward)
  "Default function for choosing the next match.

Implements default isearch behaviour, i.e. it stops at every
match."
  (cond
   ((null last-match)
    ;; Goto first match from top or bottom of the window.
    (let* ((iedges (pdf-util-image-displayed-edges))
           (pos (pdf-util-with-edges (iedges)
                  (if forward
                      (list iedges-left iedges-top
                            iedges-left iedges-top)
                    (list iedges-right iedges-bot
                          iedges-right iedges-bot)))))
      (pdf-isearch-closest-match (list pos) matches forward)))
   ((not (eq last-page this-page))
    ;; First match from top-left or bottom-right of the new
    ;; page.
    (car matches))
   (same-search-p
    ;; Next match after the last one.
    (if last-match
        (cadr (member last-match matches))))
   (matches
    ;; Next match of new search closest to the last one.
    (pdf-isearch-closest-match
     last-match matches forward))))

(defun pdf-isearch-focus-match-isearch (match)
  "Make the image area in MATCH visible in the selected window."
  (pdf-util-scroll-to-edges (apply 'pdf-util-edges-union match)))

(defun pdf-isearch-next-match-batch (last-page this-page last-match
                                               matches same-search-p
                                               forward-p)
  "Select the next match, unseen in the current search direction."

  (if (or (null last-match)
          (not same-search-p)
          (not (eq last-page this-page)))
      (pdf-isearch-next-match-isearch
       last-page this-page last-match matches same-search-p forward-p)
    (pdf-util-with-edges (match iedges)
      (let ((iedges (pdf-util-image-displayed-edges)))
        (car (cl-remove-if
              ;; Filter matches visible on screen.
              (lambda (edges)
                (let ((match (apply 'pdf-util-edges-union edges)))
                  (and (<= match-right iedges-right)
                       (<= match-bot iedges-bot)
                       (>= match-left iedges-left)
                       (>= match-top iedges-top))))
              (cdr (member last-match matches))))))))

(defun pdf-isearch-focus-match-batch (match)
  "Make the image area in MATCH eagerly visible in the selected window."
  (pdf-util-scroll-to-edges (apply 'pdf-util-edges-union match) t))

(cl-deftype pdf-isearch-match ()
  `(satisfies
    (lambda (match)
      (cl-every (lambda (edges)
                  (and (consp edges)
                       (= (length edges) 4)
                       (cl-every 'numberp edges)))
                match))))

(cl-deftype list-of (type)
  `(satisfies
    (lambda (l)
      (and (listp l)
           (cl-every (lambda (x)
                       (cl-typep x ',type))
                     l)))))

(defun pdf-isearch-closest-match (match matches
                                        &optional forward-p)
  "Find the nearest element to MATCH in MATCHES.

The direction in which to look is determined by FORWARD-P.

MATCH should be a list of edges, MATCHES a list of such element;
it is assumed to be ordered with respect to FORWARD-P."


  (cl-check-type match pdf-isearch-match)
  (cl-check-type matches (list-of pdf-isearch-match))
  (let ((matched (apply 'pdf-util-edges-union match)))
    (pdf-util-with-edges (matched)
      (cl-loop for next in matches do
        (let ((edges (apply 'pdf-util-edges-union next)))
          (pdf-util-with-edges (edges)
            (when (if forward-p
                      (or (>= edges-top matched-bot)
                          (and (or (>= edges-top matched-top)
                                   (>= edges-bot matched-bot))
                               (>= edges-right matched-right)))
                    (or (<= edges-bot matched-top)
                        (and (or (<= edges-bot matched-bot)
                                 (<= edges-top matched-top))
                             (<= edges-left matched-left))))
              (cl-return next))))))))



;; * ================================================================== *
;; * Display
;; * ================================================================== *


(defun pdf-isearch-current-colors ()
  "Return the current color set.

The return value depends on `pdf-view-dark-minor-mode' and
`pdf-isearch-batch-mode'.  It is a list of four colors \(MATCH-FG
MATCH-BG LAZY-FG LAZY-BG\)."
  (let ((dark-p pdf-view-dark-minor-mode))
    (cond
     (pdf-isearch-batch-mode
      (let ((colors (pdf-util-face-colors 'pdf-isearch-batch dark-p)))
        (list (car colors)
              (cdr colors)
              (car colors)
              (cdr colors))))
     (t
      (let ((match (pdf-util-face-colors 'pdf-isearch-match dark-p))
            (lazy (pdf-util-face-colors 'pdf-isearch-lazy dark-p)))
        (list (car match)
              (cdr match)
              (car lazy)
              (cdr lazy)))))))

(defvar pdf-isearch--hl-matches-tick 0)

(defun pdf-isearch-hl-matches (current matches &optional occur-hack-p)
  "Highlighting edges CURRENT and MATCHES."
  (cl-check-type current pdf-isearch-match)
  (cl-check-type matches (list-of pdf-isearch-match))
  (cl-destructuring-bind (fg1 bg1 fg2 bg2)
      (pdf-isearch-current-colors)
    (let* ((width (car (pdf-view-image-size)))
           (page (pdf-view-current-page))
           (window (selected-window))
           (buffer (current-buffer))
           (tick (cl-incf pdf-isearch--hl-matches-tick))
           (pdf-info-asynchronous
            (lambda (status data)
              (when (and (null status)
                         (eq tick pdf-isearch--hl-matches-tick)
                         (buffer-live-p buffer)
                         (window-live-p window)
                         (eq (window-buffer window)
                             buffer))
                (with-selected-window window
                  (when (and (derived-mode-p 'pdf-view-mode)
                             (or isearch-mode
                                 occur-hack-p)
                             (eq page (pdf-view-current-page)))
                    (pdf-view-display-image
                     (pdf-view-create-image data :width width))))))))
      (pdf-info-renderpage-text-regions
       page width t nil nil
       `(,fg1 ,bg1 ,@(pdf-util-scale-pixel-to-relative
                      current))
       `(,fg2 ,bg2 ,@(pdf-util-scale-pixel-to-relative
                      (apply 'append
                        (remove current matches))))))))


;; * ================================================================== *
;; * Debug
;; * ================================================================== *

;; The following isearch-search function is debuggable.
;;
(when nil
  (defun isearch-search ()
    ;; Do the search with the current search string.
    (if isearch-message-function
        (funcall isearch-message-function nil t)
      (isearch-message nil t))
    (if (and (eq isearch-case-fold-search t) search-upper-case)
        (setq isearch-case-fold-search
              (isearch-no-upper-case-p isearch-string isearch-regexp)))
    (condition-case lossage
        (let ((inhibit-point-motion-hooks
               ;; FIXME: equality comparisons on functions is asking for trouble.
               (and (eq isearch-filter-predicate 'isearch-filter-visible)
                    search-invisible))
              (inhibit-quit nil)
              (case-fold-search isearch-case-fold-search)
              (retry t))
          (setq isearch-error nil)
          (while retry
            (setq isearch-success
                  (isearch-search-string isearch-string nil t))
            ;; Clear RETRY unless the search predicate says
            ;; to skip this search hit.
            (if (or (not isearch-success)
                    (bobp) (eobp)
                    (= (match-beginning 0) (match-end 0))
                    (funcall isearch-filter-predicate
                             (match-beginning 0) (match-end 0)))
                (setq retry nil)))
          (setq isearch-just-started nil)
          (if isearch-success
              (setq isearch-other-end
                    (if isearch-forward (match-beginning 0) (match-end 0)))))

      (quit (isearch-unread ?\C-g)
            (setq isearch-success nil))

      (invalid-regexp
       (setq isearch-error (car (cdr lossage)))
       (if (string-match
            "\\`Premature \\|\\`Unmatched \\|\\`Invalid "
            isearch-error)
           (setq isearch-error "incomplete input")))

      (search-failed
       (setq isearch-success nil)
       (setq isearch-error (nth 2 lossage)))

      ;; (error
      ;;  ;; stack overflow in regexp search.
      ;;  (setq isearch-error (format "%s" lossage)))
      )

    (if isearch-success
        nil
      ;; Ding if failed this time after succeeding last time.
      (and (isearch--state-success (car isearch-cmds))
           (ding))
      (if (functionp (isearch--state-pop-fun (car isearch-cmds)))
          (funcall (isearch--state-pop-fun (car isearch-cmds))
                   (car isearch-cmds)))
      (goto-char (isearch--state-point (car isearch-cmds))))))


(provide 'pdf-isearch)

;;; pdf-isearch.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; End:
