;;; pdf-sync.el --- Use synctex to correlate LaTeX-Sources with PDF positions. -*- lexical-binding:t -*-
;; Copyright (C) 2013, 2014  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
;; Keywords: files, doc-view, pdf

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
;; The backward search uses a heuristic, which is pretty simple, but
;; effective: It extracts the text around the click-position in the
;; PDF, normalizes its whitespace, deletes certain notorious
;; characters and translates certain other characters into their latex
;; equivalents.  This transformed text is split into a series of
;; token.  A similar operation is performed on the source code around
;; the position synctex points at.  These two sequences of token are
;; aligned with a standard sequence alignment algorithm, resulting in
;; an alist of matched and unmatched tokens.  This is then used to
;; find the corresponding word from the PDF file in the LaTeX buffer.


(require 'pdf-view)
(require 'pdf-info)
(require 'pdf-util)
(require 'let-alist)

;;; Code:

(defgroup pdf-sync nil
  "Jump from TeX sources to PDF pages and back."
  :group 'pdf-tools)

(defcustom pdf-sync-forward-display-pdf-key "C-c C-g"
  "Key to jump from a TeX buffer to its PDF file.

This key is added to `TeX-source-correlate-method', when
command `pdf-sync-minor-mode' is activated and this map is defined."
  :type 'key-sequence)

(make-obsolete-variable
 'pdf-sync-forward-display-pdf-key
 "Bound in Auctex's to C-c C-v, if TeX-source-correlate-mode is activate." "1.0")

(defcustom pdf-sync-backward-hook nil
  "Hook ran after going to a source location.

The hook is run in the TeX buffer."
  :type 'hook
  :options '(pdf-sync-backward-beginning-of-word))

(defcustom pdf-sync-forward-hook nil
  "Hook ran after displaying the PDF buffer.

The hook is run in the PDF's buffer."
  :type 'hook)

(defcustom pdf-sync-forward-display-action nil
  "Display action used when displaying PDF buffers."
  :type 'display-buffer--action-custom-type)

(defcustom pdf-sync-backward-display-action nil
  "Display action used when displaying TeX buffers."
  :type 'display-buffer--action-custom-type)

(defcustom pdf-sync-locate-synctex-file-functions nil
  "A list of functions for locating the synctex database.

Each function on this hook should accept a single argument: The
absolute path of a PDF file.  It should return the absolute path
of the corresponding synctex database or nil, if it was unable to
locate it."
  :type 'hook)

(defvar pdf-sync-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap [double-mouse-1] #'pdf-sync-backward-search-mouse)
    (define-key kmap [C-mouse-1] #'pdf-sync-backward-search-mouse)
    kmap))

(defcustom pdf-sync-backward-redirect-functions nil
  "List of functions which may redirect a backward search.

Functions on this hook should accept three arguments, namely
SOURCE, LINE and COLUMN, where SOURCE is the absolute filename of
the source file and LINE and COLUMN denote the position in the
file.  COLUMN may be negative, meaning unspecified.

These functions should either return nil, if no redirection is
necessary.  Or a list of the same structure, with some or all (or
none) values modified.

AUCTeX installs a function here which changes the backward search
location for synthetic `TeX-region' files back to the equivalent
position in the original tex file."
  :type '(repeat function))


;;;###autoload
(define-minor-mode pdf-sync-minor-mode
  "Correlate a PDF position with the TeX file.
\\<pdf-sync-minor-mode-map>
This works via SyncTeX, which means the TeX sources need to have
been compiled with `--synctex=1'.  In AUCTeX this can be done by
setting `TeX-source-correlate-method' to `synctex' (before AUCTeX
is loaded) and enabling `TeX-source-correlate-mode'.

Then \\[pdf-sync-backward-search-mouse] in the PDF buffer will
open the corresponding TeX location.

If AUCTeX is your preferred tex-mode, this library arranges to
bind `pdf-sync-forward-display-pdf-key' (the default is `C-c C-g')
to `pdf-sync-forward-search' in `TeX-source-correlate-map'.  This
function displays the PDF page corresponding to the current
position in the TeX buffer.  This function only works together
with AUCTeX."
  :group 'pdf-sync
  (pdf-util-assert-pdf-buffer))


;; * ================================================================== *
;; * Backward search (PDF -> TeX)
;; * ================================================================== *

(defcustom pdf-sync-backward-use-heuristic t
  "Whether to apply a heuristic when backward searching.

If nil, just go where Synctex tells us.  Otherwise try to find
the exact location of the clicked-upon text in the PDF."
  :type 'boolean)

(defcustom pdf-sync-backward-text-translations
  '((88 "X" "sum")
    (94 "textasciicircum")
    (126 "textasciitilde")
    (169 "copyright" "textcopyright")
    (172 "neg" "textlnot")
    (174 "textregistered" "textregistered")
    (176 "textdegree")
    (177 "pm" "textpm")
    (181 "upmu" "mu")
    (182 "mathparagraph" "textparagraph" "P" "textparagraph")
    (215 "times")
    (240 "eth" "dh")
    (915 "Upgamma" "Gamma")
    (920 "Uptheta" "Theta")
    (923 "Uplambda" "Lambda")
    (926 "Upxi" "Xi")
    (928 "Uppi" "Pi")
    (931 "Upsigma" "Sigma")
    (933 "Upupsilon" "Upsilon")
    (934 "Upphi" "Phi")
    (936 "Uppsi" "Psi")
    (945 "upalpha" "alpha")
    (946 "upbeta" "beta")
    (947 "upgamma" "gamma")
    (948 "updelta" "delta")
    (949 "upvarepsilon" "varepsilon")
    (950 "upzeta" "zeta")
    (951 "upeta" "eta")
    (952 "uptheta" "theta")
    (953 "upiota" "iota")
    (954 "upkappa" "varkappa" "kappa")
    (955 "uplambda" "lambda")
    (957 "upnu" "nu")
    (958 "upxi" "xi")
    (960 "uppi" "pi")
    (961 "upvarrho" "uprho" "rho")
    (962 "varsigma")
    (963 "upvarsigma" "upsigma" "sigma")
    (964 "uptau" "tau")
    (965 "upupsilon" "upsilon")
    (966 "upphi" "phi")
    (967 "upchi" "chi")
    (968 "uppsi" "psi")
    (969 "upomega" "omega")
    (977 "upvartheta" "vartheta")
    (981 "upvarphi" "varphi")
    (8224 "dagger")
    (8225 "ddagger")
    (8226 "bullet")
    (8486 "Upomega" "Omega")
    (8501 "aleph")
    (8592 "mapsfrom" "leftarrow")
    (8593 "uparrow")
    (8594 "to" "mapsto" "rightarrow")
    (8595 "downarrow")
    (8596 "leftrightarrow")
    (8656 "shortleftarrow" "Leftarrow")
    (8657 "Uparrow")
    (8658 "Mapsto" "rightrightarrows" "Rightarrow")
    (8659 "Downarrow")
    (8660 "Leftrightarrow")
    (8704 "forall")
    (8706 "partial")
    (8707 "exists")
    (8709 "varnothing" "emptyset")
    (8710 "Updelta" "Delta")
    (8711 "nabla")
    (8712 "in")
    (8722 "-")
    (8725 "setminus")
    (8727 "*")
    (8734 "infty")
    (8743 "wedge")
    (8744 "vee")
    (8745 "cap")
    (8746 "cup")
    (8756 "therefore")
    (8757 "because")
    (8764 "thicksim" "sim")
    (8776 "thickapprox" "approx")
    (8801 "equiv")
    (8804 "leq")
    (8805 "geq")
    (8810 "lll")
    (8811 "ggg")
    (8814 "nless")
    (8815 "ngtr")
    (8822 "lessgtr")
    (8823 "gtrless")
    (8826 "prec")
    (8832 "nprec")
    (8834 "subset")
    (8835 "supset")
    (8838 "subseteq")
    (8839 "supseteq")
    (8853 "oplus")
    (8855 "otimes")
    (8869 "bot" "perp")
    (9702 "circ")
    (9792 "female" "venus")
    (9793 "earth")
    (9794 "male" "mars")
    (9824 "spadesuit")
    (9827 "clubsuit")
    (9829 "heartsuit")
    (9830 "diamondsuit"))
  "Alist mapping PDF character to a list of LaTeX macro names.

Adding a character here with its LaTeX equivalent names allows
the heuristic backward search to find its location in the source
file.  These strings should not match
`pdf-sync-backward-source-flush-regexp'.

Has no effect if `pdf-sync-backward-use-heuristic' is nil."
  :type '(alist :key-type character
                :value-type (repeat string)))

(defconst pdf-sync-backward-text-flush-regexp
  "[][.·{}|\\]\\|\\C.\\|-\n+"
  "Regexp of ignored text when backward searching.")

(defconst pdf-sync-backward-source-flush-regexp
  "\\(?:\\\\\\(?:begin\\|end\\|\\(?:eq\\)?ref\\|label\\|cite\\){[^}]*}\\)\\|[][\\&{}$_]"
  "Regexp of ignored source when backward searching.")

(defconst pdf-sync-backward-context-limit 64
  "Number of character to include in the backward search.")

(defun pdf-sync-backward-search-mouse (ev)
  "Go to the source corresponding to position at event EV."
  (interactive "@e")
  (let* ((posn (event-start ev))
         (image (posn-image posn))
         (xy (posn-object-x-y posn)))
    (unless image
      (error "Outside of image area"))
    (pdf-sync-backward-search (car xy) (cdr xy))))

(defun pdf-sync-backward-search (x y)
  "Go to the source corresponding to image coordinates X, Y.

Try to find the exact position, if
`pdf-sync-backward-use-heuristic' is non-nil."
  (cl-destructuring-bind (source finder)
      (pdf-sync-backward-correlate x y)
    (pop-to-buffer (or (find-buffer-visiting source)
                       (find-file-noselect source))
                   pdf-sync-backward-display-action)
    (push-mark)
    (funcall finder)
    (run-hooks 'pdf-sync-backward-hook)))

(defun pdf-sync-backward-correlate (x y)
  "Find the source corresponding to image coordinates X, Y.

Returns a list \(SOURCE FINDER\), where SOURCE is the name of the
TeX file and FINDER a function of zero arguments which, when
called in the buffer of the aforementioned file, will try to move
point to the correct position."

  (pdf-util-assert-pdf-window)
  (let ((size (pdf-view-image-size))
        (page (pdf-view-current-page)))
    (setq x (/ x (float (car size)))
          y (/ y (float (cdr size))))
    (let-alist (pdf-info-synctex-backward-search page x y)
      (let ((data (list (expand-file-name .filename)
                        .line .column)))
        (cl-destructuring-bind (source line column)
            (or (save-selected-window
                  (apply #'run-hook-with-args-until-success
                    'pdf-sync-backward-redirect-functions data))
                data)
          (list source
                (if (not pdf-sync-backward-use-heuristic)
                    (lambda nil
                      (pdf-util-goto-position line column))
                  (let ((context (pdf-sync-backward--get-text-context page x y)))
                    (lambda nil
                      (pdf-sync-backward--find-position line column context))))))))))

(defun pdf-sync-backward--find-position (line column context)
  (pdf-util-goto-position line column)
  (cl-destructuring-bind (windex chindex words)
      context
    (let* ((swords (pdf-sync-backward--get-source-context
                    nil (* 6 pdf-sync-backward-context-limit)))
           (similarity-fn (lambda (text source)
                            (if (if (consp text)
                                    (member source text)
                                  (equal text source))
                                1024 -1024)))
           (alignment
            (pdf-util-seq-alignment
             words swords similarity-fn 'infix)))
      (setq alignment (cl-remove-if-not 'car (cdr alignment)))
      (cl-assert (< windex (length alignment)))

      (let ((word (cdr (nth windex alignment))))
        (unless word
          (setq chindex 0
                word (cdr (nth (1+ windex) alignment))))
        (unless word
          (setq word (cdr (nth (1- windex) alignment))
                chindex (length word)))
        (when word
          (cl-assert (get-text-property 0 'position word) t)
          (goto-char (get-text-property 0 'position word))
          (forward-char chindex))))))

(defun pdf-sync-backward--get-source-context (&optional position limit)
  (save-excursion
    (when position (goto-char position))
    (goto-char (line-beginning-position))
    (let* ((region
            (cond
             ((eq limit 'line)
              (cons (line-beginning-position)
                    (line-end-position)))

             ;; Synctex usually jumps to the end macro, in case it
             ;; does not understand the environment.
             ((and (fboundp 'LaTeX-find-matching-begin)
                   (looking-at " *\\\\\\(end\\){"))
              (cons (or (ignore-errors
                          (save-excursion
                            (LaTeX-find-matching-begin)
                            (forward-line 1)
                            (point)))
                        (point))
                    (point)))
             ((and (fboundp 'LaTeX-find-matching-end)
                   (looking-at " *\\\\\\(begin\\){"))
              (goto-char (line-end-position))
              (cons (point)
                    (or (ignore-errors
                          (save-excursion
                            (LaTeX-find-matching-end)
                            (forward-line 0)
                            (point)))
                        (point))))
             (t (cons (point) (point)))))
           (begin (car region))
           (end (cdr region)))
      (when (numberp limit)
        (let ((delta (- limit (- end begin))))
          (when (> delta 0)
            (setq begin (max (point-min)
                             (- begin (/ delta 2)))
                  end (min (point-max)
                           (+ end (/ delta 2)))))))
      (let ((string (buffer-substring-no-properties begin end)))
        (dotimes (i (length string))
          (put-text-property i (1+ i) 'position (+ begin i) string))
        (nth 2 (pdf-sync-backward--tokenize
                (pdf-sync-backward--source-strip-comments string)
                nil
                pdf-sync-backward-source-flush-regexp))))))

(defun pdf-sync-backward--source-strip-comments (string)
  "Strip all standard LaTeX comments from string."
  (with-temp-buffer
    (save-excursion (insert string))
    (while (re-search-forward
            "^\\(?:[^\\\n]\\|\\(?:\\\\\\\\\\)\\)*\\(%.*\\)" nil t)
      (delete-region (match-beginning 1) (match-end 1)))
    (buffer-string)))

(defun pdf-sync-backward--get-text-context (page x y)
  (cl-destructuring-bind (&optional char edges)
      (car (pdf-info-charlayout page (cons x y)))
    (when edges
      (setq x (nth 0 edges)
            y (nth 1 edges)))
    (let* ((prefix (pdf-info-gettext page (list 0 0 x y)))
           (suffix (pdf-info-gettext page (list x y 1 1)))
           (need-suffix-space-p (memq char '(?\s ?\n)))
           ;; Figure out whether we missed a space by matching the
           ;; prefix's suffix with the line's prefix.  Due to the text
           ;; extraction in poppler, spaces are only inserted in
           ;; between words.  This test may fail, if prefix and line
           ;; do not overlap, which may happen in various cases, but
           ;; we don't care.
           (need-prefix-space-p
            (and (not need-suffix-space-p)
                 (memq
                  (ignore-errors
                    (aref (pdf-info-gettext page (list x y x y) 'line)
                          (- (length prefix)
                             (or (cl-position ?\n prefix :from-end t)
                                 -1)
                             1)))
                  '(?\s ?\n)))))
      (setq prefix
            (concat
             (substring
              prefix (max 0 (min (1- (length prefix))
                                 (- (length prefix)
                                    pdf-sync-backward-context-limit))))
             (if need-prefix-space-p " "))
            suffix
            (concat
             (if need-suffix-space-p " ")
             (substring
              suffix 0 (max 0 (min (1- (length suffix))
                                   pdf-sync-backward-context-limit)))))
      (pdf-sync-backward--tokenize
       prefix suffix
       pdf-sync-backward-text-flush-regexp
       pdf-sync-backward-text-translations))))

(defun pdf-sync-backward--tokenize (prefix &optional suffix flush-re translation)
  (with-temp-buffer
    (when prefix (insert prefix))
    (let* ((center (copy-marker (point)))
           (case-fold-search nil))
      (when suffix (insert suffix))
      (goto-char 1)
      ;; Delete ignored text.
      (when flush-re
        (save-excursion
          (while (re-search-forward flush-re nil t)
            (replace-match " " t t))))
      ;; Normalize whitespace.
      (save-excursion
        (while (re-search-forward "[ \t\f\n]+" nil t)
          (replace-match " " t t)))
      ;; Split words and non-words
      (save-excursion
        (while (re-search-forward "[^ ]\\b\\|[^ [:alnum:]]" nil t)
          (insert-before-markers " ")))
      ;; Replace character
      (let ((translate
             (lambda (string)
               (or (and (= (length string) 1)
                        (cdr (assq (aref string 0)
                                   translation)))
                   string)))
            words
            (windex -1)
            (chindex 0))
        (skip-chars-forward " ")
        (while (and (not (eobp))
                    (<= (point) center))
          (cl-incf windex)
          (skip-chars-forward "^ ")
          (skip-chars-forward " "))
        (goto-char center)
        (when (eq ?\s (char-after))
          (skip-chars-backward " "))
        (setq chindex (- (skip-chars-backward "^ ")))
        (setq words (split-string (buffer-string)))
        (when translation
          (setq words (mapcar translate words)))
        (list windex chindex words)))))

(defun pdf-sync-backward-beginning-of-word ()
  "Maybe move to the beginning of the word.

Don't move if already at the beginning, or if not at a word
character.

This function is meant to be put on `pdf-sync-backward-hook', when
word-level searching is desired."
  (interactive)
  (unless (or (looking-at "\\b\\w")
              (not (looking-back "\\w" (1- (point)))))
    (backward-word)))

;; * ------------------------------------------------------------------ *
;; * Debugging backward search
;; * ------------------------------------------------------------------ *

(defvar pdf-sync-backward-debug-trace nil)

(defun pdf-sync-backward-debug-wrapper (fn-symbol fn &rest args)
  (cond
   ((eq fn-symbol 'pdf-sync-backward-search)
    (setq pdf-sync-backward-debug-trace nil)
    (apply fn args))
   (t
    (let ((retval (apply fn args)))
      (push `(,args . ,retval)
            pdf-sync-backward-debug-trace)
      retval))))

(define-minor-mode pdf-sync-backward-debug-minor-mode
  "Aid in debugging the backward search."
  :group 'pdf-sync
  (let ((functions
         '(pdf-sync-backward-search
           pdf-sync-backward--tokenize
           pdf-util-seq-alignment)))
    (cond
     (pdf-sync-backward-debug-minor-mode
      (dolist (fn functions)
        (advice-add fn :around
                    (apply-partially #'pdf-sync-backward-debug-wrapper fn)
                    `((name . ,(format "%s-debug" fn))))))
     (t
      (dolist (fn functions)
        (advice-remove fn (format "%s-debug" fn)))))))

(defun pdf-sync-backward-debug-explain ()
  "Explain the last backward search.

Needs to have `pdf-sync-backward-debug-minor-mode' enabled."

  (interactive)
  (unless pdf-sync-backward-debug-trace
    (error "No last search or `pdf-sync-backward-debug-minor-mode' not enabled."))

  (with-current-buffer (get-buffer-create "*pdf-sync-backward trace*")
    (cl-destructuring-bind (text source alignment &rest ignored)
        (reverse pdf-sync-backward-debug-trace)
      (let* ((fill-column 68)
             (sep (format "\n%s\n" (make-string fill-column ?-)))
             (highlight '(:background "chartreuse" :foreground "black"))
             (or-sep "|")
             (inhibit-read-only t)
             (windex (nth 0 (cdr text)))
             (chindex (nth 1 (cdr text))))
        (erase-buffer)
        (font-lock-mode -1)
        (view-mode 1)
        (insert (propertize "Text Raw:" 'face 'font-lock-keyword-face))
        (insert sep)
        (insert (nth 0 (car text)))
        (insert (propertize "<|>" 'face highlight))
        (insert (nth 1 (car text)))
        (insert sep)
        (insert (propertize "Text Token:" 'face 'font-lock-keyword-face))
        (insert sep)
        (fill-region (point)
                     (progn
                       (insert
                        (mapconcat (lambda (elt)
                                     (if (consp elt)
                                         (mapconcat #'identity elt or-sep)
                                       elt))
                                   (nth 2 (cdr text)) " "))
                       (point)))
        (insert sep)

        (insert (propertize "Source Raw:" 'face 'font-lock-keyword-face))
        (insert sep)
        (insert (nth 0 (car source)))
        (insert sep)
        (insert (propertize "Source Token:" 'face 'font-lock-keyword-face))
        (insert sep)
        (fill-region (point)
                     (progn (insert (mapconcat #'identity (nth 2 (cdr source)) " "))
                            (point)))
        (insert sep)

        (insert (propertize "Alignment:" 'face 'font-lock-keyword-face))
        (insert (format " (windex=%d, chindex=%d" windex chindex))
        (insert sep)
        (save-excursion (newline 2))
        (let ((column 0)
              (index 0))
          (dolist (a (cdr (cdr alignment)))
            (let* ((source (cdr a))
                   (text (if (consp (car a))
                             (mapconcat #'identity (car a) or-sep)
                           (car a)))
                   (extend (max (length text)
                                (length source))))
              (when (and (not (bolp))
                         (> (+ column extend)
                            fill-column))
                (forward-line 2)
                (newline 3)
                (forward-line -2)
                (setq column 0))
              (when text
                (insert (propertize text 'face
                                    (if (= index windex)
                                        highlight
                                      (if source 'match
                                        'lazy-highlight)))))
              (move-to-column (+ column extend) t)
              (insert " ")
              (save-excursion
                (forward-line)
                (move-to-column column t)
                (when source
                  (insert (propertize source 'face (if text
                                                       'match
                                                     'lazy-highlight))))
                (move-to-column (+ column extend) t)
                (insert " "))
              (cl-incf column (+ 1 extend))
              (when text (cl-incf index)))))
        (goto-char (point-max))
        (insert sep)
        (goto-char 1)
        (pop-to-buffer (current-buffer))))))


;; * ================================================================== *
;; * Forward search (TeX -> PDF)
;; * ================================================================== *

(defun pdf-sync-forward-search (&optional line column)
  "Display the PDF location corresponding to LINE, COLUMN."
  (interactive)
  (cl-destructuring-bind (pdf page _x1 y1 _x2 _y2)
      (pdf-sync-forward-correlate line column)
    (let ((buffer (or (find-buffer-visiting pdf)
                      (find-file-noselect pdf))))
      (with-selected-window (display-buffer
                             buffer pdf-sync-forward-display-action)
        (pdf-util-assert-pdf-window)
        (when page
	  (pdf-view-goto-page page)
	  (when y1
	    (let ((top (* y1 (cdr (pdf-view-image-size)))))
	      (pdf-util-tooltip-arrow (round top))))))
      (with-current-buffer buffer
        (run-hooks 'pdf-sync-forward-hook)))))

(defun pdf-sync-forward-correlate (&optional line column)
  "Find the PDF location corresponding to LINE, COLUMN.

Returns a list \(PDF PAGE X1 Y1 X2 Y2\), where PAGE, X1, Y1, X2
and Y2 may be nil, if the destination could not be found."
  (unless (fboundp 'TeX-master-file)
    (error "This function works only with AUCTeX"))
  (unless line (setq line (line-number-at-pos)))
  (unless column (setq column (current-column)))

  (let* ((pdf (expand-file-name
               (with-no-warnings (TeX-master-file "pdf"))))
         (sfilename (pdf-sync-synctex-file-name
                     (buffer-file-name) pdf)))
    (cons pdf
	  (condition-case error
	      (let-alist (pdf-info-synctex-forward-search
			  (or sfilename
			      (buffer-file-name))
			  line column pdf)
		(cons .page .edges))
	    (error
	     (message "%s" (error-message-string error))
	     (list nil nil nil nil nil))))))



;; * ================================================================== *
;; * Dealing with synctex files.
;; * ================================================================== *

(defun pdf-sync-locate-synctex-file (pdffile)
  "Locate the synctex database corresponding to PDFFILE.

Returns either the absolute path of the database or nil.

See also `pdf-sync-locate-synctex-file-functions'."
  (cl-check-type pdffile string)
  (setq pdffile (expand-file-name pdffile))
  (or (run-hook-with-args-until-success
       'pdf-sync-locate-synctex-file-functions pdffile)
      (pdf-sync-locate-synctex-file-default pdffile)))

(defun pdf-sync-locate-synctex-file-default (pdffile)
  "The default function for locating a synctex database for PDFFILE.

See also `pdf-sync-locate-synctex-file'."
  (let ((default-directory
          (file-name-directory pdffile))
        (basename (file-name-sans-extension
                   (file-name-nondirectory pdffile))))
    (cl-labels ((file-if-exists-p (file)
                  (and (file-exists-p file)
                       file)))
      (or (file-if-exists-p
           (expand-file-name (concat basename ".synctex.gz")))
          (file-if-exists-p
           (expand-file-name (concat basename ".synctex")))
          ;; Some pdftex quote the basename.
          (file-if-exists-p
           (expand-file-name (concat "\"" basename "\"" ".synctex.gz")))
          (file-if-exists-p
           (expand-file-name (concat "\"" basename "\"" ".synctex")))))))

(defun pdf-sync-synctex-file-name (filename pdffile)
  "Find SyncTeX filename corresponding to FILENAME in the context of PDFFILE.

This function consults the synctex.gz database of PDFFILE and
searches for a filename, which is `file-equal-p' to FILENAME.
The first such filename is returned, or nil if none was found."

  (when (file-exists-p filename)
    (setq filename (expand-file-name filename))
    (let* ((synctex (pdf-sync-locate-synctex-file pdffile))
           (basename (file-name-nondirectory filename))
           (regexp (format "^ *Input *: *[^:\n]+ *:\\(.*%s\\)$"
                           (regexp-quote basename)))
           (jka-compr-verbose nil))
      (when (and synctex
                 (file-readable-p synctex))
        (with-current-buffer (find-file-noselect synctex :nowarn)
          (unless (or (verify-visited-file-modtime)
                      (buffer-modified-p))
            (revert-buffer :ignore-auto :noconfirm)
            (goto-char (point-min)))
          ;; Keep point in front of the found filename. It will
          ;; probably be queried for again next time.
          (let ((beg (point))
                (end (point-max)))
            (catch 'found
              (dotimes (_x 2)
                (while (re-search-forward regexp end t)
                  (let ((syncname (match-string-no-properties 1)))
                    (when (and (file-exists-p syncname)
                               (file-equal-p filename syncname))
                      (goto-char (line-beginning-position))
                      (throw 'found syncname))))
                (setq end beg
                      beg (point-min))
                (goto-char beg)))))))))

(provide 'pdf-sync)
;;; pdf-sync.el ends here
