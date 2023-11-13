;;; flyspell-correct.el --- Correcting words with flyspell via custom interface  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2016-2022 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/flyspell-correct
;; Version: 0.6.1
;; Package-Requires: ((emacs "24"))
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;; This package provides functionality for correcting words via custom
;; interfaces. There are several functions for this:
;;
;; - `flyspell-correct-at-point' - to correct word at point.
;; - `flyspell-correct-previous' to correct any visible word before the point.
;; - `flyspell-correct-next' to correct any visible word after the point.
;; - `flyspell-correct-wrapper' - a beefed wrapper for
;;   `flyspell-correct-previous' and `flyspell-correct-next' allowing one to
;;   correct many words at once (rapid flow) and change correction direction.
;;
;; In most cases the last function is the most convenient, so don't forget to
;; bind it.
;;
;;   (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)
;;
;; When invoked, it will show the list of corrections suggested by Flyspell.
;;
;; Most interfaces also allow you to save the new word to your dictionary,
;; accept this spelling in current buffer or for a whole session, or even skip
;; this word (useful in a rapid flow).
;;
;; Default interface is implemented using `completing-read', but it's highly
;; advised to use `flyspell-correct-ido' (which comes bundled with this package)
;; or any interface provided by following packages: `flyspell-correct-ivy',
;; `flyspell-correct-helm' and `flyspell-correct-popup'.
;;
;; In order to use `flyspell-correct-ido' interface instead of default
;; `flyspell-correct-completing-read', place following snippet in your
;; 'init.el' file.
;;
;;   (require 'flyspell-correct-ido)
;;
;; It's easy to implement your own interface for `flyspell-correct'. Checkout
;; documentation for `flyspell-correct-interface' variable.
;;
;; For more information about this and related packages, please refer to
;; attached README.org file.
;;
;;; Code:
;;

;; Requires

(require 'flyspell)

;; Variables

(defcustom flyspell-correct-interface #'flyspell-correct-completing-read
  "Interface for `flyspell-correct-at-point'.

`flyspell-correct-interface' is a function accepting two arguments:

  - candidates for correction (list of strings)
  - misspelled word (string)

Result must be either a string (replacement word) or a cons of a
command and a string (replacement word), where the command is one
of the following:

  - skip - do nothing to misspelled word, in rapid mode used for
    jumping to the next (or previous) misspelled word

  - break - do nothing to misspelled word, break from rapid mode

  - stop - do nothing to misspelled word, break from rapid
    mode (if enabled) and leave the point at the misspelled word

  - save - replace misspelled word with replacement word and save
    it to the personal dictionary

  - session - replace misspelled word with replacement word and
    save it to the session dictionary (correction will be
    discarded upon quitting Emacs)

  - buffer - replace misspelled word with replacement word and
    save it to the buffer dictionary (added to the bottom of
    buffer)"
  :group 'flyspell-correct
  :type 'function)

(defcustom flyspell-correct-highlight t
  "When non-nil highlight the word while correcting.

The face `flyspell-correct-highlight-face' is used for
highlighting."
  :group 'flyspell-correct
  :type 'boolean)

(defface flyspell-correct-highlight-face
  '((t (:inherit isearch)))
  "Face used for highlighting the word while correcting."
  :group 'flyspell-correct)

(defvar flyspell-correct-overlay nil)

;;; Default interface using `completing-read'
;;

(defvar flyspell-correct--cr-key "@"
  "Shortcut key used by `flyspell-correct-completing-read'.")

(defvar flyspell-correct--cr-actions
  '((save ?s "[Save]")
    (session ?a "[Accept (session)]")
    (buffer ?b "[Accept (buffer)]")
    (skip ?k "[Skip]")
    (stop ?p "[Stop]"))
  "Actions used by `flyspell-correct-completing-read'.")

(defun flyspell-correct--cr-index (n)
  "Generate a short unique index string for N.

The index string is used to prefix suggestion candidates. The digits 12345
encode (mod n 5) and occur as suffix of the index string. If one of the keys
12345 is pressed, the selected candidate is automatically submitted. The
remaining value (/ n 5) is encoded using the digits 67890, which occur in the
prefix of the index string."
  (let ((str (char-to-string (+ ?1 (mod n 5)))))
    (when (>= n 5)
      (setq n (/ (- n 5) 5))
      (while (>= n 0)
        (setq str (format "%c%s" (aref "67890" (mod n 5)) str)
              n (1- (/ n 5)))))
    str))

(defun flyspell-correct-completing-read (candidates word)
  "Run `completing-read' for the given CANDIDATES.

List of CANDIDATES is given by flyspell for the WORD.

Return a selected word to use as a replacement or a tuple
of (command, word) to be used by `flyspell-do-correct'."
  (let* ((idx 0)
         (candidates-alist
          (append
           (mapcar (lambda (cand)
                     (prog1
                         (cons (concat
                                (propertize (flyspell-correct--cr-index idx) 'face 'minibuffer-prompt)
                                " " cand)
                               cand)
                         (setq idx (1+ idx))))
                   candidates)
           (mapcar
            (pcase-lambda (`(,name ,key ,label))
              (setq key (char-to-string key))
              (cons (concat (propertize (format "%s%s " flyspell-correct--cr-key key) 'invisible t)
                            (replace-regexp-in-string
                             key (propertize key 'face '(bold minibuffer-prompt))
                             (propertize label 'face 'minibuffer-prompt)))
                    (cons name word)))
            flyspell-correct--cr-actions)))
         (suggestions-title (format "Suggestions (Dictionary \"%s\")"
                                    (or ispell-local-dictionary
                                        ispell-dictionary
                                        "default")))
         (actions-title (format "Actions (Shortcut key %s)" flyspell-correct--cr-key))
         (metadata `(metadata
                     (category . flyspell)
                     (display-sort-function . ,#'identity)
                     (cycle-sort-function . ,#'identity)
                     (group-function
                      . ,(lambda (cand transform)
                           (cond
                            (transform cand)
                            ((string-prefix-p flyspell-correct--cr-key cand) actions-title)
                            (t suggestions-title))))))
         (quick-result)
         (result
          (minibuffer-with-setup-hook
              (lambda ()
                (add-hook 'post-command-hook
                          (lambda ()
                            ;; Exit directly if a quick key is pressed
                            (let ((prefix (concat (minibuffer-contents-no-properties) " ")))
                              (mapc (lambda (cand)
                                      (when (string-prefix-p prefix (car cand))
                                        (setq quick-result (car cand))
                                        (exit-minibuffer)))
                                    candidates-alist)))
                          -1 'local))
            (completing-read
             (format "Suggestions for \"%s\": " word)
             ;; Use function with metadata to disable add a group function
             ;; and in order to disable sorting.
             (lambda (input predicate action)
               (if (eq action 'metadata)
                   metadata
                 (complete-with-action action candidates-alist input predicate)))
             ;; Require confirmation, if the input does not match a suggestion
             nil 'confirm nil nil
             ;; Pass the word as default value (effectively skipping)
             word))))
    (or (cdr (assoc (or quick-result result) candidates-alist)) result)))

(define-obsolete-function-alias
  'flyspell-correct-dummy
  'flyspell-correct-completing-read
  "0.6.1")

;;; On point word correction
;;

;;;###autoload
(defun flyspell-correct-at-point ()
  "Correct word before point using `flyspell-correct-interface'.
Adapted from `flyspell-correct-word-before-point'."
  (interactive)
  (unless flyspell-correct-interface
    (error "Could not correct word because `flyspell-correct-interface' is not set"))
  (let ((res))
    ;; use the correct dictionary
    (flyspell-accept-buffer-local-defs)
    (flyspell-correct--highlight-add)
    (unwind-protect
        (let ((cursor-location (point))
              (word (save-excursion (flyspell-get-word)))
              (opoint (point)))
          (if (consp word)
              (let ((start (nth 1 word))
                    (end (nth 2 word))
                    (word (car word))
                    poss ispell-filter)
                ;; now check spelling of word.
                (ispell-send-string "%\n") ;put in verbose mode
                (ispell-send-string (concat "^" word "\n"))
                ;; wait until ispell has processed word
                (while (progn
                         (accept-process-output ispell-process)
                         (not (string= "" (car ispell-filter)))))
                ;; Remove leading empty element
                (setq ispell-filter (cdr ispell-filter))
                ;; ispell process should return something after word is sent.
                ;; Tag word as valid (i.e., skip) otherwise
                (or ispell-filter
                    (setq ispell-filter '(*)))
                (if (consp ispell-filter)
                    (setq poss (ispell-parse-output (car ispell-filter))))
                (cond
                 ((or (eq poss t) (stringp poss))
                  ;; don't correct word
                  (message "%s is correct" (funcall ispell-format-word-function word))
                  t)
                 ((null poss)
                  ;; ispell error
                  (error "Ispell: error in Ispell process"))
                 (t
                  ;; The word is incorrect, we have to propose a replacement.
                  (setq res (funcall flyspell-correct-interface (nth 2 poss) word))
                  ;; Some interfaces actually eat 'C-g' so it's impossible to
                  ;; stop rapid mode. So when interface returns nil we treat it
                  ;; as a stop. Fixes #60.
                  (unless res (setq res (cons 'break word)))
                  (cond
                   ((stringp res)
                    (flyspell-do-correct
                     res poss word cursor-location start end opoint))
                   (t
                    (let ((cmd (car res))
                          (wrd (cdr res)))
                      (unless (or (eq cmd 'skip)
                                  (eq cmd 'break)
                                  (eq cmd 'stop))
                        (flyspell-do-correct
                         cmd poss wrd cursor-location start end opoint)
                        (unless (string-equal wrd word)
                          (flyspell-do-correct
                           wrd poss word cursor-location start end opoint))))))
                  (ispell-pdict-save t))))))
      (flyspell-correct--highlight-remove))
    res))

;;; Previous word correction
;;

;;;###autoload
(defun flyspell-correct-previous (position)
  "Correct the first misspelled word that occurs before POSITION.
But don't look beyond what's visible on the screen.

Uses `flyspell-correct-at-point' function for correction.

With a prefix argument, automatically continues to all prior misspelled words in the buffer."
  (interactive "d")
  (flyspell-correct-move position nil current-prefix-arg))

;;; Next word correction
;;

;;;###autoload
(defun flyspell-correct-next (position)
  "Correct the first misspelled word that occurs after POSITION.

Uses `flyspell-correct-at-point' function for correction.

With a prefix argument, automatically continues to all further
misspelled words in the buffer."
  (interactive "d")
  (flyspell-correct-move position t current-prefix-arg))

;;; Generic helpers
;;

;;;###autoload
(defun flyspell-correct-wrapper ()
  "Correct spelling error in a dwim fashion based on universal argument.

- One \\[universal-argument] enables rapid mode.
- Two \\[universal-argument]'s changes direction of spelling
  errors search.
- Three \\[universal-argument]'s changes direction of spelling
  errors search and enables rapid mode."
  (interactive)
  (let ((forward-direction nil)
		    (rapid nil))
    (cond
     ((equal current-prefix-arg '(4))  ; C-u = rapid
	    (setq rapid t))
     ((equal current-prefix-arg '(16)) ; C-u C-u = change direction
      (setq forward-direction t))
     ((equal current-prefix-arg '(64)) ; C-u C-u C-u = do both
	    (setq rapid t)
	    (setq forward-direction t)))

    (flyspell-correct-move (point) forward-direction rapid)))

;;;###autoload
(defun flyspell-correct-move (position &optional forward rapid)
  "Correct the first misspelled word that occurs before POSITION.

Uses `flyspell-correct-at-point' function for correction.

With FORWARD set non-nil, check forward instead of backward.

With RAPID set non-nil, automatically continues in direction
until all errors in buffer have been addressed."
  ;; NOTE: The way I may be pushing the mark may possibly be more
  ;; idiomatically done using the opoint arg of
  ;; `flyspell-correct-word-before-point'.
  (interactive "d")
  ;; push mark when starting
  (when (or (not (mark t))
            (/= (mark t) (point)))
    (push-mark (point) t))
  (let ((original-pos (point))
        (target-pos (point))
        (hard-move-point)
        (mark-opos))
    (unwind-protect
        (save-excursion
          (let ((incorrect-word-pos))

            ;; narrow the region
            (overlay-recenter (point))

            (let* ((unsorted-overlay-list
                    (if forward
                        (overlays-in (- position 1) (point-max))
                      (overlays-in (point-min) (+ position 1))))
                   (comp (if forward #'< #'>))
                   (overlay-list (sort
                                  unsorted-overlay-list
                                  (lambda (o1 o2)
                                    (funcall comp
                                             (overlay-start o1)
                                             (overlay-start o2)))))
                   (overlay 'dummy-value))
              (while overlay
                (setq overlay (car-safe overlay-list))
                (setq overlay-list (cdr-safe overlay-list))
                (when (and overlay
                           (flyspell-overlay-p overlay))
                  (setq incorrect-word-pos (overlay-start overlay))
                  (let ((scroll (> incorrect-word-pos (window-end))))
                    (goto-char incorrect-word-pos)
                    (when scroll (ignore-errors (recenter))))

                  ;; Point originally was on misspelled word, so we need to restore
                  ;; it. This imitates just calling `flyspell-correct-at-point'. But
                  ;; gives all the perks of `flyspell-correct-move'.
                  ;;
                  ;; But with rapid mode, `hard-move-point' will be set to nil
                  ;; eventually. Which gives more predictable point location in
                  ;; general.
                  (setq hard-move-point
                        (and (>= original-pos (overlay-start overlay))
                             (<= original-pos (overlay-end overlay))))

                  ;; Correct a word using `flyspell-correct-at-point'.
                  (let ((res (flyspell-correct-at-point)))
                    (when res
                      ;; stop at misspelled word
                      (when (eq (car-safe res) 'stop)
                        (setq target-pos incorrect-word-pos
                              hard-move-point t
                              mark-opos t))

                      ;; break from rapid mode
                      (when (or
                             ;; treat skip as one-time rapid mode enabler
                             (and (not (eq (car-safe res) 'skip))
                                  (not rapid))

                             ;; explicit rapid mode disablers
                             (eq (car-safe res) 'break)
                             (eq (car-safe res) 'stop))
                        (setq overlay nil))

                      (when (and
                             ;; don't push mark if there is no change
                             (not (memq (car-safe res) '(stop break skip)))
                             (/= (mark t) (point)))
                        ;; `flyspell-correct-at-point' may move point, use
                        ;; original `incorrect-word-pos' instead
                        (push-mark incorrect-word-pos t)))))))))

      (when hard-move-point
        (when mark-opos
          (push-mark (point) t))
        (goto-char target-pos))
      ;; We pushed the mark when starting, but if the operation is canceled
      ;; without any change that mark is redundant and needs to be cleaned-up.
      (when (= (mark t) (point)) (pop-mark)))))

;;; Overlays

(defun flyspell-correct--highlight-add ()
  "Highlight the spelling error at point."
  (when flyspell-correct-highlight
    (let* ((ov (flyspell-correct--overlay-loc))
           (ov-start (car-safe ov))
           (ov-end (cdr-safe ov)))
      (when ov
        (if flyspell-correct-overlay
	          (move-overlay flyspell-correct-overlay ov-start ov-end (current-buffer))
	        (setq flyspell-correct-overlay (make-overlay ov-start ov-end))
	        (overlay-put flyspell-correct-overlay 'priority 1001)
	        (overlay-put flyspell-correct-overlay 'face 'flyspell-correct-highlight-face))))))

(defun flyspell-correct--highlight-remove ()
  "Remove the highlight of the spelling error at point."
  (when flyspell-correct-overlay
    (delete-overlay flyspell-correct-overlay)
    (setq flyspell-correct-overlay nil)))

(defun flyspell-correct--overlay-loc ()
  "Return `cons' with start and end of `flyspell' overlay at point.

Returns nil if no overlay is found."
  (let ((ovs (overlays-at (point)))
        ov)
    (while (and (not ov) ovs)
      (let ((current (pop ovs)))
        (when (flyspell-overlay-p current)
          (setq ov current))))
    (when ov
      (let ((ov-start (overlay-start ov))
            (ov-end (overlay-end ov)))
        (cons ov-start ov-end)))))

;;; Automatically correct
;; based on `flyspell-popup-auto-correct-mode'

(defcustom flyspell-correct-auto-delay 1.6
  "Delay in seconds before `flyspell-correct-previous' is called.
Use floating point numbers to express fractions of seconds."
  :group 'flyspell
  :type 'number
  :safe #'numberp)

(defvar flyspell-correct-auto-mode-interface nil
  "Interface to use in `flyspell-correct-auto-mode'.
When set to nil `flyspell-correct-interface' is used.")

(defvar flyspell-correct--auto-timer nil
  "Timer to automatically call `flyspell-correct-previous'.")
(make-variable-buffer-local 'flyspell-correct--auto-timer)

(defvar flyspell-correct--auto-active-p nil)
(make-variable-buffer-local 'flyspell-correct--auto-active-p)

(defun flyspell-correct-auto-cancel-timer ()
  "Cancel auto correct timer."
  (when flyspell-correct--auto-timer
    (cancel-timer flyspell-correct--auto-timer)
    (setq flyspell-correct--auto-timer nil)))

(defun flyspell-correct-auto-soon ()
  "Call `flyspell-correct-previous' delayed."
  (flyspell-correct-auto-cancel-timer)
  (when (and flyspell-mode
             (not (bound-and-true-p flyspell-correct--auto-active-p)))
    (setq
     flyspell-correct--auto-timer
     (run-at-time
      flyspell-correct-auto-delay
      nil
      (lambda ()
        (flyspell-correct-auto-cancel-timer)
        (when (and flyspell-mode
                   (not (bound-and-true-p flyspell-correct--auto-active-p)))
          (setq flyspell-correct--auto-active-p t)
          (with-local-quit
            (let ((flyspell-correct-interface
                   (if (bound-and-true-p flyspell-correct-auto-mode-interface)
                       flyspell-correct-auto-mode-interface
                     flyspell-correct-interface)))
              (call-interactively #'flyspell-correct-previous)))
          (setq flyspell-correct--auto-active-p nil)))))))

;;;###autoload
(define-minor-mode flyspell-correct-auto-mode
  "Minor mode for automatically correcting word at point.

Take my advice and don't use this functionality unless you find
`flyspell-correct-previous' function useless for your purposes.
Seriously, just try named function for completion. You can find
more info in comment[1].

[1]:
https://github.com/syl20bnr/spacemacs/issues/6209#issuecomment-274320376"
  :group 'flyspell
  :lighter "auto-correct"
  (if flyspell-correct-auto-mode
      (progn
        (add-hook 'post-command-hook 'flyspell-correct-auto-soon nil 'local))
    (remove-hook 'post-command-hook 'flyspell-correct-auto-soon 'local)))

(provide 'flyspell-correct)

;;; flyspell-correct.el ends here
