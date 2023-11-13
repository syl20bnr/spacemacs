;;; helm-adaptive.el --- Adaptive Sorting of Candidates. -*- lexical-binding: t -*-

;; Original Author: Tamas Patrovics

;; Copyright (C) 2007 Tamas Patrovics
;; Copyright (C) 2012 ~ 2023 Thierry Volpiatto 

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

;;; Code:

(require 'cl-lib)
(require 'helm)


(defgroup helm-adapt nil
  "Adaptative sorting of candidates for Helm."
  :group 'helm)

(defcustom helm-adaptive-history-file
  (locate-user-emacs-file "helm-adaptive-history")
  "Path of file where history information is stored.
When nil history is not saved nor restored after Emacs restart
unless you save/restore `helm-adaptive-history' with something
else like psession or desktop."
  :type 'string)

(defcustom helm-adaptive-history-length 50
  "Maximum number of candidates stored for a source."
  :type 'number)

(defcustom helm-adaptive-sort-by-frequent-recent-usage t
  "Try to sort on an average of frequent and recent usage when non-nil.

When nil sort on frequency usage only.

Only frequency:
When candidate have low frequency, you have to hit on it many
times to make it going up on top.

Frequency+recent:
Even with a low frequency, candidate go up on top. If a candidate
have a high frequency but it is not used since some time, it goes
down slowly, but as soon you reuse it it go up on top quickly."
  :type 'boolean)

;; Internal
(defvar helm-adaptive-done nil
  "nil if history information is not yet stored for the current
selection.")

(defvar helm-adaptive-history nil
  "Contains the stored history information.
Format: ((SOURCE-NAME
         (SELECTED-CANDIDATE (PATTERN . NUMBER-OF-USE) ...) ...) ...)")

(defconst helm-adaptive-freq-coefficient 5)
(defconst helm-adaptive-recent-coefficient 2)

(defun helm-adaptive-done-reset ()
  (setq helm-adaptive-done nil))

;;;###autoload
(define-minor-mode helm-adaptive-mode
  "Toggle adaptive sorting in all sources."
  :global t
  (if helm-adaptive-mode
      (progn
        (unless helm-adaptive-history
          (helm-adaptive-maybe-load-history))
        (add-hook 'kill-emacs-hook #'helm-adaptive-save-history)
        ;; Should run at beginning of `helm-initial-setup'.
        (add-hook 'helm-before-initialize-hook #'helm-adaptive-done-reset)
        ;; Should run at beginning of `helm-exit-minibuffer'.
        (add-hook 'helm-before-action-hook #'helm-adaptive-store-selection)
        ;; Should run at beginning of `helm-select-action'.
        (add-hook 'helm-select-action-hook #'helm-adaptive-store-selection))
    (helm-adaptive-save-history)
    (setq helm-adaptive-history nil)
    (remove-hook 'kill-emacs-hook #'helm-adaptive-save-history)
    (remove-hook 'helm-before-initialize-hook #'helm-adaptive-done-reset)
    (remove-hook 'helm-before-action-hook #'helm-adaptive-store-selection)
    (remove-hook 'helm-select-action-hook #'helm-adaptive-store-selection)))

(defun helm-adapt-use-adaptive-p (&optional source-name)
  "Return current source only if it use adaptive history, nil otherwise."
  (when helm-adaptive-mode
    (let* ((source (or source-name (helm-get-current-source)))
           (adapt-source (or (assoc-default 'filtered-candidate-transformer source)
                             (assoc-default 'candidate-transformer source))))
      (if (listp adapt-source)
          (and (memq 'helm-adaptive-sort adapt-source) source)
        (and (eq adapt-source 'helm-adaptive-sort) source)))))

(defun helm-adaptive-store-selection ()
  "Store history information for the selected candidate."
  (unless helm-adaptive-done
    (setq helm-adaptive-done t)
    (let ((source (helm-adapt-use-adaptive-p)))
      (when source
        (let* ((source-name (assoc-default 'name source))
               (source-info (or (assoc source-name helm-adaptive-history)
                                (progn
                                  (push (list source-name) helm-adaptive-history)
                                  (car helm-adaptive-history))))
               (selection (helm-get-selection nil t))
               (selection-info (progn
                                 (setcdr source-info
                                         (cons
                                          (let ((found (assoc selection (cdr source-info))))
                                            (if (not found)
                                                ;; new entry
                                                (list selection)
                                              ;; move entry to the beginning of the
                                              ;; list, so that it doesn't get
                                              ;; trimmed when the history is
                                              ;; truncated
                                              (setcdr source-info
                                                      (delete found (cdr source-info)))
                                              found))
                                          (cdr source-info)))
                                 (cadr source-info)))
               (pattern-info (progn
                               (setcdr selection-info
                                       (cons
                                        (let ((found (assoc helm-pattern (cdr selection-info))))
                                          (if (not found)
                                              ;; new entry
                                              (cons helm-pattern 0)
                                            ;; move entry to the beginning of the
                                            ;; list, so if two patterns used the
                                            ;; same number of times then the one
                                            ;; used last appears first in the list
                                            (setcdr selection-info
                                                    (delete found (cdr selection-info)))
                                            found))
                                        (cdr selection-info)))
                               (cadr selection-info)))
               (timestamp-info (helm-aif (assq 'timestamp (cdr selection-info))
                                   it
                                 (setcdr selection-info (cons (cons 'timestamp 0) (cdr selection-info)))
                                 (cadr selection-info))))
          ;; Increase usage count.
          (setcdr pattern-info (1+ (cdr pattern-info)))
          ;; Update timestamp.
          (setcdr timestamp-info (float-time))
          ;; Truncate history if needed.
          (if (> (length (cdr selection-info)) helm-adaptive-history-length)
              (setcdr selection-info
                      (helm-take (cdr selection-info) helm-adaptive-history-length))))))))

(defun helm-adaptive-maybe-load-history ()
  "Load `helm-adaptive-history-file' which contain `helm-adaptive-history'.
Returns nil if `helm-adaptive-history-file' doesn't exist."
  (when (and helm-adaptive-history-file
             (file-readable-p helm-adaptive-history-file))
    (load-file helm-adaptive-history-file)))

(defun helm-adaptive-save-history (&optional arg)
  "Save history information to the file given by `helm-adaptive-history-file'."
  (interactive "p")
  (when helm-adaptive-history-file
    (with-temp-buffer
      (insert
       ";; -*- mode: emacs-lisp -*-\n"
       ";; History entries used for helm adaptive display.\n")
      (let (print-length print-level)
        (prin1 `(setq helm-adaptive-history ',helm-adaptive-history)
               (current-buffer)))
      (insert ?\n)
      (write-region (point-min) (point-max) helm-adaptive-history-file nil
                    (unless arg 'quiet)))))

(defun helm-adaptive-sort (candidates source)
  "Sort the CANDIDATES for SOURCE by usage frequency.
This is a filtered candidate transformer you can use with the
`filtered-candidate-transformer' attribute."
  (let* ((source-name (assoc-default 'name source))
         (source-info (assoc source-name helm-adaptive-history)))
    (if source-info
        (let ((usage
               ;; Loop in the SOURCE entry of `helm-adaptive-history'
               ;; and assemble a list containing the (CANDIDATE
               ;; . USAGE-COUNT) pairs.
               (cl-loop with cf = (if helm-adaptive-sort-by-frequent-recent-usage
                                      helm-adaptive-freq-coefficient 1)
                        with cr = helm-adaptive-recent-coefficient
                        for (src-cand . infos) in (cdr source-info)
                        for count-freq = 0
                        for count-rec =
                        (helm-aif (and helm-adaptive-sort-by-frequent-recent-usage
                                       (assq 'timestamp infos))
                            (* cr (+ (float-time) (cdr it)))
                          0)
                        do (cl-loop for (pattern . score) in
                                    (remove (assq 'timestamp infos) infos)
                                    ;; If current pattern is equal to
                                    ;; the previously used one then
                                    ;; this candidate has priority
                                    ;; (that's why its count-freq is
                                    ;; boosted by 10000) and it only
                                    ;; has to compete with other
                                    ;; candidates which were also
                                    ;; selected with the same pattern.
                                    if (equal pattern helm-pattern)
                                    return (setq count-freq (+ 10000 score))
                                    else do (cl-incf count-freq score))
                        and collect (cons src-cand (+ (* count-freq cf) count-rec))
                        into results
                        ;; Sort the list in descending order, so
                        ;; candidates with highest priority come
                        ;; first.
                        finally return
                        (sort results (lambda (first second)
                                        (> (cdr first) (cdr second)))))))
          (if (consp usage)
              ;; Put those candidates first which have the highest usage count.
              (cl-loop for (cand . _freq) in usage
                       for info = (or (and (assq 'multiline source)
                                           (replace-regexp-in-string
                                            "\n\\'" "" cand))
                                      ;; Some transformers like in
                                      ;; bookmarks may add a leading
                                      ;; space to provide additional
                                      ;; infos like an icon as a
                                      ;; display prop, strip out this
                                      ;; leading space for
                                      ;; comparison. Same for a
                                      ;; trailing space (helm
                                      ;; boookmark add bmk location as
                                      ;; a display prop when
                                      ;; displaying it).
                                      (helm-aand (replace-regexp-in-string "\\` " "" cand)
                                                 (replace-regexp-in-string " \\'" "" it)))
                       when (cl-member info candidates
                                       :test 'helm-adaptive-compare)
                       collect (car it) into sorted
                       and do (setq candidates
                                    (cl-remove info candidates
                                               :test 'helm-adaptive-compare))
                       finally return (append sorted candidates))
              (message "Your `%s' is maybe corrupted or too old, \
you should reinitialize it with `helm-reset-adaptive-history'"
                       helm-adaptive-history-file)
              (sit-for 1)
              candidates))
        ;; if there is no information stored for this source then do nothing
        candidates)))

;;;###autoload
(defun helm-reset-adaptive-history ()
  "Delete all `helm-adaptive-history' and his file.
Useful when you have a old or corrupted
`helm-adaptive-history-file'."
  (interactive)
  (when (y-or-n-p "Really delete all your `helm-adaptive-history'? ")
    (setq helm-adaptive-history nil)
    (when (and helm-adaptive-history-file
               (file-exists-p helm-adaptive-history-file))
      (delete-file helm-adaptive-history-file))))

(defun helm-adaptive-compare (x y)
  "Compare display parts if some of candidates X and Y.

Arguments X and Y are cons cell in (DISPLAY . REAL) format or
atoms."
  (equal (if (listp x) (car x) x)
         (if (listp y) (car y) y)))


(provide 'helm-adaptive)

;;; helm-adaptive.el ends here
