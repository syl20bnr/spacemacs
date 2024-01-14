;;; helm-font --- Font and ucs selection for Helm -*- lexical-binding: t -*-

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
(require 'helm-help)

;; No warnings in Emacs built --without-x
(declare-function x-list-fonts "xfaces.c")

(declare-function helm-generic-sort-fn "helm-utils")

(defgroup helm-font nil
  "Related applications to display fonts in Helm."
  :group 'helm)

(defcustom helm-ucs-recent-size 10
  "Number of recent chars to keep."
  :type 'integer
  :group 'helm-font)

(defcustom helm-ucs-actions
  '(("Insert character"             . helm-ucs-insert-char)
    ("Insert character name"        . helm-ucs-insert-name)
    ("Insert character code in hex" . helm-ucs-insert-code)
    ("Kill marked characters"       . helm-ucs-kill-char)
    ("Kill name"                    . helm-ucs-kill-name)
    ("Kill code"                    . helm-ucs-kill-code)
    ("Describe char"                . helm-ucs-describe-char))
  "Actions for `helm-source-ucs'."
  :group 'helm-font
  :type '(alist :key-type string :value-type function))

(defvar helm-ucs-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "<C-backspace>") 'helm-ucs-persistent-delete)
    (define-key map (kbd "<C-left>")      'helm-ucs-persistent-backward)
    (define-key map (kbd "<C-right>")     'helm-ucs-persistent-forward)
    (define-key map (kbd "C-c SPC")       'helm-ucs-persistent-insert-space)
    map)
  "Keymap for `helm-ucs'.")

(defface helm-ucs-char
  `((((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "Gold"))
  "Face used to display ucs characters."
  :group 'helm-font)

;;; Xfont selection
;;
;;
(defvar helm-xfonts-cache nil)
(defvar helm-previous-font nil)
(defvar helm-source-xfonts
  (helm-build-sync-source "X Fonts"
    :init (lambda ()
            (unless helm-xfonts-cache
              (setq helm-xfonts-cache
                    (x-list-fonts "*")))
            ;; Save current font so it can be restored in cleanup
            (setq helm-previous-font (cdr (assq 'font (frame-parameters)))))
    :candidates 'helm-xfonts-cache
    :action '(("Copy font to kill ring" . (lambda (elm)
                                            (kill-new elm)))
              ("Set font" . (lambda (elm)
                              (kill-new elm)
                              (set-frame-font elm 'keep-size)
                              (message "Font copied to kill ring"))))
    :cleanup (lambda ()
               ;; Restore previous font
               (set-frame-font helm-previous-font 'keep-size))
    :persistent-action (lambda (new-font)
                         (set-frame-font new-font 'keep-size)
                         (kill-new new-font))
    :persistent-help "Preview font and copy to kill-ring"))


;;; 𝕌𝕔𝕤 𝕊𝕪𝕞𝕓𝕠𝕝 𝕔𝕠𝕞𝕡𝕝𝕖𝕥𝕚𝕠𝕟
;;
;;
(defvar helm-ucs--max-len nil)
(defvar helm-ucs--names nil)
(defvar helm-ucs-history nil)
(defvar helm-ucs-recent nil
  "Ring of recent `helm-ucs' selections.")

(defun helm-calculate-ucs-alist-max-len (names)
  "Calculate the length of the longest NAMES list candidate."
  (cl-loop for (_n . v) in names
           maximize (length (format "#x%x:" v)) into code
           maximize (max 1 (string-width (format "%c" v))) into char
           finally return (cons code char)))

(defun helm-calculate-ucs-hash-table-max-len (names)
  "Calculate the length of the longest NAMES hash table candidate."
  (cl-loop for _n being the hash-keys of names
           using (hash-values v)
           maximize (length (format "#x%x:" v)) into code
           maximize (max 1 (string-width (format "%c" v))) into char
           finally return (cons code char)))

(defun helm-calculate-ucs-max-len ()
  "Calculate the length of the longest `ucs-names' candidate."
  (let ((ucs-struct (ucs-names)))
    (if (hash-table-p ucs-struct)
        (helm-calculate-ucs-hash-table-max-len ucs-struct)
      (helm-calculate-ucs-alist-max-len ucs-struct))))

(defun helm-ucs-collect-symbols-alist (names)
  "Collect ucs symbols from the NAMES list."
  (cl-loop with pr = (make-progress-reporter
                      "collecting ucs names"
                      0 (length names))
           for (n . v) in names
           for count from 1
           for xcode = (format "#x%x:" v)
           for len = (length xcode)
           for diff = (- (car helm-ucs--max-len) len)
           for code = (format "(#x%x): " v)
           for char = (propertize (format "%c" v)
                                  'face 'helm-ucs-char)
           unless (or (string= "" n)
                      ;; `char-displayable-p' return a font object or
                      ;; t for some char that are displayable but have
                      ;; no special font (e.g 10) so filter out char
                      ;; with no font.
                      (not (fontp (char-displayable-p (read xcode)))))
           collect
           (concat code (make-string diff ? )
                   char "  " n)
           and do (progress-reporter-update pr count)))

(defun helm-ucs-collect-symbols-hash-table (names)
  "Collect ucs symbols from the NAMES hash-table."
  (cl-loop with pr = (make-progress-reporter
                      "collecting ucs names"
                      0 (hash-table-count names))
           for n being the hash-keys of names
           using (hash-values v)
           for count from 1
           for xcode = (format "#x%x:" v)
           for len = (length xcode)
           for diff = (- (car helm-ucs--max-len) len)
           for code = (format "(#x%x): " v)
           for char = (propertize (format "%c" v)
                                  'face 'helm-ucs-char)
           unless (or (string= "" n)
                      (not (fontp (char-displayable-p (read xcode)))))
           collect
           (concat code (make-string diff ? )
                   char "  " n)
           and do (progress-reporter-update pr count)))

(defun helm-ucs-collect-symbols (ucs-struct)
  "Collect ucs symbols from UCS-STRUCT.

Depending on the Emacs version, the variable `ucs-names' can
either be an alist or a hash-table."
  (if (hash-table-p ucs-struct)
      (helm-ucs-collect-symbols-hash-table ucs-struct)
    (helm-ucs-collect-symbols-alist ucs-struct)))

(defun helm-ucs-init ()
  "Initialize a Helm buffer with ucs symbols.
Only math* symbols are collected."
  (unless helm-ucs--max-len
    (setq helm-ucs--max-len
          (helm-calculate-ucs-max-len)))
  (or helm-ucs--names
      (setq helm-ucs--names
            (helm-ucs-collect-symbols (ucs-names)))))

;; Actions (insertion)

(defun helm-ucs-match (candidate n)
  "Return the N part of an ucs CANDIDATE.
Where N=1 is the ucs code, N=2 the ucs char and N=3 the ucs
name."
  (when (string-match
         "^(\\(#x[a-f0-9]+\\)): *\\(.\\) *\\([^:]+\\)+"
         candidate)
    (match-string n candidate)))

(defun helm-ucs-save-recentest (candidate)
  (let ((lst (cons candidate (delete candidate helm-ucs-recent))))
    (setq helm-ucs-recent
          (if (> (length lst) helm-ucs-recent-size)
              (nbutlast lst 1)
            lst))))

(defun helm-ucs-insert (candidate n)
  "Insert the N part of CANDIDATE."
  (with-helm-current-buffer
    (helm-ucs-save-recentest candidate)
    (insert (helm-ucs-match candidate n))))

(defun helm-ucs-insert-char (candidate)
  "Insert ucs char part of CANDIDATE at point."
  (helm-ucs-insert candidate 2))

(defun helm-ucs-insert-code (candidate)
  "Insert ucs code part of CANDIDATE at point."
  (helm-ucs-insert candidate 1))

(defun helm-ucs-insert-name (candidate)
  "Insert ucs name part of CANDIDATE at point."
  (helm-ucs-insert candidate 3))

;; Kill actions
(defun helm-ucs-kill-char (_candidate)
  "Action that concatenate ucs marked chars."
  (let ((marked (helm-marked-candidates)))
    (cl-loop for candidate in marked
             do (helm-ucs-save-recentest candidate))
    (kill-new (mapconcat (lambda (x)
                           (helm-ucs-match x 2))
                         marked ""))))

(defun helm-ucs-kill-code (candidate)
  (helm-ucs-save-recentest candidate)
  (kill-new (helm-ucs-match candidate 1)))

(defun helm-ucs-kill-name (candidate)
  (helm-ucs-save-recentest candidate)
  (kill-new (helm-ucs-match candidate 3)))

;; Describe char
(defun helm-ucs-describe-char (candidate)
  "Describe char CANDIDATE."
  (with-temp-buffer
    (insert (helm-ucs-match candidate 2))
    (describe-char (point-min))))

;; Navigation in current-buffer (persistent)

(defun helm-ucs-forward-char (_candidate)
  (with-helm-current-buffer
    (forward-char 1)))

(defun helm-ucs-backward-char (_candidate)
  (with-helm-current-buffer
    (forward-char -1)))

(defun helm-ucs-delete-backward (_candidate)
  (with-helm-current-buffer
    (delete-char -1)))

(defun helm-ucs-insert-space (_candidate)
  (with-helm-current-buffer
    (insert " ")))

(defun helm-ucs-persistent-forward ()
  (interactive)
  (with-helm-alive-p
    (helm-set-attr 'action-forward 'helm-ucs-forward-char)
    (helm-execute-persistent-action 'action-forward)))
(put 'helm-ucs-persistent-forward 'helm-only t)

(defun helm-ucs-persistent-backward ()
  (interactive)
  (with-helm-alive-p
    (helm-set-attr 'action-back 'helm-ucs-backward-char)
    (helm-execute-persistent-action 'action-back)))
(put 'helm-ucs-persistent-backward 'helm-only t)

(defun helm-ucs-persistent-delete ()
  (interactive)
  (with-helm-alive-p
    (helm-set-attr 'action-delete 'helm-ucs-delete-backward)
    (helm-execute-persistent-action 'action-delete)))
(put 'helm-ucs-persistent-delete 'helm-only t)

(defun helm-ucs-persistent-insert-space ()
  (interactive)
  (with-helm-alive-p
    (helm-set-attr 'action-insert-space 'helm-ucs-insert-space)
    (helm-execute-persistent-action 'action-insert-space)))

(defvar helm-source-ucs-recent
  (helm-build-sync-source "Recent UCS"
    :action 'helm-ucs-actions
    :candidates (lambda () helm-ucs-recent)
    :help-message helm-ucs-help-message
    :keymap helm-ucs-map
    :volatile t))

(defvar helm-source-ucs
  (helm-build-in-buffer-source "UCS names"
    :data #'helm-ucs-init
    :get-line #'buffer-substring
    :help-message 'helm-ucs-help-message
    :filtered-candidate-transformer
    (lambda (candidates _source) (sort candidates #'helm-generic-sort-fn))
    :action 'helm-ucs-actions
    :persistent-action (lambda (candidate)
                         (helm-ucs-insert-char candidate)
                         (helm-force-update))
    :keymap helm-ucs-map)
  "Source for collecting `ucs-names' math symbols.")

;;;###autoload
(defun helm-select-xfont ()
  "Preconfigured `helm' to select Xfont."
  (interactive)
  (helm :sources 'helm-source-xfonts
        :buffer "*helm select xfont*"))

;;;###autoload
(defun helm-ucs (arg)
  "Preconfigured `helm' for `ucs-names'.

Called with a prefix arg force reloading cache."
  (interactive "P")
  (when arg
    (setq helm-ucs--names nil
          helm-ucs--max-len nil
          ucs-names nil))
  (let ((char (helm-aif (char-after) (string it))))
    (helm :sources (list helm-source-ucs-recent helm-source-ucs)
          :history 'helm-ucs-history
          :input (and char (multibyte-string-p char) char)
          :buffer "*helm ucs*")))

(provide 'helm-font)

;;; helm-font.el ends here
