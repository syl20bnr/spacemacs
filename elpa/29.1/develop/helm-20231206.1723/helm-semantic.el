;;; helm-semantic.el --- Helm interface for Semantic -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2017 Daniel Hackney <dan@haxney.org>
;;               2012 ~ 2023  Thierry Volpiatto

;; Author: Daniel Hackney <dan@haxney.org>

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

;; Uses `candidates-in-buffer' for speed.

;;; Code:

(require 'cl-lib)
(require 'semantic)
(require 'helm-help)
(require 'helm-imenu)

(declare-function pulse-momentary-highlight-one-line "pulse.el" (point &optional face))

(defgroup helm-semantic nil
  "Semantic tags related libraries and applications for helm."
  :group 'helm)

(defcustom helm-semantic-display-style
  '((python-mode . semantic-format-tag-summarize)
    (c-mode . semantic-format-tag-concise-prototype-c-mode)
    (emacs-lisp-mode . semantic-format-tag-abbreviate-emacs-lisp-mode))
  "Function to present a semantic tag according to `major-mode'.

It is an alist where the `car' of each element is a `major-mode' and
the `cdr' a `semantic-format-tag-*' function.

If no function is found for current `major-mode', fall back to
`semantic-format-tag-summarize' default function.

You can have more or less informations depending of the `semantic-format-tag-*'
function you choose.

All the supported functions are prefixed with \"semantic-format-tag-\",
you have completion on these functions with `C-M i' in the customize interface."
  :group 'helm-semantic
  :type '(alist :key-type symbol :value-type symbol))

;;; keymap
(defvar helm-semantic-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    map))

(defcustom helm-semantic-lynx-style-map nil
  "Use Arrow keys to jump to occurences."
  :group 'helm-semantic
  :type  'boolean
  :set (lambda (var val)
         (set var val)
         (if val
             (progn
               (define-key helm-semantic-map (kbd "<right>")  'helm-execute-persistent-action)
               (define-key helm-semantic-map (kbd "<left>")   'helm-maybe-exit-minibuffer))
           (define-key helm-semantic-map (kbd "<right>") nil)
           (define-key helm-semantic-map (kbd "<left>")  nil))))

;; Internals vars
(defvar helm-semantic--tags-cache nil)

(defun helm-semantic--fetch-candidates (tags depth &optional class)
  "Write the contents of TAGS to the current buffer."
  (let ((class class) cur-type
        (stylefn (or (with-helm-current-buffer
                       (assoc-default major-mode helm-semantic-display-style))
                     #'semantic-format-tag-summarize)))
    (dolist (tag tags)
      (when (listp tag)
        (cl-case (setq cur-type (semantic-tag-class tag))
          ((function variable type)
           (let ((spaces (make-string (* depth 2) ?\s))
                 (type-p (eq cur-type 'type)))
             (unless (and (> depth 0) (not type-p))
               (setq class nil))
             (insert
              (if (and class (not type-p))
                  (format "%s%s(%s) "
                          spaces (if (< depth 2) "" "├►") class)
                spaces)
              ;; Save the tag for later
              (propertize (funcall stylefn tag nil t)
                          'semantic-tag tag)
              "\n")
             (and type-p (setq class (car tag)))
             ;; Recurse to children
             (unless (eq cur-type 'function)
               (helm-semantic--fetch-candidates
                (semantic-tag-components tag) (1+ depth) class))))

          ;; Don't do anything with packages or includes for now
          ((package include)
           (insert
            (propertize (funcall stylefn tag nil t)
                        'semantic-tag tag)
            "\n")
           )
          ;; Catch-all
          (t))))))

(defun helm-semantic-default-action (_candidate &optional persistent)
  ;; By default, helm doesn't pass on the text properties of the selection.
  ;; Fix this.
  (helm-log-run-hook "helm-semantic-default-action"
                     'helm-goto-line-before-hook)
  (with-current-buffer helm-buffer
    (when (looking-at " ")
      (goto-char (next-single-property-change
                  (pos-bol) 'semantic-tag nil (pos-eol))))
    (let ((tag (get-text-property (point) 'semantic-tag)))
      (semantic-go-to-tag tag)
      (unless persistent
        (pulse-momentary-highlight-one-line (point))))))

(defun helm-semantic--maybe-set-needs-update ()
  (with-helm-current-buffer
    (when (semantic-parse-tree-needs-update-p)
      (semantic-parse-tree-set-needs-update))))

(defvar helm-source-semantic nil)

(defclass helm-semantic-source (helm-source-in-buffer)
  ((init :initform (lambda ()
                     (helm-semantic--maybe-set-needs-update)
                     (setq helm-semantic--tags-cache (semantic-fetch-tags))
                     (with-current-buffer (helm-candidate-buffer 'global)
                       (let ((major-mode (with-helm-current-buffer major-mode)))
                         (helm-semantic--fetch-candidates helm-semantic--tags-cache 0)))))
   (get-line :initform 'buffer-substring)
   (persistent-help :initform "Show this entry")
   (keymap :initform 'helm-semantic-map)
   (help-message :initform 'helm-semantic-help-message)
   (persistent-action :initform (lambda (elm)
                                  (helm-semantic-default-action elm t)
                                  (helm-highlight-current-line)))
   (action :initform 'helm-semantic-default-action)))

(defcustom helm-semantic-fuzzy-match nil
  "Enable fuzzy matching in `helm-source-semantic'."
  :group 'helm-semantic
  :type  'boolean
  :set (lambda (var val)
         (set var val)
         (setq helm-source-semantic
               (helm-make-source "Semantic Tags" 'helm-semantic-source
                 :fuzzy-match helm-semantic-fuzzy-match))))

;;;###autoload
(defun helm-semantic (arg)
  "Preconfigured `helm' for `semantic'.
If ARG is supplied, pre-select symbol at point instead of current."
  (interactive "P")
  (let ((tag (helm-aif (car (semantic-current-tag-parent))
                 (let ((curtag (car (semantic-current-tag))))
                   (if (string= it curtag)
                       (format "\\_<%s\\_>" curtag)
                     (cons (format "\\_<%s\\_>" it)
                           (format "\\_<%s\\_>" curtag))))
               (format "\\_<%s\\_>" (car (semantic-current-tag)))))
        (helm-highlight-matches-around-point-max-lines 'never))
    (unless helm-source-semantic
      (setq helm-source-semantic
            (helm-make-source "Semantic Tags" 'helm-semantic-source
              :fuzzy-match helm-semantic-fuzzy-match)))
    (helm :sources 'helm-source-semantic
          :candidate-number-limit 9999
          :preselect (if arg
                         (thing-at-point 'symbol)
                       tag)
          :buffer "*helm semantic*")))

;;;###autoload
(defun helm-semantic-or-imenu (arg)
  "Preconfigured helm for `semantic' or `imenu'.
If ARG is supplied, pre-select symbol at point instead of current
semantic tag in scope.

If `semantic-mode' is active in the current buffer, then use
semantic for generating tags, otherwise fall back to `imenu'.
Fill in the symbol at point by default."
  (interactive "P")
  (unless helm-source-semantic
    (setq helm-source-semantic
          (helm-make-source "Semantic Tags" 'helm-semantic-source
            :fuzzy-match helm-semantic-fuzzy-match)))
  (unless helm-source-imenu
    (setq helm-source-imenu
          (helm-make-source "Imenu" 'helm-imenu-source
            :fuzzy-match helm-imenu-fuzzy-match)))
  (let* ((source (if (semantic-active-p)
                     'helm-source-semantic
                     'helm-source-imenu))
         (helm-highlight-matches-around-point-max-lines 'never)
         (imenu-p (eq source 'helm-source-imenu))
         (imenu-auto-rescan imenu-p)
         (str (thing-at-point 'symbol))
         (helm-execute-action-at-once-if-one
          (and imenu-p
               helm-imenu-execute-action-at-once-if-one))
         (tag (helm-aif (car (semantic-current-tag-parent))
                  (let ((curtag (car (semantic-current-tag))))
                    (if (string= it curtag)
                        (format "\\_<%s\\_>" curtag)
                      (cons (format "\\_<%s\\_>" it)
                            (format "\\_<%s\\_>" curtag))))
                (format "\\_<%s\\_>" (car (semantic-current-tag))))))
    (helm :sources source
          :candidate-number-limit 9999
          :default (and imenu-p (list (concat "\\_<" (and str (regexp-quote str)) "\\_>") str))
          :preselect (if (or arg imenu-p) str tag)
          :buffer "*helm semantic/imenu*")))

(provide 'helm-semantic)

;;; helm-semantic.el ends here
