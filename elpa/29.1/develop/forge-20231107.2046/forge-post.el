;;; forge-post.el --- Post support  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2023 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'markdown-mode)

(require 'forge)

;;; Options

(defcustom forge-post-mode-hook
  '(visual-line-mode
    turn-on-flyspell)
  "Hook run after entering Forge-Post mode."
  :package-version '(forge . "0.2.0")
  :group 'forge
  :type 'hook
  :options '(visual-line-mode
             turn-on-flyspell))

(defcustom forge-buffer-draft-p nil
  "Whether new pull-requests start out as drafts by default.

The buffer-local value is use to keep track of the draft status
of the current pull-request."
  :package-version '(forge . "0.4.0")
  :group 'forge
  :type 'boolean)

;;; Class

(defclass forge-post (forge-object) () :abstract t)

;;; Query
;;;; Get

(cl-defmethod forge-get-parent ((post forge-post))
  (forge-get-topic post))

(cl-defmethod forge-get-repository ((post forge-post))
  (forge-get-repository (forge-get-topic post)))

;;;; Current

(defun forge-post-at-point (&optional assert)
  "Return the post at point.
If there is no such post and DEMAND is non-nil, then signal
an error."
  (or (magit-section-value-if '(issue pullreq post))
      (and assert (user-error "There is no post at point"))))

(defun forge-comment-at-point (&optional assert)
  "Return the comment at point.
If there is no such comment and DEMAND is non-nil, then signal
an error."
  (or (and (magit-section-value-if '(post))
           (let ((post (oref (magit-current-section) value)))
             (and (or (forge-pullreq-post-p post)
                      (forge-issue-post-p post))
                  post)))
      (and assert (user-error "There is no comment at point"))))

;;; Utilities

(cl-defmethod forge--format ((post forge-post) slot &optional spec)
  (forge--format (forge-get-topic post) slot
                 `(,@spec (?I . ,(oref post number)))))

;;; Mode

(defvar-keymap forge-post-mode-map
  "C-c C-e"                                #'forge-post-dispatch
  "C-c C-c"                                #'forge-post-submit
  "<remap> <evil-save-and-close>"          #'forge-post-submit
  "<remap> <evil-save-modified-and-close>" #'forge-post-submit
  "C-c C-k"                                #'forge-post-cancel
  "<remap> <kill-buffer>"                  #'forge-post-cancel
  "<remap> <ido-kill-buffer>"              #'forge-post-cancel
  "<remap> <iswitchb-kill-buffer>"         #'forge-post-cancel
  "<remap> <evil-quit>"                    #'forge-post-cancel)

(define-derived-mode forge-post-mode gfm-mode "Forge-Post" "")

(defvar-local forge--buffer-base-branch nil)
(defvar-local forge--buffer-head-branch nil)
(defvar-local forge--buffer-post-object nil)
(defvar-local forge--buffer-issue nil)
(defvar-local forge--submit-post-function nil)
(defvar-local forge--cancel-post-function nil)
(defvar-local forge--pre-post-buffer nil)
(make-variable-buffer-local 'forge-buffer-draft-p)

(defun forge--prepare-post-buffer (filename &optional header source target)
  (let ((file (convert-standard-filename
               (expand-file-name (concat "magit/posts/" filename)
                                 (magit-gitdir)))))
    (make-directory (file-name-directory file) t)
    (let ((prevbuf (current-buffer))
          (resume (and (file-exists-p file)
                       (> (file-attribute-size (file-attributes file)) 0)))
          (buf (find-file-noselect file)))
      (with-current-buffer buf
        (forge-post-mode)
        (when header
          (magit-set-header-line-format header))
        (setq forge--pre-post-buffer prevbuf)
        (when resume
          (forge--display-post-buffer buf)
          (when (magit-read-char-case "A draft already exists.  " nil
                  (?r "[r]esume editing existing draft")
                  (?d "[d]iscard draft and start over" t))
            (erase-buffer)
            (setq resume nil)))
        (when (and (not resume) (string-prefix-p "new" filename))
          (let-alist (forge--topic-template
                      (forge-get-repository t)
                      (if source 'forge-pullreq 'forge-issue))
            (cond
             (.url
              (browse-url .url)
              (forge-post-cancel)
              (setq buf nil)
              (message "Using browser to visit %s instead of opening an issue"
                       .url))
             (.name
              ;; A Github issue with yaml frontmatter.
              (save-excursion (insert .text))
              (unless (re-search-forward "^title: " nil t)
                (when (re-search-forward "^---" nil t 2)
                  (beginning-of-line)
                  (insert "title: \n")
                  (backward-char))))
             (t
              (insert "# ")
              (let ((single
                     (and source
                          (= (car (magit-rev-diff-count source target)) 1))))
                (save-excursion
                  (when single
                    ;; A pull-request.
                    (magit-rev-insert-format "%B" source))
                  (when .text
                    (if single
                        (insert "-------\n")
                      (insert "\n"))
                    (insert "\n" .text)))))))))
      buf)))

(defun forge--display-post-buffer (buf)
  (magit-display-buffer buf #'display-buffer))

(defun forge-post-cancel ()
  "Cancel the post that is being edited in the current buffer."
  (interactive)
  (save-buffer)
  (if-let ((fn forge--cancel-post-function))
      (funcall fn forge--buffer-post-object)
    (magit-mode-bury-buffer 'kill)))

(defun forge-post-submit ()
  "Submit the post that is being edited in the current buffer."
  (interactive)
  (save-buffer)
  (if-let ((fn forge--submit-post-function))
      (funcall fn
               (forge-get-repository forge--buffer-post-object)
               forge--buffer-post-object)
    (error "forge--submit-post-function is nil")))

(defun forge--post-submit-callback ()
  (let* ((file    buffer-file-name)
         (editbuf (current-buffer))
         (prevbuf forge--pre-post-buffer)
         (topic   (ignore-errors (forge-get-topic forge--buffer-post-object)))
         (repo    (forge-get-repository topic)))
    (lambda (value headers status req)
      (run-hook-with-args 'forge-post-submit-callback-hook
                          value headers status req)
      (delete-file file t)
      (let ((dir (file-name-directory file)))
        (unless (cddr (directory-files dir nil nil t))
          (delete-directory dir nil t)))
      (when (buffer-live-p editbuf)
        (with-current-buffer editbuf
          (magit-mode-bury-buffer 'kill)))
      (with-current-buffer
          (if (buffer-live-p prevbuf) prevbuf (current-buffer))
        (if (and topic
                 (forge--childp repo 'forge-github-repository)
                 (or (and (fboundp 'forge-pullreq-p)
                          (forge-pullreq-p topic))
                     (oref repo selective-p)))
            (forge--pull-topic repo topic)
          (forge-pull))))))

(defun forge--post-submit-errorback ()
  (lambda (error &rest _)
    (error "Failed to submit post: %S" error)))

(transient-define-prefix forge-post-dispatch ()
  "Dispatch a post creation command."
  ["Variables"
   ("d" "Create draft" forge-post-toggle-draft)]
  ["Act"
   ("C-c" "Submit" forge-post-submit)
   ("C-k" "Cancel" forge-post-cancel)])

(transient-define-infix forge-post-toggle-draft ()
  "Toggle whether the pull-request being created is a draft."
  :class 'transient-lisp-variable
  :variable 'forge-buffer-draft-p
  :reader (lambda (&rest _) (not forge-buffer-draft-p))
  :if (lambda () (equal (file-name-nondirectory buffer-file-name) "new-pullreq")))

;;; Notes

(defclass forge-note (forge-post) ())

(defvar-keymap forge-note-section-map
  "<remap> <magit-edit-thing>" #'forge-edit-topic-note)

(defun forge--save-note (_repo topic)
  (let ((value (string-trim (buffer-substring-no-properties
                             (point-min)
                             (point-max)))))
    (oset topic note (if (equal value "") nil value)))
  (delete-file buffer-file-name t)
  (let ((dir (file-name-directory buffer-file-name)))
    (unless (cddr (directory-files dir nil nil t))
      (delete-directory dir)))
  (let ((prevbuf forge--pre-post-buffer))
    (magit-mode-bury-buffer 'kill)
    (when (buffer-live-p prevbuf)
      (magit-refresh))))

;;; _
(provide 'forge-post)
;;; forge-post.el ends here
