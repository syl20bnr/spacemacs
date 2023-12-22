;;; forge-list.el --- Tabulated-list interface  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2024 Jonas Bernoulli

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

(require 'hl-line)
(require 'tabulated-list)

(require 'forge)

(defvar x-stretch-cursor)

;;; Options

(defcustom forge-topic-list-mode-hook '(hl-line-mode)
  "Hook run after entering Forge-Topic-List mode."
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type 'hook
  :options '(hl-line-mode))

(defconst forge--tablist-columns-type
  '(repeat
    (list :tag "Column"
          (string  :tag "Header Label")
          (choice  :tag "Value source"
                   (function)
                   (symbol :tag "Object slot"))
          (integer :tag "Column Width")
          (choice  :tag "Sort predicate"
                   (const :tag "Don't sort" nil)
                   (const :tag "Default" t)
                   (function))
          (repeat  :tag "Properties"
                   (list (choice :tag "Property"
                                 (const :right-align)
                                 (const :pad-right)
                                 (symbol))
                         (sexp :tag "Value"))))))

(defcustom forge-topic-list-columns
  '(("#"     forge--format-topic-slug          5 nil nil)
    ("Title" forge--format-topic-title+labels 35 nil nil))
  "List of columns displayed when listing topics for a single repository.

Each element has the form (HEADER SOURCE WIDTH SORT PROPS).

HEADER is the string displayed in the header.  WIDTH is the width
of the column.  SOURCE is used to get the value, it has to be the
name of a slot of `forge-topic' or a function that takes such an
object as argument.  SORT is a boolean or a function used to sort
by this column.  Supported PROPS include `:right-align' and
`:pad-right'."
  :package-version '(forge . "0.4.0")
  :group 'forge
  :type forge--tablist-columns-type)

(defcustom forge-global-topic-list-columns
  '(("Owner" (repository owner)               15 nil nil)
    ("Name"  (repository name)                20 nil nil)
    ("#"     forge--format-topic-slug          5 nil nil)
    ("Title" forge--format-topic-title+labels 35 nil nil))
  "List of columns displayed when listing topics for all repositories.

Each element has the form (HEADER SOURCE WIDTH SORT PROPS).

HEADER is the string displayed in the header.  WIDTH is the width
of the column.  SOURCE is used to get the value, it has to be the
name of a slot of `forge-topic' or a function that takes such an
object as argument.  SORT is a boolean or a function used to sort
by this column.  Supported PROPS include `:right-align' and
`:pad-right'."
  :package-version '(forge . "0.4.0")
  :group 'forge
  :type forge--tablist-columns-type)

(defcustom forge-repository-list-columns
  '(("Owner"    owner         20   t nil)
    ("Name"     name          20   t nil)
    ("N"        sparse-p       1   t nil)
    ("S"        selective-p    1   t nil)
    ("Worktree" worktree      99   t nil))
  "List of columns displayed when listing repositories.

Each element has the form (HEADER SOURCE WIDTH SORT PROPS).

HEADER is the string displayed in the header.  WIDTH is the width
of the column.  SOURCE is used to get the value, it has to be the
name of a slot of `forge-repository' or a function that takes
such an object as argument.  SORT is a boolean or a function used
to sort by this column.  Supported PROPS include `:right-align'
and `:pad-right'."
  :package-version '(forge . "0.4.0")
  :group 'forge
  :type forge--tablist-columns-type)

(defcustom forge-owned-accounts nil
  "An alist of accounts that are owned by you.
This should include your username as well as any organization
that you own.  Used by the commands `forge-list-owned-issues',
`forge-list-owned-pullreqs' and `forge-fork'.

Each element has the form (ACCOUNT . PLIST).  The following
properties are currently being used:

`remote-name' The default name suggested by `forge-fork' for a
  fork created within this account.  If unspecified, then the
  name of the account is used."
  :package-version '(forge . "0.2.0")
  :group 'forge
  :type '(repeat (cons (string :tag "Account") plist)))

(defcustom forge-owned-ignored nil
  "A list of repositories that are ignored when listing those owned by you.
This is a list of package names.  Used by the commands
`forge-list-owned-issues' and `forge-list-owned-pullreqs'."
  :package-version '(forge . "0.2.0")
  :group 'forge
  :type '(repeat (string :tag "Name")))

;;; Faces

(defface forge-active-suffix '((t :inherit transient-value))
  "Face used for suffixes whose effects is currently active."
  :group 'forge)

(defface forge-tablist-hl-line
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :box ( :line-width ,(if (>= emacs-major-version 28) (cons -1 -1) -1)
            :color "grey25"
            :style nil))
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :box ( :line-width ,(if (>= emacs-major-version 28) (cons -1 -1) -1)
            :color "grey75"
            :style nil)))
  "Face uses instead of `hl-line' in Forge's `tabulated-list-mode' buffers.
It is recommended that you stick to using a box for this purpose,
as using the background color would shadow the background colors
used for labels."
  :group 'forge-faces)

(defface forge-tablist-topic-label
  `((t :inherit forge-topic-label))
  "Face used for topic labels in Forge's `tabulated-list-mode' buffers.
This face can be used to control whether a box is added to labels
and how that is styled.  The background colors used for any given
label, cannot be changed independently of the color used in the
forges web interface."
  :group 'forge-faces)

;;; Variables

(defvar-local forge--tabulated-list-columns nil)
(put 'forge--tabulated-list-columns 'permanent-local t)

(defvar-local forge--tabulated-list-query nil)
(put 'forge--tabulated-list-query 'permanent-local t)

(defvar-local forge--buffer-list-type nil)
(defvar-local forge--buffer-list-filter nil)
(defvar-local forge--buffer-list-global nil)

;;; Modes
;;;; Topics

(defvar-keymap forge-topic-list-mode-map
  :doc "Local keymap for Forge-Topic-List mode buffers."
  :parent tabulated-list-mode-map
  "RET"      #'forge-visit-this-topic
  "<return>" #'forge-visit-this-topic
  "o"        #'forge-browse-this-topic
  "L"        #'forge-topics-menu
  "'"        #'forge-dispatch
  "?"        #'magit-dispatch)

(defvar forge-topic-list-mode-name
  '((:eval
     (let ((info (capitalize
                  (concat (if forge--buffer-list-filter
                              (format "%s " forge--buffer-list-filter)
                            "")
                          (if forge--buffer-list-type
                              (format "%ss" forge--buffer-list-type)
                            "topics")))))
       (if (fboundp 'moody-tab) (moody-tab info) info))))
  "Information shown in the mode-line for `forge-topic-list-mode'.
Must be set before `forge-list' is loaded.")

(define-derived-mode forge-topic-list-mode tabulated-list-mode
  forge-topic-list-mode-name
  "Major mode for browsing a list of topics."
  (setq-local x-stretch-cursor  nil)
  (setq-local hl-line-face 'forge-tablist-hl-line)
  (setq tabulated-list-padding  0)
  (setq tabulated-list-sort-key (cons "#" nil)))

(defun forge-topic-get-buffer (&optional repo create)
  (let ((name (if repo
                  (format "*forge-topics: %s*" (oref repo slug))
                "*forge-topics*")))
    (if create
        (get-buffer-create name)
      (get-buffer name))))

(defun forge-topic-list-setup (type filter fn &optional repo global columns)
  (let* ((repo (or repo
                   (and (not global)
                        (forge-get-repository t))))
         (topdir (and repo (oref repo worktree)))
         (buffer (forge-topic-get-buffer repo t)))
    (with-current-buffer buffer
      (setq default-directory (or topdir "/"))
      (setq forge-buffer-repository repo)
      (setq forge--tabulated-list-columns (or columns forge-topic-list-columns))
      (setq forge--tabulated-list-query
            (cond ((not (functionp fn))
                   (lambda ()
                     (cl-sort (mapcan (-cut funcall <> repo) fn)
                              #'> :key (-cut oref <> number))))
                  (repo (apply-partially fn repo))
                  (fn)))
      (cl-letf (((symbol-function #'tabulated-list-revert) #'ignore)) ; see #229
        (forge-topic-list-mode))
      (setq forge--buffer-list-type type)
      (setq forge--buffer-list-filter filter)
      (setq forge--buffer-list-global global)
      (forge-topic-list-refresh)
      (add-hook 'tabulated-list-revert-hook
                #'forge-topic-list-refresh nil t)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (when hl-line-mode
        (hl-line-highlight)))
    (switch-to-buffer buffer)))

(defun forge-topic-list-refresh ()
  (setq tabulated-list-format
        (vconcat (mapcar (pcase-lambda (`(,name ,_get ,width ,sort ,props))
                           `(,name ,width ,sort . ,props))
                         forge--tabulated-list-columns)))
  (tabulated-list-init-header)
  (setq tabulated-list-entries
        (mapcar
         (lambda (topic)
           (list (oref topic id)
                 (vconcat
                  (mapcar (pcase-lambda (`(,_name ,get ,_width ,_sort ,_props))
                            (cond
                             ((functionp get)
                              (funcall get topic))
                             ((eq (car-safe get) 'repository)
                              (eieio-oref (forge-get-repository topic)
                                          (cadr get)))
                             ((eieio-oref topic get))))
                          forge--tabulated-list-columns))))
         (funcall forge--tabulated-list-query))))

;;;; Repository

(defvar-keymap forge-repository-list-mode-map
  :doc "Local keymap for Forge-Repository-List mode buffers."
  :parent tabulated-list-mode-map
  "RET"      #'forge-visit-this-repository
  "<return>" #'forge-visit-this-repository
  "o"        #'forge-browse-this-repository
  "L"        #'forge-repository-menu
  "'"        #'forge-dispatch
  "?"        #'magit-dispatch)

(defvar forge-repository-list-buffer-name "*forge-repositories*"
  "Buffer name to use for displaying lists of repositories.")

(defvar forge-repository-list-mode-name
  '((:eval
     (let ((info (capitalize
                  (concat (if forge--buffer-list-filter
                              (format "%s " forge--buffer-list-filter)
                            "")
                          "repositories"))))
       (if (fboundp 'moody-tab) (moody-tab info) info))))
  "Information shown in the mode-line for `forge-repository-list-mode'.
Must be set before `forge-list' is loaded.")

(define-derived-mode forge-repository-list-mode tabulated-list-mode
  forge-repository-list-mode-name
  "Major mode for browsing a list of repositories."
  (setq-local x-stretch-cursor  nil)
  (setq forge--tabulated-list-columns forge-repository-list-columns)
  (setq tabulated-list-padding  0)
  (setq tabulated-list-sort-key (cons "Owner" nil))
  (setq tabulated-list-format
        (vconcat (mapcar (pcase-lambda (`(,name ,_get ,width ,sort ,props))
                           `(,name ,width ,sort . ,props))
                         forge--tabulated-list-columns)))
  (tabulated-list-init-header))

(defun forge-repository-list-setup (filter fn)
  (let ((buffer (get-buffer-create forge-repository-list-buffer-name)))
    (with-current-buffer buffer
      (setq default-directory "/")
      (cl-letf (((symbol-function #'tabulated-list-revert) #'ignore)) ; see #229
        (forge-repository-list-mode))
      (funcall fn)
      (setq forge--buffer-list-type 'repo)
      (setq forge--buffer-list-filter filter)
      (setq forge--buffer-list-global t)
      (add-hook 'tabulated-list-revert-hook fn nil t)
      (tabulated-list-print))
    (switch-to-buffer buffer)))

(defun forge-repository-list-refresh ()
  (forge-repository--tabulate-entries))

(defun forge-repository-list-owned-refresh ()
  (forge-repository--tabulate-entries
   [:where (and (in owner $v2) (not (in name $v3)))]
   (vconcat (mapcar #'car forge-owned-accounts))
   (vconcat forge-owned-ignored)))

(defun forge-repository--tabulate-entries (&optional where &rest args)
  (setq tabulated-list-entries
        (mapcar
         (pcase-lambda (`(,id . ,row))
           (list id (vconcat (mapcar (lambda (v) (if v (format "%s" v) "")) row))))
         (apply #'forge-sql
                (vconcat [:select $i1 :from repository]
                         where
                         [:order-by [(asc owner) (asc name)]])
                (vconcat [id] (mapcar #'cadr forge--tabulated-list-columns))
                args))))

;;; Commands
;;;; Menus

;;;###autoload (autoload 'forge-topics-menu "forge-list" nil t)
(transient-define-prefix forge-topics-menu ()
  "Control list of topics and topic at point."
  :transient-suffix t
  :transient-non-suffix t
  :transient-switch-frame nil
  :refresh-suffixes t
  [:hide always ("q" forge-menu-quit-list)]
  [["Type"
    (:info "topics           " :face forge-active-suffix)
    ("n"   "notifications... " forge-notification-menu :transient replace)
    ("r"   "repositories...  " forge-repository-menu   :transient replace)]
   [:description (lambda ()
                   (if forge--buffer-list-global
                       "Per-repository lists"
                     "Subtype"))
    ("t" "topics"           forge-list-topics)
    ("i" "issues"           forge-list-issues)
    ("p" "pull-requests"    forge-list-pullreqs)
    ""]
   ["Filter"
    :if (lambda () (and (not forge--buffer-list-global)
                   (eq forge--buffer-list-type 'topic)))
    ("l" "labeled"          forge-list-labeled-topics)
    ("c" "created"          forge-list-authored-topics)
    ("a" "assigned"         forge-list-assigned-topics)]
   ["Filter"
    :if (lambda () (and (not forge--buffer-list-global)
                   (eq forge--buffer-list-type 'issue)))
    ("l" "labeled"          forge-list-labeled-issues)
    ("c" "created"          forge-list-authored-issues)
    ("a" "assigned"         forge-list-assigned-issues)]
   ["Filter"
    :if (lambda () (and (not forge--buffer-list-global)
                   (eq forge--buffer-list-type 'pullreq)))
    ("l" "labeled"          forge-list-labeled-pullreqs)
    ("c" "created"          forge-list-authored-pullreqs)
    ("a" "assigned"         forge-list-assigned-pullreqs)
    ("w" "awaiting review"  forge-list-requested-reviews)]]
  [["Set state"
    ("s o" forge-topic-state-set-open)
    ("s c" forge-issue-state-set-completed)
    ("s u" forge-issue-state-set-unplanned)
    ("s m" forge-pullreq-state-set-merged)
    ("s x" forge-pullreq-state-set-closed)
    """Set status"
    ("s i" forge-topic-status-set-unread)
    ("s p" forge-topic-status-set-pending)
    ("s d" forge-topic-status-set-done)
    ("s s" forge-topic-toggle-saved)]
   ["Global lists"
    ("o t" "owned topics"        forge-list-owned-topics)
    ("o i" "owned issues"        forge-list-owned-issues)
    ("o p" "owned pull-requests" forge-list-owned-pullreqs)]
   ["Actions"
    ("f" "fetch all topics"  forge-pull)
    ("m" "show more actions" forge-dispatch)]]
  (interactive)
  (catch 'add-instead
    (unless (derived-mode-p 'forge-topic-list-mode)
      (let ((repo (forge-current-repository)))
        (cond
         ((or (not repo) (not (oref repo sparse-p))))
         ((yes-or-no-p
           (format "Add %s to database, so its topics can be listed?"
                   (oref repo slug)))
          (oset repo sparse-p nil)
          (forge--pull repo nil #'ignore)
          (throw 'add-instead t))
         ((setq repo nil)))
        (if-let ((buffer (forge-topic-get-buffer repo)))
            (switch-to-buffer buffer)
          (if repo
              (forge-list-topics repo)
            (forge-list-owned-topics)))))
    (transient-setup 'forge-topics-menu)))

;;;###autoload (autoload 'forge-repository-menu "forge-list" nil t)
(transient-define-prefix forge-repository-menu ()
  "Control list of repositories and repository at point."
  :transient-suffix t
  :transient-non-suffix 'call
  :transient-switch-frame nil
  :refresh-suffixes t
  [:hide always ("q" forge-menu-quit-list)]
  [["Type"
    ("t" "topics..."        forge-topics-menu       :transient replace)
    ("n" "notifications..." forge-notification-menu :transient replace)
    ("r" "repositories"     forge-list-repositories)]
   ["Filter"
    ("o" "owned" forge-list-owned-repositories)]]
  (interactive)
  (unless (derived-mode-p 'forge-repository-list-mode)
    (if-let ((buffer (get-buffer forge-repository-list-buffer-name)))
        (switch-to-buffer buffer)
      (with-no-warnings ; "interactive use only"
        (forge-list-repositories))))
  (transient-setup 'forge-repository-menu))

(defun forge-menu-quit-list ()
  "From a transient menu, quit the list buffer and the menu.

If quitting the list buffer causes another topic, repository
or notification list buffer to becomes current in the selected
window, then display the respective menu, otherwise display no
menu."
  (interactive)
  (when (derived-mode-p 'forge-topic-list-mode
                        'forge-repository-list-mode
                        'forge-notifications-mode)
    (quit-window))
  (cond ((derived-mode-p 'forge-topic-list-mode)
         (setq transient--exitp 'replace)
         (transient-setup (setq this-command 'forge-topics-menu)))
        ((derived-mode-p 'forge-repository-list-mode)
         (setq transient--exitp 'replace)
         (transient-setup (setq this-command 'forge-repository-menu)))
        ((derived-mode-p 'forge-notifications-mode)
         (setq transient--exitp 'replace)
         (transient-setup (setq this-command 'forge-notification-menu)))
        (t
         (setq transient--exitp t)
         (transient--pre-exit)
         (transient--stack-zap))))

;;;; Suffix Class

(defclass forge--topic-list-command (transient-suffix)
  ((type       :initarg :type   :initform nil)
   (filter     :initarg :filter :initform nil)
   (global     :initarg :global :initform nil)
   (inapt-if                    :initform 'forge--topic-list-inapt)
   (inapt-face                  :initform nil)))

(defun forge--topic-list-inapt ()
  (transient-with-shadowed-buffer
    (with-slots (type filter global) transient--pending-suffix
      (and (eq type   forge--buffer-list-type)
           (eq filter forge--buffer-list-filter)
           (eq global forge--buffer-list-global)))))

(cl-defmethod transient-format-description ((obj forge--topic-list-command))
  (transient-with-shadowed-buffer
    (with-slots (description type filter global) obj
      (if (and (eq   type   forge--buffer-list-type)
               (memq filter (list nil forge--buffer-list-filter))
               (eq   global forge--buffer-list-global))
          (propertize description 'face 'forge-active-suffix)
        description))))

;;;; Topic

(defun forge--topic-list-setup (filter fn &optional repo global columns)
  (forge-topic-list-setup 'topic filter fn repo global columns))

;;;###autoload (autoload 'forge-list-topics "forge-list" nil t)
(transient-define-suffix forge-list-topics (&optional repository)
  "List topics of the current repository.
Non-interactively if optional REPOSITORY is non-nil, then list
topics for that instead."
  :class 'forge--topic-list-command :type 'topic
  (interactive)
  (forge--topic-list-setup nil (list #'forge-ls-issues #'forge-ls-pullreqs)
                           repository))
(put 'forge-list-topics 'interactive-only nil)

;;;###autoload (autoload 'forge-list-labeled-topics "forge-list" nil t)
(transient-define-suffix forge-list-labeled-topics (label)
  "List topics of the current repository that have LABEL."
  :class 'forge--topic-list-command :type 'topic :filter 'labeled
  (interactive (list (forge-read-topic-label)))
  (forge--topic-list-setup 'labeled
                           (list (-cut forge--ls-labeled-issues   <> label)
                                 (-cut forge--ls-labeled-pullreqs <> label))))

;;;###autoload (autoload 'forge-list-assigned-topics "forge-list" nil t)
(transient-define-suffix forge-list-assigned-topics ()
  "List topics of the current repository that are assigned to you."
  :class 'forge--topic-list-command :type 'topic :filter 'assigned
  (interactive)
  (forge--topic-list-setup 'assigned
                           (list #'forge--ls-assigned-issues
                                 #'forge--ls-assigned-pullreqs)))

;;;###autoload (autoload 'forge-list-authored-topics "forge-list" nil t)
(transient-define-suffix forge-list-authored-topics ()
  "List open topics from the current repository that are authored by you."
  :class 'forge--topic-list-command :type 'topic :filter 'authored
  (interactive)
  (forge--topic-list-setup 'authored
                           (list #'forge--ls-authored-issues
                                 #'forge--ls-authored-pullreqs)))

;;;###autoload (autoload 'forge-list-owned-topics "forge-list" nil t)
(transient-define-suffix forge-list-owned-topics ()
  "List open pull-requests from all your Github repositories.
Options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now."
  :class 'forge--topic-list-command :type 'topic :filter 'owned :global t
  (interactive)
  (forge--topic-list-setup 'owned
                           (list (lambda (_) (forge--ls-owned-issues))
                                 (lambda (_) (forge--ls-owned-pullreqs)))
                           nil t forge-global-topic-list-columns))
(put 'forge-list-owned-topics 'interactive-only nil)

;;;; Issue

(defun forge--issue-list-setup (filter fn &optional repo global columns)
  (forge-topic-list-setup 'issue filter fn repo global columns))

;;;###autoload (autoload 'forge-list-issues "forge-list" nil t)
(transient-define-suffix forge-list-issues ()
  "List issues of the current repository."
  :class 'forge--topic-list-command :type 'issue
  (interactive)
  (forge--issue-list-setup nil #'forge-ls-issues))

;;;###autoload (autoload 'forge-list-labeled-issues "forge-list" nil t)
(transient-define-suffix forge-list-labeled-issues (label)
  "List issues of the current repository that have LABEL."
  :class 'forge--topic-list-command :type 'issue :filter 'labeled
  (interactive (list (forge-read-topic-label)))
  (forge--issue-list-setup 'labeled (-cut forge--ls-labeled-issues <> label)))

;;;###autoload (autoload 'forge-list-assigned-issues "forge-list" nil t)
(transient-define-suffix forge-list-assigned-issues ()
  "List issues of the current repository that are assigned to you."
  :class 'forge--topic-list-command :type 'issue :filter 'assigned
  (interactive)
  (forge--issue-list-setup 'assigned #'forge--ls-assigned-issues))

;;;###autoload (autoload 'forge-list-authored-issues "forge-list" nil t)
(transient-define-suffix forge-list-authored-issues ()
  "List open issues from the current repository that are authored by you."
  :class 'forge--topic-list-command :type 'issue :filter 'authored
  (interactive)
  (forge--issue-list-setup 'authored #'forge--ls-authored-issues))

;;;###autoload (autoload 'forge-list-owned-issues "forge-list" nil t)
(transient-define-suffix forge-list-owned-issues ()
  "List open issues from all your Github repositories.
Options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now."
  :class 'forge--topic-list-command :type 'issue :filter 'owned :global t
  (interactive)
  (forge--issue-list-setup 'owned #'forge--ls-owned-issues
                           nil t forge-global-topic-list-columns))

;;;; Pullreq

(defun forge--pullreq-list-setup (filter fn &optional repo global columns)
  (forge-topic-list-setup 'pullreq filter fn repo global columns))

;;;###autoload (autoload 'forge-list-pullreqs "forge-list" nil t)
(transient-define-suffix forge-list-pullreqs ()
  "List pull-requests of the current repository."
  :class 'forge--topic-list-command :type 'pullreq
  (interactive)
  (forge--pullreq-list-setup nil #'forge-ls-pullreqs))

;;;###autoload (autoload 'forge-list-labeled-pullreqs "forge-list" nil t)
(transient-define-suffix forge-list-labeled-pullreqs (label)
  "List pull-requests of the current repository that have LABEL."
  :class 'forge--topic-list-command :type 'pullreq :filter 'labeled
  (interactive (list (forge-read-topic-label)))
  (forge--pullreq-list-setup 'labeled (-cut forge--ls-labeled-pullreqs <> label)))

;;;###autoload (autoload 'forge-list-assigned-pullreqs "forge-list" nil t)
(transient-define-suffix forge-list-assigned-pullreqs ()
  "List pull-requests of the current repository that are assigned to you."
  :class 'forge--topic-list-command :type 'pullreq :filter 'assigned
  (interactive)
  (forge--pullreq-list-setup 'assigned #'forge--ls-assigned-pullreqs))

;;;###autoload (autoload 'forge-list-requested-reviews "forge-list" nil t)
(transient-define-suffix forge-list-requested-reviews ()
  "List pull-requests of the current repository that are awaiting your review."
  :class 'forge--topic-list-command :type 'pullreq :filter 'review
  (interactive)
  (forge--pullreq-list-setup 'review #'forge--ls-requested-reviews))

;;;###autoload (autoload 'forge-list-authored-pullreqs "forge-list" nil t)
(transient-define-suffix forge-list-authored-pullreqs ()
  "List open pull-requests of the current repository that are authored by you."
  :class 'forge--topic-list-command :type 'pullreq :filter 'authored
  (interactive)
  (forge--pullreq-list-setup 'authored #'forge--ls-authored-pullreqs))

;;;###autoload (autoload 'forge-list-owned-pullreqs "forge-list" nil t)
(transient-define-suffix forge-list-owned-pullreqs ()
  "List open pull-requests from all your Github repositories.
Options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now."
  :class 'forge--topic-list-command :type 'pullreq :filter 'owned :global t
  (interactive)
  (forge--pullreq-list-setup 'owned #'forge--ls-owned-pullreqs
                             nil t forge-global-topic-list-columns))

;;;; Repository

;;;###autoload (autoload 'forge-list-repositories "forge-list" nil t)
(transient-define-suffix forge-list-repositories ()
  "List known repositories in a separate buffer.
Here \"known\" means that an entry exists in the local database."
  :class 'forge--topic-list-command :type 'repo :global t
  (interactive)
  (forge-repository-list-setup nil #'forge-repository-list-refresh))

;;;###autoload (autoload 'forge-list-owned-repositories "forge-list" nil t)
(transient-define-suffix forge-list-owned-repositories ()
  "List your own known repositories in a separate buffer.
Here \"known\" means that an entry exists in the local database
and options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now."
  :class 'forge--topic-list-command :type 'repo :filter 'owned :global t
  (interactive)
  (forge-repository-list-setup 'owned #'forge-repository-list-owned-refresh))

;;; _
(provide 'forge-list)
;;; forge-list.el ends here
