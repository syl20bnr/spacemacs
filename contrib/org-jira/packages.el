;;; packages.el --- org-jira Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Jean-Francois Im <jeanfrancois.im@gmail.com>
;; URL: https://github.com/jfim/org-jira
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar org-jira-packages '(org org-jira))

(defvar org-jira-excluded-packages '() "List of packages to exclude.")

(defun org-jira/init-org-jira ()
  (use-package org-jira
    :config
    (progn
      ;;(spacemacs/declare-prefix-for-mode 'org-mode "mj" "jira")
      ;;(spacemacs/declare-prefix-for-mode 'org-mode "mjp" "projects")
      ;;(spacemacs/declare-prefix-for-mode 'org-mode "mji" "issues")
      ;;(spacemacs/declare-prefix-for-mode 'org-mode "mjs" "subtasks")
      ;;(spacemacs/declare-prefix-for-mode 'org-mode "mjc" "comments")
      ;;(spacemacs/declare-prefix-for-mode 'org-mode "mjt" "todos")
      (evil-leader/set-key-for-mode 'org-mode
        "mjpg" 'org-jira-get-projects
        "mjib" 'org-jira-browse-issue
        "mjig" 'org-jira-get-issues
        "mjih" 'org-jira-get-issues-headonly
        "mjif" 'org-jira-get-issues-from-filter-headonly
        "mjiF" 'org-jira-get-issues-from-filter
        "mjiu" 'org-jira-update-issue
        "mjiw" 'org-jira-progress-issue
        "mjir" 'org-jira-refresh-issue
        "mjic" 'org-jira-create-issue
        "mjik" 'org-jira-copy-current-issue-key
        "mjsc" 'org-jira-create-subtask
        "mjsg" 'org-jira-get-subtasks
        "mjcu" 'org-jira-update-comment
        "mjtj" 'org-jira-todo-to-jira)
      )
    ))
