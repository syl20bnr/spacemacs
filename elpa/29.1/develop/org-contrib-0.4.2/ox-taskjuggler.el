;;; ox-taskjuggler.el --- TaskJuggler Back-End for Org Export Engine
;;
;; Copyright (C) 2007-2021 Free Software Foundation, Inc.
;;
;; Emacs Lisp Archive Entry
;; Filename: ox-taskjuggler.el
;; Author: Christian Egli
;;      Nicolas Goaziou <n dot goaziou at gmail dot com>
;; Maintainer: Christian Egli
;; Keywords: org, taskjuggler, project planning
;; Description: Converts an Org mode buffer into a TaskJuggler project plan

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library implements a TaskJuggler exporter for Org mode.
;; TaskJuggler is a project planing tool that uses a text format to
;; define projects, tasks and resources, so it is a natural fit for
;; Org mode.  It can produce all sorts of reports for tasks or
;; resources in either HTML, CSV or PDF.  TaskJuggler is implemented
;; in Ruby and should therefore run on any platform.
;;
;; The exporter does not export all the nodes of a document or
;; strictly follow the order of the nodes in the document.
;;
;; Instead the TaskJuggler exporter looks for a tree that defines the
;; tasks and a optionally tree that defines the resources for this
;; project.  It then creates a TaskJuggler file based on these trees
;; and the attributes defined in all the nodes.
;;
;; * Installation
;;
;; Put this file into your load-path and the following line into your
;; ~/.emacs:
;;
;;   (add-to-list 'org-export-backends 'taskjuggler)
;;
;; or customize `org-export-backends' variable.
;;
;; The interactive functions are the following:
;;
;; M-x `org-taskjuggler-export'
;; M-x `org-taskjuggler-export-and-open'
;;
;; * Tasks
;;
;; Let's illustrate the usage with a small example.  Create your tasks
;; as you usually do with org-mode.  Assign efforts to each task using
;; properties (it's easiest to do this in the column view).  You
;; should end up with something similar to the example by Peter Jones
;; in:
;;
;;   https://www.devalot.com/assets/articles/2008/07/project-planning/project-planning.org.
;;
;; Now mark the top node of your tasks with a tag named
;; "taskjuggler_project" (or whatever you customized
;; `org-taskjuggler-project-tag' to).  You are now ready to export the
;; project plan with `org-taskjuggler-export-and-open' which will
;; export the project plan and open a Gantt chart in TaskJugglerUI.
;;
;; * Resources
;;
;; Next you can define resources and assign those to work on specific
;; tasks.  You can group your resources hierarchically.  Tag the top
;; node of the resources with "taskjuggler_resource" (or whatever you
;; customized `org-taskjuggler-resource-tag' to).  You can optionally
;; assign an identifier (named "resource_id") to the resources (using
;; the standard org properties commands) or you can let the exporter
;; generate identifiers automatically (the exporter picks the first
;; word of the headline as the identifier as long as it is unique, see
;; the documentation of `org-taskjuggler--build-unique-id').  Using that
;; identifier you can then allocate resources to tasks.  This is again
;; done with the "allocate" property on the tasks.  Do this in column
;; view or when on the task type
;;
;;  C-c C-x p allocate RET <resource_id> RET
;;
;; Once the allocations are done you can again export to TaskJuggler
;; and check in the Resource Allocation Graph which person is working
;; on what task at what time.
;;
;; * Export of properties
;;
;; The exporter also takes TODO state information into consideration,
;; i.e. if a task is marked as done it will have the corresponding
;; attribute in TaskJuggler ("complete 100").  Also it will export any
;; property on a task resource or resource node which is known to
;; TaskJuggler, such as limits, vacation, shift, booking, efficiency,
;; journalentry, rate for resources or account, start, note, duration,
;; end, journalentry, milestone, reference, responsible, scheduling,
;; etc for tasks.
;;
;; * Dependencies
;;
;; The exporter will handle dependencies that are defined in the tasks
;; either with the ORDERED attribute (see TODO dependencies in the Org
;; mode manual) or with the BLOCKER attribute (see org-depend.el) or
;; alternatively with a depends attribute.  Both the BLOCKER and the
;; depends attribute can be either "previous-sibling" or a reference
;; to an identifier (named "task_id") which is defined for another
;; task in the project.  BLOCKER and the depends attribute can define
;; multiple dependencies separated by either space or comma.  You can
;; also specify optional attributes on the dependency by simply
;; appending it.  The following examples should illustrate this:
;;
;; * Training material
;;   :PROPERTIES:
;;   :task_id:  training_material
;;   :ORDERED:  t
;;   :END:
;; ** Markup Guidelines
;;    :PROPERTIES:
;;    :Effort:   2d
;;    :END:
;; ** Workflow Guidelines
;;    :PROPERTIES:
;;    :Effort:   2d
;;    :END:
;; * Presentation
;;   :PROPERTIES:
;;   :Effort:   2d
;;   :BLOCKER:  training_material { gapduration 1d } some_other_task
;;   :END:
;;
;;;; * TODO
;;   - Look at org-keyword-properties, org-global-properties and
;;     org-global-properties-fixed
;;   - What about property inheritance and org-property-inherit-p?
;;   - Use TYPE_TODO as an way to assign resources
;;   - Add support for org-export-with-planning
;;
;;; Code:

(eval-when-compile (require 'cl))

(require 'ox)



;;; User Variables

(defgroup org-export-taskjuggler nil
  "Options specific for TaskJuggler export back-end."
  :tag "Org Export TaskJuggler"
  :group 'org-export)

(defcustom org-taskjuggler-extension ".tjp"
  "Extension of TaskJuggler files."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-taskjuggler-project-tag "taskjuggler_project"
  "Tag marking project's tasks.
This tag is used to find the tree containing all the tasks for
the project."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-taskjuggler-resource-tag "taskjuggler_resource"
  "Tag marking project's resources.
This tag is used to find the tree containing all the resources
for the project."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-taskjuggler-report-tag "taskjuggler_report"
  "Tag marking project's reports.
This tag is used to find the tree containing all the reports for
the project."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-taskjuggler-target-version 3.0
  "Which version of TaskJuggler the exporter is targeting.
By default a project plan is exported which conforms to version
3.x of TaskJuggler.  For a project plan that is compatible with
versions of TaskJuggler older than 3.0 set this to 2.4.

If you change this variable be sure to also change
`org-taskjuggler-default-reports' as the format of reports has
changed considerably between version 2.x and 3.x of TaskJuggler"
  :group 'org-export-taskjuggler
  :type 'number)

(defcustom org-taskjuggler-default-project-version "1.0"
  "Default version string for the project.
This value can also be set with the \":VERSION:\" property
associated to the headline defining the project."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-taskjuggler-default-project-duration 280
  "Default project duration.
The value will be used if no start and end date have been defined
in the root node of the task tree, i.e. the tree that has been
marked with `org-taskjuggler-project-tag'"
  :group 'org-export-taskjuggler
  :type 'integer)

(defcustom org-taskjuggler-default-reports
  '("textreport report \"Plan\" {
  formats html
  header '== %title =='

  center -8<-
    [#Plan Plan] | [#Resource_Allocation Resource Allocation]
    ----
    === Plan ===
    <[report id=\"plan\"]>
    ----
    === Resource Allocation ===
    <[report id=\"resourceGraph\"]>
  ->8-
}

# A traditional Gantt chart with a project overview.
taskreport plan \"\" {
  headline \"Project Plan\"
  columns bsi, name, start, end, effort, chart
  loadunit shortauto
  hideresource 1
}

# A graph showing resource allocation. It identifies whether each
# resource is under- or over-allocated for.
resourcereport resourceGraph \"\" {
  headline \"Resource Allocation Graph\"
  columns no, name, effort, weekly
  loadunit shortauto
  hidetask ~(isleaf() & isleaf_())
  sorttasks plan.start.up
}")
  "Default reports for the project.
These are sensible default reports to give a good out-of-the-box
result when exporting without defining any reports.  \"%title\"
anywhere in the reports will be replaced with the document title.
If you want to define your own reports you can change them here
or simply define the default reports so that they include an
external report definition as follows:

include reports.tji

These default are made to work with tj3.  If you are targeting
TaskJuggler 2.4 (see `org-taskjuggler-target-version') please
change these defaults to something like the following:

taskreport \"Gantt Chart\" {
  headline \"Project Gantt Chart\"
  columns hierarchindex, name, start, end, effort, duration, completed, chart
  timeformat \"%Y-%m-%d\"
  hideresource 1
  loadunit shortauto
}

resourcereport \"Resource Graph\" {
  headline \"Resource Allocation Graph\"
  columns no, name, utilization, freeload, chart
  loadunit shortauto
  sorttasks startup
  hidetask ~isleaf()
}"
  :group 'org-export-taskjuggler
  :type '(repeat (string :tag "Report")))

(defcustom org-taskjuggler-default-global-header ""
  "Default global header for the project.
This goes before project declaration, and might be useful for
early macros."
  :group 'org-export-taskjuggler
  :type '(string :tag "Preamble"))

(defcustom org-taskjuggler-default-global-properties
  "shift s40 \"Part time shift\" {
  workinghours wed, thu, fri off
}
"
  "Default global properties for the project.

Here you typically define global properties such as shifts,
accounts, rates, vacation, macros and flags.  Any property that
is allowed within the TaskJuggler file can be inserted.  You
could for example include another TaskJuggler file.

The global properties are inserted after the project declaration
but before any resource and task declarations."
  :group 'org-export-taskjuggler
  :type '(string :tag "Preamble"))

(defcustom org-taskjuggler-valid-task-attributes
  '(account start note duration endbuffer endcredit end
	    flags journalentry length limits maxend maxstart minend
	    minstart period reference responsible scheduling
	    startbuffer startcredit statusnote chargeset charge)
  "Valid attributes for Taskjuggler tasks.
If one of these appears as a property for a headline, it will be
exported with the corresponding task.

Note that multiline properties are not supported, so attributes
like note or journalentry have to be on a single line."
  :group 'org-export-taskjuggler)

(defcustom org-taskjuggler-valid-project-attributes
  '(timingresolution timezone alertlevels currency currencyformat
  dailyworkinghours extend includejournalentry now numberformat
  outputdir scenario shorttimeformat timeformat trackingscenario
  weekstartsmonday weekstartssunday workinghours
  yearlyworkingdays)
  "Valid attributes for Taskjuggler project.
If one of these appears as a property for a headline that is a
project definition, it will be exported with the corresponding
task. Attribute 'timingresolution' should be the first in the
list."
  :group 'org-export-taskjuggler)

(defcustom org-taskjuggler-valid-resource-attributes
  '(limits vacation shift booking efficiency journalentry rate
	   workinghours flags)
  "Valid attributes for Taskjuggler resources.
If one of these appears as a property for a headline, it will be
exported with the corresponding resource."
  :group 'org-export-taskjuggler)

(defcustom org-taskjuggler-valid-report-attributes
  '(headline columns definitions timeformat hideresource hidetask
	     loadunit sorttasks formats period)
  "Valid attributes for Taskjuggler reports.
If one of these appears as a property for a headline, it will be
exported with the corresponding report."
  :group 'org-export-taskjuggler)

(defcustom org-taskjuggler-process-command
  "tj3 --silent --no-color --output-dir %o %f"
  "Command to process a Taskjuggler file.
The command will be given to the shell as a command to process a
Taskjuggler file.  \"%f\" in the command will be replaced by the
full file name, \"%o\" by the reports directory (see
`org-taskjuggler-reports-directory').

If you are targeting Taskjuggler 2.4 (see
`org-taskjuggler-target-version') this setting is ignored."
  :group 'org-export-taskjuggler)

(defcustom org-taskjuggler-reports-directory "reports"
  "Default directory to generate the Taskjuggler reports in.
The command `org-taskjuggler-process-command' generates the
reports and associated files such as CSS inside this directory.

If the directory is not an absolute path it is relative to the
directory of the exported file.  The directory is created if it
doesn't exist.

If you are targeting Taskjuggler 2.4 (see
`org-taskjuggler-target-version') this setting is ignored."
  :group 'org-export-taskjuggler)

(defcustom org-taskjuggler-keep-project-as-task t
  "Non-nil keeps the project headline as an umbrella task for all tasks.
Setting this to nil will allow maintaining completely separated
task buckets, while still sharing the same resources pool."
  :group 'org-export-taskjuggler
  :type 'boolean)



;;; Hooks

(defvar org-taskjuggler-final-hook nil
  "Hook run after a TaskJuggler files has been saved.
This hook is run with the name of the file as argument.")



;;; Back-End Definition

(org-export-define-backend 'taskjuggler
  '((template . org-taskjuggler-project-plan))
  :menu-entry
  '(?J "Export to TaskJuggler"
       ((?j "As TJP file" (lambda (a s v b) (org-taskjuggler-export a s v)))
	(?p "As TJP file and process"
	    (lambda (a s v b)
	      (if a (org-taskjuggler-export a s v)
		(org-taskjuggler-export-and-process s v))))
	(?o "As TJP file, process and open"
	    (lambda (a s v b)
	      (if a (org-taskjuggler-export a s v)
		(org-taskjuggler-export-process-and-open s v))))))
  ;; This property will be used to store unique ids in communication
  ;; channel.  Ids will be retrieved with `org-taskjuggler-get-id'.
  :options-alist '((:taskjuggler-unique-ids nil nil nil)))



;;; Unique IDs

(defun org-taskjuggler-assign-task-ids (tasks info)
  "Assign a unique ID to each task in TASKS.
TASKS is a list of headlines.  INFO is a plist used as a
communication channel.  Return value is an alist between
headlines and their associated ID.  IDs are hierarchical, which
means they only need to be unique among the task siblings."
  (let* (alist
	 build-id			; For byte-compiler.
         (build-id
          (lambda (tasks local-ids)
            (org-element-map tasks 'headline
              (lambda (task)
                (let ((id (org-taskjuggler--build-unique-id task local-ids)))
                  (push id local-ids)
                  (push (cons task id) alist)
                  (funcall build-id (org-element-contents task) nil)))
              info nil 'headline))))
    (funcall build-id tasks nil)
    alist))

(defun org-taskjuggler-assign-resource-ids (resources info)
  "Assign a unique ID to each resource within RESOURCES.
RESOURCES is a list of headlines.  INFO is a plist used as a
communication channel.  Return value is an alist between
headlines and their associated ID."
  (let (ids)
    (org-element-map resources 'headline
      (lambda (resource)
        (let ((id (org-taskjuggler--build-unique-id resource ids)))
          (push id ids)
          (cons resource id)))
      info)))



;;; Accessors

(defun org-taskjuggler-get-project (info)
  "Return project in parse tree.
INFO is a plist used as a communication channel.  First headline
in buffer with `org-taskjuggler-project-tag' defines the project.
If no such task is defined, pick the first headline in buffer.
If there is no headline at all, return nil."
  (let ((tree (plist-get info :parse-tree)))
    (or (org-element-map tree 'headline
	  (lambda (hl)
	    (and (member org-taskjuggler-project-tag
			 (org-export-get-tags hl info))
		 hl))
	  info t)
	(org-element-map tree 'headline 'identity info t))))

(defun org-taskjuggler-get-id (item info)
  "Return id for task or resource ITEM.
ITEM is a headline.  INFO is a plist used as a communication
channel.  Return value is a string."
  (cdr (assq item (plist-get info :taskjuggler-unique-ids))))

(defun org-taskjuggler-get-name (item)
  "Return name for task or resource ITEM.
ITEM is a headline.  Return value is a string."
  ;; Quote double quotes in name.
  (replace-regexp-in-string
   "\"" "\\\"" (org-element-property :raw-value item) t t))

(defun org-taskjuggler-get-start (item)
  "Return start date for task or resource ITEM.
ITEM is a headline.  Return value is a string or nil if ITEM
doesn't have any start date defined."
  (let ((scheduled (org-element-property :scheduled item)))
    (or
     (and scheduled (org-timestamp-format scheduled "%Y-%02m-%02d"))
     (and (memq 'start org-taskjuggler-valid-task-attributes)
	  (org-element-property :START item)))))

(defun org-taskjuggler-get-end (item)
  "Return end date for task or resource ITEM.
ITEM is a headline.  Return value is a string or nil if ITEM
doesn't have any end date defined."
  (let ((deadline (org-element-property :deadline item)))
    (and deadline (org-timestamp-format deadline "%Y-%02m-%02d"))))



;;; Internal Functions

(defun org-taskjuggler--indent-string (s)
  "Indent string S by 2 spaces.
Return new string.  If S is the empty string, return it."
  (if (equal "" s) s (replace-regexp-in-string "^ *\\S-" "  \\&" s)))

(defun org-taskjuggler--build-attributes (item attributes)
  "Return attributes string for ITEM.
ITEM is a project, task, resource or report headline.  ATTRIBUTES
is a list of symbols representing valid attributes for ITEM."
  (mapconcat
   (lambda (attribute)
     (let ((value (org-element-property
                   (intern (upcase (format ":%s" attribute)))
                   item)))
       (and value (format "%s %s\n" attribute value))))
   (remq nil attributes) ""))

(defun org-taskjuggler--build-unique-id (item unique-ids)
  "Return a unique id for a given task or a resource.
ITEM is an `headline' type element representing the task or
resource.  Its id is derived from its name and made unique
against UNIQUE-IDS.  If the (downcased) first token of the
headline is not unique try to add more (downcased) tokens of the
headline or finally add more underscore characters (\"_\")."
  (let ((id (org-string-nw-p (org-element-property :TASK_ID item))))
    ;; If an id is specified, use it, as long as it's unique.
    (if (and id (not (member id unique-ids))) id
      (let* ((parts (split-string (org-element-property :raw-value item)))
	     (id (org-taskjuggler--clean-id (downcase (pop parts)))))
	;; Try to add more parts of the headline to make it unique.
	(while (and (car parts) (member id unique-ids))
	  (setq id (concat id "_"
			   (org-taskjuggler--clean-id (downcase (pop parts))))))
	;; If it's still not unique, add "_".
	(while (member id unique-ids)
	  (setq id (concat id "_")))
	id))))

(defun org-taskjuggler--clean-id (id)
  "Clean and return ID to make it acceptable for TaskJuggler.
ID is a string."
  ;; Replace non-ascii by "_".
  (replace-regexp-in-string
   "[^a-zA-Z0-9_]" "_"
   ;; Make sure id doesn't start with a number.
   (replace-regexp-in-string "^\\([0-9]\\)" "_\\1" id)))



;;; Dependencies

(defun org-taskjuggler-resolve-dependencies (task info)
  "Return a list of all tasks TASK depends on.
TASK is a headline.  INFO is a plist used as a communication
channel."
  (let ((deps-ids
         ;; Get all dependencies specified in BLOCKER and DEPENDS task
         ;; properties.  Clean options from them.
         (let ((deps (concat (org-element-property :BLOCKER task)
                             (org-element-property :DEPENDS task))))
           (and deps
                (split-string (replace-regexp-in-string "{.*?}" "" deps)
			      "[ ,]* +"))))
        depends)
    (when deps-ids
      ;; Find tasks with :task_id: property matching id in DEPS-IDS.
      ;; Add them to DEPENDS.
      (let* ((project (org-taskjuggler-get-project info))
             (tasks (if org-taskjuggler-keep-project-as-task project
                      (org-element-contents project))))
        (setq depends
              (org-element-map tasks 'headline
                (lambda (task)
                  (let ((task-id (or (org-element-property :TASK_ID task)
				     (org-element-property :ID task))))
                    (and task-id (member task-id deps-ids) task)))
                info)))
      ;; Check BLOCKER and DEPENDS properties.  If "previous-sibling"
      ;; belongs to DEPS-ID, add it to DEPENDS.
      (when (and (member-ignore-case "previous-sibling" deps-ids)
                 (not (org-export-first-sibling-p task info)))
        (let ((prev (org-export-get-previous-element task info)))
          (and (not (memq prev depends)) (push prev depends)))))
    ;; Check ORDERED status of parent.
    (let ((parent (org-export-get-parent task)))
      (when (and parent
                 (org-element-property :ORDERED parent)
                 (not (org-export-first-sibling-p task info)))
        (push (org-export-get-previous-element task info) depends)))
    ;; Return dependencies.
    depends))

(defun org-taskjuggler-format-dependencies (dependencies task info)
  "Format DEPENDENCIES to match TaskJuggler syntax.
DEPENDENCIES is list of dependencies for TASK, as returned by
`org-taskjuggler-resolve-depedencies'.  TASK is a headline.
INFO is a plist used as a communication channel.  Return value
doesn't include leading \"depends\"."
  (let* ((dep-str (concat (org-element-property :BLOCKER task)
			  " "
			  (org-element-property :DEPENDS task)))
	 (get-path
	  (lambda (dep)
	    ;; Return path to DEP relatively to TASK.
	    (let ((parent (org-export-get-parent task))
		  (exclamations 1)
		  (option
		   (let ((id (org-element-property :TASK_ID dep)))
		     (and id
			  (string-match (concat id " +\\({.*?}\\)") dep-str)
			  (match-string-no-properties 1 dep-str))))
		  path)
	      ;; Compute number of exclamation marks by looking for the
	      ;; common ancestor between TASK and DEP.
	      (while (not (org-element-map parent 'headline
			  (lambda (hl) (eq hl dep))))
		(cl-incf exclamations)
		(setq parent (org-export-get-parent parent)))
	      ;; Build path from DEP to PARENT.
	      (while (not (eq parent dep))
		(push (org-taskjuggler-get-id dep info) path)
		(setq dep (org-export-get-parent dep)))
	      ;; Return full path.  Add dependency options, if any.
	      (concat (make-string exclamations ?!)
		      (mapconcat 'identity path ".")
		      (and option (concat " " option)))))))
    ;; Return dependencies string, without the leading "depends".
    (mapconcat (lambda (dep) (funcall get-path dep)) dependencies ", ")))



;;; Translator Functions

(defun org-taskjuggler-project-plan (contents info)
  "Build TaskJuggler project plan.
CONTENTS is ignored.  INFO is a plist holding export options.
Return complete project plan as a string in TaskJuggler syntax."
  (let* ((tree (plist-get info :parse-tree))
         (project (or (org-taskjuggler-get-project info)
                      (error "No project specified"))))
    (concat
     ;; 1. Insert header.
     (org-element-normalize-string org-taskjuggler-default-global-header)
     ;; 2. Insert project.
     (org-taskjuggler--build-project project info)
     ;; 3. Insert global properties.
     (org-element-normalize-string org-taskjuggler-default-global-properties)
     ;; 4. Insert resources.  Provide a default one if none is
     ;;    specified.
     (let ((main-resources
            ;; Collect contents from various trees marked with
            ;; `org-taskjuggler-resource-tag'.  Only gather top level
            ;; resources.
            (apply 'append
                   (org-element-map tree 'headline
                     (lambda (hl)
                       (and (member org-taskjuggler-resource-tag
                                    (org-export-get-tags hl info))
                            (org-element-map (org-element-contents hl) 'headline
                              'identity info nil 'headline)))
                     info nil 'headline))))
       ;; Assign a unique ID to each resource.  Store it under
       ;; `:taskjuggler-unique-ids' property in INFO.
       (setq info
             (plist-put info :taskjuggler-unique-ids
                        (org-taskjuggler-assign-resource-ids
                         main-resources info)))
       (concat
        (if main-resources
            (mapconcat
             (lambda (resource) (org-taskjuggler--build-resource resource info))
             main-resources "")
          (format "resource %s \"%s\" {\n}\n" (user-login-name) user-full-name))
        ;; 5. Insert tasks.
        (let ((main-tasks
               ;; If `org-taskjuggler-keep-project-as-task' is
               ;; non-nil, there is only one task.  Otherwise, every
               ;; direct children of PROJECT is a top level task.
               (if org-taskjuggler-keep-project-as-task (list project)
                 (or (org-element-map (org-element-contents project) 'headline
                       'identity info nil 'headline)
                     (error "No task specified")))))
          ;; Assign a unique ID to each task.  Add it to
          ;; `:taskjuggler-unique-ids' property in INFO.
          (setq info
                (plist-put info :taskjuggler-unique-ids
                           (append
                            (org-taskjuggler-assign-task-ids main-tasks info)
                            (plist-get info :taskjuggler-unique-ids))))
          ;; If no resource is allocated among tasks, allocate one to
          ;; the first task.
          (unless (org-element-map main-tasks 'headline
                    (lambda (task) (org-element-property :ALLOCATE task))
                    info t)
            (org-element-put-property
             (car main-tasks) :ALLOCATE
             (or (org-taskjuggler-get-id (car main-resources) info)
                 (user-login-name))))
          (mapconcat
           (lambda (task) (org-taskjuggler--build-task task info))
           main-tasks ""))
        ;; 6. Insert reports.  If no report is defined, insert default
        ;;    reports.
        (let ((main-reports
               ;; Collect contents from various trees marked with
               ;; `org-taskjuggler-report-tag'.  Only gather top level
               ;; reports.
               (apply 'append
                      (org-element-map tree 'headline
                        (lambda (hl)
                          (and (member org-taskjuggler-report-tag
                                       (org-export-get-tags hl info))
                               (org-element-map (org-element-contents hl)
                                   'headline 'identity info nil 'headline)))
                        info nil 'headline))))
          (if main-reports
              (mapconcat
               (lambda (report) (org-taskjuggler--build-report report info))
               main-reports "")
	    ;; insert title in default reports
	    (let* ((title (org-export-data (plist-get info :title) info))
		   (report-title (if (string= title "")
				     (org-taskjuggler-get-name project)
				   title)))
	      (mapconcat
	       'org-element-normalize-string
	       (mapcar
		(lambda (report)
		  (replace-regexp-in-string "%title" report-title  report t t))
		org-taskjuggler-default-reports) "")))))))))

(defun org-taskjuggler--build-project (project info)
  "Return a project declaration.
PROJECT is a headline.  INFO is a plist used as a communication
channel.  If no start date is specified, start today.  If no end
date is specified, end `org-taskjuggler-default-project-duration'
days from now."
  (concat
   ;; Opening project.
   (format "project %s \"%s\" \"%s\" %s %s {\n"
	   (org-taskjuggler-get-id project info)
	   (org-taskjuggler-get-name project)
	   ;; Version is obtained through :TASKJUGGLER_VERSION:
	   ;; property or `org-taskjuggler-default-project-version'.
	   (or (org-element-property :VERSION project)
	       org-taskjuggler-default-project-version)
	   (or (org-taskjuggler-get-start project)
	       (format-time-string "%Y-%m-%d"))
	   (let ((end (org-taskjuggler-get-end project)))
	     (or (and end (format "- %s" end))
		 (format "+%sd"
			 org-taskjuggler-default-project-duration))))
   ;; Add attributes.
   (org-taskjuggler--indent-string
    (org-taskjuggler--build-attributes
     project org-taskjuggler-valid-project-attributes))
   ;; Closing project.
   "}\n"))

(defun org-taskjuggler--build-resource (resource info)
  "Return a resource declaration.

RESOURCE is a headline.  INFO is a plist used as a communication
channel.

All valid attributes from RESOURCE are inserted.  If RESOURCE
defines a property \"resource_id\" it will be used as the id for
this resource.  Otherwise it will use the ID property.  If
neither is defined a unique id will be associated to it."
  (concat
   ;; Opening resource.
   (format "resource %s \"%s\" {\n"
           (org-taskjuggler--clean-id
            (or (org-element-property :RESOURCE_ID resource)
                (org-element-property :ID resource)
                (org-taskjuggler-get-id resource info)))
           (org-taskjuggler-get-name resource))
   ;; Add attributes.
   (org-taskjuggler--indent-string
    (org-taskjuggler--build-attributes
     resource org-taskjuggler-valid-resource-attributes))
   ;; Add inner resources.
   (org-taskjuggler--indent-string
    (mapconcat
     'identity
     (org-element-map (org-element-contents resource) 'headline
       (lambda (hl) (org-taskjuggler--build-resource hl info))
       info nil 'headline)
     ""))
   ;; Closing resource.
   "}\n"))

(defun org-taskjuggler--build-report (report info)
  "Return a report declaration.
REPORT is a headline.  INFO is a plist used as a communication
channel."
  (concat
   ;; Opening report.
   (format "%s \"%s\" {\n"
           (or (org-element-property :REPORT_KIND report) "taskreport")
           (org-taskjuggler-get-name report))
   ;; Add attributes.
   (org-taskjuggler--indent-string
    (org-taskjuggler--build-attributes
     report org-taskjuggler-valid-report-attributes))
   ;; Add inner reports.
   (org-taskjuggler--indent-string
    (mapconcat
     'identity
     (org-element-map (org-element-contents report) 'headline
       (lambda (hl) (org-taskjuggler--build-report hl info))
       info nil 'headline)
     ""))
   ;; Closing report.
   "}\n"))

(defun org-taskjuggler--build-task (task info)
  "Return a task declaration.

TASK is a headline.  INFO is a plist used as a communication
channel.

All valid attributes from TASK are inserted.  If TASK defines
a property \"task_id\" it will be used as the id for this task.
Otherwise it will use the ID property.  If neither is defined
a unique id will be associated to it."
  (let* ((allocate (org-element-property :ALLOCATE task))
         (complete
          (if (eq (org-element-property :todo-type task) 'done) "100"
            (org-element-property :COMPLETE task)))
         (depends (org-taskjuggler-resolve-dependencies task info))
         (effort (let ((property
			(intern (concat ":" (upcase org-effort-property)))))
		   (org-element-property property task)))
         (milestone
          (or (org-element-property :MILESTONE task)
              (not (or (org-element-map (org-element-contents task) 'headline
			 'identity info t)  ; Has task any child?
		       effort
		       (org-element-property :LENGTH task)
		       (org-element-property :DURATION task)
		       (and (org-taskjuggler-get-start task)
			    (org-taskjuggler-get-end task))
		       (org-element-property :PERIOD task)))))
         (priority
          (let ((pri (org-element-property :priority task)))
            (and pri
                 (max 1 (/ (* 1000 (- org-priority-lowest pri))
                           (- org-priority-lowest org-priority-highest)))))))
    (concat
     ;; Opening task.
     (format "task %s \"%s\" {\n"
             (org-taskjuggler-get-id task info)
             (org-taskjuggler-get-name task))
     ;; Add default attributes.
     (and depends
          (format "  depends %s\n"
                  (org-taskjuggler-format-dependencies depends task info)))
     (and allocate
          (format "  purge %s\n  allocate %s\n"
                  ;; Compatibility for previous TaskJuggler versions.
                  (if (>= org-taskjuggler-target-version 3.0) "allocate"
                    "allocations")
                  allocate))
     (and complete (format "  complete %s\n" complete))
     (and effort (format "  effort %s\n" effort))
     (and priority (format "  priority %s\n" priority))
     (and milestone "  milestone\n")
     ;; Add other valid attributes.
     (org-taskjuggler--indent-string
      (org-taskjuggler--build-attributes
       task org-taskjuggler-valid-task-attributes))
     ;; Add inner tasks.
     (org-taskjuggler--indent-string
      (mapconcat 'identity
                 (org-element-map (org-element-contents task) 'headline
                   (lambda (hl) (org-taskjuggler--build-task hl info))
                   info nil 'headline)
                 ""))
     ;; Closing task.
     "}\n")))



;;; Interactive Functions

;;;###autoload
(defun org-taskjuggler-export (&optional async subtreep visible-only)
  "Export current buffer to a TaskJuggler file.

The exporter looks for a tree with tag that matches
`org-taskjuggler-project-tag' and takes this as the tasks for
this project.  The first node of this tree defines the project
properties such as project name and project period.

If there is a tree with tag that matches
`org-taskjuggler-resource-tag' this tree is taken as resources
for the project.  If no resources are specified, a default
resource is created and allocated to the project.

Also the TaskJuggler project will be created with default reports
as defined in `org-taskjuggler-default-reports'.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile
         (org-export-output-file-name org-taskjuggler-extension subtreep)))
    (org-export-to-file 'taskjuggler outfile
      async subtreep visible-only nil nil
      (lambda (file)
	(run-hook-with-args 'org-taskjuggler-final-hook file) nil))))

;;;###autoload
(defun org-taskjuggler-export-and-process (&optional subtreep visible-only)
  "Export current buffer to a TaskJuggler file and process it.

The exporter looks for a tree with tag that matches
`org-taskjuggler-project-tag' and takes this as the tasks for
this project.  The first node of this tree defines the project
properties such as project name and project period.

If there is a tree with tag that matches
`org-taskjuggler-resource-tag' this tree is taken as resources
for the project.  If no resources are specified, a default
resource is created and allocated to the project.

Also the TaskJuggler project will be created with default reports
as defined in `org-taskjuggler-default-reports'.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return a list of reports."
  (interactive)
  (let ((file (org-taskjuggler-export nil subtreep visible-only)))
    (org-taskjuggler-compile file)))

;;;###autoload
(defun org-taskjuggler-export-process-and-open (&optional subtreep visible-only)
  "Export current buffer to a TaskJuggler file, process and open it.

Export and process the file using
`org-taskjuggler-export-and-process' and open the generated
reports with a browser.

If you are targeting TaskJuggler 2.4 (see
`org-taskjuggler-target-version') the processing and display of
the reports is done using the TaskJuggler GUI."
  (interactive)
  (if (< org-taskjuggler-target-version 3.0)
      (let* ((process-name "TaskJugglerUI")
	     (command
	      (concat process-name " "
		      (org-taskjuggler-export nil subtreep visible-only))))
	(start-process-shell-command process-name nil command))
    (dolist (report (org-taskjuggler-export-and-process subtreep visible-only))
      (org-open-file report))))

(defun org-taskjuggler-compile (file)
  "Compile a TaskJuggler file.

FILE is the name of the file being compiled.  Processing is done
through the command given in `org-taskjuggler-process-command'.

Return a list of reports."
  (let* ((full-name (file-truename file))
	 (out-dir
	  (expand-file-name
	   org-taskjuggler-reports-directory (file-name-directory file)))
	 errors)
    (message (format "Processing TaskJuggler file %s..." file))
    (save-window-excursion
      (let ((outbuf (get-buffer-create "*Org Taskjuggler Output*")))
	(unless (file-directory-p out-dir)
	  (make-directory out-dir t))
	(with-current-buffer outbuf (erase-buffer))
	(shell-command
	 (replace-regexp-in-string
	  "%f" (shell-quote-argument full-name)
	  (replace-regexp-in-string
	   "%o" (shell-quote-argument out-dir)
	   org-taskjuggler-process-command t t) t t) outbuf)
	;; Collect standard errors from output buffer.
	(setq errors (org-taskjuggler--collect-errors outbuf)))
      (if (not errors)
	  (message "Process completed.")
	(error (format "TaskJuggler failed with errors: %s" errors))))
    (file-expand-wildcards (format "%s/*.html" out-dir))))

(defun org-taskjuggler--collect-errors (buffer)
  "Collect some kind of errors from \"tj3\" command output.

BUFFER is the buffer containing output.

Return collected error types as a string, or nil if there was
none."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t)
	    (errors ""))
	(while (re-search-forward "^.+:[0-9]+: \\(.*\\)$" nil t)
	  (setq errors (concat errors " " (match-string 1))))
	(and (org-string-nw-p errors) (org-trim errors))))))


(provide 'ox-taskjuggler)

;; Local variables:
;; sentence-end-double-space: t
;; End:

;;; ox-taskjuggler.el ends here
