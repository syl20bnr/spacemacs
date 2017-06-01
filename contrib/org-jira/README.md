# org-jira Spacemacs layer

This Spacemacs layer allows using Jira from within org-mode.

## Installation

1. Clone the git repository somewhere and add it as a private layer

```bash
git clone git@github.com:jfim/org-jira.git
ln -s "`pwd`/org-jira" ~/.emacs.d/private/org-jira
```

2. Add the =org-jira= layer to your .spacemacs file
3. Add the Jira url to your .spacemacs file. For example, if your Jira is installed at https://example:443/secure/Dashboard.jspa

```lisp
(setq jiralib-url "https://example:443")
```

## Usage

When in org-mode:

* <kbd>SPC m j p g</kbd> 'org-jira-get-projects
* <kbd>SPC m j i b</kbd> 'org-jira-browse-issue
* <kbd>SPC m j i g</kbd> 'org-jira-get-issues
* <kbd>SPC m j i h</kbd> 'org-jira-get-issues-headonly
* <kbd>SPC m j i f</kbd> 'org-jira-get-issues-from-filter-headonly
* <kbd>SPC m j i F</kbd> 'org-jira-get-issues-from-filter
* <kbd>SPC m j i u</kbd> 'org-jira-update-issue
* <kbd>SPC m j i w</kbd> 'org-jira-progress-issue
* <kbd>SPC m j i r</kbd> 'org-jira-refresh-issue
* <kbd>SPC m j i c</kbd> 'org-jira-create-issue
* <kbd>SPC m j i k</kbd> 'org-jira-copy-current-issue-key
* <kbd>SPC m j s c</kbd> 'org-jira-create-subtask
* <kbd>SPC m j s g</kbd> 'org-jira-get-subtasks
* <kbd>SPC m j c u</kbd> 'org-jira-update-comment
* <kbd>SPC m j t j</kbd> 'org-jira-todo-to-jira
