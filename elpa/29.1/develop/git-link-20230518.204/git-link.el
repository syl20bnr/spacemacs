;;; git-link.el --- Get the GitHub/Bitbucket/GitLab URL for a buffer location -*- lexical-binding: t -*-

;; Copyright (C) 2013-2022 Skye Shaw and others
;; Author: Skye Shaw <skye.shaw@gmail.com>
;; Version: 0.9.0
;; Keywords: git, vc, github, bitbucket, gitlab, sourcehut, aws, azure, convenience
;; URL: http://github.com/sshaw/git-link
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
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

;; Create URLs for files and commits in GitHub/Bitbucket/GitLab/...
;; repositories.  `git-link' returns the URL for the current buffer's file
;; location at the current line number or active region.  `git-link-commit'
;; returns the URL for a commit.  URLs are added to the kill ring.
;;
;; With a prefix argument prompt for the remote's name.  Defaults to "origin".

;;; Change Log:

;; 2023-02-15 - v0.9.0
;; * Add support for GoogleSource (thanks Peter Becich)
;; * Add plain=1 to force showing a non rendered GitHub links (thanks Erick Navarro)
;; * Add prefix arg to override git-link-use-commit when calling git-link (thanks Nacho Barrientos)
;; * Fix git-link-homepage for Bitbucket (Issue #97 thanks Sibi Prabakaran)
;; * Fix the Codeberg links to line ranges (Issue #109 thanks Wojciech Siewierski)
;;
;; 2022-02-17 - v0.8.6
;; * Fix URL escaping of pathnames on Emacs < 27 (Issue #93 thanks David Ongaro)
;;
;; 2022-02-06 - v0.8.5
;; * URL escape pathnames (thanks David Ongaro)
;; * Mark misspelled Savannah functions as obsolete (thanks Hendursaga)
;; * Add support for AWS CodeCommit (thanks Ram Krishnan)
;; * Add support for cgit (thanks Hendursaga)
;;
;; 2020-01-20 - v0.8.4
;; * Add support for Codeberg (thanks Jeremy Friesen)
;; * Add git-link-homepage-remote-alist (thanks Wim Van Deun)
;;
;; 2020-01-20 - v0.8.3
;; * Add support for Savannah
;;
;; 2020-12-14 - v0.8.2
;; * Fix sourcehut URL, don't link to raw version (Issue #77)
;; * Fix sourcehut multi-line URLs
;;
;; 2020-11-23 - v0.8.1
;; * Fix URL casing (Issue #57)
;; * Fix byte-compile warnings (Issue #75 thanks Brian Leung)
;;
;; 2020-07-21 - v0.8.0
;; * Add `-' prefix argument to git-link to generate links without line numbers
;; * Add git-link-use-single-line-number
;; * Fix sourcehut's git-link handler
;;
;; 2020-03-31 - v0.7.6
;; * Adapt to changes in Azure interface (Issue #65, thanks Roey Darwish Dror)
;;
;; 2019-08-28 - v0.7.5
;; * Add support for Azure DevOps (Issue #62, thanks Roey Darwish Dror)
;;
;; 2019-08-16 - v0.7.4
;; * Add support for Magit-Blob buffers (Issue #61, thanks Miciah Dashiel Butler Masters)
;;
;; 2019-03-09 - v0.7.3
;; * Add support for sourcehut
;;
;; 2018-10-30 - v0.7.2
;; * Fix suffix stripping on remote path only if it ends in .git (Issue #58, thanks Marko Crnic)
;;
;; 2018-07-08 - v0.7.1
;; * Add support for vc-revision-other-window files (Issue #54)
;;
;; 2018-06-07 - v0.7.0
;; * Add support for Tramp (Issue #49, thanks Jürgen Hötzel)
;; * Fix various compiler warnings
;; * Fix differences between url-path-and-query across Emacs versions
;; * Require Emacs 24.3
;;
;; 2018-04-23 - v0.6.0
;; * Fix parsing of remotes with auth info (Issue #51)
;; * Removed remote regex in favor of url-parse
;;
;; 2017-06-03 - v0.5.1
;; * Add support for more magit modes
;;
;; 2017-06-01 - v0.5.0
;; * Add support for linking in dired and magit modes
;; * Add support for defcustom
;; * Change git-link-remote-regex to support more remote URL formats (Thanks Kaushal Modi)
;; * Change git-link-remote-alist to use regex matching (Thanks Kaushal Modi)
;; * Fix point on commit hash regex and support uppercase SHAs (Thanks Kaushal Modi!)
;; * Fix git-link-commit message so that SHA text is displayed without properties
;; * Enabled lexical-binding (Thanks Kaushal Modi!!)
;;
;; -- Note that v0.5.0 was released as "v0.5.0 (unreleased)"
;;
;; 2016-10-19 - v0.4.5
;; * Fix for branches containing reserved URLs characters (Issue #36)
;;
;; 2016-09-11 - v0.4.4
;; * Added support for git-link-homepage
;;
;; 2016-08-13 - v0.4.3
;; * Added support for git-timemachine (Issue #22, thanks Diego Berrocal)
;;
;; 2016-08-09 - v0.4.2
;; * Fix for URLs with ports (Issue #32)
;;
;; 2016-04-01 - v0.4.1
;; * Better handling for branches that have no explicit remote
;; * Better error messages
;;
;; 2016-02-16 - v0.4.0
;; * Try branch's tracking remote when other branch settings are not specified
;; * git-link-default-remote now defaults to nil
;;
;; 2015-09-21 - v0.3.0
;; * Support for setting branch and remote names via `git config`
;; * Added git-link-default-branch
;; * Removed some functions, use emacs "private" convention for others
;;
;; 2015-09-12 - v0.2.2
;; * Support for BitBucket's multiline format
;;
;; 2015-07-25 - v0.2.1
;; * Fix for BitBucket's new URL format (Thanks Ev Dolzhenko)
;; * Fix for GitLab's multiline format (Thanks Enrico Carlesso)
;;
;; 2015-06-05 - v0.2.0
;; * Deactivate mark after killing the link (Thanks Kaushal Modi)
;; * Support for GitLab (Thanks Swaroop C H)
;; * Use completing-read when prompting for remotes (Thanks Andrew Gwozdziewycz)
;; * Display URL in minibuffer when adding to kill ring (Thanks Andrew Gwozdziewycz)
;; * Added git-link-use-commit variable (Thanks Kaushal Modi)
;; * Fix for displaying link in minibuffer when interprogram-cut-function is set (Thanks Ric Lister)
;; * Fix to ignore point at beginning of line in regions (Thanks Kaushal Modi)
;; * Fix for narrow-to-region (Bug #10, thanks Andrew Gwozdziewycz)
;; * Fix to use remote hostname when constructing link URLs (Thanks David Hull)
;;
;; 2015-02-05 - v0.1.0
;; * Added git-link-commit (Thanks Ryan Barrett)
;; * Added git-link-open-in-browser variable (Thanks Ryan Barrett)
;; * Use call-process instead of shell-command-to-string
;; * Use --short option when calling symbolic-ref (Thanks Steven Huwig)
;;
;; 2014-02-27 - v0.0.2
;; * Fix for buffers visiting files through symlinks (Issue #1, thanks Evgeniy Dolzhenko)

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'thingatpt)
(require 'url-util)
(require 'url-parse)

(defgroup git-link nil
  "Get the GitHub/Bitbucket/GitLab URL for a buffer location"
  :prefix "git-link-"
  :link '(url-link :tag "Report a Bug" "https://github.com/sshaw/git-link/issues")
  :link '(url-link :tag "Homepage" "https://github.com/sshaw/git-link")
  :group 'convenience)

(eval-when-compile
  (defvar git-timemachine-revision))    ;; silence reference to free variable warning

(defcustom git-link-default-remote nil
  "Name of the remote to link to."
  :type 'string
  :group 'git-link)

(defcustom git-link-default-branch nil
  "Name of the branch to link to."
  :type 'string
  :group 'git-link)

(defcustom git-link-open-in-browser nil
  "If t also open the link via `browse-url'.  To use an alternate
function set to that function's symbol."
  :type '(choice boolean function)
  :group 'git-link)

(defcustom git-link-use-commit nil
  "If non-nil use the latest commit's hash in the link instead of the branch name."
  :type 'boolean
  :group 'git-link)

(defcustom git-link-use-single-line-number t
  "If t a link to a single line will always contain the line number.
If nil line numbers will only be added when a selection contains
more than 1 line.

Note that `git-link' can exclude line numbers on a per invocation basis.
See its docs."
  :type 'boolean
  :group 'git-link)

(defcustom git-link-remote-alist
  '(("git.sr.ht" git-link-sourcehut)
    ("codeberg.org" git-link-codeberg)
    ("github" git-link-github)
    ("bitbucket" git-link-bitbucket)
    ("gitorious" git-link-gitorious)
    ("gitlab" git-link-gitlab)
    ("git\\.\\(sv\\|savannah\\)\\.gnu\\.org" git-link-savannah)
    ("googlesource.com" git-link-googlesource)
    ("visualstudio\\|azure" git-link-azure)
    ("sourcegraph" git-link-sourcegraph)
    ("\\(amazonaws\\|amazon\\)\\.com" git-link-codecommit))
  "Alist of host names and functions creating file links for those.
Each element looks like (REGEXP FUNCTION) where REGEXP is used to
match the remote's host name and FUNCTION is used to generate a link
to the file on remote host.

As an example, \"gitlab\" will match with both \"gitlab.com\" and
\"gitlab.example.com\"."
  :type '(alist :key-type string :value-type (group function))
  :group 'git-link)

(defcustom git-link-commit-remote-alist
  '(("git.sr.ht" git-link-commit-github)
    ("codeberg.org" git-link-commit-codeberg)
    ("github" git-link-commit-github)
    ("bitbucket" git-link-commit-bitbucket)
    ("gitorious" git-link-commit-gitorious)
    ("gitlab" git-link-commit-gitlab)
    ("git\\.\\(sv\\|savannah\\)\\.gnu\\.org" git-link-commit-savannah)
    ("googlesource.com" git-link-commit-googlesource)
    ("visualstudio\\|azure" git-link-commit-azure)
    ("sourcegraph" git-link-commit-sourcegraph)
    ("\\(amazonaws\\|amazon\\)\\.com" git-link-commit-codecommit))
  "Alist of host names and functions creating commit links for those.
Each element looks like (REGEXP FUNCTION) where REGEXP is used to
match the remote's host name and FUNCTION is used to generate a link
to the commit on remote host.

As an example, \"gitlab\" will match with both \"gitlab.com\" and
\"gitlab.example.com\"."
  :type '(alist :key-type string :value-type (group function))
  :group 'git-link)

(defcustom git-link-homepage-remote-alist
  '(("git.sr.ht" git-link-homepage-github)
    ("github" git-link-homepage-github)
    ("bitbucket" git-link-homepage-github)
    ("gitorious" git-link-homepage-github)
    ("gitlab" git-link-homepage-github)
    ("git\\.\\(sv\\|savannah\\)\\.gnu\\.org" git-link-homepage-savannah)
    ("googlesource.com" git-link-homepage-github)
    ("visualstudio\\|azure" git-link-homepage-github)
    ("sourcegraph" git-link-homepage-github)
    ("\\(amazonaws\\|amazon\\)\\.com" git-link-homepage-codecommit))
  "Alist of host names and functions creating homepage links for those.
Each element looks like (REGEXP FUNCTION) where REGEXP is used to
match the remote's host name and FUNCTION is used to generate a link
to the commit on remote host.

As an example, \"gitlab\" will match with both \"gitlab.com\" and
\"gitlab.example.com\"."
  :type '(alist :key-type string :value-type (group function))
  :group 'git-link)

(defcustom git-link-extensions-rendered-plain '("org" "md" "rst" "adoc" "markdown" "asciidoc")
  "List of extensions that should be rendered in plain mode, systems like
Github, Gitlab, etc show a rendered version by default, for these extensions
we can prevent that behaviour."
  :type 'list
  :group 'git-link)

(defun git-link--exec(&rest args)
  (ignore-errors
    (with-temp-buffer
      (when (zerop (apply #'process-file "git" nil (current-buffer) nil args))
        (goto-char (point-min))
        (cl-loop until (eobp)
                 collect (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))
                 do (forward-line 1))))))

(defun git-link--get-config (name)
  (car (git-link--exec "config" "--get" name)))

(defun git-link--remotes ()
  (git-link--exec "remote"))

(defun git-link--last-commit ()
  (car (git-link--exec "--no-pager" "log" "-n1" "--pretty=format:%H")))

(defvar magit-buffer-revision)

(defun git-link--commit ()
  (cond
   ((git-link--using-git-timemachine)
    (car git-timemachine-revision))
   ((git-link--using-magit-blob-mode)
    magit-buffer-revision)
   (t (git-link--last-commit))))

(defun git-link--current-branch ()
  (car (git-link--exec "symbolic-ref" "--short" "HEAD")))

(defun git-link--repo-root ()
  (let ((dir (car (git-link--exec "rev-parse" "--show-toplevel"))))
    (if (file-remote-p default-directory)
	(concat (file-remote-p default-directory) dir)
      dir)))

(defun git-link--remote-url (name)
  (car (git-link--exec "remote" "get-url" name)))

(defun git-link--branch-remote (branch)
  (git-link--get-config (format "branch.%s.remote" branch)))

(declare-function magit-rev-branch "ext:magit-git")

(defun git-link--branch ()
  (or (git-link--get-config "git-link.branch")
      git-link-default-branch
      (when (git-link--using-magit-blob-mode)
        (magit-rev-branch magit-buffer-revision))
      (git-link--current-branch)))

(defun git-link--remote ()
  (let* ((branch (git-link--current-branch))
	 (remote (or (git-link--get-config "git-link.remote")
		     git-link-default-remote
		     (git-link--branch-remote branch))))

    ;; Git defaults to "." if the branch has no remote.
    ;; If the branch has no remote we try master's, which may be set.
    (if (or (null remote)
	    (and (string= remote ".")
		 (not (string= branch "master"))))
	(setq remote (git-link--branch-remote "master")))

    (if (or (null remote) (string= remote "."))
	"origin"
      remote)))

(defun git-link--handler (alist str)
  "For an ALIST whose `car' (a regexp) matches STR, return cadr.

The ALIST consists of (REGEXP FN) list elements.
Valid ALISTs are `git-link-remote-alist',`git-link-commit-remote-alist'.

For the first ALIST element whose REGEXP matches with STR, FN is
returned.

Return nil,
- if STR does not match with REGEXP in any of the elements of ALIST, or
- if STR is not a string"
  (when (stringp str)
    (cadr (cl-find-if (lambda (lst)
                        (string-match-p (car lst) str))
                      alist))))

(defun git-link--parse-vc-revision (filename)
"If FILENAME appears to be from `vc-revision-other-window'
return (FILENAME . REVISION) otherwise nil."
  (when (and (string-match "\\(.+\\)\\.~\\([^~]+\\)~$" filename)
             (file-exists-p (match-string 1 filename)))
    (cons (match-string 1 filename)
          (match-string 2 filename))))

(defvar magit-buffer-file-name)

(defun git-link--relative-filename ()
  (let* ((filename (buffer-file-name (buffer-base-buffer)))
	 (dir      (git-link--repo-root)))

    (when (null filename)
      (cond
       ((eq major-mode 'dired-mode)
        (setq filename (dired-file-name-at-point)))
       ((git-link--using-magit-blob-mode)
        (setq filename magit-buffer-file-name))
       ((and (string-match-p "^magit-" (symbol-name major-mode))
             (fboundp 'magit-file-at-point))
        (setq filename (magit-file-at-point)))))

    (if (and dir filename
             ;; Make sure filename is not above dir, e.g. "/foo/repo-root/.."
             (< (length dir) (length (file-truename filename))))
	(substring (file-truename filename)
		   (1+ (length dir))))))

(defun git-link--parse-remote (url)
  "Parse URL and return a list as (HOST DIR).  DIR has no leading slash or `git' extension."
  (let (host path parsed)
    (unless (string-match "^[a-zA-Z0-9]+://" url)
      (setq url (concat "ssh://" url)))

    (setq parsed (url-generic-parse-url url)
          ;; Normalize path.
          ;; If none, will be nil on Emacs < 25. Later versions return "".
          path (or (car (url-path-and-query parsed)) "")
          host (url-host parsed))

    (when host
      (when (and (not (string= "/" path))
                 (not (string= ""  path)))
        (setq path (substring
                    (if (string-match "\\.git\\'" path)
                        (file-name-sans-extension path)
                      path)
                    1)))

      ;; Fix-up scp style URLs.
      ;; git@foo:UsEr/repo gives a host of foo:user
      ;; We also need to preserve case so we take UsEr from the original url
      (when (string-match ":" host)
        (let ((parts (split-string host ":" t))
              (case-fold-search t))
          (string-match (concat (car parts) ":\\(" (cadr parts) "\\)/") url)
          (setq host (car parts)
                path (concat (match-string 1 url) "/" path))))

      ;; Fix-up Azure SSH URLs
      (when (string= "ssh.dev.azure.com" host)
        (setq host "dev.azure.com")
        (setq path (replace-regexp-in-string
                    "v3/\\([^/]+\\)/\\([^/]+\\)/\\([^/]+\\)"
                    "\\1/\\2/_git/\\3"
                    path)))
      (when (string= "vs-ssh.visualstudio.com" host)
        (setq host (concat (url-user parsed) ".visualstudio.com"))
        (setq path (replace-regexp-in-string
                    (concat "^v3/" (url-user parsed) "/\\([^/]+\\)/")
                    "\\1/_git/"
                    path)))

      ;; For Savannah
      (when (string= "git.savannah.gnu.org" host)
        (cond
         ((string-match "\\`git/" path)
          (setq path (substring path 4)))
         ((string-match "\\`srv/git/" path)
          (setq path (substring path 8)))))

      ;; For AWS CodeCommit
      (when (string-match "git-codecommit\\.\\(.*\\)\\.amazonaws.com" host)
        (let* ((matchp (string-match "\\([^\\.]*\\)\\.\\([^\\.]*\\)" host))
               (region (when matchp
                         (match-string 2 host)))
               (domainname ".console.aws.amazon.com"))
          (when region
            (setq host (concat region domainname))))
        (when (string-match "v1/repos/" path)
          (setq path (concat "codesuite/codecommit/repositories/"
                             (substring path 9)))))

      (list host path))))

(defun git-link--using-git-timemachine ()
  (and (boundp 'git-timemachine-revision)
       git-timemachine-revision))

(defun git-link--using-magit-blob-mode ()
  (bound-and-true-p magit-blob-mode))

(defun git-link--read-remote ()
  (let ((remotes (git-link--remotes))
	(current (git-link--remote)))
    (completing-read "Remote: "
		     remotes
		     nil
		     t
		     ""
		     nil
		     (if (member current remotes)
			 current
		       (car remotes)))))

(defun git-link--get-region ()
  (save-restriction
    (widen)
    (save-excursion
      (let* ((use-region (use-region-p))
             (start (when use-region (region-beginning)))
             (end   (when use-region (region-end)))
             (line-start (line-number-at-pos start))
             line-end)
        (when use-region
          ;; Avoid adding an extra blank line to the selection.
          ;; This happens when point or mark is at the start of the next line.
          ;;
          ;; When selection is from bottom to top, exchange point and mark
          ;; so that the `point' and `(region-end)' are the same.
          (when (< (point) (mark))
            (exchange-point-and-mark))
          (when (= end (line-beginning-position))
            ;; Go up and avoid the blank line
            (setq end (1- end)))
          (setq line-end (line-number-at-pos end))
          (when (<= line-end line-start)
            (setq line-end nil)))
        (list line-start line-end)))))

(defun git-link--new (link)
  (kill-new link)
  ;; prevent URL escapes from being interpreted as format strings
  (message (replace-regexp-in-string "%" "%%" link t t))
  (setq deactivate-mark t)
  (when git-link-open-in-browser
    (if (fboundp git-link-open-in-browser)
        (funcall git-link-open-in-browser link)
      (browse-url link))))

(defun git-link-codeberg (hostname dirname filename branch commit start end)
    (format "https://%s/%s/src/%s/%s"
	    hostname
	    dirname
	    (or branch commit)
            (concat filename
                    (when start
                      (concat "#"
                              (if end
                                  (format "L%s-L%s" start end)
                                (format "L%s" start)))))))

(defun git-link-gitlab (hostname dirname filename branch commit start end)
  (format "https://%s/%s/-/blob/%s/%s"
	  hostname
	  dirname
	  (or branch commit)
          (concat filename
                  (when start
                    (concat "#"
                            (if end
                                (format "L%s-%s" start end)
                              (format "L%s" start)))))))

(defun git-link-github (hostname dirname filename branch commit start end)
  (format "https://%s/%s/blob/%s/%s"
	  hostname
	  dirname
	  (or branch commit)
	  (concat filename
                  (when start
                    (concat (if (git-link--should-render-plain filename) "?plain=1#" "#")
                            (if end
                                (format "L%s-L%s" start end)
                              (format "L%s" start)))))))

(defun git-link-googlesource (hostname dirname filename branch commit start end)
  (format "https://%s/%s/+/%s/%s"
	  hostname
	  dirname
	  (or branch commit)
	  (concat filename
                  (when start
                    (format "#%s" start)
                    ))))

(defun git-link-azure (hostname dirname filename branch commit start end)
  (format "https://%s/%s?path=%%2F%s&version=%s&line=%s&lineEnd=%s&lineStartColumn=1&lineEndColumn=9999&lineStyle=plain"
	  hostname
	  dirname
      filename
      (concat "G" (if branch "B" "C") (or branch commit))
      (or start "")
      (or end start "")))

(defun git-link-sourcehut (hostname dirname filename branch commit start end)
  (format "https://%s/%s/tree/%s/%s"
	  hostname
	  dirname
	  (or branch commit)
	  (concat filename
                  (when start
                    (concat "#"
                            (if end
                                (format "L%s-%s" start end)
                              (format "L%s" start)))))))

(defun git-link-commit-gitlab (hostname dirname commit)
  (format "https://%s/%s/-/commit/%s"
	  hostname
	  dirname
	  commit))

(defun git-link-commit-github (hostname dirname commit)
  (format "https://%s/%s/commit/%s"
	  hostname
	  dirname
	  commit))

(defun git-link-commit-googlesource (hostname dirname commit)
  (format "https://%s/%s/+/%s"
	  hostname
	  dirname
          commit))

(defun git-link-commit-azure (hostname dirname commit)
 (format "https://%s/%s/commit/%s"
	  hostname
	  dirname

      ;; Azure only supports full 32 characters SHA
      (car (git-link--exec "rev-parse" commit))))

(defun git-link-commit-codeberg (hostname dirname commit)
    (format "https://%s/%s/commit/%s"
	    hostname
	    dirname
	    commit))

(defun git-link-gitorious (hostname dirname filename _branch commit start _end)
  (format "https://%s/%s/source/%s:%s#L%s"
	  hostname
	  dirname
	  commit
	  filename
	  start))

(defun git-link-commit-gitorious (hostname dirname commit)
  (format "https://%s/%s/commit/%s"
	  hostname
	  dirname
	  commit))

(defun git-link-bitbucket (hostname dirname filename _branch commit start end)
  ;; ?at=branch-name
  (format "https://%s/%s/src/%s/%s"
          hostname
          dirname
          commit
          (if (string= "" (file-name-nondirectory filename))
              filename
            (concat filename
                    "#"
                    (file-name-nondirectory filename)
                    (when start
                      (if end
                          (format "-%s:%s" start end)
                        (format "-%s" start)))))))

(defun git-link-commit-bitbucket (hostname dirname commit)
  ;; ?at=branch-name
  (format "https://%s/%s/commits/%s"
	  hostname
	  dirname
	  commit))

(defun git-link-cgit (hostname dirname filename branch commit start _end)
  (format "https://%s/%s/tree/%s?h=%s"
	  hostname
	  dirname
          filename
          (concat
           (or branch commit)
           (when start
             (concat "#" (format "n%s" start))))))

(defun git-link-commit-cgit (hostname dirname commit)
  (format "https://%s/%s/commit/?id=%s"
	  hostname
          dirname
	  commit))

(defun git-link-savannah (hostname dirname filename branch commit start end)
  (git-link-cgit hostname
                 (format "cgit/%s.git" dirname) ; unique to Savannah
                 filename
                 branch
                 commit
                 start
                 end))

(defun git-link-commit-savannah (hostname dirname commit)
  (git-link-commit-cgit hostname
                        (format "cgit/%s.git" dirname) ; also unique to Savannah
                        commit))

(defun git-link-sourcegraph (hostname dirname filename branch commit start end)
  (let ((line-or-range (cond ((and start end) (format "#L%s-%s" start end))
                             (start (format "#L%s" start))
                             (t "")))
        (branch-or-commit (or branch commit))
        (dir-file-name (directory-file-name dirname)))
    (format "https://%s/%s@%s/-/blob/%s%s"
            hostname
            dir-file-name
            branch-or-commit
            filename
            line-or-range)))

(defun git-link-commit-sourcegraph (hostname dirname commit)
  (let ((dir-file-name (directory-file-name dirname)))
    (format "https://%s/%s/-/commit/%s"
            hostname
            dir-file-name
            commit)))

(defun git-link-homepage-github (hostname dirname)
  (format "https://%s/%s"
	  hostname
	  dirname))

(defun git-link-homepage-savannah (hostname dirname)
  (format "https://%s/cgit/%s.git/"
	  hostname
	  dirname))

(defun git-link-codecommit (hostname
                            dirname
                            filename
                            branch
                            commit
                            start
                            end)
  (format "https://%s/%s/browse/refs/heads/%s/--/%s"
          hostname
          dirname
          (or branch commit)
          (concat filename
                  (when start
                    (format "?lines=%s-%s"
                            start
                            (or end start))))))

(defun git-link-commit-codecommit (hostname dirname commit)
  (format "https://%s/%s/commit/%s" hostname dirname commit))

(defun git-link-homepage-codecommit (hostname dirname)
  (format "https://%s/%s/browse" hostname dirname))

(define-obsolete-function-alias
  'git-link-homepage-svannah 'git-link-homepage-savannah "cf947f9")

(defun git-link--select-remote ()
  (if (equal '(4) current-prefix-arg)
      (git-link--read-remote)
    (git-link--remote)))

(defun git-link--should-render-plain (filename)
  "Check if the extension of the given filename belongs
to the list of extensions which generated link should be
shown as a plain file"
  (let ((extension (or (file-name-extension filename) "")))
    (member (downcase extension) git-link-extensions-rendered-plain)))

;;;###autoload
(defun git-link (remote start end)
  "Create a URL representing the current buffer's location in its
GitHub/Bitbucket/GitLab/... repository at the current line number
or active region. The URL will be added to the kill ring.  If
`git-link-open-in-browser' is non-nil also call `browse-url'.

With a prefix argument of - generate a link without line number(s).
Also see `git-link-use-single-line-number'.

With a single prefix argument prompt for the remote's name.
Defaults to \"origin\".

With a double prefix argument invert the value of
`git-link-use-commit'."
  (interactive
   (if (equal '- current-prefix-arg)
       (list (git-link--remote) nil nil)
     (let* ((remote (git-link--select-remote))
            (region (when (or buffer-file-name (git-link--using-magit-blob-mode))
                      (git-link--get-region))))

       (if (and (null git-link-use-single-line-number) (null (cadr region)))
           (list remote nil nil)
         (list remote (car region) (cadr region))))))

  (let (filename branch commit handler remote-info (remote-url (git-link--remote-url remote)))
    (if (null remote-url)
        (message "Remote `%s' not found" remote)

      (setq remote-info (git-link--parse-remote remote-url)
            filename    (git-link--relative-filename)
            branch      (git-link--branch)
            commit      (git-link--commit)
            handler     (git-link--handler git-link-remote-alist (car remote-info)))

      (cond ((null filename)
             (message "Can't figure out what to link to"))
            ((null (car remote-info))
             (message "Remote `%s' contains an unsupported URL" remote))
            ((not (functionp handler))
             (message "No handler found for %s" (car remote-info)))
            ;; TODO: null ret val
            (t
             (let ((vc-revison (git-link--parse-vc-revision filename)))
               (when vc-revison
                 (setq filename (car vc-revison)
                       commit   (cdr vc-revison)))

               (git-link--new
                (funcall handler
                         (car remote-info)
                         (cadr remote-info)
                         (url-hexify-string filename (url--allowed-chars (cons ?/ url-unreserved-chars)))
                         (if (or (git-link--using-git-timemachine)
                                 (git-link--using-magit-blob-mode)
                                 vc-revison
                                 (if (equal '(16) current-prefix-arg)
                                     (not git-link-use-commit)
                                   git-link-use-commit))
                             nil
                           (url-hexify-string branch))
                         commit
                         start
                         end))))))))

;;;###autoload
(defun git-link-commit (remote)
  "Create a URL representing the commit for the hash under point
in the current buffer's GitHub/Bitbucket/GitLab/...
repository. The URL will be added to the kill ring.

With a prefix argument prompt for the remote's name.
Defaults to \"origin\"."

  (interactive (list (git-link--select-remote)))
  (let* (commit handler remote-info (remote-url (git-link--remote-url remote)))
    (if (null remote-url)
        (message "Remote `%s' not found" remote)

      (setq remote-info (git-link--parse-remote remote-url)
            commit (word-at-point)
            handler (git-link--handler git-link-commit-remote-alist (car remote-info)))

      (cond ((null (car remote-info))
             (message "Remote `%s' contains an unsupported URL" remote))
            ((not (string-match-p "[a-fA-F0-9]\\{7,40\\}" (or commit "")))
             (message "Point is not on a commit hash"))
            ((not (functionp handler))
             (message "No handler for %s" (car remote-info)))
            ;; null ret val
            ((git-link--new
              (funcall handler
                       (car remote-info)
                       (cadr remote-info)
                       (substring-no-properties commit))))))))

;;;###autoload
(defun git-link-homepage (remote)
  "Create a URL representing the homepage of the current
buffer's GitHub/Bitbucket/GitLab/... repository. The
URL will be added to the kill ring.

With a prefix argument prompt for the remote's name.
Defaults to \"origin\"."

  (interactive (list (git-link--select-remote)))
  (let* (handler remote-info (remote-url (git-link--remote-url remote)))
    (if (null remote-url)
        (message "Remote `%s' not found" remote)

      (setq remote-info (git-link--parse-remote remote-url)
            handler (git-link--handler git-link-homepage-remote-alist (car remote-info)))

      (cond ((null (car remote-info))
             (message "Remote `%s' contains an unsupported URL" remote))
            ((not (functionp handler))
             (message "No handler for %s" (car remote-info)))
            ;; null ret val
            ((git-link--new
              (funcall handler
                       (car remote-info)
                       (cadr remote-info))))))))

(provide 'git-link)
;;; git-link.el ends here
