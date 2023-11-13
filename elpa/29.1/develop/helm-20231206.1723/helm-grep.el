;;; helm-grep.el --- Helm Incremental Grep. -*- lexical-binding: t -*-

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
(require 'ansi-color)
(require 'cl-lib)
(require 'format-spec)
(require 'helm)
(require 'helm-help)
(require 'helm-regexp)

;;; load wgrep proxy if it's available
(require 'wgrep-helm nil t)

(declare-function helm-buffer-list "helm-buffers")
(declare-function View-quit "view")
(declare-function doc-view-goto-page "doc-view" (page))
(declare-function pdf-view-goto-page "pdf-view" (page &optional window))
(declare-function helm-mm-split-pattern "helm-multi-match")
(declare-function helm-comp-read "helm-mode")
(declare-function helm-occur "helm-occur")

(defvar helm--ansi-color-regexp)
(defvar helm-ff-default-directory)
(defvar helm-tramp-verbose)
(defvar helm-grep-ack-types-cache)
(defvar helm-grep-git-grep-command)
(defvar helm-source-grep-git)
(defvar tramp-verbose)
(defvar helm-current-error)

;;; Internals vars
;;
;;
(defvar helm-rzgrep-cache (make-hash-table :test 'equal))
(defvar helm-grep-default-function 'helm-grep-init)
(defvar helm-zgrep-recurse-flag nil)
(defvar helm-grep-history nil)
(defvar helm-grep-ag-history nil)
(defvar helm-grep-last-targets nil)
(defvar helm-grep-include-files nil)
(defvar helm-grep-in-recurse nil)
(defvar helm-grep-use-zgrep nil)
(defvar helm-grep-default-directory-fn nil
  "A function that should return a directory to expand candidate to.
It is intended to use as a let-bound variable, DON'T set this globaly.")
(defvar helm-pdfgrep-targets nil)
(defvar helm-grep-last-cmd-line nil)
(defvar helm-grep-split-line-regexp "^\\([[:lower:][:upper:]]?:?.*?\\):\\([0-9]+\\):\\(.*\\)")


;;; Keymaps
;;
;;
(defvar helm-grep-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'helm-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-goto-precedent-file)
    (define-key map (kbd "C-c o")    'helm-grep-run-other-window-action)
    (define-key map (kbd "C-c C-o")  'helm-grep-run-other-frame-action)
    (define-key map (kbd "C-x C-s")  'helm-grep-run-save-buffer)
    (define-key map (kbd "DEL")      'helm-delete-backward-no-update)
    map)
  "Keymap used in Grep sources.")

(defvar helm-pdfgrep-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'helm-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-goto-precedent-file)
    (define-key map (kbd "DEL")      'helm-delete-backward-no-update)
    map)
  "Keymap used in pdfgrep.")

(defvar helm-grep-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")      'helm-grep-mode-jump)
    (define-key map (kbd "C-o")      'helm-grep-mode-jump-other-window)
    (define-key map (kbd "<C-down>") 'helm-grep-mode-jump-other-window-forward)
    (define-key map (kbd "<C-up>")   'helm-grep-mode-jump-other-window-backward)
    (define-key map (kbd "<M-down>") 'helm-gm-next-file)
    (define-key map (kbd "<M-up>")   'helm-gm-precedent-file)
    (define-key map (kbd "M-n")      'helm-grep-mode-jump-other-window-forward)
    (define-key map (kbd "M-p")      'helm-grep-mode-jump-other-window-backward)
    (define-key map (kbd "M-N")      'helm-gm-next-file)
    (define-key map (kbd "M-P")      'helm-gm-precedent-file)
    map))


(defgroup helm-grep nil
  "Grep related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-grep-default-command
  "grep --color=always -a -d skip %e -n%cH -e %p %f"
  "Default grep format command for `helm-do-grep-1'.
Where:
'%e' format spec is for --exclude or --include grep options or
     ack-grep --type option.               (Not mandatory)

'%c' format spec is for case-fold-search,
     whether to use the -i option of grep. (Not mandatory)
     When you specify this spec, helm grep will use smartcase
     that is when a upcase character is found in pattern case will
     be respected and no \\='-i' option will be used, otherwise, when
     no upcase character is found in pattern always use \\='-i'.
     If you don't want this behavior, don't use this spec and
     specify or not the \\='-i' option.
     Note that with ack-grep this is not needed, just specify
     the \\='--smart-case' option.

'%p' format spec is for pattern.           (Mandatory)

'%f' format spec is for filenames.         (Mandatory)

If your grep version doesn't support the --exclude/include args
don't specify the \\='%e' format spec.

Helm also support ack-grep and git-grep.  The following is a
default command example for ack-grep:

\(setq helm-grep-default-command
       \"ack-grep -Hn --color --smart-case --no-group %e -- %p %f\"
       helm-grep-default-recurse-command
       \"ack-grep -H --color --smart-case --no-group %e -- %p %f\")

You can ommit the %e spec if you don't want to be prompted for
types.

NOTE: Helm for ack-grep support ANSI sequences, so you can remove
the \"--no-color\" option safely (recommended).
However you should specify --color to enable multi matches highlighting
because ack disable it when output is piped.

Same for grep you can use safely the option \"--color=always\" (default).
You can customize the color of matches using GREP_COLORS env var.
e.g: (setenv \"GREP_COLORS\"
              \"ms=30;43:mc=30;43:sl=01;37:cx=:fn=35:ln=32:bn=32:se=36\")

To enable ANSI color in git-grep just add \"--color=always\".
To customize the ANSI color in git-grep, GREP_COLORS have no effect,
you will have to setup this in your .gitconfig:

    [color \"grep\"]
        match = black yellow

Where \"black\" is the foreground and \"yellow\" the background.
See the git documentation for more infos.

`helm-grep-default-command' and
`helm-grep-default-recurse-command' are independent, so you can
enable `helm-grep-default-command' with ack-grep and
`helm-grep-default-recurse-command' with grep if you want to be
faster on recursive grep.

NOTE: Remote grepping is not available with ack-grep, and badly
      supported with grep because tramp handles badly repeated
      remote processes in a short delay (< to 5s)."
  :type  'string)

(defcustom helm-grep-default-recurse-command
  "grep --color=always -a -d recurse %e -n%cH -e %p %f"
  "Default recursive grep format command for `helm-do-grep-1'.
See `helm-grep-default-command' for format specs and infos about
ack-grep."
  :type  'string)

(defcustom helm-default-zgrep-command
  "zgrep --color=always -a -n%cH -e %p %f"
  "Default command for Zgrep.
See `helm-grep-default-command' for infos on format specs.
Option --color=always is supported and can be used safely to
replace the Helm internal match highlighting.  See
`helm-grep-default-command' for more infos."
  :type  'string)

(defcustom helm-pdfgrep-default-command
  "pdfgrep --color always -niH %s %s"
  "Default command for pdfgrep.
Option \"--color always\" is supported starting Helm version
1.7.8.  When used matches will be highlighted according to
GREP_COLORS env var."
  :type  'string)

(defcustom helm-pdfgrep-default-recurse-command
  "pdfgrep --color always -rniH %s %s"
  "Default recurse command for pdfgrep.
Option \"--color always\" is supported starting Helm version
1.7.8.  When used matches will be highlighted according to
GREP_COLORS env var."
  :type  'string)

(defcustom helm-pdfgrep-default-read-command nil
  "Default command to read pdf files from pdfgrep.
Where \\='%f' format spec is filename and \\='%p' is page number.
E.g. In Ubuntu you can set it to:

    \"evince --page-label=%p \\='%f'\"

If set to nil either `doc-view-mode' or `pdf-view-mode' will be
used instead of an external command."
  :type  'string)

(defcustom helm-grep-max-length-history 100
  "Max number of elements to save in `helm-grep-history'."
  :type 'integer)

(defcustom helm-zgrep-file-extension-regexp
  ".*\\(\\.gz\\|\\.bz\\|\\.xz\\|\\.lzma\\)$"
  "Default file extensions zgrep will search in."
  :type 'string)

(defcustom helm-grep-preferred-ext nil
  "This file extension will be preselected for grep."
  :type  'string)

(defcustom helm-grep-save-buffer-name-no-confirm nil
  "When *hgrep* already exists, auto append suffix."
  :type 'boolean)

(defcustom helm-grep-ignored-files
  (cons ".#*" (delq nil (mapcar (lambda (s)
                                  (unless (string-match-p "/\\'" s)
                                    (concat "*" s)))
                                completion-ignored-extensions)))
  "List of file names which `helm-grep' shall exclude."
  :type '(repeat string))

(defcustom helm-grep-ignored-directories
  helm-walk-ignore-directories
  "List of names of sub-directories which `helm-grep' shall not recurse into."
  :type '(repeat string))

(defcustom helm-grep-truncate-lines t
  "When nil the grep line that appears will not be truncated."
  :type 'boolean)

(defcustom helm-grep-file-path-style 'basename
  "File path display style when grep results are displayed.
Possible value are:
    basename: displays only the filename, none of the directory path
    absolute: displays absolute path
    relative: displays relative path from root grep directory."
  :type '(choice (const :tag "Basename" basename)
                 (const :tag "Absolute" absolute)
                 (const :tag "Relative" relative)))

(defcustom helm-grep-actions
  (helm-make-actions
   "Find File" 'helm-grep-action
   "Find file other frame" 'helm-grep-other-frame
   "Save results in grep buffer" 'helm-grep-save-results
   "Find file other window (C-u vertically)" 'helm-grep-other-window)
  "Actions for helm grep."
  :type '(alist :key-type string :value-type function))

(defcustom helm-grep-pipe-cmd-switches nil
  "A list of additional parameters to pass to grep pipe command.
This will be used to pipe command for multiple pattern matching
for grep, zgrep ack-grep and git-grep backends.
If you add extra args for ack-grep, use ack-grep options, for
others (grep, zgrep and git-grep) use grep options.
Here are the commands where you may want to add switches:

    grep --color=always
    ack-grep --smart-case --color

You probably don't need to use this unless you know what you are
doing."
  :type '(repeat string))

(defcustom helm-grep-ag-pipe-cmd-switches nil
  "A list of additional parameters to pass to grep-ag pipe command.
Use parameters compatibles with the backend you are using
\(i.e. AG for AG, PT for PT or RG for RG)
Here are the commands where you may want to add switches:

    ag -S --color
    rg -N -S --color=?

For RG the value of --color= is computed according to the --color=
value used in `helm-grep-ag-command'.

Note also that by default the \"--\" option is always used, you don't
need to add it here.
 
You probably don't need to use this unless you know what you are
doing."
  :type '(repeat string))

(defcustom helm-grep-input-idle-delay 0.1
  "Idle time before updating, specified in seconds.
A lower value (default) means Helm will display the results
faster. Increasing it to a higher value (e.g. 0.6) prevents the
buffer from flickering when updating."
  :type 'float)

(defcustom helm-grep-use-ioccur-style-keys t
  "Use Arrow keys to jump to occurences.
Note that if you define this variable with `setq' your change
will have no effect, use customize instead."
  :type  'boolean
  :set (lambda (var val)
         (set var val)
         (if val
             (progn
               (define-key helm-grep-map (kbd "<right>")  'helm-execute-persistent-action)
               (define-key helm-grep-map (kbd "<left>")   'helm-grep-run-default-action))
           (define-key helm-grep-map (kbd "<right>") nil)
           (define-key helm-grep-map (kbd "<left>")  nil))))

(defcustom helm-grep-ag-command
  (cond ((executable-find "rg")
         "rg --color=always --smart-case --search-zip --no-heading --line-number %s -- %s %s")
        ((executable-find "ag")
         "ag --line-numbers -S --color --nogroup %s -- %s %s"))
  "The default command for RG or AG.

Prefer RG by default if available.

Update: PT is still mentionned in this documentation but it seems it
doesn't exists anymore, or at least it is no more maintained.

Takes three format specs, the first for type(s), the second for
pattern and the third for directory.

You can use safely \"--color\" (used by default) with AG RG and
PT.

NOTE: Usage of \"--color=never\" is discouraged as it uses Elisp
to colorize matched items which is slower than using the native
colorization of backend, however it is still supported.

For ripgrep here is the command line to use:

    rg --color=always --smart-case --no-heading --line-number %s -- %s %s

And to customize colors (always for ripgrep) use something like this:

    rg --color=always --colors \\='match:bg:yellow' --colors \\='match:fg:black'
\--smart-case --no-heading --line-number %s -- %s %s

This will change color for matched items from foreground red (the
default) to a yellow background with a black foreground.  Note
that your color settings for RG will not work properly with
multiples pattern if you have configured colors in rg config file
instead of command line. For more enhanced settings of ansi
colors see https://github.com/emacs-helm/helm/issues/2313

You must use an output format that fit with helm grep, that is:

    \"filename:line-number:string\"

The option \"--nogroup\" allow this.
The option \"--line-numbers\" is also mandatory except with
PT (not supported).
For RG the options \"--no-heading\" and \"--line-number\" are the
ones to use.

When modifying the default colors of matches with e.g.
\"--color-match\" option of AG or \"--colors\" option of ripgrep
you may want to modify as well `helm-grep-ag-pipe-cmd-switches'
to have all matches colorized with the same color in multi
match.

Of course you can use several other options, see the man page of the
backend you are using."
  :type 'string)

(defcustom helm-grep-git-grep-command
  "git --no-pager grep -n%cH --color=always --full-name -e %p -- %f"
  "The git grep default command line.
The option \"--color=always\" can be used safely.
The color of matched items can be customized in your .gitconfig
See `helm-grep-default-command' for more infos.

The \"--exclude-standard\" and \"--no-index\" switches allow
skipping unwanted files specified in ~/.gitignore_global and
searching files not already staged (not enabled by default).

You have also to enable this in global \".gitconfig\" with
    \"git config --global core.excludesfile ~/.gitignore_global\"."
  :type 'string)


;;; Faces
;;
;;
(defgroup helm-grep-faces nil
  "Customize the appearance of helm-grep."
  :prefix "helm-"
  :group 'helm-grep
  :group 'helm-faces)

(defface helm-grep-match
  `((((background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "#b00000")
    (((background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "gold1"))
  "Face used to highlight grep matches.
Have no effect when grep backend use \"--color=\"."
  :group 'helm-grep-faces)

(defface helm-grep-file
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "BlueViolet"
       :underline t))
  "Face used to highlight grep results filenames."
  :group 'helm-grep-faces)

(defface helm-grep-lineno
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "Darkorange1"))
  "Face used to highlight grep number lines."
  :group 'helm-grep-faces)

(defface helm-grep-finish
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "Green"))
  "Face used in mode line when grep is finish."
  :group 'helm-grep-faces)

(defface helm-grep-cmd-line
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :inherit font-lock-type-face))
  "Face used to highlight grep command line when no results."
  :group 'helm-grep-faces)


;;; Init
;;
;;
(defun helm-grep-prepare-candidates (candidates in-directory)
  "Prepare filenames and directories CANDIDATES for grep command line."
  ;; If one or more candidate is a directory, search in all files
  ;; of this candidate (e.g /home/user/directory/*).
  ;; If r option is enabled search also in subdidrectories.
  ;; We need here to expand wildcards to support crap windows filenames
  ;; as grep doesn't accept quoted wildcards (e.g "dir/*.el").
  (if helm-zgrep-recurse-flag
      (mapconcat 'shell-quote-argument candidates " ")
      ;; When candidate is a directory, search in all its files.
      ;; NOTE that `file-expand-wildcards' will return also
      ;; directories, they will be ignored by grep but not
      ;; by ack-grep that will grep all files of this directory
      ;; without recursing in their subdirs though, see that as a one
      ;; level recursion with ack-grep.
      ;; So I leave it as it is, considering it is a feature. [1]
      (cl-loop for i in candidates append
               (cond ((string-match "^git" helm-grep-default-command)
                      (list i))
                     ;; Candidate is a directory and we use recursion or ack.
                     ((and (file-directory-p i)
                           (or helm-grep-in-recurse
                               ;; ack-grep accept directory [1].
                               (helm-grep-use-ack-p)))
                      (list (expand-file-name i)))
                     ;; Grep doesn't support directory only when not in recurse.
                     ((file-directory-p i)
                      (file-expand-wildcards
                       (concat (file-name-as-directory (expand-file-name i)) "*") t))
                     ;; Candidate is a file or wildcard and we use recursion, use the
                     ;; current directory instead of candidate.
                     ((and (or (file-exists-p i) (string-match "[*]" i))
                           helm-grep-in-recurse)
                      (list (expand-file-name
                             (directory-file-name ; Needed for windoze.
                              (file-name-directory (directory-file-name i))))))
                     ;; Else should be one or more file/directory
                     ;; possibly marked.
                     ;; When real is a normal filename without wildcard
                     ;; file-expand-wildcards returns a list of one file.
                     ;; wildcards should have been already handled by
                     ;; helm-read-file-name or helm-find-files but do it from
                     ;; here too in case we are called from elsewhere.
                     (t (file-expand-wildcards i t))) into all-files ; [1]
               finally return
               (let ((files (if (file-remote-p in-directory)
                                ;; Grep don't understand tramp filenames
                                ;; use the local name.
                                (mapcar (lambda (x)
                                          (file-remote-p x 'localname))
                                        all-files)
                                all-files)))
                 ;; When user mark files and use recursion with grep
                 ;; backend enabled, the loop collect on each marked
                 ;; candidate its `file-name-directory' and we endup with
                 ;; duplicates (Bug#1714). FIXME: For now as a quick fix
                 ;; I just remove dups here but I should handle this inside
                 ;; the cond above.
                 (setq files (helm-fast-remove-dups files :test 'equal))
                 (if (string-match "^git" helm-grep-default-command)
                     (mapconcat 'identity files " ")
                     (mapconcat 'shell-quote-argument files " "))))))

(defun helm-grep-command (&optional recursive grep)
  (let* ((com (if recursive
                  helm-grep-default-recurse-command
                  helm-grep-default-command))
         (exe (if grep
                  (symbol-name grep)
                  (and com (car (split-string com " "))))))
    (if (and exe (string= exe "git")) "git-grep" exe)))

(cl-defun helm-grep-use-ack-p (&key where)
  (let* ((rec-com (helm-grep-command t))
         (norm-com (helm-grep-command))
         (norm-com-ack-p (string-match "\\`ack" norm-com))
         (rec-com-ack-p (and rec-com (string-match "\\`ack" rec-com))))
    (cl-case where
      (default   (and norm-com norm-com-ack-p))
      (recursive (and rec-com rec-com-ack-p))
      (strict    (and norm-com rec-com rec-com-ack-p norm-com-ack-p))
      (t         (and (not (and norm-com (string= norm-com "git-grep")))
                      (or (and norm-com norm-com-ack-p)
                          (and rec-com rec-com-ack-p)))))))

(defun helm-grep--pipe-command-for-grep-command (smartcase pipe-switches &optional grep-cmd)
  (pcase (or grep-cmd (helm-grep-command))
    ;; Use grep for GNU regexp based tools.
    ((or "grep" "zgrep" "git-grep")
     (format "grep --color=always%s %s"
             (if smartcase " -i" "")
             pipe-switches))
    ;; Use ack-grep for PCRE based tools.
    ;; Sometimes ack-grep cmd is ack only.
    ((and (pred (string-match-p "ack")) ack)
     (format "%s --smart-case --color %s" ack pipe-switches))))

(defun helm-grep--prepare-cmd-line (only-files &optional include zgrep)
  (let* ((default-directory (or helm-ff-default-directory
                                (helm-default-directory)
                                default-directory))
         (fnargs            (helm-grep-prepare-candidates
                             only-files default-directory))
         (ignored-files     (unless (helm-grep-use-ack-p)
                              (mapconcat
                               (lambda (x)
                                   (concat "--exclude="
                                           (shell-quote-argument x)))
                               helm-grep-ignored-files " ")))
         (ignored-dirs      (unless (helm-grep-use-ack-p)
                              (mapconcat
                               ;; Need grep version >=2.5.4
                               ;; of Gnuwin32 on windoze.
                               (lambda (x)
                                   (concat "--exclude-dir="
                                           (shell-quote-argument x)))
                               helm-grep-ignored-directories " ")))
         (exclude           (unless (helm-grep-use-ack-p)
                              (let ((inc     (and include
                                                  (concat include " ")))
                                    (igfiles (and ignored-files
                                                  (concat ignored-files " ")))
                                    (igdirs  (and helm-grep-in-recurse
                                                  ignored-dirs)))
                                (concat inc igfiles igdirs))))
         (types             (and (helm-grep-use-ack-p)
                                 ;; When %e format spec is not specified
                                 ;; in `helm-grep-default-command'
                                 ;; we need to pass an empty string
                                 ;; to types to avoid error.
                                 (or include "")))
         (smartcase         (if (helm-grep-use-ack-p)
                                ""
                                (unless (let ((case-fold-search nil))
                                          (string-match-p
                                           "[[:upper:]]" helm-pattern))
                                  "i")))
         (helm-grep-default-command
          (concat helm-grep-default-command " %m")) ; `%m' like multi.
         (patterns (helm-mm-split-pattern helm-pattern t))
         (pipe-switches (mapconcat 'identity helm-grep-pipe-cmd-switches " "))
         (pipes
          (helm-aif (cdr patterns)
              (cl-loop with pipcom = (helm-grep--pipe-command-for-grep-command
                                      smartcase pipe-switches)
                       for p in it concat
                       (format " | %s %s" pipcom (shell-quote-argument p)))
            "")))
    (format-spec
     helm-grep-default-command
     (delq nil
           (list (unless zgrep
                   (if types
                       (cons ?e types)
                     (cons ?e exclude)))
                 (cons ?c (or smartcase ""))
                 (cons ?p (shell-quote-argument (car patterns)))
                 (cons ?f fnargs)
                 (cons ?m pipes))))))

(defun helm-grep-init (cmd-line)
  "Start an asynchronous grep process with CMD-LINE using ZGREP if non-nil."
  (let* ((default-directory (or helm-ff-default-directory
                                (helm-default-directory)
                                default-directory))
         (zgrep (string-match "\\`zgrep" cmd-line))
         ;; Use pipe only with grep, zgrep or git-grep.
         (process-connection-type (and (not zgrep) (helm-grep-use-ack-p)))
         (tramp-verbose helm-tramp-verbose)
         (start-time (float-time))
         (proc-name (if helm-grep-use-zgrep
                        "Zgrep"
                        (capitalize
                         (if helm-grep-in-recurse
                             (helm-grep-command t)
                             (helm-grep-command)))))
         non-essential)
    ;; Start grep process.
    (helm-log "helm-grep-init" "Starting Grep process in directory `%s'" default-directory)
    (helm-log "helm-grep-init" "Command line used was:\n\n%s"
              (concat ">>> " (propertize cmd-line 'face 'helm-grep-cmd-line) "\n\n"))
    (prog1            ; This function should return the process first.
        (start-file-process-shell-command
         proc-name helm-buffer cmd-line)
      ;; Init sentinel.
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       (lambda (process event)
           (let* ((err      (process-exit-status process))
                  (noresult (= err 1)))
             (unless (and err (> err 0))
               (helm-process-deferred-sentinel-hook
                process event (helm-default-directory)))
             (cond ((and noresult
                         ;; This is a workaround for zgrep
                         ;; that exit with code 1
                         ;; after a certain amount of results.
                         (with-helm-buffer (helm-empty-buffer-p)))
                    (with-helm-buffer
                      (insert (concat "* Exit with code 1, no result found,"
                                      " command line was:\n\n "
                                      (propertize helm-grep-last-cmd-line
                                                  'face 'helm-grep-cmd-line)))
                      (setq mode-line-format
                            `(" " mode-line-buffer-identification " "
                              (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                              (:eval (propertize
                                      (format
                                       "[%s process finished - (no results)] "
                                       ,proc-name)
                                      'face 'helm-grep-finish))))))
                   ((or (string= event "finished\n")
                        (and noresult
                             ;; This is a workaround for zgrep
                             ;; that exit with code 1
                             ;; after a certain amount of results.
                             (with-helm-buffer (not (helm-empty-buffer-p)))))
                    (helm-log "helm-grep-init" "%s process finished with %s results in %fs"
                              proc-name
                              (helm-get-candidate-number)
                              (- (float-time) start-time))
                    (helm-maybe-show-help-echo)
                    (with-helm-window
                      (setq mode-line-format
                            `(" " mode-line-buffer-identification " "
                              (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                              (:eval (propertize
                                      (format
                                       "[%s process finished in %.2fs - (%s results)] "
                                       ,proc-name
                                       ,(- (float-time) start-time)
                                       (helm-get-candidate-number))
                                      'face 'helm-grep-finish))))
                      (force-mode-line-update)
                      (when (and helm-allow-mouse helm-selection-point)
                        (helm--bind-mouse-for-selection helm-selection-point))))
                   ;; Catch error output in log.
                   (t (helm-log
                       "helm-grep-init"
                       "Error: %s %s"
                       proc-name
                       (replace-regexp-in-string "\n" "" event))))))))))

(defun helm-grep-collect-candidates ()
  (let ((cmd-line (helm-grep--prepare-cmd-line
                   helm-grep-last-targets
                   helm-grep-include-files
                   helm-grep-use-zgrep)))
    (set (make-local-variable 'helm-grep-last-cmd-line) cmd-line)
    (funcall helm-grep-default-function cmd-line)))


;;; Actions
;;
;;
(defun helm-grep-action (candidate &optional where)
  "Define a default action for `helm-do-grep-1' on CANDIDATE.
WHERE can be `other-window' or `other-frame'."
  (let* ((split        (helm-grep-split-line candidate))
         (split-pat    (helm-mm-split-pattern helm-input))
         (lineno       (string-to-number (nth 1 split)))
         (loc-fname        (or (with-current-buffer
                                   (if (eq major-mode 'helm-grep-mode)
                                       (current-buffer)
                                       helm-buffer)
                                 (get-text-property (pos-bol)
                                                    'helm-grep-fname))
                               (car split)))
         (tramp-fname  (file-remote-p (or helm-ff-default-directory
                                          default-directory)))
         (fname        (if tramp-fname
                           (concat tramp-fname loc-fname)
                         loc-fname)))
    (helm-log "helm-grep-action" "helm-grep-action fname: %s" fname )
    (cl-case where
      (other-window (helm-window-show-buffers
                     (list (find-file-noselect fname)) t))
      (other-frame  (find-file-other-frame fname))
      (grep         (helm-grep-save-results-1))
      (pdf          (if helm-pdfgrep-default-read-command
                        (helm-pdfgrep-action-1 split lineno (car split))
                      (find-file (car split))
                      (if (derived-mode-p 'pdf-view-mode)
                          (pdf-view-goto-page lineno)
                        (doc-view-goto-page lineno))))
      (t            (find-file fname)))
    (unless (or (eq where 'grep) (eq where 'pdf))
      (helm-goto-line lineno)
      ;; Move point to the nearest matching regexp from bol.
      (cl-loop for reg in split-pat
               when (save-excursion
                      (condition-case _err
                          (if helm-migemo-mode
                              (helm-mm-migemo-forward reg (pos-eol) t)
                            (re-search-forward reg (pos-eol) t))
                        (invalid-regexp nil)))
               collect (match-beginning 0) into pos-ls
               finally (when pos-ls (goto-char (apply #'min pos-ls))))
      ;; Save history
      (unless (or helm-in-persistent-action
                  (eq major-mode 'helm-grep-mode)
                  (string= helm-pattern ""))
        (setq helm-grep-history
              (cons helm-pattern
                    (delete helm-pattern helm-grep-history)))
        (when (> (length helm-grep-history)
                 helm-grep-max-length-history)
          (setq helm-grep-history
                (delete (car (last helm-grep-history))
                        helm-grep-history)))))))

(defun helm-grep-persistent-action (candidate)
  "Persistent action for `helm-do-grep-1'.
With a prefix arg record CANDIDATE in `mark-ring'."
  (helm-grep-action candidate)
  (helm-highlight-current-line))

(defun helm-grep-other-window (candidate)
  "Jump to result in other window from helm grep."
  (helm-grep-action candidate 'other-window))

(defun helm-grep-other-frame (candidate)
  "Jump to result in other frame from helm grep."
  (helm-grep-action candidate 'other-frame))

(defun helm-goto-next-or-prec-file (n)
  "Go to next or precedent candidate file in helm grep/etags buffers.
If N is positive go forward otherwise go backward."
  (let* ((allow-mode (or (eq major-mode 'helm-grep-mode)
                         (eq major-mode 'helm-moccur-mode)
                         (eq major-mode 'helm-occur-mode)))
         (sel (if allow-mode
                  (buffer-substring (pos-bol) (pos-eol))
                (helm-get-selection nil t)))
         (current-line-list  (helm-grep-split-line sel))
         (current-fname      (nth 0 current-line-list))
         (bob-or-eof         (if (eq n 1) 'eobp 'bobp))
         (mark-maybe (lambda ()
                         (if allow-mode
                             (ignore)
                           (helm-mark-current-line)))))
    (catch 'break
      (while (not (funcall bob-or-eof))
        (forward-line n) ; Go forward or backward depending of n value.
        ;; Exit when current-fname is not matched or in `helm-grep-mode'
        ;; the line is not a grep line i.e 'fname:num:tag'.
        (setq sel (buffer-substring (pos-bol) (pos-eol)))
        (when helm-allow-mouse
          (helm--mouse-reset-selection-help-echo))
        (unless (or (string= current-fname
                             (car (helm-grep-split-line sel)))
                    (and (eq major-mode 'helm-grep-mode)
                         (not (get-text-property (pos-bol) 'helm-grep-fname))))
          (funcall mark-maybe)
          (throw 'break nil))))
    (cond ((and (> n 0) (eobp))
           (re-search-backward ".")
           (forward-line 0)
           (funcall mark-maybe))
          ((and (< n 0) (bobp))
           (helm-aif (next-single-property-change (pos-bol) 'helm-grep-fname)
               (goto-char it)
             (forward-line 1))
           (funcall mark-maybe)))
    (unless allow-mode
      (helm-follow-execute-persistent-action-maybe)
      (helm-log-run-hook "helm-goto-next-or-prec-file" 'helm-move-selection-after-hook))))

;;;###autoload
(defun helm-goto-precedent-file ()
  "Go to previous file in Helm grep/etags buffers."
  (interactive)
  (with-helm-alive-p
    (with-helm-window
      (helm-goto-next-or-prec-file -1))))
(put 'helm-goto-precedent-file 'helm-only t)

;;;###autoload
(defun helm-goto-next-file ()
  "Go to previous file in Helm grep/etags buffers."
  (interactive)
  (with-helm-window
    (helm-goto-next-or-prec-file 1)))

(helm-make-command-from-action helm-grep-run-default-action
  "Run grep default action from `helm-do-grep-1'."
  'helm-grep-action)

(helm-make-command-from-action helm-grep-run-other-window-action
  "Run grep goto other window action from `helm-do-grep-1'."
  'helm-grep-other-window)

(helm-make-command-from-action helm-grep-run-other-frame-action
  "Run grep goto other frame action from `helm-do-grep-1'."
  'helm-grep-other-frame)

(helm-make-command-from-action helm-grep-run-save-buffer
  "Run grep save results action from `helm-do-grep-1'."
  'helm-grep-save-results)

(defun helm-grep-quit-an-find-file-fn (source)
  (let* ((sel (helm-get-selection nil nil source))
         (grep-line (and (stringp sel)
                         (helm-grep-split-line sel))))
    (if (and grep-line (file-exists-p (car grep-line)))
        (expand-file-name (car grep-line))
      default-directory)))

;;; helm-grep-mode
;;
;;
(defun helm-grep-save-results (candidate)
  (helm-grep-action candidate 'grep))

(defvar helm-grep-mode-use-pcre nil)
(defun helm-grep-save-results-1 ()
  "Save Helm grep result in a `helm-grep-mode' buffer."
  (let* ((buf "*hgrep*")
         new-buf
         (pattern (with-helm-buffer helm-input-local))
         (src (helm-get-current-source))
         (src-name (assoc-default 'name src)))
    (when (get-buffer buf)
      (if helm-grep-save-buffer-name-no-confirm
          (setq new-buf  (format "*hgrep|%s|-%s" pattern
                                 (format-time-string "%H-%M-%S*")))
        (setq new-buf (helm-read-string "GrepBufferName: " buf))
        (cl-loop for b in (helm-buffer-list)
                 when (and (string= new-buf b)
                           (not (y-or-n-p
                                 (format "Buffer `%s' already exists overwrite? "
                                         new-buf))))
                 do (setq new-buf (helm-read-string "GrepBufferName: " "*hgrep "))))
      (setq buf new-buf))
    (with-current-buffer (get-buffer-create buf)
      (setq default-directory (or helm-ff-default-directory
                                  (helm-default-directory)
                                  default-directory))
      (setq-local helm-grep-mode-use-pcre (helm-get-attr 'pcre src))
      (setq buffer-read-only t)
      (let ((inhibit-read-only t)
            (map (make-sparse-keymap)))
        (erase-buffer)
        (insert "-*- mode: helm-grep -*-\n\n"
                (format "%s Results for `%s':\n\n" src-name pattern))
        (save-excursion
          (insert (with-current-buffer helm-buffer
                    (goto-char (point-min)) (forward-line 1)
                    (buffer-substring (point) (point-max)))))
        (save-excursion
          (while (not (eobp))
            (add-text-properties (pos-bol) (pos-eol)
                                 `(keymap ,map
                                          help-echo ,(concat
                                                      (get-text-property
                                                       (point) 'helm-grep-fname)
                                                      "\nmouse-1: set point\nmouse-2: jump to selection")
                                          mouse-face highlight))
            (define-key map [mouse-1] 'mouse-set-point)
            (define-key map [mouse-2] 'helm-grep-mode-mouse-jump)
            (define-key map [mouse-3] 'ignore)
            (forward-line 1))))
      (helm-grep-mode))
    (pop-to-buffer buf)
    (setq next-error-last-buffer (get-buffer buf))
    (message "Helm %s Results saved in `%s' buffer" src-name buf)))

(defun helm-grep-mode-mouse-jump (event)
  (interactive "e")
  (let* ((window (posn-window (event-end event)))
         (pos    (posn-point (event-end event))))
    (with-selected-window window
      (when (eq major-mode 'helm-grep-mode)
        (goto-char pos)
        (helm-grep-mode-jump)))))
(put 'helm-grep-mode-mouse-jump 'helm-only t)

(defun helm-grep-next-error (&optional argp reset)
  "Goto ARGP position from a `helm-grep-mode' buffer.
RESET non-nil means rewind to the first match.
This is the `next-error-function' for `helm-grep-mode'."
  (interactive "p")
  (goto-char (cond (reset (point-min))
		   ((and (< argp 0) helm-current-error)
                    (line-beginning-position))
		   ((and (> argp 0) helm-current-error)
                    (line-end-position))
		   ((point))))
  (let ((fun (if (> argp 0)
                 #'next-single-property-change
               #'previous-single-property-change)))
    (helm-aif (funcall fun (point) 'helm-grep-fname)
        (progn
          (goto-char it)
          ;; `helm-current-error' is set in
          ;; `helm-grep-mode-jump'.
          (helm-grep-mode-jump))
      (user-error "No more matches"))))
(put 'helm-grep-next-error 'helm-only t)

;;;###autoload
(defun helm-revert-next-error-last-buffer ()
  "Revert last `next-error' buffer from `current-buffer'.

Accept to revert only `helm-grep-mode' or `helm-occur-mode' buffers.
Use this when you want to revert the `next-error' buffer after
modifications in `current-buffer'."
  (interactive)
  (let ((buffer  (next-error-find-buffer))
        (linum   (line-number-at-pos))
        (bufname (buffer-name)))
    (if buffer
        (with-current-buffer buffer
          (helm-aif (memq major-mode '(helm-grep-mode helm-occur-mode))
              (progn (revert-buffer)
                     ;; helm-occur-mode revert fn is synchronous so
                     ;; reajust from here (it is done with
                     ;; helm-grep-mode in its sentinel).
                     (when (eq (car it) 'helm-occur-mode)
                       (helm-grep-goto-closest-from-linum linum bufname)))
            (error "No suitable buffer to revert found")))
      (error "No suitable buffer to revert found"))))

(define-derived-mode helm-grep-mode
    special-mode "helm-grep"
    "Major mode to provide actions in helm grep saved buffer.

Special commands:
\\{helm-grep-mode-map}"
    (set (make-local-variable 'helm-grep-last-cmd-line)
         (with-helm-buffer helm-grep-last-cmd-line))
    (set (make-local-variable 'revert-buffer-function)
         #'helm-grep-mode--revert-buffer-function)
    (set (make-local-variable 'next-error-function)
         #'helm-grep-next-error)
    (set (make-local-variable 'helm-current-error) nil))
(put 'helm-grep-mode 'helm-only t)

(defun helm-grep-mode--revert-buffer-function (&optional _ignore-auto _noconfirm)
  (goto-char (point-min))
  (when (re-search-forward helm-grep-split-line-regexp nil t) (forward-line 0))
  (let ((inhibit-read-only t))
    (delete-region (point) (point-max)))
  (message "Reverting buffer...")
  (let ((process-connection-type
         ;; Git needs a nil value otherwise it tries to use a pager.
         (null (string-match-p "\\`git" helm-grep-last-cmd-line))))
    (set-process-sentinel
     (start-file-process-shell-command
      "hgrep"  (generate-new-buffer "*hgrep revert*") helm-grep-last-cmd-line)
     'helm-grep-mode--sentinel)))

(defun helm-grep-mode--sentinel (process event)
  (when (string= event "finished\n")
    (with-current-buffer (if (eq major-mode 'helm-grep-mode)
                             (current-buffer)
                           (next-error-find-buffer))
      (let ((inhibit-read-only t))
        (save-excursion
          (cl-loop for l in (with-current-buffer (process-buffer process)
                              (prog1 (split-string (buffer-string) "\n")
                                (kill-buffer)))
                   for line = (if (string-match-p helm--ansi-color-regexp l)
                                  (ansi-color-apply l) l)
                   when (string-match helm-grep-split-line-regexp line)
                   do (insert (propertize
                               (car (helm-grep-filter-one-by-one
                                     line helm-grep-mode-use-pcre))
                               ;; needed for wgrep.
                               'helm-realvalue line)
                              "\n"))))
      (when (fboundp 'wgrep-cleanup-overlays)
        (wgrep-cleanup-overlays (point-min) (point-max))))
    (unless (eq major-mode 'helm-grep-mode)
      (let ((bufname (buffer-name))
            (linum (line-number-at-pos)))
        (with-current-buffer (next-error-find-buffer)
          (helm-grep-goto-closest-from-linum linum bufname))))
    (message "Reverting buffer done")
    (when executing-kbd-macro (sit-for 1))))

(defun helm-grep-goto-closest-from-linum (linum bufname)
  (goto-char (point-min))
  (catch 'break
    (while (re-search-forward (format "^%s:\\([0-9]+\\):" (regexp-quote bufname)) nil t)
      (let ((numline (string-to-number (match-string 1))))
        (when (< (- linum numline) 0)
          (forward-line -1)
          (throw 'break nil))))))

(defun helm-gm-next-file ()
  (interactive)
  (helm-goto-next-or-prec-file 1))

(defun helm-gm-precedent-file ()
  (interactive)
  (helm-goto-next-or-prec-file -1))

(defun helm-grep-mode-jump ()
  (interactive)
  (setq next-error-last-buffer (current-buffer))
  (setq-local helm-current-error (point-marker))
  (helm-grep-action
   (buffer-substring (pos-bol) (pos-eol)))
  (helm-match-line-cleanup-pulse))

(defun helm-grep-mode-jump-other-window-1 (arg)
  (condition-case nil
      (progn
        (when (or (eq last-command 'helm-grep-mode-jump-other-window-forward)
                  (eq last-command 'helm-grep-mode-jump-other-window-backward))
          (forward-line arg))
        (save-selected-window
          (helm-grep-action (buffer-substring (pos-bol) (pos-eol))
                            'other-window)
          (helm-match-line-cleanup-pulse)
          (recenter)))
    (error nil)))

(defun helm-grep-mode-jump-other-window-forward (arg)
  (interactive "p")
  (helm-grep-mode-jump-other-window-1 arg))

(defun helm-grep-mode-jump-other-window-backward (arg)
  (interactive "p")
  (helm-grep-mode-jump-other-window-1 (- arg)))

(defun helm-grep-mode-jump-other-window ()
  (interactive)
  (setq next-error-last-buffer (current-buffer))
  (setq-local helm-current-error (point-marker))
  (let ((candidate (buffer-substring (pos-bol) (pos-eol))))
    (condition-case nil
        (progn (helm-grep-action candidate 'other-window)
               (helm-match-line-cleanup-pulse))
      (error nil))))


;;; ack-grep types
;;
;;
(defun helm-grep-hack-types ()
  "Return a list of known ack-grep types."
  (with-temp-buffer
    ;; "--help-types" works with both 1.96 and 2.1+, while
    ;; "--help types" works only with 1.96 Bug#422.
    ;; `helm-grep-command' should return the ack executable
    ;; when this function is used in the right context
    ;; i.e After checking is we are using ack-grep with
    ;; `helm-grep-use-ack-p'.
    (call-process (helm-grep-command t) nil t nil "--help-types")
    (goto-char (point-min))
    (cl-loop while (re-search-forward "^ +\\([^. ]+\\) +\\(.*\\)" nil t)
             collect (cons (concat (match-string 1)
                                   " [" (match-string 2) "]")
                           (match-string 1))
             collect (cons (concat "no" (match-string 1)
                                   " [" (match-string 2) "]")
                           (concat "no" (match-string 1))))))

(defun helm-grep-ack-types-transformer (candidates _source)
  (cl-loop for i in candidates
        if (stringp i)
        collect (rassoc i helm-grep-ack-types-cache)
        else
        collect i))

(defvar helm-grep-ack-types-cache nil)
(defun helm-grep-read-ack-type ()
  "Select types for the \\='--type' argument of ack-grep."
  (require 'helm-mode)
  (require 'helm-adaptive)
  (setq helm-grep-ack-types-cache (helm-grep-hack-types))
  (let ((types (helm-comp-read
                "Types: " helm-grep-ack-types-cache
                :name "*Ack-grep types*"
                :marked-candidates t
                :must-match t
                :fc-transformer '(helm-adaptive-sort
                                  helm-grep-ack-types-transformer)
                :buffer "*helm ack-types*")))
    (mapconcat (lambda (type) (concat "--type=" type)) types " ")))


;;; grep extensions
;;
;;
(defun helm-grep-guess-extensions (files)
  "Try to guess file extensions in FILES list when using grep recurse.
These extensions will be added to command line with --include arg
of grep."
  (cl-loop with ext-list = (list helm-grep-preferred-ext "*")
        with lst = (if (file-directory-p (car files))
                       (directory-files
                        (car files) nil
                        directory-files-no-dot-files-regexp)
                     files)
        for i in lst
        for ext = (file-name-extension i 'dot)
        for glob = (and ext (not (string= ext ""))
                        (concat "*" ext))
        unless (or (not glob)
                   (and glob-list (member glob glob-list))
                   (and glob-list (member glob ext-list))
                   (and glob-list (member glob helm-grep-ignored-files)))
        collect glob into glob-list
        finally return (delq nil (append ext-list glob-list))))

(defun helm-grep-get-file-extensions (files)
  "Try to return a list of file extensions to pass to \\='--include' arg of grep."
  (require 'helm-adaptive)
  (let* ((all-exts (helm-grep-guess-extensions
                    (mapcar 'expand-file-name files)))
         (extensions (helm-comp-read "Search Only in: " all-exts
                                     :marked-candidates t
                                     :fc-transformer 'helm-adaptive-sort
                                     :buffer "*helm grep exts*"
                                     :name "*helm grep extensions*")))
    (when (listp extensions) ; Otherwise it is empty string returned by C-RET.
      ;; If extensions is a list of one string containing spaces,
      ;; assume user entered more than one glob separated by space(s) and
      ;; split this string to pass it later to mapconcat.
      ;; e.g '("*.el *.py")
      (cl-loop for i in extensions
               append (split-string-and-unquote i " ")))))


;;; Set up source
;;
;;
(defvar helm-grep-before-init-hook nil
  "Hook that runs before initialization of the Helm buffer.")

(defvar helm-grep-after-init-hook nil
  "Hook that runs after initialization of the Helm buffer.")

(defclass helm-grep-class (helm-source-async)
  ((candidates-process :initform 'helm-grep-collect-candidates)
   (filtered-candidate-transformer :initform #'helm-grep-fc-transformer)
   (keymap :initform 'helm-grep-map)
   (pcre :initarg :pcre :initform nil
         :documentation
         "  Backend is using pcre regexp engine when non-nil.")
   (nohighlight :initform t)
   (nomark :initform t)
   (backend :initarg :backend
            :initform nil
            :documentation
            "  The grep backend that will be used.
  It is actually used only as an internal flag
  and doesn't set the backend by itself.
  You probably don't want to modify this.")
   (candidate-number-limit :initform 9999)
   (help-message :initform 'helm-grep-help-message)
   (history :initform 'helm-grep-history)
   (action :initform 'helm-grep-actions)
   (persistent-action :initform 'helm-grep-persistent-action)
   (persistent-help :initform "Jump to line (`C-u' Record in mark ring)")
   (requires-pattern :initform 2)
   (before-init-hook :initform 'helm-grep-before-init-hook)
   (after-init-hook :initform 'helm-grep-after-init-hook)
   (find-file-target :initform #'helm-grep-quit-an-find-file-fn)
   (group :initform 'helm-grep)))

(defvar helm-source-grep nil)

(cl-defmethod helm--setup-source ((source helm-grep-class))
  (cl-call-next-method)
  (helm-aif (and helm-follow-mode-persistent
                 (if (eq (slot-value source 'backend) 'git)
                     helm-source-grep-git
                     helm-source-grep))
      (setf (slot-value source 'follow)
            (assoc-default 'follow it))))

(cl-defun helm-do-grep-1 (targets &optional recurse backend exts
                                  default-input input (source 'helm-source-grep))
  "Launch helm using backend BACKEND on a list of TARGETS files.

When RECURSE is given and BACKEND is \\='grep' use -r option of
BACKEND and prompt user for EXTS to set the --include args of
BACKEND.
Interactively you can give more than one arg separated by space
at prompt.
E.g.:
    $Pattern: *.el *.py *.tex

From Lisp use the EXTS argument as a list of extensions as above.
If you are using ack-grep, you will be prompted for --type
instead and EXTS will be ignored.  If prompt is empty
`helm-grep-ignored-files' are added to --exclude.

Argument DEFAULT-INPUT is use as `default' arg of `helm' and
INPUT is used as `input' arg of `helm'.  See `helm' docstring.

Arg BACKEND when non-nil specifies which backend to use.
It is used actually to specify \\='zgrep' or \\='git'.
When BACKEND \\='zgrep' is used don't prompt for a choice in
recurse, and ignore EXTS, search being made recursively on files
matching `helm-zgrep-file-extension-regexp' only."
  (let* (non-essential
         (ack-rec-p (helm-grep-use-ack-p :where 'recursive))
         (exts (and recurse
                    ;; [FIXME] I could handle this from helm-walk-directory.
                    (not (eq backend 'zgrep)) ; zgrep doesn't handle -r opt.
                    (not ack-rec-p)
                    (or exts (helm-grep-get-file-extensions targets))))
         (include-files
          (and exts
               (mapconcat (lambda (x)
                            (concat "--include="
                                    (shell-quote-argument x)))
                          (if (> (length exts) 1)
                              (remove "*" exts)
                              exts) " ")))
         (types (and (not include-files)
                     (not (eq backend 'zgrep))
                     recurse
                     ack-rec-p
                     ;; When %e format spec is not specified
                     ;; ignore types and do not prompt for choice.
                     (string-match "%e" helm-grep-default-command)
                     (helm-grep-read-ack-type)))
         (src-name (capitalize (helm-grep-command recurse backend)))
         (com (cond ((eq backend 'zgrep) helm-default-zgrep-command)
                    ((eq backend 'git) helm-grep-git-grep-command)
                    (recurse helm-grep-default-recurse-command)
                    ;; When resuming, the local value of
                    ;; `helm-grep-default-command' is used, only git-grep
                    ;; should need this.
                    (t helm-grep-default-command))))
    ;; When called as action from an other source e.g *-find-files
    ;; we have to kill action buffer.
    (when (get-buffer helm-action-buffer)
      (kill-buffer helm-action-buffer))
    ;; If `helm-find-files' haven't already started,
    ;; give a default value to `helm-ff-default-directory'
    ;; and set locally `default-directory' to this value . See below [1].
    (unless helm-ff-default-directory
      (setq helm-ff-default-directory default-directory))
    ;; We need to store these vars locally
    ;; to pass infos later to `helm-resume'.
    (helm-set-local-variable
     'helm-zgrep-recurse-flag (and recurse (eq backend 'zgrep))
     'helm-grep-last-targets targets
     'helm-grep-include-files (or include-files types)
     'helm-grep-in-recurse recurse
     'helm-grep-use-zgrep (eq backend 'zgrep)
     'helm-grep-default-command com
     'helm-input-idle-delay helm-grep-input-idle-delay
     'default-directory helm-ff-default-directory) ;; [1]
    ;; Setup the source.
    (set source (helm-make-source src-name 'helm-grep-class
                  :backend backend
                  :pcre (string-match-p "\\`ack" com)))
    (helm
     :sources source
     :buffer (format "*helm %s*" (helm-grep-command recurse backend))
     :default default-input
     :input input
     :keymap helm-grep-map
     :history 'helm-grep-history
     :truncate-lines helm-grep-truncate-lines)))


;;; zgrep
;;
;;
(defun helm-ff-zgrep-1 (flist recursive)
  (unwind-protect
       (let* ((def-dir (or helm-ff-default-directory
                           default-directory))
              (only    (if recursive
                           (or (gethash def-dir helm-rzgrep-cache)
                               (puthash
                                def-dir
                                (helm-walk-directory
                                 def-dir
                                 :directories nil
                                 :path 'full
                                 :match helm-zgrep-file-extension-regexp)
                                helm-rzgrep-cache))
                         flist)))
         (helm-do-grep-1 only recursive 'zgrep))
    (setq helm-zgrep-recurse-flag nil)))


;;; transformers
;;
;;
(defun helm-grep-split-line (line)
  "Split a grep output line."
  ;; The output of grep may send a truncated line in this chunk,
  ;; so don't split until grep line is valid, that is
  ;; once the second part of the line comes with next chunk
  ;; send by process.
  (when (string-match helm-grep-split-line-regexp line)
    ;; Don't use split-string because buffer/file name or string
    ;; may contain a ":".
    (cl-loop for n from 1 to 3 collect (match-string n line))))

(defun helm-grep--filter-candidate-1 (candidate &optional dir pcre)
  (let* ((root   (or dir (and helm-grep-default-directory-fn
                              (funcall helm-grep-default-directory-fn))))
         (ansi-p (string-match-p helm--ansi-color-regexp candidate))
         (line   (if ansi-p (ansi-color-apply candidate) candidate))
         (split  (helm-grep-split-line line))
         (fname  (if (and root split)
                     ;; Filename should always be provided as a local
                     ;; path, if the root directory is remote, the
                     ;; tramp prefix will be added before executing
                     ;; action, see `helm-grep-action' and Bug#2032.
                     (expand-file-name (car split)
                                       (or (file-remote-p root 'localname)
                                           root))
                   (car-safe split)))
         (lineno (nth 1 split))
         (str    (nth 2 split))
         (display-fname (cl-ecase helm-grep-file-path-style
                          (basename (and fname (file-name-nondirectory fname)))
                          (absolute fname)
                          (relative (and fname root
                                         (file-relative-name fname root))))))
    (if (and display-fname lineno str)
        (cons (concat (propertize display-fname
                                  'face 'helm-grep-file
                                  'help-echo (abbreviate-file-name fname)
                                  'helm-grep-fname fname)
                      ":"
                      (propertize lineno 'face 'helm-grep-lineno)
                      ":"
                      (if ansi-p str (helm-grep-highlight-match str pcre)))
              line)
        "")))

(defun helm-grep-filter-one-by-one (candidate &optional pcre)
  "`filter-one-by-one' transformer function for `helm-do-grep-1'."
  (let ((helm-grep-default-directory-fn
         (or helm-grep-default-directory-fn
             (lambda () (or helm-ff-default-directory
                            (and helm-alive-p
                                 (helm-default-directory))
                            default-directory)))))
    (if (consp candidate)
        ;; Already computed do nothing (default as input).
        candidate
        (and (stringp candidate)
             (helm-grep--filter-candidate-1 candidate nil pcre)))))

(defun helm-grep-fc-transformer (candidates source)
  (let ((helm-grep-default-directory-fn
         (or helm-grep-default-directory-fn
             (lambda () (or helm-ff-default-directory
                            (and (null (eq major-mode 'helm-grep-mode))
                                 (helm-default-directory))
                            default-directory))))
        (pcre (helm-get-attr 'pcre source)))
    (cl-loop for c in candidates
             collect (helm-grep--filter-candidate-1 c nil pcre))))

(defun helm-grep-highlight-match (str &optional pcre)
  "Highlight in string STR all occurences matching `helm-pattern'."
  (let (beg end)
    (condition-case-unless-debug nil
        (with-temp-buffer
          (insert (propertize str 'read-only nil)) ; Fix bug#1176
          (goto-char (point-min))
          (cl-loop for reg in
                   (cl-loop for r in (helm-mm-split-pattern
                                      helm-input)
                            unless (string-match "\\`!" r)
                            collect
                            (helm-aif (and helm-migemo-mode
                                           (assoc r helm-mm--previous-migemo-info))
                                (cdr it) r))
                   do
                   (while (and (re-search-forward
                                (if pcre
                                    (helm--translate-pcre-to-elisp reg)
                                  reg)
                                nil t)
                               (> (- (setq end (match-end 0))
                                     (setq beg (match-beginning 0)))
                                  0))
                     (helm-add-face-text-properties beg end 'helm-grep-match))
                   do (goto-char (point-min)))
          (buffer-string))
      (error nil))))


;;; Grep from buffer list
;;
;;
(defun helm-grep-buffers-1 (candidate &optional zgrep)
  "Run grep on all file buffers or CANDIDATE if it is a file buffer.
If one of selected buffers is not a file buffer, it is ignored
and grep will run on all others file-buffers.
If only one candidate is selected and it is not a file buffer,
switch to this buffer and run `helm-occur'.
If a prefix arg is given run grep on all buffers ignoring
non-file buffers."
  (let* ((prefarg (or current-prefix-arg helm-current-prefix-arg))
         (helm-ff-default-directory
          (if (and helm-ff-default-directory
                   (file-remote-p helm-ff-default-directory))
              default-directory
            helm-ff-default-directory))
         (cands (if prefarg
                    (buffer-list)
                  (helm-marked-candidates)))
         (win-conf (current-window-configuration))
         ;; Non--fname and remote buffers are ignored.
         (bufs (cl-loop for buf in cands
                     for fname = (buffer-file-name (get-buffer buf))
                     when (and fname (not (file-remote-p fname)))
                     collect (expand-file-name fname))))
    (if bufs
        (if zgrep
            (helm-do-grep-1 bufs nil 'zgrep)
          (helm-do-grep-1 bufs))
      ;; bufs is empty, thats mean we have only CANDIDATE
      ;; and it is not a buffer-filename, fallback to occur.
      (switch-to-buffer candidate)
      (when (get-buffer helm-action-buffer)
        (kill-buffer helm-action-buffer))
      (helm-occur)
      (when (eq helm-exit-status 1)
        (set-window-configuration win-conf)))))

(defun helm-grep-buffers (candidate)
  "Action to grep buffers."
  (helm-grep-buffers-1 candidate))

(defun helm-zgrep-buffers (candidate)
  "Action to zgrep buffers."
  (helm-grep-buffers-1 candidate 'zgrep))


;;; Helm interface for pdfgrep
;;  pdfgrep program <http://pdfgrep.sourceforge.net/>
;;  and a pdf-reader (e.g xpdf) are needed.
;;
(defvar helm-pdfgrep-default-function 'helm-pdfgrep-init)
(defun helm-pdfgrep-init (only-files &optional recurse)
  "Start an asynchronous pdfgrep process in ONLY-FILES list."
  (let* ((default-directory (or helm-ff-default-directory
                                default-directory))
         (fnargs   (helm-grep-prepare-candidates
                    (if (file-remote-p default-directory)
                        (mapcar (lambda (x)
                                    (file-remote-p x 'localname))
                                only-files)
                      only-files)
                    default-directory))
         (cmd-line (format (if recurse
                               helm-pdfgrep-default-recurse-command
                               helm-pdfgrep-default-command)
                           helm-pattern
                           fnargs))
         process-connection-type)
    ;; Start pdf grep process.
    (helm-log "helm-pdfgrep-init" "Starting Pdf Grep process in directory `%s'" default-directory)
    (helm-log "helm-pdfgrep-init" "Command line used was:\n\n%s"
              (concat ">>> " (propertize cmd-line 'face 'helm-grep-cmd-line) "\n\n"))
    (prog1
        (start-file-process-shell-command
         "pdfgrep" helm-buffer cmd-line)
      (message nil)
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       (lambda (_process event)
           (if (string= event "finished\n")
               (with-helm-window
                 (setq mode-line-format
                       '(" " mode-line-buffer-identification " "
                         (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                         (:eval (propertize
                                 (format "[Pdfgrep Process Finish - %s result(s)] "
                                         (max (1- (count-lines
                                                   (point-min) (point-max))) 0))
                                 'face 'helm-grep-finish))))
                 (force-mode-line-update)
                 (when helm-allow-mouse
                   (helm--bind-mouse-for-selection helm-selection-point)))
             (helm-log "helm-pdfgrep-init" "Error: Pdf grep %s"
                       (replace-regexp-in-string "\n" "" event))))))))

(defun helm-do-pdfgrep-1 (only &optional recurse)
  "Launch pdfgrep with a list of ONLY files."
  (unless (executable-find "pdfgrep")
    (error "Error: No such program `pdfgrep'."))
  (let (helm-grep-in-recurse) ; recursion is implemented differently in *pdfgrep.
    ;; When called as action from an other source e.g *-find-files
    ;; we have to kill action buffer.
    (when (get-buffer helm-action-buffer)
      (kill-buffer helm-action-buffer))
    (setq helm-pdfgrep-targets only)
    (helm
     :sources (helm-build-async-source "PdfGrep"
                :init (lambda ()
                        ;; If `helm-find-files' haven't already started,
                        ;; give a default value to `helm-ff-default-directory'.
                        (setq helm-ff-default-directory (or helm-ff-default-directory
                                                            default-directory)))
                :candidates-process (lambda ()
                                      (funcall helm-pdfgrep-default-function
                                               helm-pdfgrep-targets recurse))
                :nohighlight t
                :nomark t
                :filter-one-by-one #'helm-grep-filter-one-by-one
                :candidate-number-limit 9999
                :history 'helm-grep-history
                :keymap helm-pdfgrep-map
                :help-message 'helm-pdfgrep-help-message
                :action #'helm-pdfgrep-action
                :persistent-help "Jump to PDF Page"
                :requires-pattern 2)
     :buffer "*helm pdfgrep*"
     :history 'helm-grep-history)))

(defun helm-pdfgrep-action (candidate)
  (helm-grep-action candidate 'pdf))

(defun helm-pdfgrep-action-1 (_split pageno fname)
  (save-selected-window
    (start-file-process-shell-command
     "pdf-reader" nil
     (format-spec helm-pdfgrep-default-read-command
                  (list (cons ?f fname) (cons ?p pageno))))))

;;; AG - PT - RG
;;
;;  https://github.com/ggreer/the_silver_searcher
;;  https://github.com/monochromegane/the_platinum_searcher
;;  https://github.com/BurntSushi/ripgrep

(defun helm-grep--ag-command ()
  (car (helm-remove-if-match
        "\\`[A-Z]*=" (split-string helm-grep-ag-command))))

(defun helm-grep-ag-get-types ()
  "Returns a list of AG types if available with AG version.
See AG option \"--list-file-types\"
Ripgrep (rg) types are also supported if this backend is used."
  (with-temp-buffer
    (let* ((com (helm-grep--ag-command))
           (ripgrep (string= com "rg"))
           (regex (if ripgrep "^\\(.*\\):" "^ *\\(--[a-z]*\\)"))
           (prefix (if ripgrep "-t " "")))
      (when (equal (call-process com
                                 nil t nil
                                 (if ripgrep
                                     "--type-list" "--list-file-types")) 0)
        (goto-char (point-min))
        (cl-loop while (re-search-forward regex nil t)
                 for type = (match-string 1)
                 collect (cons type (concat prefix type)))))))

(defun helm-grep-ag-prepare-cmd-line (pattern directory &optional type)
  "Prepare AG command line to search PATTERN in DIRECTORY.
When TYPE is specified it is one of what `helm-grep-ag-get-types'
returns if available with current AG version."
  (let* ((patterns (helm-mm-split-pattern pattern t))
         (pipe-switches (mapconcat 'identity helm-grep-ag-pipe-cmd-switches " "))
         (pipe-cmd (helm-acase (helm-grep--ag-command)
                     (("ag" "pt")
                      (format "%s -S --color%s" it (concat " " pipe-switches)))
                     ("rg" (format "rg -N -S --color=%s%s"
                                   (when (string-match "--color=\\([a-z]+\\) "
                                                       helm-grep-ag-command)
                                     (match-string 1 helm-grep-ag-command))
                                   (concat " " pipe-switches)))))
         (cmd (format helm-grep-ag-command
                      (mapconcat 'identity type " ")
                      (shell-quote-argument (car patterns))
                      (shell-quote-argument directory))))
    (helm-aif (cdr patterns)
        (concat cmd (cl-loop for p in it concat
                             (format " | %s -- %s"
                                     pipe-cmd (shell-quote-argument p))))
      cmd)))

(defun helm-grep-ag-init (directory &optional type)
  "Start AG process in DIRECTORY maybe searching only files of type TYPE."
  (let ((default-directory (or helm-ff-default-directory
                               (helm-default-directory)
                               default-directory))
        (cmd-line (helm-grep-ag-prepare-cmd-line
                   helm-pattern (or (file-remote-p directory 'localname)
                                    directory)
                   type))
        (start-time (float-time))
        (proc-name (helm-grep--ag-command)))
    (set (make-local-variable 'helm-grep-last-cmd-line) cmd-line)
    (helm-log "helm-grep-ag-init" "Starting %s process in directory `%s'"
              proc-name directory)
    (helm-log "helm-grep-ag-init" "Command line used was:\n\n%s"
              (concat ">>> " cmd-line "\n\n"))
    (prog1
        (start-file-process-shell-command
         proc-name helm-buffer cmd-line)
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       (lambda (process event)
         (let* ((err      (process-exit-status process))
                (noresult (= err 1)))
           (cond (noresult
                  (with-helm-buffer
                    (insert (concat "* Exit with code 1, no result found,"
                                    " command line was:\n\n "
                                    (propertize helm-grep-last-cmd-line
                                                'face 'helm-grep-cmd-line)))
                    (setq mode-line-format
                          `(" " mode-line-buffer-identification " "
                            (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                            (:eval (propertize
                                    (format
                                     "[%s process finished - (no results)] "
                                     ,(upcase proc-name))
                                    'face 'helm-grep-finish))))))
                 ((string= event "finished\n")
                  (helm-log "helm-grep-ag-init" "%s process finished with %s results in %fs"
                              proc-name
                              (helm-get-candidate-number)
                              (- (float-time) start-time))
                  (helm-maybe-show-help-echo)
                  (with-helm-window
                    (setq mode-line-format
                          `(" " mode-line-buffer-identification " "
                            (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                            (:eval (propertize
                                    (format
                                     "[%s process finished in %.2fs - (%s results)] "
                                     ,(upcase proc-name)
                                     ,(- (float-time) start-time)
                                     (helm-get-candidate-number))
                                    'face 'helm-grep-finish))))
                    (force-mode-line-update)
                    (when helm-allow-mouse
                      (helm--bind-mouse-for-selection helm-selection-point))))
                 (t (helm-log
                     "helm-grep-ag-init"
                     "Error: %s %s"
                     proc-name
                     (replace-regexp-in-string "\n" "" event))))))))))

(defclass helm-grep-ag-class (helm-source-async)
  ((nohighlight :initform t)
   (pcre :initarg :pcre :initform t
         :documentation
         "  Backend is using pcre regexp engine when non--nil.")
   (keymap :initform 'helm-grep-map)
   (history :initform 'helm-grep-ag-history)
   (help-message :initform 'helm-grep-help-message)
   (filtered-candidate-transformer :initform #'helm-grep-fc-transformer)
   (persistent-action :initform 'helm-grep-persistent-action)
   (persistent-help :initform "Jump to line (`C-u' Record in mark ring)")
   (candidate-number-limit :initform 99999)
   (requires-pattern :initform 2)
   (nomark :initform t)
   (action :initform 'helm-grep-actions)
   (find-file-target :initform #'helm-grep-quit-an-find-file-fn)
   (group :initform 'helm-grep)))

(defvar helm-source-grep-ag nil)

(cl-defmethod helm--setup-source ((source helm-grep-ag-class))
  (cl-call-next-method)
  (helm-aif (and helm-follow-mode-persistent
                 helm-source-grep-ag
                 (assoc-default 'follow helm-source-grep-ag))
      (setf (slot-value source 'follow) it)))

(defun helm-grep-ag-1 (directory &optional type input)
  "Start helm ag in DIRECTORY maybe searching in files of type TYPE.
If INPUT is provided, use it as the search string."
  (setq helm-source-grep-ag
        (helm-make-source (upcase (helm-grep--ag-command)) 'helm-grep-ag-class
          :header-name (lambda (name)
                         (format "%s [%s]"
                                 name (abbreviate-file-name directory)))
          :candidates-process
          (lambda () (helm-grep-ag-init directory type))))
  (helm-set-local-variable 'helm-input-idle-delay helm-grep-input-idle-delay)
  (helm :sources 'helm-source-grep-ag
        :keymap helm-grep-map
        :history 'helm-grep-ag-history
        :input input
        :truncate-lines helm-grep-truncate-lines
        :buffer (format "*helm %s*" (helm-grep--ag-command))))

(defun helm-grep-ag (directory with-types)
  "Start grep AG in DIRECTORY.
When WITH-TYPES is non-nil provide completion on AG types."
  (require 'helm-adaptive)
  (let ((com (capitalize (helm-grep--ag-command))))
    (helm-grep-ag-1 directory
                    (helm-aif (and with-types
                                   (helm-grep-ag-get-types))
                        (helm-comp-read
                         (format "%s type: " com) it
                         :must-match t
                         :marked-candidates t
                         :fc-transformer 'helm-adaptive-sort
                         :buffer (format "*helm %s types*" com))))))

;;; Git grep
;;
;;
(defvar helm-source-grep-git nil)

(defun helm-grep-git-1 (directory &optional all default input)
  "Run git-grep on DIRECTORY.
If DIRECTORY is not inside or part of a git repo exit with error.
If optional arg ALL is non-nil grep the whole repo otherwise
start at DIRECTORY.
Arg DEFAULT is what you will have with `next-history-element',
arg INPUT is what you will have by default at prompt on startup."
  (require 'vc)
  (let* (helm-grep-default-recurse-command
         ;; Expand filename of each candidate with the git root dir.
         ;; The filename will be in the helm-grep-fname prop.
         (helm-grep-default-directory-fn (lambda ()
                                           (vc-find-root directory ".git")))
         (helm-ff-default-directory (funcall helm-grep-default-directory-fn)))
    (cl-assert helm-ff-default-directory nil "Not inside a Git repository")
    (helm-do-grep-1 (if all '("") `(,(expand-file-name directory)))
                    nil 'git nil default input 'helm-source-grep-git)))


;;;###autoload
(defun helm-do-grep-ag (arg)
  "Preconfigured `helm' for grepping with AG in `default-directory'.
With prefix arg prompt for type if available with your AG
version."
  (interactive "P")
  (require 'helm-files)
  (helm-grep-ag (expand-file-name default-directory) arg))

;;;###autoload
(defun helm-grep-do-git-grep (arg)
  "Preconfigured `helm' for git-grepping `default-directory'.
With a prefix arg ARG git-grep the whole repository."
  (interactive "P")
  (require 'helm-files)
  (helm-grep-git-1 default-directory arg))


(provide 'helm-grep)

;;; helm-grep.el ends here
