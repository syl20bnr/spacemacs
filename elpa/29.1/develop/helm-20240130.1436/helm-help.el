;;; helm-help.el --- Help messages for Helm. -*- lexical-binding: t -*-

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
(require 'helm)


(defgroup helm-help nil
  "Embedded help for `helm'."
  :group 'helm)

(defface helm-helper
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :inherit helm-header))
  "Face for Helm help string in minibuffer."
  :group 'helm-help)

(defvar helm-help--string-list '(helm-help-message
                                 helm-buffer-help-message
                                 helm-ff-help-message
                                 helm-read-file-name-help-message
                                 helm-generic-file-help-message
                                 helm-fd-help-message
                                 helm-grep-help-message
                                 helm-pdfgrep-help-message
                                 helm-etags-help-message
                                 helm-ucs-help-message
                                 helm-bookmark-help-message
                                 helm-esh-help-message
                                 helm-buffers-ido-virtual-help-message
                                 helm-moccur-help-message
                                 helm-top-help-message
                                 helm-M-x-help-message
                                 helm-imenu-help-message
                                 helm-colors-help-message
                                 helm-semantic-help-message
                                 helm-kmacro-help-message
                                 helm-kill-ring-help-message)
  "A list of help messages (strings) used by `helm-documentation'.")

(defvar helm-documentation-buffer-name "*helm documentation*")

;;;###autoload
(defun helm-documentation ()
  "Preconfigured `helm' for Helm documentation.
With a prefix arg refresh the documentation.

Find here the documentation of all documented sources."
  (interactive)
  (let ((buf (get-buffer-create helm-documentation-buffer-name)))
    (switch-to-buffer buf)
    (set-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (save-excursion
        (cl-loop for elm in helm-help--string-list
                 for str = (helm-interpret-value elm)
                 do (insert (substitute-command-keys str) "\n\n")))
      (org-mode))
    (setq buffer-read-only t)
    (view-mode)))


;;; Local help messages.

;;; `helm-buffer-list' help
;;
;;
(defvar helm-buffer-help-message
  "* Helm Buffer

** Tips

*** Completion

**** Major-mode

You can enter a partial major-mode name (e.g. lisp, sh) to narrow down buffers.
To specify the major-mode, prefix it with \"*\" e.g. \"*lisp\".

If you want to match all buffers but the ones with a specific major-mode
\(negation), prefix the major-mode with \"!\" e.g. \"*!lisp\".

If you want to specify more than one major-mode, separate them with \",\",
e.g. \"*!lisp,!sh,!fun\" lists all buffers but the ones in lisp-mode, sh-mode
and fundamental-mode.

Then enter a space followed by a pattern to narrow down to buffers matching this
pattern.

**** Search inside buffers

If you enter a space and a pattern prefixed by \"@\", Helm searches for text
matching this pattern *inside* the buffer (i.e. not in the name of the buffer).

Negation are supported i.e. \"!\".

When you specify more than one of such patterns, it will match
buffers with contents matching each of these patterns i.e. AND,
not OR.  That means that if you specify \"@foo @bar\" the contents
of buffer will have to be matched by foo AND bar.  If you specify
\"@foo @!bar\" it means the contents of the buffer have to be
matched by foo but NOT bar.

If you enter a pattern prefixed with an escaped \"@\", Helm searches for a
buffer matching \"@pattern\" but does not search inside the buffer.

**** Search by directory name

If you prefix the pattern with \"/\", Helm matches over the directory names
of the buffers.

This feature can be used to narrow down the search to one directory while
subsequent strings entered after a space match over the buffer name only.

Note that negation is not supported for matching on buffer filename.

Starting from Helm v1.6.8, you can specify more than one directory.

**** Fuzzy matching

`helm-buffers-fuzzy-matching' turns on fuzzy matching on buffer
names, but not on directory names or major modes.  A pattern
starting with \"^\" disables fuzzy matching and matching is done
litteraly IOW do not use regexps (\"^\" or whatever special
regexp character) when you want to fuzzy match.

**** Examples

With the following pattern

    \"*lisp ^helm @moc\"

Helm narrows down the list by selecting only the buffers that are in lisp mode,
start with \"helm\" and which content matches \"moc\".

Without the \"@\"

    \"*lisp ^helm moc\"

Helm looks for lisp mode buffers starting with \"helm\" and containing \"moc\"
in their name.

With this other pattern

    \"*!lisp !helm\"

Helm narrows down to buffers that are not in \"lisp\" mode and that do not match
\"helm\".

With this last pattern

    /helm/ w3

Helm narrows down to buffers that are in any \"helm\" subdirectory and
matching \"w3\".

*** Creating buffers

When creating a new buffer, use `\\[universal-argument]' to choose a mode from a
list.  This list is customizable, see `helm-buffers-favorite-modes'.

*** Killing buffers

You can kill buffers either one by one or all the marked buffers at once.

One kill-buffer command leaves Helm while the other is persistent.  Run the
persistent kill-buffer command either with the regular
`helm-execute-persistent-action' called with a prefix argument (`\\[universal-argument] \\<helm-map>\\[helm-execute-persistent-action]')
or with its specific command `helm-buffer-run-kill-persistent'.  See the
bindings below.

*** Switching to buffers

To switch to a buffer, press RET, to switch to a buffer in another window, select this buffer
and press \\<helm-buffer-map>\\[helm-buffer-switch-other-window], when called with a prefix arg
the buffer will be displayed vertically in other window.
If you mark more than one buffer, the marked buffers will be displayed in different windows.

*** Saving buffers

If buffer is associated to a file and is modified, it is by default colorized in orange,
see [[Meaning of colors and prefixes for buffers][Meaning of colors and prefixes for buffers]].
You can save these buffers with \\<helm-buffer-map>\\[helm-buffer-save-persistent].
If you want to save all these buffers, you can mark them with \\[helm-buffers-mark-similar-buffers]
and save them with \\[helm-buffer-save-persistent].  You can also do this in one step with
\\[helm-buffer-run-save-some-buffers].  Note that you will not be asked for confirmation.
  
*** Meaning of colors and prefixes for buffers

Remote buffers are prefixed with '@'.
Red        => Buffer's file was modified on disk by an external process.
Indianred2 => Buffer exists but its file has been deleted.
Orange     => Buffer is modified and not saved to disk.
Italic     => A non-file buffer.
Yellow     => Tramp archive buffer.

** Commands
\\<helm-buffer-map>
|Keys|Description|
|-------------+----------|
|\\[helm-buffer-run-zgrep]|Grep Buffer(s) works as zgrep too (`\\[universal-argument]' to grep all buffers but non-file buffers).
|\\[helm-buffers-run-occur]|Multi-Occur buffer or marked buffers (`\\[universal-argument]' to toggle force-searching current-buffer).
|\\[helm-buffer-switch-other-window]|Switch to other window.
|\\[helm-buffer-switch-other-frame]|Switch to other frame.
|\\[helm-buffers-run-browse-project]|Browse project from buffer.
|\\[helm-buffer-run-query-replace-regexp]|Query-replace-regexp in marked buffers.
|\\[helm-buffer-run-query-replace]|Query-replace in marked buffers.
|\\[helm-buffer-run-ediff]|Ediff current buffer with candidate.  With two marked buffers, ediff those buffers.
|\\[helm-buffer-run-ediff-merge]|Ediff-merge current buffer with candidate.  With two marked buffers, ediff-merge those buffers.
|\\[helm-buffer-diff-persistent]|Toggle Diff-buffer with saved file without leaving Helm.
|\\[helm-buffer-revert-persistent]|Revert buffer without leaving Helm.
|\\[helm-buffer-save-persistent]|Save buffer without leaving Helm.
|\\[helm-buffer-run-save-some-buffers]|Save all unsaved buffers.
|\\[helm-buffer-run-kill-buffers]|Delete marked buffers and leave Helm.
|\\[helm-buffer-run-kill-persistent]|Delete buffer without leaving Helm.
|\\[helm-buffer-run-rename-buffer]|Rename buffer.
|\\[helm-toggle-all-marks]|Toggle all marks.
|\\[helm-mark-all]|Mark all.
|\\[helm-toggle-buffers-details]|Toggle details.
|\\[helm-buffers-toggle-show-hidden-buffers]|Show hidden buffers.
|\\[helm-buffers-mark-similar-buffers]|Mark all buffers of the same type (color) as current buffer.")

;;; Find files help (`helm-find-files')
;;
;;
(defvar helm-ff-help-message
  "* Helm Find Files

** Tips

*** Navigation summary

For a better experience you can enable auto completion by setting
`helm-ff-auto-update-initial-value' to non-nil in your init file.  It is not
enabled by default to not confuse new users.

**** Navigate with arrow keys

You can use <right> and <left> arrows to go down or up one level, to disable
this customize `helm-ff-lynx-style-map'.
Note that using `setq' will NOT work.

**** Use `\\<helm-find-files-map>\\[helm-execute-persistent-action]' (persistent action) on a directory to go down one level

On a symlinked directory a prefix argument expands to its true name.

**** Use `\\<helm-find-files-map>\\[helm-find-files-up-one-level]' or `DEL' on a directory to go up one level

***** `DEL' behavior

`DEL' by default deletes char backward.

But when `helm-ff-DEL-up-one-level-maybe' is non nil `DEL' behaves
differently depending on the contents of helm-pattern.  It goes up one
level if the pattern is a directory ending with \"/\" or disables HFF
auto update and delete char backward if the pattern is a filename or
refers to a non existing path.  Going up one level can be disabled
if necessary by deleting \"/\" at the end of the pattern using
\\<helm-map>\\[backward-char] and \\[helm-delete-minibuffer-contents].

Note that when deleting char backward, Helm takes care of
disabling update giving you the opportunity to edit your pattern for
e.g. renaming a file or creating a new file or directory.
When `helm-ff-auto-update-initial-value' is non nil you may want to
disable it temporarily, see [[Toggle auto-completion][Toggle auto-completion]] for this.

**** Use `\\<helm-find-files-map>\\[helm-find-files-down-last-level]' to walk back the resulting tree of all the `\\<helm-find-files-map>\\[helm-find-files-up-one-level]' or DEL you did

The tree is reinitialized each time you browse a new tree with
`\\<helm-map>\\[helm-execute-persistent-action]' or by entering some pattern in the prompt.

**** `RET' behavior

It behaves differently depending on `helm-selection' (current candidate in helm-buffer):

- candidate basename is \".\" => Open it in dired.
- candidate is a directory    => Expand it.
- candidate is a file         => Open it.

If you have marked candidates and you press RET on a directory,
Helm will navigate to this directory.  If you want to exit with
RET with default action with these marked candidates, press RET a
second time while you are on the root of this directory e.g.
\"/home/you/dir/.\" or press RET on any file which is not a
directory.  You can also exit with default action at any moment
with `f1'.

Note that when copying, renaming, etc. from `helm-find-files' the
destination file is selected with `helm-read-file-name'.

**** `TAB' behavior

Normally `TAB' is bound to `helm-select-action' in helm-map which
display the action menu.

You can change this behavior by setting in `helm-find-files-map'
a new command for `TAB':

    (define-key helm-find-files-map (kbd \"C-i\") 'helm-ff-TAB)

It will then behave slighly differently depending of
`helm-selection':

- candidate basename is \".\"  => open the action menu.
- candidate is a directory     => expand it (behave as \\<helm-map>\\[helm-execute-persistent-action]).
- candidate is a file          => open action menu.

Called with a prefix arg open menu unconditionally.

*** Filter out files or directories

You can show files or directories only with respectively
\\<helm-find-files-map>\\[helm-ff-toggle-dirs-only] and \\<helm-find-files-map>\\[helm-ff-toggle-files-only].
These are toggle commands i.e. filter/show_all.
Changing directory disable filtering.

*** Sort directory contents

When listing a directory without narrowing its contents, i.e. when pattern ends with \"/\",
you can sort alphabetically, by newest or by size by using respectively
\\<helm-find-files-map>\\[helm-ff-sort-alpha], \\[helm-ff-sort-by-newest] or \\[helm-ff-sort-by-size].
NOTE:
When starting back narrowing i.e. entering something in minibuffer after \"/\" sorting is done
again with fuzzy sorting and no more with sorting methods previously selected.

You can use these sort functions only on files or directory,
see [[Filter out files or directories][Filter out files or directories]].
 
*** Find file at point

Helm uses `ffap' partially or completely to find file at point depending on the
value of `helm-ff-guess-ffap-filenames': if non-nil, support is complete
\(annoying), if nil, support is partial.

Note that when the variable
`helm-ff-allow-non-existing-file-at-point' is non nil Helm will
insert the filename at point even if file with this name doesn't
exists.  If non existing file at point ends with numbers prefixed
with \":\" the \":\" and numbers are stripped.

**** Find file at line number

When text at point is in the form of

    ~/elisp/helm/helm.el:1234

Helm finds this file at the indicated line number, here 1234.

**** Find URL at point

When a URL is found at point, Helm expands to that URL only.
Pressing `RET' opens that URL using `browse-url-browser-function'.

**** Find e-mail address at point

When an e-mail address is found at point, Helm expands to this e-mail address
prefixed with \"mailto:\".  Pressing `RET' opens a message buffer with that
e-mail address.

*** Quick pattern expansion

**** Enter `~/' at end of pattern to quickly reach home directory

**** Enter `/' at end of pattern to quickly reach the file system root

**** Enter `./' at end of pattern to quickly reach `default-directory'

\(As per its value at the beginning of the session.)

If you already are in the `default-directory' this will move the cursor to the top.

**** Enter `../' at end of pattern will reach upper directory, moving cursor to the top

This is different from using `\\<helm-find-files-map>\\[helm-find-files-up-one-level]' in that it moves
the cursor to the top instead of remaining on the previous subdir name.

**** Enter `..name/' at end of pattern to start a recursive search

It searches directories matching \"name\" under the current directory,
see the [[Recursive completion on subdirectories][Recursive completion on subdirectories]] section below for more details.

**** Any environment variable (e.g. `$HOME') at end of pattern gets expanded

**** Any valid filename yanked after pattern gets expanded

**** Special case: URL at point

The quick expansions do not take effect after end a URL, you must kill the
pattern first (`\\[helm-delete-minibuffer-contents]').

*** Helm-find-files supports fuzzy matching

It starts from the third character of the pattern.

For instance \"fob\" or \"fbr\" will complete \"foobar\" but \"fb\" needs a
third character in order to complete it.

*** Watch briefly files contents while navigating

You can use `\\[helm-execute-persistent-action]' on a filename for this, then:

- First hit expands to that filename in the Helm buffer.
- Second hit displays the buffer filename.
- Third hit kills the buffer filename.

Note: `\\[universal-argument] \\[helm-execute-persistent-action]' displays the buffer directly.

*** Browse images directories with `helm-follow-mode' and navigate up/down

Before Emacs-27 Helm was using image-dired that works with
external ImageMagick tools.  From Emacs-27 Helm use native
display of images with image-mode by default for Emacs-27 (see `helm-ff-display-image-native'),
this allows automatic resize when changing window size, zooming with `\\[helm-ff-increase-image-size-persistent]' and `\\[helm-ff-decrease-image-size-persistent]'
and rotate images as before.

You can also use `helm-follow-action-forward' and `helm-follow-action-backward' with
`\\[helm-follow-action-forward]' and `\\[helm-follow-action-backward]' respectively.
Note that these commands have different behavior when `helm-follow-mode'
is enabled (go to next/previous line only).

Use `\\[universal-argument] \\[helm-execute-persistent-action]' to display an image or kill its buffer.

TIP: Use `\\<helm-map>\\[helm-toggle-resplit-and-swap-windows]' and `\\[helm-enlarge-window]' to display Helm window vertically
and to enlarge it while viewing images.
Note this may not work with exotic Helm windows settings such as the ones in Spacemacs.

**** Show thumbnails

Helm use image-dired to show thumbnails on image files, you can
toggle the thumbnail view with \\<helm-find-files-map>`\\[helm-ff-toggle-thumbnails]'.

**** Launch a slideshow from marked files

Helm provides an action from `helm-find-files' that allows
running a slideshow on marked files.  Just mark image files and
launch slideshow from action menu, bindings are self documented
in mode-line.  NOTE: When hitting any other keys than the ones
mentionned in mode-line, slideshow will come in pause, to restart
it you will have to press twice SPACE.

*** Open files externally

- Open file with external program (`\\<helm-find-files-map>\\[helm-ff-run-open-file-externally]',`C-u' to choose).

Helm is looking what is used by default to open file
externally (mailcap files) but have its own variable
`helm-external-programs-associations' to store external
applications.  If you call the action or its binding without
prefix arg Helm will see if there is an application suitable in
`helm-external-programs-associations', otherwise it will look in
mailcap files.  If you want to specify which external application
to use (and its options) use a prefix arg.

If you have to pass arguments after filename use `%s' in your command e.g. \"foo %s -a -b\"
If you want to detach your program from Emacs, you can use e.g. \"(foo %s &)\" (only supported on Linux/Unix).
When using `%s' do not quote it (i.e. \"%s\"), helm is already quoting filename argument.

Note: What you configure for Helm in `helm-external-programs-associations'
will take precedence on mailcap files.

- Preview file with external program (`\\[helm-ff-run-preview-file-externally]').

Same as above but doesn't quit Helm session, it is apersistent action.

- Open file externally with default tool (`\\[helm-ff-run-open-file-with-default-tool]').

Use `xdg-open' to open files.

*** Toggle auto-completion

It is useful when trying to create a new file or directory and you don't want
Helm to complete what you are writing.

Note: On a terminal, the default binding `C-<backspace>' may not work.
In this case use `C-c <backspace>'.

*** You can create a new directory and a new file at the same time

Simply write the path in the prompt and press `RET', e.g.
\"~/new/newnew/newnewnew/my_newfile.txt\".

*** To create a new directory, append a \"/\" to the new name and press `RET'

*** To create a new file, enter a filename not ending with \"/\"

Note that when you enter a new name, this one is prefixed with [+].

*** Recursive search from Helm-find-files

**** You can use Helm-browse-project (see binding below)

- With no prefix argument:
If the current directory is under version control with either git or hg and
helm-ls-git and/or helm-ls-hg are installed, it lists all the files under
version control.  Otherwise it falls back to Helm-find-files.  See
https://github.com/emacs-helm/helm-ls-git and
https://github.com/emacs-helm/helm-ls-hg.

- With one prefix argument:
List all the files under this directory and other subdirectories
\(recursion) and this list of files will be cached.

- With two prefix arguments:
Same but the cache is refreshed.

**** You can start a recursive search with \"locate\", \"find\" or [[https://github.com/sharkdp/fd][Fd]]

See \"Note\" in the [[Recursive completion on subdirectories][section on subdirectories]].

Using \"locate\", you can enable the local database with a prefix argument. If the
local database doesn't already exists, you will be prompted for its creation.
If it exists and you want to refresh it, give it two prefix args.

When using locate the Helm buffer remains empty until you type something.
Regardless Helm uses the basename of the pattern entered in the helm-find-files
session by default.  Hitting `\\[next-history-element]' should just kick in the
locate search with this pattern.  If you want Helm to automatically do this, add
`helm-source-locate' to `helm-sources-using-default-as-input'.

NOTE: On Windows use Everything with its command line ~es~ as a replacement of locate.
See [[https://github.com/emacs-helm/helm/wiki/Locate#windows][Locate on Windows]]

**** Recursive completion on subdirectories

Starting from the directory you are currently browsing, it is possible to have
completion of all directories underneath.  Say you are at \"/home/you/foo/\" and
you want to go to \"/home/you/foo/bar/baz/somewhere/else\", simply type
\"/home/you/foo/..else\" and hit `\\[helm-execute-persistent-action]' or enter
the final \"/\".  Helm will then list all possible directories under \"foo\"
matching \"else\".

Note: Completion on subdirectories uses \"locate\" as backend, you can configure
the command with `helm-locate-recursive-dirs-command'.  Because this completion
uses an index, the directory tree displayed may be out-of-date and not reflect
the latest change until you update the index (using \"updatedb\" for \"locate\").

If for some reason you cannot use an index, the \"find\" command from
\"findutils\" can be used instead.  It will be slower though.  You need to pass
the basedir as first argument of \"find\" and the subdir as the value for
'-(i)regex' or '-(i)name' with the two format specs that are mandatory in
`helm-locate-recursive-dirs-command'.

Examples:
- \"find %s -type d -name '*%s*'\"
- \"find %s -type d -regex .*%s.*$\"

[[https://github.com/sharkdp/fd][Fd]] command is now also
supported which is regexp based and very fast.  Here is the command
line to use:

- \"fd --hidden --type d .*%s.*$ %s\"

You can use also a glob based search, in this case use the --glob option:

- \"fd --hidden --type d --glob '*%s*' %s\"

*** Insert filename at point or complete filename at point

On insertion (i.e. there is nothing at point):

- `\\[helm-ff-run-complete-fn-at-point]': insert absolute file name.
- `\\[universal-argument] \\[helm-ff-run-complete-fn-at-point]': insert abbreviated file name.
- `\\[universal-argument] \\[universal-argument] \\[helm-ff-run-complete-fn-at-point]': insert relative file name.
- `\\[universal-argument] \\[universal-argument] \\[universal-argument] \\[helm-ff-run-complete-fn-at-point]': insert basename.

On completion (\\[helm-ff-run-complete-fn-at-point]):

- Target starts with \"~/\": insert abbreviate file name.
- target starts with \"/\" or \"[a-z]:/\": insert full path.
- Otherwise: insert relative file name.

*** Use the wildcard to select multiple files

Use of wildcard is supported to run an action over a set of files.

Example: You can copy all the files with \".el\" extension by using \"*.el\" and
then run copy action.

Similarly, \"**.el\" (note the two stars) will recursively select all \".el\"
files under the current directory.

Note that when recursively copying files, you may have files with same name
dispatched across different subdirectories, so when copying them in the same
directory they will get overwritten.  To avoid this Helm has a special action
called \"backup files\" that has the same behavior as the command line \"cp -f
--backup=numbered\": it allows you to copy many files with the same name from
different subdirectories into one directory.  Files with same name are renamed
as follows: \"foo.txt.~1~\".  Like with the --force option of cp, it is possible
to backup files in current directory.

This command is available only when `dired-async-mode' is active.

When using an action that involves an external backend (e.g. grep), using \"**\"
is not recommended (even thought it works fine) because it will be slower to
select all the files.  You are better off leaving the backend to do it, it will
be faster.  However, if you know you have not many files it is reasonable to use
this, also using not recursive wildcard (e.g. \"*.el\") is perfectly fine for
this.

The \"**\" feature is active by default in the option `helm-file-globstar'.  It
is different from the Bash \"shopt globstar\" feature in that to list files with
a named extension recursively you would write \"**.el\" whereas in Bash it would
be \"**/*.el\".  Directory selection with \"**/\" like Bash \"shopt globstar\"
option is not supported yet.

Helm supports different styles of wildcards:

- `sh' style, the ones supported by `file-expand-wildcards'.
e.g. \"*.el\", \"*.[ch]\" which match respectively all \".el\"
files or all \".c\" and \".h\" files.

- `bash' style (partially) In addition to what allowed in `sh'
style you can specify file extensions that have more than one
character like this: \"*.{sh,py}\" which match \".sh\" and
\".py\" files.

Of course in both styles you can specify one or two \"*\".

*** Query replace regexp on filenames

Replace different parts of a file basename with something else.

When calling this action you will be prompted twice as with
`query-replace', first for the matching expression of the text to
replace and second for the replacement text.  Several facilities,
however, are provided to make the two prompts more powerfull.

**** Syntax of the first prompt

In addition to simple regexps, these shortcuts are available:

- Basename without extension => \"%.\"
- Only extension             => \".%\"
- Substring                  => \"%:<from>:<to>\"
- Whole basename             => \"%\"

**** Syntax of the second prompt

In addition to a simple string to use as replacement, here is what you can use:

- A placeholder refering to what you have selected in the first prompt: \"\\@\".

After this placeholder you can use a search-and-replace syntax à-la sed:

    \"\\@/<regexp>/<replacement>/

You can select a substring from the string represented by the placeholder:

    \"\\@:<from>:<to>\"

- A special character representing a number which is incremented: \"\\#\".

- Shortcuts for `upcase', `downcase' and `capitalize'
are available as`%u', `%d' and `%c' respectively.

**** Examples

***** Recursively rename all files with \".JPG\" extension to \".jpg\"

Use the `helm-file-globstar' feature described in [[Use the wildcard to select multiple files][recursive globbing]]
by entering \"**.JPG\" at the end of the Helm-find-files pattern, then hit
\\<helm-find-files-map>\\[helm-ff-run-query-replace-fnames-on-marked] and enter \"JPG\" on first prompt, then \"jpg\" on second prompt and hit `RET'.

Alternatively you can enter \".%\" at the first prompt, then \"jpg\" and hit
`RET'.  Note that when using this instead of using \"JPG\" at the first prompt,
all extensions will be renamed to \"jpg\" even if the extension of one of the
files is, say, \"png\".  If you want to keep the original extension you can use
\"%d\" at the second prompt (downcase).

***** Batch-rename files from number 001 to 00x

Use \"\\#\" inside the second prompt.

Example 1: To rename the files

    foo.jpg
    bar.jpg
    baz.jpg

to

    foo-001.jpg
    foo-002.jpg
    foo-003.jpg

use \"%.\" as matching regexp and \"foo-\\#\" as replacement string.

Example 2: To rename the files

    foo.jpg
    bar.jpg
    baz.jpg

to

    foo-001.jpg
    bar-002.jpg
    baz-003.jpg

use as matching regexp \"%.\" and as replacement string \"\\@-\\#\".

***** Replace a substring

Use \"%:<from>:<to>\".

Example: To rename files

    foo.jpg
    bar.jpg
    baz.jpg

to

    fOo.jpg
    bAr.jpg
    bAz.jpg

use as matching regexp \"%:1:2\" and as replacement string \"%u\" (upcase).

Note that you \*cannot* use \"%.\" and \".%\" along with substring replacement.

***** Modify the string from the placeholder (\\@)

- By substring, i.e. only using the substring of the placeholder: \"\\@:<from>:<to>\".
The length of placeholder is used for <to> when unspecified.

Example 1: \"\\@:0:2\" replaces from the beginning to the second char of the placeholder.

Example 2: \\@:2: replaces from the second char of the placeholder to the end.

- By search-and-replace: \"\\@/<regexp>/<replacement>/\".

Incremental replacement is also handled in <replacement>.

Example 3: \"\\@/foo/bar/\" replaces \"foo\" by \"bar\" in the placeholder.

Example 4: \"\\@/foo/-\\#/\" replaces \"foo\" in the placeholder by 001, 002, etc.

***** Clash in replacements (avoid overwriting files)

When performing any of these replacement operations you may end up with same
names as replacement.  In such cases Helm numbers the file that would otherwise
overwritten.  For instance, should you remove the \"-m<n>\" part from the files
\"emacs-m1.txt\", \"emacs-m2.txt\" and \"emacs-m3.txt\" you would end up with
three files named \"emacs.txt\", the second renaming overwriting first file, and
the third renaming overwriting second file and so on.  Instead Helm will
automatically rename the second and third files as \"emacs(1).txt\" and
\"emacs(2).txt\" respectively.

***** Query-replace on filenames vs. serial-rename action

Unlike the [[Serial renaming][serial rename]] actions, the files renamed with
the query-replace action stay in their initial directory and are not moved to
the current directory.  As such, using \"\\#\" to serial-rename files only makes
sense for files inside the same directory.  It even keeps renaming files
with an incremental number in the next directories.

*** Serial renaming

You can use the serial-rename actions to rename, copy or symlink marked files to
a specific directory or in the current directory with all the files numbered
incrementally.

- Serial-rename by renaming:
Rename all marked files with incremental numbering to a specific directory.

- Serial-rename by copying:
Copy all marked files with incremental numbering to a specific directory.

- Serial-rename by symlinking:
Symlink all marked files with incremental numbering to a specific directory.

*** Edit marked files in a dired buffer

You can open a dired buffer containing only marked files with `\\<helm-find-files-map>\\[helm-ff-run-marked-files-in-dired]'.
With a prefix argument you can open this same dired buffer in wdired mode for
editing.  Note that wildcards are supported as well, so you can use e.g.
\"*.txt\" to select all \".txt\" files in the current directory or \"**.txt\" to
select all files recursively from the current directory.
See [[Use the wildcard to select multiple files]] section above.

*** Defining default target directory for copying, renaming, etc

You can customize `helm-dwim-target' to behave differently depending on the
windows open in the current frame.  Default is to provide completion on all
directories associated to each window.

*** Copying/Renaming from or to remote directories

Never use ssh tramp method to copy/rename large files, use
instead its scp method if you want to avoid out of memory
problems and crash Emacs or the whole system.  Moreover when using
scp method, you will hit a bug when copying more than 3 files at
the time, see [[https://github.com/emacs-helm/helm/issues/1945][bug#1945]].
The best way actually is using Rsync to copy files from or to
remote, see [[Use Rsync to copy files][Use Rsync to copy files]].
Also if you often work on remote you may consider using SSHFS
instead of relying on tramp.

*** Copying and renaming asynchronously

If you have the async library installed (if you got Helm from MELPA you do), you
can use it for copying/renaming files by enabling `dired-async-mode'.

Note that even when async is enabled, running a copy/rename action with a prefix
argument will execute action synchronously. Moreover it will follow the first
file of the marked files in its destination directory.

When `dired-async-mode' is enabled, an additional action named \"Backup files\"
will be available. (Such command is not natively available in Emacs).
See [[Use the wildcard to select multiple files]] for details.

*** Multiple copies of a file

The command \\<helm-find-files-map>\\[helm-ff-run-mcp] allows
copying a single file to multiple directories. To use it, mark
the file you want to copy first and then mark the directories
where you want to copy file.  For example if you run
\\[helm-ff-run-mcp] on the marked candidates '(\"foo.txt\" \"bar/\" \"baz\"),
\"foo.txt\" will be copied to directories \"bar/\" and \"baz/\".

*** Use Rsync to copy files

If Rsync is available, you can use it to copy/sync files or directories
with some restrictions though:

- Copying from/to tramp sudo method may not work (permissions).
- Copying from remote to remote is not supported (rsync restriction)
however you can mount a remote with sshfs and copy to it (best), otherwise you have to modify
the command line with a prefix arg, see [[https://unix.stackexchange.com/questions/183504/how-to-rsync-files-between-two-remotes][how-to-rsync-files-between-two-remotes]]
for the command line to use.

This command is mostly useful when copying large files as it is
fast, asynchronous and provide a progress bar in mode-line.  Each
rsync process have its own progress bar, so you can run several
rsync jobs, they are independents.  If rsync fails you can
consult the \"*helm-rsync<n>*\" buffer to see rsync errors.  An
help-echo (move mouse over progress bar) is provided to see which
file is in transfer.  Note that when copying directories, no
trailing slashes are added to directory names, which mean that
directory is created on destination if it doesn't already exists,
see rsync documentation for more infos on rsync behavior.  To
synchronize a directory, mark all in the directory and rsync all
marked to the destination directory or rsync the directory itself
to its parent, e.g. remote:/home/you/music => /home/you.

The options are configurable through `helm-rsync-switches', but
you can modify them on the fly when needed by using a prefix arg,
in this case you will be prompted for modifications.

NOTE: When selecting a remote file, if you use the tramp syntax
for specifying a port, i.e. host#2222, helm will add
automatically \"-e 'ssh -p 2222'\" to the rsync command line
unless you have specified yourself the \"-e\" option by editing
rsync command line with a prefix arg (see above).

*** Access files on Android phones from Helm

Since Android doesn't provide anymore mass storage for USB, it is
not simple to access files on Android, the best way to do this
actually seems to use Adb, here some hints to set this up, read
in addition the Tramp documentation.

1) Install Adb, most distribution provide it.
2) Enable on your phone USB debug in System/dvlpmnt settings.
3) From helm-find-files use adb tramp method:
    /adb::/
From there you can navigate as usual, mark and copy files etc...

*** Bookmark the `helm-find-files' session

You can bookmark the `helm-find-files' session with `\\[helm-ff-bookmark-set]'.
You can later retrieve these bookmarks by calling `helm-filtered-bookmarks'
or, from the current `helm-find-files' session, by hitting `\\[helm-find-files-switch-to-bookmark]'.

*** Grep files from `helm-find-files'

You can grep individual files from `helm-find-files' by using
\`\\<helm-find-files-map>\\[helm-ff-run-grep]'.  This same command can also
recursively grep files from the current directory when called with a prefix
argument.  In this case you will be prompted for the file extensions to use
\(grep backend) or the types of files to use (ack-grep backend).  See the
`helm-grep-default-command' documentation to set this up.  For compressed files
or archives, use zgrep with \`\\<helm-find-files-map>\\[helm-ff-run-zgrep]'.

Otherwise you can use recursive commands like \`\\<helm-find-files-map>\\[helm-ff-run-grep-ag]' or `\\<helm-find-files-map>\\[helm-ff-run-git-grep]'
that are much faster than using `\\<helm-find-files-map>\\[helm-ff-run-grep]' with a prefix argument.
See `helm-grep-ag-command' and `helm-grep-git-grep-command' to set this up.

You can also use \"id-utils\"' GID with \`\\<helm-find-files-map>\\[helm-ff-run-gid]'
by creating an ID index file with the \"mkid\" shell command.

All those grep commands use the symbol at point as the default pattern.
Note that default is different from input (nothing is added to the prompt until
you hit `\\[next-history-element]').

**** Grepping on remote files

On remote files grep is not well supported by TRAMP unless you suspend updates before
entering the pattern and re-enable it once your pattern is ready.
To toggle suspend-update, use `\\<helm-map>\\[helm-toggle-suspend-update]'.

*** Execute Eshell commands on files

Setting up aliases in Eshell allows you to set up powerful customized commands.

Your aliases for using eshell command on file should allow
specifying one or more files, use e.g. \"alias foo $1\" or
\"alias foo $*\", if you want your command to be asynchronous add
at end \"&\", e.g. \"alias foo $* &\".

Adding Eshell aliases to your `eshell-aliases-file' or using the
`alias' command from Eshell allows you to create personalized
commands not available in `helm-find-files' actions and use them
from `\\<helm-find-files-map>\\[helm-ff-run-eshell-command-on-file]'.

Example: You want a command to uncompress some \"*.tar.gz\" files from `helm-find-files':

1) Create an Eshell alias named, say, \"untargz\" with the command
\"alias untargz tar zxvf $*\".

2) Now from `helm-find-files' select the \"*.tar.gz\" file (you can also
mark files if needed) and hit `\\<helm-find-files-map>\\[helm-ff-run-eshell-command-on-file]'.

Note: When using marked files with this, the meaning of the prefix argument is
quite subtle.  Say you have \"foo\", \"bar\" and \"baz\" marked; when you run
the alias command `example' on these files with no prefix argument it will run
`example' sequentially on each file:

$ example foo
$ example bar
$ example baz

With a prefix argument however it will apply `example' on all files at once:

$ example foo bar baz

Of course the alias command should support this.

NOTE: Helm assume that any alias command ending with '$*' or
'$*&' supports many files as arguments, so no need to give a
prefix arg for such alias, however if your command is not an
alias you have to specify a prefix arg if you want your command
to apply all files at once.

If you add %s to the command line %s will be replaced with the candidate, this mean you can
add extra argument to your command e.g. command -extra-arg %s or command %s -extra-arg.
If you want to pass many files inside %s, don't forget to use a prefix arg.

You can also use special placeholders in extra-args,
see the specific info page once you hit `\\<helm-find-files-map>\\[helm-ff-run-eshell-command-on-file]'.

*** Using TRAMP with `helm-find-files' to read remote directories

`helm-find-files' works fine with TRAMP despite some limitations.

- Grepping files is not very well supported when used incrementally.
See [[Grepping on remote files]].

- Locate does not work on remote directories.

**** A TRAMP syntax crash course

Please refer to TRAMP's documentation for more details.

- Connect to host 192.168.0.4 as user \"foo\":

/scp:192.168.0.4@foo:

- Connect to host 192.168.0.4 as user \"foo\" on port 2222:

/scp:192.168.0.4@foo#2222:

- Connect to host 192.168.0.4 as root using multihops syntax:

/ssh:192.168.0.4@foo|sudo:192.168.0.4:

Note: You can also use `tramp-default-proxies-alist' when connecting often to
the same hosts.

As a rule of thumb, prefer the scp method unless using multihops (which only
works with the ssh method), especially when copying large files.

IMPORTANT:
You need to hit `C-j' once on top of a directory on the first connection
to complete the pattern in the minibuffer.

**** Display color for directories, symlinks etc... with tramp

Starting at helm version 2.9.7 it is somewhat possible to
colorize fnames by listing files without loosing performances with
external commands (ls and awk) if your system is compatible.
For this you can use `helm-list-dir-external' as value
for `helm-list-directory-function'.

See `helm-list-directory-function' documentation for more infos.

**** Completing host

As soon as you enter the first \":\" after method e.g =/scp:= you will
have some completion about previously used hosts or from your =~/.ssh/config=
file, hitting `\\[helm-execute-persistent-action]' or `right' on a candidate will insert this host in minibuffer
without addind the ending \":\", second hit insert the last \":\".
As soon the last \":\" is entered TRAMP will kick in and you should see the list
of candidates soon after.

**** Completion on tramp methods

If you enter \":\" directly after \"/\" or \"|\" you will have completion on tramp methods,
hitting `\\[helm-execute-persistent-action]' or `right' on a method will insert it in minibuffer.
 
When connection fails, be sure to delete your TRAMP connection with M-x
`helm-delete-tramp-connection' before retrying.

**** Editing local files as root

Use the sudo method:

\"/sudo:host:\" or simply \"/sudo::\".

*** Attach files to a mail buffer (message-mode)

If you are in a `message-mode' or `mail-mode' buffer, that action will appear
in action menu, otherwise it is available at any time with \\<helm-find-files-map>\\[helm-ff-run-mail-attach-files].
It behaves as follows:

- If you are in a (mail or message) buffer, files are attached there.

- If you are not in a mail buffer but one or more mail buffers exist, you are
prompted to attach files to one of these mail buffers.

- If you are not in a mail buffer and no mail buffer exists,
a new mail buffer is created with the attached files in it.

*** Open files in separate windows

When [[Marked candidates][marking]] multiple files or using [[Use the wildcard to select multiple files][wildcard]], helm allow opening all
this files in separate windows using an horizontal layout or a
vertical layout if you used a prefix arg, when no more windows can be
displayed in frame, next files are opened in background without being
displayed.  When using \\<helm-find-files-map>\\[helm-ff-run-switch-other-window] the current
buffer is kept and files are displayed next to it with same behavior as above.
When using two prefix args, files are opened in background without beeing displayed.

*** Expand archives as directories in a avfs directory

If you have mounted your filesystem with 'mountavfs' command,
you can expand archives in the \"~/.avfs\" directory with \\<helm-map>\\[helm-execute-persistent-action].

To umount Avfs, use ~fusermount -u ~/.avfs~

NOTE: You need the package 'avfs', on debian like distros use ~apt-get install avfs~.

*** Tramp archive support (emacs-27+ only)

As Tramp archive often crash Helm and Emacs, Helm does its best
to disable it, however it is hard to do so as Tramp Archive is
enabled inconditionally in Emacs.  Here I build my Emacs
without-dbus to ensure Tramp archive wont kickin unexpectedly.

If you want to browse archives please use [[Expand archives as
directories in a avfs directory][Avfs]] which is much better and
stable.

*** Touch files

In the completion buffer, you can choose the default which is the current-time, it is
the first candidate or the timestamp of one of the selected files.
If you need to use something else, use \\<helm-map>\\[next-history-element] and edit
the date in minibuffer.
It is also a way to quickly create a new file without opening a buffer, saving it
and killing it.
To touch more than one new file, separate you filenames with a comma (\",\").
If one wants to create (touch) a new file with comma inside the name use a prefix arg,
this will prevent splitting the name and create multiple files.

*** Change mode on files (chmod)

When running `\\<helm-find-files-map>\\[helm-ff-run-chmod]' on
marked files, you can enter the new mode in prompt but you can
also use the first marked file as model to use it as default.
For example you can mark a file with mode 777 and mark other
files with mode 664, press 'RET' and answer 'y', all marked files
will be changed to 777.

NOTE: Another way to change modes on files in helm-find-files is
running `\\<helm-find-files-map>\\[helm-ff-run-switch-to-shell]' and use 'chmod' directly.

*** Delete files

You can delete files without quitting helm with
`\\<helm-find-files-map>\\[helm-ff-persistent-delete]' or delete files and quit helm with `\\[helm-ff-run-delete-file]'.

In the second method you can choose to
make this command asynchronous by customizing
\`helm-ff-delete-files-function'.

_WARNING_: When deleting files asynchronously you will NOT be
WARNED if directories are not empty, that's mean non empty directories will
be deleted recursively in background without asking.

A good compromise is to trash your files
when using asynchronous method (see [[Trashing files][Trashing files]]).

When choosing synchronous delete, you can allow recursive
deletion of directories with `helm-ff-allow-recursive-deletes'.
Note that when trashing (synchronous) you are not asked for recursive deletion.

Note that `helm-ff-allow-recursive-deletes' have no effect when
deleting asynchronously.

First method (persistent delete) is always synchronous.

Note that when a prefix arg is given, trashing behavior is inversed.
See [[Trashing files][Trashing files]].

**** Trashing files

If you want to trash your files instead of deleting them you can
set `delete-by-moving-to-trash' to non nil, like this your files
will be moved to trash instead of beeing deleted.

You can reverse at any time the behavior of `delete-by-moving-to-trash' by using
a prefix arg with any of the delete files command.

On GNULinux distributions, when navigating to a Trash directory you
can restore any file in ..Trash/files directory with the 'Restore
from trash' action you will find in action menu (needs the
trash-cli package installed for remote files, see [[Trashing remote files with tramp][Here]]).
You can as well delete files from Trash directories with the 'delete files from trash'
action.
If you want to know where a file will be restored, hit `M-i', you will find a trash info.

Tip: Navigate to your Trash/files directories with `helm-find-files' and set a bookmark
there with \\<helm-find-files-map>\\[helm-ff-bookmark-set] for fast access to Trash.

NOTE: Restoring files from trash is working only on system using
the
[[http://freedesktop.org/wiki/Specifications/trash-spec][freedesktop trash specifications]].

***** Trashing remote files with tramp

Trashing remote files (or local files with sudo method) is disabled by default
because tramp is requiring the 'trash' command to be installed, if you want to
trash your remote files, customize `helm-trash-remote-files'.
The package on most GNU/Linux based distributions is trash-cli, it is available [[https://github.com/andreafrancia/trash-cli][here]].

NOTE:
When deleting your files with sudo method, your trashed files will not be listed
with trash-list command line until you log in as root.

*** Checksum file

Checksum is calculated with the md5sum, sha1sum, sha224sum,
sha256sum, sha384sum and sha512sum commands when available, otherwise the
Emacs function `secure-hash' is used but it is slow and may crash
Emacs and even the whole system as it eats all memory.  So if
your system doesn't have the md5sum and sha*sum command line tools be
careful when checking sum of larges files e.g. isos.

*** Ignored or boring files

Helm-find-files can ignore files matching
`helm-boring-file-regexp-list' or files that are git ignored, you
can set this with `helm-ff-skip-boring-files' or
`helm-ff-skip-git-ignored-files'.
NOTE: This will slow down helm, be warned.

*** Helm-find-files is using a cache

Helm is caching each directory files list in a hash table for
faster search, when a directory is modified it is removed from cache
so that further visit in this directory refresh cache.
You may have in some rare cases to refresh directory manually with `\\<helm-map>\\[helm-refresh]'
for example when helm-find-files session is running and a file is modified/deleted
in current visited directory by an external command from outside Emacs.

NOTE: Helm is using file-notify to watch visited directories,
nowaday most systems come with a notify package but if your
system doesn't support this, you can turn off file notifications
by customizing the variable `helm-ff-use-notify'. In this case
you will have to refresh manually directories when needed with `\\<helm-map>\\[helm-refresh]'.

*** Prefix file candidates with icons

If `all-the-icons' package is installed, turning on
`helm-ff-icon-mode' will show icons before files and directories.

** Commands
\\<helm-find-files-map>
|Keys|Description
|-----------+----------|
|\\[helm-ff-run-locate]|Run `locate' (`\\[universal-argument]' to specify locate database, `M-n' to insert basename of candidate).
|\\[helm-ff-run-browse-project]|Browse project (`\\[universal-argument]' to recurse, `\\[universal-argument] \\[universal-argument]' to recurse and refresh database).
|\\[helm-ff-run-find-sh-command]|Run `find' shell command from this directory.
|\\[helm-ff-run-grep]|Run Grep (`\\[universal-argument]' to recurse).
|\\[helm-ff-run-pdfgrep]|Run Pdfgrep on marked files.
|\\[helm-ff-run-zgrep]|Run zgrep (`\\[universal-argument]' to recurse).
|\\[helm-ff-run-grep-ag]|Run AG grep on current directory.
|\\[helm-ff-run-git-grep]|Run git-grep on current directory.
|\\[helm-ff-run-gid]|Run gid (id-utils).
|\\[helm-ff-run-etags]|Run Etags (`\\[universal-argument]' to use thing-at-point, `\\[universal-argument] \\[universal-argument]' to reload cache).
|\\[helm-ff-run-rename-file]|Rename Files (`\\[universal-argument]' to follow).
|\\[helm-ff-run-query-replace-fnames-on-marked]|Query replace on marked files.
|\\[helm-ff-run-copy-file]|Copy Files (`\\[universal-argument]' to follow).
|\\[helm-ff-run-mcp]|Copy car of marked to remaining directories.
|\\[helm-ff-run-rsync-file]|Rsync Files (`\\[universal-argument]' to edit command).
|\\[helm-ff-run-byte-compile-file]|Byte Compile Files (`\\[universal-argument]' to load).
|\\[helm-ff-run-load-file]|Load Files.
|\\[helm-ff-run-symlink-file]|Symlink Files.
|\\[helm-ff-run-hardlink-file]|Hardlink files.
|\\[helm-ff-run-relsymlink-file]|Relative symlink Files.
|\\[helm-ff-run-chmod]|Change mode on Files.
|\\[helm-ff-run-delete-file]|Delete Files.
|\\[helm-ff-run-touch-files]|Touch files.
|\\[helm-ff-run-kill-buffer-persistent]|Kill buffer candidate without leaving Helm.
|\\[helm-ff-persistent-delete]|Delete file without leaving Helm.
|\\[helm-ff-run-switch-to-shell]|Switch to prefered shell.
|\\[helm-ff-run-eshell-command-on-file]|Eshell command on file (`\\[universal-argument]' to apply on marked files, otherwise treat them sequentially).
|\\[helm-ff-run-ediff-file]|Ediff file.
|\\[helm-ff-run-ediff-merge-file]|Ediff merge file.
|\\[helm-ff-run-complete-fn-at-point]|Complete file name at point.
|\\[helm-ff-run-switch-other-window]|Switch to other window.
|\\[helm-ff-run-switch-other-frame]|Switch to other frame.
|\\[helm-ff-run-open-file-externally]|Open file with external program (`\\[universal-argument]' to choose).
|\\[helm-ff-run-preview-file-externally]|Preview file with external program.
|\\[helm-ff-run-open-file-with-default-tool]|Open file externally with default tool.
|\\[helm-ff-rotate-left-persistent]|Rotate image left.
|\\[helm-ff-rotate-right-persistent]|Rotate image right.
|\\[helm-ff-increase-image-size-persistent]|Zoom in image.
|\\[helm-ff-decrease-image-size-persistent]|Zoom out image.
|\\[helm-ff-toggle-thumbnails]|Show thumbnails on image files.
|\\[helm-find-files-up-one-level]|Go to parent directory.
|\\[helm-find-files-history]|Switch to the visited-directory history.
|\\[helm-ff-file-name-history]|Switch to file name history.
|\\[helm-ff-properties-persistent]|Show file properties in a tooltip.
|\\[helm-mark-all]|Mark all visible candidates.
|\\[helm-ff-run-toggle-auto-update]|Toggle auto-expansion of directories.
|\\[helm-unmark-all]|Unmark all candidates, visible and invisible ones.
|\\[helm-ff-run-mail-attach-files]|Attach files to message buffer.
|\\[helm-ff-run-print-file]|Print file, (`\\[universal-argument]' to refresh printer list).
|\\[helm-enlarge-window]|Enlarge Helm window.
|\\[helm-narrow-window]|Narrow Helm window.
|\\[helm-ff-run-toggle-basename]|Toggle basename/fullpath.
|\\[helm-ff-run-find-file-as-root]|Find file as root.
|\\[helm-ff-run-find-alternate-file]|Find alternate file.
|\\[helm-ff-run-insert-org-link]|Insert org link.
|\\[helm-ff-bookmark-set]|Set bookmark to current directory.
|\\[helm-find-files-switch-to-bookmark]|Jump to bookmark list.
|\\[helm-ff-sort-alpha]|Sort alphabetically.
|\\[helm-ff-sort-by-newest]|Sort by newest.
|\\[helm-ff-sort-by-size]|Sort by size.
|\\[helm-ff-toggle-dirs-only]|Show only directories.
|\\[helm-ff-toggle-files-only]|Show only files.
|\\[helm-ff-sort-by-ext]|Sort by extensions.")

;;; Help for file-name-history
;;
;;
(defvar helm-file-name-history-help-message
  "* Helm file name history

** Tips
You can open directly the selected file and exit helm or preselect the file in helm-find-files,
see actions in action menu.

You can toggle the view of deleted files, see commands below.

** Commands
\\<helm-file-name-history-map>
\\[helm-file-name-history-show-or-hide-deleted]|Toggle deleted files view.")

;;; Help for `helm-read-file-name'
;;
;;
(defun helm-read-file-name-help-message ()
  (let ((name (if helm-alive-p
                  (assoc-default 'name (helm-get-current-source))
                "generic")))
    (format
     "* Helm `%s' read file name completion

This is `%s' read file name completion that have been \"helmized\"
because you have enabled [[Helm mode][helm-mode]].
Don't confuse this with `helm-find-files' which is a native helm command,
see [[Helm functions vs helmized Emacs functions]].

** Tips

*** Navigation

**** Enter `~/' at end of pattern to quickly reach home directory

**** Enter `/' at end of pattern to quickly reach the file system root

**** Enter `./' at end of pattern to quickly reach `default-directory'

\(As per its value at the beginning of the session.)

If you already are in the `default-directory' this will move the cursor to the top.

**** Enter `../' at end of pattern will reach upper directory, moving cursor on top

This is different from using `\\[helm-find-files-up-one-level]' in that it moves
the cursor to the top instead of remaining on the previous subdir name.

**** You can complete with partial basename

It starts from the third character of the pattern.

For instance \"fob\" or \"fbr\" will complete \"foobar\" but \"fb\" needs a
third character in order to complete it.

*** Persistent actions

By default `helm-read-file-name' uses the persistent actions of `helm-find-files'.

**** Use `\\[universal-argument] \\<helm-map>\\[helm-execute-persistent-action]' to display an image

**** `\\<helm-map>\\[helm-execute-persistent-action]' on a filename will expand to this filename in Helm-buffer

Second hit displays the buffer filename.
Third hit kills the buffer filename.
Note: `\\[universal-argument] \\<helm-map>\\[helm-execute-persistent-action]' displays the buffer directly.

**** Browse images directories with `helm-follow-mode' and navigate up/down

*** Delete characters backward

When you want to delete characters backward, e.g. to create a new file or directory,
auto-update may come in the way when it keeps updating to an existent directory.
In that case, type `C-<backspace>' and then `<backspace>'.
This should not be needed when copying/renaming files because autoupdate is disabled
by default in that case.

Note: On a terminal, the default binding `C-<backspace>' may not work.
In this case use `C-c <backspace>'.

*** Create new directories and files

**** You can create a new directory and a new file at the same time

Simply write the path in prompt and press `RET', e.g.
\"~/new/newnew/newnewnew/my_newfile.txt\".

**** To create a new directory, append a \"/\" at to the new name and press `RET'

**** To create a new file, enter a filename not ending with \"/\"

File and directory creation works only with some commands (e.g. `find-file')
and it will not work with others where it is not intended to return a file or
a directory \(e.g `list-directory').

*** Exiting minibuffer with empty string

You can exit minibuffer with empty string with \\<helm-read-file-map>\\[helm-cr-empty-string].
It is useful when some commands are prompting continuously until you enter an empty prompt.

** Commands
\\<helm-read-file-map>
|Keys|Description
|-----------+----------|
|\\[helm-find-files-up-one-level]|Go to parent directory.
|\\[helm-ff-run-toggle-auto-update]|Toggle auto-expansion of directories.
|\\[helm-ff-run-toggle-basename]|Toggle basename.
|\\[helm-ff-file-name-history]|File name history.
|C/\\[helm-cr-empty-string]|Return empty string unless `must-match' is non-nil.
|\\[helm-next-source]|Go to next source.
|\\[helm-previous-source]|Go to previous source."
     name name)))

;;; FD help
;;
;;
(defvar helm-fd-help-message
  "* Helm fd

** Tips

\[[https://github.com/sharkdp/fd][The Fd command line tool]] is very fast to search files recursively.
You may have to wait several seconds at first usage when your
hard drive cache is \"cold\", then once the cache is initialized
searchs are very fast.  You can pass any [[https://github.com/sharkdp/fd#command-line-options][Fd options]] before pattern, e.g. \"-e py foo\".

The [[https://github.com/sharkdp/fd][Fd]] command line can be customized with `helm-fd-switches' user variable.
Always use =--color always= as option otherwise you will have no colors.
To customize colors see [[https://github.com/sharkdp/fd#colorized-output][Fd colorized output]].

NOTE:
Starting from fd version 8.2.1, you have to provide the env var
LS_COLORS to Emacs to have a colorized output, the easiest way is
to add to your =~/.profile= file =eval $(dircolors)=.
Another way is using =setenv= in your init file.
This is not needed when running Emacs from a terminal either with =emacs -nw=
or =emacs= because emacs inherit the env vars of this terminal.
See [[https://github.com/sharkdp/fd/issues/725][fd bugref#725]]

Search is (pcre) regexp based (see [[https://docs.rs/regex/1.0.0/regex/#syntax][Regexp syntax]]), multi patterns are _not_ supported.

** Man page

NAME
       fd - find entries in the filesystem

SYNOPSIS
       fd  [-HIEsiaLp0hV]  [-d  depth] [-t filetype] [-e ext] [-E exclude] [-c
       when] [-j num] [-x cmd] [pattern] [path...]

DESCRIPTION
       fd is a simple, fast and user-friendly alternative to find(1).

OPTIONS
       -H, --hidden
              Include hidden files  and  directories  in  the  search  results
              (default: hidden files and directories are skipped).

       -I, --no-ignore
              Show search results from files and directories that would other‐
              wise be ignored by .gitignore, .ignore, .fdignore, or the global
              ignore file.

       -u, --unrestricted
              Alias  for '--no-ignore'. Can be repeated; '-uu' is an alias for
              '--no-ignore --hidden'.

       --no-ignore-vcs
              Show search results from files and directories that would other‐
              wise be ignored by .gitignore files.

       -s, --case-sensitive
              Perform a case-sensitive search. By default, fd uses case-insen‐
              sitive searches, unless the pattern contains an uppercase  char‐
              acter (smart case).

       -i, --ignore-case
              Perform  a  case-insensitive  search.  By default, fd uses case-
              insensitive searches, unless the pattern contains  an  uppercase
              character (smart case).

       -g, --glob
              Perform  a  glob-based  search  instead  of a regular expression
              search.

       --regex
              Perform a regular-expression based seach (default). This can  be
              used to override --glob.

       -F, --fixed-strings
              Treat  the  pattern  as  a  literal  string instead of a regular
              expression.

       -a, --absolute-path
              Shows the full path starting from the root as opposed  to  rela‐
              tive paths.

       -l, --list-details
              Use a detailed listing format like 'ls -l'. This is basically an
              alias  for  '--exec-batch  ls  -l'  with  some  additional  'ls'
              options.  This can be used to see more metadata, to show symlink
              targets and to achieve a deterministic sort order.

       -L, --follow
              By default, fd does  not  descend  into  symlinked  directories.
              Using this flag, symbolic links are also traversed.

       -p, --full-path
              By default, the search pattern is only matched against the file‐
              name (or directory  name).  Using  this  flag,  the  pattern  is
              matched against the full path.

       -0, --print0
              Separate  search  results by the null character (instead of new‐
              lines). Useful for piping results to xargs.

       --max-results count
              Limit the number of search results to 'count' and  quit  immedi‐
              ately.

       -1     Limit  the  search to a single result and quit immediately. This
              is an alias for '--max-results=1'.

       --show-errors
              Enable the display of filesystem errors for situations  such  as
              insufficient permissions or dead symlinks.

       --one-file-system, --mount, --xdev
              By  default,  fd  will  traverse  the file system tree as far as
              other options dictate. With this flag, fd ensures that  it  does
              not descend into a different file system than the one it started
              in. Comparable to the -mount or -xdev filters of find(1).

       -h, --help
              Print help information.

       -V, --version
              Print version information.

       -d, --max-depth d
              Limit directory traversal to at  most  d  levels  of  depth.  By
              default, there is no limit on the search depth.

       --min-depth d
              Only  show search results starting at the given depth. See also:
              '--max-depth' and '--exact-depth'.

       --exact-depth d
              Only show search results at the exact given depth.  This  is  an
              alias for '--min-depth <depth> --max-depth <depth>'.

       -t, --type filetype
              Filter search by type:

              f, file
                     regular files

              d, directory
                     directories

              l, symlink
                     symbolic links

              x, executable
                     executable (files)

              e, empty
                     empty files or directories

              s, socket
                     sockets

              p, pipe
                     named pipes (FIFOs)

              This  option  can  be used repeatedly to allow for multiple file
              types.

       -e, --extension ext
              Filter search results by file extension ext.  This option can be
              used repeatedly to allow for multiple possible file extensions.

       -E, --exclude pattern
              Exclude  files/directories  that  match  the given glob pattern.
              This overrides any other ignore logic.   Multiple  exclude  pat‐
              terns can be specified.

       --ignore-file path
              Add  a  custom  ignore-file in '.gitignore' format.  These files
              have a low precedence.

       -c, --color when
              Declare when to colorize search results:

              auto   Colorize output when standard output is connected to terminal (default).

              never  Do not colorize output.

              always Always colorize output.

       -j, --threads num
              Set number of threads to use for searching & executing (default:
              number of available CPU cores).

       -S, --size size
              Limit results based on  the  size  of  files  using  the  format
              <+-><NUM><UNIT>

              '+'    file size must be greater than or equal to this

              '-'    file size must be less than or equal to this

              'NUM'  The numeric size (e.g. 500)

              'UNIT' The  units for NUM. They are not case-sensitive.  Allowed
                     unit values:

                     'b'    bytes

                     'k'    kilobytes (base ten, 10^3 = 1000 bytes)

                     'm'    megabytes

                     'g'    gigabytes

                     't'    terabytes

                     'ki'   kibibytes (base two, 2^10 = 1024 bytes)

                     'mi'   mebibytes

                     'gi'   gibibytes

                     'ti'   tebibytes

       --changed-within date|duration
              Filter results based on the file modification time. The argument
              can  be  provided  as  a  specific  point  in  time  (YYYY-MM-DD
              HH:MM:SS) or as a duration (10h,  1d,  35min).   --change-newer-
              than can be used as an alias.

              Examples:
                --changed-within 2weeks
                --change-newer-than \"2018-10-27 10:00:00\"

       --changed-before date|duration
              Filter results based on the file modification time. The argument
              can  be  provided  as  a  specific  point  in  time  (YYYY-MM-DD
              HH:MM:SS)  or  as  a duration (10h, 1d, 35min).  --change-older-
              than can be used as an alias.

              Examples:
                --changed-before \"2018-10-27 10:00:00\"
                --change-older-than 2weeks

       -o, --owner [user][:group]
              Filter   files   by   their   user   and/or    group.    Format:
              [(user|uid)][:(group|gid)].  Either  side  is  optional. Precede
              either side with a '!' to exclude files instead.

              Examples:
                --owner john
                --owner :students
                --owner \"!john:students\"

       -x, --exec command
              Execute command for each search result. The following placehold‐
              ers  are  substituted  by a path derived from the current search
              result:

              {}     path

              {/}    basename

              {//}   parent directory

              {.}    path without file extension

              {/.}   basename without file extension

       -X, --exec-batch command
              Execute command with all  search  results  at  once.   A  single
              occurence  of  the following placeholders is authorized and
              sub stituted by the paths derived from the search results before the
              command is executed:

              {}     path

              {/}    basename

              {//}   parent directory

              {.}    path without file extension

              {/.}   basename without file extension

** Commands
\\<helm-fd-map>
|Keys|Description
|-----------+----------|
|\\[helm-ff-run-grep]|Run grep (`\\[universal-argument]' to recurse).
|\\[helm-ff-run-zgrep]|Run zgrep.
|\\[helm-ff-run-pdfgrep]|Run PDFgrep on marked files.
|\\[helm-ff-run-copy-file]|Copy file(s)
|\\[helm-ff-run-rename-file]|Rename file(s).
|\\[helm-ff-run-symlink-file]|Symlink file(s).
|\\[helm-ff-run-hardlink-file]|Hardlink file(s).
|\\[helm-ff-run-delete-file]|Delete file(s).
|\\[helm-ff-run-byte-compile-file]|Byte compile Elisp file(s) (`\\[universal-argument]' to load).
|\\[helm-ff-run-load-file]|Load Elisp file(s).
|\\[helm-ff-run-ediff-file]|Ediff file.
|\\[helm-ff-run-ediff-merge-file]|Ediff-merge file.
|\\[helm-ff-run-switch-other-window]|Switch to other window.
|\\[helm-ff-properties-persistent]|Show file properties.
|\\[helm-ff-run-open-file-externally]|Open file with external program (`\\[universal-argument]' to choose).
|\\[helm-ff-run-open-file-with-default-tool]|Open file externally with default tool.
|\\[helm-ff-run-insert-org-link]|Insert org link.
|\\[helm-fd-previous-directory]|Move to previous directory.
|\\[helm-fd-next-directory]|Move to next directory.")

;;; Generic file help - Used by locate.
;;
;;
(defvar helm-generic-file-help-message
  "* Helm Generic files

** Tips

*** Locate

You can append to the search pattern any of the locate command line options,
e.g. -b, -e, -n <number>, etc.  See the locate(1) man page for more details.

Some other sources (at the moment \"recentf\" and \"file in current directory\")
support the -b flag for compatibility with locate when they are used with it.

When you enable fuzzy matching on locate with `helm-locate-fuzzy-match', the
search will be performed on basename only for efficiency (so don't add \"-b\" at
prompt).  As soon as you separate the patterns with spaces, fuzzy matching will
be disabled and search will be done on the full filename.  Note that in
multi-match, fuzzy is completely disabled, which means that each pattern is a
match regexp (i.e. \"helm\" will match \"helm\" but \"hlm\" will *not* match
\"helm\").

NOTE: On Windows use Everything with its command line ~es~ as a replacement of locate.
See [[https://github.com/emacs-helm/helm/wiki/Locate#windows][Locate on Windows]]

*** Browse project

When the current directory is not under version control, don't forget to refresh
the cache when files have been added/removed in the directory.

*** Find command

Recursively search files using the \"find\" shell command.

Candidates are all filenames that match all given globbing patterns.  This
respects the options `helm-case-fold-search' and
`helm-findutils-search-full-path'.

You can pass arbitrary \"find\" options directly after a \"*\" separator.
For example, this would find all files matching \"book\" that are larger
than 1 megabyte:

    book * -size +1M

** Commands
\\<helm-generic-files-map>
|Keys|Description
|-----------+----------|
|\\[helm-ff-run-toggle-basename]|Toggle basename.
|\\[helm-ff-run-grep]|Run grep (`\\[universal-argument]' to recurse).
|\\[helm-ff-run-zgrep]|Run zgrep.
|\\[helm-ff-run-pdfgrep]|Run PDFgrep on marked files.
|\\[helm-ff-run-copy-file]|Copy file(s)
|\\[helm-ff-run-rename-file]|Rename file(s).
|\\[helm-ff-run-symlink-file]|Symlink file(s).
|\\[helm-ff-run-hardlink-file]|Hardlink file(s).
|\\[helm-ff-run-delete-file]|Delete file(s).
|\\[helm-ff-run-byte-compile-file]|Byte compile Elisp file(s) (`\\[universal-argument]' to load).
|\\[helm-ff-run-load-file]|Load Elisp file(s).
|\\[helm-ff-run-ediff-file]|Ediff file.
|\\[helm-ff-run-ediff-merge-file]|Ediff-merge file.
|\\[helm-ff-run-switch-other-window]|Switch to other window.
|\\[helm-ff-properties-persistent]|Show file properties.
|\\[helm-ff-run-open-file-externally]|Open file with external program (`\\[universal-argument]' to choose).
|\\[helm-ff-run-open-file-with-default-tool]|Open file externally with default tool.
|\\[helm-ff-run-insert-org-link]|Insert org link.")

;;; Grep help
;;
;;
(defvar helm-grep-help-message
  "* Helm Grep

** Tips

With Helm supporting Git-grep and AG/RG, you are better off using
one of them for recursive searches, keeping grep or ack-grep to
grep individual or marked files.  See [[Helm AG][Helm AG]].

*** Meaning of the prefix argument
**** With grep or ack-grep

Grep recursively, in this case you are
prompted for types (ack-grep) or for wild cards (grep).

**** With AG or RG

the prefix arg allows you to specify a type of file to search in.

*** You can use wild cards when selecting files (e.g. \"*.el\")

Note that a way to grep specific files recursively is to use
e.g. \"**.el\" to select files, the variable `helm-file-globstar'
controls this (it is non nil by default), however it is much
slower than using grep recusively (see helm-find-files
documentation about this feature).

*** Grep hidden files

You may want to customize your command line for grepping hidden
files, for AG/RG use \"--hidden\", see man page
of your backend for more infos.

*** You can grep in different directories by marking files or using wild cards

*** You can save the result in a `helm-grep-mode' buffer

See [[Commands][commands]] below.

Once in that buffer you can use [[https://github.com/mhayashi1120/Emacs-wgrep][emacs-wgrep]] (external package not bundled with Helm)
to edit your changes, for Helm the package name is `wgrep-helm', it is hightly recommended.

Type `g' to update (revert) the buffer (after saving your buffer's changes to file).

NOTE: `next-error' is available from this `helm-grep-mode' buffer.

When you are running `next-error' from elsewhere, you can update
the buffer with `helm-revert-next-error-last-buffer' (up to you
to bind it to a convenient key).

*** Helm-grep supports multi-matching

\(Starting from version 1.9.4.)

Simply add a space between each pattern as for most Helm commands.

NOTE: Depending the regexp you use it may match as well the
filename, this because we pipe the first grep command which send
the filename in output.

*** See full path of selected candidate

Add (helm-popup-tip-mode 1) in your init file or enable it
interactively with M-x helm-popup-tip-mode, however it is
generally enough to just put your mouse cursor over candidate.

*** Open file in other window

The command \\<helm-grep-map>\\[helm-grep-run-other-window-action] allow you to open file
in other window horizontally or vertically if a prefix arg is supplied.

*** Performance over TRAMP

Grepping works but it is badly supported as TRAMP doesn't support multiple
processes running in a short delay (less than 5s) among other things.

Helm uses a special hook to suspend the process automatically while you are
typing.  Even if Helm handles this automatically by delaying each process by 5s,
you are adviced to this manually by hitting `\\<helm-map>\\[helm-toggle-suspend-update]' (suspend process) before
typing, and hit again `\\<helm-map>\\[helm-toggle-suspend-update]' when the regexp is ready to send to the remote
process.  For simple regexps, there should be no need for this.

Another solution is to not use TRAMP at all and mount your remote file system via
SSHFS.

* Helm GID

Still supported, but mostly deprecated, using AG/RG or Git-grep
is much more efficient, also `id-utils' seems no more maintained.

** Tips

Helm-GID reads the database created with the `mkid' command from id-utils.
The name of the database file can be customized with `helm-gid-db-file-name', it
is usually \"ID\".

Helm-GID use the symbol at point as default-input.  This command is also
accessible from `helm-find-files' which allow you to navigate to another
directory to consult its database.

Note: Helm-GID supports multi-matches but only the last pattern entered will be
highlighted since there is no ~--color~-like option in GID itself.

* Helm AG

** Tips

Helm-AG is different from grep or ack-grep in that it works on a
directory recursively and not on a list of files.  It is called
helm-AG but it support several backend, namely AG, RG and PT.
Nowaday the best backend is Ripgrep aka RG, it is the fastest and
is actively maintained, see `helm-grep-ag-command' and
`helm-grep-ag-pipe-cmd-switches' to configure it.

You can ignore files and directories with a \".agignore\" or
\".rgignore\" file, local to a directory or global when placed in
the home directory. (See the AG/RG man pages for more details.)
Note that `helm-grep-ignored-files'and
`helm-grep-ignored-directories' have no effect in helm-AG/RG.

As always you can access Helm AG from `helm-find-files'.

Starting with version 0.30, AG accepts one or more TYPE arguments on its command
line.  Helm provides completion on these TYPE arguments when available with your
AG version.  Use a prefix argument when starting a Helm-AG session to enable this
completion.  See RG and AG man pages on how to add new types.


Note: You can mark several types to match in the AG query.  The first AG
versions providing this feature allowed only one type, so in this case only the
last mark will be used.

* Helm git-grep

Helm-git-grep searches the current directory, i.e. the default directory or the
directory in Helm-find-files.  If this current directory is a subdirectory of a
project and you want to also match parent directories (i.e the whole project),
use a prefix argument.

** Commands
\\<helm-grep-map>
|Keys|Description
|-----------+----------|
|\\[helm-goto-next-file]|Next File.
|\\[helm-goto-precedent-file]|Previous File.
|\\[helm-yank-text-at-point]|Yank text at point in minibuffer.
|\\[helm-grep-run-other-window-action]|Jump to other window.
|\\[helm-grep-run-other-frame-action]|Jump to other frame.
|\\[helm-grep-run-default-action]|Run default action (same as `RET').
|\\[helm-grep-run-save-buffer]|Save to a `helm-grep-mode' enabled buffer.")

;;; PDF grep help
;;
;;
(defvar helm-pdfgrep-help-message
  "* Helm PDFgrep Map

** Commands
\\<helm-pdfgrep-map>
|Keys|Description
|-----------+----------|
|\\[helm-goto-next-file]|Next file.
|\\[helm-goto-precedent-file]|Previous file.
|\\[helm-yank-text-at-point]|Yank text at point in minibuffer.")

;;; Etags help
;;
;;
(defvar helm-etags-help-message
  "* Helm Etags Map

** Commands
\\<helm-etags-map>
|Keys|Description
|-----------+----------|
|\\[helm-goto-next-file]|Next file.
|\\[helm-goto-precedent-file]|Previous file.
|\\[helm-yank-text-at-point]|Yank text at point in minibuffer.")

;;; UCS help
;;
;;
(defvar helm-ucs-help-message
  "* Helm UCS

** Tips

Use commands below to insert unicode characters in current buffer without
leaving Helm.

** Commands
\\<helm-ucs-map>
|Keys|Description
|-----------+----------|
|\\[helm-ucs-persistent-insert]|Insert character.
|\\[helm-ucs-persistent-forward]|Forward character.
|\\[helm-ucs-persistent-backward]|Backward character.
|\\[helm-ucs-persistent-delete]|Delete character backward.
|\\[helm-ucs-persistent-insert-space]|Insert space.")

;;; Bookmark help
;;
;;
(defvar helm-bookmark-help-message
  "* Helm bookmark name

When `helm-bookmark-use-icon' is non nil and `all-the-icons'
package is installed icons before candidates will be displayed.
 
** Commands
\\<helm-bookmark-map>
|Keys|Description
|-----------+----------|
|\\[helm-bookmark-run-jump-other-window]|Jump other window.
|\\[helm-bookmark-run-delete]|Delete bookmark.
|\\[helm-bookmark-run-edit]|Edit bookmark.
|\\[helm-bookmark-toggle-filename]|Toggle bookmark location visibility.")

;;; Eshell command on file help
;;
;;
(defvar helm-esh-help-message
  "* Helm Eshell on file

** Tips

*** Pass extra arguments after filename

Normally the command or alias will be called with file as argument.  For instance

    <command> candidate_file

But you can also pass an argument or more after \"candidate_file\" like this:

    <command> %s [extra_args]

\"candidate_file\" will be added at \"%s\" and the command will look at this:

    <command> candidate_file [extra_args]

**** Use placeholders in extra arguments

placeholder for file without extension: \\@ 
placeholder for incremental number:     \\#

\"candidate_file\" will be added at \"%s\" and \\@ but without extension.

    <command %s \\@>

\"candidate_file\" will be added at \"%s\" and \\# will be replaced by an incremental number.

    <command> %s \\#

Here examples:

Say you want to use the =convert= command to convert all your .png files in a directory to .jpg.

This will convert all your files to jpg keeping the same basename.

    convert %s \\@.jpg

This will convert all your files to foo-001.jpg, foo-002.jpg etc...

    convert %s foo-\\#.jpg

You can of course combine both placeholders if needed.

    convert %s \\@-\\#.jpg

*** Specify marked files as arguments

When you have marked files and your command support only one file
as arg, helm will execute command sequencially on each file like
this:

Example:

    <command> file1
    <command> file2
    ...etc

When you have marked files and your command accept many files at
once helm will run your command with all files at once like this:

Example:

    <command> file1 file2 etc...

The two use case above are applied automatically by Helm
depending if your command is an eshell alias which value ends by
'$1' or '$*'.  If your command is not an alias, i.e. you entered
an arbitrary command on prompt with '%s' to specify filenames,
you will have to pass one prefix argument from the command
selection buffer.

Note: This does not work on remote files.

With two prefix-args the output is printed to the
`current-buffer', the command being executed in the same
conditions as described above.
NOTE: If your command is not an alias, you can't pass all files at once and print in current buffer at the same time.
Also note that running multiple files at once is not supported with remote files.

*** Run eshell commands asynchronously

You can run your commands asynchronously by adding \"&\" at end
of any commands, e.g. \"foo %s &\".  You can also directly setup
your alias in the eshell alias file with e.g. \"alias foo $1 &\".

** Commands
\\<helm-esh-on-file-map>")

;;; Ido virtual buffer help
;;
;;
(defvar helm-buffers-ido-virtual-help-message
  "* Helm Ido virtual buffers

** Commands
\\<helm-buffers-ido-virtual-map>
|Keys|Description
|-----------+----------|
|\\[helm-ff-run-switch-other-window]|Switch to other window.
|\\[helm-ff-run-switch-other-frame]|Switch to other frame.
|\\[helm-ff-run-grep]|Grep file.
|\\[helm-ff-run-zgrep]|Zgrep file.
|\\[helm-ff-run-delete-file]|Delete file.
|\\[helm-ff-run-open-file-externally]|Open file externally.")

;;; helm-occur help
;;
;;
(defvar helm-moccur-help-message
  "* Helm Moccur

** Tips

*** Searching in many buffers

Start from `helm-buffers-list' or `helm-mini', mark some buffers and hit \\<helm-buffer-map\\[helm-buffers-run-occur].
A prefix arg will change the behavior of `helm-occur-always-search-in-current'
i.e. add current buffer or not to the list of buffers to search in.

*** Matching

Multiple regexp matching is allowed, simply enter a space to separate the regexps.

Matching empty lines is supported with the regexp \"^$\", you then get the
results displayed as the buffer-name and the line number only.  You can
save and edit these results, i.e. add text to the empty line.

**** Matching shorthands symbols in Elisp code

Helm-occur have a basic support of [[info:elisp#Shorthands][read-symbol-shorthands]].
You can enable this by customizing =helm-occur-match-shorthands=.

The main usage is when you are in a given buffer with cursor on a
symbol and you want to see where the definition is or where it is
used in another buffer or other buffers.  Of course matching is
working on both versions of the definition, the short one and the
long one.  Here an example reusing the sample files used in the
Manual:

Here snu.el file with cursor on snu-lines definition:

#+begin_src elisp
     (defun snu-split (separator s &optional omit-nulls)
       \"A match-data saving variation on `split-string'.\"
       (save-match-data (split-string s separator omit-nulls)))

     (defun snu-lines (s)
       \"Split string S into a list of strings on newline characters.\"
       (snu-split \"\\\\(\\r\\n\\\\|[\\n\\r]\\\\)\" s))

     ;; Local Variables:
     ;; read-symbol-shorthands: ((\"snu-\" . \"some-nice-string-utils-\"))
     ;; End:
#+end_src

And here the my-tricks.el file reusing snu-lines but under another name:

#+begin_src elisp
     (defun t-reverse-lines (s)
       (string-join (reverse (sns-lines s)) \"\\n\"))

     ;; Local Variables:
     ;; read-symbol-shorthands: ((\"t-\" . \"my-tricks-\")
     ;;                          (\"sns-\" . \"some-nice-string-utils-\"))
     ;; End:

#+end_src

You want to know where the definition currently at point ('snu-lines') is used in the my-tricks.el buffer.
You launch for example helm-mini and start helm-occur on my-tricks.el, helm occur will match immediately
'sns-lines'.

*** Automatically match symbol at point

Helm can automatically match the symbol at point while keeping
the minibuffer empty, ready to be written to when
`helm-source-occur' and `helm-source-moccur' are member of
`helm-sources-using-default-as-input'.

*** Yank word at point in minibuffer

Use `C-w' as many times as needed, undo with =C-_=.  Note that
=C-w= and =C-_= are not standard keybindings, but bindings
provided with special helm feature
`helm-define-key-with-subkeys'.

*** Preselection

When helm-occur search symbol at point the current line is
preselected in the source related to current-buffer.  When
`helm-occur-keep-closest-position' is non nil helm-occur will
select the line which is the closest from the current line in
current-buffer after updating.

*** Jump to the corresponding line in the searched buffer

You can do this with `\\<helm-map>\\[helm-execute-persistent-action]' (persistent-action), to do it repeatedly
you can use `\\<helm-map>\\[helm-follow-action-forward]' and `\\<helm-map>\\[helm-follow-action-backward]' or enable `helm-follow-mode' with `\\<helm-map>\\[helm-follow-mode]'.
Follow mode is enabled by default in helm-occur.

*** Switch to buffer in other window

The command \\<helm-moccur-map>\\[helm-moccur-run-goto-line-ow] allow you to switch to buffer
in other window horizontally or vertically if a prefix arg is supplied.

*** Save the results

Similarly to Helm-grep, you can save the results with `\\<helm-occur-map>\\[helm-occur-run-save-buffer]'.
Once in the saved buffer, you can edit it, see [[Edit a saved buffer][below]].

Of course if you don't save the results, you can resume the Helm session with
`helm-resume'.

*** Refresh the resumed session

When the buffer(s) where you ran helm-(m)occur get(s) modified, the Helm buffer
will flash red as a warning.  You can refresh the buffer by running `\\<helm-map>\\[helm-refresh]'.
This can be done automatically by customizing `helm-moccur-auto-update-on-resume'.

*** Refresh a saved buffer

Type `g' to update (revert) the buffer.

When you are running `next-error' from elsewhere, you can update
the buffer with `helm-revert-next-error-last-buffer' (up to you
to bind it to a convenient key).

*** Edit a saved buffer

First, install wgrep (https://github.com/mhayashi1120/Emacs-wgrep) and then:

1) `C-c C-p' (`wgrep-change-to-wgrep-mode') to edit the buffer(s).
2) `C-x C-s' to save your changes.

Tip: Use the excellent iedit (https://github.com/victorhge/iedit) to modify all
occurences at once in the buffer.

NOTE: `next-error' is available from this `helm-occur-mode' buffer.

*** Search in region

When searching in current-buffer with `helm-occur', if a region
is found helm will search in this region only.  If you marked
this region with `mark-defun' the symbol that was at point before
marking defun will be used when `helm-source-occur' is member of
`helm-sources-using-default-as-input'.

*** Switch to next or previous source

See [[Moving in `helm-buffer'][Moving in `helm-buffer']].

** Commands
\\<helm-occur-map>
|Keys|Description
|-----------+----------|
|\\[helm-occur-run-goto-line-ow]|Go to line in other window.
|\\[helm-occur-run-goto-line-of]|Go to line in new frame.
|\\[helm-occur-run-save-buffer]|Save results in new buffer.")
;;; Helm Top
;;
;;
(defvar helm-top-help-message
  "* Helm Top

** Commands
\\<helm-top-map>
|Keys|Description
|-----------+----------|
|\\[helm-top-run-sort-by-com]|Sort by commands.
|\\[helm-top-run-sort-by-cpu]|Sort by CPU usage.
|\\[helm-top-run-sort-by-user]|Sort alphabetically by user.
|\\[helm-top-run-sort-by-mem]|Sort by memory.")

;;; Helm M-x
;;
;;
(defvar helm-M-x-help-message
  "* Helm M-x

** Tips

*** Display docstring without quitting session (persistent action)

You can get help on any command with persistent action (\\<helm-map>\\[helm-execute-persistent-action])

*** Display short docstring in helm buffer

You can toggle short docstring description with \\<helm-M-x-map>\\[helm-M-x-toggle-short-doc].
if you want this at startup you can configure `helm-M-x-show-short-doc'.

NOTE: helm-M-x will be slower with this enabled.

*** History source

Helm-M-x is displaying two sources, one for the commands
themselves and one for the command history, more exactly
`extended-command-history', by default the history source is
displayed in first position, however you can put it in second
position if you don't like that by customizing
`helm-M-x-reverse-history'.

**** Duplicate entries in helm-M-x history

helm-M-x history obey to history variables, if you have
duplicates in your helm-M-x history set `history-delete-duplicates' to non nil.

**** Number of entries in history

The number of entries saved is controlled by `history-length'
global value, however if you want a different value for
`extended-command-history' e.g. 50 you can add to your config:

    (put 'extended-command-history 'history-length 50)

*** Enabled modes are highlighted in helm-M-x

*** Prefix arguments

You can pass prefix arguments *after* starting `helm-M-x'.  A mode-line
counter will display the number of given prefix arguments.

If you pass prefix arguments before running `helm-M-x', it will
be displayed in the prompt.
The first `\\<global-map>\\[universal-argument]' after `helm-M-x' clears those prefix arguments.

NOTE: When you specify prefix arguments once `helm-M-x' is
started, the prefix argument apply on the next command, so if you
hit RET, it will apply on the selected command, but if you type a
new character at prompt to narrow down further candidates, the
prefix arg will apply to `self-insert-command' (e.g. if you type
`C-u e' \"eeee\" will be inserted in prompt) so select the
command you want to execute before specifying prefix arg.

** Commands
\\<helm-M-x-map>
|Keys|Description
|-----------+------------|
|\\[helm-M-x-universal-argument]|Universal argument for selected command
|\\[helm-M-x-toggle-short-doc]|Toggle details on commands")


;;; Helm imenu
;;
;;
(defvar helm-imenu-help-message
  "* Helm Imenu

** Commands
\\<helm-imenu-map>
|Keys|Description
|-----------+----------|
|\\[helm-imenu-next-section]|Go to next section.
|\\[helm-imenu-previous-section]|Go to previous section.")

;;; Helm colors
;;
;;
(defvar helm-colors-help-message
  "* Helm colors

** Commands
\\<helm-color-map>
|Keys|Description
|-----------+----------|
|\\[helm-color-run-insert-name]|Insert the entry name.
|\\[helm-color-run-kill-name]|Kill the entry name.
|\\[helm-color-run-insert-rgb]|Insert entry in RGB format.
|\\[helm-color-run-kill-rgb]|Kill entry in RGB format.")

;;; Helm Semantic
;;
;;
(defvar helm-semantic-help-message
  "* Helm Semantic

** Commands
\\<helm-semantic-map>")

;;; Helm kmacro
;;
;;
(defvar helm-kmacro-help-message
  "* Helm kmacro

** Tips

- Start recording a kmacro with `f3'.
- End the kmacro recording with `f4'.
- Run `helm-execute-kmacro' to list all your kmacros.

When you press RET, your macro goes on top of ring and become the
current macro, hit `f4' for further executions.
Use `helm-execute-kmacro' again to change eventually your macro to execute.

Note: You can't record keys running Helm commands except `helm-M-x', under the
condition that you don't choose a command using Helm completion.

See [[info:emacs#Keyboard Macros][Keyboard Macros]] for further infos on macros.

** Commands
\\<helm-kmacro-map>")

;;; Kill ring
;;
;;
(defvar helm-kill-ring-help-message
  "* Helm kill ring

** Tips

Every Helm session lets you save a candidate to the kill-ring / clipboard /
primary-selection with `\\<helm-map>\\[helm-kill-selection-and-quit]'.

To save space, Helm-kill-ring truncates the candidates longer than
`helm-kill-ring-max-offset'.
`\\<helm-kill-ring-map>\\[helm-kill-ring-kill-selection]' then saves the whole
text and not the truncated value.  The view of truncated candidates can be
toggled; see the command list below.

As opposed to `yank', numeric prefix arguments are ignored with
`helm-show-kill-ring': there is no need for them since selection happens within
Helm.  Moreover Helm has [[Shortcuts for executing the default action on the n-th candidate][Shortcuts for executing the default action on the n-th candidate]].

It is recommended to globally bind `M-y' to `helm-show-kill-ring'.  Once in the
Helm-kill-ring session you can navigate to next/previous line with `M-y' and
`M-u' for convenience.  Of course `\\[helm-next-line]' and `\\[helm-previous-line]' are still available.

It is possible to delete candidates from the kill ring with `\\<helm-kill-ring-map>\\[helm-kill-ring-delete]'
but also persistently with `\\<helm-kill-ring-map>\\[helm-kill-ring-run-persistent-delete]'.

You can concatenate marked candidates and yank them in the current
buffer, thus creating a new entry in the kill ring.  Candidates are
concatenated with `helm-kill-ring-separator' as default but you can
change interactively the separator while yanking by using two prefix
args.  When you have something else than \"\\n\" as default value for
`helm-kill-ring-separator' and you want to use \"\\n\" from prompt, use
`C-q C-j' to enter a newline in prompt.

To not push a new entry in the kill ring, use `\\<helm-map>\\[helm-copy-to-buffer]' instead of RET
\(note that you can't change separator with this).

When inserting candidates with the default action (`RET'), `point' is placed at
the end of the candidate and `mark' at the beginning.  You can revert this behavior
by using a prefix argument, i.e. `C-u RET', like the regular `yank' command does.

** Commands
\\<helm-kill-ring-map>
|Keys|Description
|-----------+----------|
|\\[helm-next-line]|Next line.
|\\[helm-previous-line]|Previous line.
|\\[helm-kill-ring-delete]|Delete entry.
|\\[helm-kill-ring-toggle-truncated]|Toggle truncated view of candidate.
|\\[helm-kill-ring-kill-selection]|Kill non-truncated of selection.")

;;; Completing-read
;;
(defun helm-comp-read-help-message ()
  (let ((com (assoc-default 'name (helm-get-current-source))))
    (format
     "* Helm completing-read completion for `%s'

Command `%s' is using a `completing-read' for completion on your input,
this completion have been \"helmized\" because you have enabled [[Helm mode][helm-mode]]'.

** Tips

*** Disabling or use something else than helm for completion of some commands

You can disable helm completion or use something else for specific commands of your choice,
for this customize variable `helm-completing-read-handlers-alist'.

*** Exiting minibuffer with empty string

You can exit minibuffer with empty string with \\<helm-comp-read-map>\\[helm-cr-empty-string].
It is useful when some commands are prompting continuously until you enter an empty prompt.

** Commands
\\<helm-comp-read-map>
|Keys|Description
|-----------+----------|
|\\[helm-cr-empty-string]|Exit minibuffer with empty string."
     com com)))


;;; Mode line strings
;;
;;
;;;###autoload
(defvar helm-comp-read-mode-line "\
\\<helm-comp-read-map>\
C/\\[helm-cr-empty-string]:Empty \
\\<helm-map>\
\\[helm-help]:Help \
\\[helm-select-action]:Act \
\\[helm-maybe-exit-minibuffer]/\
f1/f2/f-n:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend \
\\[helm-customize-group]:Conf")

;;;###autoload
(defvar helm-read-file-name-mode-line-string "\
\\<helm-read-file-map>\
\\[helm-help]:Help \
C/\\[helm-cr-empty-string]:Empty \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-maybe-exit-minibuffer]/\
f1/f2/f-n:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend \
\\[helm-customize-group]:Conf"
  "String displayed in mode-line in `helm-source-find-files'.")

;;;###autoload
(defvar helm-top-mode-line "\
\\<helm-top-map>\
\\[helm-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-maybe-exit-minibuffer]/\
f1/f2/f-n:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend \
\\[helm-customize-group]:Conf")


(provide 'helm-help)

;;; helm-help.el ends here
