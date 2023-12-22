#!/usr/bin/env sh


## Copyright (C) 2012 ~ 2023 Thierry Volpiatto 
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

## Commentary:
# Preconfigured `emacs -Q' with a basic Helm configuration.


# If TEMP env var exists, use it, otherwise declare it.
test -z "$TEMP" && TEMP="/tmp"

CONF_FILE="$TEMP/helm-cfg.el"
EMACS=emacs
QUICK=-Q
TOOLBARS=-1
LOAD_PACKAGES=

usage () {
    cat >&1 <<EOF
Usage: ${0##*/} [-P PATH] [--toolbars] [--load-packages pkgs] [-h] [EMACS-OPTIONS-OR-FILENAME]

-P --path        Specify path to emacs
-B --toolbars    Display Menu bar, scroll bar etc...
--load-packages  Load specified M/Elpa packages (separate with ",")
-h               Display this help and exit

Any other Emacs options or filename must come after.

Emacs options:

Initialization options:

--chdir DIR                 change to directory DIR
--daemon, --bg-daemon[=NAME] start a (named) server in the background
--fg-daemon[=NAME]          start a (named) server in the foreground
--debug-init                enable Emacs Lisp debugger for init file
--display, -d DISPLAY       use X server DISPLAY
--no-build-details          do not add build details such as time stamps
--no-loadup, -nl            do not load loadup.el into bare Emacs
--no-site-file              do not load site-start.el
--no-x-resources            do not load X resources
--no-window-system, -nw     do not communicate with X, ignoring $DISPLAY
--script FILE               run FILE as an Emacs Lisp script
--terminal, -t DEVICE       use DEVICE for terminal I/O

Action options:

FILE                    visit FILE
+LINE                   go to line LINE in next FILE
+LINE:COLUMN            go to line LINE, column COLUMN, in next FILE
--directory, -L DIR     prepend DIR to load-path (with :DIR, append DIR)
--file FILE             visit FILE
--find-file FILE        visit FILE
--funcall, -f FUNC      call Emacs Lisp function FUNC with no arguments
--insert FILE           insert contents of FILE into current buffer
--load, -l FILE         load Emacs Lisp FILE using the load function
--visit FILE            visit FILE

Display options:

--background-color, -bg COLOR   window background color
--basic-display, -D             disable many display features;
                                  used for debugging Emacs
--border-color, -bd COLOR       main border color
--border-width, -bw WIDTH       width of main border
--color, --color=MODE           override color mode for character terminals;
                                  MODE defaults to \`auto', and
                                  can also be \`never', \`always',
                                  or a mode name like \`ansi8'
--cursor-color, -cr COLOR       color of the Emacs cursor indicating point
--font, -fn FONT                default font; must be fixed-width
--foreground-color, -fg COLOR   window foreground color
--fullheight, -fh               make the first frame high as the screen
--fullscreen, -fs               make the first frame fullscreen
--fullwidth, -fw                make the first frame wide as the screen
--maximized, -mm                make the first frame maximized
--geometry, -g GEOMETRY         window geometry
--iconic                        start Emacs in iconified state
--internal-border, -ib WIDTH    width between text and main border
--line-spacing, -lsp PIXELS     additional space to put between lines
--mouse-color, -ms COLOR        mouse cursor color in Emacs window
--name NAME                     title for initial Emacs frame
--reverse-video, -r, -rv        switch foreground and background
--title, -T TITLE               title for initial Emacs frame
--vertical-scroll-bars, -vb     enable vertical scroll bars
--xrm XRESOURCES                set additional X resources
--parent-id XID                 set parent window
--help                          display this help and exit
--version                       output version information and exit

You can generally also specify long option names with a single -; for
example, -batch as well as --batch.  You can use any unambiguous
abbreviation for a --option.

Various environment variables and window system resources also affect
the operation of Emacs.  See the main documentation.
EOF
         }

for a in "$@"; do
    case $a in
        --path | -P)
            shift 1
            EMACS="$1"
            shift 1
            ;;
        --toolbars | -B)
            shift 1
            TOOLBARS=1
            ;;
        --load-packages)
            shift 1
            LOAD_PACKAGES="$1"
            shift 1
            ;;
        -Q | -q)
            QUICK="$a"
            ;;
        -h)
            usage
            exit 1
            ;;
    esac
done

LOAD_PATH=$($EMACS -q -batch --eval "(prin1 load-path)")

cd "${0%/*}" || exit 1

# Check if autoload file exists.
# It may be in a different directory if emacs-helm.sh is a symlink.
TRUENAME=$(find "${0%/*}" -path "$0" -printf "%l")
if [ -n "$TRUENAME" ]; then
    AUTO_FILE="${TRUENAME%/*}/helm-autoloads.el"
else
    AUTO_FILE="helm-autoloads.el"
fi
if [ ! -e "$AUTO_FILE" ]; then
    echo No autoloads found, please run make first to generate autoload file
    exit 1
fi


cat > $CONF_FILE <<EOF
(setq initial-scratch-message (concat initial-scratch-message
";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\\n\
;; This Emacs is Powered by \`HELM' using\\n\
;; emacs program \"$EMACS\".\\n\
;; This is a minimal \`helm' configuration to discover \`helm' or debug it.\\n\
;; You can retrieve this minimal configuration in \"$CONF_FILE\".\\n\
;;
;; Some original Emacs commands are replaced by their \`helm' counterparts:\\n\\n\
;; - \`find-file'(C-x C-f)            =>\`helm-find-files'\\n\
;; - \`occur'(M-s o)                  =>\`helm-occur'\\n\
;; - \`list-buffers'(C-x C-b)         =>\`helm-buffers-list'\\n\
;; - \`completion-at-point'(M-tab)    =>\`helm-lisp-completion-at-point'[1]\\n\
;; - \`apropos-command'(C-h a)        =>\`helm-apropos'\\n\
;; - \`dabbrev-expand'(M-/)           =>\`helm-dabbrev'\\n\
;; - \`execute-extended-command'(M-x) =>\`helm-M-x'\\n\\n
;; Some other Emacs commands are \"helmized\" by \`helm-mode'.\\n\
;; [1] Coming with emacs-24.4, \`completion-at-point' is \"helmized\" by \`helm-mode'\\n\

;; which provides Helm completion in many places like \`shell-mode'.\\n\
;; Find context help for most Helm commands with \`C-h m'.\\n\
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\\n\\n"))

(setq load-path (quote $LOAD_PATH))

(defvar default-package-manager nil)
;; /home/you/.emacs.d/.local/straight/build-27.1/helm
(defvar initial-package-directory (file-name-directory (file-truename "$0")))

(defvar bootstrap-version)
(let* ((packages "$LOAD_PACKAGES")
       (pkg-list (and packages
                      (not (equal packages ""))
                      (split-string packages ",")))
       ;; /home/you/.emacs.d/.local/straight/build-27.1
       (straight-path (file-name-directory (directory-file-name initial-package-directory)))
       ;; /home/you/.emacs.d/.local/straight/build-27.1/async
       (async-path (expand-file-name "async" straight-path))
       ;; /home/you/.emacs.d/.local/straight/repos/straight.el/bootstrap.el
       (bootstrap-file
        (expand-file-name "repos/straight.el/bootstrap.el"
                          (file-name-directory (directory-file-name straight-path))))
       (bootstrap-version 5))
  (when (file-exists-p bootstrap-file)
    (setq default-package-manager 'straight)
    (load bootstrap-file nil 'nomessage)
    (add-to-list 'load-path async-path)
    (when pkg-list
      (dolist (pkg pkg-list)
        (let* ((pkg-path (expand-file-name pkg straight-path))
               (autoload-file (expand-file-name
                               (format "%s-autoloads.el" pkg)
                               pkg-path)))
          (add-to-list 'load-path pkg-path)
          (if (file-exists-p autoload-file)
              (load autoload-file nil 'nomessage)
            (straight-use-package (intern pkg))))))))

(unless (eq default-package-manager 'straight)
  (require 'package)
  ;; User may be using a non standard \`package-user-dir'.
  ;; Modify \`package-directory-list' instead of \`package-user-dir'
  ;; in case the user starts Helm from a non-ELPA installation.
  (unless (file-equal-p package-user-dir (locate-user-emacs-file "elpa"))
    ;; Something like  /home/you/.emacs.d/somedir/else/elpa/
    ;; starting from default-directory is wrong in case helm.sh is a symlink
    ;; or e.g. helm --chdir foo have been used.
    (add-to-list 'package-directory-list (directory-file-name
                                          (file-name-directory
                                           (directory-file-name initial-package-directory)))))

  (let* ((str-lst "$LOAD_PACKAGES")
         (load-packages (and str-lst
                             (not (string= str-lst ""))
                             (split-string str-lst ","))))
    (setq package-load-list
          (if (equal load-packages '("all"))
              '(all)
            (append '((helm-core t) (helm t) (async t) (popup t) (wfnames t))
                    (mapcar (lambda (p) (list (intern p) t)) load-packages)))))

  (package-initialize))

(add-to-list 'load-path initial-package-directory)

(unless (> $TOOLBARS 0)
   (setq default-frame-alist '((vertical-scroll-bars . nil)
                               (tool-bar-lines . 0)
                               (menu-bar-lines . 0)
                               (fullscreen . nil))))
(blink-cursor-mode -1)
(load "helm-autoloads" nil t)
(helm-mode 1)
(with-eval-after-load 'tramp-cache (setq tramp-cache-read-persistent-data t))
(with-eval-after-load 'auth-source (setq auth-source-save-behavior nil))
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(define-key global-map [remap apropos-command] 'helm-apropos)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
(add-hook 'kill-emacs-hook #'(lambda () (and (file-exists-p "$CONF_FILE") (delete-file "$CONF_FILE"))))
EOF

$EMACS "$QUICK" -l "$CONF_FILE" "$@"
