;;; sly.el --- Sylvester the Cat's Common Lisp IDE  -*- lexical-binding: t; -*-

;; Version: 1.0.43
;; URL: https://github.com/joaotavora/sly
;; Package-Requires: ((emacs "24.3"))
;; Keywords: languages, lisp, sly

;;     Copyright (C) 2003  Eric Marsden, Luke Gorrie, Helmut Eller
;;     Copyright (C) 2004,2005,2006  Luke Gorrie, Helmut Eller
;;     Copyright (C) 2007,2008,2009  Helmut Eller, Tobias C. Rittweiler
;;     Copyright (C) 2014 João Távora
;;     For a detailed list of contributors, see the manual.

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
;;
;;        _____    __   __  __
;;       / ___/   / /   \ \/ /               |\      _,,,---,,_
;;       \__ \   / /     \  /                /,`.-'`'    -.  ;-;;,_
;;      ___/ /  / /___   / /                |,4-  ) )-,_..;\ (  `'-'
;;     /____/  /_____/  /_/                '---''(_/--'  `-'\_)
;;
;;
;; SLY is Sylvester the Cat's Common Lisp IDE.
;;
;; SLY is a direct fork of SLIME, and contains the following
;; improvements over it:
;;
;; * A full-featured REPL based on Emacs's `comint.el`;
;; * Live code annotations via a new `sly-stickers` contrib;
;; * Consistent button interface. Every Lisp object can be copied to the REPL;
;; * flex-style completion out-of-the-box, using  Emacs's completion API.
;;   Company, Helm, and others supported natively, no plugin required;
;; * Cleanly ASDF-loaded by default, including contribs, enabled out-of-the-box;
;; * Multiple inspectors and multiple REPLs;
;; * An interactive trace dialog with interactive objects.  Copies function calls
;;   to the REPL;
;; * "Presentations" replaced by interactive backreferences which
;;   highlight the object and remain stable throughout the REPL session;
;;
;; SLY is a fork of SLIME. We track its bugfixes, particularly to the
;; implementation backends.  All SLIME's familar features (debugger,
;; inspector, xref, etc...) are still available, with improved overall
;; UX.
;;
;; See the NEWS.md file (should be sitting alongside this file) for
;; more information

;;; Code:

(require 'cl-lib)

(eval-and-compile
  (if (version< emacs-version "24.3")
      (error "Sly requires at least Emacs 24.3")))

(eval-and-compile
  (or (require 'hyperspec nil t)
      (require 'hyperspec "lib/hyperspec")))
(require 'thingatpt)
(require 'comint)
(require 'pp)
(require 'easymenu)
(require 'arc-mode)
(require 'etags)
(require 'apropos)
(require 'bytecomp) ;; for `byte-compile-current-file' and
;; `sly-byte-compile-hotspots'.

(require 'sly-common     "lib/sly-common")
(require 'sly-messages   "lib/sly-messages")
(require 'sly-buttons    "lib/sly-buttons")
(require 'sly-completion "lib/sly-completion")

(require 'gv) ; for gv--defsetter

(eval-when-compile
  (require 'compile)
  (require 'gud))

(defvar sly-path nil
  "Directory containing the SLY package.
This is used to load the supporting Common Lisp library, Slynk.
The default value is automatically computed from the location of the
Emacs Lisp package.")

;; Determine `sly-path' at load time, regardless of filename (.el or
;; .elc) being loaded.
;;
(setq sly-path
      (if load-file-name
          (file-name-directory load-file-name)
        (error "[sly] fatal: impossible to determine sly-path")))

(defun sly-slynk-path ()
  "Path where the bundled Slynk server is located."
  (expand-file-name "slynk/" sly-path))

;;;###autoload
(define-obsolete-variable-alias 'sly-setup-contribs
  'sly-contribs "2.3.2")
;;;###autoload
(defvar sly-contribs '(sly-fancy)
  "A list of contrib packages to load with SLY.")

;;;###autoload
(defun sly-setup (&optional contribs)
  "Have SLY load and use extension modules CONTRIBS.
CONTRIBS defaults to `sly-contribs' and is a list (LIB1 LIB2...)
symbols of `provide'd and `require'd Elisp libraries.

If CONTRIBS is nil, `sly-contribs' is *not* affected, otherwise
it is set to CONTRIBS.

However, after `require'ing LIB1, LIB2 ..., this command invokes
additional initialization steps associated with each element
LIB1, LIB2, which can theoretically be reverted by
`sly-disable-contrib.'

Notably, one of the extra initialization steps is affecting the
value of `sly-required-modules' (which see) thus affecting the
libraries loaded in the Slynk servers.

If SLY is currently connected to a Slynk and a contrib in
CONTRIBS has never been loaded, that Slynk is told to load the
associated Slynk extension module.

To ensure that a particular contrib is loaded, use
`sly-enable-contrib' instead."
  ;; FIXME: The contract should be like some hypothetical
  ;; `sly-refresh-contribs'
  ;;
  (interactive)
  (when contribs
    (setq sly-contribs contribs))
  (sly--setup-contribs))

(defvaralias 'sly-required-modules 'sly-contrib--required-slynk-modules)

(defvar sly-contrib--required-slynk-modules '()
  "Alist of (MODULE . (WHERE CONTRIB)) for slynk-provided features.

MODULE is a symbol naming a specific Slynk feature, WHERE is
the full pathname to the directory where the file(s)
providing the feature are found and CONTRIB is a symbol as found
in `sly-contribs.'")

(cl-defmacro sly--contrib-safe (contrib &body body)
  "Run BODY catching and resignalling any errors for CONTRIB"
  (declare (indent 1))
  `(condition-case-unless-debug e
       (progn
         ,@body)
     (error (sly-error "There's an error in %s: %s"
                       ,contrib
                       e))))

(defun sly--setup-contribs ()
  "Load and initialize contribs."
  ;; active    != enabled
  ;;   ^            ^
  ;;   |            |
  ;;   v            v
  ;; forgotten != disabled
  (add-to-list 'load-path (expand-file-name "contrib" sly-path))
  (mapc (lambda (c)
          (sly--contrib-safe c (require c)))
        sly-contribs)
  (let* ((all-active-contribs
          ;; these are the contribs the user chose to activate
          ;;
          (mapcar #'sly-contrib--find-contrib
                  (cl-reduce #'append (mapcar #'sly-contrib--all-dependencies
                                              sly-contribs))))
         (defined-but-forgotten-contribs
           ;; "forgotten contribs" are the ones the chose not to
           ;; activate but whose definitions we have seen
           ;;
           (cl-remove-if #'(lambda (contrib)
                             (memq contrib all-active-contribs))
                         (sly-contrib--all-contribs))))
    ;; Disable any forgotten contribs that are enabled right now.
    ;;
    (cl-loop for to-disable in defined-but-forgotten-contribs
             when (sly--contrib-safe to-disable
                    (sly-contrib--enabled-p to-disable))
             do (funcall (sly-contrib--disable to-disable)))
    ;; Enable any active contrib that is *not* enabled right now.
    ;;
    (cl-loop for to-enable in all-active-contribs
             unless (sly--contrib-safe to-enable
                      (sly-contrib--enabled-p to-enable))
             do (funcall (sly-contrib--enable to-enable)))
    ;; Some contribs add stuff to `sly-mode-hook' or
    ;; `sly-editing-hook', so make sure we re-run those hooks now.
    (when all-active-contribs
      (defvar sly-editing-mode)         ;FIXME: Forward reference!
      (cl-loop for buffer in (buffer-list)
               do (with-current-buffer buffer
                    (when sly-editing-mode (sly-editing-mode 1)))))))

(eval-and-compile
  (defun sly-version (&optional interactive file)
    "Read SLY's version of its own sly.el file.
If FILE is passed use that instead to discover the version."
    (interactive "p")
    (let ((version
           (with-temp-buffer
             (insert-file-contents
              (or file
                  (expand-file-name "sly.el" sly-path))
              nil 0 200)
             (and (search-forward-regexp
                   ";;[[:space:]]*Version:[[:space:]]*\\(.*\\)$" nil t)
                  (match-string 1)))))
      (if interactive
          (sly-message "SLY %s" version)
        version))))

(defvar sly-protocol-version nil)

(setq sly-protocol-version
      ;; Compile the version string into the generated .elc file, but
      ;; don't actualy affect `sly-protocol-version' until load-time.
      ;;
      (eval-when-compile (sly-version nil (or load-file-name
                                              byte-compile-current-file))))


;;;; Customize groups
;;
;;;;; sly

(defgroup sly nil
  "Interaction with the Superior Lisp Environment."
  :prefix "sly-"
  :group 'applications)

;;;;; sly-ui

(defgroup sly-ui nil
  "Interaction with the Superior Lisp Environment."
  :prefix "sly-"
  :group 'sly)

(defcustom sly-truncate-lines t
  "Set `truncate-lines' in popup buffers.
This applies to buffers that present lines as rows of data, such as
debugger backtraces and apropos listings."
  :type 'boolean
  :group 'sly-ui)

(defcustom sly-kill-without-query-p nil
  "If non-nil, kill SLY processes without query when quitting Emacs.
This applies to the *inferior-lisp* buffer and the network connections."
  :type 'boolean
  :group 'sly-ui)

;;;;; sly-lisp

(defgroup sly-lisp nil
  "Lisp server configuration."
  :prefix "sly-"
  :group 'sly)

(defcustom sly-ignore-protocol-mismatches nil
  "If non-nil, ignore protocol mismatches between SLY and Slynk.
Programatically, this variable can be let-bound around calls to
`sly' or `sly-connect'."
  :type 'boolean
  :group 'sly)

(defcustom sly-init-function 'sly-init-using-asdf
  "Function bootstrapping slynk on the remote.

Value is a function of two arguments: SLYNK-PORTFILE and an
ingored argument for backward compatibility. Function should
return a string issuing very first commands issued by Sly to
the remote-connection process. Some time after this there should
be a port number ready in SLYNK-PORTFILE."
  :type '(choice (const :tag "Use ASDF"
                        sly-init-using-asdf)
                 (const :tag "Use legacy slynk-loader.lisp"
                        sly-init-using-slynk-loader))
  :group 'sly-lisp)

(define-obsolete-variable-alias 'sly-backend
  'sly-slynk-loader-backend "3.0")

(defcustom sly-slynk-loader-backend "slynk-loader.lisp"
  "The name of the slynk-loader that loads the Slynk server.
Only applicable if `sly-init-function' is set to
`sly-init-using-slynk-loader'. This name is interpreted
relative to the directory containing sly.el, but could also be
set to an absolute filename."
  :type 'string
  :group 'sly-lisp)

(defcustom sly-connected-hook nil
  "List of functions to call when SLY connects to Lisp."
  :type 'hook
  :group 'sly-lisp)

(defcustom sly-enable-evaluate-in-emacs nil
  "*If non-nil, the inferior Lisp can evaluate arbitrary forms in Emacs.
The default is nil, as this feature can be a security risk."
  :type '(boolean)
  :group 'sly-lisp)

(defcustom sly-lisp-host "localhost"
  "The default hostname (or IP address) to connect to."
  :type 'string
  :group 'sly-lisp)

(defcustom sly-port 4005
  "Port to use as the default for `sly-connect'."
  :type 'integer
  :group 'sly-lisp)

(defvar sly-connect-host-history (list sly-lisp-host))
(defvar sly-connect-port-history (list (prin1-to-string sly-port)))

(defvar sly-net-valid-coding-systems
  '((iso-latin-1-unix nil "iso-latin-1-unix")
    (iso-8859-1-unix  nil "iso-latin-1-unix")
    (binary           nil "iso-latin-1-unix")
    (utf-8-unix       t   "utf-8-unix")
    (emacs-mule-unix  t   "emacs-mule-unix")
    (euc-jp-unix      t   "euc-jp-unix"))
  "A list of valid coding systems.
Each element is of the form: (NAME MULTIBYTEP CL-NAME)")

(defun sly-find-coding-system (name)
  "Return the coding system for the symbol NAME.
The result is either an element in `sly-net-valid-coding-systems'
of nil."
  (let ((probe (assq name sly-net-valid-coding-systems)))
    (when (and probe (if (fboundp 'check-coding-system)
                         (ignore-errors (check-coding-system (car probe)))
                       (eq (car probe) 'binary)))
      probe)))

(defcustom sly-net-coding-system
  (car (cl-find-if 'sly-find-coding-system
                   sly-net-valid-coding-systems :key 'car))
  "Coding system used for network connections.
See also `sly-net-valid-coding-systems'."
  :type (cons 'choice
              (mapcar (lambda (x)
                        (list 'const (car x)))
                      sly-net-valid-coding-systems))
  :group 'sly-lisp)

;;;;; sly-mode

(defgroup sly-mode nil
  "Settings for sly-mode Lisp source buffers."
  :prefix "sly-"
  :group 'sly)

;;;;; sly-mode-faces

(defgroup sly-mode-faces nil
  "Faces in sly-mode source code buffers."
  :prefix "sly-"
  :group 'sly-mode)

(defface sly-error-face
  `((((class color) (background light))
     (:underline "tomato"))
    (((class color) (background dark))
     (:underline "tomato"))
    (t (:underline t)))
  "Face for errors from the compiler."
  :group 'sly-mode-faces)

(defface sly-warning-face
  `((((class color) (background light))
     (:underline "orange"))
    (((class color) (background dark))
     (:underline "coral"))
    (t (:underline t)))
  "Face for warnings from the compiler."
  :group 'sly-mode-faces)

(defface sly-style-warning-face
  `((((class color) (background light))
     (:underline "olive drab"))
    (((class color) (background dark))
     (:underline "khaki"))
    (t (:underline t)))
  "Face for style-warnings from the compiler."
  :group 'sly-mode-faces)

(defface sly-note-face
  `((((class color) (background light))
     (:underline "brown3"))
    (((class color) (background dark))
     (:underline "light goldenrod"))
    (t (:underline t)))
  "Face for notes from the compiler."
  :group 'sly-mode-faces)

;;;;; sly-db

(defgroup sly-debugger nil
  "Backtrace options and fontification."
  :prefix "sly-db-"
  :group 'sly)

(defmacro define-sly-db-faces (&rest faces)
  "Define the set of SLY-DB faces.
Each face specifiation is (NAME DESCRIPTION &optional PROPERTIES).
NAME is a symbol; the face will be called sly-db-NAME-face.
DESCRIPTION is a one-liner for the customization buffer.
PROPERTIES specifies any default face properties."
  `(progn ,@(cl-loop for face in faces
                     collect `(define-sly-db-face ,@face))))

(defmacro define-sly-db-face (name description &optional default)
  (let ((facename (intern (format "sly-db-%s-face" (symbol-name name)))))
    `(defface ,facename
       (list (list t ,default))
       ,(format "Face for %s." description)
       :group 'sly-debugger)))

(define-sly-db-faces
  (topline        "the top line describing the error")
  (condition "the condition class" '(:inherit error))
  (section        "the labels of major sections in the debugger buffer"
                  '(:inherit header-line))
  (frame-label    "backtrace frame numbers"
                  '(:inherit shadow))
  (restart        "restart descriptions")
  (restart-number "restart numbers (correspond to keystrokes to invoke)"
                  '(:inherit shadow))
  (frame-line     "function names and arguments in the backtrace")
  (restartable-frame-line
   "frames which are surely restartable"
   '(:inherit font-lock-constant-face))
  (non-restartable-frame-line
   "frames which are surely not restartable")
  (local-name     "local variable names")
  (catch-tag      "catch tags"))


;;;;; Key bindings
(defvar sly-doc-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") 'sly-apropos)
    (define-key map (kbd "C-z") 'sly-apropos-all)
    (define-key map (kbd "C-p") 'sly-apropos-package)
    (define-key map (kbd "C-d") 'sly-describe-symbol)
    (define-key map (kbd "C-f") 'sly-describe-function)
    (define-key map (kbd "C-h") 'sly-documentation-lookup)
    (define-key map (kbd "~") 'common-lisp-hyperspec-format)
    (define-key map (kbd "C-g") 'common-lisp-hyperspec-glossary-term)
    (define-key map (kbd "#") 'common-lisp-hyperspec-lookup-reader-macro)
    map))

(defvar sly-who-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c") 'sly-who-calls)
    (define-key map (kbd "C-w") 'sly-calls-who)
    (define-key map (kbd "C-r") 'sly-who-references)
    (define-key map (kbd "C-b") 'sly-who-binds)
    (define-key map (kbd "C-s") 'sly-who-sets)
    (define-key map (kbd "C-m") 'sly-who-macroexpands)
    (define-key map (kbd "C-a") 'sly-who-specializes)
    map))

(defvar sly-selector-map (let ((map (make-sparse-keymap)))
                           (define-key map "c" 'sly-list-connections)
                           (define-key map "t" 'sly-list-threads)
                           (define-key map "d" 'sly-db-pop-to-debugger-maybe)
                           (define-key map "e" 'sly-pop-to-events-buffer)
                           (define-key map "i" 'sly-inferior-lisp-buffer)
                           (define-key map "l" 'sly-switch-to-most-recent)
                           map)
  "A keymap for frequently used SLY shortcuts.
Access to this keymap can be installed in in
`sly-mode-map', using something like

   (global-set-key (kbd \"C-z\") sly-selector-map)

This will bind C-z to this prefix map, one keystroke away from
the available shortcuts:

\\{sly-selector-map}
As usual, users or extensions can plug in
any command into it using

  (define-key sly-selector-map (kbd \"k\") 'sly-command)

Where \"k\" is the key to bind and \"sly-command\" is any
interactive command.\".")

(defvar sly-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-r")   'sly-eval-region)
    (define-key map (kbd ":")     'sly-interactive-eval)
    (define-key map (kbd "C-e")   'sly-interactive-eval)
    (define-key map (kbd "E")     'sly-edit-value)
    (define-key map (kbd "C-l")   'sly-load-file)
    (define-key map (kbd "C-b")   'sly-interrupt)
    (define-key map (kbd "M-d")   'sly-disassemble-symbol)
    (define-key map (kbd "C-t")   'sly-toggle-trace-fdefinition)
    (define-key map (kbd "I")     'sly-inspect)
    (define-key map (kbd "C-x t") 'sly-list-threads)
    (define-key map (kbd "C-x n") 'sly-next-connection)
    (define-key map (kbd "C-x c") 'sly-list-connections)
    (define-key map (kbd "C-x p") 'sly-prev-connection)
    (define-key map (kbd "<")     'sly-list-callers)
    (define-key map (kbd ">")     'sly-list-callees)
    ;; Include DOC keys...
    (define-key map (kbd "C-d")  sly-doc-map)
    ;; Include XREF WHO-FOO keys...
    (define-key map (kbd "C-w")  sly-who-map)
    ;; `sly-selector-map' used to be bound to "C-c C-s" by default,
    ;; but sly-stickers has a better binding for that.
    ;;
    ;; (define-key map (kbd "C-s") sly-selector-map)
    map))

(defvar sly-mode-map
  (let ((map (make-sparse-keymap)))
    ;; These used to be a `sly-parent-map'
    (define-key map (kbd "M-.")     'sly-edit-definition)
    (define-key map (kbd "M-,")     'sly-pop-find-definition-stack)
    (define-key map (kbd "M-_")     'sly-edit-uses)    ; for German layout
    (define-key map (kbd "M-?")     'sly-edit-uses)    ; for USian layout
    (define-key map (kbd "C-x 4 .") 'sly-edit-definition-other-window)
    (define-key map (kbd "C-x 5 .") 'sly-edit-definition-other-frame)
    (define-key map (kbd "C-x C-e") 'sly-eval-last-expression)
    (define-key map (kbd "C-M-x")   'sly-eval-defun)
    ;; Include PREFIX keys...
    (define-key map (kbd "C-c")     sly-prefix-map)
    ;; Completion
    (define-key map (kbd "C-c TAB") 'completion-at-point)
    ;; Evaluating
    (define-key map (kbd "C-c C-p") 'sly-pprint-eval-last-expression)
    ;; Macroexpand
    (define-key map (kbd "C-c C-m") 'sly-expand-1)
    (define-key map (kbd "C-c M-m") 'sly-macroexpand-all)
    ;; Misc
    (define-key map (kbd "C-c C-u") 'sly-undefine-function)
    map))

(defvar sly-editing-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-p")     'sly-previous-note)
    (define-key map (kbd "M-n")     'sly-next-note)
    (define-key map (kbd "C-c M-c") 'sly-remove-notes)
    (define-key map (kbd "C-c C-k") 'sly-compile-and-load-file)
    (define-key map (kbd "C-c M-k") 'sly-compile-file)
    (define-key map (kbd "C-c C-c") 'sly-compile-defun)
    map))

(defvar sly-popup-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    map))


;;;; Minor modes

;;;;; sly-mode
(defvar sly-buffer-connection)
(defvar sly-dispatching-connection)
(defvar sly-current-thread)

;; exceptional forward decl
(defvar company-tooltip-align-annotations)

;;;###autoload
(define-minor-mode sly-mode
  "Minor mode for horizontal SLY functionality."
  nil nil nil
  ;; Company-mode should have this by default
  ;; See gh#166
  (set (make-local-variable 'company-tooltip-align-annotations) t))

(defun sly--lisp-indent-function (&rest args)
  (let ((fn (if (fboundp 'sly-common-lisp-indent-function)
                #'sly-common-lisp-indent-function
              #'lisp-indent-function)))
    (apply fn args)))

;;;###autoload
(define-minor-mode sly-editing-mode
  "Minor mode for editing `lisp-mode' buffers."
  nil nil nil
  (sly-mode 1)
  (setq-local lisp-indent-function #'sly--lisp-indent-function))

(define-minor-mode sly-popup-buffer-mode
  "Minor mode for all read-only SLY buffers"
  nil nil nil
  (sly-mode 1)
  (sly-interactive-buttons-mode 1)
  (setq buffer-read-only t))


;;;;;; Mode-Line
(defface sly-mode-line
  '((t (:inherit font-lock-constant-face
                 :weight bold)))
  "Face for package-name in SLY's mode line."
  :group 'sly)

(defvar sly--mode-line-format `(:eval (sly--mode-line-format)))

(put 'sly--mode-line-format 'risky-local-variable t)

(defvar sly-menu) ;; forward referenced

(defvar sly-extra-mode-line-constructs nil
  "A list of mode-line constructs to add to SLY's mode-line.
Each construct is separated by a \"/\" and may be a regular
mode-line construct or a symbol naming a function of no arguments
that returns one such construct.")

(defun sly--mode-line-format ()
  (let* ((conn (sly-current-connection))
         (conn (and (process-live-p conn) conn))
         (name (or (and conn
                        (sly-connection-name conn))
                   "*"))
         (pkg (sly-current-package))
         (format-number (lambda (n) (cond ((and n (not (zerop n)))
                                           (format "%d" n))
                                          (n "-")
                                          (t "*"))))
         (package-name (and pkg
                            (sly--pretty-package-name pkg)))
         (pending (and conn
                       (length (sly-rex-continuations conn))))
         (sly-dbs (and conn (length (sly-db-buffers conn)))))
    `((:propertize "sly"
                   face sly-mode-line
                   keymap ,(let ((map (make-sparse-keymap)))
                             (define-key map [mode-line down-mouse-1]
                               sly-menu)
                             map)
                   mouse-face mode-line-highlight
                   help-echo "mouse-1: pop-up SLY menu"
                   )
      " "
      (:propertize ,name
                   face sly-mode-line
                   keymap ,(let ((map (make-sparse-keymap)))
                             (define-key map [mode-line mouse-1] 'sly-prev-connection)
                             (define-key map [mode-line mouse-2] 'sly-list-connections)
                             (define-key map [mode-line mouse-3] 'sly-next-connection)
                             map)
                   mouse-face mode-line-highlight
                   help-echo ,(concat "mouse-1: previous connection\n"
                                      "mouse-2: list connections\n"
                                      "mouse-3: next connection"))
      "/"
      ,(or package-name "*")
      "/"
      (:propertize ,(funcall format-number pending)
                   help-echo ,(if conn (format "%s pending events outgoing\n%s"
                                               pending
                                               (concat "mouse-1: go to *sly-events* buffer"
                                                       "mouse-3: forget pending continuations"))
                                "No current connection")
                   mouse-face mode-line-highlight
                   face ,(cond ((and pending (cl-plusp pending))
                                'warning)
                               (t
                                'sly-mode-line))
                   keymap ,(let ((map (make-sparse-keymap)))
                             (define-key map [mode-line mouse-1] 'sly-pop-to-events-buffer)
                             (define-key map [mode-line mouse-3] 'sly-forget-pending-events)
                             map))
      "/"
      (:propertize ,(funcall format-number sly-dbs)
                   help-echo ,(if conn (format "%s SLY-DB buffers waiting\n%s"
                                               pending
                                               "mouse-1: go to first one")
                                "No current connection")
                   mouse-face mode-line-highlight
                   face ,(cond ((and sly-dbs (cl-plusp sly-dbs))
                                'warning)
                               (t
                                'sly-mode-line))
                   keymap ,(let ((map (make-sparse-keymap)))
                             (define-key map [mode-line mouse-1] 'sly-db-pop-to-debugger)
                             map))
      ,@(cl-loop for construct in sly-extra-mode-line-constructs
                 collect "/"
                 collect (if (and (symbolp construct)
                                  (fboundp construct))
                             (condition-case _oops
                                 (funcall construct)
                               (error "*sly-invalid*"))
                           construct)))))

(defun sly--refresh-mode-line ()
  (force-mode-line-update t))

(defun sly--pretty-package-name (name)
  "Return a pretty version of a package name NAME."
  (cond ((string-match "^#?:\\(.*\\)$" name)
         (match-string 1 name))
        ((string-match "^\"\\(.*\\)\"$" name)
         (match-string 1 name))
        (t name)))

(add-to-list 'mode-line-misc-info
             `(sly-mode (" [" sly--mode-line-format "] ")))


;;;; Framework'ey bits
;;;
;;; This section contains some standard SLY idioms: basic macros,
;;; ways of showing messages to the user, etc. All the code in this
;;; file should use these functions when applicable.
;;;
;;;;; Syntactic sugar

(cl-defmacro sly--when-let ((var value) &rest body)
  "Evaluate VALUE, if the result is non-nil bind it to VAR and eval BODY.

\(fn (VAR VALUE) &rest BODY)"
  (declare (indent 1))
  `(let ((,var ,value))
     (when ,var ,@body)))

(cl-defmacro sly--when-let* (bindings &rest body)
  "Same as `sly--when-let', but for multiple BINDINGS"
  (declare (indent 1))
  (if bindings
      `(sly--when-let ,(car bindings)
         (sly--when-let* ,(cdr bindings) ,@body))
    `(progn ,@body)))

(defmacro sly-dcase (value &rest patterns)
  (declare (indent 1)
           (debug (sexp &rest (sexp &rest form))))
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (cl-gensym "op-"))
        (operands (cl-gensym "rand-"))
        (tmp (cl-gensym "tmp-")))
    `(let* ((,tmp ,value)
            (,operator (car ,tmp))
            (,operands (cdr ,tmp)))
       (cl-case ,operator
         ,@(mapcar (lambda (clause)
                     (if (eq (car clause) t)
                         `(t ,@(cdr clause))
                       (cl-destructuring-bind ((op &rest rands) &rest body)
                           clause
                         `(,op (cl-destructuring-bind ,rands ,operands
                                 . ,(or body
                                        '((ignore)) ; suppress some warnings
                                        ))))))
                   patterns)
         ,@(if (eq (caar (last patterns)) t)
               '()
             `((t (sly-error "Elisp sly-dcase failed: %S" ,tmp))))))))

;;;;; Very-commonly-used functions

;; Interface
(cl-defun sly-buffer-name (type &key connection hidden suffix)
  (cl-assert (keywordp type))
  (mapconcat #'identity
             `(,@(if hidden `(" "))
               "*sly-"
               ,(downcase (substring (symbol-name type) 1))
               ,@(if connection
                     `(" for "
                       ,(sly-connection-name
                         (if (eq connection t)
                             (sly-current-connection)
                           connection))))
               ,@(if suffix
                     `(" ("
                       ,suffix
                       ")"))
               "*")
             ""))

(defun sly-recenter (target &optional move-point)
  "Make the region between point and TARGET visible.
Minimize window motion if possible.  If MOVE-POINT allow point to
move to make TARGET visible."
  (unless (pos-visible-in-window-p target)
    (redisplay)
    (let ((screen-line (- (line-number-at-pos)
                          (line-number-at-pos (window-start))))
          (window-end (line-number-at-pos (window-end)))
          (window-start (line-number-at-pos (window-start)))
          (target-line (line-number-at-pos target))
          recenter-arg)
      (cond ((> (point) target)
             (setq recenter-arg (+ screen-line (- window-start target-line)))
             (if (or (not move-point)
                     (<= recenter-arg (window-height)))
                 (recenter recenter-arg)
               (goto-char target)
               (recenter -1)
               (move-to-window-line -1)))
            ((<= (point) target)
             (setq recenter-arg (- screen-line (- target-line window-end)))
             (if (or (not move-point)
                     (> recenter-arg 0))
                 (recenter (max recenter-arg 0))
               (goto-char target)
               (recenter 0)
               (move-to-window-line 0)))))))

;; Interface
(defun sly-set-truncate-lines ()
  "Apply `sly-truncate-lines' to the current buffer."
  (when sly-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

;; Interface
(defun sly-read-package-name (prompt &optional initial-value allow-blank)
  "Read a package name from the minibuffer, prompting with PROMPT.
If ALLOW-BLANK may return nil to signal no particular package
selected."
  (let* ((completion-ignore-case t)
         (res (completing-read
               (concat "[sly] " prompt)
               (sly-eval
                `(slynk:list-all-package-names t))
               nil (not allow-blank) initial-value)))
    (unless (zerop (length res))
      res)))

;; Interface
(defmacro sly-propertize-region (props &rest body)
  "Execute BODY and add PROPS to all the text it inserts.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY."
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((start (cl-gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
         (add-text-properties ,start (point) ,props)))))

(defun sly-add-face (face string)
  (declare (indent 1))
  (add-text-properties 0 (length string) (list 'face face) string)
  string)

;; Interface
(defsubst sly-insert-propertized (props &rest args)
  "Insert all ARGS and then add text-PROPS to the inserted text."
  (sly-propertize-region props (apply #'insert args)))

(defmacro sly-with-rigid-indentation (level &rest body)
  "Execute BODY and then rigidly indent its text insertions.
Assumes all insertions are made at point."
  (declare (indent 1))
  (let ((start (cl-gensym)) (l (cl-gensym)))
    `(let ((,start (point)) (,l ,(or level '(current-column))))
       (prog1 (progn ,@body)
         (sly-indent-rigidly ,start (point) ,l)))))

(defun sly-indent-rigidly (start end column)
  ;; Similar to `indent-rigidly' but doesn't inherit text props.
  (let ((indent (make-string column ?\ )))
    (save-excursion
      (goto-char end)
      (beginning-of-line)
      (while (and (<= start (point))
                  (progn
                    (insert-before-markers indent)
                    (zerop (forward-line -1))))))))

(defun sly-insert-indented (&rest strings)
  "Insert all arguments rigidly indented."
  (sly-with-rigid-indentation nil
    (apply #'insert strings)))

(defun sly-compose (&rest functions)
  "Compose unary FUNCTIONS right-associatively, returning a function"
  #'(lambda (x)
      (cl-reduce #'funcall functions :initial-value x :from-end t)))

(defun sly-curry (fun &rest args)
  "Partially apply FUN to ARGS.  The result is a new function."
  (lambda (&rest more) (apply fun (append args more))))

(defun sly-rcurry (fun &rest args)
  "Like `sly-curry' but ARGS on the right are applied."
  (lambda (&rest more) (apply fun (append more args))))


;;;;; Temporary popup buffers

;; keep compiler quiet
(defvar sly-buffer-package)
(defvar sly-buffer-connection)


;; Interface
(cl-defmacro sly-with-popup-buffer ((name &key package connection select
                                          same-window-p
                                          mode)
                                    &body body)
  "Similar to `with-output-to-temp-buffer'.
Bind standard-output and initialize some buffer-local variables.
Restore window configuration when closed.  NAME is the name of
the buffer to be created.  PACKAGE is the value
`sly-buffer-package'.  CONNECTION is the value for
`sly-buffer-connection', if nil, no explicit connection is
associated with the buffer.  If t, the current connection is
taken.  MODE is the name of a major mode which will be enabled.
Non-nil SELECT indicates the buffer should be switched to, unless
it is `:hidden' meaning the buffer should not even be
displayed. SELECT can also be `:raise' meaning the buffer should
be switched to and the frame raised.  SAME-WINDOW-P is a form
indicating if the popup *can* happen in the same window. The
forms SELECT and SAME-WINDOW-P are evaluated at runtime, not
macroexpansion time.
"
  (declare (indent 1)
           (debug (sexp &rest form)))
  (let* ((package-sym (cl-gensym "package-"))
         (connection-sym (cl-gensym "connection-"))
         (select-sym (cl-gensym "select"))
         (major-mode-sym (cl-gensym "select")))
    `(let ((,package-sym ,(if (eq package t)
                              `(sly-current-package)
                            package))
           (,connection-sym ,(if (eq connection t)
                                 `(sly-current-connection)
                               connection))
           (,major-mode-sym major-mode)
           (,select-sym ,select)
           (view-read-only nil))
       (with-current-buffer (get-buffer-create ,name)
         (let ((inhibit-read-only t)
               (standard-output (current-buffer)))
           (erase-buffer)
           ,@(cond (mode
                    `((funcall ,mode)))
                   (t
                    `((sly-popup-buffer-mode 1))))
           (setq sly-buffer-package ,package-sym
                 sly-buffer-connection ,connection-sym)
           (set-syntax-table lisp-mode-syntax-table)
           ,@body
           (unless (eq ,select-sym :hidden)
             (let ((window (display-buffer
                            (current-buffer)
                            (if ,(cond (same-window-p same-window-p)
                                       (mode `(eq ,major-mode-sym ,mode)))
                                nil
                              t))))
               (when ,select-sym
                 (if window
                     (select-window window t))))
             (if (eq ,select-sym :raise) (raise-frame)))
           (current-buffer))))))

;;;;; Filename translation
;;;
;;; Filenames passed between Emacs and Lisp should be translated using
;;; these functions. This way users who run Emacs and Lisp on separate
;;; machines have a chance to integrate file operations somehow.

(defvar sly-to-lisp-filename-function #'convert-standard-filename
  "Function to translate Emacs filenames to CL namestrings.")
(defvar sly-from-lisp-filename-function #'identity
  "Function to translate CL namestrings to Emacs filenames.")

(defun sly-to-lisp-filename (filename)
  "Translate the string FILENAME to a Lisp filename."
  (funcall sly-to-lisp-filename-function (substring-no-properties filename)))

(defun sly-from-lisp-filename (filename)
  "Translate the Lisp filename FILENAME to an Emacs filename."
  (funcall sly-from-lisp-filename-function filename))


;;;; Starting SLY
;;;
;;; This section covers starting an inferior-lisp, compiling and
;;; starting the server, initiating a network connection.

;;;;; Entry points

;; We no longer load inf-lisp, but we use this variable for backward
;; compatibility.
(defcustom inferior-lisp-program "lisp"
  "Program name for starting a Lisp subprocess to Emacs.
Can be a string naming a program, a whitespace-separated string
of \"EXECUTABLE ARG1 ARG2\" or a list (EXECUTABLE ARGS...) where
EXECUTABLE and ARGS are strings."
  :type 'string
  :group 'sly-lisp)

(defvar sly-lisp-implementations nil
  "*A list of known Lisp implementations.
The list should have the form:
  ((NAME (PROGRAM PROGRAM-ARGS...) &key KEYWORD-ARGS) ...)

NAME is a symbol for the implementation.
PROGRAM and PROGRAM-ARGS are strings used to start the Lisp process.
For KEYWORD-ARGS see `sly-start'.

Here's an example:
 ((cmucl (\"/opt/cmucl/bin/lisp\" \"-quiet\") :init sly-init-command)
  (acl (\"acl7\") :coding-system emacs-mule))")

(defcustom sly-command-switch-to-existing-lisp 'ask
  "Should the `sly' command start new lisp if one is available?"
  :type '(choice (const :tag "Ask the user" ask)
                 (const :tag "Always" 'always)
                 (const :tag "Never" 'never)))

(defcustom sly-auto-select-connection 'ask
  "Controls auto selection after the default connection was closed."
  :group 'sly-mode
  :type '(choice (const never)
                 (const always)
                 (const ask)))

(defcustom sly-default-lisp nil
  "A symbol naming the preferred Lisp implementation.
See `sly-lisp-implementations'"
  :type 'function
  :group 'sly-mode)

;; dummy definitions for the compiler
(defvar sly-net-processes)
(defvar sly-default-connection)

;;;###autoload
(cl-defun sly (&optional command coding-system interactive)
  "Start a Lisp implementation and connect to it.

  COMMAND designates a the Lisp implementation to start as an
\"inferior\" process to the Emacs process. It is either a
pathname string pathname to a lisp executable, a list (EXECUTABLE
ARGS...), or a symbol indexing
`sly-lisp-implementations'. CODING-SYSTEM is a symbol overriding
`sly-net-coding-system'.

Interactively, both COMMAND and CODING-SYSTEM are nil and the
prefix argument controls the precise behaviour:

- With no prefix arg, try to automatically find a Lisp.  First
  consult `sly-command-switch-to-existing-lisp' and analyse open
  connections to maybe switch to one of those.  If a new lisp is
  to be created, first lookup `sly-lisp-implementations', using
  `sly-default-lisp' as a default strategy.  Then try
  `inferior-lisp-program' if it looks like it points to a valid
  lisp.  Failing that, guess the location of a lisp
  implementation.

- With a positive prefix arg (one C-u), prompt for a command
  string that starts a Lisp implementation.

- With a negative prefix arg (M-- M-x sly, for example) prompt
  for a symbol indexing one of the entries in
  `sly-lisp-implementations'"
  (interactive (list nil nil t))
  (sly--when-let*
      ((active (and interactive
                    (not current-prefix-arg)
                    (sly--purge-connections)))
       (target (or (and (eq sly-command-switch-to-existing-lisp 'ask)
                        (sly-prompt-for-connection
                         "[sly] Switch to open connection?\n\
  (Customize `sly-command-switch-to-existing-lisp' to avoid this prompt.)\n\
  Connections: " nil "(start a new one)"))
                   (and (eq sly-command-switch-to-existing-lisp 'always)
                        (car active)))))
    (sly-message "Switching to `%s'" (sly-connection-name target))
    (sly-connection-list-default-action target)
    (cl-return-from sly nil))
  (let ((command (or command inferior-lisp-program))
        (sly-net-coding-system (or coding-system sly-net-coding-system)))
    (apply #'sly-start
           (cond (interactive
                  (sly--read-interactive-args))
                 (t
                  (if sly-lisp-implementations
                      (sly--lookup-lisp-implementation
                       sly-lisp-implementations
                       (or (and (symbolp command) command)
                           sly-default-lisp
                           (car (car sly-lisp-implementations))))
                    (let ((command-and-args (if (listp command)
                                                command
                                              (split-string command))))
                      `(:program ,(car command-and-args)
                                 :program-args ,(cdr command-and-args)))))))))

(defvar sly-inferior-lisp-program-history '()
  "History list of command strings.  Used by M-x sly.")

(defun sly--read-interactive-args ()
  "Return the list of args which should be passed to `sly-start'.
Helper for M-x sly"
  (cond ((not current-prefix-arg)
         (cond (sly-lisp-implementations
                (sly--lookup-lisp-implementation sly-lisp-implementations
                                                 (or sly-default-lisp
                                                     (car (car sly-lisp-implementations)))))
               (t (cl-destructuring-bind (program &rest args)
                      (split-string-and-unquote
                       (sly--guess-inferior-lisp-program t))
                    (list :program program :program-args args)))))
        ((eq current-prefix-arg '-)
         (let ((key (completing-read
                     "Lisp name: " (mapcar (lambda (x)
                                             (list (symbol-name (car x))))
                                           sly-lisp-implementations)
                     nil t)))
           (sly--lookup-lisp-implementation sly-lisp-implementations (intern key))))
        (t
         (cl-destructuring-bind (program &rest program-args)
             (split-string-and-unquote
              (read-shell-command "[sly] Run lisp: "
                                  (sly--guess-inferior-lisp-program nil)
                                  'sly-inferior-lisp-program-history))
           (let ((coding-system
                  (if (eq 16 (prefix-numeric-value current-prefix-arg))
                      (read-coding-system "[sly] Set sly-coding-system: "
                                          sly-net-coding-system)
                    sly-net-coding-system)))
             (list :program program :program-args program-args
                   :coding-system coding-system))))))


(defun sly--lookup-lisp-implementation (table name)
  (let ((arguments (cl-rest (assoc name table))))
    (unless arguments
      (error "Could not find lisp implementation with the name '%S'" name))
    (when (and (= (length arguments) 1)
               (functionp (cl-first arguments)))
      (setf arguments (funcall (cl-first arguments))))
    (cl-destructuring-bind ((prog &rest args) &rest keys) arguments
      (cl-list* :name name :program prog :program-args args keys))))

(defun sly-inferior-lisp-buffer (sly-process-or-connection &optional pop-to-buffer)
  "Return PROCESS's buffer. With POP-TO-BUFFER, pop to it."
  (interactive (list (sly-process) t))
  (let ((buffer (cond ((and sly-process-or-connection
                            (process-get sly-process-or-connection
                                         'sly-inferior-lisp-process))
                       (process-buffer sly-process-or-connection))
                      (sly-process-or-connection
                       ;; call ourselves recursively with a
                       ;; sly-started process
                       ;;
                       (sly-inferior-lisp-buffer (sly-process sly-process-or-connection)
                                                 pop-to-buffer )))))
    (cond ((and buffer
                pop-to-buffer)
           (pop-to-buffer buffer))
          ((and pop-to-buffer
                sly-process-or-connection)
           (sly-message "No *inferior lisp* process for current connection!"))
          (pop-to-buffer
           (sly-error "No *inferior lisp* buffer")))
    buffer))

(defun sly--guess-inferior-lisp-program (&optional interactive)
  "Compute pathname to a seemingly valid lisp implementation.
If ERRORP, error if such a thing cannot be found"
  (let ((inferior-lisp-program-and-args
         (and inferior-lisp-program
              (if (listp inferior-lisp-program)
                  inferior-lisp-program
                (split-string-and-unquote inferior-lisp-program)))))
    (if (and inferior-lisp-program-and-args
             (executable-find (car inferior-lisp-program-and-args)))
        (combine-and-quote-strings inferior-lisp-program-and-args)
      (let ((guessed (cl-some #'executable-find
                              '("lisp" "sbcl" "clisp" "cmucl"
                                "acl" "alisp"))))
        (cond ((and guessed
                    (or (not interactive)
                        noninteractive
                        (sly-y-or-n-p
                         "Can't find `inferior-lisp-program' (set to `%s'). Use `%s' instead? "
                         inferior-lisp-program guessed)))
               guessed)
              (interactive
               (sly-error
                (substitute-command-keys
                 "Can't find a suitable Lisp. Use \\[sly-info] to read about `Multiple Lisps'")))
              (t
               nil))))))

(cl-defun sly-start (&key (program
                           (sly-error "must supply :program"))
                          program-args
                          directory
                          (coding-system sly-net-coding-system)
                          (init sly-init-function)
                          name
                          (buffer (format "*sly-started inferior-lisp for %s*"
                                          (file-name-nondirectory program)))
                          init-function
                          env)
  "Start a Lisp process and connect to it.
This function is intended for programmatic use if `sly' is not
flexible enough.

PROGRAM and PROGRAM-ARGS are the filename and argument strings
  for the subprocess.
INIT is a function that should return a string to load and start
  Slynk. The function will be called with the PORT-FILENAME and ENCODING as
  arguments.  INIT defaults to `sly-init-function'.
CODING-SYSTEM a symbol for the coding system. The default is
  sly-net-coding-system
ENV environment variables for the subprocess (see `process-environment').
INIT-FUNCTION function to call right after the connection is established.
BUFFER the name of the buffer to use for the subprocess.
NAME a symbol to describe the Lisp implementation
DIRECTORY change to this directory before starting the process.
"
  (let ((args (list :program program :program-args program-args :buffer buffer
                    :coding-system coding-system :init init :name name
                    :init-function init-function :env env)))
    (sly-check-coding-system coding-system)
    (let ((proc (sly-maybe-start-lisp program program-args env
                                      directory buffer)))
      (sly-inferior-connect proc args)
      (sly-inferior-lisp-buffer proc))))

;;;###autoload
(defun sly-connect (host port &optional _coding-system interactive-p)
  "Connect to a running Slynk server. Return the connection.
With prefix arg, asks if all connections should be closed
before."
  (interactive (list (read-from-minibuffer
                      "[sly] Host: " (cl-first sly-connect-host-history)
                      nil nil '(sly-connect-host-history . 1))
                     (string-to-number
                      (read-from-minibuffer
                       "[sly] Port: " (cl-first sly-connect-port-history)
                       nil nil '(sly-connect-port-history . 1)))
                     nil t))
  (when (and interactive-p
             sly-net-processes
             current-prefix-arg
             (sly-y-or-n-p "[sly] Close all connections first? "))
    (sly-disconnect-all))
  (sly-message "Connecting to Slynk on port %S.." port)
  (let* ((process (sly-net-connect host port))
         (sly-dispatching-connection process))
    (sly-setup-connection process)))

;;;;; Start inferior lisp
;;;
;;; Here is the protocol for starting SLY via `M-x sly':
;;;
;;;   1. Emacs starts an inferior Lisp process.
;;;   2. Emacs tells Lisp (via stdio) to load and start Slynk.
;;;   3. Lisp recompiles the Slynk if needed.
;;;   4. Lisp starts the Slynk server and writes its TCP port to a temp file.
;;;   5. Emacs reads the temp file to get the port and then connects.
;;;   6. Emacs prints a message of warm encouragement for the hacking ahead.
;;;
;;; Between steps 2-5 Emacs polls for the creation of the temp file so
;;; that it can make the connection. This polling may continue for a
;;; fair while if Slynk needs recompilation.

(defvar sly-connect-retry-timer nil
  "Timer object while waiting for an inferior-lisp to start.")

(defun sly-abort-connection ()
  "Abort connection the current connection attempt."
  (interactive)
  (cond (sly-connect-retry-timer
         (sly-cancel-connect-retry-timer)
         (sly-message "Cancelled connection attempt."))
        (t (error "Not connecting"))))

;;; Starting the inferior Lisp and loading Slynk:

(defun sly-maybe-start-lisp (program program-args env directory buffer)
  "Return a new or existing inferior lisp process."
  (cond ((not (comint-check-proc buffer))
         (sly-start-lisp program program-args env directory buffer))
        (t (sly-start-lisp program program-args env directory
                           (generate-new-buffer-name buffer)))))

(defvar sly-inferior-process-start-hook nil
  "Hook called whenever a new process gets started.")

(defun sly-start-lisp (program program-args env directory buffer)
  "Does the same as `inferior-lisp' but less ugly.
Return the created process."
  (with-current-buffer (get-buffer-create buffer)
    (when directory
      (cd (expand-file-name directory)))
    (comint-mode)
    (let ((process-environment (append env process-environment))
          (process-connection-type nil))
      (comint-exec (current-buffer) "inferior-lisp" program nil program-args))
    (lisp-mode-variables t)
    (let ((proc (get-buffer-process (current-buffer))))
      (process-put proc 'sly-inferior-lisp-process t)
      (set-process-query-on-exit-flag proc (not sly-kill-without-query-p))
      (run-hooks 'sly-inferior-process-start-hook)
      proc)))

(defun sly-inferior-connect (process args)
  "Start a Slynk server in the inferior Lisp and connect."
  (sly-delete-slynk-port-file 'quiet)
  (sly-start-slynk-server process args)
  (sly-read-port-and-connect process))

(defun sly-start-slynk-server (inf-process args)
  "Start a Slynk server on the inferior lisp."
  (cl-destructuring-bind (&key coding-system init &allow-other-keys) args
    (with-current-buffer (process-buffer inf-process)
      (process-put inf-process 'sly-inferior-lisp-args args)
      (let ((str (funcall init (sly-slynk-port-file) coding-system)))
        (goto-char (process-mark inf-process))
        (insert-before-markers str)
        (process-send-string inf-process str)))))

(defun sly-inferior-lisp-args (inf-process)
  "Return the initial process arguments.
See `sly-start'."
  (process-get inf-process 'sly-inferior-lisp-args))

(defun sly-init-using-asdf (port-filename coding-system)
  "Return a string to initialize Lisp using ASDF.
Fall back to `sly-init-using-slynk-loader' if ASDF fails."
  (format "%S\n\n"
          `(cond ((ignore-errors
                    (funcall 'require "asdf")
                    (funcall (read-from-string "asdf:version-satisfies")
                             (funcall (read-from-string "asdf:asdf-version"))
                             "2.019"))
                  (push (pathname ,(sly-to-lisp-filename (sly-slynk-path)))
                        (symbol-value
                         (read-from-string "asdf:*central-registry*")))
                  (funcall
                   (read-from-string "asdf:load-system")
                   :slynk)
                  (funcall
                   (read-from-string "slynk:start-server")
                   ,(sly-to-lisp-filename port-filename)))
                 (t
                  ,(read (sly-init-using-slynk-loader port-filename
                                                      coding-system))))))

;; XXX load-server & start-server used to be separated. maybe that was  better.
(defun sly-init-using-slynk-loader (port-filename _coding-system)
  "Return a string to initialize Lisp."
  (let ((loader (sly-to-lisp-filename
                 (expand-file-name sly-slynk-loader-backend (sly-slynk-path)))))
    ;; Return a single form to avoid problems with buffered input.
    (format "%S\n\n"
            `(progn
               (load ,loader :verbose t)
               (funcall (read-from-string "slynk-loader:init"))
               (funcall (read-from-string "slynk:start-server")
                        ,port-filename)))))

(defun sly-slynk-port-file ()
  "Filename where the SLYNK server writes its TCP port number."
  (expand-file-name (format "sly.%S" (emacs-pid)) (sly-temp-directory)))

(defun sly-temp-directory ()
  (cond ((fboundp 'temp-directory) (temp-directory))
        ((boundp 'temporary-file-directory) temporary-file-directory)
        (t "/tmp/")))

(defun sly-delete-slynk-port-file (&optional quiet)
  (condition-case data
      (delete-file (sly-slynk-port-file))
    (error
     (cl-ecase quiet
       ((nil) (signal (car data) (cdr data)))
       (quiet)
       (sly-message (sly-message "Unable to delete slynk port file %S"
                                 (sly-slynk-port-file)))))))

(defun sly-read-port-and-connect (inferior-process)
  (sly-attempt-connection inferior-process nil 1))

(defcustom sly-connection-poll-interval 0.3
  "Seconds to wait between connection attempts when first connecting."
  :type 'number
  :group 'sly-ui)

(defun sly-attempt-connection (process retries attempt)
  ;; A small one-state machine to attempt a connection with
  ;; timer-based retries.
  (sly-cancel-connect-retry-timer)
  (let ((file (sly-slynk-port-file)))
    (unless (active-minibuffer-window)
      (sly-message "Polling %S .. %d (Abort with `M-x sly-abort-connection'.)"
                   file attempt))
    (cond ((and (file-exists-p file)
                (> (nth 7 (file-attributes file)) 0)) ; file size
           (let ((port (sly-read-slynk-port))
                 (args (sly-inferior-lisp-args process)))
             (sly-delete-slynk-port-file 'message)
             (let ((c (sly-connect sly-lisp-host port
                                   (plist-get args :coding-system))))
               (sly-set-inferior-process c process))))
          ((and retries (zerop retries))
           (sly-message "Gave up connecting to Slynk after %d attempts." attempt))
          ((eq (process-status process) 'exit)
           (sly-message "Failed to connect to Slynk: inferior process exited."))
          (t
           (when (and (file-exists-p file)
                      (zerop (nth 7 (file-attributes file))))
             (sly-message "(Zero length port file)")
             ;; the file may be in the filesystem but not yet written
             (unless retries (setq retries 3)))
           (cl-assert (not sly-connect-retry-timer))
           (setq sly-connect-retry-timer
                 (run-with-timer
                  sly-connection-poll-interval nil
                  (lambda ()
                    (let ((sly-ignore-protocol-mismatches
                           sly-ignore-protocol-mismatches))
                      (sly-attempt-connection process (and retries (1- retries))
                                              (1+ attempt))))))))))

(defun sly-cancel-connect-retry-timer ()
  (when sly-connect-retry-timer
    (cancel-timer sly-connect-retry-timer)
    (setq sly-connect-retry-timer nil)))

(defun sly-read-slynk-port ()
  "Read the Slynk server port number from the `sly-slynk-port-file'."
  (save-excursion
    (with-temp-buffer
      (insert-file-contents (sly-slynk-port-file))
      (goto-char (point-min))
      (let ((port (read (current-buffer))))
        (cl-assert (integerp port))
        port))))

(defun sly-toggle-debug-on-slynk-error ()
  (interactive)
  (if (sly-eval `(slynk:toggle-debug-on-slynk-error))
      (sly-message "Debug on SLYNK error enabled.")
    (sly-message "Debug on SLYNK error disabled.")))

;;; Words of encouragement

(defun sly-user-first-name ()
  (let ((name (if (string= (user-full-name) "")
                  (user-login-name)
                (user-full-name))))
    (string-match "^[^ ]*" name)
    (capitalize (match-string 0 name))))

(defvar sly-words-of-encouragement
  `("Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    "Your hacking starts... NOW!"
    "May the source be with you!"
    "Take this REPL, brother, and may it serve you well."
    "Lemonodor-fame is but a hack away!"
    "Are we consing yet?"
    ,(format "%s, this could be the start of a beautiful program."
             (sly-user-first-name)))
  "Scientifically-proven optimal words of hackerish encouragement.")

(defun sly-random-words-of-encouragement ()
  "Return a string of hackerish encouragement."
  (eval (nth (random (length sly-words-of-encouragement))
             sly-words-of-encouragement)
        t))


;;;; Networking
;;;
;;; This section covers the low-level networking: establishing
;;; connections and encoding/decoding protocol messages.
;;;
;;; Each SLY protocol message beings with a 6-byte header followed
;;; by an S-expression as text. The sexp must be readable both by
;;; Emacs and by Common Lisp, so if it contains any embedded code
;;; fragments they should be sent as strings:
;;;
;;; The set of meaningful protocol messages are not specified
;;; here. They are defined elsewhere by the event-dispatching
;;; functions in this file and in slynk.lisp.

(defvar sly-net-processes nil
  "List of processes (sockets) connected to Lisps.")

(defvar sly-net-process-close-hooks '()
  "List of functions called when a sly network connection closes.
The functions are called with the process as their argument.")

(defun sly-secret ()
  "Find the magic secret from the user's home directory.
Return nil if the file doesn't exist or is empty; otherwise the
first line of the file."
  (condition-case _err
      (with-temp-buffer
        (insert-file-contents "~/.sly-secret")
        (goto-char (point-min))
        (buffer-substring (point-min) (line-end-position)))
    (file-error nil)))

;;; Interface
(defvar sly--net-connect-counter 0)

(defun sly-send-secret (proc)
  (sly--when-let (secret (sly-secret))
    (let* ((payload (encode-coding-string secret 'utf-8-unix))
           (string (concat (sly-net-encode-length (length payload))
                           payload)))
      (process-send-string proc string))))

(defun sly-net-connect (host port)
  "Establish a connection with a CL."
  (let* ((inhibit-quit nil)
         (name (format "sly-%s" (cl-incf sly--net-connect-counter)))
         (connection (open-network-stream name nil host port))
         (buffer (sly-make-net-buffer (format " *%s*" name))))
    (push connection sly-net-processes)
    (set-process-plist connection `(sly--net-connect-counter
                                    ,sly--net-connect-counter))
    (set-process-buffer connection buffer)
    (set-process-filter connection 'sly-net-filter)
    (set-process-sentinel connection 'sly-net-sentinel)
    (set-process-query-on-exit-flag connection (not sly-kill-without-query-p))
    (when (fboundp 'set-process-coding-system)
      (set-process-coding-system connection 'binary 'binary))
    (sly-send-secret connection)
    connection))

(defun sly-make-net-buffer (name)
  "Make a buffer suitable for a network process."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (set (make-local-variable 'kill-buffer-query-functions) nil))
    buffer))

;;;;; Coding system madness

(defun sly-check-coding-system (coding-system)
  "Signal an error if CODING-SYSTEM isn't a valid coding system."
  (interactive)
  (let ((props (sly-find-coding-system coding-system)))
    (unless props
      (error "Invalid sly-net-coding-system: %s. %s"
             coding-system (mapcar #'car sly-net-valid-coding-systems)))
    (when (and (cl-second props) (boundp 'default-enable-multibyte-characters))
      (cl-assert default-enable-multibyte-characters))
    t))

(defun sly-coding-system-mulibyte-p (coding-system)
  (cl-second (sly-find-coding-system coding-system)))

(defun sly-coding-system-cl-name (coding-system)
  (cl-third (sly-find-coding-system coding-system)))

;;; Interface
(defvar sly-net-send-translator nil
  "If non-nil, function to translate outgoing sexps for the wire.")

(defun sly--sanitize-or-lose (form)
  "Sanitize FORM for Slynk or error."
  (cl-typecase form
    (number)
    (symbol 'fonix)
    (string (set-text-properties 0 (length form) nil form))
    (cons (sly--sanitize-or-lose (car form))
          (sly--sanitize-or-lose (cdr form)))
    (t (sly-error "Can't serialize %s for Slynk." form)))
  form)

(defun sly-net-send (sexp proc)
  "Send a SEXP to Lisp over the socket PROC.
This is the lowest level of communication. The sexp will be READ and
EVAL'd by Lisp."
  (let* ((print-circle nil)
         (print-quoted nil)
         (sexp (sly--sanitize-or-lose sexp))
         (sexp (if (and sly-net-send-translator
                        (fboundp sly-net-send-translator))
                   (funcall sly-net-send-translator sexp)
                 sexp))
         (payload (encode-coding-string
                   (concat (sly-prin1-to-string sexp) "\n")
                   'utf-8-unix))
         (string (concat (sly-net-encode-length (length payload))
                         payload)))
    (sly-log-event sexp proc)
    (process-send-string proc string)))

(defun sly-safe-encoding-p (coding-system string)
  "Return true iff CODING-SYSTEM can safely encode STRING."
  (or (let ((candidates (find-coding-systems-string string))
            (base (coding-system-base coding-system)))
        (or (equal candidates '(undecided))
            (memq base candidates)))
      (and (not (multibyte-string-p string))
           (not (sly-coding-system-mulibyte-p coding-system)))))

(defun sly-net-close (connection reason &optional debug _force)
  "Close the network connection CONNECTION because REASON."
  (process-put connection 'sly-net-close-reason reason)
  (setq sly-net-processes (remove connection sly-net-processes))
  (when (eq connection sly-default-connection)
    (setq sly-default-connection nil))
  ;; Run hooks
  ;;
  (unless debug
    (run-hook-with-args 'sly-net-process-close-hooks connection))
  ;; We close the socket connection by killing its hidden
  ;; *sly-<number>* buffer, but we first unset the connection's
  ;; sentinel otherwise we could get a second `sly-net-close' call. In
  ;; case the buffer is already killed (we killed it manually), this
  ;; function is probably running as a result of that, and rekilling
  ;; it is harmless.
  ;;
  (set-process-sentinel connection nil)
  (when debug
    (set-process-filter connection nil))
  (if debug
      (delete-process connection) ; leave the buffer
    (kill-buffer (process-buffer connection))))

(defun sly-net-sentinel (process message)
  (let ((reason (format "Lisp connection closed unexpectedly: %s" message)))
    (sly-message reason)
    (sly-net-close process reason)))

;;; Socket input is handled by `sly-net-filter', which decodes any
;;; complete messages and hands them off to the event dispatcher.

(defun sly-net-filter (process string)
  "Accept output from the socket and process all complete messages."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  (sly-process-available-input process))

(defun sly-process-available-input (process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (sly-net-have-input-p)
      (let ((event (sly-net-read-or-lose process))
            (ok nil))
        (sly-log-event event process)
        (unwind-protect
            (save-current-buffer
              (sly-dispatch-event event process)
              (setq ok t))
          (unless ok
            (run-at-time 0 nil 'sly-process-available-input process)))))))

(defsubst sly-net-decode-length ()
  (string-to-number (buffer-substring (point) (+ (point) 6))
                    16))

(defun sly-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (sly-net-decode-length))))

(defun sly-handle-net-read-error (error)
  (let ((packet (buffer-string)))
    (sly-with-popup-buffer ((sly-buffer-name :error
                                             :connection (get-buffer-process (current-buffer))))
      (princ (format "%s\nin packet:\n%s" (error-message-string error) packet))
      (goto-char (point-min)))
    (cond ((sly-y-or-n-p "Skip this packet? ")
           `(:emacs-skipped-packet ,packet))
          (t
           (when (sly-y-or-n-p "Enter debugger instead? ")
             (debug 'error error))
           (signal (car error) (cdr error))))))

(defun sly-net-read-or-lose (process)
  (condition-case error
      (sly-net-read)
    (error
     (sly-net-close process "Fatal net-read error" t)
     (error "net-read error: %S" error))))

(defun sly-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (sly-net-decode-length))
         (start (+ (point) 6))
         (end (+ start length)))
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (condition-case error
                 (progn
                   (decode-coding-region start end 'utf-8-unix)
                   (setq end (point-max))
                   (read (current-buffer)))
               (error
                (sly-handle-net-read-error error))))
      (delete-region (point-min) end))))

(defun sly-net-encode-length (n)
  (format "%06x" n))

(defun sly-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (let (print-escape-nonascii
        print-escape-newlines
        print-length
        print-level)
    (prin1-to-string sexp)))


;;;; Connections
;;;
;;; "Connections" are the high-level Emacs<->Lisp networking concept.
;;;
;;; Emacs has a connection to each Lisp process that it's interacting
;;; with. Typically there would only be one, but a user can choose to
;;; connect to many Lisps simultaneously.
;;;
;;; A connection consists of a control socket, optionally an extra
;;; socket dedicated to receiving Lisp output (an optimization), and a
;;; set of connection-local state variables.
;;;
;;; The state variables are stored as buffer-local variables in the
;;; control socket's process-buffer and are used via accessor
;;; functions. These variables include things like the *FEATURES* list
;;; and Unix Pid of the Lisp process.
;;;
;;; One connection is "current" at any given time. This is:
;;;   `sly-dispatching-connection' if dynamically bound, or
;;;   `sly-buffer-connection' if this is set buffer-local, or
;;;   `sly-default-connection' otherwise.
;;;
;;; When you're invoking commands in your source files you'll be using
;;; `sly-default-connection'. This connection can be interactively
;;; reassigned via the connection-list buffer.
;;;
;;; When a command creates a new buffer it will set
;;; `sly-buffer-connection' so that commands in the new buffer will
;;; use the connection that the buffer originated from. For example,
;;; the apropos command creates the *Apropos* buffer and any command
;;; in that buffer (e.g. `M-.') will go to the same Lisp that did the
;;; apropos search. REPL buffers are similarly tied to their
;;; respective connections.
;;;
;;; When Emacs is dispatching some network message that arrived from a
;;; connection it will dynamically bind `sly-dispatching-connection'
;;; so that the event will be processed in the context of that
;;; connection.
;;;
;;; This is mostly transparent. The user should be aware that he can
;;; set the default connection to pick which Lisp handles commands in
;;; Lisp-mode source buffers, and sly hackers should be aware that
;;; they can tie a buffer to a specific connection. The rest takes
;;; care of itself.

(defvar sly-dispatching-connection nil
  "Network process currently executing.
This is dynamically bound while handling messages from Lisp; it
overrides `sly-buffer-connection' and `sly-default-connection'.")

(make-variable-buffer-local
 (defvar sly-buffer-connection nil
   "Network connection to use in the current buffer.
This overrides `sly-default-connection'."))

(defvar sly-default-connection nil
  "Network connection to use by default.
Used for all Lisp communication, except when overridden by
`sly-dispatching-connection' or `sly-buffer-connection'.")

(defun sly-current-connection ()
  "Return the connection to use for Lisp interaction.
Return nil if there's no connection."
  (or sly-dispatching-connection
      sly-buffer-connection
      sly-default-connection))

(defun sly-connection ()
  "Return the connection to use for Lisp interaction.
Signal an error if there's no connection."
  (let ((conn (sly-current-connection)))
    (cond ((and (not conn) sly-net-processes)
           (or (sly-auto-select-connection)
               (error "Connections available, but none selected.")))
          ((not conn)
           (or (sly-auto-start)
               (error "No current SLY connection.")))
          ((not (process-live-p conn))
           (error "Current connection %s is closed." conn))
          (t conn))))

(define-obsolete-variable-alias 'sly-auto-connect
  'sly-auto-start "2.5")
(defcustom sly-auto-start 'never
  "Controls auto connection when information from lisp process is needed.
This doesn't mean it will connect right after SLY is loaded."
  :group 'sly-mode
  :type '(choice (const never)
                 (const always)
                 (const ask)))

(defun sly-auto-start ()
  (cond ((or (eq sly-auto-start 'always)
             (and (eq sly-auto-start 'ask)
                  (sly-y-or-n-p "No connection.  Start SLY? ")))
         (save-window-excursion
           (sly)
           (while (not (sly-current-connection))
             (sleep-for 1))
           (sly-connection)))
        (t nil)))

(cl-defmacro sly-with-connection-buffer ((&optional process) &rest body)
  "Execute BODY in the process-buffer of PROCESS.
If PROCESS is not specified, `sly-connection' is used.

\(fn (&optional PROCESS) &body BODY))"
  (declare (indent 1))
  `(with-current-buffer
       (process-buffer (or ,process (sly-connection)
                           (error "No connection")))
     ,@body))

;;; Connection-local variables:

(defmacro sly-def-connection-var (varname &rest initial-value-and-doc)
  "Define a connection-local variable.
The value of the variable can be read by calling the function of the
same name (it must not be accessed directly). The accessor function is
setf-able.

The actual variable bindings are stored buffer-local in the
process-buffers of connections. The accessor function refers to
the binding for `sly-connection'."
  (declare (indent 2))
  `(progn
     ;; Accessor
     (defun ,varname (&optional process)
       ,(cl-second initial-value-and-doc)
       (let ((process (or process
                          (sly-current-connection)
                          (error "Can't access prop %s for no connection" ',varname))))
         (or (process-get process ',varname)
             (let ((once ,(cl-first initial-value-and-doc)))
               (process-put process ',varname once)
               once))))
     ;; Setf
     (gv-define-setter ,varname (store &optional process)
       `(let ((process (or ,process
                           (sly-current-connection)
                           (error "Can't access prop %s for no connection" ',',varname)))
              (store-once ,store))
          (process-put process ',',varname store-once)
          store-once))
     '(\, varname)))

(sly-def-connection-var sly-connection-number nil
  "Serial number of a connection.
Bound in the connection's process-buffer.")

(sly-def-connection-var sly-lisp-features '()
  "The symbol-names of Lisp's *FEATURES*.
This is automatically synchronized from Lisp.")

(sly-def-connection-var sly-lisp-modules '()
  "The strings of Lisp's *MODULES*.")

(sly-def-connection-var sly-pid nil
  "The process id of the Lisp process.")

(sly-def-connection-var sly-lisp-implementation-type nil
  "The implementation type of the Lisp process.")

(sly-def-connection-var sly-lisp-implementation-version nil
  "The implementation type of the Lisp process.")

(sly-def-connection-var sly-lisp-implementation-name nil
  "The short name for the Lisp implementation.")

(sly-def-connection-var sly-lisp-implementation-program nil
  "The argv[0] of the process running the Lisp implementation.")

(sly-def-connection-var sly-connection-name nil
  "The short name for connection.")

(sly-def-connection-var sly-inferior-process nil
  "The inferior process for the connection if any.")

(sly-def-connection-var sly-communication-style nil
  "The communication style.")

(sly-def-connection-var sly-machine-instance nil
  "The name of the (remote) machine running the Lisp process.")

(sly-def-connection-var sly-connection-coding-systems nil
  "Coding systems supported by the Lisp process.")

;;;;; Connection setup

(defvar sly-connection-counter 0
  "The number of SLY connections made. For generating serial numbers.")

;;; Interface
(defun sly-setup-connection (process)
  "Make a connection out of PROCESS."
  (let ((sly-dispatching-connection process))
    (sly-init-connection-state process)
    (sly-select-connection process)
    (sly--setup-contribs)
    process))

(defun sly-init-connection-state (proc)
  "Initialize connection state in the process-buffer of PROC."
  ;; To make life simpler for the user: if this is the only open
  ;; connection then reset the connection counter.
  (when (equal sly-net-processes (list proc))
    (setq sly-connection-counter 0))
  (sly-with-connection-buffer ()
    (setq sly-buffer-connection proc))
  (setf (sly-connection-number proc) (cl-incf sly-connection-counter))
  ;; We do the rest of our initialization asynchronously. The current
  ;; function may be called from a timer, and if we setup the REPL
  ;; from a timer then it mysteriously uses the wrong keymap for the
  ;; first command.
  (let ((sly-current-thread t))
    (sly-eval-async '(slynk:connection-info)
      (sly-curry #'sly-set-connection-info proc)
      nil
      `((sly-ignore-protocol-mismatches . ,sly-ignore-protocol-mismatches)))))

(defun sly--trampling-rename-buffer (newname)
  "Rename current buffer NEWNAME, trampling over existing ones."
  (let ((existing (get-buffer newname)))
    (unless (eq existing
                (current-buffer))
      ;; Trample over any existing buffers on reconnection
      (when existing
        (let ((kill-buffer-query-functions nil))
          (kill-buffer existing)))
      (rename-buffer newname))))

(defun sly-set-connection-info (connection info)
  "Initialize CONNECTION with INFO received from Lisp."
  (let ((sly-dispatching-connection connection)
        (sly-current-thread t))
    (cl-destructuring-bind (&key pid style lisp-implementation machine
                                 features version modules encoding
                                 &allow-other-keys) info
      (sly-check-version version connection)
      (setf (sly-pid) pid
            (sly-communication-style) style
            (sly-lisp-features) features
            (sly-lisp-modules) modules)
      (cl-destructuring-bind (&key type name version program)
          lisp-implementation
        (setf (sly-lisp-implementation-type) type
              (sly-lisp-implementation-version) version
              (sly-lisp-implementation-name) name
              (sly-lisp-implementation-program) program
              (sly-connection-name) (sly-generate-connection-name name)))
      (cl-destructuring-bind (&key instance ((:type _)) ((:version _))) machine
        (setf (sly-machine-instance) instance))
      (cl-destructuring-bind (&key coding-systems) encoding
        (setf (sly-connection-coding-systems) coding-systems)))
    (let ((args (sly--when-let (p (sly-inferior-process))
                  (sly-inferior-lisp-args p))))
      (sly--when-let (name (plist-get args ':name))
        (unless (string= (sly-lisp-implementation-name) name)
          (setf (sly-connection-name)
                (sly-generate-connection-name (symbol-name name)))))
      (sly-contrib--load-slynk-dependencies)
      (run-hooks 'sly-connected-hook)
      (sly--when-let (fun (plist-get args ':init-function))
        (funcall fun)))
    ;; Give the events buffer its final name
    (with-current-buffer (sly--events-buffer connection)
      (sly--trampling-rename-buffer (sly-buffer-name
                                     :events
                                     :connection connection)))
    ;; Rename the inferior lisp buffer if there is one (i.e. when
    ;; started via `M-x sly')
    ;;
    (let ((inferior-lisp-buffer (sly-inferior-lisp-buffer
                                 (sly-process connection))))
      (when inferior-lisp-buffer
        (with-current-buffer inferior-lisp-buffer
          (sly--trampling-rename-buffer (sly-buffer-name
                                         :inferior-lisp
                                         :connection connection)))))
    (sly-message "Connected. %s" (sly-random-words-of-encouragement))))

(defun sly-check-version (version conn)
  (or (equal version sly-protocol-version)
      (null sly-protocol-version)
      sly-ignore-protocol-mismatches
      (sly-y-or-n-p
       (format "Versions differ: %s (sly) vs. %s (slynk). Continue? "
               sly-protocol-version version))
      (sly-net-close conn "Versions differ")
      (top-level)))

(defun sly-generate-connection-name (lisp-name)
  (when (file-exists-p lisp-name)
    (setq lisp-name (file-name-nondirectory lisp-name)))
  (cl-loop for i from 1
           for name = lisp-name then (format "%s<%d>" lisp-name i)
           while (cl-find name sly-net-processes
                          :key #'sly-connection-name :test #'equal)
           finally (cl-return name)))

(defun sly-select-new-default-connection (conn)
  "If dead CONN was the default connection, select a new one."
  (when (eq conn sly-default-connection)
    (when sly-net-processes
      (sly-select-connection (car sly-net-processes))
      (sly-message "Default connection closed; default is now #%S (%S)"
                   (sly-connection-number)
                   (sly-connection-name)))))

(defcustom sly-keep-buffers-on-connection-close '(:mrepl)
  "List of buffers to keep around after a connection closes."
  :group 'sly-mode
  :type '(repeat
          (choice
           (const :tag "Debugger" :db)
           (const :tag "Repl" :mrepl)
           (const :tag "Ispector" :inspector)
           (const :tag "Stickers replay" :stickers-replay)
           (const :tag "Error" :error)
           (const :tag "Source" :source)
           (const :tag "Compilation" :compilation)
           (const :tag "Apropos" :apropos)
           (const :tag "Xref" :xref)
           (const :tag "Macroexpansion" :macroexpansion)
           (symbol :tag "Other"))))

(defun sly-kill-stale-connection-buffers (conn) ;
  "If CONN had some stale buffers, kill them.
Respect `sly-keep-buffers-on-connection-close'."
  (let ((buffer-list (buffer-list))
        (matchers
         (mapcar
          (lambda (type)
            (format ".*%s.*$"
                    ;; XXX: this is synched with `sly-buffer-name'.
                    (regexp-quote (format "*sly-%s"
                                          (downcase (substring (symbol-name type)
                                                               1))))))
          (cl-set-difference '(:db
                               :mrepl
                               :inspector
                               :stickers-replay
                               :error
                               :source
                               :compilation
                               :apropos
                               :xref
                               :macroexpansion)
                             sly-keep-buffers-on-connection-close))))
    (cl-loop for buffer in buffer-list
             when (and (cl-some (lambda (matcher)
                                  (string-match matcher (buffer-name buffer)))
                                matchers)
                       (with-current-buffer buffer
                         (eq sly-buffer-connection conn)))
             do (kill-buffer buffer))))

(add-hook 'sly-net-process-close-hooks 'sly-select-new-default-connection)
(add-hook 'sly-net-process-close-hooks 'sly-kill-stale-connection-buffers 'append)

;;;;; Commands on connections

(defun sly--purge-connections ()
  "Purge `sly-net-processes' of dead processes, return living."
  (cl-loop for process in sly-net-processes
           if (process-live-p process)
           collect process
           else do
           (sly-warning "process %s in `sly-net-processes' dead. Force closing..." process)
           (sly-net-close process "process state invalid" nil t)))

(defun sly-prompt-for-connection (&optional prompt connections dont-require-match)
  (let* ((connections (or connections (sly--purge-connections)))
         (connection-names (cl-loop for process in
                                    (sort connections
                                          #'(lambda (p1 _p2)
                                              (eq p1 (sly-current-connection))))
                                    collect (sly-connection-name process)))
         (connection-names (if dont-require-match
                               (cons dont-require-match
                                     connection-names)
                             connection-names))
         (connection-name (and connection-names
                               (completing-read
                                (or prompt "Connection: ")
                                connection-names
                                nil (not dont-require-match))))
         (target (cl-find connection-name sly-net-processes :key #'sly-connection-name
                          :test #'string=)))
    (cond (target target)
          ((and dont-require-match (or (zerop (length connection-name))
                                       (string= connection-name dont-require-match)))
           nil)
          (connection-name
           (sly-error "No such connection"))
          (t
           (sly-error "No connections")))))

(defun sly-auto-select-connection ()
  (let* ((c0 (car (sly--purge-connections)))
         (c (cond ((eq sly-auto-select-connection 'always) c0)
                  ((and (eq sly-auto-select-connection 'ask)
                        (sly-prompt-for-connection "Choose a new default connection: "))))))
    (when c
      (sly-select-connection c)
      (sly-message "Switching to connection: %s" (sly-connection-name c))
      c)))

(defvar sly-select-connection-hook nil)

(defun sly-select-connection (process)
  "Make PROCESS the default connection."
  (setq sly-default-connection process)
  (run-hooks 'sly-select-connection-hook))

(define-obsolete-function-alias 'sly-cycle-connections 'sly-next-connection "1.0.0-beta")

(defun sly-next-connection (arg &optional dont-wrap)
  "Switch to the next SLY connection, cycling through all connections.
Skip ARG-1 connections. Negative ARG means cycle back. DONT-WRAP
means don't wrap around when last connection is reached."
  (interactive "p")
  (cl-labels ((connection-full-name
               (c)
               (format "%s %s" (sly-connection-name c) (process-contact c))))
    (cond ((not sly-net-processes)
           (sly-error "No connections to cycle"))
          ((null (cdr sly-net-processes))
           (sly-message "Only one connection: %s" (connection-full-name (car sly-net-processes))))
          (t
           (let* ((dest (append (member (sly-current-connection)
                                        sly-net-processes)
                                (unless dont-wrap sly-net-processes)))
                  (len (length sly-net-processes))
                  (target (nth (mod arg len)
                               dest)))
             (unless target
               (sly-error "No more connections"))
             (sly-select-connection target)
             (if (and sly-buffer-connection
                      (not (eq sly-buffer-connection target)))
                 (sly-message "switched to: %s but buffer remains in: %s"
                              (connection-full-name target)
                              (connection-full-name sly-buffer-connection))
               (sly-message "switched to: %s (%s/%s)" (connection-full-name target)
                            (1+ (cl-position target sly-net-processes))
                            len))
             (sly--refresh-mode-line))))))

(defun sly-prev-connection (arg &optional dont-wrap)
  "Switch to the previous SLY connection, cycling through all connections.
See `sly-next-connection' for other args."
  (interactive "p")
  (sly-next-connection (- arg) dont-wrap))

(defun sly-disconnect (&optional interactive)
  "Close the current connection."
  (interactive (list t))
  (let ((connection (if interactive
                        (sly-prompt-for-connection "Connection to disconnect: ")
                      (sly-current-connection))))
    (sly-net-close connection "Disconnecting")))

(defun sly-disconnect-all ()
  "Disconnect all connections."
  (interactive)
  (mapc #'(lambda (process)
            (sly-net-close process "Disconnecting all connections"))
        sly-net-processes))

(defun sly-connection-port (connection)
  "Return the remote port number of CONNECTION."
  (cadr (process-contact connection)))

(defun sly-process (&optional connection)
  "Return the Lisp process for CONNECTION (default `sly-connection').
Return nil if there's no process object for the connection."
  (let ((proc (sly-inferior-process connection)))
    (if (and proc
             (memq (process-status proc) '(run stop)))
        proc)))

;; Non-macro version to keep the file byte-compilable.
(defun sly-set-inferior-process (connection process)
  (setf (sly-inferior-process connection) process))

(defun sly-use-sigint-for-interrupt (&optional connection)
  (let ((c (or connection (sly-connection))))
    (cl-ecase (sly-communication-style c)
      ((:fd-handler nil) t)
      ((:spawn :sigio) nil))))

(defvar sly-inhibit-pipelining t
  "*If true, don't send background requests if Lisp is already busy.")

(defun sly-background-activities-enabled-p ()
  (and (let ((con (sly-current-connection)))
         (and con
              (eq (process-status con) 'open)))
       (or (not (sly-busy-p))
           (not sly-inhibit-pipelining))))


;;;; Communication protocol

;;;;; Emacs Lisp programming interface
;;;
;;; The programming interface for writing Emacs commands is based on
;;; remote procedure calls (RPCs). The basic operation is to ask Lisp
;;; to apply a named Lisp function to some arguments, then to do
;;; something with the result.
;;;
;;; Requests can be either synchronous (blocking) or asynchronous
;;; (with the result passed to a callback/continuation function).  If
;;; an error occurs during the request then the debugger is entered
;;; before the result arrives -- for synchronous evaluations this
;;; requires a recursive edit.
;;;
;;; You should use asynchronous evaluations (`sly-eval-async') for
;;; most things. Reserve synchronous evaluations (`sly-eval') for
;;; the cases where blocking Emacs is really appropriate (like
;;; completion) and that shouldn't trigger errors (e.g. not evaluate
;;; user-entered code).
;;;
;;; We have the concept of the "current Lisp package". RPC requests
;;; always say what package the user is making them from and the Lisp
;;; side binds that package to *BUFFER-PACKAGE* to use as it sees
;;; fit. The current package is defined as the buffer-local value of
;;; `sly-buffer-package' if set, and otherwise the package named by
;;; the nearest IN-PACKAGE as found by text search (cl-first backwards,
;;; then forwards).
;;;
;;; Similarly we have the concept of the current thread, i.e. which
;;; thread in the Lisp process should handle the request. The current
;;; thread is determined solely by the buffer-local value of
;;; `sly-current-thread'. This is usually bound to t meaning "no
;;; particular thread", but can also be used to nominate a specific
;;; thread. The REPL and the debugger both use this feature to deal
;;; with specific threads.

(make-variable-buffer-local
 (defvar sly-current-thread t
   "The id of the current thread on the Lisp side.
t means the \"current\" thread;
fixnum a specific thread."))

(make-variable-buffer-local
 (defvar sly-buffer-package nil
   "The Lisp package associated with the current buffer.
This is set only in buffers bound to specific packages."))

;;; `sly-rex' is the RPC primitive which is used to implement both
;;; `sly-eval' and `sly-eval-async'. You can use it directly if
;;; you need to, but the others are usually more convenient.

(defvar sly-rex-extra-options-functions nil
  "Functions returning extra options to send with `sly-rex'.")

(cl-defmacro sly-rex ((&rest _)
                      (sexp &optional
                            (package '(sly-current-package))
                            (thread 'sly-current-thread))
                      &rest continuations)
  "(sly-rex (VAR ...) (SEXP &optional PACKAGE THREAD) CLAUSES ...)

Remote EXecute SEXP.

SEXP is evaluated and the princed version is sent to Lisp.

PACKAGE is evaluated and Lisp binds *BUFFER-PACKAGE* to this package.
The default value is (sly-current-package).

CLAUSES is a list of patterns with same syntax as
`sly-dcase'.  The result of the evaluation of SEXP is
dispatched on CLAUSES.  The result is either a sexp of the
form (:ok VALUE) or (:abort CONDITION).  CLAUSES is executed
asynchronously.

Note: don't use backquote syntax for SEXP, because various Emacs
versions cannot deal with that."
  (declare (indent 2)
           (debug (sexp (form &optional sexp sexp)
                        &rest (sexp &rest form))))
  (let ((result (cl-gensym)))
    `(sly-dispatch-event
      (cl-list* :emacs-rex ,sexp ,package ,thread
                (lambda (,result)
                  (sly-dcase ,result
                    ,@continuations))
                (cl-loop for fn in sly-rex-extra-options-functions
                         append (funcall fn))))))

;;; Interface
(defun sly-current-package ()
  "Return the Common Lisp package in the current context.
If `sly-buffer-package' has a value then return that, otherwise
search for and read an `in-package' form."
  (or sly-buffer-package
      (save-restriction
        (widen)
        (sly-find-buffer-package))))

(defvar sly-find-buffer-package-function 'sly-search-buffer-package
  "*Function to use for `sly-find-buffer-package'.
The result should be the package-name (a string)
or nil if nothing suitable can be found.")

(defun sly-find-buffer-package ()
  "Figure out which Lisp package the current buffer is associated with."
  (funcall sly-find-buffer-package-function))

(make-variable-buffer-local
 (defvar sly-package-cache nil
   "Cons of the form (buffer-modified-tick . package)"))

;; When modifing this code consider cases like:
;;  (in-package #.*foo*)
;;  (in-package #:cl)
;;  (in-package :cl)
;;  (in-package "CL")
;;  (in-package |CL|)
;;  (in-package #+ansi-cl :cl #-ansi-cl 'lisp)

(defun sly-search-buffer-package ()
  (let ((case-fold-search t)
        (regexp (concat "^[ \t]*(\\(cl:\\|common-lisp:\\)?in-package\\>[ \t']*"
                        "\\([^)]+\\)[ \t]*)")))
    (save-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (match-string-no-properties 2)))))

;;; Synchronous requests are implemented in terms of asynchronous
;;; ones. We make an asynchronous request with a continuation function
;;; that `throw's its result up to a `catch' and then enter a loop of
;;; handling I/O until that happens.

(defvar sly--stack-eval-tags nil
  "List of stack-tags of waiting on the elisp stack.
This is used by the sly-db debugger to decide whether to enter a
`recursive-edit', so that if a synchronous `sly-eval' request
errors and brings us a Slynk debugger, we can fix the error,
invoke a restart and still get the return value of the `sly-eval'
as if nothing had happened.")

(defun sly-eval (sexp &optional package cancel-on-input cancel-on-input-retval)
  "Evaluate SEXP in Slynk's PACKAGE and return the result.
If CANCEL-ON-INPUT cancel the request immediately if the user
wants to input, and return CANCEL-ON-INPUT-RETVAL."
  (when (null package) (setq package (sly-current-package)))
  (let* ((catch-tag (make-symbol (format "sly-result-%d"
                                         (sly-continuation-counter))))
         (sly--stack-eval-tags (cons catch-tag sly--stack-eval-tags))
         (cancelled nil)
         (check-conn
          (lambda ()
            (unless (eq (process-status (sly-connection)) 'open)
              (error "Lisp connection closed unexpectedly"))))
         (retval
          (unwind-protect
              (catch catch-tag
                (sly-rex ()
                    (sexp package)
                  ((:ok value)
                   (unless cancelled
                     (unless (member catch-tag sly--stack-eval-tags)
                       (error "Reply to nested `sly-eval' request with tag=%S sexp=%S"
                              catch-tag sexp))
                     (throw catch-tag (list #'identity value))))
                  ((:abort _condition)
                   (unless cancelled
                     (throw catch-tag
                            (list #'error "Synchronous Lisp Evaluation aborted")))))
                (cond (cancel-on-input
                       ;; Setting `inhibit-quit' to t helps with
                       ;; callers that wrap us in `while-no-input',
                       ;; like `fido-mode' and Helm.  It doesn't seem
                       ;; to create any specific problems, since
                       ;; `sit-for' exits immediately given input
                       ;; anyway.  This include the C-g input, and
                       ;; thus even with `inhibit-quit' set to t, quit
                       ;; happens immediately.
                       (unwind-protect
                           (let ((inhibit-quit t)) (while (sit-for 30)))
                         (setq cancelled t))
                       (funcall check-conn))
                      (t
                       (while t
                         (funcall check-conn)
                         (accept-process-output nil 30))))
                (list #'identity cancel-on-input-retval))
            ;; Protect against user quit during
            ;; `accept-process-output' or `sit-for', so that if the
            ;; Lisp is alive and replies, we don't get an error.
            (setq cancelled t))))
    (apply (car retval) (cdr retval))))

(defun sly-eval-async (sexp &optional cont package env)
  "Evaluate SEXP on the superior Lisp and call CONT with the result.

CONT is called with the overriding dynamic environment in ENV, an
alist of bindings"
  (declare (indent 1))
  (let ((buffer (current-buffer)))
    (sly-rex ()
        (sexp (or package (sly-current-package)))
      ((:ok result)
       (when cont
         (set-buffer buffer)
         (cl-progv (mapcar #'car env) (mapcar #'cdr env)
           (if debug-on-error
               (funcall cont result)
             (condition-case err
                 (funcall cont result)
               (error
                (sly-message "`sly-eval-async' errored: %s"
                             (if (and (eq 'error (car err))
                                      (stringp (cadr err)))
                                 (cadr err)
                               err))))))))
      ((:abort condition)
       (sly-message "Evaluation aborted on %s." condition))))
  ;; Guard against arbitrary return values which once upon a time
  ;; showed up in the minibuffer spuriously (due to a bug in
  ;; sly-autodoc.)  If this ever happens again, returning the
  ;; following will make debugging much easier:
  :sly-eval-async)

;;; These functions can be handy too:

(defun sly-connected-p ()
  "Return true if the Slynk connection is open."
  (not (null sly-net-processes)))

(defun sly-check-connected ()
  "Signal an error if we are not connected to Lisp."
  (unless (sly-connected-p)
    (error "Not connected. Use `%s' to start a Lisp."
           (substitute-command-keys "\\[sly]"))))

;; UNUSED
(defun sly-debugged-connection-p (conn)
  ;; This previously was (AND (SLY-DB-DEBUGGED-CONTINUATIONS CONN) T),
  ;; but an SLY-DB buffer may exist without having continuations
  ;; attached to it, e.g. the one resulting from `sly-interrupt'.
  (cl-loop for b in (sly-db-buffers)
           thereis (with-current-buffer b
                     (eq sly-buffer-connection conn))))

(defun sly-busy-p (&optional conn)
  "True if Lisp has outstanding requests.
Debugged requests are ignored."
  (let ((debugged (sly-db-debugged-continuations (or conn (sly-connection)))))
    (cl-remove-if (lambda (id)
                    (memq id debugged))
                  (sly-rex-continuations)
                  :key #'car)))

(defun sly-sync ()
  "Block until the most recent request has finished."
  (when (sly-rex-continuations)
    (let ((tag (caar (sly-rex-continuations))))
      (while (cl-find tag (sly-rex-continuations) :key #'car)
        (accept-process-output nil 0.1)))))

(defun sly-ping ()
  "Check that communication works."
  (interactive)
  (sly-message "%s" (sly-eval "PONG")))

;;;;; Protocol event handler (the guts)
;;;
;;; This is the protocol in all its glory. The input to this function
;;; is a protocol event that either originates within Emacs or arrived
;;; over the network from Lisp.
;;;
;;; Each event is a list beginning with a keyword and followed by
;;; arguments. The keyword identifies the type of event. Events
;;; originating from Emacs have names starting with :emacs- and events
;;; from Lisp don't.

(sly-def-connection-var sly-rex-continuations '()
  "List of (ID . FUNCTION) continuations waiting for RPC results.")

(sly-def-connection-var sly-continuation-counter 0
  "Continuation serial number counter.")

(defvar sly-event-hooks)

(defun sly-dispatch-event (event &optional process)
  (let ((sly-dispatching-connection (or process (sly-connection))))
    (or (run-hook-with-args-until-success 'sly-event-hooks event)
        (sly-dcase event
          ((:emacs-rex form package thread continuation &rest extra-options)
           (when (and (sly-use-sigint-for-interrupt) (sly-busy-p))
             (sly-display-oneliner "; pipelined request... %S" form))
           (let ((id (cl-incf (sly-continuation-counter))))
             ;; JT@2020-12-10: FIXME: Force inhibit-quit here to
             ;; ensure atomicity between `sly-send' and the `push'?
             ;; See Github#385..
             (sly-send `(:emacs-rex ,form ,package ,thread ,id ,@extra-options))
             (push (cons id continuation) (sly-rex-continuations))
             (sly--refresh-mode-line)))
          ((:return value id)
           (let ((rec (assq id (sly-rex-continuations))))
             (cond (rec (setf (sly-rex-continuations)
                              (remove rec (sly-rex-continuations)))
                        (funcall (cdr rec) value)
                        (sly--refresh-mode-line))
                   (t
                    (error "Unexpected reply: %S %S" id value)))))
          ((:debug-activate thread level &optional _ignored)
           (cl-assert thread)
           (sly-db--ensure-initialized thread level))
          ((:debug thread level condition restarts frames conts)
           (cl-assert thread)
           (sly-db-setup thread level condition restarts frames conts))
          ((:debug-return thread level stepping)
           (cl-assert thread)
           (sly-db-exit thread level stepping))
          ((:emacs-interrupt thread)
           (sly-send `(:emacs-interrupt ,thread)))
          ((:read-from-minibuffer thread tag prompt initial-value)
           (sly-read-from-minibuffer-for-slynk thread tag prompt
                                               initial-value))
          ((:y-or-n-p thread tag question)
           (sly-remote-y-or-n-p thread tag question))
          ((:emacs-return-string thread tag string)
           (sly-send `(:emacs-return-string ,thread ,tag ,string)))
          ((:new-features features)
           (setf (sly-lisp-features) features))
          ((:indentation-update info)
           (sly-handle-indentation-update info))
          ((:eval-no-wait form)
           (sly-check-eval-in-emacs-enabled)
           (eval (read form) t))
          ((:eval thread tag form-string)
           (sly-check-eval-in-emacs-enabled)
           (sly-eval-for-lisp thread tag form-string))
          ((:emacs-return thread tag value)
           (sly-send `(:emacs-return ,thread ,tag ,value)))
          ((:ed what)
           (sly-ed what))
          ((:inspect what thread tag)
           (let ((hook (when (and thread tag)
                         (sly-curry #'sly-send
                                    `(:emacs-return ,thread ,tag nil)))))
             (sly--open-inspector what :kill-hook hook :switch :raise)))
          ((:background-message message)
           (sly-temp-message 1 3 "[background-message] %s" message))
          ((:debug-condition thread message)
           (cl-assert thread)
           (sly-message "[debug-condition] %s" message))
          ((:ping thread tag)
           (sly-send `(:emacs-pong ,thread ,tag)))
          ((:reader-error packet condition)
           (sly-with-popup-buffer ((sly-buffer-name :error
                                                    :connection sly-dispatching-connection))
             (princ (format "Invalid protocol message:\n%s\n\n%s"
                            condition packet))
             (goto-char (point-min)))
           (error "Invalid protocol message"))
          ((:invalid-rpc id message)
           (setf (sly-rex-continuations)
                 (cl-remove id (sly-rex-continuations) :key #'car))
           (error "Invalid rpc: %s" message))
          ((:emacs-skipped-packet _pkg))
          ((:test-delay seconds) ; for testing only
           (sit-for seconds))
          ((:channel-send id msg)
           (sly-channel-send (or (sly-find-channel id)
                                 (error "Invalid channel id: %S %S" id msg))
                             msg))
          ((:emacs-channel-send id msg)
           (sly-send `(:emacs-channel-send ,id ,msg)))
          ((:invalid-channel channel-id reason)
           (error "Invalid remote channel %s: %s" channel-id reason))))))

(defvar sly--send-last-command nil
  "Value of `this-command' at time of last `sly-send' call.")

(defun sly-send (sexp)
  "Send SEXP directly over the wire on the current connection."
  (setq sly--send-last-command this-command)
  (sly-net-send sexp (sly-connection)))

(defun sly-reset ()
  "Clear all pending continuations and erase connection buffer."
  (interactive)
  (setf (sly-rex-continuations) '())
  (mapc #'kill-buffer (sly-db-buffers))
  (sly-with-connection-buffer ()
    (erase-buffer)))

(defun sly-send-sigint ()
  (interactive)
  (signal-process (sly-pid) 'SIGINT))

;;;;; Channels

;;; A channel implements a set of operations.  Those operations can be
;;; invoked by sending messages to the channel.  Channels are used for
;;; protocols which can't be expressed naturally with RPCs, e.g. for
;;; streaming data over the wire.
;;;
;;; A channel can be "remote" or "local".  Remote channels are
;;; represented by integers.  Local channels are structures.  Messages
;;; sent to a closed (remote) channel are ignored.

(sly-def-connection-var sly-channels '()
  "Alist of the form (ID . CHANNEL).")

(sly-def-connection-var sly-channels-counter 0
  "Channel serial number counter.")

(cl-defstruct (sly-channel (:conc-name sly-channel.)
                           (:constructor
                            sly-make-channel% (operations name id plist)))
  operations name id plist)

(defun sly-make-channel (operations &optional name)
  (let* ((id (cl-incf (sly-channels-counter)))
         (ch (sly-make-channel% operations name id nil)))
    (push (cons id ch) (sly-channels))
    ch))

(defun sly-close-channel (channel)
  (setf (sly-channel.operations channel) 'closed-channel)
  (let ((probe (assq (sly-channel.id channel)
                     (and (sly-current-connection)
                          (sly-channels)))))
    (cond (probe (setf (sly-channels) (delete probe (sly-channels))))
          (t (error "Can't close invalid channel: %s" channel)))))

(defun sly-find-channel (id)
  (cdr (assq id (sly-channels))))

(defun sly-channel-send (channel message)
  (apply (or (gethash (car message) (sly-channel.operations channel))
             (error "Unsupported operation %S for channel %d"
                    (car message)
                    (sly-channel.id channel)))
         channel (cdr message)))

(defun sly-channel-put (channel prop value)
  (setf (sly-channel.plist channel)
        (plist-put (sly-channel.plist channel) prop value)))

(defun sly-channel-get (channel prop)
  (plist-get (sly-channel.plist channel) prop))

(eval-and-compile
  (defun sly-channel-method-table-name (type)
    (intern (format "sly-%s-channel-methods" type))))

(defmacro sly-define-channel-type (name)
  (declare (indent defun))
  (let ((tab (sly-channel-method-table-name name)))
    `(defvar ,tab (make-hash-table :size 10))))

(defmacro sly-define-channel-method (type method args &rest body)
  (declare (indent 3) (debug (&define sexp name lambda-list
                                      def-body)))
  `(puthash ',method
            (lambda (self . ,args) ,@body)
            ,(sly-channel-method-table-name type)))

(defun sly-send-to-remote-channel (channel-id msg)
  (sly-dispatch-event `(:emacs-channel-send ,channel-id ,msg)))

;;;;; Event logging to *sly-events*
;;;
;;; The *sly-events* buffer logs all protocol messages for debugging
;;; purposes. 

(defvar sly-log-events t
  "*Log protocol events to the *sly-events* buffer.")

(defun sly-log-event (event process)
  "Record the fact that EVENT occurred in PROCESS."
  (when sly-log-events
    (with-current-buffer (sly--events-buffer process)
      ;; trim?
      (when (> (buffer-size) 100000)
        (goto-char (/ (buffer-size) 2))
        (re-search-forward "^(" nil t)
        (delete-region (point-min) (point)))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (cond ((and (stringp event)
                  (string-match "^;" event))
             (insert-before-markers event))
            (t
             (save-excursion
               (sly-pprint-event event (current-buffer)))))
      (goto-char (point-max)))))

(defun sly-pprint-event (event buffer)
  "Pretty print EVENT in BUFFER with limited depth and width."
  (let ((print-length 20)
        (print-level 6)
        (pp-escape-newlines t))
    ;; HACK workaround for gh#183
    (condition-case _oops (pp event buffer) (error (print event buffer)))))

(defun sly--events-buffer (process)
  "Return or create the event log buffer."
  (let* ((probe (process-get process 'sly--events-buffer))
         (buffer (or (and (buffer-live-p probe)
                          probe)
                     (let ((buffer (get-buffer-create
                                    (apply #'sly-buffer-name
                                           :events
                                           (if (sly-connection-name process)
                                               `(:connection ,process)
                                             `(:suffix ,(format "%s" process)))))))
                       (with-current-buffer buffer
                         (buffer-disable-undo)
                         (when (fboundp 'lisp-data-mode) ; Emacs >= 28 only
                           (funcall 'lisp-data-mode))
                         (set (make-local-variable 'sly-buffer-connection) process)
                         (sly-mode 1))
                       (process-put process 'sly--events-buffer buffer)
                       buffer))))
    buffer))

(defun sly-pop-to-events-buffer (process)
  "Pop to the SLY events buffer for PROCESS"
  (interactive (list (sly-current-connection)))
  (pop-to-buffer (sly--events-buffer process)))

(defun sly-switch-to-most-recent (mode)
  "Switch to most recent buffer in MODE, a major-mode symbol.
With prefix argument, prompt for MODE"
  (interactive
   (list (if current-prefix-arg
             (intern (completing-read
                      "Switch to most recent buffer in what mode? "
                      (mapcar #'symbol-name '(lisp-mode
                                              emacs-lisp-mode))
                      nil t))
           'lisp-mode)))
  (cl-loop for buffer in (buffer-list)
           when (and (with-current-buffer buffer (eq major-mode mode))
                     (not (eq buffer (current-buffer)))
                     (not (string-match "^ " (buffer-name buffer))))
           do (pop-to-buffer buffer) and return buffer))

(defun sly-forget-pending-events (process)
  "Forget any outgoing events for the PROCESS"
  (interactive (list (sly-current-connection)))
  (setf (sly-rex-continuations process) nil))


;;;;; Cleanup after a quit

(defun sly-restart-inferior-lisp ()
  "Kill and restart the Lisp subprocess."
  (interactive)
  (cl-assert (sly-inferior-process) () "No inferior lisp process")
  (sly-quit-lisp-internal (sly-connection) 'sly-restart-sentinel t))

(defun sly-restart-sentinel (connection _message)
  "When CONNECTION dies, start a similar inferior lisp process.
Also rearrange windows."
  (cl-assert (process-status connection) 'closed)
  (let* ((moribund-proc (sly-inferior-process connection))
         (args (sly-inferior-lisp-args moribund-proc))
         (buffer (buffer-name (process-buffer moribund-proc))))
    (sly-net-close connection "Restarting inferior lisp process")
    (sly-inferior-connect (sly-start-lisp (plist-get args :program)
                                          (plist-get args :program-args)
                                          (plist-get args :env)
                                          nil
                                          buffer)
                          args)))


;;;; Compilation and the creation of compiler-note annotations

(defvar sly-highlight-compiler-notes t
  "*When non-nil annotate buffers with compilation notes etc.")

(defcustom sly-compilation-finished-hook '(sly-maybe-show-compilation-log)
  "Hook called after compilation.
Each function is called with four arguments (SUCCESSP NOTES BUFFER LOADP)
SUCCESSP indicates if the compilation was successful.
NOTES is a list of compilation notes.
BUFFER is the buffer just compiled, or nil if a string was compiled.
LOADP is the value of the LOAD flag passed to `sly-compile-file', or t
if a string."
  :group 'sly-mode
  :type 'hook
  :options '(sly-maybe-show-compilation-log
             sly-show-compilation-log
             sly-maybe-show-xrefs-for-notes
             sly-goto-first-note))

;; FIXME: I doubt that anybody uses this directly and it seems to be
;; only an ugly way to pass arguments.
(defvar sly-compilation-policy nil
  "When non-nil compile with these optimization settings.")

(defun sly-compute-policy (arg)
  "Return the policy for the prefix argument ARG."
  (let ((between (lambda (min n max)
                   (cond ((< n min) min)
                         ((> n max) max)
                         (t n)))))
    (let ((n (prefix-numeric-value arg)))
      (cond ((not arg)   sly-compilation-policy)
            ((cl-plusp n)   `((cl:debug . ,(funcall between 0 n 3))))
            ((eq arg '-) `((cl:speed . 3)))
            (t           `((cl:speed . ,(funcall between 0 (abs n) 3))))))))

(cl-defstruct (sly-compilation-result
               (:type list)
               (:conc-name sly-compilation-result.)
               (:constructor nil)
               (:copier nil))
  tag notes successp duration loadp faslfile)

(defvar sly-last-compilation-result nil
  "The result of the most recently issued compilation.")

(defun sly-compiler-notes ()
  "Return all compiler notes, warnings, and errors."
  (sly-compilation-result.notes sly-last-compilation-result))

(defun sly-compile-and-load-file (&optional policy)
  "Compile and load the buffer's file and highlight compiler notes.

With (positive) prefix argument the file is compiled with maximal
debug settings (`C-u'). With negative prefix argument it is compiled for
speed (`M--'). If a numeric argument is passed set debug or speed settings
to it depending on its sign.

Each source location that is the subject of a compiler note is
underlined and annotated with the relevant information. The commands
`sly-next-note' and `sly-previous-note' can be used to navigate
between compiler notes and to display their full details."
  (interactive "P")
  (sly-compile-file t (sly-compute-policy policy)))

(defcustom sly-compile-file-options '()
  "Plist of additional options that C-c C-k should pass to Lisp.
Currently only :fasl-directory is supported."
  :group 'sly-lisp
  :type '(plist :key-type symbol :value-type (file :must-match t)))

(defun sly-compile-file (&optional load policy)
  "Compile current buffer's file and highlight resulting compiler notes.

See `sly-compile-and-load-file' for further details."
  (interactive)
  (unless buffer-file-name
    (error "Buffer %s is not associated with a file." (buffer-name)))
  (check-parens)
  (when (and (buffer-modified-p)
             (or (not compilation-ask-about-save)
                 (sly-y-or-n-p (format "Save file %s? " (buffer-file-name)))))
    (save-buffer))
  (let ((file (sly-to-lisp-filename (buffer-file-name)))
        (options (sly-simplify-plist `(,@sly-compile-file-options
                                       :policy ,policy))))
    (sly-eval-async
        `(slynk:compile-file-for-emacs ,file ,(if load t nil)
                                       . ,(sly-hack-quotes options))
      #'(lambda (result)
          (sly-compilation-finished result (current-buffer))))
    (sly-message "Compiling %s..." file)))

(defun sly-hack-quotes (arglist)
  ;; eval is the wrong primitive, we really want funcall
  (cl-loop for arg in arglist collect `(quote ,arg)))

(defun sly-simplify-plist (plist)
  (cl-loop for (key val) on plist by #'cddr
           append (cond ((null val) '())
                        (t (list key val)))))

(defun sly-compile-defun (&optional raw-prefix-arg)
  "Compile the current toplevel form.

With (positive) prefix argument the form is compiled with maximal
debug settings (`C-u'). With negative prefix argument it is compiled for
speed (`M--'). If a numeric argument is passed set debug or speed settings
to it depending on its sign."
  (interactive "P")
  (let ((sly-compilation-policy (sly-compute-policy raw-prefix-arg)))
    (if (use-region-p)
        (sly-compile-region (region-beginning) (region-end))
      (apply #'sly-compile-region (sly-region-for-defun-at-point)))))

(defvar sly-compile-region-function 'sly-compile-region-as-string
  "Function called by `sly-compile-region' to do actual work.")

(defun sly-compile-region (start end)
  "Compile the region."
  (interactive "r")
  ;; Check connection before running hooks things like
  ;; sly-flash-region don't make much sense if there's no connection
  (sly-connection)
  (funcall sly-compile-region-function start end))

(defun sly-compile-region-as-string (start end)
  (sly-flash-region start end)
  (sly-compile-string (buffer-substring-no-properties start end) start))

(defun sly-compile-string (string start-offset)
  (let* ((position (sly-compilation-position start-offset)))
    (sly-eval-async
        `(slynk:compile-string-for-emacs
          ,string
          ,(buffer-name)
          ',position
          ,(if (buffer-file-name) (sly-to-lisp-filename (buffer-file-name)))
          ',sly-compilation-policy)
      #'(lambda (result)
          (sly-compilation-finished result nil)))))

(defun sly-compilation-position (start-offset)
  (let ((line (save-excursion
                (goto-char start-offset)
                (list (line-number-at-pos) (1+ (current-column))))))
    `((:position ,start-offset) (:line ,@line))))

(defcustom sly-load-failed-fasl 'never
  "Which action to take when COMPILE-FILE set FAILURE-P to T.
NEVER doesn't load the fasl
ALWAYS loads the fasl
ASK asks the user."
  :type '(choice (const never)
                 (const always)
                 (const ask)))

(defun sly-load-failed-fasl-p ()
  (cl-ecase sly-load-failed-fasl
    (never nil)
    (always t)
    (ask (sly-y-or-n-p "Compilation failed.  Load fasl file anyway? "))))

(defun sly-compilation-finished (result buffer &optional message)
  (let ((notes (sly-compilation-result.notes result))
        (duration (sly-compilation-result.duration result))
        (successp (sly-compilation-result.successp result))
        (faslfile (sly-compilation-result.faslfile result))
        (loadp (sly-compilation-result.loadp result)))
    (setf sly-last-compilation-result result)
    (sly-show-note-counts notes duration (cond ((not loadp) successp)
                                               (t (and faslfile successp)))
                          (or (not buffer) loadp)
                          message)
    (when sly-highlight-compiler-notes
      (sly-highlight-notes notes))
    (when (and loadp faslfile
               (or successp
                   (sly-load-failed-fasl-p)))
      (sly-eval-async `(slynk:load-file ,faslfile)))
    (run-hook-with-args 'sly-compilation-finished-hook successp notes buffer loadp)))

(defun sly-show-note-counts (notes secs successp loadp &optional message)
  (sly-message (concat
                (cond ((and successp loadp)
                       "Compiled and loaded")
                      (successp "Compilation finished")
                      (t (sly-add-face 'font-lock-warning-face
                           "Compilation failed")))
                (if (null notes) ". (No warnings)" ": ")
                (mapconcat
                 (lambda (msgs)
                   (cl-destructuring-bind (sev . notes) msgs
                     (let ((len (length notes)))
                       (format "%d %s%s" len (sly-severity-label sev)
                               (if (= len 1) "" "s")))))
                 (sort (sly-alistify notes #'sly-note.severity #'eq)
                       (lambda (x y) (sly-severity< (car y) (car x))))
                 "  ")
                (if secs (format "  [%.2f secs]" secs))
                message)))

(defun sly-highlight-notes (notes)
  "Highlight compiler notes, warnings, and errors in the buffer."
  (interactive (list (sly-compiler-notes)))
  (with-temp-message "Highlighting notes..."
    (save-excursion
      (save-restriction
        (widen)                  ; highlight notes on the whole buffer
        (sly-remove-notes (point-min) (point-max))
        (mapc #'sly--add-in-buffer-note notes)))))


;;;;; Recompilation.

;; FIXME: This whole idea is questionable since it depends so
;; crucially on precise source-locs.

(defun sly-recompile-location (location)
  (save-excursion
    (sly-move-to-source-location location)
    (sly-compile-defun)))

(defun sly-recompile-locations (locations cont)
  (sly-eval-async
      `(slynk:compile-multiple-strings-for-emacs
        ',(cl-loop for loc in locations collect
                   (save-excursion
                     (sly-move-to-source-location loc)
                     (cl-destructuring-bind (start end)
                         (sly-region-for-defun-at-point)
                       (list (buffer-substring-no-properties start end)
                             (buffer-name)
                             (sly-current-package)
                             start
                             (if (buffer-file-name)
                                 (sly-to-lisp-filename (buffer-file-name))
                               nil)))))
        ',sly-compilation-policy)
    cont))


;;;;; Compiler notes list

(defun sly-one-line-ify (string)
  "Return a single-line version of STRING.
Each newlines and following indentation is replaced by a single space."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "\n[\n \t]*" nil t)
      (replace-match " "))
    (buffer-string)))

(defun sly-xref--get-xrefs-for-notes (notes)
  (let ((xrefs))
    (dolist (note notes)
      (let* ((location (cl-getf note :location))
             (fn (cadr (assq :file (cdr location))))
             (file (assoc fn xrefs))
             (node
              (list (format "%s: %s"
                            (cl-getf note :severity)
                            (sly-one-line-ify (cl-getf note :message)))
                    location)))
        (when fn
          (if file
              (push node (cdr file))
            (setf xrefs (cl-acons fn (list node) xrefs))))))
    xrefs))

(defun sly-maybe-show-xrefs-for-notes (_successp notes _buffer _loadp)
  "Show the compiler notes NOTES if they come from more than one file."
  (let ((xrefs (sly-xref--get-xrefs-for-notes notes)))
    (when (cdr xrefs)                   ; >1 file
      (sly-xref--show-results
       xrefs 'definition "Compiler notes" (sly-current-package)))))

(defun sly-maybe-show-compilation-log (successp notes buffer loadp)
  "Display the log on failed compilations or if NOTES is non-nil."
  (sly-show-compilation-log successp notes buffer loadp
                            (if successp :hidden nil)))

(defun sly-show-compilation-log (successp notes buffer loadp &optional select)
  "Create and display the compilation log buffer."
  (interactive (list (sly-compiler-notes)))
  (sly-with-popup-buffer ((sly-buffer-name :compilation)
                          :mode 'compilation-mode
                          :select select)
    (sly--insert-compilation-log successp notes buffer loadp)
    (insert "Compilation "
            (if successp "successful" "failed")
            ".")))

(defvar sly-compilation-log--notes (make-hash-table)
  "Hash-table (NOTE -> (BUFFER POSITION)) for finding notes in
  the SLY compilation log")

(defun sly--insert-compilation-log (_successp notes _buffer _loadp)
  "Insert NOTES in format suitable for `compilation-mode'."
  (clrhash sly-compilation-log--notes)
  (cl-multiple-value-bind (grouped-notes canonicalized-locs-table)
      (sly-group-and-sort-notes notes)
    (with-temp-message "Preparing compilation log..."
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t)) ; inefficient font-lock-hook
        (insert (format "cd %s\n%d compiler notes:\n\n"
                        default-directory (length notes)))
        (cl-loop for notes in grouped-notes
                 for loc = (gethash (cl-first notes) canonicalized-locs-table)
                 for start = (point)
                 do
                 (cl-loop for note in notes
                          do (puthash note
                                      (cons (current-buffer) start)
                                      sly-compilation-log--notes))
                 (insert
                  (sly--compilation-note-group-button
                   (sly-canonicalized-location-to-string loc) notes)
                  ":")
                 (sly-insert-note-group notes)
                 (insert "\n")
                 (add-text-properties start (point) `(field ,notes))))
      (set (make-local-variable 'compilation-skip-threshold) 0)
      (setq next-error-last-buffer (current-buffer)))))

(defun sly-insert-note-group (notes)
  "Insert a group of compiler messages."
  (insert "\n")
  (dolist (note notes)
    (insert "  " (sly-severity-label (sly-note.severity note)) ": ")
    (let ((start (point)))
      (insert (sly-note.message note))
      (let ((ctx (sly-note.source-context note)))
        (if ctx (insert "\n" ctx)))
      (sly-indent-block start 4))
    (insert "\n")))

(defun sly-indent-block (start column)
  "If the region back to START isn't a one-liner indent it."
  (when (< start (line-beginning-position))
    (save-excursion
      (goto-char start)
      (insert "\n"))
    (sly-indent-rigidly start (point) column)))

(defun sly-canonicalized-location (location)
  "Return a list (FILE LINE COLUMN) for sly-location LOCATION.
This is quite an expensive operation so use carefully."
  (save-excursion
    (sly-goto-location-buffer (sly-location.buffer location))
    (save-excursion
      (sly-move-to-source-location location)
      (list (or (buffer-file-name) (buffer-name))
            (save-restriction
              (widen)
              (line-number-at-pos))
            (1+ (current-column))))))

(defun sly-canonicalized-location-to-string (loc)
  (if loc
      (cl-destructuring-bind (filename line col) loc
        (format "%s:%d:%d"
                (cond ((not filename) "")
                      ((let ((rel (file-relative-name filename)))
                         (if (< (length rel) (length filename))
                             rel)))
                      (t filename))
                line col))
    (format "Unknown location")))

(defun sly-group-and-sort-notes (notes)
  "First sort, then group NOTES according to their canonicalized locs."
  (let ((locs (make-hash-table :test #'eq)))
    (mapc (lambda (note)
            (let ((loc (sly-note.location note)))
              (when (sly-location-p loc)
                (puthash note (sly-canonicalized-location loc) locs))))
          notes)
    (cl-values (sly-group-similar
                (lambda (n1 n2)
                  (equal (gethash n1 locs nil) (gethash n2 locs t)))
                (let* ((bottom most-negative-fixnum)
                       (+default+ (list "" bottom bottom)))
                  (sort notes
                        (lambda (n1 n2)
                          (cl-destructuring-bind (filename1 line1 col1)
                              (gethash n1 locs +default+)
                            (cl-destructuring-bind (filename2 line2 col2)
                                (gethash n2 locs +default+)
                              (cond ((string-lessp filename1 filename2) t)
                                    ((string-lessp filename2 filename1) nil)
                                    ((< line1 line2) t)
                                    ((> line1 line2) nil)
                                    (t (< col1 col2)))))))))
               locs)))

(defun sly-note.severity (note)
  (plist-get note :severity))

(defun sly-note.message (note)
  (plist-get note :message))

(defun sly-note.source-context (note)
  (plist-get note :source-context))

(defun sly-note.location (note)
  (plist-get note :location))

(defun sly-severity-label (severity)
  (cl-subseq (symbol-name severity) 1))



;;;;; Adding a single compiler note
;;;;;
(defun sly-choose-overlay-region (note)
  "Choose the start and end points for an overlay over NOTE.
If the location's sexp is a list spanning multiple lines, then the
region around the first element is used.
Return nil if there's no useful source location."
  (let ((location (sly-note.location note)))
    (when location
      (sly-dcase location
        ((:error _))                 ; do nothing
        ((:location file pos _hints)
         (cond ((eq (car file) ':source-form) nil)
               ((eq (sly-note.severity note) :read-error)
                (sly-choose-overlay-for-read-error location))
               ((equal pos '(:eof))
                (list (1- (point-max)) (point-max)))
               (t
                (sly-choose-overlay-for-sexp location))))))))

(defun sly-choose-overlay-for-read-error (location)
  (let ((pos (sly-location-offset location)))
    (save-excursion
      (goto-char pos)
      (cond ((sly-symbol-at-point)
             ;; package not found, &c.
             (list (sly-symbol-start-pos) (sly-symbol-end-pos)))
            (t
             (list pos (1+ pos)))))))

(defun sly-choose-overlay-for-sexp (location)
  (sly-move-to-source-location location)
  (skip-chars-forward "'#`")
  (let ((start (point)))
    (ignore-errors (sly-forward-sexp))
    (if (sly-same-line-p start (point))
        (list start (point))
      (list (1+ start)
            (progn (goto-char (1+ start))
                   (ignore-errors (forward-sexp 1))
                   (point))))))
(defun sly-same-line-p (pos1 pos2)
  "Return t if buffer positions POS1 and POS2 are on the same line."
  (save-excursion (goto-char (min pos1 pos2))
                  (<= (max pos1 pos2) (line-end-position))))

(defvar sly-severity-face-plist
  (list :error         'sly-error-face
        :read-error    'sly-error-face
        :warning       'sly-warning-face
        :redefinition  'sly-style-warning-face
        :style-warning 'sly-style-warning-face
        :note          'sly-note-face))

(defun sly-severity-face (severity)
  "Return the name of the font-lock face representing SEVERITY."
  (or (plist-get sly-severity-face-plist severity)
      (error "No face for: %S" severity)))

(defvar sly-severity-order
  '(:note :style-warning :redefinition :warning :error :read-error))

(defun sly-severity< (sev1 sev2)
  "Return true if SEV1 is less severe than SEV2."
  (< (cl-position sev1 sly-severity-order)
     (cl-position sev2 sly-severity-order)))

(defun sly-forward-positioned-source-path (source-path)
  "Move forward through a sourcepath from a fixed position.
The point is assumed to already be at the outermost sexp, making the
first element of the source-path redundant."
  (ignore-errors
    (sly-forward-sexp)
    (beginning-of-defun))
  (sly--when-let (source-path (cdr source-path))
    (down-list 1)
    (sly-forward-source-path source-path)))

(defun sly-forward-source-path (source-path)
  (let ((origin (point)))
    (condition-case nil
        (progn
          (cl-loop for (count . more) on source-path
                   do (progn
                        (sly-forward-sexp count)
                        (when more (down-list 1))))
          ;; Align at beginning
          (sly-forward-sexp)
          (beginning-of-sexp))
      (error (goto-char origin)))))


;; FIXME: really fix this mess
;; FIXME: the check shouln't be done here anyway but by M-. itself.

(defun sly-filesystem-toplevel-directory ()
  ;; Windows doesn't have a true toplevel root directory, and all
  ;; filenames look like "c:/foo/bar/quux.baz" from an Emacs
  ;; perspective anyway.
  (if (memq system-type '(ms-dos windows-nt))
      ""
    (file-name-as-directory "/")))

(defun sly-file-name-merge-source-root (target-filename buffer-filename)
  "Returns a filename where the source root directory of TARGET-FILENAME
is replaced with the source root directory of BUFFER-FILENAME.

If no common source root could be determined, return NIL.

E.g. (sly-file-name-merge-source-root
       \"/usr/local/src/joe/upstream/sbcl/code/late-extensions.lisp\"
       \"/usr/local/src/joe/hacked/sbcl/compiler/deftype.lisp\")

        ==> \"/usr/local/src/joe/hacked/sbcl/code/late-extensions.lisp\"
"
  (let ((target-dirs (split-string (file-name-directory target-filename)
                                   "/" t))
        (buffer-dirs (split-string (file-name-directory buffer-filename)
                                   "/" t)))
    ;; Starting from the end, we look if one of the TARGET-DIRS exists
    ;; in BUFFER-FILENAME---if so, it and everything left from that dirname
    ;; is considered to be the source root directory of BUFFER-FILENAME.
    (cl-loop with target-suffix-dirs = nil
             with buffer-dirs* = (reverse buffer-dirs)
             with target-dirs* = (reverse target-dirs)
             for target-dir in target-dirs*
             do (let  ((concat-dirs (lambda (dirs)
                                      (apply #'concat
                                             (mapcar #'file-name-as-directory
                                                     dirs))))
                       (pos (cl-position target-dir buffer-dirs*
                                         :test #'equal)))
                  (if (not pos)    ; TARGET-DIR not in BUFFER-FILENAME?
                      (push target-dir target-suffix-dirs)
                    (let* ((target-suffix
                                        ; PUSH reversed for us!
                            (funcall concat-dirs target-suffix-dirs))
                           (buffer-root
                            (funcall concat-dirs
                                     (reverse (nthcdr pos buffer-dirs*)))))
                      (cl-return (concat (sly-filesystem-toplevel-directory)
                                         buffer-root
                                         target-suffix
                                         (file-name-nondirectory
                                          target-filename)))))))))

(defun sly-highlight-differences-in-dirname (base-dirname contrast-dirname)
  "Returns a copy of BASE-DIRNAME where all differences between
BASE-DIRNAME and CONTRAST-DIRNAME are propertized with a
highlighting face."
  (setq base-dirname (file-name-as-directory base-dirname))
  (setq contrast-dirname (file-name-as-directory contrast-dirname))
  (let ((base-dirs (split-string base-dirname "/" t))
        (contrast-dirs (split-string contrast-dirname "/" t)))
    (with-temp-buffer
      (cl-loop initially (insert (sly-filesystem-toplevel-directory))
               for base-dir in base-dirs do
               (let ((pos (cl-position base-dir contrast-dirs :test #'equal)))
                 (cond ((not pos)
                        (sly-insert-propertized '(face highlight) base-dir)
                        (insert "/"))
                       (t
                        (insert (file-name-as-directory base-dir))
                        (setq contrast-dirs
                              (nthcdr (1+ pos) contrast-dirs))))))
      (buffer-substring (point-min) (point-max)))))

(defvar sly-warn-when-possibly-tricked-by-M-. t
  "When working on multiple source trees simultaneously, the way
`sly-edit-definition' (M-.) works can sometimes be confusing:

`M-.' visits locations that are present in the current Lisp image,
which works perfectly well as long as the image reflects the source
tree that one is currently looking at.

In the other case, however, one can easily end up visiting a file
in a different source root directory (the one corresponding to
the Lisp image), and is thus easily tricked to modify the wrong
source files---which can lead to quite some stressfull cursing.

If this variable is T, a warning message is issued to raise the
user's attention whenever `M-.' is about opening a file in a
different source root that also exists in the source root
directory of the user's current buffer.

There's no guarantee that all possible cases are covered, but
if you encounter such a warning, it's a strong indication that
you should check twice before modifying.")

(defun sly-maybe-warn-for-different-source-root (target-filename
                                                 buffer-filename)
  (let ((guessed-target (sly-file-name-merge-source-root target-filename
                                                         buffer-filename)))
    (when (and guessed-target
               (not (equal guessed-target target-filename))
               (file-exists-p guessed-target))
      (sly-message "Attention: This is `%s'."
                   (concat (sly-highlight-differences-in-dirname
                            (file-name-directory target-filename)
                            (file-name-directory guessed-target))
                           (file-name-nondirectory target-filename))))))

(defun sly-check-location-filename-sanity (filename)
  (when sly-warn-when-possibly-tricked-by-M-.
    (cl-macrolet ((truename-safe (file) `(and ,file (file-truename ,file))))
      (let ((target-filename (truename-safe filename))
            (buffer-filename (truename-safe (buffer-file-name))))
        (when (and target-filename
                   buffer-filename)
          (sly-maybe-warn-for-different-source-root
           target-filename buffer-filename))))))

(defun sly-check-location-buffer-name-sanity (buffer-name)
  (sly-check-location-filename-sanity
   (buffer-file-name (get-buffer buffer-name))))



(defun sly-goto-location-buffer (buffer)
  (sly-dcase buffer
    ((:file filename)
     (let ((filename (sly-from-lisp-filename filename)))
       (sly-check-location-filename-sanity filename)
       (set-buffer (or (get-file-buffer filename)
                       (let ((find-file-suppress-same-file-warnings t))
                         (find-file-noselect filename))))))
    ((:buffer buffer-name)
     (sly-check-location-buffer-name-sanity buffer-name)
     (set-buffer buffer-name))
    ((:buffer-and-file buffer filename)
     (sly-goto-location-buffer
      (if (get-buffer buffer)
          (list :buffer buffer)
        (list :file filename))))
    ((:source-form string)
     (set-buffer (get-buffer-create (sly-buffer-name :source)))
     (erase-buffer)
     (lisp-mode)
     (insert string)
     (goto-char (point-min)))
    ((:zip file entry)
     (require 'arc-mode)
     (set-buffer (find-file-noselect file t))
     (goto-char (point-min))
     (re-search-forward (concat "  " entry "$"))
     (let ((buffer (save-window-excursion
                     (archive-extract)
                     (current-buffer))))
       (set-buffer buffer)
       (goto-char (point-min))))))

(defun sly-goto-location-position (position)
  (sly-dcase position
    ((:position pos)
     (goto-char 1)
     (forward-char (- (1- pos) (sly-eol-conversion-fixup (1- pos)))))
    ((:offset start offset)
     (goto-char start)
     (forward-char offset))
    ((:line start &optional column)
     (goto-char (point-min))
     (beginning-of-line start)
     (cond (column (move-to-column column))
           (t (skip-chars-forward " \t"))))
    ((:function-name name)
     (let ((case-fold-search t)
           (name (regexp-quote name)))
       (goto-char (point-min))
       (when (or
              (re-search-forward
               (format "\\s *(def\\(\\s_\\|\\sw\\)*\\s +(*%s\\S_"
                       (regexp-quote name)) nil t)
              (re-search-forward
               (format "[( \t]%s\\>\\(\\s \\|$\\)" name) nil t))
         (goto-char (match-beginning 0)))))
    ((:method name specializers &rest qualifiers)
     (sly-search-method-location name specializers qualifiers))
    ((:source-path source-path start-position)
     (cond (start-position
            (goto-char start-position)
            (sly-forward-positioned-source-path source-path))
           (t
            (sly-forward-source-path source-path))))
    ((:eof)
     (goto-char (point-max)))))

(defun sly-eol-conversion-fixup (n)
  ;; Return the number of \r\n eol markers that we need to cross when
  ;; moving N chars forward.  N is the number of chars but \r\n are
  ;; counted as 2 separate chars.
  (if (zerop n) 0
    (cl-case (coding-system-eol-type buffer-file-coding-system)
      ((1)
       (save-excursion
         (cl-do ((pos (+ (point) n))
                 (count 0 (1+ count)))
             ((>= (point) pos) (1- count))
           (forward-line)
           (cl-decf pos))))
      (t 0))))

(defun sly-search-method-location (name specializers qualifiers)
  ;; Look for a sequence of words (def<something> method name
  ;; qualifers specializers don't look for "T" since it isn't requires
  ;; (arg without t) as class is taken as such.
  (let* ((case-fold-search t)
         (name (regexp-quote name))
         (qualifiers (mapconcat (lambda (el) (concat ".+?\\<" el "\\>"))
                                qualifiers ""))
         (specializers (mapconcat
                        (lambda (el)
                          (if (eql (aref el 0) ?\()
                              (let ((spec (read el)))
                                (if (eq (car spec) 'EQL)
                                    (concat
                                     ".*?\\n\\{0,1\\}.*?(EQL.*?'\\{0,1\\}"
                                     (format "%s" (cl-second spec)) ")")
                                  (error "don't understand specializer: %s,%s"
                                         el (car spec))))
                            (concat ".+?\n\\{0,1\\}.+?\\<" el "\\>")))
                        (remove "T" specializers) ""))
         (regexp (format "\\s *(def\\(\\s_\\|\\sw\\)*\\s +%s\\s +%s%s" name
                         qualifiers specializers)))
    (or (and (re-search-forward regexp  nil t)
             (goto-char (match-beginning 0)))
        ;;      (sly-goto-location-position `(:function-name ,name))
        )))

(defun sly-search-call-site (fname)
  "Move to the place where FNAME called.
Don't move if there are multiple or no calls in the current defun."
  (save-restriction
    (narrow-to-defun)
    (let ((start (point))
          (regexp (concat "(" fname "[)\n \t]"))
          (case-fold-search t))
      (cond ((and (re-search-forward regexp nil t)
                  (not (re-search-forward regexp nil t)))
             (goto-char (match-beginning 0)))
            (t (goto-char start))))))

(defun sly-search-edit-path (edit-path)
  "Move to EDIT-PATH starting at the current toplevel form."
  (when edit-path
    (unless (and (= (current-column) 0)
                 (looking-at "("))
      (beginning-of-defun))
    (sly-forward-source-path edit-path)))

(defun sly-move-to-source-location (location &optional noerror)
  "Move to the source location LOCATION.
If NOERROR don't signal an error,  but return nil.

Several kinds of locations are supported:

<location> ::= (:location <buffer> <position> <hints>)
             | (:error <message>)

<buffer>   ::= (:file <filename>)
             | (:buffer <buffername>)
             | (:buffer-and-file <buffername> <filename>)
             | (:source-form <string>)
             | (:zip <file> <entry>)

<position> ::= (:position <fixnum>) ; 1 based (for files)
             | (:offset <start> <offset>) ; start+offset (for C-c C-c)
             | (:line <line> [<column>])
             | (:function-name <string>)
             | (:source-path <list> <start-position>)
             | (:method <name string> <specializers> . <qualifiers>)"
  (sly-dcase location
    ((:location buffer _position _hints)
     (sly-goto-location-buffer buffer)
     (let ((pos (sly-location-offset location)))
       (cond ((and (<= (point-min) pos) (<= pos (point-max))))
             (widen-automatically (widen))
             (t
              (error "Location is outside accessible part of buffer")))
       (goto-char pos)))
    ((:error message)
     (cond (noerror
            (sly-message "%s" message)
            nil)
           (t
            (error "%s" message))))))

(defun sly--highlight-sexp (&optional start end)
  "Highlight the first sexp after point."
  (let ((start (or start (point)))
        (end (or end (save-excursion (ignore-errors (forward-sexp)) (point)))))
    (sly-flash-region start end)))

(defun sly--highlight-line (&optional timeout)
  (sly-flash-region (+ (line-beginning-position) (current-indentation))
                    (line-end-position)
                    :timeout timeout))

(make-variable-buffer-local
 (defvar sly-xref--popup-method nil
   "Helper for `sly--display-source-location'"))

(cl-defun sly--display-source-location (source-location
                                        &optional noerror (method 'window))
  "Display SOURCE-LOCATION in a window according to METHOD.
Highlight the resulting sexp. Return the window or raise an
error, unless NOERROR is nil, in which case return nil.  METHOD
specifies how to behave when a reference is selected in an xref
buffer.  If one of symbols `window' or `frame' just
`display-buffer' accordingly. If nil, just switch to buffer in
current window. If a cons (WINDOW . METHOD) consider WINDOW the
\"starting window\" and reconsider METHOD like above: If it is
nil try to use WINDOW exclusively for showing the location,
otherwise prevent that window from being reused when popping to a
new window or frame."
  (cl-labels
      ((pop-it
        (target-buffer method)
        (cond ((eq method 'window)
               (display-buffer target-buffer t))
              ((eq method 'frame)
               (let ((pop-up-frames t))
                 (display-buffer target-buffer t)))
              ((consp method)
               (let* ((window (car method))
                      (sub-method (cdr method)))
                 (cond ((not (window-live-p window))
                        ;; the original window has been deleted: all
                        ;; bets are off!
                        ;;
                        (pop-it target-buffer sub-method))
                       (sub-method
                        ;; shield window from reuse, but restoring
                        ;; any dedicatedness
                        ;;
                        (let ((dedicatedness (window-dedicated-p window)))
                          (unwind-protect
                              (progn
                                ;; (set-window-dedicated-p window 'soft)
                                ;;
                                ;; jt@2018-01-27 commented the line
                                ;; above because since the fix to
                                ;; emacs' bug#28814 in Emacs 26.1
                                ;; (which I myself authored), it won't
                                ;; work correctly. Best to disable it
                                ;; for now and eventually copy Emacs's
                                ;; approach to xref buffers, or better
                                ;; yet, reuse it.
                                (pop-it target-buffer sub-method))
                            (set-window-dedicated-p window dedicatedness))))
                       (t
                        ;; make efforts to reuse the window, respecting
                        ;; any `display-buffer' overrides
                        ;;
                        (display-buffer
                         target-buffer
                         `(,(lambda (buffer _alist)
                              (when (window-live-p window)
                                (set-window-buffer window buffer)
                                window))))))))
              (t
               (switch-to-buffer target-buffer)
               (selected-window)))))
    (when (eq method 'sly-xref)
      (setq method sly-xref--popup-method))
    (when (sly-move-to-source-location source-location noerror)
      (let ((pos (point)))
        (with-selected-window (pop-it (current-buffer) method)
          (goto-char pos)
          (recenter (if (= (current-column) 0) 1))
          (sly--highlight-sexp)
          (selected-window))))))

(defun sly--pop-to-source-location (source-location &optional method)
  "Pop to SOURCE-LOCATION using METHOD.
If called from an xref buffer, method will be `sly-xref' and
thus also honour `sly-xref--popup-method'."
  (let* ((xref-window (selected-window))
         (xref-buffer (window-buffer xref-window)))
    (when (eq method 'sly-xref)
      (quit-restore-window xref-window 'bury))
    (with-current-buffer xref-buffer
      ;; now pop to target
      ;;
      (select-window
       (sly--display-source-location source-location nil method)))
    (set-buffer (window-buffer (selected-window)))))

(defun sly-location-offset (location)
  "Return the position, as character number, of LOCATION."
  (save-restriction
    (widen)
    (condition-case nil
        (sly-goto-location-position
         (sly-location.position location))
      (error (goto-char 0)))
    (let ((hints (sly-location.hints location)))
      (sly--when-let (snippet (cl-getf hints :snippet))
        (sly-isearch snippet))
      (sly--when-let (snippet (cl-getf hints :edit-path))
        (sly-search-edit-path snippet))
      (sly--when-let (fname (cl-getf hints :call-site))
        (sly-search-call-site fname))
      (when (cl-getf hints :align)
        (sly-forward-sexp)
        (beginning-of-sexp)))
    (point)))


;;;;; Incremental search
;;
;; Search for the longest match of a string in either direction.
;;
;; This is for locating text that is expected to be near the point and
;; may have been modified (but hopefully not near the beginning!)

(defun sly-isearch (string)
  "Find the longest occurence of STRING either backwards of forwards.
If multiple matches exist the choose the one nearest to point."
  (goto-char
   (let* ((start (point))
          (len1 (sly-isearch-with-function 'search-forward string))
          (pos1 (point)))
     (goto-char start)
     (let* ((len2 (sly-isearch-with-function 'search-backward string))
            (pos2 (point)))
       (cond ((and len1 len2)
              ;; Have a match in both directions
              (cond ((= len1 len2)
                     ;; Both are full matches -- choose the nearest.
                     (if (< (abs (- start pos1))
                            (abs (- start pos2)))
                         pos1 pos2))
                    ((> len1 len2) pos1)
                    ((> len2 len1) pos2)))
             (len1 pos1)
             (len2 pos2)
             (t start))))))

(defun sly-isearch-with-function (search-fn string)
  "Search for the longest substring of STRING using SEARCH-FN.
SEARCH-FN is either the symbol `search-forward' or `search-backward'."
  (unless (string= string "")
    (cl-loop for i from 1 to (length string)
             while (funcall search-fn (substring string 0 i) nil t)
             for match-data = (match-data)
             do (cl-case search-fn
                  (search-forward  (goto-char (match-beginning 0)))
                  (search-backward (goto-char (1+ (match-end 0)))))
             finally (cl-return (if (null match-data)
                                    nil
                                  ;; Finish based on the last successful match
                                  (store-match-data match-data)
                                  (goto-char (match-beginning 0))
                                  (- (match-end 0) (match-beginning 0)))))))


;;;;; Visiting and navigating the overlays of compiler notes
(defun sly-note-button-p (button)
  (eq (button-type button) 'sly-in-buffer-note))

(defalias 'sly-next-note 'sly-button-forward)
(defalias 'sly-previous-note 'sly-button-backward)

(put 'sly-next-note 'sly-button-navigation-command t)
(put 'sly-previous-note 'sly-button-navigation-command t)

(defun sly-goto-first-note (_successp notes _buffer _loadp)
  "Go to the first note in the buffer."
  (interactive (list (sly-compiler-notes)))
  (when notes
    (goto-char (point-min))
    (sly-next-note 1)))

(defun sly-remove-notes (beg end)
  "Remove `sly-note' annotation buttons from BEG to END."
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (cl-loop for existing in (overlays-in beg end)
           when (sly-note-button-p existing)
           do (delete-overlay existing)))

(defun sly-show-notes (button &rest more-buttons)
  "Present the details of a compiler note to the user."
  (interactive)
  (let ((notes (mapcar (sly-rcurry #'button-get 'sly-note)
                       (cons button more-buttons))))
    (sly-button-flash button :face (let ((color (face-underline-p (button-get button 'face))))
                                     (if color `(:background ,color) 'highlight)))
    ;; If the compilation window is showing, try to land in a suitable
    ;; place there, too...
    ;;
    (let* ((anchor (car notes))
           (compilation-buffer (sly-buffer-name :compilation))
           (compilation-window (get-buffer-window compilation-buffer t)))
      (if compilation-window
          (with-current-buffer compilation-buffer
            (with-selected-window compilation-window
              (let ((buffer-and-pos (gethash anchor
                                             sly-compilation-log--notes)))
                (when buffer-and-pos
                  (cl-assert (eq (car buffer-and-pos) (current-buffer)))
                  (goto-char (cdr buffer-and-pos))
                  (let ((field-end (field-end (1+ (point)))))
                    (sly-flash-region (point) field-end)
                    (sly-recenter field-end))))
              (sly-message "Showing note in %s" (current-buffer))))
        ;; Else, do the next best thing, which is echo the messages.
        ;;
        (if (cdr notes)
            (sly-message "%s notes:\n%s"
                         (length notes)
                         (mapconcat #'sly-note.message notes "\n"))
          (sly-message "%s" (sly-note.message (car notes))))))))

(define-button-type 'sly-note :supertype 'sly-button)

(define-button-type 'sly-in-buffer-note :supertype 'sly-note
  'keymap (let ((map (copy-keymap button-map)))
            (define-key map "RET" nil)
            map)
  'mouse-action 'sly-show-notes
  'sly-button-echo 'sly-show-notes
  'modification-hooks '(sly--in-buffer-note-modification))

(define-button-type 'sly-compilation-note-group :supertype 'sly-note
  'face nil)

(defun sly--in-buffer-note-modification (button after? _beg _end &optional _len)
  (unless after? (delete-overlay button)))

(defun sly--add-in-buffer-note  (note)
  "Add NOTE as a `sly-in-buffer-note' button to the source buffer."
  (cl-destructuring-bind (&optional beg end)
      (sly-choose-overlay-region note)
    (when beg
      (let* ((contained (sly-button--overlays-between beg end))
             (containers (cl-set-difference (sly-button--overlays-at beg)
                                            contained)))
        (cl-loop for ov in contained do (cl-incf (sly-button--level ov)))
        (let ((but (make-button beg
                                end
                                :type 'sly-in-buffer-note
                                'sly-button-search-id (sly-button-next-search-id)
                                'sly-note note
                                'help-echo (format "[sly] %s" (sly-note.message note))
                                'face (sly-severity-face (sly-note.severity note)))))
          (setf (sly-button--level but)
                (1+ (cl-reduce #'max containers
                               :key #'sly-button--level
                               :initial-value 0))))))))

(defun sly--compilation-note-group-button  (label notes)
  "Pepare notes as a `sly-compilation-note' button.
For insertion in the `compilation-mode' buffer"
  (sly--make-text-button label nil :type 'sly-compilation-note-group 'sly-notes-group notes))


;;;; Basic arglisting
;;;;
(defun sly-show-arglist ()
  (let ((op (ignore-errors
              (save-excursion
                (backward-up-list 1)
                (down-list 1)
                (sly-symbol-at-point)))))
    (when op
      (sly-eval-async `(slynk:operator-arglist ,op ,(sly-current-package))
        (lambda (arglist)
          (when arglist
            (sly-message "%s" arglist)))))))


;;;; Edit definition

(defun sly-push-definition-stack ()
  "Add point to find-tag-marker-ring."
  (require 'etags)
  (if (fboundp 'xref-push-marker-stack)
      (xref-push-marker-stack)
    (ring-insert find-tag-marker-ring (point-marker))))

(defun sly-pop-find-definition-stack ()
  "Pop the edit-definition stack and goto the location."
  (interactive)
  (pop-tag-mark))

(cl-defstruct (sly-xref (:conc-name sly-xref.) (:type list))
  dspec location)

(cl-defstruct (sly-location (:conc-name sly-location.) (:type list)
                            (:constructor nil)
                            (:copier nil))
  tag buffer position hints)

(defun sly-location-p (o) (and (consp o) (eq (car o) :location)))

(defun sly-xref-has-location-p (xref)
  (sly-location-p (sly-xref.location xref)))

(defun make-sly-buffer-location (buffer-name position &optional hints)
  `(:location (:buffer ,buffer-name) (:position ,position)
              ,(when hints `(:hints ,hints))))

(defun make-sly-file-location (file-name position &optional hints)
  `(:location (:file ,file-name) (:position ,position)
              ,(when hints `(:hints ,hints))))



(defun sly-edit-definition (&optional name method)
  "Lookup the definition of the name at point.
If there's no name at point, or a prefix argument is given, then
the function name is prompted. METHOD can be nil, or one of
`window' or `frame' to specify if the new definition should be
popped, respectively, in the current window, a new window, or a
new frame."
  (interactive (list (or (and (not current-prefix-arg)
                              (sly-symbol-at-point t))
                         (sly-read-symbol-name "Edit Definition of: "))))
  ;; The hooks might search for a name in a different manner, so don't
  ;; ask the user if it's missing before the hooks are run
  (let ((xrefs (sly-eval `(slynk:find-definitions-for-emacs ,name))))
    (unless xrefs
      (error "No known definition for: %s (in %s)"
             name (sly-current-package)))
    (cl-destructuring-bind (1loc file-alist)
        (sly-analyze-xrefs xrefs)
      (cond (1loc
             (sly-push-definition-stack)
             (sly--pop-to-source-location
              (sly-xref.location (car xrefs)) method))
            ((null (cdr xrefs))      ; ((:error "..."))
             (error "%s" xrefs))
            (t
             (sly-push-definition-stack)
             (sly-xref--show-results file-alist 'definition name
                                     (sly-current-package)
                                     (cons (selected-window)
                                           method)))))))

(defvar sly-edit-uses-xrefs
  '(:calls :macroexpands :binds :references :sets :specializes))

;;; FIXME. TODO: Would be nice to group the symbols (in each
;;;              type-group) by their home-package.
(defun sly-edit-uses (symbol)
  "Lookup all the uses of SYMBOL."
  (interactive (list (sly-read-symbol-name "Edit Uses of: ")))
  (sly-xref--get-xrefs
   sly-edit-uses-xrefs
   symbol
   (lambda (xrefs type symbol package)
     (cond
      ((and (sly-length= xrefs 1)          ; one group
            (sly-length= (cdar  xrefs) 1)) ; one ref in group
       (cl-destructuring-bind (_ (_ loc)) (cl-first xrefs)
         (sly-push-definition-stack)
         (sly--pop-to-source-location loc)))
      (t
       (sly-push-definition-stack)
       (sly-xref--show-results xrefs type symbol package 'window))))))

(defun sly-analyze-xrefs (xrefs)
  "Find common filenames in XREFS.
Return a list (SINGLE-LOCATION FILE-ALIST).
SINGLE-LOCATION is true if all xrefs point to the same location.
FILE-ALIST is an alist of the form ((FILENAME . (XREF ...)) ...)."
  (list (and xrefs
             (let ((loc (sly-xref.location (car xrefs))))
               (and (sly-location-p loc)
                    (cl-every (lambda (x) (equal (sly-xref.location x) loc))
                              (cdr xrefs)))))
        (sly-alistify xrefs #'sly-xref-group #'equal)))

(defun sly-xref-group (xref)
  (cond ((sly-xref-has-location-p xref)
         (sly-dcase (sly-location.buffer (sly-xref.location xref))
           ((:file filename) filename)
           ((:buffer bufname)
            (let ((buffer (get-buffer bufname)))
              (if buffer
                  (format "%S" buffer) ; "#<buffer foo.lisp>"
                (format "%s (previously existing buffer)" bufname))))
           ((:buffer-and-file _buffer filename) filename)
           ((:source-form _) "(S-Exp)")
           ((:zip _zip entry) entry)))
        (t
         "(No location)")))

(defun sly-edit-definition-other-window (name)
  "Like `sly-edit-definition' but switch to the other window."
  (interactive (list (sly-read-symbol-name "Symbol: ")))
  (sly-edit-definition name 'window))

(defun sly-edit-definition-other-frame (name)
  "Like `sly-edit-definition' but switch to the other window."
  (interactive (list (sly-read-symbol-name "Symbol: ")))
  (sly-edit-definition name 'frame))



;;;;; first-change-hook

(defun sly-first-change-hook ()
  "Notify Lisp that a source file's buffer has been modified."
  ;; Be careful not to disturb anything!
  ;; In particular if we muck up the match-data then query-replace
  ;; breaks. -luke (26/Jul/2004)
  (save-excursion
    (save-match-data
      (when (and (buffer-file-name)
                 (file-exists-p (buffer-file-name))
                 (sly-background-activities-enabled-p))
        (let ((filename (sly-to-lisp-filename (buffer-file-name))))
          (sly-eval-async `(slynk:buffer-first-change ,filename)))))))

(defun sly-setup-first-change-hook ()
  (add-hook 'first-change-hook #'sly-first-change-hook nil t))

(add-hook 'sly-mode-hook 'sly-setup-first-change-hook)


;;;; Eval for Lisp

(defun sly-eval-for-lisp (thread tag form-string)
  (let ((ok nil)
        (value nil)
        (error nil)
        (c (sly-connection)))
    (unwind-protect
        (condition-case err
            (progn
              (sly-check-eval-in-emacs-enabled)
              (setq value (eval (read form-string) t))
              (sly-check-eval-in-emacs-result value)
              (setq ok t))
          ((debug error)
           (setq error err)))
      (let ((result (cond (ok `(:ok ,value))
                          (error `(:error ,(symbol-name (car error))
                                          . ,(mapcar #'prin1-to-string
                                                     (cdr error))))
                          (t `(:abort)))))
        (sly-dispatch-event `(:emacs-return ,thread ,tag ,result) c)))))

(defun sly-check-eval-in-emacs-result (x)
  "Raise an error if X can't be marshaled."
  (or (stringp x)
      (memq x '(nil t))
      (integerp x)
      (keywordp x)
      (and (consp x)
           (let ((l x))
             (while (consp l)
               (sly-check-eval-in-emacs-result (car x))
               (setq l (cdr l)))
             (sly-check-eval-in-emacs-result l)))
      (error "Non-serializable return value: %S" x)))

(defun sly-check-eval-in-emacs-enabled ()
  "Raise an error if `sly-enable-evaluate-in-emacs' isn't true."
  (unless sly-enable-evaluate-in-emacs
    (error (concat "sly-eval-in-emacs disabled for security."
                   "Set sly-enable-evaluate-in-emacs true to enable it."))))


;;;; `ED'

(defvar sly-ed-frame nil
  "The frame used by `sly-ed'.")

(defcustom sly-ed-use-dedicated-frame nil
  "*When non-nil, `sly-ed' will create and reuse a dedicated frame."
  :type 'boolean
  :group 'sly-mode)

(cl-defun sly-ed (what )
  "Edit WHAT.

WHAT can be:
  A filename (string),
  A list (:filename FILENAME &key LINE COLUMN POSITION),
  A function name (:function-name STRING)
  nil.

This is for use in the implementation of COMMON-LISP:ED."
  (when sly-ed-use-dedicated-frame
    (unless (and sly-ed-frame (frame-live-p sly-ed-frame))
      (setq sly-ed-frame (make-frame)))
    (select-frame sly-ed-frame))
  (raise-frame)
  (when what
    (sly-dcase what
      ((:filename file &key line column position bytep)
       (find-file (sly-from-lisp-filename file))
       (when line (sly-goto-line line))
       (when column (move-to-column column))
       (when position
         (goto-char (if bytep
                        (byte-to-position position)
                      position))))
      ((:function-name name)
       (sly-edit-definition name)))))

(defun sly-goto-line (line-number)
  "Move to line LINE-NUMBER (1-based).
This is similar to `goto-line' but without pushing the mark and
the display stuff that we neither need nor want."
  (cl-assert (= (buffer-size) (- (point-max) (point-min))) ()
             "sly-goto-line in narrowed buffer")
  (goto-char (point-min))
  (forward-line (1- line-number)))

(defun sly-remote-y-or-n-p (thread tag question)
  (sly-dispatch-event `(:emacs-return ,thread ,tag ,(sly-y-or-n-p question))))

(defun sly-read-from-minibuffer-for-slynk (thread tag prompt initial-value)
  (let ((answer (condition-case nil
                    (sly-read-from-minibuffer prompt initial-value t)
                  (quit nil))))
    (sly-dispatch-event `(:emacs-return ,thread ,tag ,answer))))

;;;; Interactive evaluation.

(defun sly-interactive-eval (string)
  "Read and evaluate STRING and print value in minibuffer.

A prefix argument(`C-u') inserts the result into the current
buffer. A negative prefix argument (`M--') will sends it to the
kill ring."
  (interactive (list (sly-read-from-minibuffer "SLY Eval: ")))
  (cl-case current-prefix-arg
    ((nil)
     (sly-eval-with-transcript `(slynk:interactive-eval ,string)))
    ((-)
     (sly-eval-save string))
    (t
     (sly-eval-print string))))

(defvar sly-transcript-start-hook nil
  "Hook run before start an evalution.")
(defvar sly-transcript-stop-hook nil
  "Hook run after finishing a evalution.")

(defun sly-display-eval-result (value)
  ;; Use `message', not `sly-message'
  (with-temp-buffer
    (insert value)
    (goto-char (point-min))
    (end-of-line 1)
    (if (or (< (1+ (point)) (point-max))
            (>= (- (point) (point-min)) (frame-width)))
        (sly-show-description value (sly-current-package))
      (message "=> %s" value))))

(defun sly-eval-with-transcript (form)
  "Eval FORM in Lisp.  Display output, if any."
  (run-hooks 'sly-transcript-start-hook)
  (sly-rex () (form)
    ((:ok value)
     (run-hooks 'sly-transcript-stop-hook)
     (sly-display-eval-result value))
    ((:abort condition)
     (run-hooks 'sly-transcript-stop-hook)
     (sly-message "Evaluation aborted on %s." condition))))

(defun sly-eval-print (string)
  "Eval STRING in Lisp; insert any output and the result at point."
  (sly-eval-async `(slynk:eval-and-grab-output ,string)
    (lambda (result)
      (cl-destructuring-bind (output value) result
        (push-mark)
        (let* ((start (point))
               (ppss (syntax-ppss))
               (string-or-comment-p (or (nth 3 ppss) (nth 4 ppss))))
          (insert output (if string-or-comment-p
                             ""
                           " => ") value)
          (unless string-or-comment-p
            (comment-region start (point) 1)))))))

(defun sly-eval-save (string)
  "Evaluate STRING in Lisp and save the result in the kill ring."
  (sly-eval-async `(slynk:eval-and-grab-output ,string)
    (lambda (result)
      (cl-destructuring-bind (output value) result
        (let ((string (concat output value)))
          (kill-new string)
          (sly-message "Evaluation finished; pushed result to kill ring."))))))

(defun sly-eval-describe (form)
  "Evaluate FORM in Lisp and display the result in a new buffer."
  (sly-eval-async form (sly-rcurry #'sly-show-description
                                   (sly-current-package))))

(defvar sly-description-autofocus nil
  "If non-nil select description windows on display.")

(defun sly-show-description (string package)
  ;; So we can have one description buffer open per connection. Useful
  ;; for comparing the output of DISASSEMBLE across implementations.
  ;; FIXME: could easily be achieved with M-x rename-buffer
  (let ((bufname (sly-buffer-name :description)))
    (sly-with-popup-buffer (bufname :package package
                                    :connection t
                                    :select sly-description-autofocus
                                    :mode 'lisp-mode)
      (sly-popup-buffer-mode)
      (princ string)
      (goto-char (point-min)))))

(defun sly-last-expression ()
  (buffer-substring-no-properties
   (save-excursion (backward-sexp) (point))
   (point)))

(defun sly-eval-last-expression ()
  "Evaluate the expression preceding point."
  (interactive)
  (sly-interactive-eval (sly-last-expression)))

(defun sly-eval-defun ()
  "Evaluate the current toplevel form.
Use `sly-re-evaluate-defvar' if the from starts with '(defvar'"
  (interactive)
  (let ((form (apply #'buffer-substring-no-properties
                     (sly-region-for-defun-at-point))))
    (cond ((string-match "^(defvar " form)
           (sly-re-evaluate-defvar form))
          (t
           (sly-interactive-eval form)))))

(defun sly-eval-region (start end)
  "Evaluate region."
  (interactive "r")
  (sly-eval-with-transcript
   `(slynk:interactive-eval-region
     ,(buffer-substring-no-properties start end))))

(defun sly-pprint-eval-region (start end)
  "Evaluate region; pprint the value in a buffer."
  (interactive "r")
  (sly-eval-describe
   `(slynk:pprint-eval
     ,(buffer-substring-no-properties start end))))

(defun sly-eval-buffer ()
  "Evaluate the current buffer.
The value is printed in the echo area."
  (interactive)
  (sly-eval-region (point-min) (point-max)))

(defun sly-re-evaluate-defvar (form)
  "Force the re-evaluaton of the defvar form before point.

First make the variable unbound, then evaluate the entire form."
  (interactive (list (sly-last-expression)))
  (sly-eval-with-transcript `(slynk:re-evaluate-defvar ,form)))

(defun sly-pprint-eval-last-expression ()
  "Evaluate the form before point; pprint the value in a buffer."
  (interactive)
  (sly-eval-describe `(slynk:pprint-eval ,(sly-last-expression))))

(defun sly-eval-print-last-expression (string)
  "Evaluate sexp before point; print value into the current buffer"
  (interactive (list (sly-last-expression)))
  (insert "\n")
  (sly-eval-print string))

;;;; Edit Lisp value
;;;
(defun sly-edit-value (form-string)
  "\\<sly-edit-value-mode-map>\
Edit the value of a setf'able form in a new buffer.
The value is inserted into a temporary buffer for editing and then set
in Lisp when committed with \\[sly-edit-value-commit]."
  (interactive
   (list (sly-read-from-minibuffer "Edit value (evaluated): "
                                   (sly-sexp-at-point))))
  (sly-eval-async `(slynk:value-for-editing ,form-string)
    (let ((form-string form-string)
          (package (sly-current-package)))
      (lambda (result)
        (sly-edit-value-callback form-string result
                                 package)))))

(make-variable-buffer-local
 (defvar sly-edit-form-string nil
   "The form being edited by `sly-edit-value'."))

(define-minor-mode sly-edit-value-mode
  "Mode for editing a Lisp value."
  nil
  " Edit-Value"
  '(("\C-c\C-c" . sly-edit-value-commit)))

(defun sly-edit-value-callback (form-string current-value package)
  (let* ((name (generate-new-buffer-name (format "*Edit %s*" form-string)))
         (buffer (sly-with-popup-buffer (name :package package
                                              :connection t
                                              :select t
                                              :mode 'lisp-mode)
                   (sly-mode 1)
                   (sly-edit-value-mode 1)
                   (setq sly-edit-form-string form-string)
                   (insert current-value)
                   (current-buffer))))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (sly-message "Type C-c C-c when done"))))

(defun sly-edit-value-commit ()
  "Commit the edited value to the Lisp image.
\\(See `sly-edit-value'.)"
  (interactive)
  (if (null sly-edit-form-string)
      (error "Not editing a value.")
    (let ((value (buffer-substring-no-properties (point-min) (point-max))))
      (let ((buffer (current-buffer)))
        (sly-eval-async `(slynk:commit-edited-value ,sly-edit-form-string
                                                    ,value)
          (lambda (_)
            (with-current-buffer buffer
              (quit-window t))))))))

;;;; Tracing

(defun sly-untrace-all ()
  "Untrace all functions."
  (interactive)
  (sly-eval `(slynk:untrace-all)))

(defun sly-toggle-trace-fdefinition (spec)
  "Toggle trace."
  (interactive (list (sly-read-from-minibuffer
                      "(Un)trace: " (sly-symbol-at-point))))
  (sly-message "%s" (sly-eval `(slynk:slynk-toggle-trace ,spec))))



(defun sly-disassemble-symbol (symbol-name)
  "Display the disassembly for SYMBOL-NAME."
  (interactive (list (sly-read-symbol-name "Disassemble: ")))
  (sly-eval-describe `(slynk:disassemble-form ,(concat "'" symbol-name))))

(defun sly-undefine-function (symbol-name)
  "Unbind the function slot of SYMBOL-NAME."
  (interactive (list (sly-read-symbol-name "fmakunbound: " t)))
  (sly-eval-async `(slynk:undefine-function ,symbol-name)
    (lambda (result) (sly-message "%s" result))))

(defun sly-remove-method (name qualifiers specializers)
  "Remove a method from generic function named NAME.
The method removed is identified by QUALIFIERS and SPECIALIZERS."
  (interactive (sly--read-method
                "[sly] Remove method from which generic function: "
                "[sly] Remove which method from %s"))
  (sly-eval `(slynk:remove-method-by-name ,name
                                          ',qualifiers
                                          ',specializers))
  (sly-message "Method removed"))

(defun sly-unintern-symbol (symbol-name package)
  "Unintern the symbol given with SYMBOL-NAME PACKAGE."
  (interactive (list (sly-read-symbol-name "Unintern symbol: " t)
                     (sly-read-package-name "from package: "
                                            (sly-current-package))))
  (sly-eval-async `(slynk:unintern-symbol ,symbol-name ,package)
    (lambda (result) (sly-message "%s" result))))

(defun sly-delete-package (package-name)
  "Delete the package with name PACKAGE-NAME."
  (interactive (list (sly-read-package-name "Delete package: "
                                            (sly-current-package))))
  (sly-eval-async `(cl:delete-package
                    (slynk::guess-package ,package-name))))

(defun sly-load-file (filename)
  "Load the Lisp file FILENAME."
  (interactive (list
                (read-file-name "[sly] Load file: " nil nil
                                nil (if (buffer-file-name)
                                        (file-name-nondirectory
                                         (buffer-file-name))))))
  (let ((lisp-filename (sly-to-lisp-filename (expand-file-name filename))))
    (sly-eval-with-transcript `(slynk:load-file ,lisp-filename))))

(defvar sly-change-directory-hooks nil
  "Hook run by `sly-change-directory'.
The functions are called with the new (absolute) directory.")

(defun sly-change-directory (directory)
  "Make DIRECTORY become Lisp's current directory.
Return whatever slynk:set-default-directory returns."
  (let ((dir (expand-file-name directory)))
    (prog1 (sly-eval `(slynk:set-default-directory
                       (slynk-backend:filename-to-pathname
                        ,(sly-to-lisp-filename dir))))
      (sly-with-connection-buffer nil (cd-absolute dir))
      (run-hook-with-args 'sly-change-directory-hooks dir))))

(defun sly-cd (directory)
  "Make DIRECTORY become Lisp's current directory.
Return whatever slynk:set-default-directory returns."
  (interactive (list (read-directory-name "[sly] Directory: " nil nil t)))
  (sly-message "default-directory: %s" (sly-change-directory directory)))

(defun sly-pwd ()
  "Show Lisp's default directory."
  (interactive)
  (sly-message "Directory %s" (sly-eval `(slynk:default-directory))))


;;;; Documentation

(defvar sly-documentation-lookup-function
  'sly-hyperspec-lookup)

(defun sly-documentation-lookup ()
  "Generalized documentation lookup. Defaults to hyperspec lookup."
  (interactive)
  (call-interactively sly-documentation-lookup-function))

;;;###autoload
(defun sly-hyperspec-lookup (symbol-name)
  "A wrapper for `hyperspec-lookup'"
  (interactive (list (common-lisp-hyperspec-read-symbol-name
                      (sly-symbol-at-point))))
  (hyperspec-lookup symbol-name))

(defun sly-describe-symbol (symbol-name)
  "Describe the symbol at point."
  (interactive (list (sly-read-symbol-name "Describe symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (sly-eval-describe `(slynk:describe-symbol ,symbol-name)))

(defun sly-documentation (symbol-name)
  "Display function- or symbol-documentation for SYMBOL-NAME."
  (interactive (list (sly-read-symbol-name "Documentation for symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (sly-eval-describe
   `(slynk:documentation-symbol ,symbol-name)))

(defun sly-describe-function (symbol-name)
  (interactive (list (sly-read-symbol-name "Describe symbol's function: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (sly-eval-describe `(slynk:describe-function ,symbol-name)))

(defface sly-apropos-symbol
  '((t (:inherit sly-part-button-face)))
  "Face for the symbol name in Apropos output."
  :group 'sly)

(defface sly-apropos-label
  '((t (:inherit italic)))
  "Face for label (`Function', `Variable' ...) in Apropos output."
  :group 'sly)

(defun sly-apropos-summary (string case-sensitive-p package only-external-p)
  "Return a short description for the performed apropos search."
  (concat (if case-sensitive-p "Case-sensitive " "")
          "Apropos for "
          (format "%S" string)
          (if package (format " in package %S" package) "")
          (if only-external-p " (external symbols only)" "")))

(defun sly-apropos (string &optional only-external-p package
                           case-sensitive-p)
  "Show all bound symbols whose names match STRING. With prefix
arg, you're interactively asked for parameters of the search.
With M-- (negative) prefix arg, prompt for package only. "
  (interactive
   (cond ((eq '- current-prefix-arg)
          (list (sly-read-from-minibuffer "Apropos external symbols: ")
                t
                (sly-read-package-name "Package (blank for all): "
                                       nil 'allow-blank)
                nil))
         (current-prefix-arg
          (list (sly-read-from-minibuffer "Apropos: ")
                (sly-y-or-n-p "External symbols only? ")
                (sly-read-package-name "Package (blank for all): "
                                       nil 'allow-blank)
                (sly-y-or-n-p "Case-sensitive? ")))
         (t
          (list (sly-read-from-minibuffer "Apropos external symbols: ") t nil nil))))
  (sly-eval-async
      `(slynk-apropos:apropos-list-for-emacs ,string ,only-external-p
                                             ,case-sensitive-p ',package)
    (sly-rcurry #'sly-show-apropos string package
                (sly-apropos-summary string case-sensitive-p
                                     package only-external-p))))

(defun sly-apropos-all ()
  "Shortcut for (sly-apropos <string> nil nil)"
  (interactive)
  (sly-apropos (sly-read-from-minibuffer "Apropos all symbols: ") nil nil))

(defun sly-apropos-package (package &optional internal)
  "Show apropos listing for symbols in PACKAGE.
With prefix argument include internal symbols."
  (interactive (list (let ((pkg (sly-read-package-name "Package: ")))
                       (if (string= pkg "") (sly-current-package) pkg))
                     current-prefix-arg))
  (sly-apropos "" (not internal) package))

(defvar sly-apropos-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(define-derived-mode sly-apropos-mode apropos-mode "SLY-Apropos"
  "SLY Apropos Mode

TODO"
  (sly-mode))

(defun sly-show-apropos (plists string package summary)
  (cond ((null plists)
         (sly-message "No apropos matches for %S" string))
        (t
         (sly-with-popup-buffer ((sly-buffer-name :apropos
                                                  :connection t)
                                 :package package :connection t
                                 :mode 'sly-apropos-mode)
           (if (boundp 'header-line-format)
               (setq header-line-format summary)
             (insert summary "\n\n"))
           (sly-set-truncate-lines)
           (sly-print-apropos plists (not package))
           (set-syntax-table lisp-mode-syntax-table)
           (goto-char (point-min))))))

(define-button-type 'sly-apropos-symbol :supertype 'sly-part
  'face nil
  'action 'sly-button-goto-source ;default action
  'sly-button-inspect
  #'(lambda (name _type)
      (sly-inspect (format "(quote %s)" name)))
  'sly-button-goto-source
  #'(lambda (name _type)
      (sly-edit-definition name 'window))
  'sly-button-describe
  #'(lambda (name _type)
      (sly-eval-describe `(slynk:describe-symbol ,name))))

(defun sly--package-designator-prefix (designator)
  (unless (listp designator)
    (error "unknown designator type"))
  (concat (cadr designator)
          (if (cl-caddr designator) ":" "::")))

(defun sly-apropos-designator-string (designator)
  (concat (sly--package-designator-prefix designator)
          (car designator)))

(defun sly-apropos-insert-symbol (designator item bounds package-designator-searched-p)
  (let ((label (sly-apropos-designator-string designator)))
    (setq label
          (sly--make-text-button label nil
                                 'face 'sly-apropos-symbol
                                 'part-args (list item nil)
                                 'part-label "Symbol"
                                 :type 'sly-apropos-symbol))
    (cl-loop
     with offset = (if package-designator-searched-p
                       0
                     (length (sly--package-designator-prefix designator)))
     for bound in bounds
     for (start end) = (if (listp bound) bound (list bound (1+ bound)))
     do
     (put-text-property (+ start offset) (+ end offset) 'face 'highlight label)
     finally (insert label))))

(defun sly-print-apropos (plists package-designator-searched-p)
  (cl-loop
   for plist in plists
   for designator = (plist-get plist :designator)
   for item = (substring-no-properties
               (sly-apropos-designator-string designator))
   do
   (sly-apropos-insert-symbol designator item (plist-get plist :bounds) package-designator-searched-p)
   (terpri)
   (cl-loop for (prop value) on plist by #'cddr
            for start = (point)
            unless (memq prop '(:designator
                                :package
                                :bounds))
            do
            (let ((namespace (upcase-initials
                              (replace-regexp-in-string
                               "-" " " (substring (symbol-name prop) 1)))))
              (princ "  ")
              (insert (propertize namespace
                                  'face 'sly-apropos-label))
              (princ ": ")
              (princ (cond ((and value
                                 (not (eq value :not-documented)))
                            value)
                           (t
                            "(not documented)")))
              (add-text-properties
               start (point)
               (list 'action 'sly-button-describe
                     'sly-button-describe
                     #'(lambda (name type)
                         (sly-eval-describe `(slynk:describe-definition-for-emacs ,name
                                                                                  ,type)))
                     'part-args (list item prop)
                     'button t 'apropos-label namespace))
              (terpri)))))

(defun sly-apropos-describe (name type)
  (sly-eval-describe `(slynk:describe-definition-for-emacs ,name ,type)))

(require 'info)
(defun sly-info--file ()
  (or (cl-some (lambda (subdir)
                 (cl-flet ((existing-file
                            (name) (let* ((path (expand-file-name subdir sly-path))
                                          (probe (expand-file-name name path)))
                                     (and (file-exists-p probe) probe))))
                   (or (existing-file "sly.info")
                       (existing-file "sly.info.gz"))))
               (append '("doc" ".") Info-directory-list))
      (sly-error
       "No sly.info, run `make -C doc sly.info' from a SLY git checkout")))

(require 'info)

(defvar sly-info--cached-node-names nil)

(defun sly-info--node-names (file)
  (or sly-info--cached-node-names
      (setq sly-info--cached-node-names
            (with-temp-buffer
              (info file (current-buffer))
              (ignore-errors
                (Info-build-node-completions))))))

;;;###autoload
(defun sly-info (file &optional node)
  "Read SLY manual"
  (interactive
   (let ((file (sly-info--file)))
     (list file
           (completing-read "Manual node? (`Top' to read the whole manual): "
                            (remove '("*") (sly-info--node-names file))
                            nil t))))
  (info (if node (format "(%s)%s" file node) file)))


;;;; XREF: cross-referencing

(defvar sly-xref-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'sly-xref-goto)
    (define-key map (kbd "SPC") 'sly-xref-show)
    (define-key map (kbd "n") 'sly-xref-next-line)
    (define-key map (kbd "p") 'sly-xref-prev-line)
    (define-key map (kbd ".") 'sly-xref-next-line)
    (define-key map (kbd ",") 'sly-xref-prev-line)
    (define-key map (kbd "C-c C-c") 'sly-recompile-xref)
    (define-key map (kbd "C-c C-k") 'sly-recompile-all-xrefs)

    (define-key map (kbd "q")     'quit-window)
    (set-keymap-parent map button-buffer-map)

    map))

(define-derived-mode sly-xref-mode lisp-mode "Xref"
  "sly-xref-mode: Major mode for cross-referencing.
\\<sly-xref-mode-map>\
The most important commands:
\\[sly-xref-show]       - Display referenced source and keep xref window.
\\[sly-xref-goto]       - Jump to referenced source and dismiss xref window.

\\{sly-xref-mode-map}"
  (setq font-lock-defaults nil)
  (setq delayed-mode-hooks nil)
  (setq buffer-read-only t)
  (sly-mode))

(defun sly-next-line/not-add-newlines ()
  (interactive)
  (let ((next-line-add-newlines nil))
    (forward-line 1)))


;;;;; XREF results buffer and window management

(cl-defmacro sly-with-xref-buffer ((_xref-type _symbol &optional package)
                                   &body body)
  "Execute BODY in a xref buffer, then show that buffer."
  (declare (indent 1))
  `(sly-with-popup-buffer ((sly-buffer-name :xref
                                            :connection t)
                           :package ,package
                           :connection t
                           :select t
                           :mode 'sly-xref-mode)
     (sly-set-truncate-lines)
     ,@body))

;; TODO: Have this button support more options, not just "show source"
;; and "goto-source"
(define-button-type 'sly-xref :supertype 'sly-part
  'action 'sly-button-goto-source ;default action
  'mouse-action 'sly-button-goto-source ;default action
  'sly-button-show-source #'(lambda (location)
                              (sly-xref--show-location location))
  'sly-button-goto-source #'(lambda (location)
                              (sly--pop-to-source-location location 'sly-xref)))

(defun sly-xref-button (label location)
  (sly--make-text-button label nil
                         :type 'sly-xref
                         'part-args (list location)
                         'part-label "Location"))

(defun sly-insert-xrefs (xref-alist)
  "Insert XREF-ALIST in the current-buffer.
XREF-ALIST is of the form ((GROUP . ((LABEL LOCATION) ...)) ...).
GROUP and LABEL are for decoration purposes.  LOCATION is a
source-location."
  (cl-loop for (group . refs) in xref-alist do
           (sly-insert-propertized '(face bold) group "\n")
           (cl-loop for (label location) in refs
                    for start = (point)
                    do
                    (insert
                     " "
                     (sly-xref-button (sly-one-line-ify label) location)
                     "\n")
                    (add-text-properties start (point) (list 'sly-location location))))
  ;; Remove the final newline to prevent accidental window-scrolling
  (backward-delete-char 1))

(defun sly-xref-next-line (arg)
  (interactive "p")
  (let ((button (forward-button arg)))
    (when button (sly-button-show-source button))))

(defun sly-xref-prev-line (arg)
  (interactive "p")
  (sly-xref-next-line (- arg)))

(defun sly-xref--show-location (loc)
  (cl-ecase (car loc)
    (:location (sly--display-source-location loc))
    (:error (sly-message "%s" (cadr loc)))
    ((nil))))

(defun sly-xref--show-results (xrefs _type symbol package &optional method)
  "Maybe show a buffer listing the cross references XREFS.
METHOD is used to set `sly-xref--popup-method', which see."
  (cond ((null xrefs)
         (sly-message "No references found for %s." symbol)
         nil)
        (t
         (sly-with-xref-buffer (_type _symbol package)
           (sly-insert-xrefs xrefs)
           (setq sly-xref--popup-method method)
           (goto-char (point-min))
           (current-buffer)))))


;;;;; XREF commands

(defun sly-who-calls (symbol)
  "Show all known callers of the function SYMBOL.
This is implemented with special compiler support, see `sly-list-callers' for a
portable alternative."
  (interactive (list (sly-read-symbol-name "Who calls: " t)))
  (sly-xref :calls symbol))

(defun sly-calls-who (symbol)
  "Show all known functions called by the function SYMBOL.
This is implemented with special compiler support and may not be supported by
all implementations.
See `sly-list-callees' for a portable alternative."
  (interactive (list (sly-read-symbol-name "Who calls: " t)))
  (sly-xref :calls-who symbol))

(defun sly-who-references (symbol)
  "Show all known referrers of the global variable SYMBOL."
  (interactive (list (sly-read-symbol-name "Who references: " t)))
  (sly-xref :references symbol))

(defun sly-who-binds (symbol)
  "Show all known binders of the global variable SYMBOL."
  (interactive (list (sly-read-symbol-name "Who binds: " t)))
  (sly-xref :binds symbol))

(defun sly-who-sets (symbol)
  "Show all known setters of the global variable SYMBOL."
  (interactive (list (sly-read-symbol-name "Who sets: " t)))
  (sly-xref :sets symbol))

(defun sly-who-macroexpands (symbol)
  "Show all known expanders of the macro SYMBOL."
  (interactive (list (sly-read-symbol-name "Who macroexpands: " t)))
  (sly-xref :macroexpands symbol))

(defun sly-who-specializes (symbol)
  "Show all known methods specialized on class SYMBOL."
  (interactive (list (sly-read-symbol-name "Who specializes: " t)))
  (sly-xref :specializes symbol))

(defun sly-list-callers (symbol-name)
  "List the callers of SYMBOL-NAME in a xref window.
See `sly-who-calls' for an implementation-specific alternative."
  (interactive (list (sly-read-symbol-name "List callers: ")))
  (sly-xref :callers symbol-name))

(defun sly-list-callees (symbol-name)
  "List the callees of SYMBOL-NAME in a xref window.
See `sly-calls-who' for an implementation-specific alternative."
  (interactive (list (sly-read-symbol-name "List callees: ")))
  (sly-xref :callees symbol-name))

(defun sly-xref (type symbol &optional continuation)
  "Make an XREF request to Lisp."
  (sly-eval-async
      `(slynk:xref ',type ',symbol)
    (sly-rcurry (lambda (result type symbol package cont)
                  (and (sly-xref-implemented-p type result)
                       (let* ((file-alist (cadr (sly-analyze-xrefs result))))
                         (funcall (or cont 'sly-xref--show-results)
                                  file-alist type symbol package))))
                type
                symbol
                (sly-current-package)
                continuation)))

(defun sly-xref-implemented-p (type xrefs)
  "Tell if xref TYPE is available according to XREFS."
  (cond ((eq xrefs :not-implemented)
         (sly-display-oneliner "%s is not implemented yet on %s."
                               (sly-xref-type type)
                               (sly-lisp-implementation-name))
         nil)
        (t t)))

(defun sly-xref-type (type)
  "Return a human readable version of xref TYPE."
  (format "who-%s" (sly-cl-symbol-name type)))

(defun sly-xref--get-xrefs (types symbol &optional continuation)
  "Make multiple XREF requests at once."
  (sly-eval-async
      `(slynk:xrefs ',types ',symbol)
    #'(lambda (result)
        (funcall (or continuation
                     #'sly-xref--show-results)
                 (cl-loop for (key . val) in result
                          collect (cons (sly-xref-type key) val))
                 types symbol (sly-current-package)))))


;;;;; XREF navigation

(defun sly-xref-location-at-point ()
  (save-excursion
    ;; When the end of the last line is at (point-max) we can't find
    ;; the text property there. Going to bol avoids this problem.
    (beginning-of-line 1)
    (or (get-text-property (point) 'sly-location)
        (error "No reference at point."))))

(defun sly-xref-dspec-at-point ()
  (save-excursion
    (beginning-of-line 1)
    (with-syntax-table lisp-mode-syntax-table
      (forward-sexp)                    ; skip initial whitespaces
      (backward-sexp)
      (sly-sexp-at-point))))

(defun sly-all-xrefs ()
  (let ((xrefs nil))
    (save-excursion
      (goto-char (point-min))
      (while (zerop (forward-line 1))
        (sly--when-let (loc (get-text-property (point) 'sly-location))
          (let* ((dspec (sly-xref-dspec-at-point))
                 (xref  (make-sly-xref :dspec dspec :location loc)))
            (push xref xrefs)))))
    (nreverse xrefs)))

(defun sly-xref-goto ()
  "Goto the cross-referenced location at point."
  (interactive)
  (sly--pop-to-source-location (sly-xref-location-at-point) 'sly-xref))

(defun sly-xref-show ()
  "Display the xref at point in the other window."
  (interactive)
  (sly--display-source-location (sly-xref-location-at-point)))

(defun sly-search-property (prop &optional backward prop-value-fn)
  "Search the next text range where PROP is non-nil.
Return the value of PROP.
If BACKWARD is non-nil, search backward.
If PROP-VALUE-FN is non-nil use it to extract PROP's value."
  (let ((next-candidate (if backward
                            #'previous-single-char-property-change
                          #'next-single-char-property-change))
        (prop-value-fn  (or prop-value-fn
                            (lambda ()
                              (get-text-property (point) prop))))
        (start (point))
        (prop-value))
    (while (progn
             (goto-char (funcall next-candidate (point) prop))
             (not (or (setq prop-value (funcall prop-value-fn))
                      (eobp)
                      (bobp)))))
    (cond (prop-value)
          (t (goto-char start) nil))))

(defun sly-recompile-xref (&optional raw-prefix-arg)
  "Recompile definition at point.
Uses prefix arguments like `sly-compile-defun'."
  (interactive "P")
  (let ((sly-compilation-policy (sly-compute-policy raw-prefix-arg)))
    (let ((location (sly-xref-location-at-point))
          (dspec    (sly-xref-dspec-at-point)))
      (sly-recompile-locations
       (list location)
       (sly-rcurry #'sly-xref-recompilation-cont
                   (list dspec) (current-buffer))))))

(defun sly-recompile-all-xrefs (&optional raw-prefix-arg)
  "Recompile all definitions.
Uses prefix arguments like `sly-compile-defun'."
  (interactive "P")
  (let ((sly-compilation-policy (sly-compute-policy raw-prefix-arg)))
    (let ((dspecs) (locations))
      (dolist (xref (sly-all-xrefs))
        (when (sly-xref-has-location-p xref)
          (push (sly-xref.dspec xref) dspecs)
          (push (sly-xref.location xref) locations)))
      (sly-recompile-locations
       locations
       (sly-rcurry #'sly-xref-recompilation-cont
                   dspecs (current-buffer))))))

(defun sly-xref-recompilation-cont (results dspecs buffer)
  ;; Extreme long-windedness to insert status of recompilation;
  ;; sometimes Elisp resembles more of an Ewwlisp.

  ;; FIXME: Should probably throw out the whole recompilation cruft
  ;; anyway.  -- helmut
  ;; TODO: next iteration of fixme cleanup this is going in a contrib -- jt
  (with-current-buffer buffer
    (sly-compilation-finished (sly-aggregate-compilation-results results)
                              nil)
    (save-excursion
      (sly-xref-insert-recompilation-flags
       dspecs (cl-loop for r in results collect
                       (or (sly-compilation-result.successp r)
                           (and (sly-compilation-result.notes r)
                                :complained)))))))

(defun sly-aggregate-compilation-results (results)
  `(:compilation-result
    ,(cl-reduce #'append (mapcar #'sly-compilation-result.notes results))
    ,(cl-every #'sly-compilation-result.successp results)
    ,(cl-reduce #'+ (mapcar #'sly-compilation-result.duration results))))

(defun sly-xref-insert-recompilation-flags (dspecs compilation-results)
  (let* ((buffer-read-only nil)
         (max-column (sly-column-max)))
    (goto-char (point-min))
    (cl-loop for dspec in dspecs
             for result in compilation-results
             do (save-excursion
                  (cl-loop for dspec2 = (progn (search-forward dspec)
                                               (sly-xref-dspec-at-point))
                           until (equal dspec2 dspec))
                  (end-of-line) ; skip old status information.
                  (insert-char ?\  (1+ (- max-column (current-column))))
                  (insert (format "[%s]"
                                  (cl-case result
                                    ((t)   :success)
                                    ((nil) :failure)
                                    (t     result))))))))


;;;; Macroexpansion

(defvar sly-macroexpansion-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'sly-macroexpand-again)
    (define-key map (kbd "a") 'sly-macroexpand-all-inplace)
    (define-key map (kbd "q") 'quit-window)
    (define-key map [remap sly-macroexpand-1] 'sly-macroexpand-1-inplace)
    (define-key map [remap sly-macroexpand-all] 'sly-macroexpand-all-inplace)
    (define-key map [remap sly-compiler-macroexpand-1] 'sly-compiler-macroexpand-1-inplace)
    (define-key map [remap sly-expand-1] 'sly-expand-1-inplace)
    (define-key map [remap undo] 'sly-macroexpand-undo)
    map))

(define-minor-mode sly-macroexpansion-minor-mode
  "SLY mode for macroexpansion"
  nil
  " Macroexpand"
  nil
  (read-only-mode 1))

(defun sly-macroexpand-undo (&optional arg)
  (interactive)
  ;; Emacs 22.x introduced `undo-only' which
  ;; works by binding `undo-no-redo' to t. We do
  ;; it this way so we don't break prior Emacs
  ;; versions.
  (cl-macrolet ((undo-only (arg) `(let ((undo-no-redo t)) (undo ,arg))))
    (let ((inhibit-read-only t))
      (when (fboundp 'sly-remove-edits)
        (sly-remove-edits (point-min) (point-max)))
      (undo-only arg))))

(defvar sly-eval-macroexpand-expression nil
  "Specifies the last macroexpansion preformed.
This variable specifies both what was expanded and how.")

(defun sly-eval-macroexpand (expander &optional string)
  (let ((string (or string
                    (sly-sexp-at-point 'interactive))))
    (setq sly-eval-macroexpand-expression `(,expander ,string))
    (sly-eval-async sly-eval-macroexpand-expression
      #'sly-initialize-macroexpansion-buffer)))

(defun sly-macroexpand-again ()
  "Reperform the last macroexpansion."
  (interactive)
  (sly-eval-async sly-eval-macroexpand-expression
    (sly-rcurry #'sly-initialize-macroexpansion-buffer
                (current-buffer))))

(defun sly-initialize-macroexpansion-buffer (expansion &optional buffer)
  (pop-to-buffer (or buffer (sly-create-macroexpansion-buffer)))
  (setq buffer-undo-list nil) ; Get rid of undo information from
                                        ; previous expansions.
  (let ((inhibit-read-only t)
        (buffer-undo-list t)) ; Make the initial insertion not be undoable.
    (erase-buffer)
    (insert expansion)
    (goto-char (point-min))
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings (font-lock-fontify-buffer)))))

(defun sly-create-macroexpansion-buffer ()
  (let ((name (sly-buffer-name :macroexpansion)))
    (sly-with-popup-buffer (name :package t :connection t
                                 :mode 'lisp-mode)
      (sly-macroexpansion-minor-mode 1)
      (setq font-lock-keywords-case-fold-search t)
      (current-buffer))))

(defun sly-eval-macroexpand-inplace (expander)
  "Substitute the sexp at point with its macroexpansion.

NB: Does not affect sly-eval-macroexpand-expression"
  (interactive)
  (let* ((bounds (sly-bounds-of-sexp-at-point 'interactive)))
    (let* ((start (copy-marker (car bounds)))
           (end (copy-marker (cdr bounds)))
           (point (point))
           (buffer (current-buffer)))
      (sly-eval-async
          `(,expander ,(buffer-substring-no-properties start end))
        (lambda (expansion)
          (with-current-buffer buffer
            (let ((buffer-read-only nil))
              (when (fboundp 'sly-remove-edits)
                (sly-remove-edits (point-min) (point-max)))
              (goto-char start)
              (delete-region start end)
              (sly-insert-indented expansion)
              (goto-char point))))))))

(defun sly-macroexpand-1 (&optional repeatedly)
  "Display the macro expansion of the form at point.
The form is expanded with CL:MACROEXPAND-1 or, if a prefix
argument is given, with CL:MACROEXPAND."
  (interactive "P")
  (sly-eval-macroexpand
   (if repeatedly 'slynk:slynk-macroexpand 'slynk:slynk-macroexpand-1)))

(defun sly-macroexpand-1-inplace (&optional repeatedly)
  (interactive "P")
  (sly-eval-macroexpand-inplace
   (if repeatedly 'slynk:slynk-macroexpand 'slynk:slynk-macroexpand-1)))

(defun sly-macroexpand-all (&optional just-one)
  "Display the recursively macro expanded sexp at point.
With optional JUST-ONE prefix arg, use CL:MACROEXPAND-1."
  (interactive "P")
  (sly-eval-macroexpand (if just-one
                            'slynk:slynk-macroexpand-1
                          'slynk:slynk-macroexpand-all)))

(defun sly-macroexpand-all-inplace ()
  "Display the recursively macro expanded sexp at point."
  (interactive)
  (sly-eval-macroexpand-inplace 'slynk:slynk-macroexpand-all))

(defun sly-compiler-macroexpand-1 (&optional repeatedly)
  "Display the compiler-macro expansion of sexp at point."
  (interactive "P")
  (sly-eval-macroexpand
   (if repeatedly
       'slynk:slynk-compiler-macroexpand
     'slynk:slynk-compiler-macroexpand-1)))

(defun sly-compiler-macroexpand-1-inplace (&optional repeatedly)
  "Display the compiler-macro expansion of sexp at point."
  (interactive "P")
  (sly-eval-macroexpand-inplace
   (if repeatedly
       'slynk:slynk-compiler-macroexpand
     'slynk:slynk-compiler-macroexpand-1)))

(defun sly-expand-1 (&optional repeatedly)
  "Display the macro expansion of the form at point.

The form is expanded with CL:MACROEXPAND-1 or, if a prefix
argument is given, with CL:MACROEXPAND.

Contrary to `sly-macroexpand-1', if the form denotes a compiler
macro, SLYNK-BACKEND:COMPILER-MACROEXPAND or
SLYNK-BACKEND:COMPILER-MACROEXPAND-1 are used instead."
  (interactive "P")
  (sly-eval-macroexpand
   (if repeatedly
       'slynk:slynk-expand
     'slynk:slynk-expand-1)))

(defun sly-expand-1-inplace (&optional repeatedly)
  "Display the macro expansion of the form at point.
The form is expanded with CL:MACROEXPAND-1 or, if a prefix
argument is given, with CL:MACROEXPAND."
  (interactive "P")
  (sly-eval-macroexpand-inplace
   (if repeatedly
       'slynk:slynk-expand
     'slynk:slynk-expand-1)))

(defun sly-format-string-expand (&optional string)
  "Expand the format-string at point and display it.
With prefix arg, or if no string at point, prompt the user for a
string to expand.
"
  (interactive (list (or (and (not current-prefix-arg)
                              (sly-string-at-point))
                         (sly-read-from-minibuffer "Expand format: "
                                                   (sly-string-at-point)))))
  (sly-eval-macroexpand 'slynk:slynk-format-string-expand
                        string))


;;;; Subprocess control

(defun sly-interrupt ()
  "Interrupt Lisp."
  (interactive)
  (cond ((sly-use-sigint-for-interrupt) (sly-send-sigint))
        (t (sly-dispatch-event `(:emacs-interrupt ,sly-current-thread)))))

(defun sly-quit ()
  (error "Not implemented properly.  Use `sly-interrupt' instead."))

(defun sly-quit-lisp (&optional kill interactive)
  "Quit lisp, kill the inferior process and associated buffers."
  (interactive (list current-prefix-arg t))
  (let ((connection (if interactive
                        (sly-prompt-for-connection "Connection to quit: ")
                      (sly-current-connection))))
    (sly-quit-lisp-internal connection 'sly-quit-sentinel kill)))

(defun sly-quit-lisp-internal (connection sentinel kill)
  "Kill SLY socket connection CONNECTION.
Do this by evaluating (SLYNK:QUIT-LISP) in it, and don't wait for
it to reply as usual with other evaluations.  If it's non-nil,
setup SENTINEL to run on CONNECTION when it finishes dying.  If
KILL is t, and there is such a thing, also kill the inferior lisp
process associated with CONNECTION."
  (let ((sly-dispatching-connection connection))
    (sly-eval-async '(slynk:quit-lisp))
    (set-process-filter connection  nil)
    (let ((attempt 0)
          (dying-p nil))
      (set-process-sentinel
       connection
       (lambda (connection status)
         (setq dying-p t)
         (sly-message "Connection %s is dying (%s)" connection status)
         (let ((inf-process (sly-inferior-process connection)))
           (cond ((and kill
                       inf-process
                       (not (memq (process-status inf-process) '(exit signal))))
                  (sly-message "Quitting %s: also killing the inferior process %s"
                               connection inf-process)
                  (kill-process inf-process))
                 ((and kill
                       inf-process)
                  (sly-message "Quitting %s: inferior process was already dead"
                               connection
                               inf-process))
                 ((and
                   kill
                   (not inf-process))
                  (sly-message "Quitting %s: No inferior process to kill!"
                               connection
                               inf-process))))
         (when sentinel
           (funcall sentinel connection status))))
      (sly-message
       "Waiting for connection %s to die by itself..." connection)
      (while (and (< (cl-incf attempt) 30)
                  (not dying-p))
        (sleep-for 0.1))
      (unless dying-p
        (sly-message
         "Connection %s didn't die by itself. Killing it." connection)
        (delete-process connection)))))

(defun sly-quit-sentinel (process _message)
  (cl-assert (process-status process) 'closed)
  (let* ((inferior (sly-inferior-process process))
         (inferior-buffer (if inferior (process-buffer inferior))))
    (when inferior (delete-process inferior))
    (when inferior-buffer (kill-buffer inferior-buffer))
    (sly-net-close process "Quitting lisp")
    (sly-message "Connection closed.")))


;;;; Debugger (SLY-DB)

(defvar sly-db-hook nil
  "Hook run on entry to the debugger.")

(defcustom sly-db-initial-restart-limit 6
  "Maximum number of restarts to display initially."
  :group 'sly-debugger
  :type 'integer)


;;;;; Local variables in the debugger buffer

;; Small helper.
(defun sly-make-variables-buffer-local (&rest variables)
  (mapcar #'make-variable-buffer-local variables))

(sly-make-variables-buffer-local
 (defvar sly-db-condition nil
   "A list (DESCRIPTION TYPE) describing the condition being debugged.")

 (defvar sly-db-restarts nil
   "List of (NAME DESCRIPTION) for each available restart.")

 (defvar sly-db-level nil
   "Current debug level (recursion depth) displayed in buffer.")

 (defvar sly-db-backtrace-start-marker nil
   "Marker placed at the first frame of the backtrace.")

 (defvar sly-db-restart-list-start-marker nil
   "Marker placed at the first restart in the restart list.")

 (defvar sly-db-continuations nil
   "List of ids for pending continuation."))

;;;;; SLY-DB macros

;; some macros that we need to define before the first use

(defmacro sly-db-in-face (name string)
  "Return STRING propertised with face sly-db-NAME-face."
  (declare (indent 1))
  (let ((facename (intern (format "sly-db-%s-face" (symbol-name name))))
        (var (cl-gensym "string")))
    `(let ((,var ,string))
       (sly-add-face ',facename ,var)
       ,var)))


;;;;; sly-db-mode

(defvar sly-db-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    ;; We give < and > parenthesis syntax, so that #< ... > is treated
    ;; as a balanced expression.  This enables autodoc-mode to match
    ;; #<unreadable> actual arguments in the backtraces with formal
    ;; arguments of the function.  (For Lisp mode, this is not
    ;; desirable, since we do not wish to get a mismatched paren
    ;; highlighted everytime we type < or >.)
    (modify-syntax-entry ?< "(" table)
    (modify-syntax-entry ?> ")" table)
    table)
  "Syntax table for SLY-DB mode.")

(defvar sly-db-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n"    'sly-db-down)
    (define-key map "p"    'sly-db-up)
    (define-key map "\M-n" 'sly-db-details-down)
    (define-key map "\M-p" 'sly-db-details-up)
    (define-key map "<"    'sly-db-beginning-of-backtrace)
    (define-key map ">"    'sly-db-end-of-backtrace)

    (define-key map "a"    'sly-db-abort)
    (define-key map "q"    'sly-db-abort)
    (define-key map "c"    'sly-db-continue)
    (define-key map "A"    'sly-db-break-with-system-debugger)
    (define-key map "B"    'sly-db-break-with-default-debugger)
    (define-key map "P"    'sly-db-print-condition)
    (define-key map "I"    'sly-db-invoke-restart-by-name)
    (define-key map "C"    'sly-db-inspect-condition)
    (define-key map ":"    'sly-interactive-eval)
    (define-key map "Q"    'sly-db-quit)

    (set-keymap-parent map button-buffer-map)
    map))

(define-derived-mode sly-db-mode fundamental-mode "sly-db"
  "Superior lisp debugger mode.
In addition to ordinary SLY commands, the following are
available:\\<sly-db-mode-map>

Commands to invoke restarts:
   \\[sly-db-quit]   - quit
   \\[sly-db-abort]   - abort
   \\[sly-db-continue]   - continue
   \\[sly-db-invoke-restart-0]-\\[sly-db-invoke-restart-9] - restart shortcuts
   \\[sly-db-invoke-restart-by-name]   - invoke restart by name

Navigation commands:
   \\[forward-button] - next interactive button
   \\[sly-db-down]   - down
   \\[sly-db-up]   - up
   \\[sly-db-details-down] - down, with details
   \\[sly-db-details-up] - up, with details
   \\[sly-db-beginning-of-backtrace]   - beginning of backtrace
   \\[sly-db-end-of-backtrace]   - end of backtrace

Commands to examine and operate on the selected frame:\\<sly-db-frame-map>
   \\[sly-db-show-frame-source]   - show frame source
   \\[sly-db-goto-source]   - go to frame source
   \\[sly-db-toggle-details] - toggle details
   \\[sly-db-disassemble]   - dissassemble frame
   \\[sly-db-eval-in-frame]   - prompt for a form to eval in frame
   \\[sly-db-pprint-eval-in-frame]   - eval in frame and pretty print result
   \\[sly-db-inspect-in-frame]   - inspect in frame's context
   \\[sly-db-restart-frame]   - restart frame
   \\[sly-db-return-from-frame]   - return from frame

Miscellaneous commands:\\<sly-db-mode-map>
   \\[sly-db-step]   - step
   \\[sly-db-break-with-default-debugger]   - switch to native debugger
   \\[sly-db-break-with-system-debugger]   - switch to system debugger (gdb)
   \\[sly-interactive-eval]   - eval
   \\[sly-db-inspect-condition]   - inspect signalled condition

Full list of commands:

\\{sly-db-mode-map}

Full list of frame-specific commands:

\\{sly-db-frame-map}"
  (erase-buffer)
  (set-syntax-table sly-db-mode-syntax-table)
  (sly-set-truncate-lines)
  ;; Make original sly-connection "sticky" for SLY-DB commands in this buffer
  (setq sly-buffer-connection (sly-connection))
  (setq buffer-read-only t)
  (sly-mode 1)
  (sly-interactive-buttons-mode 1))

;; Keys 0-9 are shortcuts to invoke particular restarts.
(dotimes (number 10)
  (let ((fname (intern (format "sly-db-invoke-restart-%S" number)))
        (docstring (format "Invoke restart numbered %S." number)))
    ;; FIXME: In Emacs≥25, you could avoid `eval' and use
    ;;     (defalias .. (lambda .. (:documentation docstring) ...))
    ;; instead!
    (eval `(defun ,fname ()
             ,docstring
             (interactive)
             (sly-db-invoke-restart ,number))
          t)
    (define-key sly-db-mode-map (number-to-string number) fname)))


;;;;; SLY-DB buffer creation & update

(defcustom sly-db-focus-debugger 'auto
  "Control if debugger window gets focus immediately.

If nil, the window is never focused automatically; if the symbol
`auto', the window is only focused if the user has performed no
other commands in the meantime (i.e. he/she is expecting a
possible debugger); any other non-nil value means to always
automatically focus the debugger window."
  :group 'sly-debugger
  :type '(choice (const always) (const never) (const auto)))

(defun sly-filter-buffers (predicate)
  "Return a list of where PREDICATE returns true.
PREDICATE is executed in the buffer to test."
  (cl-remove-if-not (lambda (%buffer)
                      (with-current-buffer %buffer
                        (funcall predicate)))
                    (buffer-list)))

(defun sly-db-buffers (&optional connection)
  "Return a list of all sly-db buffers (belonging to CONNECTION.)"
  (if connection
      (sly-filter-buffers (lambda ()
                            (and (eq sly-buffer-connection connection)
                                 (eq major-mode 'sly-db-mode))))
    (sly-filter-buffers (lambda () (eq major-mode 'sly-db-mode)))))

(defun sly-db-find-buffer (thread &optional connection)
  (let ((connection (or connection (sly-connection))))
    (cl-find-if (lambda (buffer)
                  (with-current-buffer buffer
                    (and (eq sly-buffer-connection connection)
                         (eq sly-current-thread thread))))
                (sly-db-buffers))))

(defun sly-db-pop-to-debugger-maybe (&optional _button)
  "Maybe pop to *sly-db* buffer for current context."
  (interactive)
  (let ((b (sly-db-find-buffer sly-current-thread)))
    (if b (pop-to-buffer b)
      (sly-error "Can't find a *sly-db* debugger for this context"))))

(defsubst sly-db-get-default-buffer ()
  "Get a sly-db buffer.
The chosen buffer the default connection's it if exists."
  (car (sly-db-buffers (sly-current-connection))))

(defun sly-db-pop-to-debugger ()
  "Pop to the first *sly-db* buffer if at least one exists."
  (interactive)
  (let ((b (sly-db-get-default-buffer)))
    (if b (pop-to-buffer b)
      (sly-error "No *sly-db* debugger buffers for this connection"))))

(defun sly-db-get-buffer (thread &optional connection)
  "Find or create a sly-db-buffer for THREAD."
  (let ((connection (or connection (sly-connection))))
    (or (sly-db-find-buffer thread connection)
        (let ((name (sly-buffer-name :db :connection connection
                                     :suffix (format "thread %d" thread))))
          (with-current-buffer (generate-new-buffer name)
            (setq sly-buffer-connection connection
                  sly-current-thread thread)
            (current-buffer))))))

(defun sly-db-debugged-continuations (connection)
  "Return the all debugged continuations for CONNECTION across SLY-DB buffers."
  (cl-loop for b in (sly-db-buffers)
           append (with-current-buffer b
                    (and (eq sly-buffer-connection connection)
                         sly-db-continuations))))

(defun sly-db-confirm-buffer-kill ()
  (when (or (not (process-live-p sly-buffer-connection))
            (sly-y-or-n-p "Really kill sly-db buffer and throw to toplevel?"))
    (ignore-errors (sly-db-quit))
    t))

(defun sly-db--display-debugger (_thread)
  "Display (or pop to) sly-db for THREAD as appropriate.
Also mark the window as a debugger window."
  (let* ((action '(sly-db--display-in-prev-sly-db-window))
         (buffer (current-buffer))
         (win
          (if (cond ((eq sly-db-focus-debugger 'auto)
                     (eq sly--send-last-command last-command))
                    (t sly-db-focus-debugger))
              (progn
                (pop-to-buffer buffer action)
                (selected-window))
            (display-buffer buffer action))))
    (set-window-parameter win 'sly-db buffer)
    win))

(defun sly-db-setup (thread level condition restarts frame-specs conts)
  "Setup a new SLY-DB buffer.
CONDITION is a string describing the condition to debug.
RESTARTS is a list of strings (NAME DESCRIPTION) for each
available restart.  FRAME-SPECS is a list of (NUMBER DESCRIPTION
&optional PLIST) describing the initial portion of the
backtrace. Frames are numbered from 0.  CONTS is a list of
pending Emacs continuations."
  (with-current-buffer (sly-db-get-buffer thread)
    (cl-assert (if (equal sly-db-level level)
                   (equal sly-db-condition condition)
                 t)
               () "Bug: sly-db-level is equal but condition differs\n%s\n%s"
               sly-db-condition condition)
    (with-selected-window (sly-db--display-debugger thread)
      (unless (equal sly-db-level level)
        (let ((inhibit-read-only t))
          (sly-db-mode)
          (add-hook 'kill-buffer-query-functions
                    #'sly-db-confirm-buffer-kill
                    nil t)
          (setq sly-current-thread thread)
          (setq sly-db-level level)
          (setq mode-name (format "sly-db[%d]" sly-db-level))
          (setq sly-db-condition condition)
          (setq sly-db-restarts restarts)
          (setq sly-db-continuations conts)
          (sly-db-insert-condition condition)
          (insert "\n\n" (sly-db-in-face section "Restarts:") "\n")
          (setq sly-db-restart-list-start-marker (point-marker))
          (sly-db-insert-restarts restarts 0 sly-db-initial-restart-limit)
          (insert "\n" (sly-db-in-face section "Backtrace:") "\n")
          (setq sly-db-backtrace-start-marker (point-marker))
          (save-excursion
            (if frame-specs
                (sly-db-insert-frames (sly-db-prune-initial-frames frame-specs) t)
              (insert "[No backtrace]")))
          (run-hooks 'sly-db-hook)
          (set-syntax-table lisp-mode-syntax-table)))
      (sly-recenter (point-min) 'allow-moving-point)
      (when sly--stack-eval-tags
        (sly-message "Entering recursive edit..")
        (recursive-edit)))))

(defun sly-db--display-in-prev-sly-db-window (buffer _alist)
  (let ((window
         (get-window-with-predicate
          #'(lambda (w)
              (let ((value (window-parameter w 'sly-db)))
                (and value
                     (not (buffer-live-p value))))))))
    (when window
      (display-buffer-record-window 'reuse window buffer)
      (set-window-buffer window buffer)
      window)))

(defun sly-db--ensure-initialized (thread level)
  "Initialize debugger buffer for THREAD.
If such a buffer exists for LEVEL, it is assumed to have been
sufficiently initialized, and this function does nothing."
  (let ((buffer (sly-db-find-buffer thread)))
    (unless (and buffer
                 (with-current-buffer buffer
                   (equal sly-db-level level)))
      (sly-rex ()
          ('(slynk:debugger-info-for-emacs 0 10)
           nil thread)
        ((:ok result)
         (apply #'sly-db-setup thread level result))))))

(defvar sly-db-exit-hook nil
  "Hooks run in the debugger buffer just before exit")

(defun sly-db-exit (thread _level &optional stepping)
  "Exit from the debug level LEVEL."
  (sly--when-let (sly-db (sly-db-find-buffer thread))
    (with-current-buffer sly-db
      (setq kill-buffer-query-functions
            (remove 'sly-db-confirm-buffer-kill kill-buffer-query-functions))
      (run-hooks 'sly-db-exit-hook)
      (cond (stepping
             (setq sly-db-level nil)
             (run-with-timer 0.4 nil 'sly-db-close-step-buffer sly-db))
            ((not (eq sly-db (window-buffer (selected-window))))
             ;; A different window selection means an indirect,
             ;; non-interactive exit, we just kill the sly-db buffer.
             (kill-buffer))
            (t
             (quit-window t))))))

(defun sly-db-close-step-buffer (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (not sly-db-level)
        (quit-window t)))))


;;;;;; SLY-DB buffer insertion

(defun sly-db-insert-condition (condition)
  "Insert the text for CONDITION.
CONDITION should be a list (MESSAGE TYPE EXTRAS).
EXTRAS is currently used for the stepper."
  (cl-destructuring-bind (msg type extras) condition
    (insert (sly-db-in-face topline msg)
            "\n"
            (sly-db-in-face condition type))
    (sly-db-dispatch-extras extras)))

(defvar sly-db-extras-hooks nil
  "Handlers for the extra options sent in a debugger invocation.
Each function is called with one argument, a list (OPTION
VALUE). It should return non-nil iff it can handle OPTION, and
thus preventing other handlers from trying.

Functions are run in the SLDB buffer.")

(defun sly-db-dispatch-extras (extras)
  ;; this is (mis-)used for the stepper
  (dolist (extra extras)
    (sly-dcase extra
      ((:show-frame-source n)
       (sly-db-show-frame-source n))
      (t
       (or (run-hook-with-args-until-success 'sly-db-extras-hooks extra)
           ;;(error "Unhandled extra element:" extra)
           )))))

(defun sly-db-insert-restarts (restarts start count)
  "Insert RESTARTS and add the needed text props
RESTARTS should be a list ((NAME DESCRIPTION) ...)."
  (let* ((len (length restarts))
         (end (if count (min (+ start count) len) len)))
    (cl-loop for (name string) in (cl-subseq restarts start end)
             for number from start
             do (insert
                 " " (sly-db-in-face restart-number (number-to-string number))
                 ": "  (sly-make-action-button (format "[%s]" name)
                                               (let ((n number))
                                                 #'(lambda (_button)
                                                     (sly-db-invoke-restart n)))
                                               'restart-number number)
                 " " (sly-db-in-face restart string))
             (insert "\n"))
    (when (< end len)
      (insert (sly-make-action-button
               " --more--"
               #'(lambda (button)
                   (let ((inhibit-read-only t))
                     (delete-region (button-start button)
                                    (1+ (button-end button)))
                     (sly-db-insert-restarts restarts end nil)
                     (sly--when-let (win (get-buffer-window (current-buffer)))
                       (with-selected-window win
                         (sly-recenter (point-max))))))
               'point-entered #'(lambda (_ new) (push-button new)))
              "\n"))))

(defun sly-db-frame-restartable-p (frame-spec)
  (and (plist-get (cl-caddr frame-spec) :restartable) t))

(defun sly-db-prune-initial-frames (frame-specs)
  "Return the prefix of FRAMES-SPECS to initially present to the user.
Regexp heuristics are used to avoid showing SLYNK-internal frames."
  (let* ((case-fold-search t)
         (rx "^\\([() ]\\|lambda\\)*slynk\\>"))
    (or (cl-loop for frame-spec in frame-specs
                 until (string-match rx (cadr frame-spec))
                 collect frame-spec)
        frame-specs)))

(defun sly-db-insert-frames (frame-specs more)
  "Insert frames for FRAME-SPECS into buffer.
If MORE is non-nil, more frames are on the Lisp stack."
  (cl-loop
   for frame-spec in frame-specs
   do (sly-db-insert-frame frame-spec)
   finally
   (when more
     (insert (sly-make-action-button
              " --more--\n"
              (lambda (button)
                (let* ((inhibit-read-only t)
                       (count 40)
                       (from (1+ (car frame-spec)))
                       (to (+ from count))
                       (frames (sly-eval `(slynk:backtrace ,from ,to)))
                       (more (sly-length= frames count)))
                  (delete-region (button-start button)
                                 (button-end button))
                  (save-excursion
                    (sly-db-insert-frames frames more))
                  (sly--when-let (win (get-buffer-window (current-buffer)))
                    (with-selected-window win
                      (sly-recenter (point-max))))))
              'point-entered #'(lambda (_ new) (push-button new)))))))

(defvar sly-db-frame-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t")   'sly-db-toggle-details)
    (define-key map (kbd "v")   'sly-db-show-frame-source)
    (define-key map (kbd ".")   'sly-db-goto-source)
    (define-key map (kbd "D")   'sly-db-disassemble)
    (define-key map (kbd "e")   'sly-db-eval-in-frame)
    (define-key map (kbd "d")   'sly-db-pprint-eval-in-frame)
    (define-key map (kbd "i")   'sly-db-inspect-in-frame)
    (define-key map (kbd "r")   'sly-db-restart-frame)
    (define-key map (kbd "R")   'sly-db-return-from-frame)
    (define-key map (kbd "RET") 'sly-db-toggle-details)

    (define-key map "s"    'sly-db-step)
    (define-key map "x"    'sly-db-next)
    (define-key map "o"    'sly-db-out)
    (define-key map "b"    'sly-db-break-on-return)

    (define-key map "\C-c\C-c" 'sly-db-recompile-frame-source)

    (set-keymap-parent map sly-part-button-keymap)
    map))

(defvar sly-db-frame-menu-map
  (let ((map (make-sparse-keymap)))
    (cl-macrolet ((item (label sym)
                        `(define-key map [,sym] '(menu-item ,label ,sym))))
      (item "Dissassemble" sly-db-disassemble)
      (item "Eval In Context" sly-db-eval-in-frame)
      (item "Eval and Pretty Print In Context" sly-db-pprint-eval-in-frame)
      (item "Inspect In Context" sly-db-inspect-in-frame)
      (item "Restart" sly-db-restart-frame)
      (item "Return Value" sly-db-return-from-frame)
      (item "Toggle Details" sly-db-toggle-details)
      (item "Show Source" sly-db-show-frame-source)
      (item "Go To Source" sly-db-goto-source))
    (set-keymap-parent map sly-button-popup-part-menu-keymap)
    map))

(define-button-type 'sly-db-frame :supertype 'sly-part
  'keymap sly-db-frame-map
  'part-menu-keymap sly-db-frame-menu-map
  'action 'sly-db-toggle-details
  'mouse-action 'sly-db-toggle-details)

(defun sly-db--guess-frame-function (frame)
  (ignore-errors
    (car (car (read-from-string
               (replace-regexp-in-string "#" ""
                                         (cadr frame)))))))

(defun sly-db-frame-button (label frame face &rest props)
  (apply #'sly--make-text-button label nil :type 'sly-db-frame
         'face face
         'field (car frame)
         'frame-number (car frame)
         'frame-string (cadr frame)
         'part-args (list (car frame)
                          (sly-db--guess-frame-function frame))
         'part-label (format "Frame %d" (car frame))
         props))

(defun sly-db-frame-number-at-point ()
  (let ((button (sly-db-frame-button-near-point)))
    (button-get button 'frame-number)))

(defun sly-db-frame-button-near-point ()
  (or (sly-button-at nil 'sly-db-frame 'no-error)
      (get-text-property (point) 'nearby-frame-button)
      (error "No frame button here")))

(defun sly-db-insert-frame (frame-spec)
  "Insert a frame for FRAME-SPEC."
  (let* ((number (car frame-spec))
         (label (cadr frame-spec))
         (origin (point)))
    (insert
     (propertize (format "%2d: " number)
                 'face 'sly-db-frame-label-face)
     (sly-db-frame-button label frame-spec
                          (if (sly-db-frame-restartable-p frame-spec)
                              'sly-db-restartable-frame-line-face
                            'sly-db-frame-line-face))
     "\n")
    (add-text-properties
     origin (point)
     (list 'field number
           'keymap sly-db-frame-map
           'nearby-frame-button (button-at (- (point) 2))))))


;;;;;; SLY-DB examining text props
(defun sly-db--goto-last-visible-frame ()
  (goto-char (point-max))
  (while (not (get-text-property (point) 'frame-string))
    (goto-char (previous-single-property-change (point) 'frame-string))))

(defun sly-db-beginning-of-backtrace ()
  "Goto the first frame."
  (interactive)
  (goto-char sly-db-backtrace-start-marker))


;;;;; SLY-DB commands
(defun sly-db-cycle ()
  "Cycle between restart list and backtrace."
  (interactive)
  (let ((pt (point)))
    (cond ((< pt sly-db-restart-list-start-marker)
           (goto-char sly-db-restart-list-start-marker))
          ((< pt sly-db-backtrace-start-marker)
           (goto-char sly-db-backtrace-start-marker))
          (t
           (goto-char sly-db-restart-list-start-marker)))))

(defun sly-db-end-of-backtrace ()
  "Fetch the entire backtrace and go to the last frame."
  (interactive)
  (sly-db--fetch-all-frames)
  (sly-db--goto-last-visible-frame))

(defun sly-db--fetch-all-frames ()
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (sly-db--goto-last-visible-frame)
    (let ((last (sly-db-frame-number-at-point)))
      (goto-char (next-single-char-property-change (point) 'frame-string))
      (delete-region (point) (point-max))
      (save-excursion
        (insert "\n")
        (sly-db-insert-frames (sly-eval `(slynk:backtrace ,(1+ last) nil))
                              nil)))))


;;;;;; SLY-DB show source
(defun sly-db-show-frame-source (frame-number)
  "Highlight FRAME-NUMBER's expression in a source code buffer."
  (interactive (list (sly-db-frame-number-at-point)))
  (sly-eval-async
      `(slynk:frame-source-location ,frame-number)
    (lambda (source-location)
      (sly-dcase source-location
        ((:error message)
         (sly-message "%s" message)
         (ding))
        (t
         (sly--display-source-location source-location))))))


;;;;;; SLY-DB toggle details
(define-button-type 'sly-db-local-variable :supertype 'sly-part
  'sly-button-inspect
  #'(lambda (frame-id var-id)
      (sly-eval-for-inspector `(slynk:inspect-frame-var ,frame-id
                                                        ,var-id)) )
  'sly-button-pretty-print
  #'(lambda (frame-id var-id)
      (sly-eval-describe `(slynk:pprint-frame-var ,frame-id
                                                  ,var-id)))
  'sly-button-describe
  #'(lambda (frame-id var-id)
      (sly-eval-describe `(slynk:describe-frame-var ,frame-id
                                                    ,var-id))))

(defun sly-db-local-variable-button (label frame-number var-id &rest props)
  (apply #'sly--make-text-button label nil
         :type 'sly-db-local-variable
         'part-args (list frame-number var-id)
         'part-label (format "Local Variable %d" var-id) props))

(defun sly-db-frame-details-region (frame-button)
  "Get (BEG END) for FRAME-BUTTON's details, or nil if hidden"
  (let ((beg (button-end frame-button))
        (end (1- (field-end (button-start frame-button) 'escape))))
    (unless (= beg end) (list beg end))))

(defun sly-db-toggle-details (frame-button)
  "Toggle display of details for the current frame.
The details include local variable bindings and CATCH-tags."
  (interactive (list (sly-db-frame-button-near-point)))
  (if (sly-db-frame-details-region frame-button)
      (sly-db-hide-frame-details frame-button)
    (sly-db-show-frame-details frame-button)))

(defun sly-db-show-frame-details (frame-button)
  "Show details for FRAME-BUTTON"
  (interactive (list (sly-db-frame-button-near-point)))
  (cl-destructuring-bind (locals catches)
      (sly-eval `(slynk:frame-locals-and-catch-tags
                  ,(button-get frame-button 'frame-number)))
    (let ((inhibit-read-only t)
          (inhibit-point-motion-hooks t))
      (save-excursion
        (goto-char (button-end frame-button))
        (let ((indent1 "      ")
              (indent2 "        "))
          (insert "\n" indent1
                  (sly-db-in-face section (if locals "Locals:" "[No Locals]")))
          (cl-loop for i from 0
                   for var in locals
                   with frame-number = (button-get frame-button 'frame-number)
                   do
                   (cl-destructuring-bind (&key name id value) var
                     (insert "\n"
                             indent2
                             (sly-db-in-face local-name
                               (concat name (if (zerop id)
                                                ""
                                              (format "#%d" id))))
                             " = "
                             (sly-db-local-variable-button value
                                                           frame-number
                                                           i))))
          (when catches
            (insert "\n" indent1 (sly-db-in-face section "Catch-tags:"))
            (dolist (tag catches)
              (sly-propertize-region `(catch-tag ,tag)
                (insert "\n" indent2 (sly-db-in-face catch-tag
                                       (format "%s" tag))))))
          ;; The whole details field is propertized accordingly...
          ;;
          (add-text-properties (button-start frame-button) (point)
                               (list 'field (button-get frame-button 'field)
                                     'keymap sly-db-frame-map
                                     'nearby-frame-button frame-button))
          ;; ...but we must remember to remove the 'keymap property from
          ;; any buttons inside the field
          ;;
          (cl-loop for pos = (point) then (button-start button)
                   for button = (previous-button pos)
                   while (and button
                              (> (button-start button)
                                 (button-start frame-button)))
                   do (remove-text-properties (button-start button)
                                              (button-end button)
                                              '(keymap nil))))))
    (sly-recenter (field-end (button-start frame-button) 'escape))))

(defun sly-db-hide-frame-details (frame-button)
  (interactive (list (sly-db-frame-button-near-point)))
  (let* ((inhibit-read-only t)
         (to-delete (sly-db-frame-details-region frame-button)))
    (cl-assert to-delete)
    (when (and (< (car to-delete) (point))
               (< (point) (cadr to-delete)))
      (goto-char (button-start frame-button)))
    (apply #'delete-region to-delete)))

(defun sly-db-disassemble (frame-number)
  "Disassemble the code for frame with FRAME-NUMBER."
  (interactive (list (sly-db-frame-number-at-point)))
  (sly-eval-async `(slynk:sly-db-disassemble ,frame-number)
    (lambda (result)
      (sly-show-description result nil))))


;;;;;; SLY-DB eval and inspect

(defun sly-db-eval-in-frame (frame-number string package)
  "Prompt for an expression and evaluate it in the selected frame."
  (interactive (sly-db-frame-eval-interactive "Eval in frame (%s)> "))
  (sly-eval-async `(slynk:eval-string-in-frame ,string ,frame-number ,package)
    'sly-display-eval-result))

(defun sly-db-pprint-eval-in-frame (frame-number string package)
  "Prompt for an expression, evaluate in selected frame, pretty-print result."
  (interactive (sly-db-frame-eval-interactive "Eval in frame (%s)> "))
  (sly-eval-async
      `(slynk:pprint-eval-string-in-frame ,string ,frame-number ,package)
    (lambda (result)
      (sly-show-description result nil))))

(defun sly-db-frame-eval-interactive (fstring)
  (let* ((frame-number (sly-db-frame-number-at-point))
         (pkg (sly-eval `(slynk:frame-package-name ,frame-number))))
    (list frame-number
          (let ((sly-buffer-package pkg))
            (sly-read-from-minibuffer (format fstring pkg)))
          pkg)))

(defun sly-db-inspect-in-frame (frame-number string)
  "Prompt for an expression and inspect it in the selected frame."
  (interactive (list
                (sly-db-frame-number-at-point)
                (sly-read-from-minibuffer
                 "Inspect in frame (evaluated): "
                 (sly-sexp-at-point))))
  (sly-eval-for-inspector `(slynk:inspect-in-frame ,string ,frame-number)))

(defun sly-db-inspect-condition ()
  "Inspect the current debugger condition."
  (interactive)
  (sly-eval-for-inspector '(slynk:inspect-current-condition)))

(defun sly-db-print-condition ()
  (interactive)
  (sly-eval-describe `(slynk:sdlb-print-condition)))


;;;;;; SLY-DB movement

(defun sly-db-down (arg)
  "Move down ARG frames. With negative ARG, move up."
  (interactive "p")
  (cl-loop
   for i from 0 below (abs arg)
   do (cl-loop
       for tries from 0 below 2
       for pos = (point) then next-change
       for next-change = (funcall (if (cl-minusp arg)
                                      #'previous-single-char-property-change
                                    #'next-single-char-property-change)
                                  pos 'frame-number)
       for prop-value = (get-text-property next-change 'frame-number)
       when prop-value do (goto-char next-change)
       until prop-value)))

(defun sly-db-up (arg)
  "Move up ARG frames. With negative ARG, move down."
  (interactive "p")
  (sly-db-down (- (or arg 1))))

(defun sly-db-sugar-move (move-fn arg)
  (let ((current-frame-button (sly-db-frame-button-near-point)))
    (when (and current-frame-button
               (sly-db-frame-details-region current-frame-button))
      (sly-db-hide-frame-details current-frame-button)))
  (funcall move-fn arg)
  (let ((frame-button (sly-db-frame-button-near-point)))
    (when frame-button
      (sly-db-show-frame-source (button-get frame-button 'frame-number))
      (sly-db-show-frame-details frame-button))))

(defun sly-db-details-up (arg)
  "Move up ARG frames and show details."
  (interactive "p")
  (sly-db-sugar-move 'sly-db-up arg))

(defun sly-db-details-down (arg)
  "Move down ARG frames and show details."
  (interactive "p")
  (sly-db-sugar-move 'sly-db-down arg))


;;;;;; SLY-DB restarts

(defun sly-db-quit ()
  "Quit to toplevel."
  (interactive)
  (cl-assert sly-db-restarts () "sly-db-quit called outside of sly-db buffer")
  (sly-rex () ('(slynk:throw-to-toplevel))
    ((:ok x) (error "sly-db-quit returned [%s]" x))
    ((:abort _))))

(defun sly-db-continue ()
  "Invoke the \"continue\" restart."
  (interactive)
  (cl-assert sly-db-restarts () "sly-db-continue called outside of sly-db buffer")
  (sly-rex ()
      ('(slynk:sly-db-continue))
    ((:ok _)
     (sly-message "No restart named continue")
     (ding))
    ((:abort _))))

(defun sly-db-abort ()
  "Invoke the \"abort\" restart."
  (interactive)
  (sly-eval-async '(slynk:sly-db-abort)
    (lambda (v) (sly-message "Restart returned: %S" v))))

(defun sly-db-invoke-restart (restart-number)
  "Invoke the restart number NUMBER.
Interactively get the number from a button at point."
  (interactive (button-get (sly-button-at (point)) 'restart-number))
  (sly-rex ()
      ((list 'slynk:invoke-nth-restart-for-emacs sly-db-level restart-number))
    ((:ok value) (sly-message "Restart returned: %s" value))
    ((:abort _))))

(defun sly-db-invoke-restart-by-name (restart-name)
  (interactive (list (let ((completion-ignore-case t))
                       (completing-read "Restart: " sly-db-restarts nil t
                                        ""
                                        'sly-db-invoke-restart-by-name))))
  (sly-db-invoke-restart (cl-position restart-name sly-db-restarts
                                      :test 'string= :key #'cl-first)))

(defun sly-db-break-with-default-debugger (&optional dont-unwind)
  "Enter default debugger."
  (interactive "P")
  (sly-rex ()
      ((list 'slynk:sly-db-break-with-default-debugger
             (not (not dont-unwind)))
       nil sly-current-thread)
    ((:abort _))))

(defun sly-db-break-with-system-debugger (&optional lightweight)
  "Enter system debugger (gdb)."
  (interactive "P")
  (sly-attach-gdb sly-buffer-connection lightweight))

(defun sly-attach-gdb (connection &optional lightweight)
  "Run `gud-gdb'on the connection with PID `pid'.

If `lightweight' is given, do not send any request to the
inferior Lisp (e.g. to obtain default gdb config) but only
operate from the Emacs side; intended for cases where the Lisp is
truly screwed up."
  (interactive
   (list (sly-read-connection "Attach gdb to: " (sly-connection)) "P"))
  (let ((pid  (sly-pid connection))
        (file (sly-lisp-implementation-program connection))
        (commands (unless lightweight
                    (let ((sly-dispatching-connection connection))
                      (sly-eval `(slynk:gdb-initial-commands))))))
    (gud-gdb (format "gdb -p %d %s" pid (or file "")))
    (with-current-buffer gud-comint-buffer
      (dolist (cmd commands)
        ;; First wait until gdb was initialized, then wait until current
        ;; command was processed.
        (while (not (looking-back comint-prompt-regexp (line-beginning-position)
                                  nil))
          (sit-for 0.01))
        ;; We do not use `gud-call' because we want the initial commands
        ;; to be displayed by the user so he knows what he's got.
        (insert cmd)
        (comint-send-input)))))

(defun sly-read-connection (prompt &optional initial-value)
  "Read a connection from the minibuffer.
Return the net process, or nil."
  (cl-assert (memq initial-value sly-net-processes))
  (let* ((to-string (lambda (p)
                      (format "%s (pid %d)"
                              (sly-connection-name p) (sly-pid p))))
         (candidates (mapcar (lambda (p) (cons (funcall to-string p) p))
                             sly-net-processes)))
    (cdr (assoc (completing-read prompt candidates
                                 nil t (funcall to-string initial-value))
                candidates))))

(defun sly-db-step (frame-number)
  "Step to next basic-block boundary."
  (interactive (list (sly-db-frame-number-at-point)))
  (sly-eval-async `(slynk:sly-db-step ,frame-number)))

(defun sly-db-next (frame-number)
  "Step over call."
  (interactive (list (sly-db-frame-number-at-point)))
  (sly-eval-async `(slynk:sly-db-next ,frame-number)))

(defun sly-db-out (frame-number)
  "Resume stepping after returning from this function."
  (interactive (list (sly-db-frame-number-at-point)))
  (sly-eval-async `(slynk:sly-db-out ,frame-number)))

(defun sly-db-break-on-return (frame-number)
  "Set a breakpoint at the current frame.
The debugger is entered when the frame exits."
  (interactive (list (sly-db-frame-number-at-point)))
  (sly-eval-async `(slynk:sly-db-break-on-return ,frame-number)
    (lambda (msg) (sly-message "%s" msg))))

(defun sly-db-break (name)
  "Set a breakpoint at the start of the function NAME."
  (interactive (list (sly-read-symbol-name "Function: " t)))
  (sly-eval-async `(slynk:sly-db-break ,name)
    (lambda (msg) (sly-message "%s" msg))))

(defun sly-db-return-from-frame (frame-number string)
  "Reads an expression in the minibuffer and causes the function to
return that value, evaluated in the context of the frame."
  (interactive (list (sly-db-frame-number-at-point)
                     (sly-read-from-minibuffer "Return from frame: ")))
  (sly-rex ()
      ((list 'slynk:sly-db-return-from-frame frame-number string))
    ((:ok value) (sly-message "%s" value))
    ((:abort _))))

(defun sly-db-restart-frame (frame-number)
  "Causes the frame to restart execution with the same arguments as it
was called originally."
  (interactive (list (sly-db-frame-number-at-point)))
  (sly-rex ()
      ((list 'slynk:restart-frame frame-number))
    ((:ok value) (sly-message "%s" value))
    ((:abort _))))

(defun sly-toggle-break-on-signals ()
  "Toggle the value of *break-on-signals*."
  (interactive)
  (sly-eval-async `(slynk:toggle-break-on-signals)
    (lambda (msg) (sly-message "%s" msg))))


;;;;;; SLY-DB recompilation commands

(defun sly-db-recompile-frame-source (frame-number &optional raw-prefix-arg)
  (interactive
   (list (sly-db-frame-number-at-point) current-prefix-arg))
  (sly-eval-async
      `(slynk:frame-source-location ,frame-number)
    (let ((policy (sly-compute-policy raw-prefix-arg)))
      (lambda (source-location)
        (sly-dcase source-location
          ((:error message)
           (sly-message "%s" message)
           (ding))
          (t
           (let ((sly-compilation-policy policy))
             (sly-recompile-location source-location))))))))


;;;; Thread control panel

(defvar sly-threads-buffer-timer nil)

(defcustom sly-threads-update-interval nil
  "Interval at which the list of threads will be updated."
  :type '(choice
          (number :value 0.5)
          (const nil))
  :group 'sly-ui)

(defun sly-list-threads ()
  "Display a list of threads."
  (interactive)
  (let ((name (sly-buffer-name :threads
                               :connection t)))
    (sly-with-popup-buffer (name :connection t
                                 :mode 'sly-thread-control-mode)
      (sly-update-threads-buffer (current-buffer))
      (goto-char (point-min))
      (when sly-threads-update-interval
        (when sly-threads-buffer-timer
          (cancel-timer sly-threads-buffer-timer))
        (setq sly-threads-buffer-timer
              (run-with-timer
               sly-threads-update-interval
               sly-threads-update-interval
               'sly-update-threads-buffer
               (current-buffer))))
      (add-hook 'kill-buffer-hook  'sly--threads-buffer-teardown
                'append 'local))))

(defun sly--threads-buffer-teardown ()
  (when sly-threads-buffer-timer
    (cancel-timer sly-threads-buffer-timer))
  (when (process-live-p sly-buffer-connection)
    (sly-eval-async `(slynk:quit-thread-browser))))

(defun sly-update-threads-buffer (&optional buffer)
  (interactive)
  (with-current-buffer (or buffer
                           (current-buffer))
    (sly-eval-async '(slynk:list-threads)
      #'(lambda (threads)
          (with-current-buffer (current-buffer)
            (sly--display-threads threads))))))

(defun sly-move-point (position)
  "Move point in the current buffer and in the window the buffer is displayed."
  (let ((window (get-buffer-window (current-buffer) t)))
    (goto-char position)
    (when window
      (set-window-point window position))))

(defun sly--display-threads (threads)
  (let* ((inhibit-read-only t)
         (old-thread-id (get-text-property (point) 'thread-id))
         (old-line (line-number-at-pos))
         (old-column (current-column)))
    (erase-buffer)
    (sly-insert-threads threads)
    (let ((new-line (cl-position old-thread-id (cdr threads)
                                 :key #'car :test #'equal)))
      (goto-char (point-min))
      (forward-line (or new-line old-line))
      (move-to-column old-column)
      (sly-move-point (point)))))

(defun sly-transpose-lists (list-of-lists)
  (let ((ncols (length (car list-of-lists))))
    (cl-loop for col-index below ncols
             collect (cl-loop for row in list-of-lists
                              collect (elt row col-index)))))

(defun sly-insert-table-row (line line-props col-props col-widths)
  (sly-propertize-region line-props
    (cl-loop for string in line
             for col-prop in col-props
             for width in col-widths do
             (sly-insert-propertized col-prop string)
             (insert-char ?\ (- width (length string))))))

(defun sly-insert-table (rows header row-properties column-properties)
  "Insert a \"table\" so that the columns are nicely aligned."
  (let* ((ncols (length header))
         (lines (cons header rows))
         (widths (cl-loop for columns in (sly-transpose-lists lines)
                          collect (1+ (cl-loop for cell in columns
                                               maximize (length cell)))))
         (header-line (with-temp-buffer
                        (sly-insert-table-row
                         header nil (make-list ncols nil) widths)
                        (buffer-string))))
    (cond ((boundp 'header-line-format)
           (setq header-line-format header-line))
          (t (insert header-line "\n")))
    (cl-loop for line in rows  for line-props in row-properties do
             (sly-insert-table-row line line-props column-properties widths)
             (insert "\n"))))

(defvar sly-threads-table-properties
  '(nil (face bold)))

(defun sly-insert-threads (threads)
  (let* ((labels (car threads))
         (threads (cdr threads))
         (header (cl-loop for label in labels collect
                          (capitalize (substring (symbol-name label) 1))))
         (rows (cl-loop for thread in threads collect
                        (cl-loop for prop in thread collect
                                 (format "%s" prop))))
         (line-props (cl-loop for (id) in threads for i from 0
                              collect `(thread-index ,i thread-id ,id)))
         (col-props (cl-loop for nil in labels for i from 0 collect
                             (nth i sly-threads-table-properties))))
    (sly-insert-table rows header line-props col-props)))


;;;;; Major mode
(defvar sly-thread-control-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'sly-thread-attach)
    (define-key map "d" 'sly-thread-debug)
    (define-key map "g" 'sly-update-threads-buffer)
    (define-key map "k" 'sly-thread-kill)
    (define-key map "q" 'quit-window)
    map))

(define-derived-mode sly-thread-control-mode fundamental-mode
  "Threads"
  "SLY Thread Control Panel Mode.

\\{sly-thread-control-mode-map}"
  (when sly-truncate-lines
    (set (make-local-variable 'truncate-lines) t))
  (read-only-mode 1)
  (sly-mode 1)
  (setq buffer-undo-list t))

(defun sly-thread-kill ()
  (interactive)
  (sly-eval `(cl:mapc 'slynk:kill-nth-thread
                      ',(sly-get-properties 'thread-index)))
  (call-interactively 'sly-update-threads-buffer))

(defun sly-get-region-properties (prop start end)
  (cl-loop for position = (if (get-text-property start prop)
                              start
                            (next-single-property-change start prop))
           then (next-single-property-change position prop)
           while (<= position end)
           collect (get-text-property position prop)))

(defun sly-get-properties (prop)
  (if (use-region-p)
      (sly-get-region-properties prop
                                 (region-beginning)
                                 (region-end))
    (let ((value (get-text-property (point) prop)))
      (when value
        (list value)))))

(defun sly-thread-attach ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-index))
        (file (sly-slynk-port-file)))
    (sly-eval-async `(slynk:start-slynk-server-in-thread ,id ,file)))
  (sly-read-port-and-connect nil))

(defun sly-thread-debug ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-index)))
    (sly-eval-async `(slynk:debug-nth-thread ,id))))


;;;;; Connection listing

(defvar sly-connection-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d"         'sly-connection-list-make-default)
    (define-key map "g"         'sly-update-connection-list)
    (define-key map (kbd "RET") 'sly-connection-list-default-action)
    (define-key map (kbd "C-m")      'sly-connection-list-default-action)
    (define-key map (kbd "C-k") 'sly-quit-connection-at-point)
    (define-key map (kbd "R")   'sly-restart-connection-at-point)
    (define-key map (kbd "q")   'quit-window)
    map))

(define-derived-mode sly-connection-list-mode tabulated-list-mode
  "SLY-Connections"
  "SLY Connection List Mode.

\\{sly-connection-list-mode-map}"
  (set (make-local-variable 'tabulated-list-format)
       `[("Default" 8) ("Name" 24 t) ("Host" 12)
         ("Port" 6) ("Pid" 6 t) ("Type" 1000 t)])
  (tabulated-list-init-header))

(defun sly--connection-at-point ()
  (or (get-text-property (point) 'tabulated-list-id)
      (error "No connection at point")))

(defvar sly-connection-list-button-action nil)

(defun sly-connection-list-default-action (connection)
  (interactive (list (sly--connection-at-point)))
  (funcall sly-connection-list-button-action connection))

(defun sly-update-connection-list ()
  (interactive)
  (set (make-local-variable 'tabulated-list-entries)
       (mapcar
        #'(lambda (p)
            (list p
                  `[,(if (eq sly-default-connection p) "*" " ")
                    (,(file-name-nondirectory (or (sly-connection-name p)
                                                  "unknown"))
                     action
                     ,#'(lambda (_button)
                          (and sly-connection-list-button-action
                               (funcall sly-connection-list-button-action p))))
                    ,(car (process-contact p))
                    ,(format "%s" (cl-second (process-contact p)))
                    ,(format "%s" (sly-pid p))
                    ,(or (sly-lisp-implementation-type p)
                         "unknown")]))
        (reverse sly-net-processes)))
  (let ((p (point)))
    (tabulated-list-print)
    (goto-char p)))

(defun sly-quit-connection-at-point (connection)
  (interactive (list (sly--connection-at-point)))
  (let ((sly-dispatching-connection connection)
        (end (time-add (current-time) (seconds-to-time 3))))
    (sly-quit-lisp t)
    (while (memq connection sly-net-processes)
      (when (time-less-p end (current-time))
        (sly-message "Quit timeout expired.  Disconnecting.")
        (delete-process connection))
      (sit-for 0.1)))
  (sly-update-connection-list))

(defun sly-restart-connection-at-point (connection)
  (interactive (list (sly--connection-at-point)))
  (when (sly-y-or-n-p "Really restart '%s'" (sly-connection-name connection))
    (let ((sly-dispatching-connection connection))
      (sly-restart-inferior-lisp))))

(defun sly-connection-list-make-default ()
  "Make the connection at point the default connection."
  (interactive)
  (sly-select-connection (sly--connection-at-point))
  (sly-update-connection-list))

(defun sly-list-connections ()
  "Display a list of all connections."
  (interactive)
  (sly-with-popup-buffer ((sly-buffer-name :connections)
                          :mode 'sly-connection-list-mode)
    (sly-update-connection-list)))



;;;; Inspector

(defgroup sly-inspector nil
  "Options for the SLY inspector."
  :prefix "sly-inspector-"
  :group 'sly)

(defvar sly--this-inspector-name nil
  "Buffer-local inspector name (a string), or nil")

(cl-defun sly-eval-for-inspector (slyfun-and-args
                                  &key (error-message "Couldn't inspect")
                                  restore-point
                                  save-selected-window
                                  (inspector-name sly--this-inspector-name)
                                  opener)
  (if (cl-some #'listp slyfun-and-args)
      (sly-warning
       "`sly-eval-for-inspector' not meant to be passed a generic form"))
  (let ((pos (and (eq major-mode 'sly-inspector-mode)
                  (sly-inspector-position))))
    (sly-eval-async `(slynk:eval-for-inspector
                      ,sly--this-inspector-name ; current inspector, if any
                      ,inspector-name   ; target inspector, if any
                      ',(car slyfun-and-args)
                      ,@(cdr slyfun-and-args))
      (or opener
          (lambda (results)
            (let ((opener (lambda ()
                            (sly--open-inspector
                             results
                             :point (and restore-point pos)
                             :inspector-name inspector-name
                             :switch (not save-selected-window)))))
              (cond (results
                     (funcall opener))
                    (t
                     (sly-message error-message)))))))))

(defun sly-read-inspector-name ()
  (let* ((names (cl-loop for b in (buffer-list)
                         when (with-current-buffer b
                                (and (eq sly-buffer-connection
                                         (sly-current-connection))
                                     (eq major-mode 'sly-inspector-mode)))
                         when (buffer-local-value 'sly--this-inspector-name b)
                         collect it))
         (result (completing-read "Inspector name: " (cons "default"
                                                           names)
                                  nil nil nil nil "default")))
    (unless (string= result "default")
      result)))

(defun sly-maybe-read-inspector-name ()
  (or (and current-prefix-arg
           (sly-read-inspector-name))
      sly--this-inspector-name))

(defun sly-inspect (string &optional inspector-name)
  "Eval an expression and inspect the result."
  (interactive
   (let* ((name (sly-maybe-read-inspector-name))
          (string (sly-read-from-minibuffer
                   (concat "Inspect value"
                           (and name
                                (format " in inspector \"%s\"" name))
                           " (evaluated): ")
                   (sly-sexp-at-point 'interactive nil nil))))
     (list string name)))
  (sly-eval-for-inspector `(slynk:init-inspector ,string)
                          :inspector-name inspector-name))

(defvar sly-inspector-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "l" 'sly-inspector-pop)
    (define-key map "n" 'sly-inspector-next)
    (define-key map [mouse-6] 'sly-inspector-pop)
    (define-key map [mouse-7] 'sly-inspector-next)

    (define-key map " " 'sly-inspector-next)
    (define-key map "D" 'sly-inspector-describe-inspectee)
    (define-key map "e" 'sly-inspector-eval)
    (define-key map "h" 'sly-inspector-history)
    (define-key map "g" 'sly-inspector-reinspect)
    (define-key map ">" 'sly-inspector-fetch-all)
    (define-key map "q" 'sly-inspector-quit)

    (set-keymap-parent map button-buffer-map)
    map))

(define-derived-mode sly-inspector-mode fundamental-mode
  "SLY-Inspector"
  "
\\{sly-inspector-mode-map}"
  (set-syntax-table lisp-mode-syntax-table)
  (sly-set-truncate-lines)
  (setq buffer-read-only t)
  (sly-mode 1))

(define-button-type 'sly-inspector-part :supertype 'sly-part
  'sly-button-inspect
  #'(lambda (id)
      (sly-eval-for-inspector `(slynk:inspect-nth-part ,id)
                              :inspector-name (sly-maybe-read-inspector-name)))
  'sly-button-pretty-print
  #'(lambda (id)
      (sly-eval-describe `(slynk:pprint-inspector-part ,id)))
  'sly-button-describe
  #'(lambda (id)
      (sly-eval-describe `(slynk:describe-inspector-part ,id)))
  'sly-button-show-source
  #'(lambda (id)
      (sly-eval-async
          `(slynk:find-source-location-for-emacs '(:inspector ,id))
        #'(lambda (result)
            (sly--display-source-location result 'noerror)))))

(defun sly-inspector-part-button (label id &rest props)
  (apply #'sly--make-text-button
         label nil
         :type 'sly-inspector-part
         'part-args (list id)
         'part-label "Inspector Object"
         props))

(defmacro sly-inspector-fontify (face string)
  `(sly-add-face ',(intern (format "sly-inspector-%s-face" face)) ,string))

(cl-defun sly--open-inspector (inspected-parts
                               &key point kill-hook inspector-name (switch t))
  "Display INSPECTED-PARTS in a new inspector window.
Optionally set point to POINT. If KILL-HOOK is provided, it is
added to local KILL-BUFFER hooks for the inspector
buffer. INSPECTOR-NAME is the name of the target inspector, or
nil if the default one is to be used. SWITCH indicates the
buffer should be switched to (defaults to t)"
  (sly-with-popup-buffer ((sly-buffer-name :inspector
                                           :connection t
                                           :suffix inspector-name)
                          :mode 'sly-inspector-mode
                          :select switch
                          :same-window-p
                          (and (eq major-mode 'sly-inspector-mode)
                               (or (null inspector-name)
                                   (eq sly--this-inspector-name inspector-name)))
                          :connection t)
    (when kill-hook
      (add-hook 'kill-buffer-hook kill-hook t t))
    (set (make-local-variable 'sly--this-inspector-name) inspector-name)
    (cl-destructuring-bind (&key id title content) inspected-parts
      (cl-macrolet ((fontify (face string)
                             `(sly-inspector-fontify ,face ,string)))
        (insert (sly-inspector-part-button title id 'skip t))
        (while (eq (char-before) ?\n)
          (backward-delete-char 1))
        (insert "\n" (fontify label "--------------------") "\n")
        (save-excursion
          (sly-inspector-insert-content content))
        (when point
          (cl-check-type point cons)
          (ignore-errors
            (goto-char (point-min))
            (forward-line (1- (car point)))
            (move-to-column (cdr point))))))
    (buffer-disable-undo)))

(defvar sly-inspector-limit 500)

(defun sly-inspector-insert-content (content)
  (sly-inspector-fetch-chunk
   content nil
   (lambda (chunk)
     (let ((inhibit-read-only t))
       (sly-inspector-insert-chunk chunk t t)))))

(defun sly-inspector-insert-chunk (chunk prev next)
  "Insert CHUNK at point.
If PREV resp. NEXT are true insert more-buttons as needed."
  (cl-destructuring-bind (ispecs len start end) chunk
    (when (and prev (> start 0))
      (sly-inspector-insert-more-button start t))
    (mapc #'sly-inspector-insert-ispec ispecs)
    (when (and next (< end len))
      (sly-inspector-insert-more-button end nil))))

(defun sly-inspector-insert-ispec (ispec)
  (insert
   (if (stringp ispec) ispec
     (sly-dcase ispec
       ((:value string id)
        (sly-inspector-part-button string id))
       ((:label string)
        (sly-inspector-fontify label string))
       ((:action string id)
        (sly-make-action-button
         string
         #'(lambda (_button)
             (sly-eval-for-inspector `(slynk::inspector-call-nth-action ,id)
                                     :restore-point t))))))))

(defun sly-inspector-position ()
  "Return a pair (Y-POSITION X-POSITION) representing the
position of point in the current buffer."
  ;; We make sure we return absolute coordinates even if the user has
  ;; narrowed the buffer.
  ;; FIXME: why would somebody narrow the buffer?
  (save-restriction
    (widen)
    (cons (line-number-at-pos)
          (current-column))))

(defun sly-inspector-pop ()
  "Reinspect the previous object."
  (interactive)
  (sly-eval-for-inspector `(slynk:inspector-pop)
                          :error-message "No previous object"))

(defun sly-inspector-next ()
  "Inspect the next object in the history."
  (interactive)
  (sly-eval-for-inspector `(slynk:inspector-next)
                          :error-message "No next object"))

(defun sly-inspector-quit (&optional reset)
  "Quit the inspector.  If RESET, clear Lisp-side history.
If RESET, any references to inspectee's that may be holding up
garbage collection are released.  If RESET, the buffer is
killed (since it would become useless otherwise), else it is just
buried."
  (interactive "P")
  (when reset (sly-eval-async `(slynk:quit-inspector)))
  (quit-window reset))

(defun sly-inspector-describe-inspectee ()
  "Describe the currently inspected object"
  (interactive)
  (sly-eval-describe `(slynk:describe-inspectee)))

(defun sly-inspector-eval (string)
  "Eval an expression in the context of the inspected object.
The `*' variable will be bound to the inspected object."
  (interactive (list (sly-read-from-minibuffer "Inspector eval: ")))
  (sly-eval-with-transcript `(slynk:inspector-eval ,string)))

(defun sly-inspector-history ()
  "Show the previously inspected objects."
  (interactive)
  (sly-eval-describe `(slynk:inspector-history)))

(defun sly-inspector-reinspect (&optional inspector-name)
  (interactive (list (sly-maybe-read-inspector-name)))
  (sly-eval-for-inspector `(slynk:inspector-reinspect)
                          :inspector-name inspector-name))

(defun sly-inspector-toggle-verbose ()
  (interactive)
  (sly-eval-for-inspector `(slynk:inspector-toggle-verbose)))

(defun sly-inspector-insert-more-button (index previous)
  (insert (sly-make-action-button
           (if previous " [--more--]\n" " [--more--]")
           #'sly-inspector-fetch-more
           'range-args (list index previous))))

(defun sly-inspector-fetch-all ()
  "Fetch all inspector contents and go to the end."
  (interactive)
  (let ((button (button-at (1- (point-max)))))
    (cond ((and button
                (button-get button 'range-args))
           (let (sly-inspector-limit)
             (sly-inspector-fetch-more button)))
          (t
           (sly-error "No more elements to fetch")))))

(defun sly-inspector-fetch-more (button)
  (cl-destructuring-bind (index prev) (button-get button 'range-args)
    (sly-inspector-fetch-chunk
     (list '() (1+ index) index index) prev
     (sly-rcurry
      (lambda (chunk prev)
        (let ((inhibit-read-only t))
          (delete-region (button-start button) (button-end button))
          (sly-inspector-insert-chunk chunk prev (not prev))))
      prev))))

(defun sly-inspector-fetch-chunk (chunk prev cont)
  (sly-inspector-fetch chunk sly-inspector-limit prev cont))

(defun sly-inspector-fetch (chunk limit prev cont)
  (cl-destructuring-bind (from to)
      (sly-inspector-next-range chunk limit prev)
    (cond ((and from to)
           (sly-eval-for-inspector
            `(slynk:inspector-range ,from ,to)
            :opener (sly-rcurry (lambda (chunk2 chunk1 limit prev cont)
                                  (sly-inspector-fetch
                                   (sly-inspector-join-chunks chunk1 chunk2)
                                   limit prev cont))
                                chunk limit prev cont)))
          (t (funcall cont chunk)))))

(defun sly-inspector-next-range (chunk limit prev)
  (cl-destructuring-bind (_ len start end) chunk
    (let ((count (- end start)))
      (cond ((and prev (< 0 start) (or (not limit) (< count limit)))
             (list (if limit (max (- end limit) 0) 0) start))
            ((and (not prev) (< end len) (or (not limit) (< count limit)))
             (list end (if limit (+ start limit) most-positive-fixnum)))
            (t '(nil nil))))))

(defun sly-inspector-join-chunks (chunk1 chunk2)
  (cl-destructuring-bind (i1 _l1 s1 e1) chunk1
    (cl-destructuring-bind (i2 l2 s2 e2) chunk2
      (cond ((= e1 s2)
             (list (append i1 i2) l2 s1 e2))
            ((= e2 s1)
             (list (append i2 i1) l2 s2 e1))
            (t (error "Invalid chunks"))))))


;;;; Indentation

(defun sly-update-indentation ()
  "Update indentation for all macros defined in the Lisp system."
  (interactive)
  (sly-eval-async '(slynk:update-indentation-information)))

(defvar sly-indentation-update-hooks)

(defun sly-intern-indentation-spec (spec)
  (cond ((consp spec)
         (cons (sly-intern-indentation-spec (car spec))
               (sly-intern-indentation-spec (cdr spec))))
        ((stringp spec)
         (intern spec))
        (t
         spec)))

;; FIXME: restore the old version without per-package
;; stuff. sly-indentation.el should be able tho disable the simple
;; version if needed.
(defun sly-handle-indentation-update (alist)
  "Update Lisp indent information.

ALIST is a list of (SYMBOL-NAME . INDENT-SPEC) of proposed indentation
settings for `sly-common-lisp-indent-function'. The appropriate property
is setup, unless the user already set one explicitly."
  (dolist (info alist)
    (let ((symbol (intern (car info)))
          (indent (sly-intern-indentation-spec (cl-second info)))
          (packages (cl-third info)))
      (if (and (boundp 'sly-common-lisp-system-indentation)
               (fboundp 'sly-update-system-indentation))
          ;; A table provided by sly-cl-indent.el.
          (funcall #'sly-update-system-indentation symbol indent packages)
        ;; Does the symbol have an indentation value that we set?
        (when (equal (get symbol 'sly-common-lisp-indent-function)
                     (get symbol 'sly-indent))
          (put symbol 'sly-common-lisp-indent-function indent)
          (put symbol 'sly-indent indent)))
      (run-hook-with-args 'sly-indentation-update-hooks
                          symbol indent packages))))


;;;; Contrib modules

(defun sly-contrib--load-slynk-dependencies ()
  (let ((needed (cl-remove-if (lambda (s)
                                (cl-find (symbol-name s)
                                         (sly-lisp-modules)
                                         :key #'downcase
                                         :test #'string=))
                              sly-contrib--required-slynk-modules
                              :key #'car)))
    (when needed
      ;; No asynchronous request because with :SPAWN that could result
      ;; in the attempt to load modules concurrently which may not be
      ;; supported by the host Lisp.
      (sly-eval `(slynk:slynk-add-load-paths ',(cl-remove-duplicates
                                                (mapcar #'cl-second needed)
                                                :test #'string=)))
      (let* ((result (sly-eval
                      `(slynk:slynk-require
                        ',(mapcar #'symbol-name (mapcar #'cl-first needed)))))
             (all-modules (cl-first result))
             (loaded-now (cl-second result)))
        ;; check if everything went OK
        ;;
        (cl-loop for n in needed
                 unless (cl-find (cl-first n) loaded-now :test #'string=)

                 ;; string= compares symbols and strings nicely
                 ;;
                 do (when (y-or-n-p (format
                                     "\
Watch out! SLY failed to load SLYNK module %s for contrib %s!\n
Disable it?" (cl-first n) (cl-third n)))
                      (sly-disable-contrib (cl-third n))
                      (sly-temp-message 3 3 "\
You'll need to re-enable %s manually with `sly-enable-contrib'\
if/when you fix the error" (cl-third n))))
        ;; Update the connection-local list of all *MODULES*
        ;;
        (setf (sly-lisp-modules) all-modules)))))

(cl-defstruct (sly-contrib
               (:conc-name sly-contrib--))
  enabled-p
  name
  sly-dependencies
  slynk-dependencies
  enable
  disable
  authors
  license)

(defmacro define-sly-contrib (name _docstring &rest clauses)
  (declare (indent 1))
  (cl-destructuring-bind (&key sly-dependencies
                               slynk-dependencies
                               on-load
                               on-unload
                               authors
                               license)
      (cl-loop for (key . value) in clauses append `(,key ,value))
    (cl-labels
        ((enable-fn (c) (intern (concat (symbol-name c) "-init")))
         (disable-fn (c) (intern (concat (symbol-name c) "-unload")))
         (path-sym (c) (intern (concat (symbol-name c) "--path")))
         (contrib-sym (c) (intern (concat (symbol-name c) "--contrib"))))
      `(progn
         (defvar ,(path-sym name))
         (defvar ,(contrib-sym name))
         (setq ,(path-sym name) (and load-file-name
                                     (file-name-directory load-file-name)))
         (eval-when-compile
           (when byte-compile-current-file; protect against eager macro expansion
             (add-to-list 'load-path
                          (file-name-as-directory
                           (file-name-directory byte-compile-current-file)))))
         (setq ,(contrib-sym name)
               (put 'sly-contribs ',name
                    (make-sly-contrib
                     :name ',name :authors ',authors :license ',license
                     :sly-dependencies ',sly-dependencies
                     :slynk-dependencies ',slynk-dependencies
                     :enable ',(enable-fn name) :disable ',(disable-fn name))))
         ,@(mapcar (lambda (d) `(require ',d)) sly-dependencies)
         (defun ,(enable-fn name) ()
           (mapc #'funcall (mapcar
                            #'sly-contrib--enable
                            (cl-remove-if #'sly-contrib--enabled-p
                                          (list ,@(mapcar #'contrib-sym
                                                          sly-dependencies)))))
           (cl-loop for dep in ',slynk-dependencies
                    do (cl-pushnew (list dep ,(path-sym name) ',name)
                                   sly-contrib--required-slynk-modules
                                   :key #'cl-first))
           ;; FIXME: It's very tricky to do Slynk calls like
           ;; `sly-contrib--load-slynk-dependencies' here, and it this
           ;; should probably loop all connections. Anyway, we try
           ;; ensure this can only happen from an interactive
           ;; `sly-setup' call.
           ;;
           (when (and (eq this-command 'sly-setup)
                      (sly-connected-p))
             (sly-contrib--load-slynk-dependencies))
           ,@on-load
           (setf (sly-contrib--enabled-p ,(contrib-sym name)) t))
         (defun ,(disable-fn name) ()
           ,@on-unload
           (cl-loop for dep in ',slynk-dependencies
                    do (setq sly-contrib--required-slynk-modules
                             (cl-remove dep sly-contrib--required-slynk-modules
                                        :key #'cl-first)))
           (sly-warning "Disabling contrib %s" ',name)
           (mapc #'funcall (mapcar
                            #'sly-contrib--disable
                            (cl-remove-if-not #'sly-contrib--enabled-p
                                              (list ,@(mapcar #'contrib-sym
                                                              sly-dependencies)))))
           (setf (sly-contrib--enabled-p ,(contrib-sym name)) nil))))))

(defun sly-contrib--all-contribs ()
  "All defined `sly-contrib' objects."
  (cl-loop for (nil val) on (symbol-plist 'sly-contribs) by #'cddr
           when (sly-contrib-p val)
           collect val))

(defun sly-contrib--all-dependencies (contrib)
  "Contrib names recursively needed by CONTRIB, including self."
  (sly--contrib-safe contrib
    (cons contrib
          (cl-mapcan #'sly-contrib--all-dependencies
                     (sly-contrib--sly-dependencies
                      (sly-contrib--find-contrib contrib))))))

(defun sly-contrib--find-contrib (designator)
  (if (sly-contrib-p designator)
      designator
    (or (get 'sly-contribs designator)
        (error "Unknown contrib: %S" designator))))

(defun sly-contrib--read-contrib-name ()
  (let ((names (cl-loop for c in (sly-contrib--all-contribs) collect
                        (symbol-name (sly-contrib--name c)))))
    (intern (completing-read "Contrib: " names nil t))))

(defun sly-enable-contrib (name)
  "Attempt to enable contrib NAME."
  (interactive (list (sly-contrib--read-contrib-name)))
  (sly--contrib-safe name
    (funcall (sly-contrib--enable (sly-contrib--find-contrib name)))))

(defun sly-disable-contrib (name)
  "Attempt to disable contrib NAME."
  (interactive (list (sly-contrib--read-contrib-name)))
  (sly--contrib-safe name
    (funcall (sly-contrib--disable (sly-contrib--find-contrib name)))))


;;;;; Pull-down menu
(easy-menu-define sly-menu sly-mode-map "SLY"
  (let ((C '(sly-connected-p)))
    `("SLY"
      [ "Edit Definition..."       sly-edit-definition ,C ]
      [ "Return From Definition"   sly-pop-find-definition-stack ,C ]
      [ "Complete Symbol"          sly-complete-symbol ,C ]
      "--"
      ("Evaluation"
       [ "Eval Defun"              sly-eval-defun ,C ]
       [ "Eval Last Expression"    sly-eval-last-expression ,C ]
       [ "Eval And Pretty-Print"   sly-pprint-eval-last-expression ,C ]
       [ "Eval Region"             sly-eval-region ,C ]
       [ "Eval Region And Pretty-Print" sly-pprint-eval-region ,C ]
       [ "Interactive Eval..."     sly-interactive-eval ,C ]
       [ "Edit Lisp Value..."      sly-edit-value ,C ]
       [ "Call Defun"              sly-call-defun ,C ])
      ("Debugging"
       [ "Inspect..."              sly-inspect ,C ]
       [ "Macroexpand Once..."     sly-macroexpand-1 ,C ]
       [ "Macroexpand All..."      sly-macroexpand-all ,C ]
       [ "Disassemble..."          sly-disassemble-symbol ,C ])
      ("Compilation"
       [ "Compile Defun"           sly-compile-defun ,C ]
       [ "Compile and Load File"       sly-compile-and-load-file ,C ]
       [ "Compile File"            sly-compile-file ,C ]
       [ "Compile Region"          sly-compile-region ,C ]
       "--"
       [ "Next Note"               sly-next-note t ]
       [ "Previous Note"           sly-previous-note t ]
       [ "Remove Notes"            sly-remove-notes t ]
       [ "List notes"              sly-show-compilation-log t ])
      ("Cross Reference"
       [ "Who Calls..."            sly-who-calls ,C ]
       [ "Who References... "      sly-who-references ,C ]
       [ "Who Sets..."             sly-who-sets ,C ]
       [ "Who Binds..."            sly-who-binds ,C ]
       [ "Who Macroexpands..."     sly-who-macroexpands ,C ]
       [ "Who Specializes..."      sly-who-specializes ,C ]
       [ "List Callers..."         sly-list-callers ,C ]
       [ "List Callees..."         sly-list-callees ,C ]
       [ "Next Location"           sly-next-location t ])
      ("Editing"
       [ "Check Parens"            check-parens t]
       [ "Update Indentation"      sly-update-indentation ,C])
      ("Documentation"
       [ "Describe Symbol..."      sly-describe-symbol ,C ]
       [ "Lookup Documentation..." sly-documentation-lookup t ]
       [ "Apropos..."              sly-apropos ,C ]
       [ "Apropos all..."          sly-apropos-all ,C ]
       [ "Apropos Package..."      sly-apropos-package ,C ]
       [ "Hyperspec..."            sly-hyperspec-lookup t ])
      "--"
      [ "Interrupt Command"        sly-interrupt ,C ]
      [ "Abort Async. Command"     sly-quit ,C ])))

(easy-menu-define sly-sly-db-menu sly-db-mode-map "SLY-DB Menu"
  (let ((C '(sly-connected-p)))
    `("SLY-DB"
      [ "Next Frame" sly-db-down t ]
      [ "Previous Frame" sly-db-up t ]
      [ "Toggle Frame Details" sly-db-toggle-details t ]
      [ "Next Frame (Details)" sly-db-details-down t ]
      [ "Previous Frame (Details)" sly-db-details-up t ]
      "--"
      [ "Eval Expression..." sly-interactive-eval ,C ]
      [ "Eval in Frame..." sly-db-eval-in-frame ,C ]
      [ "Eval in Frame (pretty print)..." sly-db-pprint-eval-in-frame ,C ]
      [ "Inspect In Frame..." sly-db-inspect-in-frame ,C ]
      [ "Inspect Condition Object" sly-db-inspect-condition ,C ]
      "--"
      [ "Restart Frame" sly-db-restart-frame ,C ]
      [ "Return from Frame..." sly-db-return-from-frame ,C ]
      ("Invoke Restart"
       [ "Continue" sly-db-continue ,C ]
       [ "Abort"    sly-db-abort ,C ]
       [ "Step"      sly-db-step ,C ]
       [ "Step next" sly-db-next ,C ]
       [ "Step out"  sly-db-out ,C ]
       )
      "--"
      [ "Quit (throw)" sly-db-quit ,C ]
      [ "Break With Default Debugger" sly-db-break-with-default-debugger ,C ])))

(easy-menu-define sly-inspector-menu sly-inspector-mode-map
  "Menu for the SLY Inspector"
  (let ((C '(sly-connected-p)))
    `("SLY-Inspector"
      [ "Pop Inspectee" sly-inspector-pop ,C ]
      [ "Next Inspectee" sly-inspector-next ,C ]
      [ "Describe this Inspectee" sly-inspector-describe ,C ]
      [ "Eval in context" sly-inspector-eval ,C ]
      [ "Show history" sly-inspector-history ,C ]
      [ "Reinspect" sly-inspector-reinspect ,C ]
      [ "Fetch all parts" sly-inspector-fetch-all ,C ]
      [ "Quit" sly-inspector-quit ,C ])))


;;;; Utilities (no not Paul Graham style)

;;; FIXME: this looks almost sly `sly-alistify', perhaps the two
;;;        functions can be merged.
(defun sly-group-similar (similar-p list)
  "Return the list of lists of 'similar' adjacent elements of LIST.
The function SIMILAR-P is used to test for similarity.
The order of the input list is preserved."
  (if (null list)
      nil
    (let ((accumulator (list (list (car list)))))
      (dolist (x (cdr list))
        (if (funcall similar-p x (caar accumulator))
            (push x (car accumulator))
          (push (list x) accumulator)))
      (nreverse (mapcar #'nreverse accumulator)))))

(defun sly-alistify (list key test)
  "Partition the elements of LIST into an alist.
KEY extracts the key from an element and TEST is used to compare
keys."
  (let ((alist '()))
    (dolist (e list)
      (let* ((k (funcall key e))
             (probe (cl-assoc k alist :test test)))
        (if probe
            (push e (cdr probe))
          (push (cons k (list e)) alist))))
    ;; Put them back in order.
    (nreverse (mapc (lambda (ent)
                      (setcdr ent (nreverse (cdr ent))))
                    alist))))

;;;;; Misc.

(defun sly-length= (list n)
  "Return (= (length LIST) N)."
  (if (zerop n)
      (null list)
    (let ((tail (nthcdr (1- n) list)))
      (and tail (null (cdr tail))))))

(defun sly-length> (seq n)
  "Return (> (length SEQ) N)."
  (cl-etypecase seq
    (list (nthcdr n seq))
    (sequence (> (length seq) n))))

(defun sly-trim-whitespace (str)
  "Chomp leading and tailing whitespace from STR."
  ;; lited from http://www.emacswiki.org/emacs/ElispCookbook
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

;;;;; Buffer related

(defun sly-column-max ()
  (save-excursion
    (goto-char (point-min))
    (cl-loop for column = (prog2 (end-of-line) (current-column) (forward-line))
             until (= (point) (point-max))
             maximizing column)))

;;;;; CL symbols vs. Elisp symbols.

(defun sly-cl-symbol-name (symbol)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match ":\\([^:]*\\)$" n)
        (let ((symbol-part (match-string 1 n)))
          (if (string-match "^|\\(.*\\)|$" symbol-part)
              (match-string 1 symbol-part)
            symbol-part))
      n)))

(defun sly-cl-symbol-package (symbol &optional default)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match "^\\([^:]*\\):" n)
        (match-string 1 n)
      default)))

(defun sly-qualify-cl-symbol-name (symbol-or-name)
  "Return a package-qualified string for SYMBOL-OR-NAME.
If SYMBOL-OR-NAME doesn't already have a package prefix the
current package is used."
  (let ((s (if (stringp symbol-or-name)
               symbol-or-name
             (symbol-name symbol-or-name))))
    (if (sly-cl-symbol-package s)
        s
      (format "%s::%s"
              (let* ((package (sly-current-package)))
                ;; package is a string like ":cl-user"
                ;; or "CL-USER", or "\"CL-USER\"".
                (if package
                    (sly--pretty-package-name package)
                  "CL-USER"))
              (sly-cl-symbol-name s)))))

;;;;; Moving, CL idiosyncracies aware (reader conditionals &c.)

(defmacro sly-point-moves-p (&rest body)
  "Execute BODY and return true if the current buffer's point moved."
  (declare (indent 0))
  (let ((pointvar (cl-gensym "point-")))
    `(let ((,pointvar (point)))
       (save-current-buffer ,@body)
       (/= ,pointvar (point)))))

(defun sly-forward-sexp (&optional count)
  "Like `forward-sexp', but understands reader-conditionals (#- and #+),
and skips comments."
  (dotimes (_i (or count 1))
    (sly-forward-cruft)
    (forward-sexp)))

(defconst sly-reader-conditionals-regexp
  ;; #!+, #!- are SBCL specific reader-conditional syntax.
  ;; We need this for the source files of SBCL itself.
  (regexp-opt '("#+" "#-" "#!+" "#!-")))

(defsubst sly-forward-reader-conditional ()
  "Move past any reader conditional (#+ or #-) at point."
  (when (looking-at sly-reader-conditionals-regexp)
    (goto-char (match-end 0))
    (let* ((plus-conditional-p (eq (char-before) ?+))
           (result (sly-eval-feature-expression
                    (condition-case e
                        (read (current-buffer))
                      (invalid-read-syntax
                       (signal 'sly-unknown-feature-expression (cdr e)))))))
      (unless (if plus-conditional-p result (not result))
        ;; skip this sexp
        (sly-forward-sexp)))))

(defun sly-forward-cruft ()
  "Move forward over whitespace, comments, reader conditionals."
  (while (sly-point-moves-p (skip-chars-forward " \t\n")
                            (forward-comment (buffer-size))
                            (sly-forward-reader-conditional))))

(defun sly-keywordify (symbol)
  "Make a keyword out of the symbol SYMBOL."
  (let ((name (downcase (symbol-name symbol))))
    (intern (if (eq ?: (aref name 0))
                name
              (concat ":" name)))))

(put 'sly-incorrect-feature-expression
     'error-conditions '(sly-incorrect-feature-expression error))

(put 'sly-unknown-feature-expression
     'error-conditions '(sly-unknown-feature-expression
                         sly-incorrect-feature-expression
                         error))

;; FIXME: let it crash
;; FIXME: the (null (cdr l)) constraint is bogus
(defun sly-eval-feature-expression (e)
  "Interpret a reader conditional expression."
  (cond ((symbolp e)
         (memq (sly-keywordify e) (sly-lisp-features)))
        ((and (consp e) (symbolp (car e)))
         (funcall (let ((head (sly-keywordify (car e))))
                    (cl-case head
                      (:and #'cl-every)
                      (:or #'cl-some)
                      (:not
                       (let ((feature-expression e))
                         (lambda (f l)
                           (cond ((null l) t)
                                 ((null (cdr l)) (not (apply f l)))
                                 (t (signal 'sly-incorrect-feature-expression
                                            feature-expression))))))
                      (t (signal 'sly-unknown-feature-expression head))))
                  #'sly-eval-feature-expression
                  (cdr e)))
        (t (signal 'sly-incorrect-feature-expression e))))

;;;;; Extracting Lisp forms from the buffer or user

(defun sly-region-for-defun-at-point (&optional pos)
  "Return a list (START END) for the positions of defun at POS.
POS defaults to point"
  (save-excursion
    (save-match-data
      (goto-char (or pos (point)))
      (end-of-defun)
      (let ((end (point)))
        (beginning-of-defun)
        (list (point) end)))))

(defun sly-beginning-of-symbol ()
  "Move to the beginning of the CL-style symbol at point."
  (while (re-search-backward "\\(\\sw\\|\\s_\\|\\s\\.\\|\\s\\\\|[#@|]\\)\\="
                             (when (> (point) 2000) (- (point) 2000))
                             t))
  (re-search-forward "\\=#[-+.<|]" nil t)
  (when (and (eq (char-after) ?@) (eq (char-before) ?\,))
    (forward-char)))

(defsubst sly-end-of-symbol ()
  "Move to the end of the CL-style symbol at point."
  (re-search-forward "\\=\\(\\sw\\|\\s_\\|\\s\\.\\|#:\\|[@|]\\)*"))

(put 'sly-symbol 'end-op 'sly-end-of-symbol)
(put 'sly-symbol 'beginning-op 'sly-beginning-of-symbol)

(defun sly-symbol-start-pos ()
  "Return the starting position of the symbol under point.
The result is unspecified if there isn't a symbol under the point."
  (save-excursion (sly-beginning-of-symbol) (point)))

(defun sly-symbol-end-pos ()
  (save-excursion (sly-end-of-symbol) (point)))

(defun sly-bounds-of-symbol-at-point ()
  "Return the bounds of the symbol around point.
The returned bounds are either nil or non-empty."
  (let ((bounds (bounds-of-thing-at-point 'sly-symbol)))
    (if (and bounds
             (< (car bounds)
                (cdr bounds)))
        bounds)))

(defun sly-symbol-at-point (&optional interactive)
  "Return the name of the symbol at point, otherwise nil."
  ;; (thing-at-point 'symbol) returns "" in empty buffers
  (let ((bounds (sly-bounds-of-symbol-at-point)))
    (when bounds
      (let ((beg (car bounds)) (end (cdr bounds)))
        (when interactive (sly-flash-region beg end))
        (buffer-substring-no-properties beg end)))))

(defun sly-bounds-of-sexp-at-point (&optional interactive)
  "Return the bounds sexp near point as a pair (or nil).
With non-nil INTERACTIVE, error if can't find such a thing."
  (or (sly-bounds-of-symbol-at-point)
      (and (equal (char-after) ?\()
           (member (char-before) '(?\' ?\, ?\@))
           ;; hide stuff before ( to avoid quirks with '( etc.
           (save-restriction
             (narrow-to-region (point) (point-max))
             (bounds-of-thing-at-point 'sexp)))
      (bounds-of-thing-at-point 'sexp)
      (and (save-excursion
             (and (ignore-errors
                    (backward-sexp 1)
                    t)
                  (bounds-of-thing-at-point 'sexp))))
      (when interactive
        (user-error "No sexp near point"))))

(cl-defun sly-sexp-at-point (&optional interactive stringp (errorp t))
  "Return the sexp at point as a string, otherwise nil.
With non-nil INTERACTIVE, flash the region and also error if no
sexp can be found, unless ERRORP, which defaults to t, is passed
as nil.  With non-nil STRINGP, only look for strings"
  (catch 'return
    (let ((bounds (sly-bounds-of-sexp-at-point (and interactive
                                                    errorp))))
      (when bounds
        (when (and stringp
                   (not (eq (syntax-class (syntax-after (car bounds)))
                            (char-syntax ?\"))))
          (if (and interactive
                   interactive)
              (user-error "No string at point")
            (throw 'return nil)))
        (when interactive
          (sly-flash-region (car bounds) (cdr bounds)))
        (buffer-substring-no-properties (car bounds)
                                        (cdr bounds))))))

(defun sly-string-at-point (&optional interactive)
  "Returns the string near point as a string, otherwise nil.
With non-nil INTERACTIVE, flash the region and error if no string
can be found."
  (sly-sexp-at-point interactive 'stringp))

(defun sly-input-complete-p (start end)
  "Return t if the region from START to END contains a complete sexp."
  (save-excursion
    (goto-char start)
    (cond ((looking-at "\\s *['`#]?[(\"]")
           (ignore-errors
             (save-restriction
               (narrow-to-region start end)
               ;; Keep stepping over blanks and sexps until the end of
               ;; buffer is reached or an error occurs. Tolerate extra
               ;; close parens.
               (cl-loop do (skip-chars-forward " \t\r\n)")
                        until (eobp)
                        do (forward-sexp))
               t)))
          (t t))))


;;;; sly.el in pretty colors

(cl-loop for sym in (list 'sly-def-connection-var
                          'sly-define-channel-type
                          'sly-define-channel-method
                          'define-sly-contrib)
         for regexp = (format "(\\(%S\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"
                              sym)
         do (font-lock-add-keywords
             'emacs-lisp-mode
             `((,regexp (1 font-lock-keyword-face)
                        (2 font-lock-variable-name-face)))))

;;;; Finishing up

(defun sly--byte-compile (symbol)
  (require 'bytecomp) ;; tricky interaction between autoload and let.
  (let ((byte-compile-warnings '()))
    (byte-compile symbol)))

(defun sly-byte-compile-hotspots (syms)
  (mapc (lambda (sym)
          (cond ((fboundp sym)
                 (unless (or (byte-code-function-p (symbol-function sym))
                             (subrp (symbol-function sym)))
                   (sly--byte-compile sym)))
                (t (error "%S is not fbound" sym))))
        syms))

(sly-byte-compile-hotspots
 '(sly-alistify
   sly-log-event
   sly--events-buffer
   sly-process-available-input
   sly-dispatch-event
   sly-net-filter
   sly-net-have-input-p
   sly-net-decode-length
   sly-net-read
   sly-print-apropos
   sly-insert-propertized
   sly-beginning-of-symbol
   sly-end-of-symbol
   sly-eval-feature-expression
   sly-forward-sexp
   sly-forward-cruft
   sly-forward-reader-conditional))

;;;###autoload
(add-hook 'lisp-mode-hook 'sly-editing-mode)

(cond
 ((or (not (memq 'slime-lisp-mode-hook lisp-mode-hook))
      noninteractive
      (prog1
          (y-or-n-p "[sly] SLIME detected in `lisp-mode-hook', causes keybinding conflicts.  Remove it for this Emacs session?")
        (warn "To restore SLIME in this session, customize `lisp-mode-hook'
and replace `sly-editing-mode' with `slime-lisp-mode-hook'.")))
  (remove-hook 'lisp-mode-hook 'slime-lisp-mode-hook)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'lisp-mode)
        (unless sly-editing-mode (sly-editing-mode 1))
        (ignore-errors (and (featurep 'slime) (funcall 'slime-mode -1)))))))
 (t
  (warn
   "`sly.el' loaded OK. To use SLY, customize `lisp-mode-hook' and remove `slime-lisp-mode-hook'.")))

(provide 'sly)

;;; sly.el ends here
;; Local Variables:
;; coding: utf-8
;; End:
