;; $Id$

;;
;; Init functions for Bioperl minor mode
;;
;; Author: Mark A. Jensen
;; Email : maj -at- fortinbras -dot- us
;;
;; Part of The Documentation Project
;; http://www.bioperl.org/wiki/The_Documentation_Project
;;
;;

;; Copyright (C) 2009 Mark A. Jensen

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA


(defconst bioperl-init-revision "$Id$"
  "The revision string of bioperl-init.el")


;;
;; menu bar keymaps
;;

(defvar menu-bar-bioperl-doc-menu
  (let (
	(map (make-sparse-keymap "BioPerl Docs"))
	)
    (define-key map [bp-pod-apx]
      '(menu-item "View appendix" bioperl-view-pod-appendix
		  :help "View pod APPENDIX of a module (where most methods are described)"
		  :keys "\\[bioperl-view-pod-appendix]"))
    (define-key map [bp-pod-dsc]
      '(menu-item "View description" bioperl-view-pod-description
		  :help "View pod DESCRIPTION of a module"
		  :keys "\\[bioperl-view-pod-description]"))
    (define-key map [bp-pod-syn]
      '(menu-item "View synopsis" bioperl-view-pod-synopsis
		  :help "View pod SYNOPSIS of a module"
		  :keys "\\[bioperl-view-pod-synopsis]"))
    (define-key map [bp-pod]
      '(menu-item "View pod" bioperl-view-pod
		  :help "Examine entire pod of a module"
		  :keys "\\[bioperl-view-pod]"))
    (define-key map [bp-pod-mth]
      '(menu-item "View method pod" bioperl-view-pod-method
		  :help "View pod (Title:, Usage:, etc) for a single method"
		  :keys "\\[bioperl-view-pod-method]"))
    map)
  "Menu-bar map for doc functions in bioperl-mode.")


(defvar menu-bar-bioperl-ins-menu
  (let (
	(map (make-sparse-keymap "BioPerl Ins"))
	)
    (define-key map [bp-ins-arr]
      '(menu-item "Insert container template" bioperl-insert-array-accessor
		  :help "Insert template functions for an object array"
		  :keys "\\[bioperl-insert-array-accessor]"))
    (define-key map [bp-ins-obj]
      '(menu-item "Insert class/object template" bioperl-insert-class
		  :help "Insert full object template plus std pod"
		  :keys "\\[bioperl-insert-class]"))
    (define-key map [bp-ins-mthpod]
      '(menu-item "Insert method pod template" bioperl-insert-method-pod
		  :help "Insert Bioperl standard method pod template"
		  :keys "\\[bioperl-insert-method-pod]"))
    (define-key map [bp-ins-acc]
      '(menu-item "Insert accessor template" bioperl-insert-accessor
		  :help "Insert accessor (getter/setter) with std pod"
		  :keys "\\[bioperl-insert-accessor]"))
    (define-key map [bp-ins-mod]
      '(menu-item "Insert module identifier" bioperl-insert-module
		  :help "Insert module identifier, with completion"
		  :keys "\\[bioperl-insert-module]"))
    map)
  "Menu-bar map for insertion functions in bioperl-mode")

;;
;; keymap
;;
;; principles:
;; C-c accesses mode functions
;; meta key commands - documentation reading (pod display, etc.)
;; control key command - documentation writing (template insertions, etc.)
;;


(defvar bioperl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\M-p" 'bioperl-view-pod)
    (define-key map "\C-c\M-d" 'bioperl-view-pod-description)
    (define-key map "\C-c\M-s" 'bioperl-view-pod-synopsis)
    (define-key map "\C-c\M-a" 'bioperl-view-pod-appendix)
    (define-key map "\C-c\M-f" 'bioperl-view-pod-method)
    (define-key map "\C-c\M-m" 'bioperl-view-pod-method)
    (define-key map "\C-cm"   'bioperl-insert-module)
    (define-key map "\C-c\C-a" 'bioperl-insert-accessor)
    (define-key map "\C-c\C-v" 'bioperl-insert-accessor)
    (define-key map "\C-c\C-A" 'bioperl-insert-array-accessor)
    (define-key map "\C-c\C-b" 'bioperl-insert-class)
    (define-key map "\C-c\C-M" 'bioperl-insert-class)
    (define-key map "\C-c\C-p" 'bioperl-insert-method-pod)
    (define-key map [menu-bar] nil)
    (define-key map [menu-bar bp-ins]
      (list 'menu-item "BP Templs" menu-bar-bioperl-ins-menu))
    (define-key map [menu-bar bp-doc]
      (list 'menu-item "BP Docs" menu-bar-bioperl-doc-menu))
    map)
  "Keymap for Bioperl minor mode")

;;
;; finders
;;


(defun bioperl-find-module-path (&optional symb val)
  "Find path to Bioperl modules and set `bioperl-module-path'.
This path points to the directory containing Bio; same principle
as 'use lib'. SYMB and VAL are dummies allowing `defcustom' to do
initializaton."
  (let (
	(old-exec-path exec-path)
	(pth) )
    ;; ask BPMODE_PATH first...
    (setq pth (getenv "BPMODE_PATH"))
    ;; try the environment
    (unless pth
      (let (
	    ( plib (concat (getenv "PERL5LIB") path-separator (getenv "PATH")))
	    ( pths )
	    )
	(if plib
	  (progn
	    (setq pths (split-string plib path-separator))
	    (while (and (not pth) pths)
	    ;; unixize
;;	    (setq pth (replace-regexp-in-string "\\\\" "/" pth))
	      (setq pth (pop pths))
	      (setq pth (if (file-exists-p (concat pth "/" "Bio")) pth nil)))
	    ))))
    ;; then ask Perl...
    (unless pth
      ;; safe path
      (if (or (not (boundp 'bioperl-mode-safe-flag))
	      bioperl-mode-safe-flag)
	  (setq exec-path bioperl-safe-PATH))

      (setq pth
	    (with-temp-buffer
	      (call-process
	       "perl" nil t t
	       "-MConfig" "-e" "print $Config{sitelib}")
	      (goto-char (point-min))
	      (thing-at-point 'line)
	      ))
      ;; reset exec-path
      (setq exec-path old-exec-path)
      ;; file name port issue - unixize
      (setq pth (replace-regexp-in-string "\\\\" "/" pth))
      (setq pth (if (file-exists-p (concat pth "/" "Bio")) pth nil)))
    ;; fall back to pwd
    (unless pth
      (setq pth (nth 1 (split-string (pwd))))
      ;; unixize
      (setq pth (replace-regexp-in-string "\\\\" "/" pth))
      (setq pth (if (file-exists-p (concat pth "/" "Bio")) pth nil))
      )
    (if pth
	(setq bioperl-module-path pth)
      (message "Can't find Bio modules; defaulting to pwd -- try setting bioperl-module-path manually")
      (setq bioperl-module-path "."))
  pth))

(defun bioperl-set-safe-PATH (&optional symb val)
  "Portably sets the safe-PATH, used when bioperl-mode calls the system.
SYMB and VAL are dummies allowing defcustom to do
initialization."
  (cond
   ( (string-match "windows\\|mingw\\|nt" system-configuration)
     (setq bioperl-safe-PATH '("c:/Perl/bin" "c:/Windows/system32")) )
   ( (string-match "unix\\|linux" system-configuration)
     (setq bioperl-safe-PATH '("/bin" "/usr/bin" "/usr/local/bin")) )
   ( (string-match "cygwin" system-configuration)
     (setq bioperl-safe-PATH '("/bin" "/usr/local/bin"
			       "/cygdrive/c/Windows/system32") ) )
   ( t
     (setq bioperl-safe-PATH '()))))

(defvar bioperl-enabled-buffer-flag nil
  "Buffer-local flag for enabling/disabling the bioperl-mode toolbar icon.")

(make-local-variable 'bioperl-enabled-buffer-flag)

;;
;; minor mode definition functions
;;

(define-minor-mode bioperl-mode
  "Toggles Bioperl minor mode.
Bioperl mode provides Bioperl-flavored template insertion and
convenient access to POD documentation. More documentation to
come."
  :init-value nil
  :lighter "[bio]"
;  :keymap bioperl-mode-map
  :group 'bioperl
  ;; version check
  (if (string-match "\\(2[0-9]\\)\.[0-9]+\\(?:\.[0-9]+\\)?" (emacs-version))
      (if (>= (string-to-number (match-string 1 (emacs-version))) 22)
	  t
	(error "Must upgrade to Emacs 22 to use bioperl-mode"))
    (error "Must upgrade to Emacs 22 to use bioperl-mode"))
  ;; set up mode
  (bioperl-skel-elements))

(define-minor-mode bioperl-view-mode
  "An easily-quittable View mode deriviation for bioperl-mode."
  :init-value nil
  :lighter "[bio]"
;  :keymap ( let ( (map (copy-keymap (cdr (assoc 'view-mode minor-mode-map-alist)))) )
;	    (if map
;		(progn
;		  (define-key map [menu-bar] nil)
;		  (define-key map [menu-bar bp-doc] (list 'menu-item "BP Docs" menu-bar-bioperl-doc-menu))
;		  (define-key map "q" 'View-kill-and-leave)
;		  (define-key map "f" 'bioperl-view-source)
;		  (define-key map "\C-m" 'bioperl-view-pod)
;		  (define-key map "\C-\M-m" 'bioperl-view-pod-method)))
;	    map )
  ;; and now, a total kludge.
    (view-mode))

(defface pod-section-face
  '( (t (:weight bold :foreground "maroon3") ) )
  "Highlight for pod section names.")
(defface pod-bioperl-identifier-face
  '( (t (:foreground "blue3" :weight bold)))
  "Highlight for bioperl identifiers")
(defface pod-method-pod-tag-face
  '( (t (:foreground "blue4")) )
  "Highlight for method pod tags (Title, Usage, etc.)")
(defface pod-blue-man-face
  '( (t (:background "blue" :foreground "dark blue")))
  "My world is blue.")
(defface pod-subsec-header-face
  '( (t (:weight bold :slant italic :foreground "blue4")))
  "Highlight pod subsection headers")
(defface pod-method-subsec-face
  '( (t (:slant italic :foreground "maroon4")))
  "Highlight for APPENDIX subsections")
(defface pod-method-name-face
  '( (t (:weight bold) ) )
  "Highlight pod method names")
(defface pod-key-value-arg-face
  '( (t (:slant italic :foreground "green3")) )
  "Highlight for key-value keys (-something)" )
(defface pod-deref-symb-face
  '( (t (:weight bold :foreground "blue4")))
  "Highlight '->' ")
(defface pod-assoc-symb-face
  '( (t (:weight bold :foreground "green3")))
  "Highlight '=>' ")


(defvar bioperl-pod-font-lock-keywords
  '(
    ;; rudimentary perl syntax highlighting
    ("[%$][{]?\\([a-zA-Z0-9_]+\\)[}]?" 1 'font-lock-variable-name-face)
    ("[^a-zA-Z0-9]@[{]?\\([a-zA-Z0-9_]+\\)[}]?" 1 'font-lock-variable-name-face)
    ("\\>->\\<" . 'pod-deref-symb-face)
    ("\\(?:\\s \\|\\>\\)\\(=>\\)\\(?:\\s \\|\\<\\|[\'\"]\\)" 1 'pod-assoc-symb-face)
    ("\\(?:\\W\\|\\s \\)\\(-[a-zA-Z0-9_]+\\)\\>" 1 'pod-key-value-arg-face)
;    ("'[^']+'" . 'font-lock-string-face)
    (pod-find-syntactic-string 1 'font-lock-string-face)
    ("\#\\s +.*"  0 'font-lock-comment-face t)
    ;; headers
    ("^\\(?:[A-Z]+\\s \\)+" . 'pod-section-face )
    ("^\\s \\{2\\}\\([A-Z][a-z]+\\s \\)+" . (0 'pod-subsec-header-face))
    ("^\\s \\{2\\}[a-z_][a-zA-Z0-9_()]+\\s " . 'pod-method-name-face)
    ("^\\s +[a-zA-Z]+\\s *:\\s " . 'pod-method-pod-tag-face)
    ("^[A-Z].*" . 'pod-method-subsec-face)
    ("Bio::\\(?:[a-zA-Z0-9_:]+\\)+" . 'pod-bioperl-identifier-face)
    ;; post-header syntax highlights
    ("\\(\\<[a-zA-Z0-9_]+\\>\\)()" 0 font-lock-function-name-face )
    ("\\(\\<[a-zA-Z0-9_]+\\>\\)[\(]" 1 font-lock-function-name-face )
    ("\\>->\\(\\<[a-zA-Z0-9_]+\\>\\)" 1 font-lock-function-name-face)

     )
  "Font lock keywords for highlighting Perl pod."
)

(defconst bioperl-pod-font-lock-defaults
  '(bioperl-pod-font-lock-keywords t nil nil ))

(define-derived-mode pod-mode fundamental-mode "Pod Fundamental"
  "Derived fundamental mode for highlighting BioPerl pod."
  :group 'bioperl
  :syntax-table nil
  :abbrev-table nil
  (set (make-local-variable 'font-lock-defaults)
       bioperl-pod-font-lock-defaults))

(defun pod-find-syntactic-string (bound)
  "String searcher for bioperl-mode font-lock."
  ;; try to infer from symbol context
  (re-search-forward "\\(?:[$@%(),]\\|->\\|=>\\|print\\).*?\\(['][^']+[']\\|[\"][^\"]+[\"]\\)" bound t))

(defun bioperl-pod-synopsis-region (buffer)
  "Return beginning & end of SYNOPSIS region (excluding the header)."
  (unless (bufferp buffer)
    (error "Buffer required at arg BUFFER"))
  (save-excursion
    (goto-char (point-min))
    (let ( (beg) (end) )
      (setq beg
	    (if (re-search-forward "^SYNOPSIS" (point-max) t)
		(progn (forward-line 1) (if (bolp) (point) nil))
	      nil))
      (setq end
	    (if (re-search-forward "^[A-Z]" (point-max) t)
		(progn (beginning-of-line) (if (bolp) (point) nil))
	    nil))
      (if (not (and beg end))
	  nil
	`(,beg ,end)))))


(defun bioperl-perl-mode-infect ()
  "Add this function to `perl-mode-hook' to associate bioperl-mode with perl-mode."
  (unless (or (not (display-graphic-p)) (key-binding [tool-bar bpmode]) )
    (define-key (current-local-map) [tool-bar bpmode]
      `(menu-item "bpmode" bioperl-mode
		  :image [,(find-image (list
					'(:type xpm :file "bpmode-tool.xpm")))
			  ,(find-image (list
					'(:type xpm :file "bpmode-tool.xpm")))
			  ,(find-image (list
					'(:type xpm :file "bpmode-tool-dis.xpm")))
			  ,(find-image (list
					'(:type xpm :file "bpmode-tool-dis.xpm")))]
		  :enable bioperl-enabled-buffer-flag
		  )))
  (setq bioperl-enabled-buffer-flag t)
  (if bioperl-mode-active-on-perl-mode-flag (bioperl-mode) nil))

;; where are you, subr.el?

(unless (boundp 'booleanp)
  (defun booleanp (x)
    "Is it boolean? Let's find out."
    (if (or (equal x t) (equal x nil))
	t
      nil)))

(provide 'bioperl-init)

;;; end bioperl-init.el
