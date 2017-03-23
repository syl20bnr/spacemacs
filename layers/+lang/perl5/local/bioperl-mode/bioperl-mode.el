;; $Id$

;; use multiple paths in bioperl-module-path

;;
;; Bioperl minor (haha!) mode
;;
;; Author: Mark A. Jensen
;; Email : maj -at- fortinbras -dot- us
;;
;; Part of The Documentation Project
;; http://www.bioperl.org/wiki/The_Documentation_Project

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

;;
;;
;; TODOs
;;
;;  - compile to byte code
;;
;; issues
;; - missing tool in tool-bar??
;; - xemacs support?
;;
;; Installation
;;
;;  The files bioperl-mode.el, bioperl-skel.el, and bioperl-init.el
;;  should be placed in $EMACS_ROOT/site-lisp, and the .xpm image
;;  files in $EMACS_ROOT/etc/images, then add
;;  (require 'bioperl-mode)
;;  to your .emacs file.
;;
;;  See http://www.bioperl.org/wiki/Emacs_bioperl-mode
;;  for more information.
;;
;; Design Notes
;;
;; POD content is obtained exclusively by accessing the user's installed
;; Bioperl modules -- so it's as up-to-date as the user's installation.
;;
;; Pod is parsed with a homebrew parser found in pod.el.
;;
;; Much of the parsing in this package depends on the standard form of
;; Bioperl pod; particularly on the typical division into NAME,
;; SYNOPSIS, DESCRIPTION, and APPENDIX sections, and on the fact that
;; pod for individual methods is found in the APPENDIX. There is some
;; dependence on the usual head levels for the headers, but this can
;; be hacked out if necessary.
;;
;; Some attempts at efficiency were made. Parsing pod for methods
;; and associated data can take a while, so parse results are cached
;; for the last module so parsed, and the cache is checked when
;; method information is requested before parsing again.
;;
;; The Bio/ path is parsed to provide a namespace completion facility
;; The relevant path names and structure is stored in an alist tree
;; called bioperl-module-names-cache. The cache is loaded lazily,
;; so that the directory structure is accessed on a desire-to-know
;; basis.
;;
;; Lazy loading of the name cache necessitated "programmed completion"
;; of namespace names in prompts. See Programmed Completion in the
;; info (elisp) node, and the function
;; bioperl-namespace-completion-function.
;;
;; Skeletons (implemented in the emacs standard package skeleton.el)
;; have been used for template insertions. These are very powerful, if
;; cumbersome, offer plug-in interactor functions, and I think allow
;; more modularity and scope for new additions than (insert)ing text
;; 'by hand'. Skeletons and (define-skeleton) declarations are
;; distributed in a separate file 'bioperl-skel.el', which is loaded
;; when the mode is initialized.
;;

(require 'skeleton)
(require 'bioperl-skel)
(require 'bioperl-init)
(require 'pod)

(defconst bioperl-mode-revision "$Id$"
  "The revision string of bioperl-mode.el")

;;
;; User customizations
;;

(defgroup bioperl nil
  "BioPerl templates and documentation access")

(defcustom bioperl-mode-active-on-perl-mode-flag t
  "If set, perl-mode will begin with bioperl-mode active.
Boolean."
  :type 'boolean
  :group 'bioperl)

(defcustom bioperl-mode-safe-flag t
  "If set, bioperl-mode with substitute `exec-path' with `bioperl-safe-PATH'.
Nil means use your current `exec-path'."
  :type 'boolean
  :group 'bioperl)

(defcustom bioperl-safe-PATH '()
  "Safe exec-path for bioperl-mode."
  :type 'sexp
  :initialize 'bioperl-set-safe-PATH
  :group 'bioperl)

(defcustom bioperl-module-path nil
  "Local path to Bioperl modules.
On init, set is attempted by `bioperl-find-module-path' Can indicate multiple search paths; define as PATH in your OS. The environment variable BPMODE_PATH will override everything."
;; better type 'choice; do later
  :type 'string
  :initialize 'bioperl-find-module-path
  :group 'bioperl)

;;
;; Hooks
;;

;;
;; bioperl- namespace variables
;;

(defvar bioperl-method-pod-cache nil
  "Stores the alist returned by the last successful `bioperl-slurp-methods-from-pod' call.
The module filepath represented by the cached info is contained
in `bioperl-cached-module'.")

(defvar bioperl-cached-module nil
  "Contains the filepath whose method pod information is
  currently stored in `bioperl-method-pod-cache'.")

(defvar bioperl-module-names-cache '( ("Bio" . nil) )
  "Storage for an alist tree structure of available module names.
Structure follows the Bio library tree:
 ( (\"Bio\" \"Seq\" ( \"SeqIO\" \"fasta\" \"msf\" ...) \"PrimarySeqI\" ...  ) )
Use `bioperl-add-module-names-to-cache' to, well, do it.")

(defvar bioperl-source-file nil
  "Contains the source file of pod being viewed. Buffer-local.")

(make-variable-buffer-local 'bioperl-source-file)
;;
;; User-interface functions
;;

(defun bioperl-insert-module (namespace module &optional dummy beg pt end)
  "Insert BioPerl module declaration interactively, using completion."
  (interactive
   (let* (
	 (mod-at-pt (bioperl-module-at-point))
	 (beg (if mod-at-pt (match-beginning 0) nil))
	 (pt (point))
	 (end (if mod-at-pt (match-end 0) nil))
	 (cr (bioperl-completing-read mod-at-pt nil nil "[insert] " t) )
	 )
     (if (not (member nil (mapcar 'not cr))) (signal 'quit t))
     (append cr (list beg pt end))
     ))
  (if namespace
      (progn
	(setq namespace (replace-regexp-in-string "::$" "" namespace))
	(let (
	      ( mod (apply 'concat
			   namespace
			   (if module (list "::" module) '(nil))) )
	      )
	  (if (not beg)
	      (insert mod)
	    (string-match (concat
			   (buffer-substring beg pt)
			   "\\(.*\\)")
			  mod)
	    (delete-region pt end)
	    (insert (substring mod (match-beginning 1) (match-end 1))))))
    nil))

;;
;; pod viewers
;;

;; TODO: refactor bioperl-view-pod to take separate nmspc mod parms...
(defun bioperl-view-pod (module &optional n)
  "View the full pod for a BioPerl module. Use completion facilities to browse interactively.
MODULE is in double-colon format. N is an index associated with a
component of `bioperl-module-path'."
  (interactive
   (let (
	 (mod (bioperl-completing-read (bioperl-module-at-point) nil t "[pod] "))
	 )
     (if (not (member nil (mapcar 'not mod))) (signal 'quit t))
     (list (apply 'concat
		  (elt mod 0)
		  (if (elt mod 1) (list "::" (elt mod 1))
		    (signal 'quit t))) (car (last mod)))))
  (bioperl-view-full-pod module n))

(defun bioperl-view-pod-method (namespace module method &optional n)
  "View desired method pod interactively. Use completion facilities to browse interactively.
N is an index associated with a component of `bioperl-module-path'."
  (interactive
   (let (
	 (cr (bioperl-completing-read (bioperl-module-at-point) t nil "[pod mth] ") )
	 )
     (if (not (member nil (mapcar 'not cr))) (signal 'quit t))
     cr))
  (if (not method) (signal 'quit t))
  (let (
	( cache-pos (if method (assoc-string method bioperl-method-pod-cache t) nil) )
	)
    (if (not cache-pos)
	(message "No such method")
      (bioperl-render-method-pod-from-cons cache-pos))
    ))

(defun bioperl-view-pod-synopsis (module &optional n)
  "View the pod synopsis for a Bioperl module.
N is an index associated with a component of `bioperl-module-path'."
  (interactive
   (let (
	 (mod (bioperl-completing-read (bioperl-module-at-point) nil t "[pod syn] "))
	 )
     (if (not (member nil (mapcar 'not mod))) (signal 'quit t))
     (list (apply 'concat
		  (elt mod 0)
		  (if (elt mod 1) (list "::" (elt mod 1))
		    (signal 'quit t) )) (car (last mod)))))
  (bioperl-view-pod-section module "SYNOPSIS" n))

(defun bioperl-view-pod-description (module &optional n)
  "View the pod synopsis for a BioPerl module.
N is an index associated with a component of `bioperl-module-path'."
  (interactive
   (let (
	 (mod (bioperl-completing-read (bioperl-module-at-point) nil t "[pod dsc] " ))
	 )
     (if (not (member nil (mapcar 'not mod))) (signal 'quit t))
     (list (apply 'concat
		  (elt mod 0)
		  (if (elt mod 1) (list "::" (elt mod 1))
		    (signal 'quit t))) (car (last mod)))))
  (bioperl-view-pod-section module "DESCRIPTION" n))

(defun bioperl-view-pod-appendix (module &optional n)
  "View the pod appendix (containing individual method information) for a Bioperl module.
N is an index associated with a component of `bioperl-module-path'."
  (interactive
   (let (
	 (mod (bioperl-completing-read (bioperl-module-at-point) nil t "[pod apx] "))
	 )
     (if (not (member nil (mapcar 'not mod))) (signal 'quit t))
     (list (apply 'concat
		  (elt mod 0)
		  (if (elt mod 1) (list "::" (elt mod 1))
		    (signal 'quit t))) (car (last mod)))))
  (bioperl-view-pod-section module "APPENDIX" n))

(defun bioperl-view-source ()
  "Display the file in `bioperl-source-file' in view mode in a new buffer."
  (interactive)
  (if (not (file-exists-p bioperl-source-file))
      nil
    (let ( (fname bioperl-source-file) )
      (set-buffer (generate-new-buffer "*BioPerl Src*"))
      (insert-file fname)
      (perl-mode)
      (view-mode)
      (pop-to-buffer (current-buffer)))))

;; "uninstall..."

(defun bioperl-mode-unload-hook &optional local
  "Remove the perl-mode hook.
If LOCAL is set, remove hook from the buffer-local value of perl-mode-hook."
  (remove-hook 'perl-mode-hook 'bioperl-perl-mode-infect local))

;;
;; Internal functions
;;

;;
;; pod slurpers
;;

(defun bioperl-view-full-pod (module &optional n)
  "Open the Bioperl POD for the MODULE for viewing in another buffer.
MODULE is in double-colon format."
  (unless (and module (stringp module))
    (error "String required at arg MODULE"))
  (unless (or (not n) (numberp n))
    (error "Number required at arg N"))
  (unless n
    (setq n 0))
  (if (not module)
      nil
    (let (
	  (pod-buf (generate-new-buffer "*BioPerl POD*"))
	  (pmfile (bioperl-path-from-perl module nil n))
	  )
      (unless pmfile
	(error "Module specified by MODULE not found in installation"))
      (save-excursion
	(set-buffer pod-buf)
	(pod-mode)
	(setq header-line-format (concat "POD - BioPerl module " module " @ "
					 (file-name-squish
					  (elt (split-string bioperl-module-path path-separator) n)) ))
	(insert-file-contents pmfile)
	(pod-parse-buffer (current-buffer))
	(goto-char (point-min))
	(bioperl-view-mode)
	(set (make-local-variable 'bioperl-source-file) pmfile)
	(pop-to-buffer pod-buf))
      )
    ;;return val
    t ))

(defun bioperl-view-pod-section (module section &optional n)
  "Open the Bioperl POD for the module PMFILE for viewing in another buffer.
MODULE is in double-colon format. SECTION is a string; one of
SYNOPSIS, DESCRIPTION, or APPENDIX. N is the index of the desired
component of bioperl-module-path."
  (unless (stringp module)
    (error "String required at arg MODULE"))
  (unless (stringp section)
    (error "String required at arg SECTION"))
  (unless (member (upcase section) '("SYNOPSIS" "DESCRIPTION" "APPENDIX"))
    (error "SECTION not recognized or handled yet"))
  (unless (or (not n) (numberp n))
    (error "Number required at arg N"))
  (unless n
    (setq n 0))
  (let (
	(pod-buf (generate-new-buffer "*BioPerl POD*"))
	(ret nil)
	(pmfile (bioperl-path-from-perl module nil n))
	)
    (unless pmfile
      (error "Module specified by MODULE not found in installation"))
    (save-excursion
      (set-buffer pod-buf)
      (pod-mode)
      (setq header-line-format (concat section " - BioPerl module " module
				       " @ " (file-name-squish
					      (elt (split-string bioperl-module-path path-separator) n)) ))
      (insert-file-contents pmfile)
      (pod-parse-buffer (current-buffer) t)
      (goto-char (point-min))
      ;; clip to desired section
      (if (search-forward (concat "== " section) (point-max) t)
	  (progn
	    (beginning-of-line)
	    (delete-region (point-min) (point))
	    (forward-line 1)
	    (search-forward "====" (point-max) 1)
	    (beginning-of-line)
	    (delete-region (point) (point-max))
	    (goto-char (point-min))
	    (while (re-search-forward "^====\\s +\\([a-zA-Z0-9_:()]+\\)\\s +==+" (point-max) t)
	      (replace-match "\\1" nil nil))
	    (goto-char (point-min))
	    (while (re-search-forward "^==\\s +\\([a-zA-Z0-9_:()]+\\)\\s +==+" (point-max) t)
	      (replace-match "  \\1" nil nil))
	    (bioperl-view-mode)
	    (set (make-local-variable 'bioperl-source-file) pmfile)
	    (pop-to-buffer pod-buf)
	    (setq ret t))
	(kill-buffer pod-buf)
	)
    )
    ret ))


(defun bioperl-slurp-methods-from-pod (module &optional n)
  "Parse pod for individual methods for module MODULE.
MODULE is in double-colon format. N is an index corresponding
to a component of `bioperl-module-path'.

Returns an associative array of the following form:

 ( METHOD_NAME . ( (PODKEY . CONTENT) (PODKEY . CONTENT) ... )
   METHOD_NAME . ( (PODKEY . CONTENT) (PODKEY . CONTENT) ... )
   ... )

where all elements are strings. The alist is sorted by
METHOD_NAME. METHOD_NAME is the name of the method (without
trailing parens), PODKEY is 'Title', 'Usage', 'Function',
'Returns', 'Args' (these keys are read directly from pod and not
standardized), CONTENT is the text that follows the colon
separating the PODKEY heading from the information (including all
text up until the next 'PODKEY :' line. Newlines are converted to
';' in the content, and whitespace is squished to single
spaces/semicolons.

This function, when successful, also sets the cache vars
`bioperl-method-pod-cache' and `bioperl-cached-module'."
  (unless (stringp module)
    (error "String required at arg MODULE"))
  (let (
	(pmfile (bioperl-path-from-perl module nil n))
	)
    (unless pmfile
      (error (concat "Module specified by MODULE not found in installation at path component " (number-to-string (if n n 0)) ".\nCheck contents of `bioperl-module-path' and call `bioperl-clear-module-cache'.") ))
    (let (
	(method nil)
	(pod-key nil)
	(content nil)
	(bound nil)
	(data '())
	(data-elt '())
	(data-elt-cdr '())
	(old-exec-path exec-path)
	)
      (with-temp-buffer
	(insert-file-contents pmfile)
	(pod-parse-buffer (current-buffer) t)
	;; clip to desired section
	(goto-char (point-min))
	(if (search-forward "= APPENDIX" (point-max) t)
	    (progn
	      (beginning-of-line)
	      (delete-region (point-min) (point))
	      ;; looking down into appendix
	      ;;
	      (while (re-search-forward "^==\\s +\\([a-zA-Z0-9_]+\\)"
					(point-max) t)
		(setq method (match-string 1))
		(setq data-elt (cons method '()))
		;; now we have the current method...
		;; find the boundary of this method's pod
		(save-excursion
		  (setq bound (progn (re-search-forward "^="
							(point-max) 1)
				     (beginning-of-line)
				     (point))))
		;; now parse out the guts of this method's pod
		;; getting pod-keys and their content...
		(while (re-search-forward
			"^\\s +\\([A-Za-z]+\\)\\s *:\\s *\\(.*\\)$"
			bound t)
		  (setq pod-key (match-string 1))
		  (setq content (match-string 2))
		  (save-excursion
		    (setq content
			  (concat content
				  (buffer-substring
				   (point) (if (re-search-forward "^\\s +[A-Za-z]+\\s *:" bound 1)
					       (progn (beginning-of-line) (point))
					     (point)))))
		    )
		  ;; squeeze whitespace from content
		  (setq content (replace-regexp-in-string "\n+" "!!" content))
		  (setq content (replace-regexp-in-string ";$" "" content))
		  (setq content (replace-regexp-in-string "\\s +" " " content))
		;; here we have, for the current method,
		  ;; the current pod-key and its content...
		  (setq data-elt-cdr (cdr data-elt))
		  (setcdr data-elt (push (cons pod-key content) data-elt-cdr )))
		;; copy the data-elt into the data alist...
		(setq data-elt-cdr (cdr data-elt))
		(push (cons (car data-elt) data-elt-cdr) data))
	      ;; set cache vars
	      (setq bioperl-method-pod-cache
		    (sort data (lambda (a b) (string-lessp (car a) (car b)))))
	      (setq bioperl-cached-module module)
	      ;; return the data alist for this module...
	      bioperl-method-pod-cache )
	  ;; the APPENDIX was not found...return nil
	  nil ) ))))

;;
;; list getters
;;

(defun bioperl-method-names (module &optional as-alist n)

  "Returns a list of method names as given in the pod of MODULE.
MODULE is in double-colon format. If AS-ALIST is t, return an
alist with elts as (NAME . nil). N is an index associated with a
component of `bioperl-module-path'.

This function looks first to see if methods for MODULE are
already loaded in `bioperl-method-pod-cache'; if not, calls
`bioperl-slurp-methods-from-pod'."
  (unless (stringp module)
    (error "String required at arg MODULE"))
  (unless (bioperl-path-from-perl module nil n)
    (error "Module specified by MODULE not found in installation"))
  ;; check the cache; might get lucky...
  (let ( (ret) )
    (setq ret
	  (if (string-equal module bioperl-cached-module)
	      (progn
		(mapcar 'car bioperl-method-pod-cache)
		;; path handling...
		)
	    (mapcar 'car (bioperl-slurp-methods-from-pod module n))))
    ;; fix alist for path handling??
    (if as-alist
	(mapcar (lambda (x) (list x nil)) ret)
      ret)))


(defun bioperl-module-names (module-dir &optional retopt as-alist)
  "Returns a list of modules contained in the directory indicated by MODULE-DIR.
MODULE-DIR is in double-colon format.  Optional RETOPT: nil,
return module names only (default); t, return directory names
only; other, return all names as a flat list. Optional AS-ALIST:
if t, return an alist with elts (NAME . PATH_STRING) (when used in
completing functions). This function checks all paths specified
in `bioperl-module-path'.


 This function is responsible for the lazy loading of the module
names cache: it will look first in `bioperl-module-names-cache';
if the MODULE-DIR is not available,
`bioperl-add-module-names-to-cache' will be called."
  (let* (
	(module-components (split-string module-dir "::" t))
	(unlist (lambda (x) (if (listp x) (car x) x)) )
	(choose-dirs (lambda (x) (if (listp (cdr x)) x nil)) )
	(choose-mods  (lambda (x) (if (listp (cdr x)) nil x)) )
	(ret) (i)
	(pths (split-string bioperl-module-path path-separator))
	(alists) (alist)
	)
    ;; add to cache
    (setq i 0)
    (while (< i (length pths))
      (bioperl-add-module-names-to-cache module-dir i)
      (setq i (1+ i)))
    ;; search
    (setq alists (deep-assoc-all module-components bioperl-module-names-cache))
    ;; here pick the directory alist
    (setq alist (if (stringp (cdr (elt alists 0)))
		    (elt alists 1) (elt alists 0)))
    (if (and alist (cdr alist))
	(cond
	 ( (not (booleanp retopt))
	   (if (stringp (cdr alist))
	       (setq ret alist)
	     (setq ret (cdr alist))))
	 ((not retopt)
	   (if (stringp (cdr alist))
	       (setq ret alist)
	     (setq ret (delete nil (mapcar choose-mods (cdr alist))))
	     ))
	 ( retopt
	   (if (stringp (cdr alist))
	       (setq ret nil)
	     (setq ret (delete nil (mapcar choose-dirs (cdr alist))))
	     ))))
    (if (not ret)
	nil
      (if (not as-alist)
	  (if (stringp (cdr ret))
	      (car ret)
	    (mapcar 'car ret))
	ret))))


;;
;; directory slurpers
;;

(defun bioperl-add-module-names-to-cache (module-dir &optional n)
  "Add alists to `bioperl-module-names-cache'.
MODULE-DIR is in double colon format. Allows for lazy build of
the cache.  Returns t if we added anything, nil if not. N is the index
of the desired bioperl-module-path component.

Cache alist format:
 ( \"Bio\" .
   ( (MODULE_NAME PATH_INDEX_STRING) ...        ; .pm file base names
     (DIRNAME . nil) ...           ; dirname read but not yet followed
     (DIRNAME . ( ... ) ) ... )    ; dirname assoc with >=1 level structure
 )
"

  (unless (and module-dir (stringp module-dir))
    (error "String required at arg MODULE-DIR"))
  (unless (or (not n) (numberp n))
    (error "Number required at arg N"))
  (unless n
    (setq n 0))
  (if (and (> n 0) (> n (1- (length (split-string bioperl-module-path path-separator)))))
      (error "Path index out of bounds at arg N"))
  (let* (
	(pth (bioperl-path-from-perl module-dir 1 n))
	(module-components (split-string module-dir "::" t))
	(module-string)
	(modules)
	(alist)
	(cache (deep-assoc-all module-components bioperl-module-names-cache))
	(cache-pos)
	(keys)
	(this-key)
	(good-keys)
	(ret)
       )
    (if (not pth)
	;; no path returned for module-dir...
	nil
      (setq cache-pos
	    (cond
	     ((not cache)
	      nil)
	     ((stringp (cdr (car cache)))
	      (elt cache 1))
	     ( t
	       (elt cache 0))))
      (if cache-pos ;; something there
	  ;; easy - a stub
	  (if (null (cdr cache-pos))
	      (progn
		(setcdr cache-pos (bioperl-slurp-module-names module-dir n))
		(setq ret t))
	      ;; less hard - branch exists
	    (let* (
		   (mod-alist (bioperl-slurp-module-names module-dir n))
		   (mod-alist-keys (mapcar 'car mod-alist))
		   (cache-item) (key)
		   )
	      (while (setq key (pop mod-alist-keys))
		(setq alist (assoc-all key cache-pos))
		(setq cache-item (if (stringp (cdr (elt alist 0)))
				     (elt alist 0) (elt alist 1)))
		(if (null cache-item)
		    (if alist
			nil
		    ;; create a new list member(s)
		      (setcdr cache-pos (append (cdr cache-pos)
						(assoc-all key mod-alist))))
		  ;;
		  (if (member n (mapcar 'string-to-number (split-string (cdr cache-item) path-separator)))
		      ;; deja vu
		      (setq mod-alist-keys nil) ;; fall-through
		    (setcdr cache-item (concat (cdr (assoc-string key mod-alist t)) path-separator (cdr cache-item)))
		    (setq ret t))))
	      ))

	;; hard - branch dne
	(setq keys module-components)
	(while (
		let ( (da (deep-assoc-all
			   (append (reverse good-keys) (list (car keys)))
			   bioperl-module-names-cache) ) )
		 (setq da (if (stringp (elt da 0)) (elt da 1) (elt da 0)))
		 (car da) );; has a member whose cdr is a list
	  (setq good-keys (push (car keys) good-keys))
	  (setq keys (cdr keys)))
	(push (pop good-keys) keys)
	(setq good-keys (nreverse good-keys))
	;; keys contains the directories we need to add, in order
	;; address for doing additions: cache-pos
	(setq alist (deep-assoc-all good-keys bioperl-module-names-cache))
	(setq cache-pos
	      (if (stringp (cdr (elt alist 0))) (elt alist 1) (elt alist 0)))
	(setq module-string (pop good-keys))
	;; prep for bioperl-anastomose
	(while good-keys
	  (setq module-string (concat module-string "::" (pop good-keys))))
	;; module-string is suitable for passing to bioperl-slurp-module-names
	(setq ret (bioperl-anastomose keys module-string cache-pos n)))
      )
    ret ))

(defun bioperl-anastomose (keys module-string cache-pos n)
  "Extends `bioperl-module-names-cache' recursively. No user-serviceable parts inside.
Call first CACHE-POS set to node to be extended.
MODULE-STRING must indicate directory corresponding to CACHE-POS."
  (unless cache-pos
    (setq cache-pos bioperl-module-names-cache))
  (if (not keys)
      t ; success
    (let (
	  (this-key (pop keys))
	  (modules)
	  (cache-ins-pos)
	  (alist)
	  )
      (setq alist (assoc-all this-key cache-pos))
      (setq cache-ins-pos (if (stringp (cdr (elt alist 0))) (elt alist 1) (elt alist 0)))
      (setq module-string (if module-string
			      (concat module-string "::" this-key)
			    this-key))
      (setq modules (bioperl-slurp-module-names module-string n))
      (if (not modules)
	  nil ; fail
	(let ( (cache-item) (uniq-modules) )
	  (while (setq cache-item (pop modules))
	    (if (or (null (cdr cache-ins-pos)) (not (member cache-item (cdr cache-ins-pos))))
		(push cache-item uniq-modules)))
	  (setcdr cache-ins-pos (append (cdr cache-ins-pos) uniq-modules )))
	(bioperl-anastomose keys module-string (cdr cache-ins-pos) n)
	t))))


(defun bioperl-slurp-module-names (module-dir &optional n)
  "Return list of the  basenames for .pm files contained in MODULE-DIR.
MODULE-DIR is in double-colon format. N is the index of the desired
bioperl-module-path component.

Return is a list of the form

 ( (MODULE_NAME . PATH_INDEX_STRING) ...
   (DIR_NAME . nil) ... )
"
  (unless (and module-dir (stringp module-dir))
    (error "String required at arg MODULE-DIR"))
  (unless (or (not n) (numberp n))
    (error "Number required at arg N"))
  (unless n
    (setq n 0))
  (let (
	(module-path (split-string bioperl-module-path path-separator))
	(pth (bioperl-path-from-perl module-dir 1 n))
	(modules)
	(fnames)
	(choose-dirs (lambda (x) (if (listp (cdr x)) x nil)) )
	(nmspc-only t)
       )
    (if (and (> n 0) (> n (1- (length module-path))))
	(error "Path index out of bounds at arg N"))
    ;; following (elt ... 0) checks if pth is dir or symlink:
    ;;  possible bug...
    (if (and pth (elt (file-attributes pth) 0))
	(progn
	  (setq fnames (directory-files pth))
	  (while fnames
 	    (let ( (str (pop fnames)))
	      ;; files - conses with path-index cdr
	      (if (string-match "\\([a-zA-Z0-9_]+\\)\.pm$" str)
		  (progn
		    (push (cons (match-string 1 str)
				(number-to-string n))  modules)
		    (setq nmspc-only nil)))
	      ;; directories - conses with nil cdr
	      (if (string-match "^\\([a-zA-Z0-9_]+\\)$" str)
		  (if (not (string-equal (match-string 1 str) "README")) (push (cons (match-string 1 str) nil) modules)))
	      ))
;; 	  (if nmspc-only
;; 	      (let ( (dirs (delete nil (mapcar choose-dirs modules)))
;; 		     (module-dir-next) )
;; 		(while dirs
;; 		  (setq module-dir-next (concat module-dir "::" (car (pop dirs))))
;; 		  (append modules (bioperl-slurp-module-names module-dir-next n)))))
	  (if (not modules)
	      nil
	    modules))
      nil)))

;;
;; string converters and finders
;;

(defun bioperl-module-at-point ()
  "Look for something like a module identifier at point, and return it."
  (interactive)
  (let (
	(found (thing-at-point-looking-at "Bio::[a-zA-Z0-9_:]+"))
	(module nil)
	(pth nil)
	)
    (if (not found)
	nil
      (setq module (apply 'buffer-substring (match-data)))
      module)))

(defun bioperl-find-module-at-point (&optional n)
  "Look for something like a module declaration at point, and return a filepath corresponding to it.
N is the index of the desired bioperl-module-path component."
  (interactive)
  (unless (or (not n) (numberp n))
    (error "Number required at arg N"))
  (unless n
    (setq n 0))
  (unless bioperl-module-path
      (error "bioperl-module-path not yet set; you can set it with bioperl-find-module-path"))
  (let (
	(module-path (elt (split-string bioperl-module-path path-separator) n))
	(found) (module) (pth)
	)
    (if (and (> n 0) (> n (1- (length module-path))))
	(error "Path index out of bounds at arg N"))
    (unless (file-exists-p (concat module-path "/Bio"))
      (error (concat "Bio modules not present in path component" module-path )))
    (setq found (thing-at-point-looking-at "Bio::[a-zA-Z0-9_:]+"))
    (if (not found)
	nil
      (setq module (apply 'buffer-substring (match-data)))
      (setq pth (bioperl-path-from-perl module n)))
    pth))


(defun bioperl-path-from-perl (module &optional dir-first n)
  "Return a path to the module file represented by the perl string MODULE.
Returns nil if no path found. If DIR-FIRST is t, return a
directory over a .pm file if there is a choice. If DIR-FIRST is
not t or nil, return a directory only. N is an integer, indicating the
desired member of bioperl-module-path to search."
  (unless bioperl-module-path
    (error "bioperl-module-path not yet set; you can set it with bioperl-find-module-path"))
  (unless (stringp module)
    (error "string arg required at MODULE"))
  (unless (or (not n) (numberp n))
    (error "number arg required at N"))
  ; default
  (unless n
    (setq n 0))
  (let (
	(module-path (elt (split-string bioperl-module-path path-separator) n))
	(module-components (split-string module "::" t))
	(pth)
	(dir (if (not (boundp 'dir-first)) nil dir-first))
	)
    (if (and (> n 0) (> n (1- (length module-path))))
	(error "Path index out of bounds at arg N"))
    (unless (file-exists-p (concat module-path "/Bio"))
      (error (concat "Bio modules not present in path component " module-path)))
    (setq module-components (split-string module "::" t))
    ;; unixize...
    (setq pth (replace-regexp-in-string "\\\\" "/" module-path))

    (while (not (null module-components))
      (setq pth (concat pth "/" (car module-components)))
      (setq module-components (cdr module-components)))
    (if (not (booleanp dir))
	(if (file-exists-p pth)
	    t
	  (setq pth nil))
      (if (and dir (file-exists-p pth))
	  t
	(if (file-exists-p (concat pth ".pm"))
	    (setq pth (concat pth ".pm"))
	  (if (file-exists-p pth)
	      t
	    (setq pth nil)))))
    pth))

(defun bioperl-split-name (module &optional dir-first n)
  "Examine MODULE and return a list splitting the argument into an existing namespace and module name.
MODULE is in double-colon format. This checks existence as well,
and returns nil if no split corresponds to an existing file. The
algorithm uses `bioperl-path-from-perl' to do its tests.  Default
behavior is to return (namespace module) if there is a choice.
If DIR-FIRST is t, return (namespace nil) over (namespace module)
if there is a choice. If DIR-FIRST is not t or nil, return only
\(namespace nil) or nil.

Finally, if the namespace portion of MODULE exists, but the module
 specified by MODULE does not, (namespace nil) is returned.
N specifies the index of the desired bioperl-module-path component. "

  (unless (or (not module) (stringp module))
    (error "String arg required at MODULE"))
  (unless (or (not n) (numberp n))
    (error "Number required at arg N"))
  (unless n
    (setq n 0))
  (if (not module)
      (list nil nil)
    (if (not (string-match "^Bio" module))
	nil
      ( let (
	     (module-path (elt
			   (split-string bioperl-module-path path-separator) n))
	     (nmspc) (mod) (pmfile)
	     )
	(if (and (> n 0) (> n (1- (length module-path))))
	    (error "Path index out of bounds at arg N"))
	(if (not (string-match "::\\([a-zA-Z0-9_]+\\)$" module))
	    (setq nmspc module)
	  (setq mod (match-string 1 module))
	  (setq nmspc (substring module 0 (- (match-beginning 1) 2))))
	(cond
	 ( (not (booleanp dir-first))
	   (if (bioperl-path-from-perl module dir-first n)
	       (list module nil)
	     (list (concat "*" module) nil)) )
	 ( t
	   (setq pmfile (bioperl-path-from-perl module dir-first n))
	   (if pmfile
	       (if (string-match "\.pm$" pmfile)
		   (list nmspc mod)
		 (list module nil))
	     (if dir-first
		 (progn (setq nmspc (concat nmspc "::" mod))
			(setq mod nil)))
	     (if (bioperl-path-from-perl nmspc 1 n)
		 (list nmspc (concat "*" mod))
	       (list (concat "*" nmspc) nil))
	     )))
	))))

(defun bioperl-render-method-pod-from-cons (cons)
  "Create a view buffer containing method pod using a member of the `bioperl-method-pod-cache' alist.

CONS has the form

 ( METHOD_NAME . ( ( POD_TAG . CONTENT) (POD_TAG . CONTENT) ... ) ).

The module name for this method is assumed to be present in
`bioperl-cached-module'"
  (unless (listp cons)
    (error "List required at arg CONS"))
  (if (not cons)
      nil
    (let* (
	  (module bioperl-cached-module)
	  (method (car cons))
	  (content (cdr cons))
	  ;; reverse below is a sort-of kludge
	  (tags (if content (reverse (mapcar 'car content)) nil))
	  (cur-tag nil)
	  (cur-content nil)
	  (pod-buf (generate-new-buffer "*BioPerl POD*"))
	  )
      (if (not content)
	  (message "No pod available")
	(save-excursion
	  (set-buffer pod-buf)
	  (pod-mode)
	  (setq header-line-format (concat "Method " method
					   "() - BioPerl module " module
					   " @ "
					   (file-name-squish
					    (elt (split-string bioperl-module-path path-separator) n))))
	  (insert "  " method)
	  (insert "\n")
	  (while (setq cur-tag (pop tags))
	    (setq cur-content (cdr (assoc-string cur-tag content t)))
	    (setq cur-content (replace-regexp-in-string "!!$" "\n" cur-content))
	    (setq cur-content (replace-regexp-in-string "!!"
							"\n             " cur-content))
	    (insert "    " cur-tag)
	    (insert-char ?  (- 8 (length cur-tag)))
	    (insert ": " cur-content))
	  (goto-char (point-min))
	  (bioperl-view-mode)
	  (pop-to-buffer pod-buf)))
      )))

;;
;; completion tricks
;;

;; TODO: modularize...
(defun bioperl-completing-read (initial-input &optional get-method dir-first prompt-prefix no-retry)
  "Specialized completing read for bioperl-mode.
INITIAL-INPUT is a namespace/module name in double-colon format,
or nil. Returns a list: (namespace module path-string) if GET-METHOD is nil,
\(namespace module method path-string) if GET-METHOD is t. DIR-FIRST is
passed along to `bioperl-split-name'; controls what is returned
when a namespace name is also a module name (e.g., Bio::SeqIO).
If NO-RETRY is nil, the reader works hard to return a valid entity;
if t, the reader barfs out whatever was finally entered."
  (let ( (parsed (bioperl-split-name initial-input dir-first))
	 (nmspc) (mod) (mth) (pthn) (name-list)
	 (done nil))
    (if (not parsed)
	nil
      (setq nmspc (elt parsed 0))
      (setq mod (elt parsed 1)))
    (while (not done)
      ;; namespace completion
      (unless (and nmspc (not (string-match "^\*" nmspc)))
	(cond
	 ( (not nmspc) nil )
	 ( (string-match "^\*" nmspc)
	   (setq initial-input (replace-regexp-in-string "^\*" "" nmspc))))
	(setq nmspc (completing-read
		     (concat prompt-prefix "Namespace: ")
		     'bioperl-namespace-completion-function
		     nil (not no-retry) (or initial-input "Bio::")) )
	(if (or (string-equal nmspc "Bio") (not (string-equal nmspc "")))
	    t
	  ;; back up
	  (setq nmspc
		(if (string-match ":" nmspc)
		    (car (split-string nmspc "::[^:]+$"))
		  nil))
	  (setq done nil)))
      ;; module completion
      (if (or (not nmspc)
		  (and mod (not (string-match "^\*" mod))))
	  (setq done t)
	(let (
	      ;; local vars here
	      )
	  (setq name-list (bioperl-module-names nmspc nil t))
	  (setq mod (completing-read
		     (concat prompt-prefix nmspc " Module: ")
		     name-list nil (not no-retry)
		     (if mod (replace-regexp-in-string "^\*" "" mod) nil)))
	  ;; allow a backup into namespace completion
	  (if (or no-retry (not (string-equal mod "")))
	      (setq done t)
	    ;; retry setup
	    ;; try again, backing up
	    (setq done nil)
	    (let ( (splt (bioperl-split-name nmspc nil)) )
	      (if (elt splt 1)
		  (progn
		    (setq nmspc (elt splt 0))
		    ;; kludge : "pretend" mod is not found using the "*"
		    (setq mod (concat "*" (elt splt 1))))
		(setq nmspc (concat "*" nmspc))
		(setq mod nil)))
	    (setq initial-input nmspc))))
      ;; path completion
      (unless (or (not (and nmspc mod)) (not done) no-retry)
	(if (not name-list)
	  (setq name-list (bioperl-module-names
			   nmspc nil t)))
	(setq pthn (cdr (assoc-string mod name-list t)))
	(if (not pthn)
	    (error "Shouldn't be here(1). Check `bioperl-module-path' and try running `bioperl-clear-module-cache'."))
	(if (not (string-match path-separator pthn))
	    ;; single path
	    (setq pthn (string-to-number pthn))
	  ;; multiple paths (e.g., "0;1") - do completion
	  (let* (
		 (module-path
		  (split-string bioperl-module-path path-separator))
		 (pthns (mapcar 'string-to-number
				(split-string pthn path-separator)))
		 (i -1)
		 (module-path-list
		  (mapcar
		   (lambda (x) (setq i (1+ i)) (list x i) )
		   module-path))
		 )
	    ;; filter list by pthns
	    (setq module-path-list
		  (delete nil (mapcar
			       (lambda (x) (if (member (elt x 1) pthns) x nil))
			       module-path-list)))
	    (if (not module-path-list)
		(error "Shouldn't be here(2). Run `bioperl-clear-module-cache' and try again"))
	    (setq pthn (completing-read
			(concat prompt-prefix nmspc "::" mod " Lib: ")
			module-path-list
			nil t (car (car module-path-list))))
	    (if (string-equal pthn "")
		(setq pthn (car (car module-path-list))))
	    (setq pthn (elt (assoc-string pthn module-path-list t) 1))
	    )))
      ;; method completion
      (setq nmspc (replace-regexp-in-string "::$" "" nmspc))
      (unless (or (not done) (not (and nmspc mod)) (not get-method))
	;; path completion if necessary
	(if pthn
	    t
	  (setq pthn (cdr (bioperl-module-names nmspc nil t)))
	  (if (not (string-match path-separator pthn))
	      ;; single path
	      (setq pthn (string-to-number pthn))
	    ;; multiple paths (e.g., "0;1") - do completion
	    (let* (
		   (module-path
		    (split-string bioperl-module-path path-separator))
		   (pthns (mapcar 'string-to-number
				  (split-string pthn path-separator)))
		   (i -1)
		   (module-path-list
		    (mapcar
		     (lambda (x) (setq i (1+ i)) (list x i) )
		     module-path))
		   )
	      ;; filter list by pthns
	      (setq module-path-list
		    (delete nil (mapcar
				 (lambda (x) (if (member (elt x 1) pthns) x nil))
				 module-path-list)))
	      (if (not module-path-list)
		  (error "Shouldn't be here(3). Run `bioperl-clear-module-cache' and try again"))
	      (setq pthn (completing-read
			  (concat prompt-prefix "Lib: ")
			  module-path-list
			nil t (car (car module-path-list))))
	      (if (string-equal pthn "")
		  (setq pthn (car (car module-path-list))))
	      (setq pthn (elt (assoc-string pthn module-path-list t) 1))
	      )
	    ))
	(setq name-list (bioperl-method-names (concat nmspc "::" mod) nil pthn))
	(let (
	      ;; local vars here...
	      )
	  (setq mth (completing-read
		     (concat prompt-prefix "Method in " nmspc "::" mod ": ")
		     name-list nil (not no-retry)))
	  (if (or no-retry (not (string-equal mth "")))
	      (setq done t)
	    ;; retry setup
	    ;; allow a backup into module completion
	    (setq done nil)
	    (let (
		  (splt (bioperl-split-name (concat nmspc "::" mod) nil pthn))
		  )
	      (setq nmspc (elt splt 0))
	      ;; kludge : "pretend" mod is not found using the "*"
	      (setq mod (concat "*" (elt splt 1))))))
	))
    ;; return values
    (if get-method
	(list nmspc mod mth pthn)
      (list nmspc mod pthn)) ))

(defun bioperl-namespace-completion-function (str pred flag)
  "A custom completion function for bioperl-mode.
Allows the lazy build of the `bioperl-module-names-cache' via `bioperl-make-collection' and `bioperl-module-names'."
  (if (not pred)
      (setq pred
	    (lambda (x) (setq x (if (listp x) (car x) x) ) (if (string-match "[a-zA-Z0-9_:]+" x) t nil))
	    ))
  (let (
	( collection (if (string-equal str "") '(("Bio" . nil )) (bioperl-make-collection str t)) )
	)
    ;; offer the right collection:
    ;; if collection was set, the str was complete and valid
    ;; if not, back up to the last :: in str (see str-trunc in above
    ;; let) and try again

    (if (not collection)
	nil
      (setq collection (sort collection (lambda (x y) (string< (car x) (car y)))))
      (cond
       ((not (booleanp flag)) ;; 'lambda' or test-completion option
	;; this is a back-compat issue: emacs 21 will send 'lambda',
	;; but doesn't have 'test-completion
	;;
	;; Note without test-completion, weird completion bugs can crop
        ;; up -- best upgrade to 22--
	(if (condition-case nil
		(symbol-function 'test-completion)
	      ('error nil))
	    (test-completion str collection pred)
	  collection
	  (try-completion str collection pred))
	)
       ( (not flag) ;; try-completion option
	   (try-completion str collection pred)
	   )
       ( flag ;; all-completion option
	   (all-completions str collection pred)
	   )
       ))))

(defun bioperl-make-collection (module-dir &optional retopt)
  "Create a completion collection for MODULE-DIR.
MODULE-DIR is in double-colon format, possibly with two trailing
colons.  RETOPT is as for `bioperl-module-names'.

This function searches all paths specified in
`bioperl-module-path'."

  ;; handle the boundary
  (if (or (not module-dir) (not (string-match ":" module-dir)))
      '(("Bio") ("Bio::"))
    (setq module-dir (progn (string-match "^\\([a-zA-Z0-9_:]+[^:]\\):*$" module-dir)
			    (match-string 1 module-dir)))
    (let* (
	   ( dirs (bioperl-module-names module-dir retopt t) )
	   ( modules (split-string module-dir "::" t) )
	   ( complet )
	   )

      ;; check once and recalc
      (if (not dirs)
	  (progn
	    ;; trim back to last ::
	    (setq module-dir
		  (progn
		    (string-match  "^\\(\\(?:[a-zA-Z0-9_]+::\\)+\\)\\(?::*\\|[a-zA-Z0-9_]*\\)$" module-dir)
		    (match-string 1 module-dir)))
	    (setq dirs (bioperl-module-names module-dir retopt t))
	    (setq modules (split-string module-dir "::" t))
	    ))
      (if (not dirs)
	  ;; fail
	  nil
	(setq complet (let* ( (l modules)
			      (m (list (pop l))) )
			(while l (push (concat (car m) "::" (pop l)) m))
			(mapcar (lambda (x) (cons x nil)) m ) ))
	;; make sure module-dir is trimmed
	(setq module-dir (replace-regexp-in-string "::$" "" module-dir))
	complet
	(append complet (mapcar (lambda (x)
				  (list
				   (concat module-dir "::" (car x))
				   (cdr x))) dirs))
	))
      ))

;;
;; utilities
;;

(defun bioperl-clear-module-cache ()
  (interactive)
  "Clears the variable `bioperl-module-names-cache'. Run if you change `bioperl-module-path'."
  (setq bioperl-module-names-cache nil)
  (setq bioperl-module-names-cache '(("Bio"))))

;;
;; utilities (out of bioperl- namespace)
;;


(defun assoc-all (key alist &optional ret)
  "Return list of *pointers* (like assoc) to all matching conses in the alist.
Uses `assoc-string' for case control."
  (let ( (c (assoc-string key alist t)) (r) )
    (if c
	(assoc-all key (cdr alist) (if ret (add-to-list 'ret c t 'eq) (list c)))
      ret)))

(defun deep-assoc (keys alist)
  "Return the associations of a set of keys in an alist tree.
Uses `assoc-string' for case control."
  (cond
   ((not keys)
    nil)
   ((not (listp alist))
    nil)
   ((= (length keys) 1)
    (assoc-string (pop keys) alist t))
   (t
    (let* ( (key (pop keys))
	    (newlist (assoc-string key alist t)) )
      (if newlist
	  (deep-assoc keys (cdr newlist))
	(deep-assoc nil nil)))
    )))

(defun deep-assoc-all (keys alist)
  "Return all associations AT THE TIP described by the set of KEYS in an alist tree.
So this is not completely general, but is specialized to the structure of `bioperl-module-names-cache'."
  (cond
   ((not keys)
    nil)
   ((not (listp alist))
    nil)
   ((= (length keys) 1)
    (assoc-all (pop keys) alist))
   (t
    (let* ( (key (pop keys))
	    (newlist (assoc-all key alist)) )
      (if newlist
	  (let ( ( i 0 ) (r)  )
	    (while (< i (length newlist))
	      (if (listp (cdr (elt newlist i)))
		  (setq r (deep-assoc-all keys (cdr (elt newlist i)))))
	      (setq i (1+ i)))
	    r)
	(deep-assoc-all nil nil)))
    )))


(defun pm-p (x)
  (not (null (string-match "[.]pm\$" x))))

(defun split-string-compat (str &optional sep omit-nulls)
  "`split-string' for 21"
  (if omit-nulls
      (delete nil (mapcar (lambda (x) (if (string-equal x "") nil x)) (split-st\
ring str sep)))
    (split-string str sep)))

(defun file-name-squish (fname)
  "Squish long file names with central elipses.
FNAME is the file name as string. Doesn't work very hard."
  (let* (
	 (fname-list (split-string fname "/"))
	 (squished)
	 )
    (if (> (length fname-list) 3)
	(concat (elt fname-list 0) "/"
		(elt fname-list 1) "/"
		(if (= (length fname-list) 4) (elt fname-list 2)  "...")
		"/"
		(car (last fname-list)))
      fname)))

;; hook into perl-mode

(add-hook 'perl-mode-hook 'bioperl-perl-mode-infect)

(provide 'bioperl-mode)

;;; end bioperl-mode.el


;;
;; scratch area
;;
(unless nil






)
