;;; core-configuration-layer.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'cl-lib)
(require 'eieio)
(require 'subr-x)
(require 'package)
(require 'warnings)
(require 'help-mode)
(require 'ht)
(require 'core-dotspacemacs)
(require 'core-funcs)
(require 'core-spacemacs-buffer)

(defvar configuration-layer--refresh-package-timeout dotspacemacs-elpa-timeout
  "Timeout in seconds to reach a package archive page.")

(defconst configuration-layer-template-directory
  (expand-file-name (concat spacemacs-core-directory "templates/"))
  "Configuration layer templates directory.")

(defconst configuration-layer-directory
  (expand-file-name (concat spacemacs-start-directory "layers/"))
  "Spacemacs contribution layers base directory.")

(defconst configuration-layer-private-directory
  (expand-file-name (concat spacemacs-start-directory "private/"))
  "Spacemacs private layers base directory.")

(defconst configuration-layer-private-layer-directory
  (let ((dotspacemacs-layer-dir
         (when dotspacemacs-directory
           (expand-file-name
            (concat dotspacemacs-directory "layers/")))))
    (if (and dotspacemacs-directory
             (file-exists-p dotspacemacs-layer-dir))
        dotspacemacs-layer-dir
      configuration-layer-private-directory))
  "Spacemacs default directory for private layers.")

(defun configuration-layer/elpa-directory (root)
  "Evaluate the correct package subdirectory of ROOT. This is
done according to the value of `dotspacemacs-elpa-subdirectory'.
If it is nil, then ROOT is returned. Otherwise a subdirectory of
ROOT is returned."
  (if (not dotspacemacs-elpa-subdirectory)
      root
    (let ((subdir (if (eq 'emacs-version dotspacemacs-elpa-subdirectory)
                      (format "%d%s%d"
                              emacs-major-version
                              version-separator
                              emacs-minor-version)
                    (eval dotspacemacs-elpa-subdirectory))))
      (file-name-as-directory (expand-file-name subdir root)))))

(defun configuration-layer/get-elpa-package-install-directory (pkg)
  "Return the install directory of elpa PKG. Return nil if it is not found."
  (let ((elpa-dir package-user-dir))
    (when (file-exists-p elpa-dir)
      (let* ((pkg-match (concat "\\`" (symbol-name pkg) "-[0-9]+"))
             (dir (car (directory-files elpa-dir 'full pkg-match))))
        (when dir (file-name-as-directory dir))))))

(defvar configuration-layer-rollback-directory
  (concat spacemacs-cache-directory ".rollback/")
  "Spacemacs rollback directory.")

(defconst configuration-layer-rollback-info "rollback-info"
  "Spacemacs rollback information file.")

(defclass cfgl-layer ()
  ((name :initarg :name
         :type symbol
         :documentation "Name of the layer.")
   (dir :initarg :dir
        :initform nil
        :type (satisfies (lambda (x) (or (null x) (stringp x))))
        :documentation "Absolute path to the layer directory.")
   (packages :initarg :packages
             :initform nil
             :type list
             :documentation "List of package symbols declared in this layer.")
   (selected-packages :initarg :selected-packages
             :initform 'all
             :type (satisfies (lambda (x) (or (and (symbolp x) (eq 'all x))
                                              (listp x))))
             :documentation "List of selected package symbols.")
   (variables :initarg :variables
              :initform nil
              :type list
              :documentation "A list of variable-value pairs.")
   (lazy-install :initarg :lazy-install
                 :initform nil
                 :type boolean
                 :documentation
                 "If non-nil then the layer needs to be installed")
   (disabled :initarg :disabled-for
             :initform nil
             :type list
             :documentation "A list of layers where this layer is disabled.")
   (enabled :initarg :enabled-for
            :initform 'unspecified
            :type (satisfies (lambda (x) (or (listp x) (eq 'unspecified x))))
            :documentation (concat "A list of layers where this layer is enabled. "
                                   "(Takes precedence over `:disabled-for'.)")))
  "A configuration layer.")

(defmethod cfgl-layer-owned-packages ((layer cfgl-layer) &optional props)
  "Return the list of owned packages by LAYER.
If PROPS is non-nil then return packages as lists with their properties.
LAYER has to be installed for this method to work properly."
  (delq nil (mapcar
             (lambda (x)
               (let* ((pkg-name (if (listp x) (car x) x))
                      (pkg (configuration-layer/get-package pkg-name)))
                 (when (eq (oref layer :name) (car (oref pkg :owners))) x)))
             (cfgl-layer-get-packages layer props))))

(defmethod cfgl-layer-owned-packages ((layer nil) &optional props)
  "Accept nil as argument and return nil."
  nil)

(defmethod cfgl-layer-get-packages ((layer cfgl-layer) &optional props)
  "Return the list of packages for LAYER.
If PROPS is non-nil then return packages as lists with their properties"
  (let ((all (eq 'all (oref layer :selected-packages))))
    (delq nil (mapcar
               (lambda (x)
                 (let ((pkg-name (if (listp x) (car x) x)))
                   (when (or all (memq pkg-name
                                       (oref layer :selected-packages)))
                     (if props x pkg-name))))
               (oref layer :packages)))))

(defclass cfgl-package ()
  ((name :initarg :name
         :type symbol
         :documentation "Name of the package.")
   (min-version :initarg :min-version
                :initform nil
                :type list
                :documentation "Minimum version to install as a version list.")
   (owners :initarg :owners
           :initform nil
           :type list
           :documentation "The layer defining the init function.")
   (pre-layers :initarg :pre-layers
               :initform '()
               :type list
               :documentation "List of layers with a pre-init function.")
   (post-layers :initarg :post-layers
               :initform '()
               :type list
               :documentation "List of layers with a post-init function.")
   (location :initarg :location
             :initform elpa
             :type (satisfies (lambda (x)
                                (or (stringp x)
                                    (memq x '(built-in local site elpa))
                                    (and (listp x) (eq 'recipe (car x))))))
             :documentation "Location of the package.")
   (toggle :initarg :toggle
           :initform t
           :type (satisfies (lambda (x) (or (symbolp x) (listp x))))
           :documentation
           "Package is enabled/installed if toggle evaluates to non-nil.")
   (step :initarg :step
         :initform nil
         :type (satisfies (lambda (x) (member x '(nil bootstrap pre))))
         :documentation "Initialization step.")
   (lazy-install :initarg :lazy-install
                 :initform nil
                 :type boolean
                 :documentation
                 "If non-nil then the package needs to be installed")
   (protected :initarg :protected
              :initform nil
              :type boolean
              :documentation
              "If non-nil then this package cannot be excluded.")
   (excluded :initarg :excluded
             :initform nil
             :type boolean
             :documentation
             "If non-nil this package is excluded from all layers.")))

(defmethod cfgl-package-enabledp ((pkg cfgl-package) &optional inhibit-messages)
  "Evaluate the `toggle' slot of passed PKG.
If INHIBIT-MESSAGES is non nil then any message emitted by the toggle evaluation
is ignored."
  (let ((message-log-max (unless inhibit-messages message-log-max))
        (toggle (oref pkg :toggle)))
    (eval toggle)))

(defmethod cfgl-package-get-safe-owner ((pkg cfgl-package))
  "Safe method to return the name of the layer which owns PKG."
  ;; The owner of a package is the first *used* layer in `:owners' slot.
  ;; Note: for packages in `configuration-layer--used-packages' the owner is
  ;; always the car of the `:owners' slot.
  (let ((layers (oref pkg :owners)))
    (while (and (consp layers)
                (not (configuration-layer/layer-usedp (car layers))))
      (pop layers))
    (when (configuration-layer/layer-usedp (car layers))
      (car layers))))

(defmethod cfgl-package-set-property ((pkg cfgl-package) slot value)
  "Set SLOT to the given VALUE for the package PKG.
If `configuration-layer--package-properties-read-onlyp' is non-nil then VALUE
is not set for the given SLOT."
  (unless configuration-layer--package-properties-read-onlyp
    (eval `(oset pkg ,slot value))))

(defvar configuration-layer--elpa-archives
  '(("melpa" . "melpa.org/packages/")
    ("org"   . "orgmode.org/elpa/")
    ("gnu"   . "elpa.gnu.org/packages/"))
  ;; '(("spacelpa" . "~/.emacs.d/.cache/spacelpa/"))
  "List of ELPA archives required by Spacemacs.")

(defvar configuration-layer-exclude-all-layers nil
  "If non nil then only the distribution layer is loaded.")

(defvar configuration-layer-force-distribution nil
  "If set, bypass the user's choice `dotspacemacs-distribution'.")

(defvar configuration-layer--package-archives-refreshed nil
  "Non nil if package archives have already been refreshed.")

(defvar configuration-layer--load-packages-files nil
  "If non-nil force loading `packages.el' files when creating layer objects.")

(defvar configuration-layer--used-layers '()
  "A non-sorted list of used layer names.")

(defvar configuration-layer--indexed-layers (make-hash-table :size 1024)
  "Hash map to index `cfgl-layer' objects by their names.")

(defvar configuration-layer--used-packages '()
  "An alphabetically sorted list of used package names.")

(defvar configuration-layer--indexed-packages (make-hash-table :size 2048)
  "Hash map to index `cfgl-package' objects by their names.")

(defvar configuration-layer--used-distant-packages '()
  "A list of all distant packages that are effectively used.")

(defvar configuration-layer--check-new-version-error-packages nil
  "A list of all packages that were skipped during last update attempt.")

(defvar configuration-layer--protected-packages nil
  "A list of packages that will be protected from removal as orphans.")

(defvar configuration-layer--lazy-mode-alist nil
  "Association list where the key is a mode and the value a regexp.")

(defvar configuration-layer--inhibit-warnings nil
  "If non-nil then warning message emitted by the layer system are ignored.")

(defvar configuration-layer--package-properties-read-onlyp nil
  "If non-nil then package properties are read only and cannot be overriden by
`configuration-layer/make-package'.")

(defvar configuration-layer--declared-layers-usedp nil
  "If non-nil then declared layers are considered to be used.")

(defvar configuration-layer-error-count nil
  "Non nil indicates the number of errors occurred during the
installation of initialization.")

(defvar configuration-layer-categories '()
  "List of strings corresponding to category names. A category is a
directory with a name starting with `+'.")

(defvar update-packages-alist '()
  "Used to collect information about rollback packages in the
cache folder.")

(defun configuration-layer/initialize ()
  "Initialize `package.el'."
  (setq configuration-layer--refresh-package-timeout dotspacemacs-elpa-timeout)
  (unless package--initialized
    (setq configuration-layer-rollback-directory
          (configuration-layer/elpa-directory configuration-layer-rollback-directory))
    (setq package-user-dir
          (configuration-layer/elpa-directory package-user-dir))
    (setq package-archives (configuration-layer//resolve-package-archives
                            configuration-layer--elpa-archives))
    ;; optimization, no need to activate all the packages so early
    (setq package-enable-at-startup nil)
    (package-initialize 'noactivate)))

(defun configuration-layer//configure-quelpa ()
  "Configure `quelpa' package."
  (setq quelpa-verbose init-file-debug
        quelpa-dir (concat spacemacs-cache-directory "quelpa/")
        quelpa-build-dir (expand-file-name "build" quelpa-dir)
        quelpa-persistent-cache-file (expand-file-name "cache" quelpa-dir)
        quelpa-update-melpa-p nil)
  (require 'quelpa))

(defun configuration-layer//package-archive-absolute-pathp (archive)
  "Return t if ARCHIVE has an absolute path defined."
  (let ((path (cdr archive)))
    (or (string-match-p "http" path)
        (string-prefix-p "~" path)
        (string-prefix-p "/" path))))

(defun configuration-layer//package-archive-local-pathp (archive)
  "Return t if ARCHIVE has a local path."
  (let ((path (cdr archive)))
    (or (string-prefix-p "~" path)
        (string-prefix-p "/" path)
        (string-prefix-p "\." path))))

(defun configuration-layer//resolve-package-archives (archives)
  "Resolve HTTP handlers for each archive in ARCHIVES and return a list
of all reachable ones.
If the address of an archive already contains the protocol then this address is
left untouched.
The returned list has a `package-archives' compliant format."
  (mapcar
   (lambda (x)
     (let ((aname (car x))
           (apath (cdr x)))
       (cons aname
             (if (configuration-layer//package-archive-absolute-pathp x)
                 apath
               (concat
                (if (and dotspacemacs-elpa-https
                         (not spacemacs-insecure))
                    "https://"
                  "http://")
                apath)))))
   archives))

(defun configuration-layer/retrieve-package-archives (&optional quiet force)
  "Retrieve all archives declared in current `package-archives'.

This function first performs a simple GET request with a timeout in order to
fix very long refresh time when an archive is not reachable.

Note that this simple GET is a heuristic to determine the availability
likelihood of an archive, so it can gives false positive if the archive
page is served but the archive is not.

If QUIET is non nil then the function does not print message in the Spacemacs
home buffer.

If FORCE is non nil then refresh the archives even if they have been already
refreshed during the current session."
  (unless (and configuration-layer--package-archives-refreshed
               (not force))
    (setq configuration-layer--package-archives-refreshed t)
    (let ((count (length package-archives))
          (i 1))
      (dolist (archive package-archives)
        (let ((aname (car archive))
              (apath (cdr archive)))
          (unless quiet
            (spacemacs-buffer/replace-last-line
             (format "--> refreshing package archive: %s... [%s/%s]"
                     aname i count) t))
          (spacemacs//redisplay)
          (setq i (1+ i))
          (unless
              (and (not (configuration-layer//package-archive-local-pathp
                         archive))
                   (eq 'error
                       (with-timeout
                           (dotspacemacs-elpa-timeout
                            (progn
                              (display-warning
                               'spacemacs
                               (format
                                "\nError connection time out for %s repository!"
                                aname) :warning)
                              'error))
                         (condition-case err
                             (url-retrieve-synchronously apath)
                           ('error
                            (display-warning
                             'spacemacs
                             (format
                              "\nError while contacting %s repository!"
                              aname) :warning)
                            'error)))))
            (let ((package-archives (list archive)))
              (package-refresh-contents)))))
      (package-read-all-archive-contents)
      (unless quiet (spacemacs-buffer/append "\n")))))

(defun configuration-layer/sync (&optional no-install)
  "Synchronize declared layers in dotfile with spacemacs.
If NO-INSTALL is non nil then install steps are skipped."
  (dotspacemacs|call-func dotspacemacs/layers "Calling dotfile layers...")
  (setq dotspacemacs--configuration-layers-saved
        dotspacemacs-configuration-layers)
  (when (spacemacs-buffer//choose-banner)
    (spacemacs-buffer//inject-version))
  ;; declare used layers then packages as soon as possible to resolve
  ;; usage and ownership
  (configuration-layer/discover-layers)
  (configuration-layer//declare-used-layers dotspacemacs-configuration-layers)
  (configuration-layer//declare-used-packages configuration-layer--used-layers)
  ;; then load the functions and finally configure the layers
  (configuration-layer//load-layers-files configuration-layer--used-layers
                                          '("funcs.el"))
  (configuration-layer//configure-layers configuration-layer--used-layers)
  ;; pre-filter some packages to save some time later in the loading process
  (setq configuration-layer--used-distant-packages
        (configuration-layer//get-distant-packages
         configuration-layer--used-packages t))
  ;; load layers lazy settings
  (configuration-layer/load-auto-layer-file)
  ;; install and/or uninstall packages
  (unless no-install
    (let ((packages
           (append
            ;; install used packages
            (configuration-layer/filter-objects
             configuration-layer--used-distant-packages
             (lambda (x)
               (let ((pkg (configuration-layer/get-package x)))
                 (not (oref pkg :lazy-install)))))
            ;; also install all other packages if requested
            (when (eq 'all dotspacemacs-install-packages)
              (let (all-other-packages)
                (dolist (layer (configuration-layer/get-layers-list))
                  (let ((configuration-layer--declared-layers-usedp nil)
                        (configuration-layer--load-packages-files t))
                    (configuration-layer/declare-layer layer)
                    (let* ((obj (configuration-layer/get-layer layer))
                           (pkgs (when obj (oref obj :packages))))
                      (configuration-layer/make-packages-from-layers
                       (list layer))
                      (dolist (pkg pkgs)
                        (let ((pkg-name (if (listp pkg) (car pkg) pkg)))
                          (add-to-list 'all-other-packages pkg-name))))))
                (configuration-layer//get-distant-packages
                 all-other-packages nil))))))
      (configuration-layer//install-packages packages)
      (when (and (or (eq 'used dotspacemacs-install-packages)
                     (eq 'used-only dotspacemacs-install-packages))
                 (not configuration-layer-force-distribution)
                 (not configuration-layer-exclude-all-layers))
        (configuration-layer/delete-orphan-packages packages))))
  ;; configure used packages
  (configuration-layer//configure-packages configuration-layer--used-packages)
  (configuration-layer//load-layers-files configuration-layer--used-layers
                                          '("keybindings.el")))

(defun configuration-layer/load-auto-layer-file ()
  "Load `auto-layer.el' file"
  (let ((file (concat configuration-layer-directory "auto-layer.el")))
    (when (file-exists-p file)
      (spacemacs-buffer/message "Loading auto-layer file...")
      (load-file file))))

(defun configuration-layer/create-layer ()
  "Ask the user for a configuration layer name and the layer
directory to use. Create a layer with this name in the selected
layer directory."
  (interactive)
  (let* ((current-layer-paths (mapcar (lambda (dir) (expand-file-name dir))
                                      (cl-pushnew
                               configuration-layer-private-layer-directory
                               dotspacemacs-configuration-layer-path)))
         (other-choice "Another directory...")
         (helm-lp-source
          `((name . "Configuration Layer Paths")
            (candidates . ,(append current-layer-paths
                                   (list other-choice)))
            (action . (lambda (c) c))))
         (layer-path-sel (if (configuration-layer/layer-usedp 'ivy)
                             (ivy-read "Configuration layer path: "
                                       (append current-layer-paths
                                               (list other-choice)))
                           (helm :sources helm-lp-source
                                 :prompt "Configuration layer path: ")))
         (layer-path (cond
                      ((string-equal layer-path-sel other-choice)
                       (read-directory-name (concat "Other configuration "
                                                    "layer path: ") "~/" ))
                      ((member layer-path-sel current-layer-paths)
                       layer-path-sel)
                      (t
                       (error "Please select an option from the list"))))
         (name (read-from-minibuffer "Configuration layer name: " ))
         (layer-dir (concat layer-path "/" name)))
    (cond
     ((string-equal "" name)
      (message "Cannot create a configuration layer without a name."))
     ((file-exists-p layer-dir)
      (message (concat "Cannot create configuration layer \"%s\", "
                       "this layer already exists.") name))
     (t
      (make-directory layer-dir t)
      (configuration-layer//copy-template name "packages.el" layer-dir)
      (when (y-or-n-p "Create readme?")
        (configuration-layer//copy-template name "README.org" layer-dir))
      (message "Configuration layer \"%s\" successfully created." name)))))

(defun configuration-layer//select-packages (layer-specs packages)
  "Return the selected packages of LAYER-SPECS from given PACKAGES list."
  (let* ((value (when (listp layer-specs)
                  (spacemacs/mplist-get layer-specs :packages)))
         (selected-packages (if (and (not (null (car value)))
                                     (listp (car value)))
                                (car value)
                              value)))
    (cond
     ;; select packages
     ((and selected-packages
           (not (memq (car selected-packages) '(all not))))
      selected-packages)
     ;; unselect packages
     ((and selected-packages
           (eq 'not (car selected-packages)))
      (delq nil (mapcar (lambda (x)
                          (let ((pkg-name (if (listp x) (car x) x)))
                            (unless (memq pkg-name selected-packages)
                              pkg-name)))
                        packages)))
     ;; no package selections or all package selected
     (t 'all))))

(defun configuration-layer/make-layer (layer-specs &optional obj usedp dir)
  "Return a `cfgl-layer' object based on LAYER-SPECS.
If LOAD-PKGS is non-nil then load the `packages.el' file of the layer.
DIR is the directory where the layer is, if it is nil then search in the
indexed layers for the path."
  (let* ((layer-name (if (listp layer-specs) (car layer-specs) layer-specs))
         (obj (if obj obj (cfgl-layer (symbol-name layer-name)
                                      :name layer-name)))
         (dir (or dir (oref obj :dir))))
    (if (or (null dir)
            (and dir (not (file-exists-p dir))))
        (configuration-layer//warning
         "Cannot make layer %S without a valid directory!"
         layer-name)
      (let* ((dir (file-name-as-directory dir))
             (disabled (when (listp layer-specs)
                         (spacemacs/mplist-get layer-specs :disabled-for)))
             (enabled (if (and (listp layer-specs)
                               (memq :enabled-for layer-specs))
                          (spacemacs/mplist-get layer-specs :enabled-for)
                        'unspecified))
             (variables (when (listp layer-specs)
                          (spacemacs/mplist-get layer-specs :variables)))
             (packages-file (concat dir "packages.el"))
             (packages
              (if (and (or usedp configuration-layer--load-packages-files)
                       (file-exists-p packages-file))
                  (progn
                    (load packages-file)
                    (symbol-value (intern (format "%S-packages" layer-name))))
                (oref obj :packages)))
             (selected-packages (if packages
                                    (configuration-layer//select-packages
                                     layer-specs packages)
                                  ;; default value
                                  'all)))
        (oset obj :dir dir)
        (when usedp
          (oset obj :disabled-for disabled)
          (oset obj :enabled-for enabled)
          (oset obj :variables variables))
        (when packages
          (oset obj :packages packages)
          (oset obj :selected-packages selected-packages))
        obj))))

(defun configuration-layer/make-package (pkg layer-name &optional obj)
  "Return a `cfgl-package' object based on PKG.
If OBJ is non nil then copy PKG properties into OBJ, otherwise create
a new object.
Properties that can be copied are `:location', `:step' and `:excluded'.
If TOGGLEP is nil then `:toggle' parameter is ignored."
  (let* ((pkg-name (if (listp pkg) (car pkg) pkg))
         (pkg-name-str (symbol-name pkg-name))
         (layer (unless (eq 'dotfile layer-name)
                  (configuration-layer/get-layer layer-name)))
         (min-version (when (listp pkg) (plist-get (cdr pkg) :min-version)))
         (step (when (listp pkg) (plist-get (cdr pkg) :step)))
         (toggle (when (listp pkg) (plist-get (cdr pkg) :toggle)))
         (excluded (when (listp pkg) (plist-get (cdr pkg) :excluded)))
         (location (when (listp pkg) (plist-get (cdr pkg) :location)))
         (protected (when (listp pkg) (plist-get (cdr pkg) :protected)))
         (init-func (intern (format "%S/init-%S"
                                    layer-name pkg-name)))
         (pre-init-func (intern (format "%S/pre-init-%S"
                                        layer-name pkg-name)))
         (post-init-func (intern (format "%S/post-init-%S"
                                         layer-name pkg-name)))
         (copyp (not (null obj)))
         (obj (if obj obj (cfgl-package pkg-name-str :name pkg-name)))
         (ownerp (or (and (eq 'dotfile layer-name)
                          (null (oref obj :owners)))
                     (fboundp init-func))))
    (when min-version
      (cfgl-package-set-property obj :min-version (version-to-list min-version)))
    (when step (cfgl-package-set-property obj :step step))
    (when toggle (cfgl-package-set-property obj :toggle toggle))
    (cfgl-package-set-property obj :excluded
                               (and (configuration-layer/layer-usedp layer-name)
                                    (or excluded (oref obj :excluded))))
    (when location
      (if (and (listp location)
               (eq (car location) 'recipe)
               (eq (plist-get (cdr location) :fetcher) 'local))
          (cond
           (layer (let ((path (expand-file-name
                               (format "%s%s/%s.el"
                                       (configuration-layer/get-layer-local-dir
                                        layer-name)
                                       pkg-name-str pkg-name-str))))
                    (cfgl-package-set-property
                     obj :location `(recipe :fetcher file :path ,path))))
           ((eq 'dotfile layer-name)
            ;; TODO what is the local path for a packages owned by the dotfile?
            nil))
        (cfgl-package-set-property obj :location location)))
    ;; cannot override protected packages
    (unless copyp
      ;; a bootstrap package is protected
      (cfgl-package-set-property
       obj :protected (or protected (eq 'bootstrap step)))
      (when protected
        (push pkg-name configuration-layer--protected-packages)))
    (when ownerp
      ;; warn about mutliple owners
      (when (and (oref obj :owners)
                 (not (memq layer-name (oref obj :owners))))
        (configuration-layer//warning
         (format (concat "More than one init function found for "
                         "package %S. Previous owner was %S, "
                         "replacing it with layer %S.")
                 pkg-name (car (oref obj :owners)) layer-name)))
      ;; last owner wins over the previous one
      (object-add-to-list obj :owners layer-name))
    ;; check consistency between package and defined init functions
    (unless (or ownerp
                (eq 'dotfile layer-name)
                (eq 'system layer-name)
                (fboundp pre-init-func)
                (fboundp post-init-func)
                (oref obj :excluded))
      (configuration-layer//warning
       (format (concat "package %s not initialized in layer %s, "
                       "you may consider removing this package from "
                       "the package list or use the :toggle keyword "
                       "instead of a `when' form.")
               pkg-name layer-name)))
    ;; check if toggle can be applied
    (when (and (not ownerp)
               (and (not (eq 'unspecified toggle))
                    toggle))
      (configuration-layer//warning
       (format (concat "Ignoring :toggle for package %s because "
                       "layer %S does not own it.")
               pkg-name layer-name)))
    (when (fboundp pre-init-func)
      (object-add-to-list obj :pre-layers layer-name))
    (when (fboundp post-init-func)
      (object-add-to-list obj :post-layers layer-name))
    obj))

(define-button-type 'help-dotfile-variable
  :supertype 'help-xref
  'help-function
  (lambda (variable)
    (with-current-buffer (find-file-noselect dotspacemacs-filepath)
      (pop-to-buffer (current-buffer))
      (goto-char (point-min))
      ;; try to exclude comments
      (if (re-search-forward (format "^[a-z\s\\(\\-]*%s" variable)
                             nil 'noerror)
          (beginning-of-line)
        (message "Unable to find location in file"))))
  'help-echo
  (purecopy (concat "mouse-2, RET: "
                    "visit the Spacemacs dotfile where variable is defined.")))

(defun configuration-layer/describe-package (pkg-symbol
                                             &optional layer-list pkg-list)
  "Describe a package in the context of the configuration layer system."
  (interactive
   (list (intern
          (completing-read "Package: " configuration-layer--used-packages))))
  (let* ((pkg (configuration-layer/get-package pkg-symbol))
         (owners (oref pkg :owners))
         (owner (car owners)))
    (with-help-window (help-buffer)
      ;; declaration location
      (princ pkg-symbol)
      (princ " is a package declared and configured ")
      (cond
       ((eq 'dotfile owner)
        (princ "by the variable `dotspacemacs-additional-packages' ")
        (with-current-buffer standard-output
          (save-excursion
            (re-search-backward "`\\([^`']+\\)'" nil t)
            (help-xref-button 1 'help-variable
                              'dotspacemacs-additional-packages
                              dotspacemacs-filepath)))
        (princ "in your `dotfile'.\n")
        (with-current-buffer standard-output
          (save-excursion
            (re-search-backward "`\\([^`']+\\)'" nil t)
            (help-xref-button
             1 'help-dotfile-variable 'dotspacemacs-additional-packages))))
       ((not (null owner))
        (let* ((layer (configuration-layer/get-layer owner))
               (path (concat (oref layer dir) "packages.el")))
          (princ "by the layer `")
          (princ owner)
          (princ "'.\n")
          (with-current-buffer standard-output
            (save-excursion
              (re-search-backward "`\\([^`']+\\)'" nil t)
              (help-xref-button
               1 'help-function-def
               (intern (format "%S/init-%S" owner pkg-symbol)) path)))))
       (t
        (princ "in an unknown place in the lisp parenthesis universe.\n")))
      ;; exclusion/protection
      (if (oref pkg :protected)
          (princ "\nThis package is protected and cannot be excluded.\n")
        (when (oref pkg :excluded)
          (princ "\nThis package is excluded and cannot be installed.\n")))
      ;; toggle
      (unless (or (oref pkg :excluded) (eq t (oref pkg :toggle)))
        (princ "\nA toggle is defined for this package, it is currently ")
        (princ (if (cfgl-package-enabledp pkg t) "on" "off"))
        (princ " because the following expression evaluates to ")
        (princ (if (cfgl-package-enabledp pkg t) "t:\n" "nil:\n"))
        (princ (oref pkg :toggle))
        (princ "\n"))
      (unless (oref pkg :excluded)
        ;; usage and installation
        (if (not (configuration-layer/package-usedp pkg-symbol))
            (princ "\nYou are not using this package.\n")
          (princ "\nYou are using this package")
          (if (or (memq (oref pkg :location) '(built-in local site))
                  (stringp (oref pkg :location)))
              (princ ".\n")
            (if (not (package-installed-p pkg-symbol))
                (princ " but it is not yet installed.\n")
              (princ ", it is currently installed ")
              (if (featurep pkg-symbol)
                  (princ "and loaded.\n")
                (princ "but it has not been loaded yet.\n")))))
        (when (configuration-layer/package-lazy-installp pkg-symbol)
          (princ
           "\nThis package can be lazily installed using `auto-mode-alist'.\n")
          (with-current-buffer standard-output
            (save-excursion
              (re-search-backward "`\\([^`']+\\)'" nil t)
              (help-xref-button 1 'help-variable 'auto-mode-alist)))
          (when (assq pkg-symbol configuration-layer--lazy-mode-alist)
            (princ (concat "Actually it will be installed when one of the "
                           "following files is opened: \n"))
            (princ (cdr (assq pkg-symbol
                              configuration-layer--lazy-mode-alist)))
            (princ "\n")))
        ;; source location
        (let ((location (oref pkg :location)))
          (cond
           ((eq 'built-in location)
            (princ "\nThis is a built-in package distributed with Emacs.\n"))
           ((eq 'local location)
            (let* ((layer (configuration-layer/get-layer owner))
                   (path (format "%slocal/%S" (oref layer dir) pkg-symbol)))
              (princ (concat "\nThis is a local package whose source files "
                             "can be found in layer `"))
              (princ owner)
              (princ "'.\n")
              (with-current-buffer standard-output
                (save-excursion
                  (re-search-backward "`\\([^`']+\\)'" nil t)
                  (help-xref-button 1 'help-package-def path)))))
           ((eq 'site location)
            ;; TODO find a way to find the location on disk and detect if it is
            ;; really installed
            (princ "\nWhen used it must be installed by a third party.\n"))
           ((eq 'elpa location)
            ;; TODO find a way to find the ELPA repository
            (princ "\nWhen used it is downloaded from an ELPA repository.\n"))
           ((and (listp location) (eq 'recipe (car location)))
            (princ (concat "\nWhen used it is downloaded using `quelpa' "
                           "with the following recipe:\n"))
            (with-current-buffer standard-output
              (save-excursion
                (re-search-backward "`\\([^`']+\\)'" nil t)
                (help-xref-button
                 1 'help-url "https://github.com/quelpa/quelpa")))
            (princ location)
            (princ "\n"))))
        ;; pre/post init functions
        (when (or (oref pkg pre-layers) (oref pkg post-layers))
          (princ (concat "\nAdditional configuration for this package "
                         "can be found in the following "))
          (if (null layer-list)
              (princ "used layers:\n")
            (princ "layers:\n"))
          (when (oref pkg pre-layers)
            (princ "(pre-init)  ")
            (dolist (layer-sym (sort (oref pkg pre-layers) 'string<))
              (let* ((layer (configuration-layer/get-layer layer-sym))
                     (path (concat (oref layer dir) "packages.el")))
                (princ (concat "`" (symbol-name layer-sym) "'"))
                (with-current-buffer standard-output
                  (save-excursion
                    (re-search-backward "`\\([^`']+\\)'" nil t)
                    (help-xref-button
                     1 'help-function-def
                     (intern (format "%S/pre-init-%S" layer-sym pkg-symbol))
                     path))))
              (princ " "))
            (princ "\n"))
          (when (oref pkg post-layers)
            (princ "(post-init) ")
            (dolist (layer-sym (sort (oref pkg post-layers) 'string<))
              (let* ((layer (configuration-layer/get-layer layer-sym))
                     (path (concat (oref layer dir) "packages.el")))
                (princ (concat "`" (symbol-name layer-sym) "'"))
                (with-current-buffer standard-output
                  (save-excursion
                    (re-search-backward "`\\([^`']+\\)'" nil t)
                    (help-xref-button
                     1 'help-function-def
                     (intern (format "%S/post-init-%S" layer-sym pkg-symbol))
                     path))))
              (princ " "))
            (princ "\n"))))
      (princ (concat "\nClick [here] to display an Emacs description "
                     "for this package.\n"))
      (with-current-buffer standard-output
        (save-excursion
          (re-search-backward "\\(\\[.+\\]\\)" nil t)
          (help-xref-button 1 'help-package pkg-symbol))))))

(defun configuration-layer//warning (msg &rest args)
  "Display MSG as a warning message in buffer `*Messages*'.
If `configuration-layer--inhibit-warnings' is non nil then this function is a
no-op."
  (unless configuration-layer--inhibit-warnings
    (apply 'spacemacs-buffer/warning msg args)))

(defun configuration-layer//add-layer (layer &optional usedp)
  "Add a LAYER object to the system.
USEDP non-nil means that PKG is a used layer."
  (let ((layer-name (oref layer :name)))
    (puthash layer-name layer configuration-layer--indexed-layers)
    (when usedp
      (add-to-list 'configuration-layer--used-layers layer-name))))

(defun configuration-layer/remove-layers (layer-names)
  "Remove layers with LAYER-NAMES from used layers."
  (mapc 'configuration-layer/remove-layer layer-names))

(defun configuration-layer/remove-layer (layer-name)
  "Remove an used layer with name LAYER-NAME."
  (setq configuration-layer--used-layers
        (delq layer-name configuration-layer--used-layers)))

(defun configuration-layer/get-layer (layer-name)
  "Return a layer object with name LAYER-NAME.
Return nil if layer object is not found."
  (when (ht-contains? configuration-layer--indexed-layers layer-name)
    (ht-get configuration-layer--indexed-layers layer-name)))

(defun configuration-layer/get-layers-list ()
  "Return a list of all discovered layer symbols."
  (ht-keys configuration-layer--indexed-layers))

(defun configuration-layer/get-layer-local-dir (layer)
  "Return the value of SLOT for the given LAYER."
  (let ((obj (ht-get configuration-layer--indexed-layers layer)))
    (when obj (concat (oref obj :dir) "local/"))))

(defun configuration-layer/get-layer-path (layer)
  "Return the path for LAYER symbol."
  (let ((obj (ht-get configuration-layer--indexed-layers layer)))
    (when obj (oref obj :dir))))

(defun configuration-layer//add-package (pkg &optional usedp)
  "Add a PKG object to the system.
USEDP non-nil means that PKG is a used package."
  (let ((pkg-name (oref pkg :name)))
    (puthash pkg-name pkg configuration-layer--indexed-packages)
    (when usedp
      (add-to-list 'configuration-layer--used-packages pkg-name))))

(defun configuration-layer/get-packages-list ()
  "Return a list of all package symbols."
  (ht-keys configuration-layer--indexed-packages))

(defun configuration-layer/get-package (pkg-name)
  "Return a package object with name PKG-NAME.
Return nil if package object is not found."
  (when (ht-contains? configuration-layer--indexed-packages pkg-name)
    (ht-get configuration-layer--indexed-packages pkg-name)))

(defun configuration-layer//sort-packages (packages)
  "Return a sorted list of PACKAGES objects."
  (sort packages (lambda (x y) (string< (symbol-name x) (symbol-name y)))))

(defun configuration-layer/make-all-packages (&optional skip-layer-discovery)
  "Create objects for _all_ packages supported by Spacemacs.
If SKIP-LAYER-DISCOVERY is non-nil then do not check for new layers."
  (let ((all-layers (configuration-layer/get-layers-list))
        (configuration-layer--load-packages-files t)
        (configuration-layer--package-properties-read-onlyp t)
        (configuration-layer--inhibit-warnings t))
    (unless skip-layer-discovery
      (configuration-layer/discover-layers))
    (configuration-layer/declare-layers all-layers)
    (configuration-layer/make-packages-from-layers all-layers)))

(defun configuration-layer/make-packages-from-layers
    (layer-names &optional usedp)
  "Read the package lists of layers with name LAYER-NAMES and create packages.
USEDP if non-nil indicates that made packages are used packages.
DOTFILE if non-nil will process the dotfile `dotspacemacs-additional-packages'
variable as well."
  (dolist (layer-name layer-names)
    (let ((layer (configuration-layer/get-layer layer-name)))
      (dolist (pkg (cfgl-layer-get-packages layer 'with-props))
        (let* ((pkg-name (if (listp pkg) (car pkg) pkg))
               (obj (configuration-layer/get-package pkg-name)))
          (setq obj (configuration-layer/make-package pkg layer-name obj))
          (configuration-layer//add-package
           obj (and (cfgl-package-get-safe-owner obj) usedp)))))))

(defun configuration-layer/make-packages-from-dotfile (&optional usedp)
  "Read the additonal packages declared in the dotfile and create packages.
USEDP if non-nil indicates that made packages are used packages."
  (dolist (pkg dotspacemacs-additional-packages)
    (let* ((pkg-name (if (listp pkg) (car pkg) pkg))
           (obj (configuration-layer/get-package pkg-name)))
      (if obj
          (setq obj (configuration-layer/make-package pkg 'dotfile obj))
        (setq obj (configuration-layer/make-package pkg 'dotfile)))
      (configuration-layer//add-package obj usedp)))
  (dolist (xpkg dotspacemacs-excluded-packages)
    (let ((obj (configuration-layer/get-package xpkg)))
      (unless obj
        (setq obj (configuration-layer/make-package xpkg 'dotfile)))
      (configuration-layer//add-package obj usedp)
      (cfgl-package-set-property obj :excluded t))))

(defun configuration-layer/lazy-install (layer-name &rest props)
  "Configure auto-installation of layer with name LAYER-NAME."
  (declare (indent 1))
  (when (configuration-layer//lazy-install-p layer-name)
    (let ((extensions (spacemacs/mplist-get props :extensions))
          (interpreter (plist-get props :interpreter)))
      (when (configuration-layer/layer-usedp layer-name)
        (let* ((layer (configuration-layer/get-layer layer-name))
               (package-names (when layer (cfgl-layer-owned-packages layer))))
          ;; set lazy install flag for a layer if and only if its owned
          ;; distant packages are all not already installed
          (let ((lazy
                 (or (eq 'all dotspacemacs-enable-lazy-installation)
                     (cl-reduce
                      (lambda (x y) (and x y))
                      (mapcar
                       (lambda (p)
                         (let ((pkg (configuration-layer/get-package p)))
                           (or (not (eq layer-name (car (oref pkg :owners))))
                               (null (package-installed-p
                                      (oref pkg :name))))))
                       package-names)
                      :initial-value t))))
            (oset layer :lazy-install lazy)
            (dolist (pkg-name package-names)
              (let ((pkg (configuration-layer/get-package pkg-name)))
                (cfgl-package-set-property pkg :lazy-install lazy))))))
      ;; configure `auto-mode-alist'
      (dolist (x extensions)
        (let ((ext (car x))
              (mode (cadr x)))
          (add-to-list 'configuration-layer--lazy-mode-alist (cons mode ext))
          (add-to-list
           'auto-mode-alist
           `(,ext . (lambda ()
                      (configuration-layer//auto-mode
                       ',layer-name ',mode))))
          ))
      ;; configure `interpreter-mode-alist'
      (when interpreter
        (let ((regex (car interpreter))
              (mode (cadr interpreter)))
          (add-to-list
           'interpreter-mode-alist
           `(,regex . (lambda () (configuration-layer//auto-mode
                               ',layer-name ',mode)))))))))

(defun configuration-layer//auto-mode (layer-name mode)
  "Auto mode support of lazily installed layers."
  (let ((layer (configuration-layer/get-layer layer-name)))
    (when (or (oref layer :lazy-install)
              (not (configuration-layer/layer-usedp layer-name)))
      (configuration-layer//lazy-install-packages layer-name mode)))
  (when (fboundp mode) (funcall mode)))

(defun configuration-layer/filter-objects (objects ffunc)
  "Return a filtered OBJECTS list where each element satisfies FFUNC."
  (reverse (cl-reduce (lambda (acc x) (if (funcall ffunc x) (push x acc) acc))
                      objects
                      :initial-value nil)))

(defun configuration-layer//get-distant-packages (packages usedp)
  "Return the distant packages (ie to be intalled).
If USEDP is non nil then returns only the used packages; if it is nil then
return both used and unused packages."
  (configuration-layer/filter-objects
   packages
   (lambda (x)
     (let ((pkg (configuration-layer/get-package x)))
       (if pkg
           (and (not (memq (oref pkg :location) '(built-in site local)))
                (not (stringp (oref pkg :location)))
                (or (null usedp)
                    (and (not (null (oref pkg :owners)))
                         (not (oref pkg :excluded))
                         (cfgl-package-enabledp pkg t))))
         (spacemacs-buffer/warning "Cannot find package for %s" x)
         nil)))))

(defun configuration-layer//get-private-layer-dir (name)
  "Return an absolute path to the private configuration layer string NAME."
  (file-name-as-directory
   (concat configuration-layer-private-layer-directory name)))

(defun configuration-layer//copy-template (name template &optional layer-dir)
  "Copy and replace special values of TEMPLATE to layer string NAME.
If LAYER_DIR is nil, the private directory is used."
  (cl-flet ((substitute (old new) (let ((case-fold-search nil))
                                    (save-excursion
                                      (goto-char (point-min))
                                      (while (search-forward old nil t)
                                        (replace-match new t))))))
    (let ((src (concat configuration-layer-template-directory
                       (format "%s.template" template)))
          (dest (if layer-dir
                    (concat layer-dir "/" (format "%s" template))
                  (concat (configuration-layer//get-private-layer-dir name)
                          (format "%s" template)))))
      (copy-file src dest)
      (find-file dest)
      (substitute "%LAYER_NAME%" name)
      (cond
       (user-full-name
        (substitute "%USER_FULL_NAME%" user-full-name)
        (substitute "%USER_MAIL_ADDRESS%" user-mail-address))
       (t
        (substitute "%USER_FULL_NAME%" "Sylvain Benner & Contributors")
        (substitute "%USER_MAIL_ADDRESS%" "sylvain.benner@gmail.com")))
      (save-buffer))))

(defun configuration-layer//directory-type (path)
  "Return the type of directory pointed by PATH.
Possible return values:
  layer    - the directory is a layer
  category - the directory is a category
  nil      - the directory is a regular directory."
  (when (file-directory-p path)
    (if (string-match
         "^+" (file-name-nondirectory
               (directory-file-name
                (concat configuration-layer-directory path))))
        'category
      (let ((files (directory-files path)))
        ;; most frequent files encoutered in a layer are tested first
        (when (or (member "packages.el" files)
                  (member "layers.el" files)
                  (member "config.el" files)
                  (member "keybindings.el" files)
                  (member "funcs.el" files))
          'layer)))))

(defun configuration-layer//get-category-from-path (dirpath)
  "Return a category symbol from the given DIRPATH.
The directory name must start with `+'.
Returns nil if the directory is not a category."
  (when (file-directory-p dirpath)
    (let ((dirname (file-name-nondirectory
                    (directory-file-name
                     (concat configuration-layer-directory
                             dirpath)))))
      (when (string-match "^+" dirname)
        (intern (substring dirname 1))))))

(defun configuration-layer/discover-layers ()
  "Initialize `configuration-layer--indexed-layers' with layer directories."
  ;; load private layers at the end on purpose we asume that the user layers
  ;; must have the final word on configuration choices. Let
  ;; `dotspacemacs-directory' override the private directory if it exists.
  (setq  configuration-layer--indexed-layers (make-hash-table :size 1024))
  (spacemacs-buffer/set-mode-line "Indexing layers...")
  (spacemacs//redisplay)
  (let ((search-paths (append
                       ;; layers shipped with spacemacs
                       (list configuration-layer-directory)
                       ;; layers in private folder ~/.emacs.d/private
                       (list configuration-layer-private-directory)
                       ;; layers in dotdirectory
                       (when dotspacemacs-directory
                         (list (expand-file-name (concat dotspacemacs-directory
                                                         "layers/"))))
                       ;; additional layer directories provided by the user
                       dotspacemacs-configuration-layer-path))
        (discovered '()))
    ;; filter out directories that don't exist
    (setq search-paths (configuration-layer/filter-objects
                        search-paths
                        (lambda (x)
                          (let ((exists (file-exists-p x)))
                            (unless exists
                              (configuration-layer//warning
                               "Layer directory \"%s\" not found. Ignoring it."
                               x))
                            exists))))
    ;; depth-first search of subdirectories
    (while search-paths
      (let ((current-path (car search-paths)))
        (setq search-paths (cdr search-paths))
        (dolist (sub (directory-files current-path t nil 'nosort))
          ;; ignore ".", ".." and non-directories
          (unless (or (string-equal ".." (substring sub -2))
                      (string-equal "." (substring sub -1))
                      (not (file-directory-p sub)))
            (let ((type (configuration-layer//directory-type sub)))
              (cond
               ((eq 'category type)
                (let ((category (configuration-layer//get-category-from-path
                                 sub)))
                  (spacemacs-buffer/message "-> Discovered category: %S"
                                            category)
                  (push category configuration-layer-categories)
                  (setq search-paths (cons sub search-paths))))
               ((eq 'layer type)
                (let* ((layer-name-str (file-name-nondirectory sub))
                       (layer-name (intern layer-name-str))
                       (indexed-layer (configuration-layer/get-layer
                                       layer-name)))
                  (if indexed-layer
                      ;; the same layer may have been discovered twice,
                      ;; in which case we don't need a warning
                      (unless (string-equal
                               (directory-file-name (oref indexed-layer :dir))
                               (directory-file-name sub))
                        (configuration-layer//warning
                         (concat
                          "Duplicated layer %s detected in directory \"%s\", "
                          "replacing old directory \"%s\" with new directory.")
                         layer-name-str sub (oref indexed-layer :dir))
                        (oset indexed-layer :dir sub))
                    (spacemacs-buffer/message
                     "-> Discovered configuration layer: %s" layer-name-str)
                    (let ((configuration-layer--load-packages-files nil))
                      (configuration-layer//add-layer
                       (configuration-layer/make-layer layer-name
                                                       nil nil sub))))))
               (t
                ;; layer not found, add it to search path
                (setq search-paths (cons sub search-paths)))))))))))

(defun configuration-layer/declare-layers (layers-specs)
  "Declare layers with LAYERS-SPECS."
  (mapc 'configuration-layer/declare-layer layers-specs))

(defun configuration-layer/declare-layer (layer-specs)
  "Declare a single layer with spec LAYER-SPECS.
Set the variable `configuration-layer--declared-layers-usedp' to control
wether the declared layer is an used one or not."
  (let* ((layer-name (if (listp layer-specs) (car layer-specs) layer-specs))
         (layer (configuration-layer/get-layer layer-name))
         (usedp configuration-layer--declared-layers-usedp))
    (if layer
        (let ((obj (configuration-layer/make-layer
                    layer-specs
                    (configuration-layer/get-layer layer-name)
                    usedp)))
          (configuration-layer//add-layer obj usedp)
          (configuration-layer//set-layer-variables obj)
          (when (or usedp configuration-layer--load-packages-files)
            (configuration-layer//load-layer-files layer-name '("layers.el"))))
      (configuration-layer//warning "Unknown layer %s declared in dotfile."
                                    layer-name))))

(defun configuration-layer//declare-used-layers (layers-specs)
  "Declare used layers from LAYERS-SPECS list."
  (setq configuration-layer--used-layers nil)
  (let ((configuration-layer--declared-layers-usedp t))
    (unless configuration-layer-exclude-all-layers
      (dolist (layer-specs layers-specs)
        (let* ((layer-name (if (listp layer-specs)
                               (car layer-specs)
                             layer-specs))
               (layer (configuration-layer/get-layer layer-name)))
          (if layer
              (let ((layer-path (oref layer :dir)))
                (unless (string-match-p "+distributions" layer-path)
                  (configuration-layer/declare-layer layer-specs)))
            (configuration-layer//warning
             "Unknown layer %s declared in dotfile." layer-name))))
      (setq configuration-layer--used-layers
            (reverse configuration-layer--used-layers)))
    ;; distribution and bootstrap layers are always first
    (let ((distribution (if configuration-layer-force-distribution
                            configuration-layer-force-distribution
                          dotspacemacs-distribution)))
      (unless (eq 'spacemacs-bootstrap distribution)
        (configuration-layer/declare-layer distribution)))
    (configuration-layer/declare-layer 'spacemacs-bootstrap)))

(defun configuration-layer//set-layers-variables (layers)
  "Set the configuration variables for the passed LAYERS."
  (mapc 'configuration-layer//set-layer-variables layers))

(defun configuration-layer//set-layer-variables (layer)
  "Set the configuration variables for the passed LAYER."
  (let ((variables (oref layer :variables)))
    (while variables
      (let ((var (pop variables)))
        (if (consp variables)
            (condition-case-unless-debug err
                (set-default var (eval (pop variables)))
              ('error
               (configuration-layer//increment-error-count)
               (spacemacs-buffer/append
                (format (concat "\nAn error occurred while setting layer "
                                "variable %s "
                                "(error: %s). Be sure to quote the value "
                                "if needed.\n") var err))))
          (configuration-layer//warning "Missing value for variable %s !"
                                    var))))))

(defun configuration-layer/layer-usedp (layer-name)
  "Return non-nil if LAYER-NAME is the name of a used layer."
  (let ((obj (configuration-layer/get-layer layer-name)))
    (when obj (memq layer-name configuration-layer--used-layers))))

(defun configuration-layer/package-usedp (name)
  "Return non-nil if NAME is the name of a used package."
  (let ((obj (configuration-layer/get-package name)))
    (and obj (cfgl-package-get-safe-owner obj)
         (not (oref obj :excluded)))))

(defun  configuration-layer/package-lazy-installp (name)
  "Return non-nil if NAME is the name of a package to be lazily installed."
  (let ((obj (configuration-layer/get-package name)))
    (when obj (oref obj :lazy-install))))

(defun configuration-layer//configure-layers (layer-names)
  "Configure layers with LAYER-NAMES."
  (let ((warning-minimum-level :error))
    (dolist (layer-name layer-names)
      (configuration-layer//load-layer-files layer-name '("config.el")))))

(defun configuration-layer//declare-used-packages (layers)
  "Declare used packages contained in LAYERS."
  (setq configuration-layer--used-packages nil)
  (let* ((warning-minimum-level :error))
    (configuration-layer/make-packages-from-layers layers t)
    (configuration-layer/make-packages-from-dotfile t)
    (setq configuration-layer--used-packages
          (configuration-layer//sort-packages
           configuration-layer--used-packages))))

(defun configuration-layer//load-layers-files (layer-names files)
  "Load the files of list FILES for all passed LAYER-NAMES."
  (dolist (layer-name layer-names)
    (configuration-layer//load-layer-files layer-name files)))

(defun configuration-layer//load-layer-files (layer-name files)
  "Load the files of list FILES for the layer with the given LAYER-NAME."
  (let ((obj (configuration-layer/get-layer layer-name)))
    (when obj
      (dolist (file files)
        (let ((file (concat (oref obj :dir) file)))
          (if (file-exists-p file) (load file)))))))

(defun configuration-layer/configured-packages-stats (packages)
  "Return a statistics alist regarding the number of configured PACKAGES."
  `((total ,(length packages))
    (elpa ,(length (configuration-layer/filter-objects
                    packages
                    (lambda (x)
                      (let ((pkg (configuration-layer/get-package x)))
                        (eq 'elpa (oref pkg :location)))))))
    (recipe ,(length (configuration-layer/filter-objects
                      packages
                      (lambda (x)
                        (let* ((pkg (configuration-layer/get-package x))
                               (location (oref pkg :location)))
                          (and (listp location)
                               (eq 'recipe (car location))))))))
    (local ,(length (configuration-layer/filter-objects
                     packages
                     (lambda (x)
                       (let ((pkg (configuration-layer/get-package x)))
                         (memq (oref pkg :location) '(local site)))))))
    (built-in ,(length (configuration-layer/filter-objects
                        packages
                        (lambda (x)
                          (let ((pkg (configuration-layer/get-package x)))
                            (eq 'built-in (oref pkg :location)))))))))

(defun configuration-layer//install-package (pkg)
  "Unconditionally install the package PKG."
  (let* ((layer (when pkg (car (oref pkg :owners))))
         (location (when pkg (oref pkg :location)))
         (min-version (when pkg (oref pkg :min-version))))
    (spacemacs-buffer/replace-last-line
     (format "--> installing %s: %s%s... [%s/%s]"
             (if layer "package" "dependency")
             pkg-name (if layer (format "@%S" layer) "")
             installed-count not-inst-count) t)
    (spacemacs//redisplay)
    (unless (package-installed-p pkg-name min-version)
      (condition-case-unless-debug err
          (cond
           ((or (null pkg) (eq 'elpa location))
            (configuration-layer//install-from-elpa pkg-name)
            (when pkg (cfgl-package-set-property pkg :lazy-install nil)))
           ((and (listp location) (eq 'recipe (car location)))
            (configuration-layer//install-from-recipe pkg)
            (cfgl-package-set-property pkg :lazy-install nil))
           (t (configuration-layer//warning "Cannot install package %S."
                                        pkg-name)))
        ('error
         (configuration-layer//increment-error-count)
         (spacemacs-buffer/append
          (format (concat "\nAn error occurred while installing %s "
                          "(error: %s)\n") pkg-name err))
         (spacemacs//redisplay))))))

(defun configuration-layer//lazy-install-p (layer-name)
  "Return non nil if the layer with LAYER-NAME should be lazy installed."
  (or (eq 'all dotspacemacs-enable-lazy-installation)
      (and (memq dotspacemacs-enable-lazy-installation '(unused t))
           (not (configuration-layer/layer-usedp layer-name)))))

(defun configuration-layer//lazy-install-packages (layer-name mode)
  "Install layer with LAYER-NAME to support MODE."
  (when (or (not dotspacemacs-ask-for-lazy-installation)
            (yes-or-no-p (format
                          (concat "Support for %s requires installation of "
                                  "layer %s, do you want to install it?")
                          mode layer-name)))
    (when (dotspacemacs/add-layer layer-name)
      (configuration-layer/sync 'no-install))
    (let* ((layer (configuration-layer/get-layer layer-name))
           (inst-pkgs
            (delq nil
                  (mapcar
                   (lambda (x)
                     (let* ((pkg-name (if (listp x) (car x) x))
                            (pkg (configuration-layer/get-package pkg-name)))
                       (cfgl-package-set-property pkg :lazy-install nil)
                       (when (memq pkg-name
                                   configuration-layer--used-distant-packages)
                         pkg-name)))
                   (oref layer :packages)))))
      (let ((last-buffer (current-buffer))
            (sorted-pkg (configuration-layer//sort-packages inst-pkgs)))
        (spacemacs-buffer/goto-buffer)
        (goto-char (point-max))
        (configuration-layer//install-packages sorted-pkg)
        (configuration-layer//configure-packages sorted-pkg)
        (configuration-layer//load-layer-files layer '("keybindings.el"))
        (oset layer :lazy-install nil)
        (switch-to-buffer last-buffer)))))

(defun configuration-layer//install-packages (packages)
  "Install PACKAGES which are not lazy installed."
  (interactive)
  ;; Force the display of warning buffers at the bottom
  (let ((display-buffer-alist
         '(("\\(\\*Compile-Log\\*\\)\\|\\(\\*Warnings\\*\\)"
            (display-buffer-in-side-window)
            (inhibit-same-window . t)
            (side . bottom)
            (window-height . 0.2)))))
    ;; ensure we have quelpa available first
    (configuration-layer//configure-quelpa)
    (let* ((upkg-names (configuration-layer//get-uninstalled-packages packages))
           (not-inst-count (length upkg-names))
           installed-count)
      ;; installation
      (when upkg-names
        (spacemacs-buffer/set-mode-line "Installing packages...")
        (spacemacs//redisplay)
        (let ((delayed-warnings-backup delayed-warnings-list))
          (spacemacs-buffer/append
           (format "Found %s new package(s) to install...\n"
                   not-inst-count))
          (configuration-layer/retrieve-package-archives)
          (setq installed-count 0)
          (spacemacs//redisplay)
          (dolist (pkg-name upkg-names)
            (setq installed-count (1+ installed-count))
            (configuration-layer//install-package
             (configuration-layer/get-package pkg-name)))
          (spacemacs-buffer/append "\n")
          (unless init-file-debug
            ;; get rid of all delayed warnings when byte-compiling packages
            ;; unless --debug-init was passed on the command line
            (setq delayed-warnings-list delayed-warnings-backup)))))))

(defun configuration-layer//install-from-elpa (pkg-name)
  "Install PKG from ELPA."
  (if (not (assq pkg-name package-archive-contents))
      (spacemacs-buffer/append
       (format (concat "\nPackage %s is unavailable. "
                       "Is the package name misspelled?\n")
               pkg-name))
    (let ((pkg-desc (assq pkg-name package-archive-contents)))
      (dolist
          (dep (configuration-layer//get-package-deps-from-archive
                pkg-name))
        (if (package-installed-p (car dep) (cadr dep))
            (configuration-layer//activate-package (car dep))
          (configuration-layer//install-from-elpa (car dep))))
      (if pkg-desc
          (package-install (cadr pkg-desc))
        (package-install pkg-name)))))

(defun configuration-layer//install-from-recipe (pkg)
  "Install PKG from a recipe."
  (let* ((pkg-name (oref pkg :name))
         (layer (car (oref pkg :owners)))
         (recipe (cons pkg-name (cdr (oref pkg :location)))))
    (if recipe
        (quelpa recipe)
      (configuration-layer//warning
       (concat "Cannot find any recipe for package %S! Be sure "
               "to add a recipe for it in alist %S.")
       pkg-name recipes-var))))

(defun configuration-layer//filter-packages-with-deps
    (pkg-names filter &optional use-archive)
  "Return a filtered PKG-NAMES list where each elements satisfies FILTER."
  (when pkg-names
    (let (result)
      (dolist (pkg-name pkg-names)
        ;; recursively check dependencies
        (let* ((deps
                (if use-archive
                    (configuration-layer//get-package-deps-from-archive
                     pkg-name)
                  (configuration-layer//get-package-deps-from-alist pkg-name)))
               (install-deps
                (when deps (configuration-layer//filter-packages-with-deps
                            (mapcar 'car deps) filter))))
          (when install-deps
            (setq result (append install-deps result))))
        (when (funcall filter pkg-name)
          (add-to-list 'result pkg-name t)))
      (delete-dups result))))

(defun configuration-layer//get-uninstalled-packages (pkg-names)
  "Return a filtered list of PKG-NAMES to install."
  (configuration-layer//filter-packages-with-deps
   pkg-names (lambda (x)
               (let* ((pkg (configuration-layer/get-package x))
                      (min-version (when pkg (oref pkg :min-version))))
                 (not (package-installed-p x min-version))))))

(defun configuration-layer//package-has-recipe-p (pkg-name)
  "Return non nil if PKG-NAME is the name of a package declared with a recipe."
  (when (configuration-layer/get-package pkg-name)
    (let* ((pkg (configuration-layer/get-package pkg-name))
           (location (oref pkg :location)))
      (and (listp location) (eq 'recipe (car location))))))

(defun configuration-layer//get-package-recipe (pkg-name)
  "Return the recipe for PGK-NAME if it has one."
  (let ((pkg (configuration-layer/get-package pkg-name)))
    (when pkg
      (let ((location (oref pkg :location)))
        (when (and (listp location) (eq 'recipe (car location)))
          (cons pkg-name (cdr location)))))))

(defun configuration-layer//new-version-available-p (pkg-name)
  "Return non nil if there is a new version available for PKG-NAME."
  (let ((recipe (configuration-layer//get-package-recipe pkg-name))
        (cur-version (configuration-layer//get-package-version-string pkg-name))
        (quelpa-upgrade-p t)
        new-version)
    (when cur-version
      (setq new-version
            (if recipe
                (or (quelpa-checkout recipe (expand-file-name (symbol-name pkg-name) quelpa-build-dir)) cur-version)
              (configuration-layer//get-latest-package-version-string
               pkg-name)))
      ;; (message "%s: %s > %s ?" pkg-name cur-version new-version)
      (if new-version
          (version< cur-version new-version)
        (cl-pushnew pkg-name
                    configuration-layer--check-new-version-error-packages
                    :test #'eq)
        nil))))

(defun configuration-layer//get-packages-to-update (pkg-names)
  "Return a filtered list of PKG-NAMES to update."
  (configuration-layer//filter-packages-with-deps
   pkg-names 'configuration-layer//new-version-available-p 'use-archive))

(defun configuration-layer//configure-packages (packages)
  "Configure all passed PACKAGES honoring the steps order."
  (setq spacemacs-loading-dots-chunk-threshold
        (/ (length configuration-layer--used-packages)
           spacemacs-loading-dots-chunk-count))
  (spacemacs-buffer/message "+ Configuring bootstrap packages...")
  (configuration-layer//configure-packages-2
   (configuration-layer/filter-objects
    packages (lambda (x)
               (let ((pkg (configuration-layer/get-package x)))
                 (eq 'bootstrap (oref pkg :step))))))
  (spacemacs-buffer/message "+ Configuring pre packages...")
  (configuration-layer//configure-packages-2
   (configuration-layer/filter-objects
    packages (lambda (x)
               (let ((pkg (configuration-layer/get-package x)))
                 (eq 'pre (oref pkg :step))))))
  (spacemacs-buffer/message "+ Configuring packages...")
  (configuration-layer//configure-packages-2
   (configuration-layer/filter-objects
    packages (lambda (x)
               (let ((pkg (configuration-layer/get-package x)))
                 (null (oref pkg :step)))))))

(defun configuration-layer//configure-packages-2 (packages)
  "Configure all passed PACKAGES."
  (dolist (pkg-name packages)
    (spacemacs-buffer/loading-animation)
    (let ((pkg (configuration-layer/get-package pkg-name)))
      (cond
       ((oref pkg :lazy-install)
        (spacemacs-buffer/message
         (format "%S ignored since it can be lazily installed." pkg-name)))
       ((and (oref pkg :excluded)
             (not (oref pkg :protected)))
        (spacemacs-buffer/message
         (format "%S ignored since it has been excluded." pkg-name)))
       ((null (oref pkg :owners))
        (spacemacs-buffer/message
         (format "%S ignored since it has no owner layer." pkg-name)))
       ((not (cfgl-package-enabledp pkg))
        (spacemacs-buffer/message (format "%S is toggled off." pkg-name)))
       (t
        ;; load-path
        (let ((location (oref pkg :location)))
          (cond
           ((stringp location)
            (if (file-directory-p location)
                (push (file-name-as-directory location) load-path)
              (configuration-layer//warning
               "Location path for package %S does not exists (value: %s)."
               pkg location)))
           ((and (eq 'local location)
                 (eq 'dotfile (car (oref pkg :owners))))
            (push (file-name-as-directory
                   (concat configuration-layer-private-directory "local/"
                           (symbol-name (oref pkg :name))))
                  load-path))
           ((eq 'local location)
            (let* ((owner (configuration-layer/get-layer
                           (car (oref pkg :owners))))
                   (dir (when owner (oref owner :dir))))
              (push (format "%slocal/%S/" dir pkg-name) load-path)))))
        ;; configuration
        (unless (memq (oref pkg :location) '(local site built-in))
          (configuration-layer//activate-package pkg-name))
        (cond
         ((eq 'dotfile (car (oref pkg :owners)))
          (spacemacs-buffer/message
           (format "%S is configured in the dotfile." pkg-name)))
         (t
          (configuration-layer//configure-package pkg))))))))

(defun configuration-layer//package-enabled-p (pkg layer)
  "Returns true if PKG should be configured for LAYER.
LAYER must not be the owner of PKG."
  (let* ((owner (configuration-layer/get-layer (car (oref pkg :owners))))
         (disabled (oref owner :disabled-for))
         (enabled (oref owner :enabled-for)))
    (if (not (eq 'unspecified enabled))
        (memq layer enabled)
      (not (memq layer disabled)))))

(defun configuration-layer//configure-package (pkg)
  "Configure PKG object."
  (let* ((pkg-name (oref pkg :name))
         (owner (car (oref pkg :owners))))
    (spacemacs-buffer/message (format "Configuring %S..." pkg-name))
    ;; pre-init
    (mapc
     (lambda (layer)
       (when (configuration-layer/layer-usedp layer)
         (if (not (configuration-layer//package-enabled-p pkg layer))
             (spacemacs-buffer/message
              (format "  -> ignored pre-init (%S)..." layer))
           (spacemacs-buffer/message
            (format "  -> pre-init (%S)..." layer))
           (condition-case-unless-debug err
               (funcall (intern (format "%S/pre-init-%S" layer pkg-name)))
             ('error
              (configuration-layer//increment-error-count)
              (spacemacs-buffer/append
               (format
                (concat "\nAn error occurred while pre-configuring %S "
                        "in layer %S (error: %s)\n")
                pkg-name layer err)))))))
          (oref pkg :pre-layers))
    ;; init
    (spacemacs-buffer/message (format "  -> init (%S)..." owner))
    (funcall (intern (format "%S/init-%S" owner pkg-name)))
    ;; post-init
    (mapc
     (lambda (layer)
       (when (configuration-layer/layer-usedp layer)
         (if (not (configuration-layer//package-enabled-p pkg layer))
             (spacemacs-buffer/message
              (format "  -> ignored post-init (%S)..." layer))
           (spacemacs-buffer/message
            (format "  -> post-init (%S)..." layer))
           (condition-case-unless-debug err
               (funcall (intern (format "%S/post-init-%S" layer pkg-name)))
             ('error
              (configuration-layer//increment-error-count)
              (spacemacs-buffer/append
               (format
                (concat "\nAn error occurred while post-configuring %S "
                        "in layer %S (error: %s)\n")
                pkg-name layer err)))))))
          (oref pkg :post-layers))))

(defun configuration-layer//cleanup-rollback-directory ()
  "Clean up the rollback directory."
  (let* ((dirattrs (delq nil
                         (mapcar (lambda (d)
                                   (unless (eq t d) d))
                                 (directory-files-and-attributes
                                  configuration-layer-rollback-directory
                                  nil "\\`\\(\\.\\{0,2\\}[^.\n].*\\)\\'" t))))
         (dirs (sort dirattrs
                     (lambda (d e)
                       (time-less-p (nth 6 d) (nth 6 e))))))
    (dotimes (c (- (length dirs) dotspacemacs-max-rollback-slots))
      (delete-directory (concat configuration-layer-rollback-directory
                                "/" (car (pop dirs)))
                        t t))))

(defun configuration-layer/update-packages (&optional always-update)
  "Update packages.

If called with a prefix argument ALWAYS-UPDATE, assume yes to update."
  (interactive "P")
  (spacemacs-buffer/insert-page-break)
  (spacemacs-buffer/append "\nUpdating package archives, please wait...\n")
  (configuration-layer/retrieve-package-archives nil 'force)
  (setq configuration-layer--check-new-version-error-packages nil)
  (let* ((update-packages
          (configuration-layer//get-packages-to-update
           configuration-layer--used-distant-packages))
         (skipped-count (length
                         configuration-layer--check-new-version-error-packages))
         (date (format-time-string "%y-%m-%d_%H.%M.%S"))
         (rollback-dir (expand-file-name
                        (concat configuration-layer-rollback-directory
                                (file-name-as-directory date))))
         (upgrade-count (length update-packages))
         (upgraded-count 0)
         (update-packages-alist))
    (when configuration-layer--check-new-version-error-packages
      (spacemacs-buffer/append
       (format (concat "--> Warning: cannot update %s package(s), possibly due"
                       " to a temporary network problem: %s\n")
               skipped-count
               (mapconcat #'symbol-name
                          configuration-layer--check-new-version-error-packages
                          " "))))
    ;; (message "packages to udpate: %s" update-packages)
    (when (> upgrade-count 0)
      (spacemacs-buffer/append
       (format (concat "--> Found %s package(s) to update"
                       (if (> skipped-count 0)
                           (format " (skipped %s):\n" skipped-count)
                         ":\n"))
               upgrade-count) t)
      (mapc (lambda (x)
              (spacemacs-buffer/append
               (format (if (memq (intern x) dotspacemacs-frozen-packages)
                           "%s (won't be updated because package is frozen)\n"
                         "%s\n") x) t))
            (sort (mapcar 'symbol-name update-packages) 'string<))
      (if (and (not always-update)
               (not (yes-or-no-p
                     (format "Do you want to update %s package(s) ? "
                             upgrade-count))))
          (spacemacs-buffer/append "Packages update has been cancelled.\n" t)
        ;; backup the package directory and construct an alist
        ;; variable to be cached for easy update and rollback
        (spacemacs-buffer/append
         "--> performing backup of package(s) to update...\n" t)
        (spacemacs//redisplay)
        (dolist (pkg update-packages)
          (unless (memq pkg dotspacemacs-frozen-packages)
            (let* ((src-dir (configuration-layer//get-package-directory pkg))
                   (dest-dir (expand-file-name
                              (concat rollback-dir
                                      (file-name-as-directory
                                       (file-name-nondirectory src-dir))))))
              (copy-directory src-dir dest-dir 'keeptime 'create 'copy-content)
              (push (cons pkg (file-name-nondirectory src-dir))
                    update-packages-alist))))
        (spacemacs/dump-vars-to-file
         '(update-packages-alist)
         (expand-file-name (concat rollback-dir
                                   configuration-layer-rollback-info)))
        (dolist (pkg update-packages)
          (unless (memq pkg dotspacemacs-frozen-packages)
            (setq upgraded-count (1+ upgraded-count))
            (spacemacs-buffer/replace-last-line
             (format "--> preparing update of package %s... [%s/%s]"
                     pkg upgraded-count upgrade-count) t)
            (spacemacs//redisplay)
            (configuration-layer//package-delete pkg)))
        (spacemacs-buffer/append
         (format "\n--> %s package(s) to be updated.\n" upgraded-count))
        (spacemacs-buffer/append
         (concat "\nEmacs has to be restarted to actually install the "
                 "new version of the packages (SPC q r).\n"))
        (configuration-layer//cleanup-rollback-directory)
        (spacemacs//redisplay)))
    (when (eq upgrade-count 0)
      (spacemacs-buffer/append "--> All packages are up to date.\n")
      (spacemacs//redisplay))))

(defun configuration-layer//ido-candidate-rollback-slot ()
  "Return a list of candidates to select a rollback slot."
  (let ((rolldir configuration-layer-rollback-directory))
    (when (file-exists-p rolldir)
      (reverse
       (delq nil (mapcar
                  (lambda (x)
                    (when (and (file-directory-p (concat rolldir x))
                               (not (or (string= "." x) (string= ".." x))))
                      (let ((p (length (directory-files (file-name-as-directory
                                                         (concat rolldir x))))))
                        ;; -3 for . .. and rollback-info
                        (format "%s (%s packages)" x (- p 3)))))
                  (directory-files rolldir)))))))

(defun configuration-layer/rollback (slot)
  "Rollback all the packages in the given SLOT.
If called interactively and SLOT is nil then an ido buffers appears
to select one."
  (interactive
   (list
    (if (boundp 'slot) slot
      (let ((candidates (configuration-layer//ido-candidate-rollback-slot)))
        (when candidates
          (ido-completing-read "Rollback slots (most recent are first): "
                               candidates))))))
  (spacemacs-buffer/insert-page-break)
  (if (not slot)
      (message "No rollback slot available.")
    (string-match "^\\(.+?\\)\s.*$" slot)
    (let* ((slot-dir (match-string 1 slot))
           (rollback-dir (file-name-as-directory
                          (concat configuration-layer-rollback-directory
                                  (file-name-as-directory slot-dir))))
           (info-file (expand-file-name
                       (concat rollback-dir
                               configuration-layer-rollback-info))))
      (spacemacs-buffer/append
       (format "\nRollbacking ELPA packages from slot %s...\n" slot-dir))
      (load-file info-file)
      (let ((rollback-count (length update-packages-alist))
            (rollbacked-count 0))
        (spacemacs-buffer/append
         (format "Found %s package(s) to rollback...\n" rollback-count))
        (spacemacs//redisplay)
        (dolist (apkg update-packages-alist)
          (let* ((pkg (car apkg))
                 (pkg-dir-name (cdr apkg))
                 (installed-ver
                  (configuration-layer//get-package-version-string pkg))
                 (elpa-dir (file-name-as-directory package-user-dir))
                 (src-dir (expand-file-name
                           (concat rollback-dir (file-name-as-directory
                                                 pkg-dir-name))))
                 (dest-dir (expand-file-name
                            (concat elpa-dir (file-name-as-directory
                                              pkg-dir-name)))))
            (unless (memq pkg dotspacemacs-frozen-packages)
              (setq rollbacked-count (1+ rollbacked-count))
              (if (string-equal (format "%S-%s" pkg installed-ver) pkg-dir-name)
                  (spacemacs-buffer/replace-last-line
                   (format "--> package %s already rolled back! [%s/%s]"
                           pkg rollbacked-count rollback-count) t)
                ;; rollback the package
                (spacemacs-buffer/replace-last-line
                 (format "--> rolling back package %s... [%s/%s]"
                         pkg rollbacked-count rollback-count) t)
                (configuration-layer//package-delete pkg)
                (copy-directory src-dir dest-dir
                                'keeptime 'create 'copy-content)))
            (spacemacs//redisplay)))
        (spacemacs-buffer/append
         (format "\n--> %s packages rolled back.\n" rollbacked-count))
        (spacemacs-buffer/append
         "\nEmacs has to be restarted for the changes to take effect.\n")))))

(defun configuration-layer//activate-package (pkg)
  "Activate PKG."
  (unless (memq pkg package-activated-list)
    (package-activate pkg)))

(defun configuration-layer//get-packages-upstream-dependencies-from-alist ()
  "Returns upsteam dependencies hash map for all packages in `package-alist'.
The keys are package names and the values are lists of package names that
depends on it."
  (let ((result (make-hash-table :size 1024)))
    (dolist (pkg package-alist)
      (let* ((pkg-sym (car pkg))
             (deps (configuration-layer//get-package-deps-from-alist pkg-sym)))
        (dolist (dep deps)
          (let* ((dep-sym (car dep))
                 (value (ht-get result dep-sym)))
            (puthash dep-sym
                     (if value (add-to-list 'value pkg-sym) (list pkg-sym))
                     result)))))
    result))

(defun configuration-layer//get-implicit-packages-from-alist (packages)
  "Returns packages in `packages-alist' which are not found in PACKAGES."
  (let (imp-pkgs)
    (dolist (pkg package-alist)
      (let ((pkg-sym (car pkg)))
        (unless (memq pkg-sym packages)
          (add-to-list 'imp-pkgs pkg-sym))))
    imp-pkgs))

(defun configuration-layer//get-orphan-packages
    (dist-pkgs implicit-pkgs dependencies)
  "Return orphan packages."
  (let (result)
    (dolist (imp-pkg implicit-pkgs)
      (when (configuration-layer//is-package-orphan
             imp-pkg dist-pkgs dependencies)
        (add-to-list 'result imp-pkg)))
    result))

(defun configuration-layer//is-package-orphan (pkg-name dist-pkgs dependencies)
  "Returns not nil if PKG-NAME is the name of an orphan package."
  (unless (or (memq pkg-name dist-pkgs)
              (memq pkg-name configuration-layer--protected-packages))
    (if (ht-contains? dependencies pkg-name)
        (let ((parents (ht-get dependencies pkg-name)))
          (cl-reduce (lambda (x y) (and x y))
                     (mapcar (lambda (p) (configuration-layer//is-package-orphan
                                          p dist-pkgs dependencies))
                             parents)
                     :initial-value t))
      (not (memq pkg-name dist-pkgs)))))

(defun configuration-layer//get-package-directory (pkg-name)
  "Return the directory path for package with name PKG-NAME."
  (let ((pkg-desc (assq pkg-name package-alist)))
    (package-desc-dir (cadr pkg-desc))))

(defun configuration-layer//get-package-deps-from-alist (pkg-name)
  "Return the dependencies alist for package with name PKG-NAME."
  (let ((pkg-desc (assq pkg-name package-alist)))
    (when pkg-desc (package-desc-reqs (cadr pkg-desc)))))

(defun configuration-layer//get-package-deps-from-archive (pkg-name)
  "Return the dependencies alist for a PKG-NAME from the archive data."
  (let* ((pkg-arch (assq pkg-name package-archive-contents))
         (reqs (when pkg-arch (package-desc-reqs (cadr pkg-arch)))))
    ;; recursively get the requirements of reqs
    (dolist (req reqs)
      (let* ((pkg-name2 (car req))
             (reqs2 (configuration-layer//get-package-deps-from-archive
                     pkg-name2)))
        (when reqs2 (setq reqs (append reqs2 reqs)))))
    reqs))

(defun configuration-layer//get-package-version-string (pkg-name)
  "Return the version string for package with name PKG-NAME."
  (let ((pkg-desc (assq pkg-name package-alist)))
    (when pkg-desc
      (package-version-join (package-desc-version (cadr pkg-desc))))))

(defun configuration-layer//get-package-version (pkg-name)
  "Return the version list for package with name PKG-NAME."
  (let ((version-string (configuration-layer//get-package-version-string
                         pkg-name)))
    (unless (string-empty-p version-string)
      (version-to-list version-string))))

(defun configuration-layer//get-latest-package-version-string (pkg-name)
  "Return the version string for package with name PKG-NAME."
  (let ((pkg-arch (assq pkg-name package-archive-contents)))
    (when pkg-arch
      (package-version-join (package-desc-version (cadr pkg-arch))))))

(defun configuration-layer//get-latest-package-version (pkg-name)
  "Return the versio list for package with name PKG-NAME."
  (let ((version-string
         (configuration-layer//get-latest-package-version-string pkg-name)))
    (unless (string-empty-p version-string)
      (version-to-list version-string))))

(defun configuration-layer//package-delete (pkg-name)
  "Delete package with name PKG-NAME."
  (cond
   ((version<= "25.0.50" emacs-version)
    (let ((p (cadr (assq pkg-name package-alist))))
      ;; add force flag to ignore dependency checks in Emacs25
      (when p (package-delete p t t))))
   (t (let ((p (cadr (assq pkg-name package-alist))))
        (when p (package-delete p))))))

(defun configuration-layer/delete-orphan-packages (packages)
  "Delete PACKAGES if they are orphan."
  (interactive)
  (let* ((dependencies
          (configuration-layer//get-packages-upstream-dependencies-from-alist))
         (implicit-packages
          (configuration-layer//get-implicit-packages-from-alist
           packages))
         (orphans (configuration-layer//get-orphan-packages
                   packages
                   implicit-packages
                   dependencies))
         (orphans-count (length orphans))
         deleted-count)
    ;; (message "dependencies: %s" dependencies)
    ;; (message "implicit: %s" implicit-packages)
    ;; (message "orphans: %s" orphans)
    (if orphans
        (progn
          (spacemacs-buffer/set-mode-line "Uninstalling not used packages...")
          (spacemacs//redisplay)
          (spacemacs-buffer/append
           (format "Found %s orphan package(s) to delete...\n"
                   orphans-count))
          (setq deleted-count 0)
          (dolist (orphan orphans)
            (setq deleted-count (1+ deleted-count))
            (spacemacs-buffer/replace-last-line
             (format "--> deleting %s... [%s/%s]"
                     orphan
                     deleted-count
                     orphans-count) t)
            (configuration-layer//package-delete orphan)
            (spacemacs//redisplay))
          (spacemacs-buffer/append "\n"))
      (spacemacs-buffer/message "No orphan package to delete."))))

(defun configuration-layer//gather-auto-mode-extensions (mode)
  "Return a regular expression matching all the extensions associate to MODE."
  (let (gather-extensions)
    (dolist (x auto-mode-alist)
      (let ((ext (car x))
            (auto-mode (cdr x)))
        (when (and (stringp ext)
                   (symbolp auto-mode)
                   (eq auto-mode mode))
          (push (car x) gather-extensions))))
    (when gather-extensions
        (concat "\\("
                (string-join gather-extensions "\\|")
                "\\)"))))

(defun configuration-layer//lazy-install-extensions-for-layer (layer-name)
  "Return an alist of owned modes and extensions for the passed layer."
  (let* ((layer (configuration-layer/get-layer layer-name))
         (package-names (cfgl-layer-owned-packages layer))
         result)
    (dolist (pkg-name package-names)
      (dolist (mode (list pkg-name (intern (format "%S-mode" pkg-name))))
        (let ((ext (configuration-layer//gather-auto-mode-extensions mode)))
          (when ext (push (cons mode ext) result)))))
    result))

(defun configuration-layer//insert-lazy-install-form (layer-name mode ext)
  "Insert a configuration form for lazy installation of MODE."
  (let ((str (concat "(configuration-layer/lazy-install '"
                     (symbol-name layer-name)
                     " :extensions '("
                     (let ((print-quoted t)) (prin1-to-string ext))
                     " "
                     (symbol-name mode)
                     "))\n")))
    (insert str)))

(defun configuration-layer/insert-lazy-install-configuration ()
  "Prompt for a layer and insert the forms to configure lazy installation."
  (interactive)
  (let ((layer-name
         (intern (completing-read
                  "Choose a used layer"
                  (sort configuration-layer--used-layers
                        (lambda (x y)
                          (string< (oref (cdr x) :name)
                                   (oref (cdr y) :name))))))))
    (let ((mode-exts (configuration-layer//lazy-install-extensions-for-layer
                      layer-name)))
      (dolist (x mode-exts)
        (configuration-layer//insert-lazy-install-form
         layer-name (car x) (cdr x))))))

(defvar configuration-layer--spacemacs-startup-time nil
  "Spacemacs full startup duration.")

(defun configuration-layer/display-summary (start-time)
  "Display a summary of loading time."
  (unless configuration-layer--spacemacs-startup-time
    (setq configuration-layer--spacemacs-startup-time
          (float-time (time-subtract (current-time) emacs-start-time))))
  (let ((stats (configuration-layer/configured-packages-stats
                configuration-layer--used-packages)))
    (spacemacs-buffer/insert-page-break)
    (spacemacs-buffer/append
     (format "\n%s packages loaded in %.3fs (e:%s r:%s l:%s b:%s)"
             (cadr (assq 'total stats))
             configuration-layer--spacemacs-startup-time
             (cadr (assq 'elpa stats))
             (cadr (assq 'recipe stats))
             (cadr (assq 'local stats))
             (cadr (assq 'built-in stats))))
    (with-current-buffer (get-buffer-create spacemacs-buffer-name)
      (let ((buffer-read-only nil))
	(spacemacs-buffer//center-line)
	(insert "\n")))))

(defun configuration-layer/load-or-install-protected-package
    (pkg &optional install log file-to-load)
  "Load PKG package, and protect it against being deleted as an orphan.
See `configuration-layer/load-or-install-package' for more information."
  (push pkg configuration-layer--protected-packages)
  (configuration-layer/load-or-install-package pkg log file-to-load))

(defun configuration-layer/load-or-install-package
    (pkg &optional install log file-to-load)
  "Load PKG package. PKG will be installed if it is not already installed.
Whenever the initial require fails the absolute path to the package
directory is returned.
If INSTALL is non-nil then try to install the package if needed.
If LOG is non-nil a message is displayed in spacemacs-buffer-mode buffer.
FILE-TO-LOAD is an explicit file to load after the installation."
  (let ((warning-minimum-level :error))
    (unless (require pkg nil 'noerror)
      ;; not installed, we try to initialize package.el only if required to
      ;; precious seconds during boot time
      (require 'cl)
      (let ((pkg-elpa-dir
             (configuration-layer/get-elpa-package-install-directory pkg)))
        (if pkg-elpa-dir
            (add-to-list 'load-path pkg-elpa-dir)
          ;; install the package
          (when install
            (when log
              (spacemacs-buffer/append
               (format "Installing package: %s...\n" pkg))
              (spacemacs//redisplay))
            (configuration-layer/retrieve-package-archives 'quiet)
            (let ((delayed-warnings-backup delayed-warnings-list))
              (package-install pkg)
              (unless init-file-debug
                (setq delayed-warnings-list delayed-warnings-backup)))
            (setq pkg-elpa-dir
                  (configuration-layer/get-elpa-package-install-directory pkg))))
        (unless (configuration-layer/get-package pkg)
          (let ((obj (configuration-layer/make-package pkg 'system)))
                (configuration-layer//add-package obj)))
        (require pkg nil 'noerror)
        (when file-to-load
          (load-file (concat pkg-elpa-dir file-to-load)))
        pkg-elpa-dir))))

(defun configuration-layer//get-indexed-elpa-package-names ()
  "Return a list of all ELPA packages in indexed packages and dependencies."
  (let (result)
    (dolist (pkg-sym (configuration-layer//get-distant-packages
                      (ht-keys configuration-layer--indexed-packages) nil))
      (when (assq pkg-sym package-archive-contents)
        (let* ((deps (mapcar 'car
                             (configuration-layer//get-package-deps-from-archive
                              pkg-sym)))
               (elpa-deps (configuration-layer/filter-objects
                           deps (lambda (x)
                                  (assq x package-archive-contents)))))
          (dolist (pkg (cons pkg-sym elpa-deps))
            ;; avoid duplicates
            (add-to-list 'result pkg)))))
    result))

(defun configuration-layer//create-archive-contents-item (pkg-name)
  "Return an item with an ELPA archive-contents compliant format."
  (let ((obj (cadr (assq pkg-name package-archive-contents))))
    (cons pkg-name `[,(package-desc-version obj)
                     ,(package-desc-reqs obj)
                     ,(package-desc-summary obj)
                     ,(package-desc-kind obj)
                     ,(package-desc-extras obj)])))

(defun configuration-layer//download-elpa-file
    (pkg-name filename archive-url output-dir
              &optional signaturep readmep)
  "Download FILENAME from distant ELPA repository to OUTPUT-DIR.

Original code from dochang at https://github.com/dochang/elpa-clone"
  (let ((source (concat archive-url filename))
        (target (expand-file-name filename output-dir)))
    (unless (file-exists-p target)
      (let* ((readme-filename (format "%S-readme.txt" pkg-name))
             (source-readme (concat archive-url readme-filename)))
        (when (and readmep (url-http-file-exists-p source-readme))
          (url-copy-file source-readme
                         (expand-file-name readme-filename output-dir)
                         'ok-if-already-exists)))
      (when signaturep
        (let* ((sig-filename (concat filename ".sig"))
               (source-sig (concat archive-url sig-filename))
               (target-sig (expand-file-name sig-filename output-dir)))
          (url-copy-file source-sig target-sig 'ok-if-already-exists)))
      (url-copy-file source target))))

(defun configuration-layer//sync-elpa-packages-files (packages output-dir)
  "Synchronize PACKAGES files from remote ELPA directory to OUTPUT-DIR"
  (message "Synchronizing files in ELPA repository at %s..." output-dir)
  (let (filenames
        (output-filenames (directory-files
                           output-dir nil "\\.\\(el\\|tar\\)$"))
        (pkg-count (length packages))
        (i 1))
    (dolist (pkg-name packages)
      (let* ((obj (cadr (assq pkg-name package-archive-contents)))
             (filename (concat (package-desc-full-name obj)
                               (package-desc-suffix obj)))
             (archive-url (cdr (assq (package-desc-archive obj)
                                     package-archives))))
        (push filename filenames)
        (if (member filename output-filenames)
            (message "[%s/%s] Skip %s..." i pkg-count filename)
          (message "[%s/%s] Download %s..." i pkg-count filename)
          (configuration-layer//download-elpa-file
           pkg-name filename archive-url output-dir))
        (setq i (1+ i))))
    (dolist (ofilename output-filenames)
      (unless (member ofilename filenames)
        (message "Remove outdated %s..." ofilename)
        (delete-file (concat output-dir ofilename))))))

(defun configuration-layer/create-elpa-repository (name output-dir)
  "Create an ELPA repository containing all packages supported by Spacemacs."
  (configuration-layer/make-all-packages 'no-discover)
  (let (package-archive-contents
        (package-archives '(("melpa" . "https://melpa.org/packages/")
                            ("org"   . "https://orgmode.org/elpa/")
                            ("gnu"   . "https://elpa.gnu.org/packages/"))))
    (package-refresh-contents)
    (package-read-all-archive-contents)
    (let* ((packages (configuration-layer//get-indexed-elpa-package-names))
           (archive-contents
            (mapcar 'configuration-layer//create-archive-contents-item
                    packages))
           (path (file-name-as-directory (concat output-dir name))))
      (unless (file-exists-p path) (make-directory path 'create-parents))
      (configuration-layer//sync-elpa-packages-files packages path)
      (push 1 archive-contents)
      (with-current-buffer (find-file-noselect
                            (concat path "archive-contents"))
        (erase-buffer)
        (prin1 archive-contents (current-buffer))
        (save-buffer)))))

(defun configuration-layer//package-install-org (func &rest args)
  "Advice around `package-install' to patch package name and dependencies at
install time in order to replace all `org' package installation by
`org-plus-contrib'. We avoid installing unecessarily both `org' and
`org-plus-contrib' at the same time (i.e. we always install `org-plus-contrib')"
  (let* ((pkg (car args))
         (patched
          (cond
           ;; patch symbol name
           ((and (symbolp pkg) (eq 'org pkg))
            (setcar args 'org-plus-contrib)
            t)
           ;; patch name in package-desc object
           ((and (package-desc-p pkg)
                 (eq 'org (package-desc-name pkg)))
            (setf (package-desc-name pkg) 'org-plus-contrib)
            t)
           ;; patch dependencies in package-desc object
           ((and (package-desc-p pkg)
                 (assq 'org (package-desc-reqs pkg)))
            (setf (car (assq 'org (package-desc-reqs pkg))) 'org-plus-contrib)
            t))))
    (let ((name (if (package-desc-p pkg)
                    (package-desc-name pkg)
                  pkg)))
      ;; check manually if `org-plus-contrib' is already installed since
      ;; package.el may install `org-plus-contrib' more than once.
      ;; Maybe we could hook somewhere else (at transaction computation time?)
      (if (or patched (eq 'org-plus-contrib name))
          (unless (package-installed-p name)
            (apply func args))
        (apply func args)))))
(advice-add 'package-install :around #'configuration-layer//package-install-org)

(defun configuration-layer//increment-error-count ()
  "Increment the error counter."
  (if configuration-layer-error-count
      (setq configuration-layer-error-count
            (1+ configuration-layer-error-count))
    (setq configuration-layer-error-count 1)))

(provide 'core-configuration-layer)

;;; core-configuration-layer.el ends here
