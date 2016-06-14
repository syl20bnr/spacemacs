;;; core-configuration-layer.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
  (expand-file-name (concat user-emacs-directory "layers/"))
  "Spacemacs contribution layers base directory.")

(defconst configuration-layer-private-directory
  (expand-file-name (concat user-emacs-directory "private/"))
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

(defconst configuration-layer-rollback-directory
  (expand-file-name (concat spacemacs-cache-directory ".rollback/"))
  "Spacemacs rollback directory.")

(defconst configuration-layer-rollback-info "rollback-info"
  "Spacemacs rollback information file.")

(defclass cfgl-layer ()
  ((name :initarg :name
         :type symbol
         :documentation "Name of the layer.")
   (dir :initarg :dir
        :type string
        :documentation "Absolute path to the layer directory.")
   (packages :initarg :packages
             :initform nil
             :type list
             :documentation "List of package symbols declared in this layer.")
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
             :documentation "A list of layer where this layer is disabled."))
  "A configuration layer.")

(defmethod cfgl-layer-owned-packages ((layer cfgl-layer))
  "Return the list of owned packages by LAYER.
LAYER has to be installed for this method to work properly."
  (delq nil (mapcar
             (lambda (x)
               (let ((pkg (object-assoc x :name configuration-layer--packages)))
                 (when (and pkg (eq (oref layer :name) (car (oref pkg :owners))))
                   pkg)))
             (oref layer :packages))))

(defmethod cfgl-layer-owned-packages ((layer nil))
  "Accept nil as argument and return nil."
  nil)

(defclass cfgl-package ()
  ((name :initarg :name
         :type symbol
         :documentation "Name of the package.")
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

(defmethod cfgl-package-enabledp ((pkg cfgl-package))
  "Evaluate the `toggle' slot of passed PKG."
  (let ((toggle (oref pkg :toggle))) (eval toggle)))

(defmethod cfgl-package-get-safe-owner ((pkg cfgl-package))
  "Safe method to return the name of the layer which owns PKG."
  ;; The owner of a package is the first *used* layer in `:owners' slot.
  ;; Note: for packages in `configuration-layer--packages' the owner is always
  ;;       the car of the `:owners' slot.
  ;;       An example where it is not necessarily the case is in
  ;;       `helm-spacemacs-help-all-packages' which we can find in the helm
  ;;       layer.
  ;; For performance reason `cfgl-package-get-safe-owner' is not used in the
  ;; layer system itself. This functions should be only used outside of the
  ;; system in order to safely get the true owner of a layer.
  (let ((layers (oref pkg :owners)))
    (while (and (consp layers)
                (not (configuration-layer/layer-usedp (car layers))))
      (pop layers))
    (when (configuration-layer/layer-usedp (car layers))
      (car layers))))

(defvar configuration-layer--elpa-archives
  '(("melpa" . "melpa.org/packages/")
    ("org"   . "orgmode.org/elpa/")
    ("gnu"   . "elpa.gnu.org/packages/"))
  "List of ELPA archives required by Spacemacs.")

(defvar configuration-layer-no-layer nil
  "If non nil then only the distribution layer is loaded.")

(defvar configuration-layer-distribution nil
  "If set, bypass the user's choice `dotspacemacs-distribution'.")

(defvar configuration-layer--package-archives-refreshed nil
  "Non nil if package archives have already been refreshed.")

(defvar configuration-layer--layers '()
  "A non-sorted list of `cfgl-layer' objects.")

(defvar configuration-layer--delayed-layers '()
  "A list of layers to check again after first pass of package declaration.")

(defvar configuration-layer--packages '()
  "An alphabetically sorted list of `cfgl-package' objects.")

(defvar configuration-layer--used-distant-packages '()
  "A list of all distant packages that are effectively used.")

(defvar configuration-layer--skipped-packages nil
  "A list of all packages that were skipped during last update attempt.")

(defvar configuration-layer--protected-packages nil
  "A list of packages that will be protected from removal as orphans.")

(defvar configuration-layer--lazy-mode-alist nil
  "Association list where the key is a mode and the value a regexp.")

(defvar configuration-layer--inhibit-warnings nil
  "If non-nil then warning message emitted by the layer system are ignored.")

(defvar configuration-layer-error-count nil
  "Non nil indicates the number of errors occurred during the
installation of initialization.")

(defvar configuration-layer-paths (make-hash-table :size 256)
  "Hash table of layers locations. The key is a layer symbol and the value is
the path for this layer.")

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
    (setq package-archives (configuration-layer//resolve-package-archives
                            configuration-layer--elpa-archives))
    ;; optimization, no need to activate all the packages so early
    (setq package-enable-at-startup nil)
    (package-initialize 'noactivate)
    ;; TODO remove the following hack when 24.3 support ends
    ;; Emacs 24.3 and above ships with python.el but in some Emacs 24.3.1
    ;; packages for Ubuntu, python.el seems to be missing.
    ;; This hack adds marmalade repository for this case only.
    (unless (or (package-installed-p 'python) (version< emacs-version "24.3"))
      (add-to-list 'package-archives
                   '("marmalade" . "https://marmalade-repo.org/packages/")))))

(defun configuration-layer//install-quelpa ()
  "Install `quelpa'."
  (setq quelpa-verbose init-file-debug
        quelpa-dir (concat spacemacs-cache-directory "quelpa/")
        quelpa-build-dir (expand-file-name "build" quelpa-dir)
        quelpa-persistent-cache-file (expand-file-name "cache" quelpa-dir)
        quelpa-update-melpa-p nil)
  (configuration-layer/load-or-install-protected-package 'package-build)
  (configuration-layer/load-or-install-protected-package 'quelpa))

(defun configuration-layer//resolve-package-archives (archives)
  "Resolve HTTP handlers for each archive in ARCHIVES and return a list
of all reachable ones.
If the address of an archive already contains the protocol then this address is
left untouched.
The returned list has a `package-archives' compliant format."
  (mapcar
   (lambda (x)
     (cons (car x)
           (if (string-match-p "http" (cdr x))
               (cdr x)
             (concat (if (and dotspacemacs-elpa-https
                              (not spacemacs-insecure)
                              ;; for now org ELPA repository does
                              ;; not support HTTPS
                              ;; TODO when org ELPA repo support
                              ;; HTTPS remove the check
                              ;; `(not (equal "org" (car x)))'
                              (not (equal "org" (car x))))
                         "https://"
                       "http://") (cdr x)))))
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
        (unless quiet
          (spacemacs-buffer/replace-last-line
           (format "--> refreshing package archive: %s... [%s/%s]"
                   (car archive) i count) t))
        (spacemacs//redisplay)
        (setq i (1+ i))
        (unless (eq 'error
                    (with-timeout
                        (dotspacemacs-elpa-timeout
                         (progn
                           (display-warning
                            'spacemacs
                            (format
                             "\nError connection time out for %s repository!"
                             (car archive)) :warning)
                           'error))
                      (condition-case err
                          (url-retrieve-synchronously (cdr archive))
                        ('error
                         (display-warning 'spacemacs
                          (format
                           "\nError while contacting %s repository!"
                           (car archive)) :warning)
                         'error))))
          (let ((package-archives (list archive)))
            (package-refresh-contents))))
      (package-read-all-archive-contents)
      (unless quiet (spacemacs-buffer/append "\n")))))

(defun configuration-layer/sync (&optional no-install)
  "Synchronize declared layers in dotfile with spacemacs.
If NO-INSTALL is non nil then install steps are skipped."
  (dotspacemacs|call-func dotspacemacs/layers "Calling dotfile layers...")
  (when (spacemacs-buffer//choose-banner)
    (spacemacs-buffer//inject-version))
  ;; first, declare layer then package as soon as possible to
  ;; resolve usage and ownership (in other words, get the list of used
  ;; layers and packages as soon as possible)
  (configuration-layer//declare-layers)
  (configuration-layer//declare-packages configuration-layer--layers)
  ;; then load the functions and finally configure the layers
  (configuration-layer//load-layers-files configuration-layer--layers
                                          '("funcs.el"))
  (configuration-layer//configure-layers configuration-layer--layers)
  ;; pre-filter some packages to save some time later in the loading process
  (setq configuration-layer--used-distant-packages
        (configuration-layer//get-distant-packages
         configuration-layer--packages t))
  ;; install/uninstall packages
  (configuration-layer/load-auto-layer-file)
  (unless no-install
    (if (eq 'all dotspacemacs-download-packages)
        (configuration-layer//install-packages
         (configuration-layer//get-distant-packages
          (configuration-layer/get-all-packages) nil))
      (configuration-layer//install-packages
       (configuration-layer/filter-objects
        configuration-layer--used-distant-packages
        (lambda (x)
          (not (oref x :lazy-install))))))
    (configuration-layer//configure-packages configuration-layer--packages)
    (configuration-layer//load-layers-files
     configuration-layer--layers '("keybindings.el"))
    (when (and (eq 'used dotspacemacs-download-packages)
               (not configuration-layer-distribution)
               (not configuration-layer-no-layer))
      (configuration-layer/delete-orphan-packages
       configuration-layer--packages))))

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

(defun configuration-layer/make-layer (layer)
  "Return a `cfgl-layer' object based on LAYER."
  (let* ((name-sym (if (listp layer) (car layer) layer))
         (name-str (symbol-name name-sym))
         (base-dir (configuration-layer/get-layer-path name-sym))
         (disabled (when (listp layer)
                     (spacemacs/mplist-get layer :disabled-for)))
         (variables (when (listp layer)
                      (spacemacs/mplist-get layer :variables))))
    (if base-dir
        (let* ((dir (format "%s%s/" base-dir name-str)))
          (cfgl-layer name-str
                      :name name-sym
                      :dir dir
                      :disabled-for disabled
                      :variables variables))
      (configuration-layer//warning "Cannot find layer %S !" name-sym)
      nil)))

(defun configuration-layer//make-layers (symbols)
  "Make `cfgl-layer' objects from the passed layer SYMBOLS."
  (delq nil (mapcar 'configuration-layer/make-layer symbols)))

(defun configuration-layer/make-package (pkg &optional obj togglep)
  "Return a `cfgl-package' object based on PKG.
If OBJ is non nil then copy PKG properties into OBJ, otherwise create
a new object.
Properties that can be copied are `:location', `:step' and `:excluded'.
If TOGGLEP is non nil then `:toggle' parameter is ignored."
  (let* ((name-sym (if (listp pkg) (car pkg) pkg))
         (name-str (symbol-name name-sym))
         (location (when (listp pkg) (plist-get (cdr pkg) :location)))
         (step (when (listp pkg) (plist-get (cdr pkg) :step)))
         (excluded (when (listp pkg) (plist-get (cdr pkg) :excluded)))
         (toggle (when (and togglep (listp pkg)) (plist-get (cdr pkg) :toggle)))
         (protected (when (listp pkg) (plist-get (cdr pkg) :protected)))
         (copyp (not (null obj)))
         (obj (if obj obj (cfgl-package name-str :name name-sym))))
    (when location (oset obj :location location))
    (when step (oset obj :step step))
    (oset obj :excluded excluded)
    (when toggle (oset obj :toggle toggle))
    ;; cannot override protected packages
    (unless copyp
      ;; a bootstrap package is protected
      (oset obj :protected (or protected (eq 'bootstrap step)))
      (when protected
        (push name-sym configuration-layer--protected-packages)))
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
          (completing-read
           "Package: "
           (mapcar (lambda (pkg) (symbol-name (oref pkg :name)))
                   ;; TODO make it work with `configuration-layer/get-all-packages'
                   configuration-layer--packages)))))
  (let* ((pkg (object-assoc pkg-symbol
                            :name (or pkg-list configuration-layer--packages)))
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
        (let* ((layer (object-assoc
                       owner :name (or layer-list configuration-layer--layers)))
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
        (princ (if (cfgl-package-enabledp pkg) "on" "off"))
        (princ " because the following expression evaluates to ")
        (princ (if (cfgl-package-enabledp pkg) "t:\n" "nil:\n"))
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
            (let* ((layer (object-assoc
                           owner :name (or layer-list
                                           configuration-layer--layers)))
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
              (let* ((layer (object-assoc
                             layer-sym
                             :name (or layer-list configuration-layer--layers)))
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
              (let* ((layer (object-assoc
                             layer-sym
                             :name (or layer-list configuration-layer--layers)))
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

(defun configuration-layer/get-packages (layers &optional dotfile)
  "Read the package lists of LAYERS and dotfile and return a list of packages."
  (dolist (layer layers)
    (let* ((layer-name (oref layer :name))
           (layer-dir (oref layer :dir))
           (packages-file (concat layer-dir "packages.el")))
      ;; packages
      (when (file-exists-p packages-file)
        ;; required for lazy-loading of unused layers
        ;; for instance for helm-spacemacs-help
        (eval `(defvar ,(intern (format "%S-packages" layer-name)) nil))
        (load packages-file)
        (dolist (pkg (symbol-value (intern (format "%S-packages"
                                                   layer-name))))
          (let* ((pkg-name (if (listp pkg) (car pkg) pkg))
                 (init-func (intern (format "%S/init-%S"
                                            layer-name pkg-name)))
                 (pre-init-func (intern (format "%S/pre-init-%S"
                                                layer-name pkg-name)))
                 (post-init-func (intern (format "%S/post-init-%S"
                                                 layer-name pkg-name)))
                 (ownerp (fboundp init-func))
                 (obj (object-assoc pkg-name
                                    :name configuration-layer--packages)))
            (cl-pushnew pkg-name (oref layer :packages))
            (if obj
                (setq obj (configuration-layer/make-package pkg obj ownerp))
              (setq obj (configuration-layer/make-package pkg nil ownerp))
              (push obj configuration-layer--packages))
            (when ownerp
              ;; last owner wins over the previous one,
              ;; still warn about mutliple owners
              (when (and (oref obj :owners)
                         (not (memq layer-name (oref obj :owners))))
                (configuration-layer//warning
                 (format (concat "More than one init function found for "
                                 "package %S. Previous owner was %S, "
                                 "replacing it with layer %S.")
                         pkg-name (car (oref obj :owners)) layer-name)))
              (push layer-name (oref obj :owners)))
            ;; if no function at all is found for the package, then check
            ;; again this layer later to resolve `package-usedp'  usage in
            ;; `packages.el' files
            (unless (or ownerp
                        (fboundp pre-init-func)
                        (fboundp post-init-func)
                        (oref obj :excluded))
              (unless (object-assoc layer-name :name
                                    configuration-layer--delayed-layers)
                (configuration-layer//warning
                 (format (concat "package %s not initialized in layer %s, "
                                 "you may consider removing this package from "
                                 "the package list or use the :toggle keyword "
                                 "instead of a `when' form.")
                         pkg-name layer-name))
                (push layer configuration-layer--delayed-layers)))
            ;; check if toggle can be applied
            (when (and (not ownerp)
                       (listp pkg)
                       (spacemacs/mplist-get pkg :toggle))
              (configuration-layer//warning
               (format (concat "Ignoring :toggle for package %s because "
                               "layer %S does not own it.")
                       pkg-name layer-name)))
            (when (fboundp pre-init-func)
              (push layer-name (oref obj :pre-layers)))
            (when (fboundp post-init-func)
              (push layer-name (oref obj :post-layers))))))))
  ;; additional and excluded packages from dotfile
  (when dotfile
    (dolist (pkg dotspacemacs-additional-packages)
      (let* ((pkg-name (if (listp pkg) (car pkg) pkg))
             (obj (object-assoc pkg-name :name configuration-layer--packages)))
        (if obj
            (setq obj (configuration-layer/make-package pkg obj t))
          (setq obj (configuration-layer/make-package pkg nil t))
          (push obj configuration-layer--packages)
          (push 'dotfile (oref obj :owners)))))
    (dolist (xpkg dotspacemacs-excluded-packages)
      (let ((obj (object-assoc xpkg :name configuration-layer--packages)))
        (unless obj
          (setq obj (configuration-layer/make-package xpkg))
          (push obj configuration-layer--packages))
        (oset obj :excluded t)))))

(defun configuration-layer/get-all-packages ()
  "Return a list of _all_ packages."
  (let (configuration-layer--packages)
    (configuration-layer/get-packages
     (mapcar 'configuration-layer/make-layer
             (configuration-layer/get-layers-list)))
    configuration-layer--packages))

(defun configuration-layer//sort-packages (packages)
  "Return a sorted list of PACKAGES objects."
  (sort packages (lambda (x y) (string< (symbol-name (oref x :name))
                                        (symbol-name (oref y :name))))))

(defun configuration-layer/lazy-install (layer-name &rest props)
  "Configure auto-installation of layer with name LAYER-NAME."
  (declare (indent 1))
  (when (configuration-layer//lazy-install-p layer-name)
    (let ((extensions (spacemacs/mplist-get props :extensions)))
      (when (configuration-layer/layer-usedp layer-name)
        (let* ((layer (object-assoc layer-name
                                    :name configuration-layer--layers))
               (packages (when layer (cfgl-layer-owned-packages layer))))
          ;; set lazy install flag for a layer if and only if its owned
          ;; distant packages are all not already installed
          (let ((lazy (cl-reduce
                       (lambda (x y) (and x y))
                       (mapcar (lambda (p)
                                 (or (not (eq layer-name (car (oref p :owners))))
                                     (null (package-installed-p
                                            (oref p :name)))))
                               (configuration-layer//get-distant-packages
                                packages t))
                       :initial-value t)))
            (oset layer :lazy-install lazy)
            (dolist (pkg packages)
              (oset pkg :lazy-install lazy)))))
      (dolist (x extensions)
        (let ((ext (car x))
              (mode (cadr x)))
          (add-to-list 'configuration-layer--lazy-mode-alist (cons mode ext))
          (add-to-list
           'auto-mode-alist
           `(,ext . (lambda ()
                      (configuration-layer//auto-mode
                       ',layer-name ',mode)))))))))

(defun configuration-layer//auto-mode (layer-name mode)
  "Auto mode support of lazily installed layers."
  (let ((layer (object-assoc layer-name :name configuration-layer--layers)))
    (when (or (null layer)
              (oref layer :lazy-install))
      (configuration-layer//lazy-install-packages layer-name mode)))
  (when (fboundp mode) (funcall mode)))

(defun configuration-layer/filter-objects (objects ffunc)
  "Return a filtered OBJECTS list where each element satisfies FFUNC."
  (reverse (cl-reduce (lambda (acc x)
                     (if (funcall ffunc x) (push x acc) acc))
                   objects
                   :initial-value nil)))

(defun configuration-layer//get-distant-packages (packages usedp)
  "Return the distant packages (ie to be intalled).
If USEDP is not nil then only return only the used packages, if it is nil then
return both used and unused packages."
  (configuration-layer/filter-objects
   packages (lambda (x)
              (and (not (memq (oref x :location) '(built-in site local)))
                   (not (stringp (oref x :location)))
                   (or (null usedp)
                       (and (not (null (oref x :owners)))
                            (cfgl-package-enabledp x)
                            (not (oref x :excluded))))))))

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

(defun configuration-layer//discover-layers ()
  "Return a hash table where the key is the layer symbol and the value is its
path."
  ;; load private layers at the end on purpose we asume that the user layers
  ;; must have the final word on configuration choices. Let
  ;; `dotspacemacs-directory' override the private directory if it exists.
  (let ((search-paths (append (list configuration-layer-directory)
                              dotspacemacs-configuration-layer-path
                              (list configuration-layer-private-layer-directory)
                              (when dotspacemacs-directory
                                (list dotspacemacs-directory))))
        (discovered '())
        (result (make-hash-table :size 256)))
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
                (let ((layer-name (file-name-nondirectory sub))
                      (layer-dir (file-name-directory sub)))
                  (spacemacs-buffer/message
                   "-> Discovered configuration layer: %s" layer-name)
                  (push (cons (intern layer-name) layer-dir) discovered)))
               (t
                ;; layer not found, add it to search path
                (setq search-paths (cons sub search-paths)))))))))
    ;; add discovered layers to hash table
    (mapc (lambda (l)
            (if (ht-contains? result (car l))
                ;; the same layer may have been discovered twice,
                ;; in which case we don't need a warning
                (unless (string-equal (ht-get result (car l)) (cdr l))
                  (configuration-layer//warning
                   (concat "Duplicated layer %s detected in directory \"%s\""
                           ", keeping only the layer in directory \"%s\"")
                   (car l) (cdr l) (ht-get result (car l))))

              (puthash (car l) (cdr l) result)))
          discovered)
    result))

(defun configuration-layer//declare-layers ()
  "Declare default layers and user layers declared in the dotfile."
  (setq configuration-layer--layers nil)
  (setq configuration-layer-paths (configuration-layer//discover-layers))
  (unless configuration-layer-no-layer
    (dolist (layer dotspacemacs-configuration-layers)
      (let* ((layer-name (if (listp layer) (car layer) layer))
             (layer-path (ht-get configuration-layer-paths layer-name)))
        (if (stringp layer-path)
            (unless (string-match-p "+distributions" layer-path)
              (configuration-layer/declare-layer layer))
          (configuration-layer//warning "Unknown layer %s declared in dotfile."
                                    layer-name))))
    (setq configuration-layer--layers (reverse configuration-layer--layers)))
  ;; distribution and bootstrap layers are always first
  (let ((distribution (if configuration-layer-distribution
                          configuration-layer-distribution
                        dotspacemacs-distribution)))
    (unless (eq 'spacemacs-bootstrap distribution)
      (configuration-layer/declare-layer distribution)))
  (configuration-layer/declare-layer 'spacemacs-bootstrap))

(defun configuration-layer/declare-layers (layer-names)
  "Add layers with LAYER-NAMES to used layers."
  (mapc 'configuration-layer/declare-layer layer-names))

(defun configuration-layer/declare-layer (layer)
  "Declare a single layer."
  (let ((layer-name (if (listp layer) (car layer) layer)))
    (unless (object-assoc layer-name :name configuration-layer--layers)
      (if (ht-contains? configuration-layer-paths layer-name)
          (let ((new-layer (configuration-layer/make-layer layer)))
            (push new-layer configuration-layer--layers)
            (configuration-layer//set-layer-variables new-layer)
            (configuration-layer//load-layer-files new-layer '("layers.el")))
        (configuration-layer//warning "Unknown layer %s declared in dotfile."
                                  layer-name)))))

(defun configuration-layer/remove-layers (layer-names)
  "Remove layers with LAYER-NAMES from used layers."
  (mapc 'configuration-layer/remove-layer layer-names))

(defun configuration-layer/remove-layer (layer-name)
  "Remove an used layer with name LAYER-NAME."
  (setq configuration-layer--layers
        (delete (object-assoc layer-name :name configuration-layer--layers)
                configuration-layer--layers)))

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

(defun configuration-layer/layer-usedp (name)
  "Return non-nil if NAME is the name of a used layer."
  (not (null (object-assoc name :name configuration-layer--layers))))

(defun  configuration-layer/layer-lazy-installp (name)
  "Return non-nil if NAME is the name of a layer to be lazily installed."
  (let ((obj (object-assoc name :name configuration-layer--layers)))
    (when obj (oref obj :lazy-install))))

(defun configuration-layer/package-usedp (name)
  "Return non-nil if NAME is the name of a used package."
  (let ((obj (object-assoc name :name configuration-layer--packages)))
    (when (and obj (not (oref obj :excluded)))
      (not (null (oref obj :owners))))))

(defun  configuration-layer/package-lazy-installp (name)
  "Return non-nil if NAME is the name of a package to be lazily installed."
  (let ((obj (object-assoc name :name configuration-layer--packages)))
    (when obj (oref obj :lazy-install))))

(defun configuration-layer//configure-layers (layers)
  "Configure LAYERS."
  (let ((warning-minimum-level :error))
    (dolist (l layers)
      (configuration-layer//load-layer-files l '("config.el")))))

(defun configuration-layer//declare-packages (layers)
  "Declare packages contained in LAYERS in `configuration-layer--packages'."
  (setq configuration-layer--packages nil)
  (let* ((warning-minimum-level :error))
    ;; first pass
    (configuration-layer/get-packages layers t)
    ;; second pass to resolve package-usedp calls
    (configuration-layer/get-packages configuration-layer--delayed-layers)
    (setq configuration-layer--packages
          (configuration-layer//sort-packages configuration-layer--packages))))

(defun configuration-layer//load-layers-files (layers files)
  "Load the files of list FILES for all passed LAYERS."
  (dolist (layer layers)
    (configuration-layer//load-layer-files layer files)))

(defun configuration-layer//load-layer-files (layer files)
  "Load the files of list FILES for the given LAYER."
  (dolist (file files)
    (let ((file (concat (oref layer :dir) file)))
      (if (file-exists-p file) (load file)))))

(defun configuration-layer/configured-packages-stats (packages)
  "Return a statistics alist regarding the number of configured PACKAGES."
  `((total ,(length packages))
    (elpa ,(length (configuration-layer/filter-objects
                    packages (lambda (x)
                               (eq 'elpa (oref x :location))))))
    (recipe ,(length (configuration-layer/filter-objects
                      packages
                      (lambda (x)
                        (let ((location (oref x :location)))
                          (and (listp location)
                               (eq 'recipe (car location))))))))
    (local ,(length (configuration-layer/filter-objects
                     packages (lambda (x)
                                (memq (oref x :location) '(local site))))))
    (built-in ,(length (configuration-layer/filter-objects
                        packages (lambda (x)
                                   (eq 'built-in (oref x :location))))))))

(defun configuration-layer//install-package (pkg)
  "Unconditionally install the package PKG."
  (let* ((layer (when pkg (car (oref pkg :owners))))
         (location (when pkg (oref pkg :location))))
    (spacemacs-buffer/replace-last-line
     (format "--> installing %s: %s%s... [%s/%s]"
             (if layer "package" "dependency")
             pkg-name (if layer (format "@%S" layer) "")
             installed-count not-inst-count) t)
    (spacemacs//redisplay)
    (unless (package-installed-p pkg-name)
      (condition-case-unless-debug err
          (cond
           ((or (null pkg) (eq 'elpa location))
            (configuration-layer//install-from-elpa pkg-name)
            (when pkg (oset pkg :lazy-install nil)))
           ((and (listp location) (eq 'recipe (car location)))
            (configuration-layer//install-from-recipe pkg)
            (oset pkg :lazy-install nil))
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
    (let* ((layer (object-assoc layer-name :name configuration-layer--layers))
           (inst-pkgs
            (delq nil (mapcar
                       (lambda (x)
                         (object-assoc
                          x :name configuration-layer--used-distant-packages))
                       (oref layer :packages))))
           (config-pkgs
            (delq nil (mapcar
                       (lambda (x)
                         (let ((pkg (object-assoc
                                     x :name configuration-layer--packages)))
                           (oset pkg :lazy-install nil)
                           pkg))
                       (oref layer :packages)))))
      (let ((last-buffer (current-buffer))
            (sorted-inst (configuration-layer//sort-packages inst-pkgs))
            (sorted-config (configuration-layer//sort-packages config-pkgs)))
        (spacemacs-buffer/goto-buffer)
        (goto-char (point-max))
        (oset layer :lazy-install nil)
        (configuration-layer//install-packages sorted-inst)
        (configuration-layer//configure-packages sorted-config)
        (configuration-layer//load-layer-files layer '("keybindings.el"))
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
    (configuration-layer//install-quelpa)
    (let* ((not-inst-pkg-names
            (configuration-layer//get-uninstalled-packages
             (mapcar 'car (object-assoc-list :name packages))))
           (not-inst-count (length not-inst-pkg-names))
           installed-count)
      ;; installation
      (when not-inst-pkg-names
        (spacemacs-buffer/append
         (format "Found %s new package(s) to install...\n"
                 not-inst-count))
        (configuration-layer/retrieve-package-archives)
        (setq installed-count 0)
        (spacemacs//redisplay)
        (dolist (pkg-name not-inst-pkg-names)
          (setq installed-count (1+ installed-count))
          (configuration-layer//install-package
           (object-assoc pkg-name :name packages)))
        (spacemacs-buffer/append "\n")))))

(defun configuration-layer//install-from-elpa (pkg-name)
  "Install PKG from ELPA."
  (if (not (assq pkg-name package-archive-contents))
      (spacemacs-buffer/append
       (format (concat "\nPackage %s is unavailable. "
                       "Is the package name misspelled?\n")
               pkg-name))
    (dolist
        (dep (configuration-layer//get-package-deps-from-archive
              pkg-name))
      (if (package-installed-p (car dep))
          (configuration-layer//activate-package (car dep))
        (package-install (car dep))))
    (package-install pkg-name)))

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
   pkg-names (lambda (x) (not (package-installed-p x)))))

(defun configuration-layer//package-has-recipe-p (pkg-name)
  "Return non nil if PKG-NAME is the name of a package declared with a recipe."
  (when (object-assoc pkg-name :name configuration-layer--packages)
    (let* ((pkg (object-assoc pkg-name :name configuration-layer--packages))
           (location (oref pkg :location)))
      (and (listp location) (eq 'recipe (car location))))))

(defun configuration-layer//get-package-recipe (pkg-name)
  "Return the recipe for PGK-NAME if it has one."
  (let ((pkg (object-assoc pkg-name :name configuration-layer--packages)))
    (when pkg
      (let ((location (oref pkg :location)))
        (when (and (listp location) (eq 'recipe (car location)))
          location)))))

(defun configuration-layer//new-version-available-p (pkg-name)
  "Return non nil if there is a new version available for PKG-NAME."
  (let ((recipe (configuration-layer//get-package-recipe pkg-name))
        (cur-version (configuration-layer//get-package-version-string pkg-name))
        new-version)
    (when cur-version
      (setq new-version
            (if recipe
                (quelpa-checkout recipe (expand-file-name (symbol-name pkg-name)
                                                          quelpa-build-dir))
              (configuration-layer//get-latest-package-version-string
               pkg-name)))
      ;; (message "%s: %s > %s ?" pkg-name cur-version new-version)
      (if new-version
          (version< cur-version new-version)
        (cl-pushnew pkg-name configuration-layer--skipped-packages :test #'eq)
        nil))))

(defun configuration-layer//get-packages-to-update (pkg-names)
  "Return a filtered list of PKG-NAMES to update."
  (configuration-layer//filter-packages-with-deps
   pkg-names 'configuration-layer//new-version-available-p 'use-archive))

(defun configuration-layer//configure-packages (packages)
  "Configure all passed PACKAGES honoring the steps order."
  (setq spacemacs-loading-dots-chunk-threshold
        (/ (length configuration-layer--packages)
           spacemacs-loading-dots-chunk-count))
  (spacemacs-buffer/message "+ Configuring bootstrap packages...")
  (configuration-layer//configure-packages-2
   (configuration-layer/filter-objects
    packages (lambda (x) (eq 'bootstrap (oref x :step)))))
  (spacemacs-buffer/message "+ Configuring pre packages...")
  (configuration-layer//configure-packages-2
   (configuration-layer/filter-objects
    packages (lambda (x) (eq 'pre (oref x :step)))))
  (spacemacs-buffer/message "+ Configuring packages...")
  (configuration-layer//configure-packages-2
   (configuration-layer/filter-objects
    packages (lambda (x) (null (oref x :step))))))

(defun configuration-layer//configure-packages-2 (packages)
  "Configure all passed PACKAGES."
  (dolist (pkg packages)
    (spacemacs-buffer/loading-animation)
    (let ((pkg-name (oref pkg :name)))
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
            (let* ((owner (object-assoc (car (oref pkg :owners))
                                        :name configuration-layer--layers))
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

(defun configuration-layer//configure-package (pkg)
  "Configure PKG."
  (let* ((pkg-name (oref pkg :name))
         (owner (car (oref pkg :owners)))
         (owner-layer (object-assoc owner :name configuration-layer--layers))
         (disabled-for-layers (oref owner-layer :disabled-for)))
    (spacemacs-buffer/message (format "Configuring %S..." pkg-name))
    ;; pre-init
    (mapc (lambda (layer)
            (if (memq layer disabled-for-layers)
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
                   pkg-name layer err))))))
          (oref pkg :pre-layers))
    ;; init
    (spacemacs-buffer/message (format "  -> init (%S)..." owner))
    (funcall (intern (format "%S/init-%S" owner pkg-name)))
    ;; post-init
    (mapc (lambda (layer)
            (if (memq layer disabled-for-layers)
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
                   pkg-name layer err))))))
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
  (setq configuration-layer--skipped-packages nil)
  (let* ((update-packages
          (configuration-layer//get-packages-to-update
           (mapcar 'car (object-assoc-list
                         :name configuration-layer--used-distant-packages))))
         (skipped-count (length configuration-layer--skipped-packages))
         (date (format-time-string "%y-%m-%d_%H.%M.%S"))
         (rollback-dir (expand-file-name
                        (concat configuration-layer-rollback-directory
                                (file-name-as-directory date))))
         (upgrade-count (length update-packages))
         (upgraded-count 0)
         (update-packages-alist))
    (when configuration-layer--skipped-packages
      (spacemacs-buffer/append
       (format (concat "--> Warning: cannot update %s package(s), possibly due"
                       " to a temporary network problem: %s\n")
               skipped-count
               (mapconcat #'symbol-name
                          configuration-layer--skipped-packages
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
              (spacemacs-buffer/append (format "%s\n" x) t))
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
          (let* ((src-dir (configuration-layer//get-package-directory pkg))
                 (dest-dir (expand-file-name
                            (concat rollback-dir
                                    (file-name-as-directory
                                     (file-name-nondirectory src-dir))))))
            (copy-directory src-dir dest-dir 'keeptime 'create 'copy-content)
            (push (cons pkg (file-name-nondirectory src-dir))
                  update-packages-alist)))
        (spacemacs/dump-vars-to-file
         '(update-packages-alist)
         (expand-file-name (concat rollback-dir
                                   configuration-layer-rollback-info)))
        (dolist (pkg update-packages)
          (setq upgraded-count (1+ upgraded-count))
          (spacemacs-buffer/replace-last-line
           (format "--> preparing update of package %s... [%s/%s]"
                   pkg upgraded-count upgrade-count) t)
          (spacemacs//redisplay)
          (configuration-layer//package-delete pkg))
        (spacemacs-buffer/append
         (format "\n--> %s package(s) to be updated.\n" upgraded-count))
        (spacemacs-buffer/append
         "\nEmacs has to be restarted to actually install the new packages.\n")
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
              (copy-directory src-dir dest-dir 'keeptime 'create 'copy-content))
            (spacemacs//redisplay)))
        (spacemacs-buffer/append
         (format "\n--> %s packages rolled back.\n" rollbacked-count))
        (spacemacs-buffer/append
         "\nEmacs has to be restarted for the changes to take effect.\n")))))

(defun configuration-layer//activate-package (pkg)
  "Activate PKG."
  (if (version< emacs-version "24.3.50")
      ;; fake version list to always activate the package
      (package-activate pkg '(0 0 0 0))
    (unless (memq pkg package-activated-list)
      (package-activate pkg))))

(defun configuration-layer/get-layers-list ()
  "Return a list of all discovered layer symbols."
  (ht-keys configuration-layer-paths))

(defun configuration-layer/get-layer-property (layer slot)
  "Return the value of SLOT for the given LAYER."
  (slot-value (object-assoc layer :name configuration-layer--layers) slot))

(defun configuration-layer/get-layer-local-dir (layer)
  "Return the value of SLOT for the given LAYER."
  (concat (slot-value (object-assoc layer :name configuration-layer--layers)
                      :dir) "local/"))

(defun configuration-layer/get-layer-path (layer)
  "Return the path for LAYER symbol."
  (ht-get configuration-layer-paths layer))

(defun configuration-layer//get-packages-dependencies ()
  "Returns dependencies hash map for all packages in `package-alist'."
  (let ((result (make-hash-table :size 512)))
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

(defun configuration-layer//get-implicit-packages (packages)
  "Returns packages in `packages-alist' which are not found in PACKAGES."
  (let (imp-pkgs)
    (dolist (pkg package-alist)
      (let ((pkg-sym (car pkg)))
        (unless (object-assoc pkg-sym :name packages)
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
  (unless (or (object-assoc pkg-name :name dist-pkgs)
              (memq pkg-name configuration-layer--protected-packages))
    (if (ht-contains? dependencies pkg-name)
        (let ((parents (ht-get dependencies pkg-name)))
          (cl-reduce (lambda (x y) (and x y))
                     (mapcar (lambda (p) (configuration-layer//is-package-orphan
                                          p dist-pkgs dependencies))
                             parents)
                     :initial-value t))
      (not (object-assoc pkg-name :name dist-pkgs)))))

(defun configuration-layer//get-package-directory (pkg-name)
  "Return the directory path for package with name PKG-NAME."
  (let ((pkg-desc (assq pkg-name package-alist)))
    (cond
     ((version< emacs-version "24.3.50")
      (let* ((version (aref (cdr pkg-desc) 0))
             (elpa-dir (file-name-as-directory package-user-dir))
             (pkg-dir-name (format "%s-%s.%s"
                                   (symbol-name pkg-name)
                                   (car version)
                                   (cadr version))))
        (expand-file-name (concat elpa-dir pkg-dir-name))))
     (t (package-desc-dir (cadr pkg-desc))))))

(defun configuration-layer//get-package-deps-from-alist (pkg-name)
  "Return the dependencies alist for package with name PKG-NAME."
  (let ((pkg-desc (assq pkg-name package-alist)))
    (when pkg-desc
      (cond
       ((version< emacs-version "24.3.50") (aref (cdr pkg-desc) 1))
       (t (package-desc-reqs (cadr pkg-desc)))))))

(defun configuration-layer//get-package-deps-from-archive (pkg-name)
  "Return the dependencies alist for a PKG-NAME from the archive data."
  (let* ((pkg-arch (assq pkg-name package-archive-contents))
         (reqs (when pkg-arch (if (version< emacs-version "24.3.50")
                              (aref (cdr pkg-arch) 1)
                            (package-desc-reqs (cadr pkg-arch))))))
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
      (cond
       ((version< emacs-version "24.3.50") (package-version-join
                                            (aref (cdr pkg-desc) 0)))
       (t (package-version-join (package-desc-version (cadr pkg-desc))))))))

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
      (cond
       ((version< emacs-version "24.3.50") (package-version-join
                                            (aref (cdr pkg-arch) 0)))
       (t (package-version-join (package-desc-version (cadr pkg-arch))))))))

(defun configuration-layer//get-latest-package-version (pkg-name)
  "Return the versio list for package with name PKG-NAME."
  (let ((version-string
         (configuration-layer//get-latest-package-version-string pkg-name)))
    (unless (string-empty-p version-string)
      (version-to-list version-string))))

(defun configuration-layer//package-delete (pkg-name)
  "Delete package with name PKG-NAME."
  (cond
   ((version< emacs-version "24.3.50")
    (let ((v (configuration-layer//get-package-version-string pkg-name)))
      (when v (package-delete (symbol-name pkg-name) v))))
   ((version<= "25.0.50" emacs-version)
    (let ((p (cadr (assq pkg-name package-alist))))
      ;; add force flag to ignore dependency checks in Emacs25
      (when p (package-delete p t t))))
   (t (let ((p (cadr (assq pkg-name package-alist))))
        (when p (package-delete p))))))

(defun configuration-layer/delete-orphan-packages (packages)
  "Delete PACKAGES if they are orphan."
  (interactive)
  (let* ((dependencies (configuration-layer//get-packages-dependencies))
         (implicit-packages (configuration-layer//get-implicit-packages
                             configuration-layer--used-distant-packages))
         (orphans (configuration-layer//get-orphan-packages
                   configuration-layer--used-distant-packages
                   implicit-packages
                   dependencies))
         (orphans-count (length orphans))
         deleted-count)
    ;; (message "dependencies: %s" dependencies)
    ;; (message "implicit: %s" implicit-packages)
    ;; (message "orphans: %s" orphans)
    (if orphans
        (progn
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

(defun configuration-layer//lazy-install-extensions-for-layer (layer-symbol)
  "Return an alist of owned modes and extensions for the passed layer."
  (let* ((layer (object-assoc layer-symbol :name configuration-layer--layers))
         (packages (cfgl-layer-owned-packages layer))
         result)
    (dolist (pkg packages)
      (let ((pkg-sym (oref pkg :name)))
        (dolist (mode (list pkg-sym (intern (format "%S-mode" pkg-sym))))
          (let ((ext (configuration-layer//gather-auto-mode-extensions mode)))
            (when ext (push (cons mode ext) result))))))
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
                  (sort (object-assoc-list :name configuration-layer--layers)
                        (lambda (x y)
                          (string< (oref (cdr x) :name)
                                   (oref (cdr y) :name))))))))
    (let ((mode-exts (configuration-layer//lazy-install-extensions-for-layer
                      layer-name)))
      (dolist (x mode-exts)
        (configuration-layer//insert-lazy-install-form
         layer-name (car x) (cdr x))))))

(defun configuration-layer/display-summary (start-time)
  "Display a summary of loading time."
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time)))
        (stats (configuration-layer/configured-packages-stats
                configuration-layer--packages)))
    (spacemacs-buffer/append
     (format "\n%s packages loaded in %.3fs (e:%s r:%s l:%s b:%s)\n"
             (cadr (assq 'total stats))
             elapsed
             (cadr (assq 'elpa stats))
             (cadr (assq 'recipe stats))
             (cadr (assq 'local stats))
             (cadr (assq 'built-in stats))))))

(defun configuration-layer/load-or-install-protected-package
    (pkg &optional log file-to-load)
  "Load PKG package, and protect it against being deleted as an orphan.
See `configuration-layer/load-or-install-package' for more information."
  (push pkg configuration-layer--protected-packages)
  (configuration-layer/load-or-install-package pkg log file-to-load))

(defun configuration-layer/load-or-install-package
    (pkg &optional log file-to-load)
  "Load PKG package. PKG will be installed if it is not already installed.
Whenever the initial require fails the absolute path to the package
directory is returned.
If LOG is non-nil a message is displayed in spacemacs-buffer-mode buffer.
FILE-TO-LOAD is an explicit file to load after the installation."
  (let ((warning-minimum-level :error))
    (unless (require pkg nil 'noerror)
      ;; not installed, we try to initialize package.el only if required to
      ;; precious seconds during boot time
      (require 'cl)
      (let ((pkg-elpa-dir (spacemacs//get-package-directory pkg)))
        (if pkg-elpa-dir
            (add-to-list 'load-path pkg-elpa-dir)
          ;; install the package
          (when log
            (spacemacs-buffer/append
             (format "(Bootstrap) Installing %s...\n" pkg))
            (spacemacs//redisplay))
          (configuration-layer/retrieve-package-archives 'quiet)
          (package-install pkg)
          (setq pkg-elpa-dir (spacemacs//get-package-directory pkg)))
        (require pkg nil 'noerror)
        (when file-to-load
          (load-file (concat pkg-elpa-dir file-to-load)))
        pkg-elpa-dir))))

(defun configuration-layer//increment-error-count ()
  "Increment the error counter."
  (if configuration-layer-error-count
      (setq configuration-layer-error-count
            (1+ configuration-layer-error-count))
    (setq configuration-layer-error-count 1)))

(provide 'core-configuration-layer)

;;; core-configuration-layer.el ends here

